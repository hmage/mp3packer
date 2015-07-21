#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/custom.h>

#include <caml/unixsupport.h>

#include "ptr.h"


#if defined(__WIN32__) || defined(WIN32) || defined(_WIN32)
#define WIN32
#endif


#define NUM_BIG_TABLES 32
#define NUM_BIG_POSSIBILITIES 256
#define Big_table_index(i,j) (((i & 0xF) << 4) | (j & 0xF))
#define Count1_table_index(i,j,k,l) (((i & 1) << 3) | ((j & 1) << 2) | ((k & 1) << 1) | (l & 1))

#define Big_table_quant_index(t,i,j) ((Big_table_index(i,j) * NUM_BIG_TABLES) | ((t) & (NUM_BIG_TABLES - 1)))
#define Count1_table_quant_index(t,i,j,k,l) ((Count1_table_index(i,j,k,l) * 2) | ((t) & 1))

#define MAX_QUANTS 576
#define MAX_BANDS 22
#define TOO_MANY_BITS 32768
#define REGION1_MAX_BANDS 16
#define REGION2_MAX_BANDS 8

#define not_too_many(x) ((x >= TOO_MANY_BITS) ? (-1) : (x))

// I guess the min function doesn't always exist...
#define MIN(x,y) ((x)<(y) ? (x) : (y))

struct table_and_bits_t {
	int table;
	int bits;
};

struct table_and_bits_higher_t {
	int prev_length;
	struct table_and_bits_t current;
};

struct table_and_bits_count1_t {
	int table1;
	int bits1;
	int length; // length in 4-quant encoding groups
};

const int max_quant_per_table[] = {0,1,2,2,-1,3,3,5,5,5,7,7,7,15,-1,15,16,18,22,30,78,270,1038,8206,30,46,78,142,270,526,2062,8206};
const int linbits_per_table[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,6,8,10,13,4,5,6,7,8,9,11,13};


uint32 sub_sat(uint32 x, uint32 y) {
	uint32 z = x - y;
	uint32 over = z >> 31;
	uint32 mask = over - 1;
//	printf("z=%08X over=%08X mask=%08X\n", z, over, mask);
//	z ^= over;
	return(z & mask);
}

// This doesn't actually return the max, just a number which is guaranteed to have the same high-order bit
// This is really all we need for calculating the proper linbit table
uint32 max_with_sub15(uint32 so_far, uint32 q1, uint32 q2) {
	return(so_far | sub_sat(q1, 15) | sub_sat(q2, 15));
}


CAMLprim value mfu_find_best_config_base(
	value quant_bits_ptr,
	value quant_bits_count1_char_ptr,
	value scf_bands_ptr,
	value quant_raw_ptr,
	value debug_val
) {
	CAMLparam5(
		quant_bits_ptr,
		quant_bits_count1_char_ptr,
		scf_bands_ptr,
		quant_raw_ptr,
		debug_val
	);
	uint16_t *quant_bits = (uint16_t *)Begin_val(quant_bits_ptr);
	unsigned char *quant_bits_count1 = (unsigned char *)Begin_val(quant_bits_count1_char_ptr);
	uint16_t *scf_bands = (uint16_t *)Begin_val(scf_bands_ptr);
	int num_scf_bands = Length_val(scf_bands_ptr) / sizeof(uint16_t) - 1;
	int16_t *quants_raw = (int16_t *)Begin_val(quant_raw_ptr);
	int num_quants = Length_val(quant_raw_ptr) / sizeof(uint16_t);
	int debug = Bool_val(debug_val);

	int last_nonzero_quant = -1;
	int last_nonzero_band = -1;
	int last_big_quant = -1;
	int last_big_band = -1;
	int last_big_full_band = -1;
	int last_nonzero_full_band = -1;
	int q, b, i, qv, t;

	uint16_t quants_full[MAX_QUANTS];
	uint16_t quants[MAX_QUANTS];

	struct table_and_bits_count1_t count1_bits[MAX_QUANTS / 2 + 1];
	// These are indexed by the *length* of the regions, in bands
	struct table_and_bits_t big1_bits[MAX_BANDS + 1]; // Don't really need all of this...
	struct table_and_bits_higher_t big12_bits[MAX_BANDS + 1]; // or this...
	struct table_and_bits_higher_t big123_bits[MAX_BANDS + 1]; // ...

	int working_bits[MAX_BANDS][NUM_BIG_TABLES];

	int smallest_table;
	int smallest_bits;
	int smallest_end_quant;
	int smallest_prev_length;

	CAMLlocal1(out_val);

	if(debug) fflush(stdout);

	enter_blocking_section();

	for(b = 0; b < num_scf_bands; b++) {
		if(debug) printf("%2d:%-3d", b, scf_bands[b]);
		for(q = scf_bands[b]; q < scf_bands[b + 1]; q++) {
			quants_full[q] = abs(quants_raw[q]);
			quants[q] = MIN(15, quants_full[q]);
			if(debug) printf(" %d", quants_full[q]);
		}
		if(debug) printf("\n");
	}
/*
	for(t = 0; t < 8192; t++) {
		uint32 x = sub_sat(t, 15);
		uint32 y = (t > 15) ? (t - 15) : 0;
		if(x != y) printf("UNVALID! %d", t);
	}
*/

	// Define last_nonzero_quant, last_nonzero_band, last_big_quant, and last_big_band
	for(q = num_quants - 2, b = num_scf_bands - 1; q >= 0; q -= 2) {
		int x = quants[q + 0];
		int y = quants[q + 1];
		while(q < scf_bands[b]) b--;
		if((x | y) > 0) {
			last_nonzero_quant = q + 1;
			last_nonzero_band = b;
//			if(debug) printf("Last nonzero quant is %d in band %d\n", q, b);
			break;
		}
	}
	for(/*Reuse old values*/; q >= 0; q -= 2) {
		int x = quants[q + 0];
		int y = quants[q + 1];
		while(q < scf_bands[b]) b--;
		if((x | y) > 1) {
			last_big_quant = q + 1;
			last_big_band = b;
//			if(debug) printf("Last big quant is %d in band %d\n", q, b);
			break;
		}
	}
	// Also set the last band which is completely contained by big_values
	last_big_full_band = ((scf_bands[last_big_band + 1] == last_big_quant + 1) ? last_big_band : (last_big_band - 1));
	last_nonzero_full_band = ((scf_bands[last_nonzero_band + 1] == last_nonzero_quant + 1) ? last_nonzero_band : (last_nonzero_band - 1));
	if(debug) printf("Big quant: %-3d, nonzero quant: %-3d\n", last_big_quant, last_nonzero_quant);
	if(debug) printf("Big band: %-2d, big full band: %-2d\n", last_big_band, last_big_full_band);
	if(debug) printf("Nonzero b: %-2d, nonzero full b: %-2d\n", last_nonzero_band, last_nonzero_full_band);

	// We need to keep track of how many bits the count1 part will use
	{
		int working_table_0 = 0;
		int working_table_1 = 0;
		int num_groups;
		for(q = num_quants, num_groups = 0; q > (last_nonzero_quant & ~3); q -= 4) {
			count1_bits[q / 2].table1 = 0;
			count1_bits[q / 2].bits1 = 0;
			count1_bits[q / 2].length = 0;
		}
		for(/*q = (last_nonzero_quant & ~3)*/; q > last_big_quant; q -= 4, num_groups++) {
			qv = Count1_table_quant_index(0, quants[q], quants[q + 1], quants[q + 2], quants[q + 3]);
			working_table_0 += quant_bits_count1[qv + 0];
			working_table_1 += quant_bits_count1[qv + 1];
			if(working_table_0 < working_table_1) {
				count1_bits[q / 2].table1 = 0;
				count1_bits[q / 2].bits1 = working_table_0;
				count1_bits[q / 2].length = num_groups + 1;
			} else {
				count1_bits[q / 2].table1 = 1;
				count1_bits[q / 2].bits1 = working_table_1;
				count1_bits[q / 2].length = num_groups + 1;
			}
		}
		for(/*Reuse last*/; q >= 0; q -= 4) {
			count1_bits[q / 2].table1 = 0;
			count1_bits[q / 2].bits1 = TOO_MANY_BITS;
			count1_bits[q / 2].length = 0;
		}
	}
	if(last_nonzero_quant + 1 == num_quants) {
		// don't bother with the "odd" quants
		for(q = num_quants - 2; q >= 0; q -= 4) {
			count1_bits[q / 2].table1 = 0;
			count1_bits[q / 2].bits1 = TOO_MANY_BITS;
			count1_bits[q / 2].length = 0;
		}
	} else {
		int working_table_0 = 0;
		int working_table_1 = 0;
		int num_groups;
		for(q = num_quants - 2, num_groups = 0; q > ((last_nonzero_quant + 2) & ~3) - 2; q -= 4) {
			count1_bits[q / 2].table1 = 0;
			count1_bits[q / 2].bits1 = 0;
			count1_bits[q / 2].length = 0;
		}
		for(/*q = ((last_nonzero_quant + 2) & ~3) - 2*/; q > last_big_quant; q -= 4, num_groups++) {
			qv = Count1_table_quant_index(0, quants[q], quants[q + 1], quants[q + 2], quants[q + 3]);
			working_table_0 += quant_bits_count1[qv + 0];
			working_table_1 += quant_bits_count1[qv + 1];
			if(working_table_0 < working_table_1) {
				count1_bits[q / 2].table1 = 0;
				count1_bits[q / 2].bits1 = working_table_0;
				count1_bits[q / 2].length = num_groups + 1;
			} else {
				count1_bits[q / 2].table1 = 1;
				count1_bits[q / 2].bits1 = working_table_1;
				count1_bits[q / 2].length = num_groups + 1;
			}
		}
		for(/**/; q >= 0; q -= 4) {
			count1_bits[q / 2].table1 = 0;
			count1_bits[q / 2].bits1 = TOO_MANY_BITS;
			count1_bits[q / 2].length = 0;
		}
	}
//	printf("Last big quant     = %d\n", last_big_quant);
//	printf("Last nonzero quant = %d\n", last_nonzero_quant);
	if(debug) {
		for(q = 0; q <= num_quants; q += 2) {
			printf("%3d: %3d %d:%d\n", q, count1_bits[q / 2].length, count1_bits[q / 2].table1, count1_bits[q / 2].bits1);
		}
	}


	///////////
	// CACHE //
	///////////

	// Cache the first region
	for(t = 0; t < NUM_BIG_TABLES; t++) {
		working_bits[0][t] = 0;
	}
	big1_bits[0].table = 0;
	big1_bits[0].bits = 0;
	for(b = 0; b <= MIN(REGION1_MAX_BANDS - 1, last_nonzero_band - 2); b++) {
		int smallest_table = -1;
		int smallest_bits = TOO_MANY_BITS;
		uint32 largest_quant_sub15 = 0;
		if(debug) printf("B = %d\n", b);
		for(q = scf_bands[b]; q < scf_bands[b + 1]; q += 2) {
			uint16_t *bits_here = quant_bits + Big_table_quant_index(0, quants[q], quants[q + 1]);
			largest_quant_sub15 = max_with_sub15(largest_quant_sub15, quants_full[q], quants_full[q + 1]);
			if(debug) printf(" Q = %d (%d,%d)\n", q, quants_full[q], quants_full[q + 1]);
			if(debug) printf(" ");
			for(t = 0; t < NUM_BIG_TABLES; t++) {
				working_bits[0][t] += bits_here[t];
				if(debug) printf(" %4d", (working_bits[0][t] >= TOO_MANY_BITS ? -1 : working_bits[0][t]));
			}
			if(debug) printf("\n");
		}
		if(largest_quant_sub15 > 0) {
			// Adjust the tables if the max quant is larger than the base Huffman part
//			printf("Max quant in band %d is %d (or %d+15)\n", b, largest_quant, largest_quant_sub15);
			for(t = 0; t < NUM_BIG_TABLES; t++) {
				if(largest_quant_sub15 >> linbits_per_table[t]) working_bits[0][t] = TOO_MANY_BITS;
			}
		}
		// Find the lowest band and store it to big1_bits
		for(t = 0; t < NUM_BIG_TABLES; t++) {
			if(working_bits[0][t] < smallest_bits) {
				smallest_table = t;
				smallest_bits = working_bits[0][t];
			}
		}
		if(debug) printf(" smallest table = %-2d (%d bits)\n", smallest_table, smallest_bits);
		big1_bits[b + 1].table = smallest_table;
		big1_bits[b + 1].bits = smallest_bits;
	}
	for(b = REGION1_MAX_BANDS; b <= last_nonzero_band - 2; b++) {
		big1_bits[b + 1].table = -1;
		big1_bits[b + 1].bits = TOO_MANY_BITS;
	}

	// Cache the second region
	// First initialize the working values with the end values of the best first region
	for(b = 0; b <= last_nonzero_band - 2 + 1; b++) {
		for(t = 0; t < NUM_BIG_TABLES; t++) {
			working_bits[b][t] = big1_bits[b].bits;
		}
	}
//	printf("last_big_quant = %d\n", last_big_quant);
//	for(t = 0; t < NUM_BIG_TABLES; t++) {
//		working_bits[last_nonzero_band - 1][t] = 0;
//	}
//	printf("last_big_quant = %d\n", last_big_quant);
	big12_bits[0].prev_length = 0;
	big12_bits[0].current.table = 0;
	big12_bits[0].current.bits = 0;
/*
	for(b = 0; b <= last_nonzero_band - 1; b++) {
		printf("start band %d:\n", b);
		for(t = 0; t < NUM_BIG_TABLES; t++) {
			printf(" %4d", (working_bits[b][t] >= TOO_MANY_BITS ? -1 : working_bits[b][t]));
		}
		printf("\n");
	}
*/
	for(b = 0; b <= last_nonzero_band - 1; b++) {
		int smallest_table = -1;
		int smallest_bits = TOO_MANY_BITS;
		int smallest_region1_length = 0;
//		int largest_quant = 15;
		uint32 largest_quant_sub15 = 0;
		if(debug) printf("B2 = %d\n", b);
		for(q = scf_bands[b]; q < scf_bands[b + 1]; q += 2) {
			uint16_t *bits_here = quant_bits + Big_table_quant_index(0, quants[q], quants[q + 1]);
//			largest_quant = max(largest_quant, max(quants_full[q], quants_full[q + 1]));
			largest_quant_sub15 = max_with_sub15(largest_quant_sub15, quants_full[q], quants_full[q + 1]);
			for(i = 0; i <= b; i++) {
				// The variable i is the first band which the specified range starts on
				for(t = 0; t < NUM_BIG_TABLES; t++) {
					working_bits[i][t] += bits_here[t];
				}
			}
		}
		if(/*largest_quant > 15 || */largest_quant_sub15 > 0) {
			if(debug) printf("Max quant in band %d is around %d\n", b, largest_quant_sub15 + 15);
			for(i = 0; i <= b; i++) {
				for(t = 0; t < NUM_BIG_TABLES; t++) {
//					if(max_quant_per_table[t] < largest_quant) working_bits[i][t] = TOO_MANY_BITS;
					if(largest_quant_sub15 >> linbits_per_table[t]) working_bits[i][t] = TOO_MANY_BITS;
				}
			}
		}
		// Remove possibilities for the second region being more than REGION2_MAX_BANDS bands long
		for(i = 0; i <= b - REGION2_MAX_BANDS; i++) {
			for(t = 0; t < NUM_BIG_TABLES; t++) {
				working_bits[i][t] = TOO_MANY_BITS;
			}
		}
		for(i = 0; i <= b; i++) {
			for(t = 0; t < NUM_BIG_TABLES; t++) {
				if(working_bits[i][t] < smallest_bits) {
					smallest_table = t;
					smallest_bits = working_bits[i][t];
					smallest_region1_length = i;
				}
			}
		}
		big12_bits[b + 1].prev_length = smallest_region1_length;
		big12_bits[b + 1].current.bits = smallest_bits;
		big12_bits[b + 1].current.table = smallest_table;

		if(debug) {
			for(i = 0; i <= b; i++) {
				printf(" start = %d\n", i);
				printf(" ");
				for(t = 0; t < NUM_BIG_TABLES; t++) {
					printf(" %4d", (working_bits[i][t] >= TOO_MANY_BITS ? -1 : working_bits[i][t]));
				}
				printf("\n");
			}
			printf(" best configuration is %d bands, then table %d (%d total bits)\n", smallest_region1_length, smallest_table, smallest_bits);
		}
	}
/*
	for(b = 0; b <= last_nonzero_band - 1; b++) {
		printf("start band %d:\n", b);
		for(t = 0; t < NUM_BIG_TABLES; t++) {
			printf(" %4d", (working_bits[b][t] >= TOO_MANY_BITS ? -1 : working_bits[b][t]));
		}
		printf("\n");
	}
*/


	// Now take care of the third region...
	smallest_table = -1;
	smallest_bits = TOO_MANY_BITS;
	smallest_end_quant = -1;
	smallest_prev_length = 0;
	for(b = 0; b <= last_nonzero_band; b++) {
		if(debug) printf("123 is %d:%d\n", b, big12_bits[b].current.bits);
		for(t = 0; t < NUM_BIG_TABLES; t++) {
			working_bits[b][t] = big12_bits[b].current.bits;
		}
	}
	if(0 > last_big_quant) {
		// Have to consider no bigquants for possible endpoints
		int count1_bits_here = count1_bits[0].bits1;
		if(debug) printf(" Possible output with no big quants (%d count1 bits here)\n", count1_bits_here);
		if(count1_bits_here < smallest_bits) {
			if(debug) printf("  Best bits so far is %d\n", count1_bits_here);
			if(debug) printf("   count1 groups = %d\n", count1_bits[0].length);
			smallest_table = 0;
			smallest_bits = count1_bits_here;
			smallest_end_quant = 0;
			smallest_prev_length = 0;
		}
	}
	for(b = 0; b <= last_nonzero_band; b++) {
		uint32 largest_quant_sub15 = 0;
		if(debug) printf("B3 = %d\n", b);
		for(q = scf_bands[b]; q < scf_bands[b + 1]; q += 2) {
			uint16_t *bits_here = quant_bits + Big_table_quant_index(0, quants[q], quants[q + 1]);
			largest_quant_sub15 = max_with_sub15(largest_quant_sub15, quants_full[q], quants_full[q + 1]);
			for(i = 0; i <= b; i++) {
				// The variable i is the first band which the specified range starts on
				for(t = 0; t < NUM_BIG_TABLES; t++) {
					working_bits[i][t] += bits_here[t];
				}
			}
			if(q + 2 > last_big_quant) {
//				if(q > last_nonzero_quant + 2) break;
				// Have to consider this as a possible endpoint
				// That means we need to apply the largest_quant stuff here
				int count1_bits_here = count1_bits[q / 2 + 1].bits1;
				if(debug) printf(" Possible output quant at Q = %d (%d count1 bits here)\n", q, count1_bits_here);
				if(largest_quant_sub15 > 0) {
					if(debug) printf("  Wiping invalid linbits\n");
					for(i = 0; i <= b; i++) {
						for(t = 0; t < NUM_BIG_TABLES; t++) {
							if(largest_quant_sub15 >> linbits_per_table[t]) working_bits[i][t] = TOO_MANY_BITS;
						}
					}
					largest_quant_sub15 = 0;
				}
				for(i = 0; i <= b; i++) {
					for(t = 0; t < NUM_BIG_TABLES; t++) {
						if(working_bits[i][t] + count1_bits_here < smallest_bits) {
							if(debug) printf("  Best bits so far is %d\n", working_bits[i][t] + count1_bits_here);
							if(debug) printf("   Table = %d, prev length = %d, count1 groups = %d\n", t, i, count1_bits[q / 2 + 1].length);
							smallest_table = t;
							smallest_bits = working_bits[i][t] + count1_bits_here;
							smallest_end_quant = q + 2;
							smallest_prev_length = i;
						}
					}
				}
			}
		}
		if(largest_quant_sub15 > 0) {
			if(debug) printf("Max quant in band %d is around %d\n", b, largest_quant_sub15 + 15);
			for(i = 0; i <= b; i++) {
				for(t = 0; t < NUM_BIG_TABLES; t++) {
					if(largest_quant_sub15 >> linbits_per_table[t]) working_bits[i][t] = TOO_MANY_BITS;
				}
			}
		}
		if(debug) {
			for(i = 0; i <= b; i++) {
				printf(" start = %d\n", i);
				printf(" ");
				for(t = 0; t < NUM_BIG_TABLES; t++) {
					printf(" %4d", (working_bits[i][t] >= TOO_MANY_BITS ? -1 : working_bits[i][t]));
				}
				printf("\n");
			}
		}
	}




	if(debug) {
		for(b = 0; b <= last_nonzero_band - 1; b++) {
			printf(" [0+%d] %d, [%d+%d] %d\n",
				big12_bits[b].prev_length,
				big1_bits[big12_bits[b].prev_length].table,
				big12_bits[b].prev_length,
				b - big12_bits[b].prev_length,
				big12_bits[b].current.table
			);
		}
	}

	if(0) {
		printf("Best tables for first region:\n");
		for(b = 0; b <= last_nonzero_band - 2; b++) {
			printf(" %d:%d", big1_bits[b].table, big1_bits[b].bits);
		}
		printf("\n");
	}
/*
	printf("Done (best configuration is tables %d,%d,%d, length %db,%db,%dq)\n",
		big1_bits[big12_bits[smallest_prev_length].prev_length].table,
		big12_bits[smallest_prev_length].current.table,
		smallest_table,
		big12_bits[smallest_prev_length].prev_length,
		smallest_prev_length,
		smallest_end_quant
	);
*/
//	printf("C_SAYS %d %d %d %d\n", last_nonzero_quant, last_nonzero_band, last_big_quant, last_big_band);

	leave_blocking_section();

	// Now we have to collect the bigvalues and distribute them to the first regions
	// (by default, if only one region is needed, it will have come out of the above algorithm as being the third region)
	{
		int len1 = big12_bits[smallest_prev_length].prev_length;
		int len2 = smallest_prev_length - big12_bits[smallest_prev_length].prev_length;
		int table1 = big1_bits[big12_bits[smallest_prev_length].prev_length].table;
		int table2 = big12_bits[smallest_prev_length].current.table;
		int table3 = smallest_table;

		if(smallest_end_quant < 0) {
			len1 = 1;
			table1 = 0;
			len2 = 1;
			table2 = 0;
			table3 = 0;
		} else if(len1 == 0 && len2 == 0) {
			len1 = 1;
			table1 = table3;
			len2 = 1;
			table2 = table3;
		} else if(len1 == 0 && len2 == 1) {
			len1 = 1;
			table1 = table2;
			len2 = 1;
			table2 = table3;
		} else if(len1 == 0) {
			len1 = 1;
			table1 = table2;
			len2 -= 1;
		} else if(len2 == 0) {
			len2 = 1;
			table2 = table3;
		}
		if(debug) printf("%d:%d  %d:%d\n", len1, table1, len2, table2);

		out_val = caml_alloc_tuple(8);
		Store_field(out_val, 0, Val_int(len1 - 1));
		Store_field(out_val, 1, Val_int(len2 - 1));
		Store_field(out_val, 2, Val_int(smallest_end_quant / 2));
		Store_field(out_val, 3, Val_int(count1_bits[smallest_end_quant / 2].length));
		Store_field(out_val, 4, Val_int(table1));
		Store_field(out_val, 5, Val_int(table2));
		Store_field(out_val, 6, Val_int(table3));
		Store_field(out_val, 7, Val_bool(count1_bits[smallest_end_quant / 2].table1));

		if(debug) fflush(stdout);

		CAMLreturn(out_val);
	}
}


/*************/
/* TRY AGAIN */
/*************/
// Now with more SSEs!
// This function depends on 16-bit unsigned numbers and saturated arithmetic everywhere!


// Only use for Windows -- I don't really want to debug this too much
#ifdef WIN32

#include <emmintrin.h>
#include <smmintrin.h>

#define SHORTS_PER_128 (sizeof(__m128i) / sizeof(uint16_t))

typedef union {
	uint16_t s[4 * SHORTS_PER_128];
	__m128i v[4]; // Array of 4x 128-bit vectors, each with 8x 16-bit unsigned integers
} table_vec;




// Adds each element in the two lists (storing into a), saturating to 0xFFFF
void add_sat_sse2(table_vec *a, table_vec *b) {
	a->v[0] = _mm_adds_epu16(a->v[0], b->v[0]);
	a->v[1] = _mm_adds_epu16(a->v[1], b->v[1]);
	a->v[2] = _mm_adds_epu16(a->v[2], b->v[2]);
	a->v[3] = _mm_adds_epu16(a->v[3], b->v[3]);
}

void clear_table_vec(table_vec *a) {
	a->v[0] = _mm_setzero_si128();
	a->v[1] = _mm_setzero_si128();
	a->v[2] = _mm_setzero_si128();
	a->v[3] = _mm_setzero_si128();
}
void clear_all_table_vec(table_vec *a, int num) {
	int i;
	for(i = 0; i < num; i++, a++) {
		clear_table_vec(a);
	}
}
void set_table_vec(table_vec *a, uint16_t s) {
	__m128i svec = _mm_set1_epi32(s * 0x00010001);
	a->v[0] = svec;
	a->v[1] = svec;
	a->v[2] = svec;
	a->v[3] = svec;
}
/*
void invalid_table_vec(table_vec *a) {
	__m128i set = _mm_set1_epi32(0xFFFFFFFF);
	a->v[0] = set;
	a->v[1] = set;
	a->v[2] = set;
	a->v[3] = set;
}
*/
void print_table_vec(table_vec *a) {
	int i;
	for(i = 0; i < 4 * SHORTS_PER_128; i++) {
/*
		if((i & 7) == 0)
			printf(" ");
		else
			printf(":");
*/
		if(a->s[i] >= TOO_MANY_BITS)
			printf(" ----");
		else
			printf(" %4d", a->s[i]);
	}
	printf("\n");
}

// Gets the nth short from the __m128i
// without dumping anything out of the registers
// A little odd since the only non-static shuffle is byte-wise
// so we need to make sure we get both bytes of the uint16_t in order
uint16_t get_nth_ssse3(__m128i a, uint16_t i) {
	uint16_t mask = i * 0x0202 + 0x0100;
	// Removing the initialization for ignore_me is fine since the only part of it we care about is the 0 index, which is set by the _mm_insert_epi16 below
	__m128i ignore_me;// = _mm_setzero_si128();
	__m128i bigmask = _mm_insert_epi16(ignore_me, mask, 0);//_mm_set1_epi32(mask);
	__m128i res = _mm_shuffle_epi8(a, bigmask);
	return(_mm_extract_epi16(res, 0));
}


// This function is 100x-200x faster than the non-SSE one!
// Finds the table with the fewest bits; returns the table index
// and puts the corresponding value in out_value
uint16_t min_index_sse41(table_vec *a, uint16_t *out_value) {
	__m128i min0 = _mm_minpos_epu16(a->v[0]);
	__m128i min1 = _mm_minpos_epu16(a->v[1]);
	__m128i min2 = _mm_minpos_epu16(a->v[2]);
	__m128i min3 = _mm_minpos_epu16(a->v[3]);
	__m128i min02 = _mm_unpacklo_epi16(min0, min2);
	__m128i min13 = _mm_unpacklo_epi16(min1, min3);
	__m128i min0123 = _mm_unpacklo_epi16(min02, min13);

	// min0123 now contains the lowest value in each of the 4 vectors, then the index of each of those values in their respective vectors
#if 0
	// Set the indexes to 0xFFFF, then calculate the min on the 4 remaining values
	// This will work even if the 4 values are also 0xFFFF, since the lowest index is returned in case of a tie
	__m128i hi_mask = _mm_set_epi32(0xFFFFFFFF,0xFFFFFFFF,0x00000000,0x00000000);
	__m128i min_val_only = _mm_or_si128(min0123, hi_mask);
	__m128i min_index_only = _mm_shuffle_epi32(min0123, _MM_SHUFFLE(1,0,3,2));
	__m128i final_min = _mm_minpos_epu16(min_val_only);
	int min_array_index = _mm_extract_epi16(final_min, 1);

	// Now get the actual index back
	int final_index = get_nth_ssse3(min_index_only, min_array_index);
	*out_value = _mm_extract_epi16(final_min, 0);
	return(final_index + 8 * min_array_index);
#else
	// We can avoid loading the constant if we just double all the values, then multiply by 4 instead of 8 at the end
	__m128i val00112233 = _mm_unpacklo_epi16(min0123,min0123);
	__m128i pos00112233 = _mm_unpackhi_epi16(min0123,min0123);
	__m128i final_min_times_2 = _mm_minpos_epu16(val00112233);
	__m128i final_min = _mm_minpos_epu16(val00112233);
	int min_array_index = _mm_extract_epi16(final_min, 1);

	int final_index = get_nth_ssse3(pos00112233, min_array_index);
	*out_value = _mm_extract_epi16(final_min, 0);
	return(final_index + 4 * min_array_index);
#endif
}

// Same as above but with an array of 2 table_vecs
// This should be a bit more efficient since we don't need to load a mask or shuffle anything around
uint16_t min_index_2_sse41(table_vec *a, unsigned short *out_value) {
	__m128i min0 = _mm_unpacklo_epi16(_mm_minpos_epu16(a[0].v[0]), _mm_minpos_epu16(a[1].v[0]));
	__m128i min1 = _mm_unpacklo_epi16(_mm_minpos_epu16(a[0].v[1]), _mm_minpos_epu16(a[1].v[1]));
	__m128i min2 = _mm_unpacklo_epi16(_mm_minpos_epu16(a[0].v[2]), _mm_minpos_epu16(a[1].v[2]));
	__m128i min3 = _mm_unpacklo_epi16(_mm_minpos_epu16(a[0].v[3]), _mm_minpos_epu16(a[1].v[3]));
	__m128i min02 = _mm_unpacklo_epi16(min0, min2);
	__m128i min13 = _mm_unpacklo_epi16(min1, min3);
	__m128i min0123 = _mm_unpackhi_epi16(min02, min13);
	__m128i val0123 = _mm_unpacklo_epi16(min02, min13);
	__m128i final_min = _mm_minpos_epu16(val0123);
	int min_array_index = _mm_extract_epi16(final_min, 1);

	int final_index = get_nth_ssse3(min0123, min_array_index);
	*out_value = _mm_extract_epi16(final_min, 0);
//	printf("Got best index %d:%d (value is %d)\n", min_array_index, final_index, *out_value);
	return(final_index + 8 * min_array_index);
}

// Basically just out[i] = min(in[i], 15)
/*
void write_max_15_sse41(__m128i *out, __m128i *in, int count_128) {
	int i;
	// _mm_set1_epi16 seems to not be as efficient as it should be
	__m128i fifteen = _mm_set1_epi32(0x000F000F);
	for(i = 0; i < count_128; i++, in++, out++) {
		*out = _mm_min_epu16(*in, fifteen);
	}
}
*/

// Given the quants, output an absolute-value array and a max-15 array
void process_quants_ssse3(__m128i *quant_full_abs, __m128i *quants, __m128i *quant_in) {
	int i = 0;
	int j;
	__m128i fifteen = _mm_set1_epi32(0x000F000F);
//	__m128i abs, max_15;
	while(i < MAX_QUANTS / SHORTS_PER_128) {
		// It looks like the compiler refuses to unroll loops
		*quant_full_abs = _mm_abs_epi16(*quant_in);
		*quants = _mm_min_epu16(*quant_full_abs, fifteen);
		quant_full_abs++; quants++; quant_in++; i++;
		*quant_full_abs = _mm_abs_epi16(*quant_in);
		*quants = _mm_min_epu16(*quant_full_abs, fifteen);
		quant_full_abs++; quants++; quant_in++; i++;
		*quant_full_abs = _mm_abs_epi16(*quant_in);
		*quants = _mm_min_epu16(*quant_full_abs, fifteen);
		quant_full_abs++; quants++; quant_in++; i++;
		*quant_full_abs = _mm_abs_epi16(*quant_in);
		*quants = _mm_min_epu16(*quant_full_abs, fifteen);
		quant_full_abs++; quants++; quant_in++; i++;
	}
}

// This is the max value that can be stored with the linbits per table
table_vec max_quant_sub15 = {{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,3,7,15,63,255,1023,8191,15,31,63,127,255,511,2047,8191}};
// If max_quant_here cannot be stored with a given table, that value in a will be masked out to 0xFFFF
// This is a bit dodgy since _mm_cmpgt_epi16 operates on signed numbers, but no input should be larger than 8191
void quant_can_be_stored_sse2(table_vec *a, uint16_t max_quant_here) {
	__m128i max_quant_vec = _mm_set1_epi32(0x00010001 * max_quant_here);
	__m128i gt0, gt1, gt2, gt3;
	gt0 = _mm_cmpgt_epi16(max_quant_vec, max_quant_sub15.v[0]);
	gt1 = _mm_cmpgt_epi16(max_quant_vec, max_quant_sub15.v[1]);
	gt2 = _mm_cmpgt_epi16(max_quant_vec, max_quant_sub15.v[2]);
	gt3 = _mm_cmpgt_epi16(max_quant_vec, max_quant_sub15.v[3]);
	a->v[0] = _mm_or_si128(gt0, a->v[0]);
	a->v[1] = _mm_or_si128(gt1, a->v[1]);
	a->v[2] = _mm_or_si128(gt2, a->v[2]);
	a->v[3] = _mm_or_si128(gt3, a->v[3]);
}




typedef struct {
	int table1;
	int bits1;
	int length;
} count1_table;

typedef struct {
	int region0_table;
	int region0_bits;
} region0_table;

typedef struct {
	int region0_bands;
	int region0_table;
	int region1_table;
	int total_bits;
} region1_table;

typedef struct {
	int region0_bands;
	int region0_table;
	int region1_bands;
	int region1_table;
	int region2_table;
	int total_bits;
} region2_table;

// NOTE: takes 16-bit quants! (must be 16-byte aligned)
CAMLprim value mfu_find_best_config_sse41(
	value quant_bits_ptr,
	value quant_bits_count1_char_ptr,
	value scf_bands_ptr,
	value quant_raw_ptr,
	value debug_val
) {
	CAMLparam5(
		quant_bits_ptr,
		quant_bits_count1_char_ptr,
		scf_bands_ptr,
		quant_raw_ptr,
		debug_val
	);
	table_vec *quant_bits = (table_vec *)Begin_val(quant_bits_ptr);
	unsigned char *quant_bits_count1 = (unsigned char *)Begin_val(quant_bits_count1_char_ptr);
	uint16_t *scf_bands = (uint16_t *)Begin_val(scf_bands_ptr);
	int num_scf_bands = Length_val(scf_bands_ptr) / sizeof(uint16_t) - 1;
	__m128i *quants_raw128 = (__m128i *)Begin_val(quant_raw_ptr);
	int num_quants = Length_val(quant_raw_ptr) / sizeof(uint16_t);
	int debug = Bool_val(debug_val);

	int q, b, i, qv, t;

	__m128i quants_full128[MAX_QUANTS / SHORTS_PER_128];
	uint16_t *quants_full = (uint16_t *)quants_full128;
	__m128i quants128[MAX_QUANTS / SHORTS_PER_128];
	uint16_t *quants = (uint16_t *)quants128;

	count1_table count1_bits[MAX_QUANTS / 2 + 1];
	region0_table region0_bits[MAX_BANDS + 1];
	region1_table region1_bits[MAX_BANDS + 1];

	table_vec working_region0[1];
	table_vec working_region1[MAX_BANDS + 1];
	table_vec working_region2[MAX_BANDS + 1];

	uint16_t final_possibility_total_bits = TOO_MANY_BITS;
	uint16_t final_possibility_big_quants = 0;
	uint16_t final_possibility_region0_table;
	uint16_t final_possibility_region0_bands;
	uint16_t final_possibility_region1_table;
	uint16_t final_possibility_region1_bands;
	uint16_t final_possibility_region2_table;
	uint16_t final_possibility_part1_table;

#define PRINT_BEST_CONFIG { \
	printf("Best is %2d bands with t%2d, then %2d bands with t%2d, then t%2d until quant %d, then count1 t%d (%d bits)\n", \
		final_possibility_region0_bands, \
		final_possibility_region0_table, \
		final_possibility_region1_bands, \
		final_possibility_region1_table, \
		final_possibility_region2_table, \
		final_possibility_big_quants, \
		final_possibility_part1_table, \
		final_possibility_total_bits \
	); \
}


	CAMLlocal1(out_val);

	if(debug) {
		fflush(stdout);
		printf("DOING THINGS\n");
		fflush(stdout);
	}

	enter_blocking_section();

	// Set the abs and min(15,abs) arrays
	process_quants_ssse3(quants_full128, quants128, quants_raw128);
//	write_max_15_sse41(quants128, quants_full128, num_quants / SHORTS_PER_128);

	if(debug) {
		printf("After process_quants\n");
		fflush(stdout);
	}

	////////////
	// COUNT1 //
	////////////
	{
		int working_table_0 = 0;
		int working_table_1 = 0;
		int num_groups;
		// Set the initial (unsupported) one
		for(q = num_quants, num_groups = 0; q >= num_quants; q -= 4) {
			count1_bits[q/2].table1 = 0;
			count1_bits[q/2].bits1 = 0;
			count1_bits[q/2].length = 0;
		}
		// Iterate until a nonzero quant has been reached
		for(/*Reuse*/; q >= 0; q -= 4) {
			qv = (quants[q] | quants[q + 1] | quants[q + 2] | quants[q + 3]);
			if(qv > 0) {
				break;
			} else {
				count1_bits[q/2].table1 = 0;
				count1_bits[q/2].bits1 = 0;
				count1_bits[q/2].length = 0;
			}
		}
		// Iterate until a quant > 1 has been reached
		for(/*Reuse*/; q >= 0; q -= 4, num_groups++) {
			qv = (quants[q] | quants[q + 1] | quants[q + 2] | quants[q + 3]);
			if(qv > 1) {
				// Too many bits; give up and go home
				break;
			} else {
				qv = Count1_table_quant_index(0, quants[q], quants[q + 1], quants[q + 2], quants[q + 3]);
				working_table_0 += quant_bits_count1[qv + 0];
				working_table_1 += quant_bits_count1[qv + 1];
				count1_bits[q/2].length = num_groups + 1;
				if(working_table_0 < working_table_1) {
					count1_bits[q/2].table1 = 0;
					count1_bits[q/2].bits1 = working_table_0;
				} else {
					count1_bits[q/2].table1 = 1;
					count1_bits[q/2].bits1 = working_table_1;
				}
			}
		}
		// Fill up the rest with impossibilities
		for(/*Reuse*/; q >= 0; q -= 4) {
			count1_bits[q/2].table1 = 0;
			count1_bits[q/2].bits1 = TOO_MANY_BITS;
			count1_bits[q/2].length = num_groups;
		}
	}
	{
		int working_table_0 = 0;
		int working_table_1 = 0;
		int num_groups;
		// Check the last value to see if it's good
		for(q = num_quants - 2, num_groups = 0; q >= num_quants - 2; q -= 4) {
			qv = (quants[q] | quants[q + 1]);
			count1_bits[q/2].table1 = 0;
			count1_bits[q/2].length = 0;
			if(qv > 0) {
				// No good; poison all other possibilities here
				working_table_0 = TOO_MANY_BITS;
				working_table_1 = TOO_MANY_BITS;
				count1_bits[q/2].bits1 = TOO_MANY_BITS;
			} else {
				count1_bits[q/2].bits1 = 0;
			}
		}
		// Iterate until a nonzero quant has been reached
		for(/*Reuse*/; q >= 0 && working_table_0 < TOO_MANY_BITS; q -= 4) {
			qv = (quants[q] | quants[q + 1] | quants[q + 2] | quants[q + 3]);
			if(qv > 0) {
				break;
			} else {
				count1_bits[q/2].table1 = 0;
				count1_bits[q/2].bits1 = 0;
				count1_bits[q/2].length = 0;
			}
		}
		// Iterate until a quant > 1 has been reached
		for(/*Reuse*/; q >= 0 && working_table_0 < TOO_MANY_BITS; q -= 4, num_groups++) {
			qv = (quants[q] | quants[q + 1] | quants[q + 2] | quants[q + 3]);
			if(qv > 1) {
				// Too many bits; give up and go home
				break;
			} else {
				qv = Count1_table_quant_index(0, quants[q], quants[q + 1], quants[q + 2], quants[q + 3]);
				working_table_0 += quant_bits_count1[qv + 0];
				working_table_1 += quant_bits_count1[qv + 1];
				count1_bits[q/2].length = num_groups + 1;
				if(working_table_0 < working_table_1) {
					count1_bits[q/2].table1 = 0;
					count1_bits[q/2].bits1 = working_table_0;
				} else {
					count1_bits[q/2].table1 = 1;
					count1_bits[q/2].bits1 = working_table_1;
				}
			}
		}
		// Fill up the rest with impossibilities
		for(/*Reuse*/; q >= 0; q -= 4) {
			count1_bits[q/2].table1 = 0;
			count1_bits[q/2].bits1 = TOO_MANY_BITS;
			count1_bits[q/2].length = num_groups;
		}
	}
	if(debug) {
		printf("Count1:\n");
		for(q = 0; q <= num_quants; q += 2) {
			printf(" %3d: Table %d with size %3d and length %d\n", q, count1_bits[q/2].table1, not_too_many(count1_bits[q/2].bits1),  count1_bits[q/2].length);
		}
	}

	////////////////
	// BIG VALUES //
	////////////////
	clear_all_table_vec(working_region0, 1);
	clear_all_table_vec(working_region1, MAX_BANDS);
	clear_all_table_vec(working_region2, MAX_BANDS);
	region0_bits[0].region0_table = 0;
	region0_bits[0].region0_bits = 0;
	region1_bits[0].region0_bands = 0;
	region1_bits[0].region0_table = 0;
	region1_bits[0].region1_table = 0;
	region1_bits[0].total_bits = 0;
	if(count1_bits[0].bits1 < final_possibility_total_bits) {
		// There is a possibliity to output only count1 values
		final_possibility_total_bits = count1_bits[0].bits1;
		final_possibility_big_quants = 0;
		final_possibility_region0_table = 0;
		final_possibility_region0_bands = 1;
		final_possibility_region1_table = 0;
		final_possibility_region1_bands = 1;
		final_possibility_region2_table = 0;
	}
	if(debug) {
		printf("Before bands:          ");
		print_table_vec(working_region0);
	}
	for(b = 0; b < MAX_BANDS; b++) {
		uint16_t smallest_region0_bits;
		int smallest_region0_length;
		uint16_t smallest_region1_bits;
		uint16_t smallest_region1_bits_here;
		int smallest_region1_table;
		int smallest_region1_table_here;
		uint16_t smallest_region2_bits = TOO_MANY_BITS;
		uint16_t smallest_total_bits_here;
		uint16_t smallest_region2_table;
		uint16_t smallest_region2_table_here;
		uint32 largest_quant_sub15 = 0;
		if(debug) printf("Band %d, quants %d-%d\n", b, scf_bands[b], scf_bands[b + 1] - 1);
		for(q = scf_bands[b]; q < scf_bands[b + 1]; q += 2) {
			table_vec *bits_here = quant_bits + Big_table_index(quants[q], quants[q + 1]);
			largest_quant_sub15 = max_with_sub15(largest_quant_sub15, quants_full[q], quants_full[q + 1]);

			// Only 16 bands are OK for region0
			if(b == REGION1_MAX_BANDS/*OK, region 0 was called region 1 at one point*/) {
				// Zero out region0
				set_table_vec(working_region0, TOO_MANY_BITS);
			} else if (b < REGION1_MAX_BANDS) {
				add_sat_sse2(working_region0, bits_here);
			}

			// Only 8 bands are OK for part1
			for(i = b; i >= 0 && i > b - REGION2_MAX_BANDS; i--) {
				add_sat_sse2(working_region1 + i, bits_here);
			}
			if(i >= 0) {
				set_table_vec(working_region1 + i, TOO_MANY_BITS);
			}

			for(i = 0; i <= b; i++) {
				add_sat_sse2(working_region2 + i, bits_here);
			}
			if(count1_bits[q/2+1].bits1 < TOO_MANY_BITS) {
				// This is a possible end point
				if(debug) {
					printf("Possible big quants end at %d\n", q + 2);
					printf("Region2 here (last band tables have not been culled for >15 quants):\n");
					for(i = 0; i <= b; i++) {
						printf("  Region2 from %2d to quant %3d:   ", i, q + 2);
						print_table_vec(working_region2 + i);
					}
				}
				i = 0;
/*
				for(i = 0; i <= b - 1; i += 2) {
					if(debug) printf("Checking region0+1 = %2d and %2d (best is %2d with t%2d)\n", i, i + 1, region1_bits[i].region0_bands, region1_bits[i].region0_table);
					quant_can_be_stored_sse2(working_region2 + i + 0, largest_quant_sub15);
					quant_can_be_stored_sse2(working_region2 + i + 1, largest_quant_sub15);
					smallest_region2_table_here = min_index_2_sse41(working_region2 + i, &smallest_total_bits_here);
					smallest_total_bits_here += count1_bits[q/2+1].bits1;
					if(smallest_total_bits_here < final_possibility_total_bits) {
						int j = i + smallest_region2_table_here / NUM_BIG_TABLES;
						final_possibility_total_bits = smallest_total_bits_here;
						final_possibility_big_quants = q + 2;
						final_possibility_region2_table = (smallest_region2_table_here & (NUM_BIG_TABLES - 1));
						final_possibility_region1_table = region1_bits[j].region1_table;
						final_possibility_region0_table = region1_bits[j].region0_table;
						final_possibility_region0_bands = region1_bits[j].region0_bands;
						final_possibility_region1_bands = j - final_possibility_region0_bands;
						final_possibility_part1_table = count1_bits[q/2+1].table1;
						if(debug) PRINT_BEST_CONFIG;
					}
				}
*/
				for(/*i = 0*/; i <= b; i++) {
					if(debug) printf("Checking region0+1 = %2d (best is %2d with t%2d)\n", i, region1_bits[i].region0_bands, region1_bits[i].region0_table);
					quant_can_be_stored_sse2(working_region2 + i, largest_quant_sub15);
					smallest_region2_table_here = min_index_sse41(working_region2 + i, &smallest_total_bits_here);
					smallest_total_bits_here += count1_bits[q/2+1].bits1;
					if(smallest_total_bits_here < final_possibility_total_bits) {
						final_possibility_total_bits = smallest_total_bits_here;
						final_possibility_big_quants = q + 2;
						final_possibility_region2_table = smallest_region2_table_here;
						final_possibility_region1_table = region1_bits[i].region1_table;
						final_possibility_region0_table = region1_bits[i].region0_table;
						final_possibility_region0_bands = region1_bits[i].region0_bands;
						final_possibility_region1_bands = i - final_possibility_region0_bands;
						final_possibility_part1_table = count1_bits[q/2+1].table1;
						if(debug) PRINT_BEST_CONFIG;
					}
				}
				if(count1_bits[q/2+1].length == 0) {
					if(debug) printf("Give up here\n");
					b = MAX_BANDS;
					q = MAX_QUANTS;
					break;
				}
			}
		}

		if(b == MAX_BANDS) {
			// The only way this can be the case is if we hit the "Give up here" part above
			// I don't like the idea of goto functions...
			if(debug) printf("Still giving up here\n");
			break;
		}

		// REGION0
		// Now handle the largest_quant thing
		quant_can_be_stored_sse2(working_region0, largest_quant_sub15);
		// And find the best table for ending on this band
		region0_bits[b + 1].region0_table = min_index_sse41(working_region0, &smallest_region0_bits);
		region0_bits[b + 1].region0_bits = smallest_region0_bits;

		// Add region0 to the region1 possibility that starts here
		set_table_vec(working_region1 + b + 1, smallest_region0_bits);

		// REGION1 AND REGION2
		smallest_region1_bits = TOO_MANY_BITS;
		// Iterate through all the possible endpoints for region0
		for(i = 0; i <= b; i++) {
			if(largest_quant_sub15 > 0) {
				quant_can_be_stored_sse2(working_region1 + i, largest_quant_sub15);
				quant_can_be_stored_sse2(working_region2 + i, largest_quant_sub15);
			}
			smallest_region1_table_here = min_index_sse41(working_region1 + i, &smallest_region1_bits_here);
			if(smallest_region1_bits_here < smallest_region1_bits) {
				smallest_region1_bits = smallest_region1_bits_here;
				smallest_region1_table = smallest_region1_table_here;
				smallest_region0_length = i;
			}
		}
		region1_bits[b + 1].region0_bands = smallest_region0_length;
		region1_bits[b + 1].region0_table = region0_bits[smallest_region0_length].region0_table;
		region1_bits[b + 1].region1_table = smallest_region1_table;
		region1_bits[b + 1].total_bits = smallest_region1_bits;

		// Add region0 and region1 to the region2 possibilitiy that starts here
		set_table_vec(working_region2 + b + 1, smallest_region1_bits);

		if(debug) {
			printf("Region0 after band %2d: ", b);
			print_table_vec(working_region0);
			printf("Region1:\n");
			for(i = 0; i <= b; i++) {
				printf("  Region1 from %2d through %2d: ", i, b);
				print_table_vec(working_region1 + i);
			}
			printf("Region2:\n");
			for(i = 0; i <= b; i++) {
				printf("  Region2 from %2d through %2d:   ", i, b);
				print_table_vec(working_region2 + i);
			}

			printf("Region0 best is t%2d with %d bits\n", region0_bits[b + 1].region0_table, region0_bits[b + 1].region0_bits);
			printf("Region1 best is %2d bands with t%2d, then t%2d (for %d bits)\n", region1_bits[b + 1].region0_bands, region1_bits[b + 1].region0_table, region1_bits[b + 1].region1_table, region1_bits[b + 1].total_bits);
			printf("\n");
		}
	}

	if(debug) {
		printf("FOUND BEST CONFIGURATION:\n");
		printf("Best is %2d bands with t%2d, then %2d bands with t%2d, then t%2d until quant %d, then count1 t%d (%d bits)\n",
			final_possibility_region0_bands,
			final_possibility_region0_table,
			final_possibility_region1_bands,
			final_possibility_region1_table,
			final_possibility_region2_table,
			final_possibility_big_quants,
			final_possibility_part1_table,
			final_possibility_total_bits
		);
	}

	if(final_possibility_big_quants < 0) {
		final_possibility_region0_bands = 1;
		final_possibility_region0_table = 0;
		final_possibility_region1_bands = 1;
		final_possibility_region1_table = 0;
		final_possibility_region2_table = 0;
	} else if(final_possibility_region0_bands == 0 && final_possibility_region1_bands == 0) {
		final_possibility_region0_bands = 1;
		final_possibility_region0_table = final_possibility_region2_table;
		final_possibility_region1_bands = 1;
		final_possibility_region1_table = final_possibility_region2_table;
	} else if(final_possibility_region0_bands == 0 && final_possibility_region1_bands == 1) {
		final_possibility_region0_bands = 1;
		final_possibility_region0_table = final_possibility_region1_table;
		final_possibility_region1_bands = 1;
		final_possibility_region1_table = final_possibility_region2_table;
	} else if(final_possibility_region0_bands == 0) {
		final_possibility_region0_bands = 1;
		final_possibility_region0_table = final_possibility_region1_table;
		final_possibility_region1_bands -= 1;
	} else if(final_possibility_region1_bands == 0) {
		final_possibility_region1_bands = 1;
		final_possibility_region1_table = final_possibility_region2_table;
	}

	leave_blocking_section();

	out_val = caml_alloc_tuple(8);
	Store_field(out_val, 0, Val_int(final_possibility_region0_bands - 1));
	Store_field(out_val, 1, Val_int(final_possibility_region1_bands - 1));
	Store_field(out_val, 2, Val_int(final_possibility_big_quants / 2));
	Store_field(out_val, 3, Val_int(count1_bits[final_possibility_big_quants / 2].length));
	Store_field(out_val, 4, Val_int(final_possibility_region0_table));
	Store_field(out_val, 5, Val_int(final_possibility_region1_table));
	Store_field(out_val, 6, Val_int(final_possibility_region2_table));
	Store_field(out_val, 7, Val_bool(count1_bits[final_possibility_big_quants / 2].table1));

	if(debug) {
		printf("Count1:\n");
		printf("Neg1 count1 is %d %d %d\n", count1_bits[-1].table1, count1_bits[-1].bits1, count1_bits[-1].length);
		for(q = 0; q <= num_quants; q += 2) {
			printf(" %3d: Table %d with size %3d and length %d\n", q, count1_bits[q/2].table1, not_too_many(count1_bits[q/2].bits1),  count1_bits[q/2].length);
		}
	}
	if(debug) printf("Finally, count1 t%d\n", count1_bits[final_possibility_big_quants / 2].table1);

	if(debug) {
		fflush(stdout);
		printf("DOING THINGS\n");
		fflush(stdout);
	}

/*
	if(debug) {
		for(i = 0; i < 256; i++) {
			print_table_vec(quant_bits + i);
		}
		for(q = 0; q < MAX_QUANTS / 2 + 1; q++) {
			printf("%3d: Table %d, %3d bits, %d values\n", q * 2, count1_bits[q].table1, not_too_many(count1_bits[q].bits1), count1_bits[q].length);
		}
	}
	print_table_vec(&max_quant_sub15);
*/

//	if(debug) flush(stdout);

//	out_val = Val_int(0);
	CAMLreturn(out_val);
}

#else // WIN32

CAMLprim value mfu_find_best_config_sse41(
	value quant_bits_ptr,
	value quant_bits_count1_char_ptr,
	value scf_bands_ptr,
	value quant_raw_ptr,
	value debug_val
) {
	return mfu_find_best_config(quant_bits_ptr, quant_bits_count1_char_ptr, scf_bands_ptr, quant_raw_ptr, debug_val);
}

#endif // WIN32
