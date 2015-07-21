//#//include <malloc.h>

// Must be defined in this file???
#define _XOPEN_SOURCE 500

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/custom.h>

#include <caml/unixsupport.h>

#include "ptr.h"


//#pragma message ( "BLOCKING SECTION OVERRIDDEN ptr-c.c" )
//#define enter_blocking_section()
//#define leave_blocking_section()

#ifdef _WIN32
#define HAS_BYTESWAP 1
#endif


/*
 * This is a new style for ptrs: the custom ptr points to a ptr_struct struct
 */

/*
 * Windows seems to like aligning to 16 byte values
 * (Modern Linuxes should be at least 8 since malloc() says the alignment is suitable for any variable)
 * This function will ignore alignment if the value is 1, 2, 4, 8, or 16.
 * It does not check the malloc for NULL, so the output of this function must be checked by the caller!
 * If the malloc alignment does not match then it returns NULL
 */
void *attempt_aligned_alloc(intnat length, intnat align) {
	void *ret;
	intnat align_minus_1 = align - 1;
	if(
		((align | 0x1F) == 0x1F) &&    /* Makes sure the value is somewhere between 0 and 31 */
		((align & align_minus_1) == 0) /* Silly way of checking for a power of 2 */
	) {
		// We can try doing a regular malloc
//		printf("  Mallocing %lld to %lld\n", length, align);
		ret = malloc(length);
//		printf("  Malloced to %016llX & %016llX = %016llX\n", (intnat)ret, align_minus_1, (intnat)ret & align_minus_1);
		if(((intnat)ret & align_minus_1) == 0) {
			// Success: all the lower bits are 0
//			printf("S");
			return ret;
		} else {
			// Try again
//			printf("f");
			free(ret);
			return NULL;
		}
	} else {
		// Can't use this
//		printf("  align not valid (%d) (%d)\n", align | 0x3F, align & align_minus_1);
		return NULL;
	}
}


CAMLprim value ptr_make(value length_val, value align_val) {
	CAMLparam2(length_val, align_val);
	char *begin;
	char *alloc_begin;
	struct ptr_struct *p;
	intnat length = Long_val(length_val);
	intnat align = (Long_val(align_val) > 0 ? Long_val(align_val) : 1);
	CAMLlocal1(cust);

//	printf("Attempt...\n");
	alloc_begin = (char *)attempt_aligned_alloc(length, align);
	if(alloc_begin == NULL) {
		// Aligned alloc failed for some reason; do a regular padded alloc
		alloc_begin = (char *)malloc(length + align - 1);
		if(alloc_begin == NULL) raise_out_of_memory();

		begin = (char *)((((size_t)alloc_begin + align - 1) / align) * align);
	} else {
		// Aligned alloc succeeded! The allocated range is exactly the returned range
//		printf("  other alloc OK\n");
		begin = alloc_begin;
	}
	cust = caml_alloc_custom(&generic_ptr_opts, sizeof(struct ptr_struct), length, 64 * 1024 * 1024);
//	cust = caml_alloc_custom(&generic_ptr_opts, sizeof(struct ptr_struct), 1, 64);
	p = Struct_val(cust);

//	printf("Just MALLOC'd %lld bytes\n", length);
//	fflush(stdout);

	p->begin = begin;
	p->alloc_begin = alloc_begin;
	p->length = length;
	p->align = align;
	p->type = PTR_MALLOC;
//	printf("  Ptr returning value %p,%p length %lld align %lld\n", p->alloc_begin, p->begin, p->length, p->align);
	CAMLreturn(cust);
}


value ptr_get_page_size() {
#ifdef _WIN32
	SYSTEM_INFO si;
	GetSystemInfo(&si);
	return(Val_long(si.dwPageSize));
#else
	return(Val_long(getpagesize()));
#endif
}
/*
 * This function does not check whether the length is a multiple of the page size.
 * The first argument is NOT used to compute anything; it is only used for passing to the align field.
 * It MUST be set to the system's memory page size
 * The only reason it exists is that I don't know how to make a global value in C from a function
 */
CAMLprim value ptr_make_virtual_alloc(value page_size_val, value length_val) {
#if _WIN32
	CAMLparam2(page_size_val, length_val);
	char *begin;
	struct ptr_struct *p;
	intnat length = Long_val(length_val);
	CAMLlocal1(cust);

	begin = (char *)VirtualAlloc(NULL, length, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
	if(begin == NULL) raise_out_of_memory();

	cust = caml_alloc_custom(&generic_ptr_opts, sizeof(struct ptr_struct), length, 64 * 1024 * 1024);
	p = Struct_val(cust);

	p->begin = begin;
	p->alloc_begin = begin;
	p->length = length;
	p->align = Long_val(page_size_val);
	p->type = PTR_VIRTUALALLOC;

	CAMLreturn(cust);
#else
	// Fake it in Linux
	return(ptr_make(length_val, page_size_val));
#endif
}

CAMLprim value ptr_length(value ptr_val) {
	return(Val_long(Length_val(ptr_val)));
}
CAMLprim value ptr_align(value ptr_val) {
	return(Val_int(Align_val(ptr_val)));
}


/********/
/* BLIT */
/********/

CAMLprim void ptr_clear(value ptr_val) {
//	CAMLparam1(ptr_val);
	char *ptr = Begin_val(ptr_val);
	intnat len = Length_val(ptr_val);
	memset(ptr, '\0', len);
//	CAMLreturn0;
}

CAMLprim void ptr_blit(value from_val, value from_off_val, value to_val, value to_off_val, value len_val) {
//	CAMLparam5(from_val, from_off_val, to_val, to_off_val, len_val);
	char *from = Begin_val(from_val) + Long_val(from_off_val);
	char *to = Begin_val(to_val) + Long_val(to_off_val);
	size_t l = Long_val(len_val);
//	printf("BLITTING %lld bytes from %p to %p\n", l, from, to);
	memmove(to, from, l);
//	CAMLreturn0;
}

CAMLprim void ptr_blit_from_string(value str_val, value str_off_val, value ptr_val, value ptr_off_val, value len_val) {
//	CAMLparam5(str_val, str_off_val, ptr_val, ptr_off_val, len_val);
	char *s = String_val(str_val) + Long_val(str_off_val);
	char *p = Begin_val(ptr_val) + Long_val(ptr_off_val);
	size_t l = Long_val(len_val);
	memmove(p, s, l);
//	CAMLreturn0;
}

CAMLprim void ptr_blit_to_string(value ptr_val, value ptr_off_val, value str_val, value str_off_val, value len_val) {
//	CAMLparam5(ptr_val, ptr_off_val, str_val, str_off_val, len_val);
	char *p = Begin_val(ptr_val) + Long_val(ptr_off_val);
	char *s = String_val(str_val) + Long_val(str_off_val);
	size_t l = Long_val(len_val);
//	printf("Moving %d from %p to %p\n", l, p, s);
	memmove(s, p, l);
//	CAMLreturn0;
}


/**************/
/* SMALL COPY */
/**************/

/* PUT */
CAMLprim void ptr_put_8_of_int(value ptr_val, value offset_val, value put_val) {
//	CAMLparam3(ptr_val, offset_val, put_val);
	char *loc = Begin_val(ptr_val) + Long_val(offset_val);
	*loc = Int_val(put_val);
//	CAMLreturn0;
}
CAMLprim void ptr_put_16_of_int(value ptr_val, value offset_val, value put_val) {
//	CAMLparam3(ptr_val, offset_val, put_val);
	int16_t *loc = (int16_t *)(Begin_val(ptr_val) + Long_val(offset_val));
	*loc = Int_val(put_val);
//	CAMLreturn0;
}
CAMLprim void ptr_put_32_of_int(value ptr_val, value offset_val, value put_val) {
//	CAMLparam3(ptr_val, offset_val, put_val);
	int32 *loc = (int32 *)(Begin_val(ptr_val) + Long_val(offset_val));
	*loc = Int_val(put_val);
//	CAMLreturn0;
}
CAMLprim void ptr_put_64_of_int(value ptr_val, value offset_val, value put_val) {
//	CAMLparam3(ptr_val, offset_val, put_val);
	int64 *loc = (int64 *)(Begin_val(ptr_val) + Long_val(offset_val));
	*loc = Long_val(put_val);
//	CAMLreturn0;
}
CAMLprim void ptr_put_64_of_int64(value ptr_val, value offset_val, value put_val) {
//	CAMLparam3(ptr_val, offset_val, put_val);
	int64 *loc = (int64 *)(Begin_val(ptr_val) + Long_val(offset_val));
	*loc = Int64_val(put_val);
//	CAMLreturn0;
}
CAMLprim void ptr_put_32_of_float(value ptr_val, value offset_val, value put_val) {
//	CAMLparam3(ptr_val, offset_val, put_val);
	float *loc = (float *)(Begin_val(ptr_val) + Long_val(offset_val));
	*loc = Double_val(put_val);
//	CAMLreturn0;
}
CAMLprim void ptr_put_64_of_float(value ptr_val, value offset_val, value put_val) {
//	CAMLparam3(ptr_val, offset_val, put_val);
	double *loc = (double *)(Begin_val(ptr_val) + Long_val(offset_val));
	*loc = Double_val(put_val);
//	CAMLreturn0;
}


/* GET */
CAMLprim value ptr_get_int_of_8(value ptr_val, value offset_val) {
//	CAMLparam2(ptr_val, offset_val);
	char *loc = Begin_val(ptr_val) + Long_val(offset_val);
//	CAMLreturn(Val_int(*loc));
	return(Val_int(*loc));
}
CAMLprim value ptr_get_int_of_8u(value ptr_val, value offset_val) {
//	CAMLparam2(ptr_val, offset_val);
	unsigned char *loc = (unsigned char *)(Begin_val(ptr_val) + Long_val(offset_val));
//	CAMLreturn(Val_int(*loc));
	return(Val_int(*loc));
}
CAMLprim value ptr_get_int_of_16(value ptr_val, value offset_val) {
//	CAMLparam2(ptr_val, offset_val);
	int16_t *loc = (int16_t *)(Begin_val(ptr_val) + Long_val(offset_val));
//	CAMLreturn(Val_int(*loc));
	return(Val_int(*loc));
}
CAMLprim value ptr_get_int_of_16u(value ptr_val, value offset_val) {
//	CAMLparam2(ptr_val, offset_val);
	uint16_t *loc = (uint16_t *)(Begin_val(ptr_val) + Long_val(offset_val));
//	CAMLreturn(Val_int(*loc));
	return(Val_int(*loc));
}
CAMLprim value ptr_get_int_of_32(value ptr_val, value offset_val) {
//	CAMLparam2(ptr_val, offset_val);
	int32 *loc = (int32 *)(Begin_val(ptr_val) + Long_val(offset_val));
//	CAMLreturn(Val_int(*loc));
	return(Val_int(*loc));
}
CAMLprim value ptr_get_int_of_32u(value ptr_val, value offset_val) {
//	CAMLparam2(ptr_val, offset_val);
	uint32 *loc = (uint32 *)(Begin_val(ptr_val) + Long_val(offset_val));
//	CAMLreturn(Val_int(*loc));
	return(Val_int(*loc));
}
CAMLprim value ptr_get_int_of_64(value ptr_val, value offset_val) {
//	CAMLparam2(ptr_val, offset_val);
	int64 *loc = (int64 *)(Begin_val(ptr_val) + Long_val(offset_val));
//	CAMLreturn(Val_int(*loc));
	return(Val_int(*loc));
}
CAMLprim value ptr_get_int_of_64u(value ptr_val, value offset_val) {
//	CAMLparam2(ptr_val, offset_val);
	uint64 *loc = (uint64 *)(Begin_val(ptr_val) + Long_val(offset_val));
//	CAMLreturn(Val_int(*loc));
	return(Val_int(*loc));
}
CAMLprim value ptr_get_int64_of_64(value ptr_val, value offset_val) {
	CAMLparam2(ptr_val, offset_val);
	int64 *loc = (int64 *)(Begin_val(ptr_val) + Long_val(offset_val));
	CAMLlocal1(out_val);
	out_val = caml_copy_int64(*loc);
	CAMLreturn(out_val);
}
CAMLprim value ptr_get_float_of_32(value ptr_val, value offset_val) {
	CAMLparam2(ptr_val, offset_val);
	float *loc = (float *)(Begin_val(ptr_val) + Long_val(offset_val));
	CAMLlocal1(out_val);
	out_val = caml_copy_double(*loc);
	CAMLreturn(out_val);
}
CAMLprim value ptr_get_float_of_64(value ptr_val, value offset_val) {
	CAMLparam2(ptr_val, offset_val);
	double *loc = (double *)(Begin_val(ptr_val) + Long_val(offset_val));
	CAMLlocal1(out_val);
	out_val = caml_copy_double(*loc);
	CAMLreturn(out_val);
}

// Byte swap functions
// Apparently CAMLparamX is only needed if a heap allocation takes place before a heap access
CAMLprim void ptr_put_16_of_int_bswap(value ptr_val, value offset_val, value put_val) {
	uint16_t *loc = (uint16_t *)(Begin_val(ptr_val) + Long_val(offset_val));
	uint16_t put = Int_val(put_val);
	// It looks like _byteswap_ushort just maps to a 16-bit rotate, so use a more portable version
	// It should map the exact same way
	put = (((put & 0x00FF) << 8) | ((put & 0xFF00) >> 8));
	*loc = put;
}

CAMLprim void ptr_put_32_of_int_bswap(value ptr_val, value offset_val, value put_val) {
	uint32 *loc = (uint32 *)(Begin_val(ptr_val) + Long_val(offset_val));
	uint32 put = Int_val(put_val);
#if HAS_BYTESWAP
	put = _byteswap_ulong(put);
#else
	put = (((put & 0x0000FFFF) << 16) | ((put & 0xFFFF0000) >> 16));
	put = (((put & 0x00FF00FF) <<  8) | ((put & 0xFF00FF00) >>  8));
#endif
	*loc = put;
}

// bswap only works on ints, so we have to go double -> float -> int
CAMLprim void ptr_put_32_of_float_bswap(value ptr_val, value offset_val, value put_val) {
	uint32 *loc = (uint32 *)(Begin_val(ptr_val) + Long_val(offset_val));
	float put_float = Double_val(put_val);
	uint32 put = *((uint32 *)(&put_float));
#if HAS_BYTESWAP
	put = _byteswap_ulong(put);
#else
	put = (((put & 0x0000FFFF) << 16) | ((put & 0xFFFF0000) >> 16));
	put = (((put & 0x00FF00FF) <<  8) | ((put & 0xFF00FF00) >>  8));
#endif
	*loc = put;
}

CAMLprim value ptr_get_int_of_32u_bswap(value ptr_val, value offset_val) {
	uint32 *loc = (uint32 *)(Begin_val(ptr_val) + Long_val(offset_val));
	uint32 got = loc[0];
#if HAS_BYTESWAP
	got = _byteswap_ulong(got);
#else
// GCC has __builtin_bswap32, but the assembly doesn't look too much better
	got = (((got & 0x0000FFFF) << 16) | ((got & 0xFFFF0000) >> 16));
	got = (((got & 0x00FF00FF) <<  8) | ((got & 0xFF00FF00) >>  8));
#endif
	return(Val_int(got));
}




/********/
/* MMAP */
/********/
// This is VERY different between Windows and Linux

#ifdef _WIN32
DWORD map_access_array[] = {PAGE_WRITECOPY, PAGE_READONLY, PAGE_READWRITE};
DWORD view_access_array[] = {FILE_MAP_COPY, FILE_MAP_READ, FILE_MAP_WRITE};
#else
int map_protect_array[] = {PROT_READ | PROT_WRITE, PROT_READ, PROT_READ | PROT_WRITE};
int map_flag_array[] = {MAP_PRIVATE, MAP_PRIVATE, MAP_SHARED};
#endif

CAMLprim value ptr_map_handle(value h_val, value from_val, value len_val, value access_val) {
	CAMLparam4(h_val, from_val, len_val, access_val);
#ifdef _WIN32
	HANDLE h;
	HANDLE h_map;
	ULARGE_INTEGER from_large;
	LARGE_INTEGER len_large;
	DWORD map_access = map_access_array[Int_val(access_val)];
	DWORD view_access = view_access_array[Int_val(access_val)];
	CAMLlocal1(cust);

	from_large.QuadPart = Long_val(from_val);
	len_large.QuadPart = Long_val(len_val);

	h = Handle_val(h_val);
	if(len_large.QuadPart == 0) {
		if(!GetFileSizeEx(h, &len_large)) {
			len_large.QuadPart = 0;
		}
	}
	if(len_large.QuadPart == 0) {
		struct ptr_struct *p;
		cust = caml_alloc_custom(&generic_ptr_opts, sizeof(struct ptr_struct), 0, 64*1024*1024);
		p = Struct_val(cust);
		p->begin = NULL;
		p->alloc_begin = NULL;
		p->length = 0;
		p->align = 0;
		p->type = PTR_NULL;
	} else {
	h_map = CreateFileMapping(
		h,
		NULL,
		map_access,
		len_large.HighPart,
		len_large.LowPart,
		NULL
	);
	if(h_map == NULL) {
//		printf("NULL! %d\n", GetLastError());
		caml_failwith("CreateFileMapping failed");
	} else {
		char *map_ptr;
		map_ptr = (char *)MapViewOfFile(
			h_map,
			view_access,
			from_large.HighPart,
			from_large.LowPart,
			len_large.QuadPart
		);
		if(map_ptr == NULL) {
//			printf("2NULL! %d\n", GetLastError());
			caml_failwith("MapViewOfFile failed");
		} else {
			SYSTEM_INFO sysinfo;
			MEMORY_BASIC_INFORMATION info;
			SIZE_T ret;

			GetSystemInfo(&sysinfo);

			CloseHandle(h_map);

			ret = VirtualQuery(
				map_ptr,
				&info,
				sizeof(MEMORY_BASIC_INFORMATION)
			);
			if(ret == 0) {
//				printf("3NULL! %d\n", GetLastError());
				caml_failwith("VirtualQuery failed");
			} else {
				// Now make the ptr
				struct ptr_struct *p;
//				int i;
				cust = caml_alloc_custom(&generic_ptr_opts, sizeof(struct ptr_struct), info.RegionSize, 64*1024*1024);
				p = Struct_val(cust);

				p->begin = map_ptr;
				p->alloc_begin = map_ptr;
				p->length = info.RegionSize;
				p->align = sysinfo.dwPageSize;
				p->type = PTR_MMAP;

				}
			}
		}
	}
	CAMLreturn(cust);
#else // _WIN32 for mmap
//	CAMLparam4(h_val, from_val, len_val, access_val);
	int h = Int_val(h_val);
	off_t from = Long_val(from_val);
	size_t len = Long_val(len_val);
	int prot = map_protect_array[Int_val(access_val)];
	int flags = map_flag_array[Int_val(access_val)];
	char *map_ptr;
	CAMLlocal1(cust);

	if(len == 0) {
		// Windows will map the whole file in this case, which is kind of useful
		off_t start = lseek(h, 0, SEEK_CUR);
		len = (size_t)lseek(h, 0, SEEK_END) - start;
		lseek(h, start, SEEK_SET);
	}

	if(len == 0) {
		struct ptr_struct *p;
		cust = caml_alloc_custom(&generic_ptr_opts, sizeof(struct ptr_struct), 0, 64*1024*1024);
		p = Struct_val(cust);
		p->begin = NULL;
		p->alloc_begin = NULL;
		p->length = 0;
		p->align = 0;
		p->type = PTR_NULL;
	} else {
		map_ptr = (char *)mmap(NULL, len, prot, flags, h, from);
		if(map_ptr == (char *)MAP_FAILED) {
//			printf("Got prot %d, flags %d (wanted %d and %d)\n", prot, flags, PROT_READ, MAP_PRIVATE);
			caml_failwith("MMAP failed");
		} else {
			struct ptr_struct *p;
			cust = caml_alloc_custom(&generic_ptr_opts, sizeof(struct ptr_struct), len, 64*1024*1024);
			p = Struct_val(cust);

			p->begin = map_ptr;
			p->alloc_begin = map_ptr;
			p->length = len;
			p->align = getpagesize();
			p->type = PTR_MMAP;
		}
	}
	CAMLreturn(cust);
#endif // _WIN32 for mmap
}
CAMLprim value ptr_flush_map(value ptr_val) {
	CAMLparam1(ptr_val);
	if(Type_val(ptr_val) == PTR_MMAP) {
#ifdef _WIN32
		CAMLreturn(Val_bool(FlushViewOfFile(
			Alloc_begin_val(ptr_val),
			Length_val(ptr_val)
		)));
#else
		CAMLreturn(Val_bool(msync(Alloc_begin_val(ptr_val), Length_val(ptr_val), MS_SYNC)));
#endif
	} else {
		CAMLreturn(Val_bool(FALSE));
	}
}
CAMLprim void ptr_unmap(value ptr_val) {
	CAMLparam1(ptr_val);
	if(Type_val(ptr_val) == PTR_MMAP) {
#ifdef _WIN32
		UnmapViewOfFile(Alloc_begin_val(ptr_val));
#else
		munmap(Alloc_begin_val(ptr_val), Length_val(ptr_val));
#endif
		// Now invalidate the ptr
		Begin_val(ptr_val) = NULL;
		Alloc_begin_val(ptr_val) = NULL;
		Length_val(ptr_val) = 0;
		Type_val(ptr_val) = PTR_NULL;
	}
	CAMLreturn0;
}
/*
CAMLprim value ptr_create_shared_memory(value name_val) {
	CAMLparam1(name_val);
	CAMLlocal1(h_val);
#ifdef _WIN32
	;
#else
#endif
	CAMLreturn(h_val);
}
*/

/************************/
/* UNIXY FILE FUNCTIONS */
/************************/

#ifdef _WIN32
CAMLprim value ptr_read(value fd, value ptr, value ofs, value len, value really_val, value pos_or_negative_val) {
	CAMLparam5(fd, ptr, ofs, len, really_val);
	CAMLxparam1(pos_or_negative_val);
	DWORD num_bytes;
	DWORD passed_num_bytes;
	DWORD num_read;
	DWORD total_bytes = 0;
	DWORD err = 0;
	int really = Int_val(really_val);
	char *c = Begin_val(ptr) + Long_val(ofs);

	/* overlapping stuff */
	intnat pos_passed = Long_val(pos_or_negative_val);
	OVERLAPPED o;
	LPOVERLAPPED o_star;
	if(pos_passed < 0) {
		o_star = NULL;
	} else {
		o.Internal = 0;
		o.InternalHigh = 0;
		o.Offset = pos_passed;
#ifdef ARCH_SIXTYFOUR
		o.OffsetHigh = (pos_passed >> 32);
#else
		o.OffsetHigh = 0;
#endif
		o.hEvent = NULL;
		o_star = &o;
//		printf("POINTER IS %p\n", o.Pointer);
//		printf("Offest is %08X:%08X\n", o.OffsetHigh, o.Offset);
	}

	num_bytes = passed_num_bytes = Long_val(len);
	if(Descr_kind_val(fd) == KIND_SOCKET) {
		int ret;
		SOCKET s = Socket_val(fd);

		enter_blocking_section();
		do {
			ret = recv(s, c, num_bytes, 0);
			if(ret == SOCKET_ERROR) {
				err = WSAGetLastError();
				break;
			}
			total_bytes += ret; c += ret;
			num_bytes -= ret;
			// Messy, but whatever
			if(really && ret == 0 && num_bytes != 0) {
				err = ERROR_HANDLE_EOF;
				break;
			}
		} while(really && num_bytes > 0);
		leave_blocking_section();

		num_read = ret;
	} else {
		HANDLE h = Handle_val(fd);

		enter_blocking_section();
		do {
			if(!ReadFile(h, c, num_bytes, &num_read, o_star)) {
				err = GetLastError();
				break;
			}
			total_bytes += num_read; c += num_read;
			num_bytes -= num_read;
			// OOPS! I forgot to update the overlapped structure here!
			if(o_star != NULL) {
				o_star->Offset += num_read;
			}
		} while(really && num_bytes > 0);
		leave_blocking_section();

	}
	if(err) {
		win32_maperr(err);
		uerror("Ptr.read", Nothing);
	}
	CAMLreturn(Val_int(total_bytes));
}
#else
CAMLprim value ptr_read(value fd, value ptr, value ofs, value len, value really_val, value pos_or_negative_val) {
	CAMLparam5(fd, ptr, ofs, len, really_val);
	CAMLxparam1(pos_or_negative_val);
	int h = Int_val(fd);
	size_t num_bytes = Long_val(len);
	ssize_t num_read;
	size_t total_bytes = 0;
	int err = 0;
	int really = Int_val(really_val);
	off_t pos_passed = Long_val(pos_or_negative_val);
	char *c = Begin_val(ptr) + Long_val(ofs);

	if(pos_passed < 0) {
		// Read normally
		enter_blocking_section();
		do {
			num_read = read(h, c, num_bytes);
			if(num_read <= 0) {
				err = errno;
				break;
			}
			total_bytes += num_read; c += num_read;
		} while(really && total_bytes < num_bytes);
		leave_blocking_section();
	} else {
		// Use pread to read from a specific point
		enter_blocking_section();
		do {
			num_read = pread(h, c, num_bytes, pos_passed);
			if(num_read <= 0) {
				err = errno;
				break;
			}
			total_bytes += num_read; c += num_read; pos_passed += num_read;
		} while(really && total_bytes < num_bytes);
		leave_blocking_section();
	}
	// Uhhh... I assume uerror uses errno
	// unix_error seems to be the same, but you can specify your own errno
	if(err) {
		unix_error(err, "Ptr.read", Nothing);
	}
	CAMLreturn(Val_int(total_bytes));
}
#endif

CAMLprim value ptr_read_bytecode(value *argv, int argn) {
	return ptr_read(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}


#ifdef _WIN32
CAMLprim value ptr_write(value fd, value ptr, value ofs, value len, value really_val, value pos_or_negative_val) {
	CAMLparam5(fd, ptr, ofs, len, really_val);
	CAMLxparam1(pos_or_negative_val);
	DWORD passed_num_bytes;
	DWORD num_bytes;
	DWORD num_wrote;
	DWORD total_bytes = 0;
	DWORD err = 0;
	int really = Int_val(really_val);
	char *c = Begin_val(ptr) + Long_val(ofs);

	/* Overlap */
	intnat pos_passed = Long_val(pos_or_negative_val);
	OVERLAPPED o;
	LPOVERLAPPED o_star;
	if(pos_passed < 0) {
		o_star = NULL;
	} else {
		o.Internal = 0;
		o.InternalHigh = 0;
		o.Offset = pos_passed;
#ifdef ARCH_SIXTYFOUR
		o.OffsetHigh = (pos_passed >> 32);
#else
		o.OffsetHigh = 0;
#endif
		o.hEvent = NULL;
		o_star = &o;
	}

	num_bytes = passed_num_bytes = Long_val(len);
	if(Descr_kind_val(fd) == KIND_SOCKET) {
		int ret;
		SOCKET s = Socket_val(fd);

		enter_blocking_section();
		do {
			ret = send(s, c, num_bytes, 0);
			if(ret == SOCKET_ERROR) {
				err = WSAGetLastError();
				break;
			}
			// XXX XXX XXX I FORGOT THIS LINE IN OTHER VERSIONS!!! XXX XXX XXX
			total_bytes += ret; c += ret;
			num_bytes -= ret;
			if(really && ret == 0 && total_bytes != num_bytes) {
				// Wrote nothing
				err = ERROR_HANDLE_EOF;
				break;
			}
		} while(really && num_bytes > 0);
		leave_blocking_section();

		num_wrote = ret;
	} else {
		HANDLE h = Handle_val(fd);

		enter_blocking_section();
		do {
			if(!WriteFile(h, c, num_bytes, &num_wrote, o_star)) {
				err = GetLastError();
				break;
			}
			total_bytes += num_wrote; c += num_wrote;
			num_bytes -= num_wrote;
		} while(really && num_bytes > 0);
		leave_blocking_section();

	}
	if(err) {
		win32_maperr(err);
//		printf("FAILED WITH WINDOWS ERROR %d\n", err);
		uerror("Ptr.write", Nothing);
	}
	CAMLreturn(Val_int(total_bytes));
}
#else
CAMLprim value ptr_write(value fd, value ptr, value ofs, value len, value really_val, value pos_or_negative_val) {
	CAMLparam5(fd, ptr, ofs, len, really_val);
	CAMLxparam1(pos_or_negative_val);
	int h = Int_val(fd);
	size_t num_bytes = Long_val(len);
	ssize_t num_write;
	size_t total_bytes = 0;
	int err = 0;
	int really = Int_val(really_val);
	off_t pos_passed = Long_val(pos_or_negative_val);
	char *c = Begin_val(ptr) + Long_val(ofs);

	if(pos_passed < 0) {
		// Write normally
		enter_blocking_section();
		do {
			num_write = write(h, c, num_bytes);
			if(num_write <= 0) {
				err = errno;
				break;
			}
			total_bytes += num_write; c += num_write;
		} while(really && total_bytes < num_bytes);
		leave_blocking_section();
	} else {
		// Use pwrite
		enter_blocking_section();
		do {
			num_write = pwrite(h, c, num_bytes, pos_passed);
			if(num_write <= 0) {
				err = errno;
				break;
			}
			total_bytes += num_write; c += num_write; pos_passed += num_write;
		} while(really && total_bytes < num_bytes);
		leave_blocking_section();
	}
	if(err) {
		unix_error(err, "Ptr.write", Nothing);
	}
	CAMLreturn(Val_int(total_bytes));
}
#endif

CAMLprim value ptr_write_bytecode(value *argv, int argn) {
	return ptr_write(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

#if 0
CAMLprim value make_ptr_caml(value size_val, value align_val) {
	CAMLparam2(size_val, align_val);
	char *actual_pointer;
	char *returned_pointer;
	intnat align = (Int_val(align_val) > 0 ? Int_val(align_val) : 1);
	intnat alloc_size = Long_val(size_val) + sizeof(/*__int32*/size_t) + align - 1;
	CAMLlocal2(cust,tuple);
	actual_pointer = (char *)malloc(alloc_size);

	if(actual_pointer == NULL) raise_out_of_memory();

	returned_pointer = (char *)((((size_t)actual_pointer + sizeof(/*__int32*/size_t) + align - 1) / align) * align);
	((char **)returned_pointer)[-1] = actual_pointer;

	cust = caml_alloc_custom(&generic_ptr_opts, sizeof(char *), Int_val(size_val), 64*1024*1024);
	ptr_custom(cust) = returned_pointer;
	tuple = caml_alloc_tuple(2);
	Store_field(tuple,0,size_val);
	Store_field(tuple,1,cust);
	CAMLreturn(tuple);
}

CAMLprim value length_ptr_caml(value ptr_val) {
	CAMLparam1(ptr_val);
	CAMLreturn(ptr_size(ptr_val));
}

CAMLprim value aligned_to_mask_caml(value mask_val, value ptr_val) {
	CAMLparam2(mask_val, ptr_val);
	char *caml_pointer = ptr_value(ptr_val);
	printf("%016X & %016X %d\n", (size_t)caml_pointer, Long_val(mask_val), ((size_t)caml_pointer & Long_val(mask_val)) == 0);
	CAMLreturn(Val_bool(((size_t)caml_pointer & Long_val(mask_val)) == 0));
}

void ptr_of_string_caml(value str, value ptr) {
	CAMLparam2(str, ptr);
	char *p = ptr_value(ptr);
	char *s = String_val(str);
	size_t l = Long_val(ptr_size(ptr));
//	printf("%p -> %p (%d)\n", s, p, l);
//	fflush(stdout);
	memmove(p, s, l);
	CAMLreturn0;
}

void string_of_ptr_caml(value ptr, value str) {
	CAMLparam2(ptr, str);
	memmove(String_val(str), ptr_value(ptr), string_length(str));
	CAMLreturn0;
}

CAMLprim value ptr_compare_caml(value v1, value v2) {
	CAMLparam2(v1, v2);
	long l1 = Long_val(ptr_size(v1));
	long l2 = Long_val(ptr_size(v2));
	unsigned char *c1 = ptr_value(v1);
	unsigned char *c2 = ptr_value(v2);
	int i;
	int ret = 0;

	enter_blocking_section();
	if(l1 < l2) {
		ret = -1;
	} else if(l1 > l2) {
		ret = 1;
	} else {
		// Same length
		for(i = 0; i < l1; i++) {
			if(*c1 < *c2) {
				ret = -1;
				break;
			} else if(*c1 > *c2) {
				ret = 1;
				break;
			} else {
				c1++;
				c2++;
			}
		}
	}
	leave_blocking_section();

	CAMLreturn(Val_int(ret));
}

CAMLprim value ptr_compare_sub_caml(value v1, value v1_off, value v2, value v2_off, value len_val) {
	CAMLparam5(v1, v1_off, v2, v2_off, len_val);
	long len = Long_val(len_val);
	unsigned char *c1 = ptr_value(v1) + Long_val(v1_off);
	unsigned char *c2 = ptr_value(v2) + Long_val(v2_off);
	int i;
	int ret = 0;

	enter_blocking_section();
	for(i = 0; i < len; i++) {
		if(*c1 < *c2) {
			ret = -1;
			break;
		} else if(*c1 > *c2) {
			ret = 1;
			break;
		} else {
			c1++;
			c2++;
		}
	}
	leave_blocking_section();

	CAMLreturn(Val_int(ret));
}



CAMLprim void blit_caml(value src_val, value src_off_val, value dest_val, value dest_off_val, value len_val) {
	CAMLparam5(src_val, src_off_val, dest_val, dest_off_val, len_val);
	memmove(ptr_value(dest_val) + Int_val(dest_off_val), ptr_value(src_val) + Int_val(src_off_val), Int_val(len_val));
	CAMLreturn0;
}

CAMLprim void blit_from_string_caml(value str_val, value str_off_val, value ptr_val, value ptr_off_val, value len_val) {
	CAMLparam5(str_val, str_off_val, ptr_val, ptr_off_val, len_val);
	memmove(ptr_value(ptr_val) + Int_val(ptr_off_val), String_val(str_val) + Int_val(str_off_val), Int_val(len_val));
	CAMLreturn0;
}


CAMLprim void blit_to_string_caml(value ptr_val, value ptr_off_val, value str_val, value str_off_val, value len_val) {
	CAMLparam5(str_val, str_off_val, ptr_val, ptr_off_val, len_val);
	memmove(String_val(str_val) + Int_val(str_off_val), ptr_value(ptr_val) + Int_val(ptr_off_val), Int_val(len_val));
	CAMLreturn0;
}

/**************/
/* SMALL COPY */
/**************/

CAMLprim void put_int_to_32_caml(value ptr_val, value offset_val, value put_val) {
	CAMLparam3(ptr_val, offset_val, put_val);
	int32 *loc = (int32 *)(ptr_value(ptr_val) + Int_val(offset_val));
	*loc = Int_val(put_val);
	CAMLreturn0;
}

CAMLprim void put_int_to_64_caml(value ptr_val, value offset_val, value put_val) {
	CAMLparam3(ptr_val, offset_val, put_val);
	int64 *loc = (int64 *)(ptr_value(ptr_val) + Int_val(offset_val));
	*loc = Long_val(put_val);
	CAMLreturn0;
}

CAMLprim value get_32_to_int_caml(value ptr_val, value offset_val) {
	CAMLparam2(ptr_val, offset_val);
	int32 *loc = (int32 *)(ptr_value(ptr_val) + Int_val(offset_val));
	CAMLreturn(Val_int(*loc));
}

CAMLprim value get_64_to_int_caml(value ptr_val, value offset_val) {
	CAMLparam2(ptr_val, offset_val);
	int64 *loc = (int64 *)(ptr_value(ptr_val) + Int_val(offset_val));
	CAMLreturn(Val_int(*loc));
}

CAMLprim void put_int64_caml(value ptr_val, value offset_val, value int64_val) {
	CAMLparam3(ptr_val, offset_val, int64_val);
	int64 *loc = (int64 *)(ptr_value(ptr_val) + Int_val(offset_val));
	*loc = Int64_val(int64_val);
	CAMLreturn0;
}

CAMLprim value get_int64_caml(value ptr_val, value offset_val) {
	CAMLparam2(ptr_val, offset_val);
	int64 *loc;
	CAMLlocal1(out_val);

	loc = (int64 *)(ptr_value(ptr_val) + Int_val(offset_val));
	out_val = caml_copy_int64(*loc);
	CAMLreturn(out_val);
}

CAMLprim void put_int32_caml(value ptr_val, value offset_val, value int32_val) {
	CAMLparam3(ptr_val, offset_val, int32_val);
	int32 *loc = (int32 *)(ptr_value(ptr_val) + Int_val(offset_val));
	*loc = Int64_val(int32_val);
	CAMLreturn0;
}

CAMLprim value get_int32_caml(value ptr_val, value offset_val) {
	CAMLparam2(ptr_val, offset_val);
	int32 *loc;
	CAMLlocal1(out_val);

	loc = (int32 *)(ptr_value(ptr_val) + Int_val(offset_val));
	out_val = caml_copy_int32(*loc);
	CAMLreturn(out_val);
}

CAMLprim void put_int_to_16_caml(value ptr_val, value off_val, value put_val) {
	CAMLparam3(ptr_val, off_val, put_val);
	int16_t *loc = (int16_t *)(ptr_value(ptr_val) + Int_val(off_val));
	*loc = (int16_t)Int_val(put_val);
	CAMLreturn0;
}
CAMLprim value get_16_to_int_caml(value ptr_val, value off_val) {
	CAMLparam2(ptr_val, off_val);
	int16_t *loc = (int16_t *)(ptr_value(ptr_val) + Int_val(off_val));
	CAMLreturn(Val_int(*loc));
}

CAMLprim void put_int_to_8_caml(value ptr_val, value offset_val, value put_val) {
	CAMLparam3(ptr_val, offset_val, put_val);
	unsigned char *loc = (unsigned char *)(ptr_value(ptr_val) + Int_val(offset_val));
	*loc = (unsigned char)(Int_val(put_val) & 0xFF);
	CAMLreturn0;
}

CAMLprim value get_8_to_int_caml(value ptr_val, value offset_val) {
	CAMLparam2(ptr_val, offset_val);
	unsigned char *loc = (unsigned char *)(ptr_value(ptr_val) + Int_val(offset_val));
	CAMLreturn(Val_int(*loc));
}



/* byte-swap */
/* Is __inline only available in MSVC? */
/* Note that these functions should NOT be used to store an OCaml int, since the lowest bit will be chopped off */
static __inline unsigned short bs16(unsigned short x) {
	return ((x >> 8) | (x << 8));
}
static __inline uint32 bs32(uint32 x) {
	return ((bs16(x & 0xFFFF) << 16) | (bs16(x >> 16)));
}
static __inline uint64 bs64(uint64 x) {
	return (((uint64)bs32(x & 0xFFFFFFFF) << 32) | (bs32(x >> 32)));
}

CAMLprim void put_int64_bs_caml(value ptr_val, value offset_val, value int64_val) {
	CAMLparam3(ptr_val, offset_val, int64_val);
	uint64 *loc = (uint64 *)(ptr_value(ptr_val) + Int_val(offset_val));
	*loc = bs64(Int64_val(int64_val));
//	printf("P: %p\n", bs64(Int64_val(int64_val)));

	CAMLreturn0;
}

CAMLprim value get_int64_bs_caml(value ptr_val, value offset_val) {
	CAMLparam2(ptr_val, offset_val);
	CAMLlocal1(out_val);
	uint64 *loc = (uint64 *)(ptr_value(ptr_val) + Int_val(offset_val));

	out_val = caml_copy_int64(bs64(*loc));

	CAMLreturn(out_val);
}

CAMLprim void put_int_to_64_bs_caml(value ptr_val, value offset_val, value int_val) {
	CAMLparam3(ptr_val, offset_val, int_val);
	uint64 *loc = (uint64 *)(ptr_value(ptr_val) + Int_val(offset_val));
	*loc = bs64((int64)Long_val(int_val));
	CAMLreturn0;
}

/* Specific layout */
CAMLprim value is_big_endian_caml(value muffins) {
	CAMLparam1(muffins);
#ifdef ARCH_BIG_ENDIAN
	CAMLreturn(Val_int(1));
#else
	CAMLreturn(Val_int(0));
#endif
}





/************************/
/* UNIXY FILE FUNCTIONS */
/************************/

#ifdef _WIN32
CAMLprim value ptr_read_caml(value fd, value ptr, value ofs, value len) {
	CAMLparam4(fd, ptr, ofs, len);
	DWORD numbytes;
	DWORD numread;
	DWORD err = 0;
	char *c = ptr_value(ptr) + Long_val(ofs);
	numbytes = Long_val(len);
	if(Descr_kind_val(fd) == KIND_SOCKET) {
		int ret;
		SOCKET s = Socket_val(fd);
		enter_blocking_section();
		ret = recv(s, c, numbytes, 0);
		if(ret == SOCKET_ERROR) {
			err = WSAGetLastError();
		}
		leave_blocking_section();
		numread = ret;
	} else {
		HANDLE h = Handle_val(fd);
		enter_blocking_section();
		if(!ReadFile(h, c, numbytes, &numread, NULL)) {
			err = GetLastError();
		}
		leave_blocking_section();
	}
	if(err) {
		win32_maperr(err);
		uerror("Ptr.read", Nothing);
	}
	CAMLreturn(Val_int(numread));
}
#else
CAMLprim value ptr_read_caml(value fd, value ptr, value ofs, value len) {
	CAMLparam4(fd, ptr, ofs, len);
	long numbytes;
	int ret;
	char *c = ptr_value(ptr) + Long_val(ofs);
	numbytes = Long_val(len);
	enter_blocking_section();
	ret = read(Int_val(fd), c, (int)numbytes);
	leave_blocking_section();
	if(ret == -1) {
		uerror("Ptr.read", Nothing);
	}
	CAMLreturn(Val_int(ret));
}
#endif

#ifdef _WIN32
CAMLprim value ptr_write_caml(value fd, value ptr, value ofs, value len) {
	CAMLparam4(fd, ptr, ofs, len);
	DWORD numbytes;
	DWORD numwrote;
	DWORD err = 0;
	char *c = ptr_value(ptr) + Long_val(ofs);
	numbytes = Long_val(len);
	if(Descr_kind_val(fd) == KIND_SOCKET) {
		int ret;
		SOCKET s = Socket_val(fd);
		enter_blocking_section();
		ret = send(s, c, numbytes, 0);
		if(ret == SOCKET_ERROR) {
			err = WSAGetLastError();
		}
		leave_blocking_section();
		numwrote = ret;
	} else {
		HANDLE h = Handle_val(fd);
		enter_blocking_section();
		if(!WriteFile(h, c, numbytes, &numwrote, NULL)) {
			err = GetLastError();
		}
		leave_blocking_section();
	}
	if(err) {
		win32_maperr(err);
//		printf("ptr_write_caml got error %d\n", err);
		uerror("Ptr.write", Nothing);
	}
	CAMLreturn(Val_int(numwrote));
}
#else
CAMLprim value ptr_write_caml(value fd, value ptr, value ofs, value len) {
	CAMLparam4(fd, ptr, ofs, len);
	long numbytes;
	int ret;
	char *c = ptr_value(ptr) + Long_val(ofs);
	numbytes = Long_val(len);
	enter_blocking_section();
	ret = write(Int_val(fd), c, (int)numbytes);
	leave_blocking_section();
	if(ret == -1) {
		uerror("Ptr.write", Nothing);
	}
	CAMLreturn(Val_int(ret));
}
#endif

#endif
