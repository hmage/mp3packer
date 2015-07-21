(*******************************************************************************
	This file is a part of mp3packer.

	mp3packer is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.

	mp3packer is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with mp3packer; if not, write to the Free Software
	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*******************************************************************************)

open Types;;
open Mp3read;;
open Pack;;

(*
let t1_ref = ref (counter ());;
let printf f =
	Printf.ksprintf (fun s ->
		let t = counter () in
		let t_delta = t - !t1_ref in
		t1_ref := t;
		Printf.printf "%10d %s" t_delta s
	) f
;;
*)


let make_xing xing header_and_side_info =
	let first_out = Ptr.make 8 0 in
	Ptr.blit_from_string xing.xingTagType 0 first_out 0 4;
	(* frames, bytes, toc, quality *)
	let (ptrref_after_xingNumFrames, num_frames_flag) = (match xing.xingNumFrames with
		| None -> (Ptr.Ref.null, 0x00)
		| Some x -> (
			let p = Ptr.make 4 0 in
			Ptr.put_32_of_int_bswap p 0 x;
			(Ptr.Ref.append Ptr.Ref.null (Ptr.Ref.of_ptr p), 0x01)
		)
	) in
	let (ptrref_after_xingNumBytes, num_bytes_flag) = (match xing.xingNumBytes with
		| None -> (ptrref_after_xingNumFrames, 0x00)
		| Some x -> (
			let p = Ptr.make 4 0 in
			Ptr.put_32_of_int_bswap p 0 x;
			(Ptr.Ref.append ptrref_after_xingNumFrames (Ptr.Ref.of_ptr p), 0x02)
		)
	) in
	let (ptrref_after_xingTOC, toc_flag) = (match xing.xingTOC with
		| None -> (ptrref_after_xingNumBytes, 0x00)
		| Some x -> (
			let toc_ptr = Ptr.make (Array.length x) 0 in
			Array.iteri (fun i c -> Ptr.put_8_of_int toc_ptr i c) x;
			(Ptr.Ref.append ptrref_after_xingNumBytes (Ptr.Ref.of_ptr toc_ptr), 0x04)
		)
	) in
	let (ptrref_after_xingQuality, quality_flag) = (match xing.xingQuality with
		| None -> (ptrref_after_xingTOC, 0x00)
		| Some x -> (
			let p = Ptr.make 4 0 in
			Ptr.put_32_of_int_bswap p 0 x;
			(Ptr.Ref.append ptrref_after_xingTOC (Ptr.Ref.of_ptr p), 0x08)
		)
	) in

(*	Ptr.put_8_of_int first_out 7 (num_frames_flag lor num_bytes_flag lor toc_flag lor quality_flag);*)
	Ptr.put_32_of_int_bswap first_out 4 (num_frames_flag lor num_bytes_flag lor toc_flag lor quality_flag);
(*	first_out_str.[7] <- Char.chr (num_frames_flag lor num_bytes_flag lor toc_flag lor quality_flag);*)
	let ptrref_after_xing = Ptr.Ref.append (Ptr.Ref.of_ptr first_out) ptrref_after_xingQuality in
	let after_lame = (
		match xing.xingLame with
		| None -> (
			let encoder = Ptr.clearret (Ptr.make 20 0) in
			Ptr.blit_from_string xing.xingEncoder 0 encoder 0 (min (Ptr.length encoder) (String.length xing.xingEncoder));
(*			let encoder_string = String.sub (xing.xingEncoder ^ (String.make 20 '\x00')) 0 20 in*)
			Ptr.Ref.append ptrref_after_xing (Ptr.Ref.of_ptr encoder);
		)
		| Some l -> (
			let encoder = Ptr.clearret (Ptr.make 9 0) in
			Ptr.blit_from_string xing.xingEncoder 0 encoder 0 (min (Ptr.length encoder) (String.length xing.xingEncoder));
			let ptrref_after_encoder = Ptr.Ref.append ptrref_after_xing (Ptr.Ref.of_ptr encoder) in

			let lame = Ptr.make 25 0 in
			let p = Ptr.put_bits lame in
			p   0  4 l.lameRevision;
			p   4  4 l.lameVBRMethod;
			p   8  8 l.lameLowpass;
			Ptr.put_32_of_float_bswap lame 2 l.lamePeakAmplitude;
			p  48 16 l.lameRGTrack;
			p  64 16 l.lameRGAlbum;
			p  80  1 (if l.lameNoGapPrev   then 1 else 0);
			p  81  1 (if l.lameNoGapNext   then 1 else 0);
			p  82  1 (if l.lameNSSafeJoint then 1 else 0);
			p  83  1 (if l.lameNSPsyTune   then 1 else 0);
			p  84  4 l.lameATHType;
			p  88  8 l.lameABRBitrate;
			p  96 12 l.lameDelayStart;
			p 108 12 l.lameDelayEnd;
			p 120  2 l.lameSourceFrequency;
			p 122  1 (if l.lameUnwise then 1 else 0);
			p 123  3 l.lameStereoMode;
			p 126  2 l.lameNoiseShaping;
			p 128  8 l.lameMP3Gain;
			p 136  2 0; (* Unused *)
			p 138  3 l.lameSurround;
			p 141 11 l.lamePreset;
			p 152  2 0; (* Only 30 bits used *)
			p 154 30 l.lameMusicLength;
			p 184 16 l.lameMusicCRC;

			let ptrref_after_lame = Ptr.Ref.append ptrref_after_encoder (Ptr.Ref.of_ptr lame) in
			let ptrref_before_crc = Ptr.Ref.append header_and_side_info ptrref_after_lame in
			let crc = Crc.lame_create_ptrref ptrref_before_crc 0 in
			let crc_p = Ptr.make 2 0 in
			Ptr.put_16_of_int_bswap crc_p 0 crc;
			Ptr.Ref.append ptrref_after_lame (Ptr.Ref.of_ptr crc_p);
		)
	) in
	after_lame
;;


let bit_blit =
	let rec b s1 o1 s2 o2 l = (
		if l > 30 then (
			packBits s2 o2 30 (unpackBits s1 o1 30);
			b s1 (o1 + 30) s2 (o2 + 30) (l - 30)
		) else (
			packBits s2 o2 l (unpackBits s1 o1 l);
		)
	) in
	fun s1 o1 s2 o2 l -> (
		if l < 0 || o1 < 0 || o2 < 0 || o1 + l > String.length s1 lsl 3 || o2 + l > String.length s2 lsl 3 then (
			invalid_arg "bit_blit"
		) else (
			b s1 o1 s2 o2 l
		)
	)
;;

let side_info_find_ok :
	type id chan. bool -> (id,chan) side_t -> Ptr.Ref.ref_t -> int -> (id,chan) side_t * Ptr.Ref.ref_t * bool
= fun zero_whole_bad_frame -> function
	| {side_bits = Bits_1_mono (a,b)} as input_side -> (fun reservoir input_offset ->
		let first_bit = input_offset lsl 3 in (* The offset in bits *)
		let second_bit = first_bit + a in     (* The first bit of the second granule *)
		let last_bit = second_bit + b in      (* The first bit after the second granule *)
		let reservoir_length_in_bits = Ptr.Ref.length reservoir lsl 3 in

		let (first_granule_ok, second_granule_ok) = (
			(* The first granule's OK if there's no data (always known) or if the first and second bits are in the string *)
			let fgok = (first_bit = second_bit || (first_bit >= 0 && second_bit <= reservoir_length_in_bits)) in
			(* Ditto for second granule *)
			let sgok = (second_bit = last_bit || (second_bit >= 0 && last_bit <= reservoir_length_in_bits)) in
			(* If zero_whole_bad_frame is set, then if one granule is bad count them both as bad *)
			if zero_whole_bad_frame then (fgok && sgok, fgok && sgok) else (fgok, sgok)
		) in

		let first_granule_bits = if first_granule_ok then max 0 a else 0 in
		let second_granule_bits = if second_granule_ok then max 0 b else 0 in

(*		Printf.printf "BOOGA OK? %B %B\n" first_granule_ok second_granule_ok;*)

		(* The beginning of the valid data *)
		(* (the end of the valid data is output_offset + first_granule_bits + second_granule_bits) *)
		(* If neither granule is good, set to 0 rather than last_bit since last_bit may result in a substring off the end of the string, but with length 0 *)
		let output_offset_bits = if first_granule_ok then first_bit else if second_granule_ok then second_bit else 0 in

		let output_raw = if first_granule_ok && second_granule_ok then (
			input_side.side_raw
		) else (
			let output_raw_ptr = Ptr.Ref.to_ptr input_side.side_raw in
			if not first_granule_ok then (
				(* Zero out the first granule's data *)
				Ptr.put_bits output_raw_ptr  18 30 0;
				Ptr.put_bits output_raw_ptr  48 29 0;
			);
			if not second_granule_ok then (
				Ptr.put_bits output_raw_ptr  77 30 0;
				Ptr.put_bits output_raw_ptr 107 29 0;
			);
			Ptr.Ref.of_ptr output_raw_ptr
		) in
		let output_side = {
			side_raw = output_raw;
			side_offset = 0;
			side_bits = Bits_1_mono (first_granule_bits,second_granule_bits);
			side_bytes = (first_granule_bits + second_granule_bits + 7) asr 3;
		} in
		let output_data = (
			let output_length_bytes = output_side.side_bytes in
			if output_length_bytes = 0 then (
				(* Don't try to get any data if there is no data to get! *)
				Ptr.Ref.null
			) else if output_offset_bits land 7 = 0 then (
				(* Byte-aligned; just sub the ptrref *)
				Ptr.Ref.sub reservoir (output_offset_bits asr 3) output_length_bytes
			) else (
				(* UH-OH! Need to do a bit-blit *)
				let out = String.create output_length_bytes in
				out.[output_length_bytes - 1] <- '\x00';
				let reservoir_string = Ptr.Ref.to_string reservoir in
				bit_blit reservoir_string output_offset_bits out 0 (first_granule_bits + second_granule_bits);

				Ptr.Ref.of_string out
			)
		) in
		(output_side, output_data, first_granule_ok && second_granule_ok)
	)
	| {side_bits = Bits_1_stereo (a,b,c,d)} as input_side -> (fun reservoir input_offset ->
		let first_bit = input_offset lsl 3 in
		let second_bit = first_bit + a + b in
		let last_bit = second_bit + c + d in
		let reservoir_length_in_bits = Ptr.Ref.length reservoir lsl 3 in

		let (first_granule_ok, second_granule_ok) = (
			let fgok = (first_bit = second_bit || (first_bit >= 0 && second_bit <= reservoir_length_in_bits)) in
			let sgok = (second_bit = last_bit || (second_bit >= 0 && last_bit <= reservoir_length_in_bits)) in
			if zero_whole_bad_frame then (fgok && sgok, fgok && sgok) else (fgok, sgok)
		) in

		let new_a = if first_granule_ok then a else 0 in
		let new_b = if first_granule_ok then b else 0 in
		let new_c = if second_granule_ok then c else 0 in
		let new_d = if second_granule_ok then d else 0 in

		let output_offset_bits = if first_granule_ok then max 0 first_bit else if second_granule_ok then max 0 second_bit else 0 in

		let output_raw = if first_granule_ok && second_granule_ok then (
			input_side.side_raw
		) else (
			let output_raw_ptr = Ptr.Ref.to_ptr input_side.side_raw in
			if not first_granule_ok then (
				Ptr.put_bits output_raw_ptr  20 30 0;
				Ptr.put_bits output_raw_ptr  50 29 0;
				Ptr.put_bits output_raw_ptr  79 30 0;
				Ptr.put_bits output_raw_ptr 109 29 0;
			);
			if not second_granule_ok then (
				Ptr.put_bits output_raw_ptr 138 30 0;
				Ptr.put_bits output_raw_ptr 168 29 0;
				Ptr.put_bits output_raw_ptr 197 30 0;
				Ptr.put_bits output_raw_ptr 227 29 0;
			);
			Ptr.Ref.of_ptr output_raw_ptr
		) in
		let output_side = {
			side_raw = output_raw;
			side_offset = 0;
			side_bits = Bits_1_stereo (new_a,new_b,new_c,new_d);
			side_bytes = (new_a + new_b + new_c + new_d + 7) asr 3
		} in
		let output_data = (
			let output_length_bytes = output_side.side_bytes in
			if output_length_bytes = 0 then (
				(* No data here *)
				Ptr.Ref.null
			) else if output_offset_bits land 7 = 0 then (
				(* Sub the ptrref *)
				Ptr.Ref.sub reservoir (output_offset_bits asr 3) output_length_bytes
			) else (
				(* bit-blit! *)
				let out = String.create output_length_bytes in
				(* Zero the last byte so that no random memory junk gets in after the data bits *)
				out.[output_length_bytes - 1] <- '\x00';
				let reservoir_string = Ptr.Ref.to_string reservoir in
				bit_blit reservoir_string output_offset_bits out 0 (new_a + new_b + new_c + new_d);

				Ptr.Ref.of_string out
			)
		) in
		(output_side, output_data, first_granule_ok && second_granule_ok)
	)
	| {side_bits = Bits_2_mono a} as input_side -> (fun reservoir input_offset ->
		let first_bit = input_offset lsl 3 in
		let last_bit = first_bit + a in
		let reservoir_length_in_bits = Ptr.Ref.length reservoir lsl 3 in

		let granule_ok = (first_bit = last_bit || (first_bit >= 0 && last_bit <= reservoir_length_in_bits)) in

		let granule_bits = if granule_ok then max 0 a else 0 in

		let output_offset_bits = if granule_ok then first_bit else 0 in

		let output_raw = if granule_ok then (
			input_side.side_raw
		) else (
			let output_raw_ptr = Ptr.Ref.to_ptr input_side.side_raw in
			Ptr.put_bits output_raw_ptr  9 30 0;
			Ptr.put_bits output_raw_ptr 39 30 0;
			Ptr.put_bits output_raw_ptr 69  3 0;
			Ptr.Ref.of_ptr output_raw_ptr
		) in
		let output_side = {
			side_raw = output_raw;
			side_offset = 0;
			side_bits = Bits_2_mono granule_bits;
			side_bytes = (granule_bits + 7) asr 3;
		} in
		let output_data = (
			let output_length_bytes = output_side.side_bytes in
			if output_length_bytes = 0 then (
				Ptr.Ref.null
			) else if output_offset_bits land 7 = 0 then (
				Ptr.Ref.sub reservoir (output_offset_bits asr 3) output_length_bytes
			) else (
				let out = String.create output_length_bytes in
				out.[output_length_bytes - 1] <- '\x00';
				let reservoir_string = Ptr.Ref.to_string reservoir in
				bit_blit reservoir_string output_offset_bits out 0 granule_bits;

				Ptr.Ref.of_string out
			)
		) in
		(output_side, output_data, granule_ok)
	)
	| {side_bits = Bits_2_stereo (a,b)} as input_side -> (fun reservoir input_offset ->
		let first_bit = input_offset lsl 3 in
		let last_bit = first_bit + a + b in
		let reservoir_length_in_bits = Ptr.Ref.length reservoir lsl 3 in

		let granule_ok = (first_bit = last_bit || (first_bit >= 0 && last_bit <= reservoir_length_in_bits)) in

		let new_a = if granule_ok then max 0 a else 0 in
		let new_b = if granule_ok then max 0 b else 0 in

		let output_offset_bits = if granule_ok then first_bit else 0 in

		let output_raw = if granule_ok then (
			input_side.side_raw
		) else (
			let output_raw_ptr = Ptr.Ref.to_ptr input_side.side_raw in
			Ptr.put_bits output_raw_ptr  10 30 0;
			Ptr.put_bits output_raw_ptr  40 30 0;
			Ptr.put_bits output_raw_ptr  70  3 0;
			Ptr.put_bits output_raw_ptr  73 30 0;
			Ptr.put_bits output_raw_ptr 103 30 0;
			Ptr.put_bits output_raw_ptr 133  3 0;
			Ptr.Ref.of_ptr output_raw_ptr
		) in
		let output_side = {
			side_raw = output_raw;
			side_offset = 0;
			side_bits = Bits_2_stereo (new_a,new_b);
			side_bytes = (new_a + new_b + 7) asr 3;
		} in
		let output_data = (
			let output_length_bytes = output_side.side_bytes in
			if output_length_bytes = 0 then (
				Ptr.Ref.null
			) else if output_offset_bits land 7 = 0 then (
				Ptr.Ref.sub reservoir (output_offset_bits asr 3) output_length_bytes
			) else (
				let out = String.create output_length_bytes in
				out.[output_length_bytes - 1] <- '\x00';
				let reservoir_string = Ptr.Ref.to_string reservoir in
				bit_blit reservoir_string output_offset_bits out 0 (new_a + new_b);

				Ptr.Ref.of_string out
			)
		) in
		(output_side, output_data, granule_ok)
	)
;;



let do_queue recompress_obj state file_state (in_obj : Mp3read.mp3read_ptr_2) out_obj =
	let debug_queue = state.q_debug_queue in
	let min_bitrate = state.q_min_bitrate in
	let delete_beginning_junk = state.q_delete_beginning_junk in
	let delete_end_junk = state.q_delete_end_junk in
	let padding = state.q_padding in
	let recompress = state.q_recompress in
	let zero_whole_bad_frame = state.q_zero_whole_bad_frame in
	let minimize_bit_reservoir = state.q_minimize_bit_reservoir in

	let p = state.q_print_queue in



(*
	(* Number of buffer errors so far *)
	let buffer_errors_ref = ref 0 in
	(* The number of sync errors which occured *)
	let sync_errors_ref = ref 0 in
	(* The number of frame CRC errors *)
	let crc_errors_ref = ref 0 in
	(* The number of errors encountered while recompressing the frames (will be 0 without -z) *)
	let recompress_errors_ref = ref 0 in
*)
	(* Set whether the recompress should warn about frequency overflows *)
(*	let recompress_freq_overflow_warn_ref = ref (not state.q_silent) in*)
	if state.q_silent then ignore file_state#freq_warn;

	let t1 = Unix.gettimeofday () in

(*
	let in_obj = new mp3read_unix ~debug:debug_in in_name in
	let out_obj = new Mp3write.mp3write_unix out_name in
*)

(*
	let out_file = open_out_bin out_name in
	let output_this = output_string out_file in
*)

	(* Sync to first frame *)
	let (new_req, IF_ext first_frame, (first_wanted_at, first_got_at), in_xing_option) = (
		let before_lame_reqs = {
			req_id            = Req_equal;
			req_crc           = Req_any;
			req_bitrate       = Req_any;
			req_samplerate    = Req_equal;
			req_padding       = Req_any;
			req_private       = Req_any;
			req_channel_mode  = Req_any;
			req_channel_count = Req_any;
			req_copyright     = Req_any;
			req_original      = Req_any;
			req_emphasis      = Req_any;
		} in
		let after_lame_reqs = {
			req_id            = Req_equal;
			req_crc           = Req_any;
			req_bitrate       = Req_any;
			req_samplerate    = Req_equal;
			req_padding       = Req_any;
			req_private       = Req_any;
			req_channel_mode  = Req_any;
			req_channel_count = Req_equal;
			req_copyright     = Req_any;
			req_original      = Req_any;   (* Req_equal has had some problems in the past *)
			req_emphasis      = Req_any;
		} in
		let (first_req, IF_ext first_frame, (_ (* 0 *), first_got)) = in_obj#find_next_frame ~force_resync:true ~lame_search:true before_lame_reqs in
		match first_frame.if_xing with
		| None -> (
			(* The first frame was NOT an XING frame; restart and use more strict after_lame_reqs *)
			in_obj#seek first_got;
			let (real_first_req, real_first_frame_ext, (_ (* first_got *), real_first_got)) = in_obj#find_next_frame ~force_resync:true after_lame_reqs in
			(real_first_req, real_first_frame_ext, (first_got, real_first_got), None)
		)
		| Some x -> (
			(* Found an XING frame; do another *)
			let (second_req, second_frame_ext, (second_wanted, second_got)) = in_obj#find_next_frame ~force_resync:true after_lame_reqs in
			(second_req, second_frame_ext, (second_wanted, second_got), Some x)
		)
	) in
	(* Pretend that didn't happen, since it's easier to read the frame at the beginning of the frame loop *)
	in_obj#seek first_got_at;

	let k = first_frame.if_header in (* This replaces the old "k" global setting variable *)
	let side_info_size = Ptr.Ref.length first_frame.if_side_raw in
	let unpadded_frame_length = unpadded_frame_length k.header_samplerate in



	(***********************)
	(* DATA INITIALIZATION *)
	(***********************)

	(* An exact bitrate is taken to be dithered according to the table in Types *)
	(* One more than an exact bitrate is assumed to be always padded *)
	(* Anything larger than the max bitrate will be truncated to a padded max frame *)
	(* Everything else rounds up to the next highest valid bitrate *)
	let (number_to_bitrate, bytes_to_bitrate) = (
		let (max_bitrate, lists) = match k.header_id with
			| MPEG1   -> (320, [(1, 32);(2, 40);(3, 48);(4, 45);(5, 64);(6, 80);(7, 96);(8,112);(9,128);(10,160);(11,192);(12,224);(13,256);(14,320)])
			| MPEG2 _ -> (160, [(1,  8);(2, 16);(3, 24);(4, 32);(5, 40);(6, 48);(7, 56);(8, 64);(9, 80);(10, 96);(11,112);(12,128);(13,144);(14,160)])
		in
		(fun num ->
			let exact = List.exists (fun (_,a) -> a = num) lists in (* Did the caller specify an exact bitrate? *)
			let exactP1 = List.exists (fun (_,a) -> a + 1 = num) lists in (* Did the caller specify 1 more than an exact bitrate? *)
			let over = (num > max_bitrate) in (* Is the caller Way Out There? *)
			let (index,real_bitrate) = try (List.find (fun (_,a) -> num <= a + 1) lists) with Not_found -> (14,max_bitrate) in
			let unpad_length = unpadded_frame_length real_bitrate in
			(fun i ->
				let padded = if exact then (
					padded_frame k.header_samplerate num i
				) else (
					exactP1 || over
				) in
				let pad_add = if padded then 1 else 0 in
				{
					bitrate_num = real_bitrate;
					bitrate_padding = padded;
					bitrate_size = unpad_length + pad_add;
					bitrate_data = unpad_length + pad_add - 4 - side_info_size;
					bitrate_index = index
				}
			)
		), (fun bytes ->
			let bph = bytes + 4 + side_info_size in (* bytes plus header *)
			let out = ref None in
			List.iter (fun (index, real_bitrate) ->
				match !out with
				| Some _ -> ()
				| None -> (
					let bytes_unpadded = unpadded_frame_length real_bitrate in
					if bytes_unpadded >= bph then (
						(* OK without padding *)
						out := Some {
							bitrate_num = real_bitrate;
							bitrate_padding = false;
							bitrate_size = bytes_unpadded;
							bitrate_data = bytes_unpadded - 4 - side_info_size;
							bitrate_index = index
						}
					) else if bytes_unpadded + 1 >= bph then (
						(* Needs padding *)
						out := Some {
							bitrate_num = real_bitrate;
							bitrate_padding = true;
							bitrate_size = bytes_unpadded + 1;
							bitrate_data = bytes_unpadded + 1 - 4 - side_info_size;
							bitrate_index = index
						}
					) (* else keep going *)
				)
			) lists;
			match !out with
			| None -> raise Too_many_bytes (* No valid frame found! *)
			| Some x -> x
		)
	) in

	(* Returns a valid header given the bitrate info, common settings, and stereo mode *)
	let (bitrate_to_header_string, bitrate_to_header) = (
		let template = "\xFF\xFF\xFF\xFF" in
		let pack_head = packBits template in

		pack_head 0 11 0b11111111111;
		pack_head 11 2 (match k.header_id with
			| MPEG1        -> 0b11
			| MPEG2 MPEG20 -> 0b10
			| MPEG2 MPEG25 -> 0b00
		);
		pack_head 13 2 0b01; (* Layer 3 *)
		pack_head 15 1 0b1; (* CRC *)
		pack_head 20 2 (id_index_of_samplerate k.header_samplerate);
		pack_head 23 1 0b0; (* Privates *)
		pack_head 24 2 (match k.header_channel_mode with
			| Stereo Stereo_simple  -> 0b00
			| Stereo Stereo_joint _ -> 0b01
			| Stereo Stereo_dual    -> 0b10
			| Mono                  -> 0b11
		);
		pack_head 28 1 (if k.header_copyright then 1 else 0);
		pack_head 29 1 (if k.header_original then 1 else 0);
		pack_head 30 2 (match k.header_emphasis with
			| Emphasis_none  -> 0b00
			| Emphasis_5015  -> 0b01
			| Emphasis_CCITT -> 0b11
		);

		(
			(fun br ms is -> (
				let out_head = String.copy template in
				let pack_head = packBits out_head in
				pack_head 16 4 br.bitrate_index; (* Bitrate *)
				pack_head 22 1 (if br.bitrate_padding then 1 else 0);
				pack_head 26 1 (if ms then 1 else 0);
				pack_head 27 1 (if is then 1 else 0);
				out_head
			))
		,
			(fun br ms is -> (
				let out_head = Ptr.of_string template in
				let p = Ptr.put_bits out_head in
				p 16 4 br.bitrate_index;
				p 22 1 (if br.bitrate_padding then 1 else 0);
				p 26 1 (if ms then 1 else 0);
				p 27 1 (if is then 1 else 0);
				out_head
			))
		)
	) in
	let min_bitrate_now = number_to_bitrate min_bitrate in

	(* Makes a string out of a given header and bitrate info *)
	(* Always indicates no CRC *)
	let ptrref_of_header_and_bitrate header bitrate =
		let p = Ptr.Ref.to_ptr header.header_raw in
		Ptr.put_bits p 15 1 1;
		Ptr.put_bits p 16 4 bitrate.bitrate_index;
		Ptr.put_bits p 22 1 (if bitrate.bitrate_padding then 1 else 0);
		Ptr.Ref.of_ptr p
	in

	(* Changes a side info string to be the specified padding *)
(*
	let ptrref_of_side_and_offset =
		let bits = match k.header_id with
			| MPEG1 -> 9
			| _ -> 8
		in
		fun side offset -> (
			let new_ptr = Ptr.Ref.to_ptr side.side_raw in
			Ptr.put_bits new_ptr 0 bits offset;
			Ptr.Ref.of_ptr new_ptr
		)
	in
*)
(*
	let ptrref_of_f1_and_offset : type id chan. (id,chan) f1_t -> _ = fun f1 offset ->
		let bits = match f1.f1_header.header_id with
			| MPEG1   -> 9
			| MPEG2 _ -> 8
		in
		let new_ptr = Ptr.Ref.to_ptr f1.f1_side.side_raw in
		Ptr.put_bits new_ptr 0 bits offset;
		Ptr.Ref.of_ptr new_ptr
	in
*)
	(* For some reason I can't take the bits calculation out of the function... *)
	let ptrref_of_side_and_offset : type id chan. (id,chan) side_t -> _ = fun side offset ->
		let bits = match k.header_id with
			| MPEG1   -> 9
			| MPEG2 _ -> 8
		in
		let new_ptr = Ptr.Ref.to_ptr side.side_raw in
		Ptr.put_bits new_ptr 0 bits offset;
		Ptr.Ref.of_ptr new_ptr
	in

	(* Pretty-printer for time from a frame number *)
	let seconds_per_frame = bspfk_of_samplerate k.header_samplerate *. 0.008(*kbit/byte*) in
	let p_time_of_frame frame_num p =
		let s_float = float_of_int frame_num *. seconds_per_frame in
		let (s_floatfrac, s_floatint) = modf s_float in
		let s_unnormal = int_of_float s_floatint in
		if s_unnormal < 60 then ( (* 0:ss.cc *)
			p (Char '0');
			p (Char ':');
			p (Int0N (2, s_unnormal));
		) else (
			let m_unnormal = s_unnormal / 60 in
			if m_unnormal < 60 then ( (* m:ss.cc *)
				p (Int m_unnormal)
			) else (
				let h_unnormal = m_unnormal / 60 in
				if h_unnormal < 24 then ( (* h:mm:ss.cc *)
					p (Int h_unnormal)
				) else ( (* d hh:mm:ss.cc *)
					p (Int (h_unnormal / 24));
					p (Char 'd');
					p (Spaces 1);
					p (Int0N (2, h_unnormal mod 24));
				);
				p (Char ':');
				p (Int0N (2, m_unnormal mod 60));
			);
			p (Char ':');
			p (Int0N (2, s_unnormal mod 60));
		);
		p (Char '.');
		p (Int0N (2, int_of_float (s_floatfrac *. 100.0)));
	in



	(* Make room for the beginning data, if it is to be saved *)
	if not delete_beginning_junk then (
		p [Str "Writing the first "; Int in_obj#first_mp3_byte; Str " bytes to the output file"];
		let in_pos = in_obj#pos in
		in_obj#seek 0;
		let length = in_obj#first_mp3_byte in
		let ref = in_obj#read_ptrref length in
		out_obj#output_this ref;
		in_obj#seek in_pos;
	);

	(* Make room for the LAME/XING header *)
	let (xing_bitrate, xing_pos, output_is_lame, xing_header_and_side_info) = (
		let is_lame = (match in_xing_option with
			| None -> false
			| Some xing -> (match xing.xingLame with
				| None -> false
				| Some lame -> true
			)
		) in
		let min_lame_bitrate = bytes_to_bitrate (if is_lame then 156 else 140) in
		let bitrate = max (min_bitrate_now 0) min_lame_bitrate in
		let output_header = Ptr.Ref.of_ptr (bitrate_to_header bitrate false false) in
		p [Str "XING header "; Ptrref output_header];
		out_obj#output_this output_header;
		out_obj#seek (out_obj#pos + side_info_size);
		let xing_header_and_side_info = Ptr.Ref.append_ptr (Ptr.Ref.of_ptr (bitrate_to_header bitrate false false)) (Ptr.clearret (Ptr.make side_info_size 0)) in
		let wheresit = out_obj#pos in
		out_obj#seek (out_obj#pos + bitrate.bitrate_data);
		p [Str "XING frame located at "; Int wheresit];
		(bitrate, wheresit, is_lame, xing_header_and_side_info)
	) in

	(* Reservoir initialization *)
	let max_reservoir_size = match k.header_id with
		| MPEG1 -> 511
		|   _   -> 255
	in

	let max_data_per_frame = match k.header_id with
		| MPEG1 -> unpadded_frame_length 320 + 1 - 4 - side_info_size
		|   _   -> unpadded_frame_length 160 + 1 - 4 - side_info_size
	in
	p [Str "Max "; Int max_data_per_frame; Str " bytes of data per frame"];

	(*****************)
	(* INFORMATIONAL *)
	(*****************)
	(* Largest and smallest bitrates used *)
	let max_output_bitrate_ref = ref {
		bitrate_data = 0;
		bitrate_size = 0;
		bitrate_num = 0;
		bitrate_padding = false;
		bitrate_index = 0
	} in
	let min_output_bitrate_ref = ref {
		bitrate_data = 500000;
		bitrate_size = 500000;
		bitrate_num = 500000;
		bitrate_padding = true;
		bitrate_index = 15
	} in
(*	let total_frames_ref = ref 0 in*)
	let total_frame_bytes_ref = ref xing_bitrate.bitrate_size in

	let frame_locations = Expandarray.create (match first_frame.if_xing with
		| None -> (in_obj#length / 418) (* Assume a 128kbps file *)
		| Some xing -> (match xing.xingNumFrames with
			| None -> (in_obj#length / 418) (* Again with the 128 *)
			| Some y -> (y + 2) (* Plus or minus a few *)
		)
	) in

	(* Make the initial frame filled with whatever padding is *)
	let ptr_template_padding =
		let len = String.length padding in
		let num = (max_data_per_frame - 1) / len + 1 in
		let ptr = Ptr.make (num * len) 0 in
		for i = 0 to num - 1 do
			Ptr.blit_from_string padding 0 ptr (i * len) len;
		done;
		ptr
	in
	let pad_f3_frame f3 goto =
		let len_now = Ptr.Ref.length f3.f3_output_data in
		let len_add = goto - len_now in
		if len_add < 0 then (
			failwith "ERROR: immutable frame data was set to be overwritten";
		) else if len_add = 0 then (
			()
		) else (
			p [Str "padding frame..."];
			let ptrref_padding = Ptr.Ref.of_subptr ptr_template_padding len_now len_add in
			f3.f3_output_data <- Ptr.Ref.append f3.f3_output_data ptrref_padding
		)
	in
	let pad_full_f3_frame f3 = pad_f3_frame f3 f3.f3_bitrate.bitrate_data in


	let q0 = List2.create () in (* For the recompression *)
	let q1 = List2.create () in
	let q2 = List2.create () in
	let q3 = List2.create () in
	let q3_bytes_ref = ref 0 in (* The number of bytes currently in Q3 *)
	let q3_current_reservoir_ref = ref 0 in (* The current byte reservoir in Q3. Although this is implied through the Q2 list, it must be explicitly set if Q2 is not used (minimize_bit_reservoir = false) *)

	(************)
	(************)
	(** QUEUE! **)
	(************)
	(************)
	let rec input_to_q0 frame_num update_percent bit_reservoir_so_far bit_reservoir_so_far_unused = (
		(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
		(* Same as A but add to Q0 instead *)
		p [];

		let frame_stuff = (try
			Some (in_obj#find_next_frame new_req)
		with
			End_of_file -> None
		) in

		match frame_stuff with
		| Some (_, IF_ext if_now, (wanted_at, got_at)) -> (
			p [Str "FRAME "; Int frame_num; Str " putting to Q0"];

			let next_update_percent = if float_of_int in_obj#pos > float_of_int in_obj#length /. 100. *. float_of_int update_percent then (
(*				if not debug_queue && not state.q_silent then printf "\r%2d%% done on frame %d%!" update_percent frame_num;*)
				P.print_replace_always [IntN (2,update_percent); Str "% done on frame "; Int frame_num];
				succ update_percent
			) else (
				update_percent
			) in

			if wanted_at <> got_at then (
(*				if not state.q_silent then printf "\rWARNING: Sync error on frame %d at %s (wanted at %d, found at %d)\n" frame_num (string_time_of_frame frame_num) wanted_at got_at;*)
				P.print_always [
					Str "WARNING: Sync error on frame "; Int frame_num; Str " at "; Fun (p_time_of_frame frame_num);
					Str " (wanted at "; Int wanted_at; Str ", found at "; Int got_at; Str ")"
				];
				file_state#add_sync_error;
			);

			if not if_now.if_crc_ok then (
(*				if not state.q_silent then printf "\rWARNING: CRC error on frame %d at %s\n" frame_num (string_time_of_frame frame_num);*)
				P.print_always [Str "WARNING: CRC error on frame "; Int frame_num; Str " at "; Fun (p_time_of_frame frame_num)];
				file_state#add_crc_error;
			);

			let side = side_info_of_if_2 if_now in
			if debug_queue then (
				p [Str " Reservoir bytes available: "; Int bit_reservoir_so_far_unused];
				p [Str " Reservoir bytes used:      "; Int side.side_offset];
(*				p [Str " Data bits:                 "; List (Array.fold_right (fun a b -> Int a :: Char ' ' :: b) side.side_bits [])];*)
				p [Str " Data bits:                 "; Fun (fun p ->
					match side.side_bits with
					| Bits_1_mono (a,b) -> (p (Int a); p (Char ' '); p (Int b))
					| Bits_1_stereo (a,b,c,d) -> (p (Int a); p (Char ' '); p (Int b); p (Char ' '); p (Int c); p (Char ' '); p (Int d))
					| Bits_2_mono (a) -> (p (Int a))
					| Bits_2_stereo (a,b) -> (p (Int a); p (Char ' '); p (Int b))
				)];
				p [Str " Total data bytes:          "; Int side.side_bytes];
				p [Str " Bit reservoir value length:"; Int (Ptr.Ref.length bit_reservoir_so_far)];
			);
			let combined_bit_reservoir = Ptr.Ref.append bit_reservoir_so_far if_now.if_data_raw in
			if false then (
				Printf.printf "Combined reservoir ptr:\n";
				Ptr.Ref.print combined_bit_reservoir;
			);

			let start_offset = Ptr.Ref.length bit_reservoir_so_far - side.side_offset in

			let (side_use, data_use, everythings_ok) = side_info_find_ok zero_whole_bad_frame side combined_bit_reservoir start_offset in

			p [Str " Side: "; Ptrref side_use.side_raw];
			p [Str " Data: "; Ptrref data_use];

			let new_reservoir_unused = Ptr.Ref.length combined_bit_reservoir - start_offset - Ptr.Ref.length data_use in

			if everythings_ok && if_now.if_crc_ok then (
(*				printf "NOT CHECKING CRC\n";*)
				p [Str " Putting to Q0"];
				let frame_to_compress = {
					f1_num = frame_num;
					f1_header = if_now.if_header;
					f1_side = side_use;
					f1_data = data_use;
					f1_pad_exact = None;
				 } in

				let put_token = if true then (
					recompress_obj#send (state, file_state, F1_ext frame_to_compress)
				) else (
					p [Str "ADDING TO Q0 SYNCHRONOUSLY"];
					recompress_obj#bypass (state, file_state, F1_ext frame_to_compress) (recompress_obj#f (state, file_state, F1_ext frame_to_compress))
				) in

				List2.append q0 (F1_ext frame_to_compress, put_token);
			) else (
				(* Skip the repacking *)
				p [Str " Skipping thread pool; putting to Q0"];
				if if_now.if_crc_ok then (
(*					if not state.q_silent then printf "\rWARNING: Buffer over/underflow on frame %d at %s\n" frame_num (string_time_of_frame frame_num);*)
					P.print_always [Str "WARNING: Buffer over/underflow on frame "; Int frame_num; Str " at "; Fun (p_time_of_frame frame_num)];
					file_state#add_buffer_error;
				);

				let frame_to_not_compress = {
					f1_num = frame_num;
					f1_header = if_now.if_header;
					f1_side = side_use;
					f1_data = data_use;
					f1_pad_exact = None;
				} in

				let bypass_token = recompress_obj#bypass (state, file_state, F1_ext frame_to_not_compress) (Normal (F1_ext frame_to_not_compress, false)) in

				List2.append q0 (F1_ext frame_to_not_compress, bypass_token)
			);

			let new_bit_reservoir = if Ptr.Ref.length combined_bit_reservoir < max_reservoir_size then (
				combined_bit_reservoir
			) else (
				let over = Ptr.Ref.length combined_bit_reservoir - max_reservoir_size in
				Ptr.Ref.sub combined_bit_reservoir over max_reservoir_size
			) in

			p [Str " X->Y"];
			q0_to_q1 ();
			p [Str " X->X"];
			input_to_q0 (succ frame_num) next_update_percent new_bit_reservoir new_reservoir_unused
		)
		| None -> (
			p [Str " X->Z (no more frames; target is 0)"];
			flush_q0 ();
			frame_num
		)
	) and q0_to_q1 () = (
		(* YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY *)
		if List2.length q0 > 4 * recompress_obj#max_threads then (
			let (F1_ext orig, put_token) = List2.take_first q0 in
			p [Str " requesting frame from Q0"];
			(match recompress_obj#recv put_token with
				| Normal (F1_ext new_f1, q_error) -> (
					if Ptr.Ref.length new_f1.f1_data > Ptr.Ref.length orig.f1_data then (
						p [
							Str " Oops. The repacked frame is larger than the original (";
							Int (Ptr.Ref.length new_f1.f1_data);
							Str " > ";
							Int (Ptr.Ref.length orig.f1_data);
							Str "); reusing the input frame"
						];
						List2.append q1 (F1_ext orig)
					) else (
						List2.append q1 (F1_ext new_f1)
					)
				)
				| Error e -> (
					(* May print twice. Oh well *)
					let print_this = [Str " Oops. Repacking the frame failed with error \""; Str (Printexc.to_string e); Str "\""] in
					state.q_print_recompress print_this;
					p print_this;
					file_state#add_rehuff_error;
					List2.append q1 (F1_ext orig);
				)
			);
			p [Str " Y->B"];
			mark_q1 ();
			p [Str " Y->Y"];
			q0_to_q1 ()
		) else (
			(* Not enough in the queue yet *)
			p [Str " Y->_!"];
			()
		)
	) and flush_q0 () = (
		(* ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ *)
		match List2.take_first_perhaps q0 with
		| Some ((F1_ext orig), put_token) -> (
			p [Str " requesting frame from Q0"];
			match recompress_obj#recv put_token with
			| Normal ((F1_ext new_f1), q_error) -> (
				if Ptr.Ref.length new_f1.f1_data > Ptr.Ref.length orig.f1_data then (
					p [
						Str " Oops. The repacked frame is larger than the original (";
						Int (Ptr.Ref.length new_f1.f1_data);
						Str " > ";
						Int (Ptr.Ref.length orig.f1_data);
						Str "); reusing the input frame"
					];
					List2.append q1 (F1_ext orig)
				) else (
					List2.append q1 (F1_ext new_f1)
				);
				p [Str " Z->Z"];
				flush_q0 ()
			)
			| Error e -> (
				let print_this = [Str " Oops. Repacking the frame failed with error \""; Str (Printexc.to_string e); Str "\""] in
				state.q_print_recompress print_this;
				p print_this;
				file_state#add_rehuff_error;
				List2.append q1 (F1_ext orig);
				p [Str " Z->Z"];
				flush_q0 ()
			)
		)
		| None -> (
			p [Str " Z->H (Q0 flushed)"];
			flush_q1 ()
		)
	) and input_to_q1 frame_num update_percent bit_reservoir_so_far bit_reservoir_so_far_unused = (
		(* AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA *)

		p [];

		let frame_stuff = (try
			Some (in_obj#find_next_frame new_req)
		with
			End_of_file -> None
		) in

		match frame_stuff with
		| Some (_, IF_ext if_now, (wanted_at, got_at)) -> (
			p [Str "FRAME "; Int frame_num];

			(* This is not correct for some files when a frame is larger than 1% of the whole file length *)
			(* However, these files must be fairly short (1000 frames in the worst case) when it goes very fast anyway *)
			let next_update_percent = if float_of_int in_obj#pos > float_of_int in_obj#length /. 100. *. float_of_int update_percent then (
(*				if not debug_queue && not state.q_silent then printf "\r%2d%% done on frame %d%!" update_percent frame_num;*)
				P.print_replace_always [IntN (2,update_percent); Str "% done on frame "; Int frame_num];
				succ update_percent
			) else (
				update_percent
			) in

(*			let side = side_info_of_string if_now.if_side_string in*)
			let side = side_info_of_if_2 if_now in
			if debug_queue then (
				p [Str " Reservoir bytes available: "; Int bit_reservoir_so_far_unused];
				p [Str " Reservoir bytes used:      "; Int side.side_offset];
(*				p [Str " Data bits:                 "; List (Array.fold_right (fun a b -> Int a :: Char ' ' :: b) side.side_bits [])];*)
				p [Str " Data bits:                 "; Fun (fun p ->
					match side.side_bits with
					| Bits_1_mono (a,b) -> (p (Int a); p (Char ' '); p (Int b))
					| Bits_1_stereo (a,b,c,d) -> (p (Int a); p (Char ' '); p (Int b); p (Char ' '); p (Int c); p (Char ' '); p (Int d))
					| Bits_2_mono (a) -> (p (Int a))
					| Bits_2_stereo (a,b) -> (p (Int a); p (Char ' '); p (Int b))
				)];
				p [Str " Total data bytes:          "; Int side.side_bytes];
				p [Str " Bit reservoir value length:"; Int (Ptr.Ref.length bit_reservoir_so_far)];
			);
			let combined_bit_reservoir = Ptr.Ref.append bit_reservoir_so_far if_now.if_data_raw in
			if false then (
				Printf.printf "Combined reservoir ptr:\n";
				Ptr.Ref.print combined_bit_reservoir;
			);
			let start_offset = Ptr.Ref.length bit_reservoir_so_far - side.side_offset in

			let (new_side_info, new_data, everythings_ok) = side_info_find_ok zero_whole_bad_frame side combined_bit_reservoir start_offset in
			let buffer_error = not everythings_ok in

			let new_reservoir_unused = Ptr.Ref.length combined_bit_reservoir - start_offset - Ptr.Ref.length new_data in


			(* Overwrite the bit reservoir thingie here *)
			(* TODO: should this be <= instead of < ? *)

			let new_bit_reservoir = if Ptr.Ref.length combined_bit_reservoir < max_reservoir_size then (
				combined_bit_reservoir
			) else (
				let over = Ptr.Ref.length combined_bit_reservoir - max_reservoir_size in
				Ptr.Ref.sub combined_bit_reservoir over max_reservoir_size
			) in
			if wanted_at <> got_at then (
(*				if not state.q_silent then printf "\rWARNING: Sync error on frame %d at %s (wanted at %d, found at %d)\n" frame_num (string_time_of_frame frame_num) wanted_at got_at;*)
				P.print_always [
					Str "WARNING: Sync error on frame "; Int frame_num; Str " at "; Fun (p_time_of_frame frame_num);
					Str " (wanted at "; Int wanted_at; Str ", found at "; Int got_at; Str ")"
				];
				file_state#add_sync_error;
			);
			if not if_now.if_crc_ok then (
(*				if not state.q_silent then printf "\rWARNING: CRC error on frame %d at %s\n" frame_num (string_time_of_frame frame_num);*)
				P.print_always [Str "WARNING: CRC error on frame "; Int frame_num; Str " at "; Fun (p_time_of_frame frame_num)];
				file_state#add_crc_error;
			);
			if buffer_error then (
(*				if not state.q_silent then printf "\rWARNING: Buffer over/underflow on frame %d at %s\n" frame_num (string_time_of_frame frame_num);*)
				P.print_always [Str "WARNING: Buffer over/underflow on frame "; Int frame_num; Str " at "; Fun (p_time_of_frame frame_num)];
				file_state#add_buffer_error;
			);

			p [Str " Side: "; Ptrref new_side_info.side_raw];
			p [Str " Data: "; Ptrref new_data];

			List2.append q1 (F1_ext {
				f1_num = frame_num;
				f1_header = if_now.if_header;
				f1_side = new_side_info;
				f1_data = new_data;
				f1_pad_exact = None;
			});

			p [Str " A->B (found frame "; Int frame_num; Str ")"];
			mark_q1 ();
			p [Str " A->A (found frame; after B)"];
			input_to_q1 (succ frame_num) next_update_percent new_bit_reservoir new_reservoir_unused
		)
		| None -> (
			p [Str " A->H (no frame found)"];
			flush_q1 ();
			frame_num
		)
	) and mark_q1 () = (
		(* BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB *)

		(* This marks any frame with an amount of padding that can be used no matter what data is coming up *)

		(* Ignore since the padding is set by side-effect *)
		ignore (List2.rev_fold (fun (new_pad_real,new_pad_max) (F1_ext f1) ->
			let bytes_to_end_in_this_frame = Ptr.Ref.length f1.f1_data + new_pad_real in
			let bytes_to_end_in_this_frame_max = Ptr.Ref.length f1.f1_data + new_pad_max in

			let required_bytes_from_previous_frame = max 0 (bytes_to_end_in_this_frame - max_data_per_frame) in
			let required_bytes_from_previous_frame_max = max 0 (min max_reservoir_size (bytes_to_end_in_this_frame_max - max_data_per_frame)) in

			p [Str "  B mark: "; Int f1.f1_num; Str " ("; Int (Ptr.Ref.length f1.f1_data); Str "b) "; IntN (3,new_pad_real); Str " - "; IntN (3,new_pad_max)];

			if new_pad_real = new_pad_max then f1.f1_pad_exact <- Some new_pad_real;

			(required_bytes_from_previous_frame,required_bytes_from_previous_frame_max)
		) (0,max_reservoir_size) q1);

		(* Check the first frame to see if anything was actually done *)
		let marked = (
			if List2.is_empty q1 then false else match List2.peek_first q1 with
			| F1_ext {f1_pad_exact = None} -> false
			| _ -> true
		) in

		if not marked then (
			(* END *)
			p [Str " B->_!"];
		) else if minimize_bit_reservoir then (
			p [Str " B->C (Q1 was marked)"];
			q1_to_q2 false
		) else (
			p [Str " B->D (Q1 was marked)"];
			q1_to_q3 false
		)
	) and q1_to_q2 eof = (
		(* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC *)

		(* Is there any more to copy? *)
(*		let copy_stuff_padding = (if List2.is_empty q1 then None else (List2.peek_first q1).f1_pad_exact) in*)
		let copy_stuff_padding = match List2.peek_first_perhaps q1 with
			| None -> None
			| Some (F1_ext f) -> f.f1_pad_exact
		in

		match copy_stuff_padding with
		| Some pad -> (

			let F1_ext f1 = List2.take_first q1 in

			(* Figure out how much data is in the last frame in Q2. *)
			(* This is necessary to take padding into account for bitrate allocation *)
			let (bytes_to_store, prev_padding) = if List2.is_empty q2 then (
				(Ptr.Ref.length f1.f1_data + pad, 0)
			) else (
				let F2_ext f2 = List2.peek_last q2 in
				(Ptr.Ref.length f1.f1_data + pad - (min max_reservoir_size f2.f2_bytes_left), f2.f2_bytes_left)
			) in

			let bitrate_optimal = bytes_to_bitrate bytes_to_store in
			let bitrate_minimum = min_bitrate_now f1.f1_num in
			p [Str "  "; Int f1.f1_num; Str ": "; Int (Ptr.Ref.length f1.f1_data); Str "+"; Int pad; Str " bytes ("; Int bitrate_optimal.bitrate_data; Str " optimal, "; Int bitrate_minimum.bitrate_data; Str " minimum)"];

			let bitrate_use = if bitrate_optimal.bitrate_data > bitrate_minimum.bitrate_data then bitrate_optimal else bitrate_minimum in

			(* See if there is a gap right here *)
			let check_output = if List2.is_empty q2 then (
				true
			) else (
				let F2_ext f2_last = List2.peek_last q2 in
				(* NOTE: E (mark_q2) will clear the gap setting on any processed frame, so if the previous frame has a gap it must not have been processed *)
				(* This part is necessary to propogate the gap flag to the end of the queue *)
				if f2_last.f2_check_output then (
					p [Str "  GAP since last added frame has one"];
					true
				) else (
					(* If the frame offset has maxed out, hit 0, or the data goes all the way to the end of the frame *)
					f2_last.f2_bytes_left > min max_reservoir_size prev_padding || prev_padding = 0 || bitrate_use.bitrate_data - bytes_to_store + pad = 0
				)
			) in
			if check_output then (
				p [Str "  CHECKOUTPUT"];
			) else (
				p [Str "  NOCHECK"];
			);

			List2.append q2 (F2_ext {
				f2_num = f1.f1_num;
				f2_bitrate = bitrate_use;
				f2_header = f1.f1_header;
				f2_side = f1.f1_side;
(*				f2_string = f1.f1_string;*)
				f2_data = f1.f1_data;
				f2_pad = pad;
				f2_offset = min max_reservoir_size prev_padding;
				f2_bytes_left = bitrate_use.bitrate_data - bytes_to_store + pad;
				f2_flag = false;
				f2_check_output = check_output;
			});

			p [Str " C->C (copied to Q2)"];
			q1_to_q2 eof
		)
		| None when eof -> (
			p [Str " C->I (not copied to Q2, EOF)"];
			flush_q2 ()
		)
		| None -> (
			p [Str " C->E (not copied to Q2)"];
			mark_q2 ()
		)
	) and q1_to_q3 eof = (
		(* DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *)

		(* Is there any more to copy? *)
(*		let copy_stuff_padding = (if List2.is_empty q1 then None else (List2.peek_first q1).f1_pad_exact) in*)
		let copy_stuff_padding = match List2.peek_first_perhaps q1 with
			| None -> None
			| Some (F1_ext f) -> f.f1_pad_exact
		in

		match copy_stuff_padding with
		| Some pad -> (
			let F1_ext f1 = List2.take_first q1 in

			let (bytes_to_store, prev_padding) = (Ptr.Ref.length f1.f1_data + pad - !q3_current_reservoir_ref, !q3_current_reservoir_ref) in

			let bitrate_optimal = bytes_to_bitrate bytes_to_store in
			let bitrate_minimum = min_bitrate_now f1.f1_num in
			p [Str "  "; Int f1.f1_num; Str ": "; Int (Ptr.Ref.length f1.f1_data); Str "+"; Int pad; Str " bytes ("; Int bitrate_optimal.bitrate_data; Str " optimal, "; Int bitrate_minimum.bitrate_data; Str " minimum)"];
			p [Str "   Reservoir bytes: "; Int !q3_current_reservoir_ref];

(*			printf "A:  %d\n" !q3_current_reservoir_ref;*)

			let bitrate_use = if bitrate_optimal.bitrate_data > bitrate_minimum.bitrate_data then bitrate_optimal else bitrate_minimum in

			let (bytes_seen, read_from_pos, frame_started_at) = List2.fold (fun (bytes_seen, read_from_pos, frame_started_at) f3 ->
				p [Str "   Writing to frame "; Int f3.f3_num];
				let write_to_pos = !q3_bytes_ref - bytes_seen - !q3_current_reservoir_ref + read_from_pos in
				p [Str "    Start writing byte "; Int read_from_pos; Str " to byte "; Int write_to_pos];
				if write_to_pos >= f3.f3_bitrate.bitrate_data then (
					p [Str "    Oops. Nothing on this frame yet (length "; Int (f3.f3_bitrate.bitrate_data); Str ")"];
					(* We need to pad the ptrref to the bitrate size *)
					pad_full_f3_frame f3;
					f3.f3_flag <- true;
					p [Str "     "; Ptrref f3.f3_output_data];
					(bytes_seen + f3.f3_bitrate.bitrate_data, read_from_pos, bytes_seen + f3.f3_bitrate.bitrate_data)
				) else (
					let bytes_to_write = min (f3.f3_bitrate.bitrate_data - write_to_pos) (Ptr.Ref.length f1.f1_data - read_from_pos) in
					p [Str "    Output "; Int bytes_to_write; Str " bytes"];
					if bytes_to_write > 0 then (
(*						String.blit f1.f1_string read_from_pos f3.f3_output_data write_to_pos bytes_to_write;*)
						if Ptr.Ref.length f3.f3_output_data < write_to_pos then (
							p [Str "     Need some padding, though ("; Int (Ptr.Ref.length f3.f3_output_data); Str "<"; Int write_to_pos; Str ")"];
							pad_f3_frame f3 write_to_pos;
						);
						let add_data = Ptr.Ref.sub f1.f1_data read_from_pos bytes_to_write in
						f3.f3_output_data <- Ptr.Ref.append f3.f3_output_data add_data;
					);
					p [Str "     "; Ptrref f3.f3_output_data];
					let new_frame_started_at = (if read_from_pos = 0 then bytes_seen + write_to_pos else frame_started_at) in
					(bytes_seen + f3.f3_bitrate.bitrate_data, read_from_pos + bytes_to_write, new_frame_started_at)
				)
			) (0,0,0) q3 in

			p [Str "   Resultant bytes seen: "; Int bytes_seen; Str " ("; Int !q3_bytes_ref; Str " total); read_from_pos: "; Int read_from_pos];
			p [Str "   Frame started at: "; Int frame_started_at];

			(* Create a header and side info for the frame *)
(*			printf " B: %d\n" !q3_current_reservoir_ref;*)
(*			let header_side_raw_string = (string_of_header_and_bitrate ~new_bitrate:bitrate_use f1.f1_header) ^ (string_of_side_and_offset ~new_offset:!q3_current_reservoir_ref f1.f1_side) in*)
			let header_side_raw = Ptr.Ref.append (ptrref_of_header_and_bitrate f1.f1_header bitrate_use) (ptrref_of_side_and_offset f1.f1_side !q3_current_reservoir_ref) in

			p [Str "Found string in frame "; Int f1.f1_num; Str ":"];
			p [Str " "; Ptrref header_side_raw];

(*			printf " ?: %d\n" (unpackBits header_side_raw 0 9);*)

			(* Now output the current frame *)
			let f_new = {
				f3_num = f1.f1_num;
				f3_header_side_raw = header_side_raw;
(*				f3_output_string = String.sub template_padding 0 bitrate_use.bitrate_data;*)
				f3_output_data = Ptr.Ref.null;
				f3_bitrate = bitrate_use;
				f3_flag = false;
			} in
			let bytes_left_for_current_frame = max 0 (Ptr.Ref.length f1.f1_data - read_from_pos) in
			if bytes_left_for_current_frame > 0 then (
				p [Str "   Writing last "; Int bytes_left_for_current_frame; Str " bytes to current frame "; Int f1.f1_num];
(*				String.blit f1.f1_string read_from_pos f_new.f3_output_string 0 bytes_left_for_current_frame;*)
				let add_data = Ptr.Ref.sub f1.f1_data read_from_pos bytes_left_for_current_frame in
				f_new.f3_output_data <- Ptr.Ref.append f_new.f3_output_data add_data;
			);

			List2.append q3 f_new;
			q3_bytes_ref := !q3_bytes_ref + bitrate_use.bitrate_data;
			q3_current_reservoir_ref := min max_reservoir_size (!q3_bytes_ref - frame_started_at - Ptr.Ref.length f1.f1_data);

			p [Str "   New Q3 bytes ref: "; Int !q3_bytes_ref];
			p [Str "   New reservoir: "; Int !q3_current_reservoir_ref; Str " ("; Int !q3_bytes_ref; Str " - "; Int frame_started_at; Str " - "; Int (Ptr.Ref.length f1.f1_data); Str ")"];

			p [Str " D->D (copied to Q3)"];
			q1_to_q3 eof
		)
		| None -> (
			p [Str " D->G (not copied to Q3)"];
			q3_to_output eof
		)
	) and mark_q2 () = (
		(* EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE *)

		let F2_ext f2_last = List2.peek_last q2 in (* I think this is safe, since only C goes here (q1_to_q2) and C is only called if something is marked to be put into q2 *)
		let frame_after_last_use_bytes = f2_last.f2_bytes_left in

		if f2_last.f2_check_output then (
			(* Only process this if we need to check it *)

			(* This SHOULD be true every time a frame may leave q2 (but may be true even if no frames leave) *)
			let new_guess = f2_last.f2_check_output in
			p [Str "  Guess "; Bool new_guess; Str " ("; Int f2_last.f2_offset; Str ", "; Int frame_after_last_use_bytes; Str ")"];

			ignore (List2.rev_fold (fun (output_ok, next_frame_use_bytes) (F2_ext f2) ->
				let k = min f2.f2_offset (f2.f2_bytes_left - next_frame_use_bytes) in
				(* The two numbers %d -> %d will only be different if the location of a later frame is limited by the max size of the bit reservoir *)
				(* Note that the frame starts out in q2 with the highest offset possible (that is, the data is the furthest forward) *)
				p [Str "  "; Str (if output_ok || f2.f2_offset - k = 0 then "*" else ""); Int f2.f2_num; Str " -> "; Int f2.f2_offset; Str " (check: "; Bool f2.f2_check_output; Str ")"]; (*%s%d: %d -> %d (check: %B)\n (if output_ok || f2.f2_offset - k = 0 then "*" else "") f2.f2_num f2.f2_offset (f2.f2_offset - k) f2.f2_check_output;*)
				f2.f2_offset <- f2.f2_offset - k;
				f2.f2_bytes_left <- f2.f2_bytes_left - k;
				f2.f2_check_output <- false; (* Not needed any more *)
				f2.f2_flag <- output_ok; (* This can't be based off the new output_ok since the next frame from q1 needs to know how much space is left in this frame *)
				if f2.f2_offset = 0 then (
					(true, f2.f2_offset)
				) else (
					(output_ok, f2.f2_offset)
				)
			) (frame_after_last_use_bytes = 0, frame_after_last_use_bytes) q2);

		);

		let F2_ext f2_first = List2.peek_first q2 in
		let marked = f2_first.f2_flag in (* If getting f2_last is safe, this is safe too *)

		if not marked then (
			(* END *)
			p [Str " E->_!"];
		) else (
			p [Str " E->F (Q2 was marked)"];
			q2_to_q3 false
		)
	) and q2_to_q3 eof = (
		(* FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF *)

		(* Is there any more to copy? *)
		let copy_stuff = match List2.peek_first_perhaps q2 with
			| None -> false
			| Some (F2_ext {f2_flag = f}) -> f
		in

		if copy_stuff then (

			(* Do things here *)
			let F2_ext f2 = List2.take_first q2 in
			p [Str "  "; Int f2.f2_num; Str ": "; Int (Ptr.Ref.length f2.f2_data); Str "+"; Int f2.f2_pad; Str " bytes in "; Int f2.f2_bitrate.bitrate_data; Str " byte frame with reservoir "; Int f2.f2_offset; Str " ("; Int f2.f2_bytes_left; Str " bytes left)"];(*"%d: %d+%d bytes in %d byte frame with reservoir %d (%d bytes left)\n" f2.f2_num (Ptr.Ref.length f2.f2_data) f2.f2_pad f2.f2_bitrate.bitrate_data f2.f2_offset f2.f2_bytes_left;*)

			let (bytes_seen, read_from_pos) = List2.fold (fun (bytes_seen, read_from_pos) f3 ->
				p [Str "   Writing to frame "; Int f3.f3_num];
				let write_to_pos = !q3_bytes_ref - bytes_seen - f2.f2_offset + read_from_pos in
				p [Str "    Start writing byte "; Int read_from_pos; Str " to byte "; Int write_to_pos];
				if write_to_pos >= f3.f3_bitrate.bitrate_data then (
					p [Str "    Oops. Nothing on this frame yet (length "; Int f3.f3_bitrate.bitrate_data; Str ")"];
					(* Pad the full frame *)
					pad_full_f3_frame f3;
					f3.f3_flag <- true;
					(bytes_seen + f3.f3_bitrate.bitrate_data, read_from_pos)
				) else (
					let bytes_to_write = min (f3.f3_bitrate.bitrate_data - write_to_pos) (Ptr.Ref.length f2.f2_data - read_from_pos) in
					p [Str "    Output "; Int bytes_to_write; Str " bytes"];
					if bytes_to_write > 0 then (
(*						String.blit f2.f2_string read_from_pos f3.f3_output_string write_to_pos bytes_to_write;*)
						if Ptr.Ref.length f3.f3_output_data < write_to_pos then (
							p [Str "     Need some padding, though ("; Int (Ptr.Ref.length f3.f3_output_data); Str "<"; Int write_to_pos; Str ")"];
							pad_f3_frame f3 write_to_pos;
						);
						let add_data = Ptr.Ref.sub f2.f2_data read_from_pos bytes_to_write in
						f3.f3_output_data <- Ptr.Ref.append f3.f3_output_data add_data;
					);
					(bytes_seen + f3.f3_bitrate.bitrate_data, read_from_pos + bytes_to_write)
				)
			) (0,0) q3 in

(*			let header_side_raw_string = (string_of_header_and_bitrate ~new_bitrate:f2.f2_bitrate f2.f2_header) ^ (string_of_side_and_offset ~new_offset:f2.f2_offset f2.f2_side) in*)
			let header_side_raw = Ptr.Ref.append (ptrref_of_header_and_bitrate f2.f2_header f2.f2_bitrate) (ptrref_of_side_and_offset f2.f2_side f2.f2_offset) in

			p [Str "Found string in frame "; Int f2.f2_num; Str ":"];
			p [Str " "; Ptrref header_side_raw];

			(* Add the new frame to Q3 *)
			let f_new = {
				f3_num = f2.f2_num;
				f3_header_side_raw = header_side_raw;
(*				f3_output_string = String.sub template_padding 0 f2.f2_bitrate.bitrate_data;*)
				f3_output_data = Ptr.Ref.null;
				f3_bitrate = f2.f2_bitrate;
				f3_flag = false;
			} in
			let bytes_left_for_current_frame = Ptr.Ref.length f2.f2_data - read_from_pos in
			if read_from_pos < Ptr.Ref.length f2.f2_data then (
				p [Str "   Writing last "; Int bytes_left_for_current_frame; Str " bytes to current frame "; Int f2.f2_num];
(*				String.blit f2.f2_string read_from_pos f_new.f3_output_string 0 bytes_left_for_current_frame;*)
				let add_data = Ptr.Ref.sub f2.f2_data read_from_pos bytes_left_for_current_frame in
				f_new.f3_output_data <- Ptr.Ref.append f_new.f3_output_data add_data;
			);
			List2.append q3 f_new;
			q3_bytes_ref := !q3_bytes_ref + f2.f2_bitrate.bitrate_data;

			p [Str "   New Q3 bytes ref: "; Int !q3_bytes_ref];

			(* New part to clean up q3 faster *)
			p [Str " F->G (clean up flagged q3 frames)"];
			q3_to_output false;

			p [Str " F->F (copied to Q3; after F->G)"];
			q2_to_q3 eof
		) else (
			p [Str " F->G (not copied to Q3)"];
			q3_to_output eof
		)
	) and q3_to_output eof = (
		(* GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG *)

		(* This really needs to be run every time q2_to_q3 flags something in q3 *)
		(* Otherwise it will keep searching for a place to start writing at the first frame of q3, which may be quite inefficient *)

		(* Ignore the f3_flag if eof is set *)
		let copy_stuff = (if List2.is_empty q3 then false else if eof then true else (List2.peek_first q3).f3_flag) in

		if copy_stuff then (

			let f3 = List2.take_first q3 in
			q3_bytes_ref := !q3_bytes_ref - f3.f3_bitrate.bitrate_data;


			if eof then pad_full_f3_frame f3;

(*			if debug_queue then Ptr.Ref.print f3.f3_output_data;*)
(*			assert (Ptr.Ref.length f3.f3_output_data = f3.f3_bitrate.bitrate_data);*)

			(* Update output info *)
(*			let f3_bitrate = (bytes_to_bitrate (String.length f3.f3_output_string)) in*)
			let f3_bitrate = f3.f3_bitrate in
			max_output_bitrate_ref := max !max_output_bitrate_ref f3_bitrate;
			min_output_bitrate_ref := min !min_output_bitrate_ref f3_bitrate;
(*			incr total_frames_ref;*)
			total_frame_bytes_ref := !total_frame_bytes_ref + Ptr.Ref.length f3.f3_header_side_raw + f3.f3_bitrate.bitrate_data;
			Expandarray.set frame_locations f3.f3_num out_obj#pos;

			out_obj#output_this f3.f3_header_side_raw;
			out_obj#output_this f3.f3_output_data;
(*
			if debug_queue then (
				Ptr.Ref.print f3.f3_output_data;
			);
*)
			p [Str " G->G (outputted)"];
			q3_to_output eof
		) else (
			(* END *)
			p [Str " G->_!"];
		)
	) and flush_q1 () = (
		(* HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH *)

		(* This is basically the same as B (mark Q1) except it utilizes the fact that the maximum padding needed on the last frame is 0 *)
		(* Ignore since the padding is set by side-effect *)
		ignore (List2.rev_fold (fun new_pad_real (F1_ext f1) ->
			let bytes_to_end_in_this_frame = Ptr.Ref.length f1.f1_data + new_pad_real in

			let required_bytes_from_previous_frame = max 0 (bytes_to_end_in_this_frame - max_data_per_frame) in

			p [Str "  "; Int f1.f1_num; Str " ("; Int (Ptr.Ref.length f1.f1_data); Str "b) "; IntN (3,new_pad_real); Str " exactly"];

			f1.f1_pad_exact <- Some new_pad_real;

			required_bytes_from_previous_frame
		) 0 q1);

		if minimize_bit_reservoir then (
			p [Str " H->C (Q1 flushed)"];
			q1_to_q2 true
		) else (
			p [Str " H->D (Q1 flushed)"];
			q1_to_q3 true
		)
	) and flush_q2 () = (
		(* IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII *)

		(* Same as E (mark_q2), except let the last frame go as far forward as possible *)
		ignore (List2.rev_fold (fun next_frame_use_bytes (F2_ext f2) ->
			let k = min f2.f2_offset (f2.f2_bytes_left - next_frame_use_bytes) in
			p [Str "  "; Int f2.f2_num; Str ": "; Int f2.f2_offset; Str " -> "; Int (f2.f2_offset - k); Str " exactly"];
			f2.f2_offset <- f2.f2_offset - k;
			f2.f2_bytes_left <- f2.f2_bytes_left - k;
			f2.f2_flag <- true;
			f2.f2_offset
		) 0 q2);

		p [Str " I->F (Q2 flushed)"];
		q2_to_q3 true
	) in

	(* Modify parts for multiproc *)
	(* Basically we need to do A separately from the rest *)
(*	let multi_input_to_q1*)

	let total_frames = if recompress then (
		p [Str " X"];
		input_to_q0 0 1 Ptr.Ref.null 0
	) else (
		p [Str " A"];
		input_to_q1 0 1 Ptr.Ref.null 0
	) in

	let t2 = Unix.gettimeofday () in

	if not state.q_silent then (
		let file_time = float_of_int total_frames *. seconds_per_frame in
(*		printf "\r100%% done with %d frames (%.2fx)\n%!" total_frames (file_time /. (t2 -. t1));*)
		let time = if t2 <= t1 then (
			Str "too fast!"
		) else (
			List [Float_N (2, (file_time /. (t2 -. t1))); Char 'x']
		) in
		P.print_always [Str "100% done with "; Int total_frames; Str " frames ("; time; Str ")"];
	);

	(********************************)
	(* EVERY FRAME HAS BEEN WRITTEN *)
	(********************************)
(*
	procs#scatter_exit;
	Thread.join get_thread;
*)
	(* See whether the input info matches the actual info *)
	(match in_xing_option with
		| Some {xingNumFrames = Some xing_frames} when xing_frames <> total_frames -> (
(*			printf "\rWARNING: actual number of frames (%d) does not match the input info (%d)\n" total_frames xing_frames*)
			P.print_always [Str "WARNING: Actual number of frames ("; Int total_frames; Str ") does not match the input info ("; Int xing_frames; Str ")"];
		)
		| _ -> ()
	);

	(* Write the trailing non-MP3 data, if it is to be saved *)
	if not delete_end_junk then (
		p [Str "Writing the last "; Int (in_obj#length - in_obj#last_mp3_byte - 1); Str " bytes to the output file"];
		let in_pos = in_obj#pos in
		let length = in_obj#length - (in_obj#last_mp3_byte + 1) in
		in_obj#seek (in_obj#last_mp3_byte + 1);
		let ref = in_obj#read_ptrref length in
		out_obj#output_this ref;
		in_obj#seek in_pos;
	);

	let total_bytes_written = out_obj#pos in

	if debug_queue then (
		let (b,s,c,r) = file_state#get_errors in
		p [Str "Number of buffer errors: "; Int b];
		p [Str "Number of sync errors:   "; Int s];
		p [Str "Number of CRC errors:    "; Int c];
		p [Str "Number of rehuff errors: "; Int r];
		p [Str "Bitrate used: "; Int !min_output_bitrate_ref.bitrate_num; Str (if !min_output_bitrate_ref.bitrate_padding then "*" else ""); Str " - "; Int !max_output_bitrate_ref.bitrate_num; Str (if !max_output_bitrate_ref.bitrate_padding then "*" else "")];
		p [Str "MP3 data range: "; Int in_obj#first_mp3_byte; Str " - "; Int in_obj#last_mp3_byte];
		p [Str "Wrote "; Int !total_frame_bytes_ref; Str " in "; Int total_frames; Str " frames"];
		(* FRAME POSITIONS *)
		p [Str "Frame locations:"];
		for i = 0 to Expandarray.length frame_locations - 1 do
			p [Str " "; IntN (5,i); Str " = "; Int (Expandarray.get frame_locations i)];
		done;
	);

	(********)
	(* XING *)
	(********)

	let toc = Array.init 100 (fun percent ->
		let frame = min (Expandarray.length frame_locations - 1) (percent * Expandarray.length frame_locations / 100) in
		let pos = Expandarray.get frame_locations frame in
		let frac = int_of_float (float_of_int pos *. 256. /. (float_of_int total_bytes_written)) in
		frac
	) in

	if debug_queue then (
		let parse_toc f = Array.iter (fun x -> f (Hex (2,x))) toc in
		p [Str "TOC: "; Fun parse_toc];
(*
		printf "TOC: ";
		Array.iter (fun x -> printf "%02X" x) toc;
		printf "\n";
*)
	);
	let out_xing = (match (output_is_lame, in_xing_option) with
		| (false, None) -> { (* Make up an XING frame *)
			xingRawTag = ""; (* Nobody cares *)
			xingTagType = (if !min_output_bitrate_ref.bitrate_num = !max_output_bitrate_ref.bitrate_num then "Info" else "Xing");
			xingNumFrames = Some total_frames;
			xingNumBytes = Some !total_frame_bytes_ref;
			xingTOC = Some toc;
			xingQuality = None;
			xingEncoder = padding;
			xingLame = None;
		}
		| (false, Some x) -> { (* Use old parts of XING frame *)
			xingRawTag = "";
			xingTagType = (if !min_output_bitrate_ref.bitrate_num = !max_output_bitrate_ref.bitrate_num then "Info" else "Xing");
			xingNumFrames = Some total_frames;
			xingNumBytes = Some !total_frame_bytes_ref;
			xingTOC = Some toc;
			xingQuality = x.xingQuality;
			xingEncoder = (if x.xingEncoder = "" then padding else x.xingEncoder);
			xingLame = None;
		}
		| (true, None) -> { (* Make up a LAME frame (is this even used?) *)
			xingRawTag = "";
			xingTagType = (if !min_output_bitrate_ref.bitrate_num = !max_output_bitrate_ref.bitrate_num then "Info" else "Xing");
			xingNumFrames = Some total_frames;
			xingNumBytes = Some !total_frame_bytes_ref;
			xingTOC = Some toc;
			xingQuality = None;
			xingEncoder = padding;
			xingLame = Some {
				lameRevision = 1;
				lameVBRMethod = 0;
				lameLowpass = 0;
				lamePeakAmplitude = 0.0;
				lameRGTrack = 0;
				lameRGAlbum = 0;
				lameNSPsyTune = false;
				lameNSSafeJoint = false;
				lameNoGapPrev = false;
				lameNoGapNext = false;
				lameATHType = 0;
				lameABRBitrate = min 255 !min_output_bitrate_ref.bitrate_num;
				lameDelayStart = 576; (* Assume there's a 576 sample delay on both ends *)
				lameDelayEnd = 576;
				lameNoiseShaping = 0;
				lameStereoMode = (match k.header_channel_mode with
					| Stereo Stereo_simple -> 1
					| Stereo Stereo_joint _ -> 3
					| Stereo Stereo_dual -> 2
					| Mono -> 0
				);
				lameUnwise = true; (* Better to say a good one's bad than say a bad one's good *)
				lameSourceFrequency = 1; (* Most things come from a CD *)
				lameMP3Gain = 0;
				lameSurround = 0;
				lamePreset = 0;
				lameMusicLength = 0;
				lameMusicCRC = 0;
			}
		}
		| (true, Some x) -> ( (* Use old parts in a new LAME tag *)
			match x.xingLame with
			| None -> { (* Use the XING part, make up the LAME part (is this used either?) *)
				xingRawTag = "";
				xingTagType = (if !min_output_bitrate_ref.bitrate_num = !max_output_bitrate_ref.bitrate_num then "Info" else "Xing");
				xingNumFrames = Some total_frames;
				xingNumBytes = Some !total_frame_bytes_ref;
				xingTOC = Some toc;
				xingQuality = x.xingQuality;
				xingEncoder = (if x.xingEncoder = "" then padding else x.xingEncoder);
				xingLame = Some {
					lameRevision = 1;
					lameVBRMethod = 0;
					lameLowpass = 0;
					lamePeakAmplitude = 0.0;
					lameRGTrack = 0;
					lameRGAlbum = 0;
					lameNSPsyTune = false;
					lameNSSafeJoint = false;
					lameNoGapPrev = false;
					lameNoGapNext = false;
					lameATHType = 0;
					lameABRBitrate = min 255 !min_output_bitrate_ref.bitrate_num;
					lameDelayStart = 576; (* Assume there's a 576 sample delay on both ends *)
					lameDelayEnd = 576;
					lameNoiseShaping = 0;
					lameStereoMode = (match k.header_channel_mode with
						| Stereo Stereo_simple -> 1
						| Stereo Stereo_joint _ -> 3
						| Stereo Stereo_dual -> 2
						| Mono -> 0
					);
					lameUnwise = true; (* Better to say a good one's bad than say a bad one's good *)
					lameSourceFrequency = 1; (* Most things come from a CD *)
					lameMP3Gain = 0;
					lameSurround = 0;
					lamePreset = 0;
					lameMusicLength = 0;
					lameMusicCRC = 0;
				}
			}
			| Some l -> { (* Use the old LAME tag *)
				xingRawTag = "";
				xingTagType = (if !min_output_bitrate_ref.bitrate_num = !max_output_bitrate_ref.bitrate_num then "Info" else "Xing");
				xingNumFrames = Some total_frames;
				xingNumBytes = Some !total_frame_bytes_ref;
				xingTOC = Some toc;
				xingQuality = x.xingQuality;
				xingEncoder = (if x.xingEncoder = "" then padding else x.xingEncoder);
				xingLame = Some l
			}
		)
	) in (* out_xing *)
	let xing = make_xing out_xing xing_header_and_side_info in
	p [Str "XING tag:"];
	p [Str "  "; Ptrref xing];
	if debug_queue then (match (output_is_lame, in_xing_option) with
		| (false, None) ->   p [Str "  None -> XING"];
		| (false, Some x) -> p [Str "  XING -> XING"];
		| (true, None) ->    p [Str "  None -> LAME (???)"];
		| (true, Some x) -> (
			match x.xingLame with
			| None ->   p [Str "  XING -> LAME (???)"];
			| Some l -> p [Str "  LAME -> LAME"];
		)
	);
	out_obj#seek xing_pos;
	p [Str "  Writing tag at "; Int xing_pos];
	out_obj#output_this xing;

	in_obj#close;
	out_obj#close;

;;
