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

(* Giving in to the imperative side of the Force *)
let (+=) a b = (a := !a + b);;

let do_info ?(only_bitrate=false) p ?(debug_info=false) in_name =
(*	let t1 = Sys.time () in*)
	let in_obj = new mp3read_ptr_2 p in_name in
(*	let in_obj = new mp3read_ptr ~debug:debug_in in_name in*)
	
(*
	let (_, first_frame) = in_obj#find_next_frame in
	let first_data_frame = match first_frame.frameXing with
		| None -> (printDebug "1st frame is data"; first_frame)
		| Some _ -> (printDebug "1st frame is Xing; read another"; snd in_obj#find_next_frame)
	in
	let k = first_frame.frameHeader.headerCommon in
*)

	(* NEW 20070401 *)
	let (new_req, IF_ext first_real_frame, (first_wanted_at, first_got_at), in_xing_option) = (
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
			req_crc           = Req_equal;
			req_bitrate       = Req_any;
			req_samplerate    = Req_equal;
			req_padding       = Req_any;
			req_private       = Req_any;
			req_channel_mode  = Req_any;
			req_channel_count = Req_equal;
			req_copyright     = Req_equal; (* It would seem silly to have only parts of a song copyrighted *)
			req_original      = Req_equal; (* Ditto with original *)
			req_emphasis      = Req_equal; (* And emphasis... *)
		} in
		let (_(*first_req*), IF_ext first_frame, (_ (* 0 *), first_got)) = in_obj#find_next_frame ~force_resync:true ~lame_search:true before_lame_reqs in
		match first_frame.if_xing with
		| None -> (
			(* The first frame was NOT an XING frame; restart and use more strict after_lame_reqs *)
			in_obj#seek first_got;
			let (real_first_req, real_first_frame, (_ (* first_got *), real_first_got)) = in_obj#find_next_frame ~force_resync:true after_lame_reqs in
			(real_first_req, real_first_frame, (first_got, real_first_got), None)
		)
		| Some x -> (
			(* Found an XING frame; do another *)
			let (second_req, second_frame, (second_wanted, second_got)) = in_obj#find_next_frame after_lame_reqs in
			(second_req, second_frame, (second_wanted, second_got), Some x)
		)
	) in

	let bspfk = bspfk_of_samplerate first_real_frame.if_header.header_samplerate in

	let max_reservoir_size = match first_real_frame.if_header.header_id with
		| MPEG1 -> 511
		|   _   -> 255
	in
	
	(* OUTPUT DATA INITIALIZATION *)
	let bQ = Expandarray.create (match in_xing_option with
		| Some {xingNumFrames = Some y} -> y + 2
		| _ -> (in_obj#length / 418) (* Assume 128kbps *)
	) in (* Bitrate queue *)
	let bH = Hashtbl.create 28 in (* Frame bitrate hashtable *)
	let total_frame_size_ref = ref 0 in
	let	total_data_bits_ref = ref 0 in
	let total_data_bytes_ref = ref 0 in
	let most_bits_per_frame_ref = ref 0 in
	let frame_inky_ref = ref 0 in
	let frame_ref = ref (IF_ext first_real_frame) in
	let frame_sync_error_ref = ref (first_wanted_at <> first_got_at) in
	let sync_errors_ref = ref 0 in
	let crc_errors_ref = ref 0 in
	let next_update_ref = ref 0 in
	let next_update_percent_ref = ref 0 in
	let file_length = in_obj#length in
	(try (
		while true do
			let IF_ext f = !frame_ref in
			if !frame_sync_error_ref then incr sync_errors_ref;
			if not f.if_crc_ok then incr crc_errors_ref;
			if not only_bitrate && in_obj#pos > !next_update_ref then (
				Printf.printf "\r%2d%% done on frame %d" !next_update_percent_ref !frame_inky_ref;
				flush stdout;
				next_update_percent_ref += 1;
				next_update_ref := int_of_float (float_of_int file_length *. float_of_int !next_update_percent_ref *. 0.01);
			);
			let side = side_info_of_if_2 f in
			let get_side_data_bits : type id chan. (id,chan) side_t -> int = function
				| {side_bits = Bits_1_mono (a,b)} -> a + b
				| {side_bits = Bits_1_stereo (a,b,c,d)} -> a + b + c + d
				| {side_bits = Bits_2_mono a} -> a
				| {side_bits = Bits_2_stereo (a,b)} -> a + b
			in
			let side_data_bits = get_side_data_bits side in
			total_frame_size_ref += f.if_header.header_bitrate.bitrate_size; (*!frame_ref.frameHeader.headerFrameLength;*)
			total_data_bits_ref += side_data_bits; (*!frame_ref.frameSide.sideDataBits;*)
			total_data_bytes_ref += side.side_bytes; (*(!frame_ref.frameSide.sideDataBits + 7) lsr 3;*)
			Expandarray.add bQ side.side_bytes;
(*Printf.printf " %d" side.side_bytes;*)
			most_bits_per_frame_ref := max !most_bits_per_frame_ref side_data_bits;
			let current = (f.if_header.header_bitrate.bitrate_num, f.if_header.header_padding) in
			(match Hashtbl.mem bH current with
				| false -> Hashtbl.add bH current (ref 1)
				| true -> (Hashtbl.find bH current) += 1
			);
			
			(* Next frame *)
			let (_, next_frame, (next_wanted, next_got)) = in_obj#find_next_frame new_req in
			frame_ref := next_frame;
			frame_inky_ref += 1;
			frame_sync_error_ref := next_wanted <> next_got;
		done;
		in_obj#close;
		assert false (* This is never reached; it's just here to make the typechecker happy *)
	) with
		| End_of_file -> (
			Printf.printf "\r";
			let p = (
				let h f = (Printf.printf "%s\n" f) in
				fun a -> Printf.kprintf h a
			) in
			if not only_bitrate then (
				p "INFO:                                           "; (* A bunch of spaces to clear the percentage-done indicator *)
				p " %s layer 3" (match first_real_frame.if_header.header_id with
					| MPEG1 -> "MPEG1"
					| MPEG2 MPEG20 -> "MPEG2"
					| MPEG2 MPEG25 -> "MPEG2.5"
				);
				p " %d frames" (!frame_inky_ref + 1);
				p " %d Hz" (int_of_samplerate first_real_frame.if_header.header_samplerate);
				p " %f frames per second" (125.0 /. bspfk);
				let sec = (bspfk *. (float_of_int (!frame_inky_ref + 1) *. 0.008)) in (* 0.008 = 1 / 125, to convert bytes to kilobits *)
				p " %f seconds" sec;
			
				let kilobitPerSecondByte = 1.0 /. (bspfk *. float_of_int (!frame_inky_ref + 1)) in
				let total_mp3_data = (!total_data_bytes_ref + (!frame_inky_ref + 1) * (4 + Ptr.Ref.length first_real_frame.if_side_raw)) in
			
				p " %d bytes in file (%f kbps)" in_obj#length (float_of_int in_obj#length *. kilobitPerSecondByte);
				p " %d bytes in MP3 frames (%f kbps) = current bitrate" !total_frame_size_ref (float_of_int !total_frame_size_ref *. kilobitPerSecondByte);
				p " %d bits of payload data (%f kbps)" !total_data_bits_ref (float_of_int !total_data_bits_ref *. kilobitPerSecondByte *. 0.125);
				p " %d bytes of payload data (%f kbps)" !total_data_bytes_ref (float_of_int !total_data_bytes_ref *. kilobitPerSecondByte);
				p " %d bits wasted from partially-full bytes (%f kbps)" (!total_data_bytes_ref lsl 3 - !total_data_bits_ref) (float_of_int (!total_data_bytes_ref lsl 3 - !total_data_bits_ref) *. kilobitPerSecondByte *. 0.125);
				p " %d bytes of MP3 data (%f kbps) = minimum bitrate possible" total_mp3_data (float_of_int total_mp3_data *. kilobitPerSecondByte);
				p " %d bytes of padding (%f kbps)" (!total_frame_size_ref - total_mp3_data) (float_of_int (!total_frame_size_ref - total_mp3_data) *. kilobitPerSecondByte);
				p " %d bytes outside MP3 frames (%f kbps)" (in_obj#length - !total_frame_size_ref) (float_of_int (in_obj#length - !total_frame_size_ref) *. kilobitPerSecondByte);
			
				if !sync_errors_ref = 1 then (p " %d sync error" !sync_errors_ref) else (p " %d sync errors" !sync_errors_ref);
			
				p " Bitrate distribution:";
				Array.iter (fun br ->
					let unpadded = try !(Hashtbl.find bH (br, false)) with _ -> 0 in
					let   padded = try !(Hashtbl.find bH (br,  true)) with _ -> 0 in
					if unpadded <> 0 || padded <> 0 then (
						p "  %3d: %d,%d" br unpadded padded
					);
				) (bitrate_array_of_id first_real_frame.if_header.header_id);
			
				p " Largest frame uses %d bits = %d bytes = %f kbps" !most_bits_per_frame_ref ((!most_bits_per_frame_ref + 7) lsr 3) (float_of_int !most_bits_per_frame_ref *. 0.125 /. bspfk);
			); (* If not only_bitrate *)

			(***********************************)
			(* Figure out smallest CBR bitrate *)
			(***********************************)
			let (_(*number_to_bitrate*), bytes_to_bitrate) = (
				let (max_bitrate, lists) = match first_real_frame.if_header.header_id with
					| MPEG1 -> (320, [(1, 32);(2, 40);(3, 48);(4, 45);(5, 64);(6, 80);(7, 96);(8,112);(9,128);(10,160);(11,192);(12,224);(13,256);(14,320)])
					|   _   -> (160, [(1,  8);(2, 16);(3, 24);(4, 32);(5, 40);(6, 48);(7, 56);(8, 64);(9, 80);(10, 96);(11,112);(12,128);(13,144);(14,160)])
				in
				(fun num i ->
					let exact = List.exists (fun (_,a) -> a = num) lists in (* Did the caller specify an exact bitrate? *)
					let exactP1 = List.exists (fun (_,a) -> a + 1 = num) lists in (* Did the caller specify 1 more than an exact bitrate? *)
					let over = (num > max_bitrate) in (* Is the caller Way Out There? *)
					let padded = match (exact, exactP1 || over) with
						| ( true,  _  ) -> padded_frame first_real_frame.if_header.header_samplerate num i
						| (false, true) -> true
						| (false,false) -> false
					in
					let (index,real_bitrate) = try (List.find (fun (_,a) -> num <= a + 1) lists) with Not_found -> (14,max_bitrate) in
					let unpad_length = unpadded_frame_length first_real_frame.if_header.header_samplerate real_bitrate in
					let pad_add = if padded then 1 else 0 in
					{
						bitrate_num = real_bitrate;
						bitrate_padding = padded;
						bitrate_size = unpad_length + pad_add;
						bitrate_data = unpad_length + pad_add - 4 - Ptr.Ref.length first_real_frame.if_side_raw;
						bitrate_index = index
					}
				), (fun bytes ->
					let bph = bytes + 4 + Ptr.Ref.length first_real_frame.if_side_raw in (* bytes plus header *)
					let out = ref None in
					List.iter (fun (index, real_bitrate) ->
						match !out with
						| Some _ -> ()
						| None -> (
							let bytes_unpadded = unpadded_frame_length first_real_frame.if_header.header_samplerate real_bitrate in
							if bytes_unpadded >= bph then (
								(* OK without padding *)
								out := Some {
									bitrate_num = real_bitrate;
									bitrate_padding = false;
									bitrate_size = bytes_unpadded;
									bitrate_data = bytes_unpadded - 4 - Ptr.Ref.length first_real_frame.if_side_raw;
									bitrate_index = index
								}
							) else if bytes_unpadded + 1 >= bph then (
								(* Needs padding *)
								out := Some {
									bitrate_num = real_bitrate;
									bitrate_padding = true;
									bitrate_size = bytes_unpadded + 1;
									bitrate_data = bytes_unpadded + 1 - 4 - Ptr.Ref.length first_real_frame.if_side_raw;
									bitrate_index = index
								}
							) (* else keep going *)
						)
					) lists;
					if debug_info then (
						Printf.printf "   Finding a spot for %d bytes (%d)\n" bytes bph;
						flush stdout
					);
					match !out with
					| None -> raise Too_many_bytes (* No valid frame found! *)
					| Some x -> x
				)
			) in

			let check_bitrate br_index = (
				let br_num = (bitrate_array_of_id first_real_frame.if_header.header_id).(br_index) in
				let res_ref = ref 0 in
				let br_unpadded_bytes = unpadded_frame_length first_real_frame.if_header.header_samplerate br_num - 4 - Ptr.Ref.length first_real_frame.if_side_raw in
				let br_padded_bytes = br_unpadded_bytes + 1 in
				if debug_info then p "  %d (%d) %d" br_index br_num br_unpadded_bytes;
				let find_padded_frame = padded_frame first_real_frame.if_header.header_samplerate br_num in
				try (
					Expandarray.iteri (fun i bytes ->
						res_ref := min !res_ref max_reservoir_size;
(*						let max_bytes = number_to_bitrate bytes i in*)
						let max_frame_data = if find_padded_frame i then br_padded_bytes else br_unpadded_bytes in
						if debug_info then p "   %5d = %d %d (%d)" i bytes max_frame_data !res_ref;
						res_ref := !res_ref + max_frame_data - bytes;
						if !res_ref < 0 then ( (* ERREUR! *)
							raise Loop_end;
						)
					) bQ;
					true
				) with
				| Loop_end -> false
			) in

			let rec check_bitrate_rec now last = (
				if now > last then (
					(bitrate_array_of_id first_real_frame.if_header.header_id).(14)
				) else (
					match (check_bitrate now, now = last) with
					| (false, false) -> check_bitrate_rec (now + 1) last
					| (false, true) -> (bitrate_array_of_id first_real_frame.if_header.header_id).(14)
					| (true, _) -> (bitrate_array_of_id first_real_frame.if_header.header_id).(now)
				)
			) in
(*
			for br_index = (bytes_to_bitrate (!most_bits_per_frame_ref lsr 3 - max_reservoir_size - 1)).bitrateIndex to Array.length k.common_bitrate_index - 1 do
			done;
*)
			let min_cbr_br = check_bitrate_rec (bytes_to_bitrate (!most_bits_per_frame_ref lsr 3 - max_reservoir_size - 1)).bitrate_index (13) in
			if only_bitrate then (
				p "%d" min_cbr_br
			) else (
				p " Smallest bitrate for CBR is %d" min_cbr_br
			);

(*			Printf.printf "TIME: %f\n" (Sys.time () -. t1);*)

			in_obj#close;

			flush stdout;

			(0,!sync_errors_ref,!crc_errors_ref,0)
		)
	);
;;

