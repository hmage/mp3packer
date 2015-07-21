open Types;;
open Mp3read;;

let in_obj = new mp3read_ptr ~debug:false Sys.argv.(1);;


let (new_req, first_frame, (first_wanted_at, first_got_at), in_xing_option) = (
	let before_lame_reqs = {
		req_id           = Req_equal;
		req_crc          = Req_any;
		req_bitrate      = Req_any;
		req_samplerate   = Req_equal;
		req_padding      = Req_any;
		req_private      = Req_any;
		req_channel_mode = Req_any;
		req_ms           = Req_any;
		req_is           = Req_any;
		req_copyright    = Req_any;
		req_original     = Req_any;
		req_emphasis     = Req_any;
	} in
	let after_lame_reqs = {
		req_id           = Req_equal;
		req_crc          = Req_equal;
		req_bitrate      = Req_any;
		req_samplerate   = Req_equal;
		req_padding      = Req_any;
		req_private      = Req_any;
		req_channel_mode = Req_equal;
		req_ms           = Req_any;   (* MS and IS can change if channel_mode is JS, and are ignored otherwise *)
		req_is           = Req_any;
		req_copyright    = Req_equal; (* It would seem silly to have only parts of a song copyrighted *)
		req_original     = Req_any;   (* Req_any has had some problems in the past *)
		req_emphasis     = Req_equal; (* And emphasis... *)
	} in
	let (first_req, first_frame, (_ (* 0 *), first_got)) = in_obj#find_next_frame ~force_resync:true ~lame_search:true before_lame_reqs in
	match first_frame.if_xing with
	| None -> (
		(* The first frame was NOT an XING frame; restart and use more strict after_lame_reqs *)
		in_obj#seek first_got;
		let (real_first_req, real_first_frame, (_ (* first_got *), real_first_got)) = in_obj#find_next_frame ~force_resync:true after_lame_reqs in
		(real_first_req, real_first_frame, (first_got, real_first_got), None)
	)
	| Some x -> (
		(* Found an XING frame; do another *)
		let (second_req, second_frame, (second_wanted, second_got)) = in_obj#find_next_frame ~force_resync:true after_lame_reqs in
		(second_req, second_frame, (second_wanted, second_got), Some x)
	)
);;
in_obj#seek first_got_at;;

Printf.printf "XING present? %B\n" (match in_xing_option with | None -> false | Some _ -> true);;
Printf.printf "Found first real frame at %d\n" first_got_at;;

match new_req with
	| {req_id           = Req_matches x} -> (Printf.printf " ID           [%s ]\n" (List.fold_left (fun s n -> s ^ " " ^ (string_of_mpeg     n)) "" x))
	| {req_id           = Req_any      } -> (Printf.printf " ID           ANY\n")
	| {req_id           = Req_equal    } -> (Printf.printf " ID           ?\n")
;;
match new_req with
	| {req_crc          = Req_matches x} -> (Printf.printf " CRC          [%s ]\n" (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x))
	| {req_crc          = Req_any      } -> (Printf.printf " CRC          ANY\n")
	| {req_crc          = Req_equal    } -> (Printf.printf " CRC          ?\n")
;;
match new_req with
	| {req_bitrate      = Req_matches x} -> (Printf.printf " Bitrate      [%s ]\n" (List.fold_left (fun s n -> s ^ " " ^ (string_of_int      n)) "" x))
	| {req_bitrate      = Req_any      } -> (Printf.printf " Bitrate      ANY\n")
	| {req_bitrate      = Req_equal    } -> (Printf.printf " Bitrate      ?\n")
;;
match new_req with
	| {req_samplerate   = Req_matches x} -> (Printf.printf " Samplerate   [%s ]\n" (List.fold_left (fun s n -> s ^ " " ^ (string_of_int (int_of_samplerate n))) "" x))
	| {req_samplerate   = Req_any      } -> (Printf.printf " Samplerate   ANY\n")
	| {req_samplerate   = Req_equal    } -> (Printf.printf " Samplerate   ?\n")
;;
match new_req with
	| {req_padding      = Req_matches x} -> (Printf.printf " Padding      [%s ]\n" (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x))
	| {req_padding      = Req_any      } -> (Printf.printf " Padding      ANY\n")
	| {req_padding      = Req_equal    } -> (Printf.printf " Padding      ?\n")
;;
match new_req with
	| {req_private      = Req_matches x} -> (Printf.printf " Private      [%s ]\n" (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x))
	| {req_private      = Req_any      } -> (Printf.printf " Private      ANY\n")
	| {req_private      = Req_equal    } -> (Printf.printf " Private      ?\n")
;;
match new_req with
	| {req_channel_mode = Req_matches x} -> (Printf.printf " Channel mode [%s ]\n" (List.fold_left (fun s n -> s ^ " " ^ (string_of_channel  n)) "" x))
	| {req_channel_mode = Req_any      } -> (Printf.printf " Channel mode ANY\n")
	| {req_channel_mode = Req_equal    } -> (Printf.printf " Channel mode ?\n")
;;
match new_req with
	| {req_ms           = Req_matches x} -> (Printf.printf " MS           [%s ]\n" (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x))
	| {req_ms           = Req_any      } -> (Printf.printf " MS           ANY\n")
	| {req_ms           = Req_equal    } -> (Printf.printf " MS           ?\n")
;;
match new_req with
	| {req_is           = Req_matches x} -> (Printf.printf " IS           [%s ]\n" (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x))
	| {req_is           = Req_any      } -> (Printf.printf " IS           ANY\n")
	| {req_is           = Req_equal    } -> (Printf.printf " IS           ?\n")
;;
match new_req with
	| {req_copyright    = Req_matches x} -> (Printf.printf " Copyright    [%s ]\n" (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x))
	| {req_copyright    = Req_any      } -> (Printf.printf " Copyright    ANY\n")
	| {req_copyright    = Req_equal    } -> (Printf.printf " Copyright    ?\n")
;;
match new_req with
	| {req_original     = Req_matches x} -> (Printf.printf " Original     [%s ]\n" (List.fold_left (fun s n -> s ^ " " ^ (string_of_bool     n)) "" x))
	| {req_original     = Req_any      } -> (Printf.printf " Original     ANY\n")
	| {req_original     = Req_equal    } -> (Printf.printf " Original     ?\n")
;;
match new_req with
	| {req_emphasis     = Req_matches x} -> (Printf.printf " Emphasis     [%s ]\n" (List.fold_left (fun s n -> s ^ " " ^ (string_of_emphasis n)) "" x))
	| {req_emphasis     = Req_any      } -> (Printf.printf " Emphasis     ANY\n")
	| {req_emphasis     = Req_equal    } -> (Printf.printf " Emphasis     ?\n")
;;

let frame_offsets = List2.create ();;
let frame_bits = List2.create ();;
let frame_bits_used = List2.create ();;
let frame_bitrate = List2.create ();;

let rec grab_frame frame_num = (

	let frame_stuff = (try
		Some (in_obj#find_next_frame new_req)
	with
		End_of_file -> None
	) in
	
	match frame_stuff with
	| Some (_, if_now, (wanted_at, got_at)) -> (
		let side = side_info_of_if if_now in
		List2.append frame_offsets side.side_offset;
		List2.append frame_bits (Ptr.Ref.length if_now.if_data_raw lsl 3);
		List2.append frame_bits_used (Array.fold_left (+) 0 side.side_bits);
		List2.append frame_bitrate if_now.if_header.header_bitrate;
		grab_frame (succ frame_num)
	)
	| None -> (
		frame_num
	)
);;

let num_frames = grab_frame 0;;
Printf.printf "Found %d frames total\n" num_frames;;

Printf.printf "offsets = {\n";;
List2.fold (fun so_far x ->
	if so_far = List2.length frame_offsets then Printf.printf "%d" x else Printf.printf "%d," x;
	(succ so_far)
) 1 frame_offsets;;
Printf.printf "\n};\n";;

Printf.printf "framebits = {\n";;
List2.fold (fun so_far x ->
	if so_far = List2.length frame_bits then Printf.printf "%d" x else Printf.printf "%d," x;
	(succ so_far)
) 1 frame_bits;;
Printf.printf "\n};\n";;

Printf.printf "usedbits = {\n";;
List2.fold (fun so_far x ->
	if so_far = List2.length frame_bits_used then Printf.printf "%d" x else Printf.printf "%d," x;
	(succ so_far)
) 1 frame_bits_used;;
Printf.printf "\n};\n";;

Printf.printf "bitrate = {\n";;
List2.fold (fun so_far x ->
	if so_far = List2.length frame_bitrate then Printf.printf "%d" x else Printf.printf "%d," x;
	(succ so_far)
) 1 frame_bitrate;;
Printf.printf "\n};\n";;
