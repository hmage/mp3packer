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

(* Unicode argv *)
let argv = match Unicode.argv_opt with
	| Unicode.Normal u -> u
	| Unicode.Error _ -> Sys.argv
;;

type ('a,'b) error_t = Normal of 'a | Error of 'b;;

let (@@) a b = a b;;

let trap_exception a b =
	try
		Normal (a b)
	with
		e -> Error e
;;
let trap_exception_2 a b c =
	try
		Normal (a b c)
	with
		e -> Error e
;;

let make_exception = function
	| Normal x -> x
	| Error e -> raise e
;;

let to_hex s =
  let result = String.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    String.blit (Printf.sprintf "%02X" (int_of_char s.[i])) 0 result (2*i) 2;
  done;
  result;;

let to_bin =
	let lookup = [| 128;64;32;16;8;4;2;1 |] in
	fun s -> (
		let result = String.create (8 * String.length s) in
		for chr = 0 to String.length s - 1 do
			let code = Char.code s.[chr] in
			for bit = 0 to 7 do
				result.[(chr lsl 3) lor bit] <- if (code land lookup.(bit) = 0) then '0' else '1'
			done
		done;
		result
	)
;;



(* STUFF FOR THE LIST-BASED PRINTING FUNCTIONS *)
type p_type =
	| Bool of bool
	| Int of int
	| IntN of int * int (* width, int *)
	| Int0N of int * int (* 0-padded IntN *)
(*	| Int64 of int64*)
	| Hex of int * int (* width, int (always filled with 0) *)
	| Float of float
	| FloatN of int * float (* width, int -- like "%*f" *)
	| Float_N of int * float (* Equivalent to "%.*f" *)
	| FloatN_N of int * int * float (* "%*.*f" *)
	| Str of string
	| StrN of int * string
	| StrS of string (* Escaped like %S *)
	| StrNS of int * string
	| Char of char
	| Spaces of int (* Just some number of spaces *)
	| Ptr of Ptr.t
	| Subptr of Ptr.t * int * int (* ptr, offset, length *)
	| Ptrref of Ptr.Ref.ref_t
	| Fun of ((p_type -> unit) -> unit)
	| List of p_type list
;;


(* Small, handy functions *)

(* Some functions depend on these identities *)
assert (max_int / 2 + 1 = 1 lsl (Sys.word_size - 3));;
assert (min_int = 1 lsl (Sys.word_size - 2));;

(* if x = 0 then 0 else 1 *)
let one_if_not_0 x = (x lor ~-x) lsr (Sys.word_size - 2);;
(* if x = 0 then 0 else -1 *)
let neg_one_if_not_0 x = (x lor ~-x) asr (Sys.word_size - 2);;




(* File name functions *)
let strip_multiple_slashes =
	let regexp = Str.regexp "[/\\\\]+" in
	fun str -> Str.global_replace regexp "/" str
;;
let strip_trailing_slash str =
	let str2 = strip_multiple_slashes str in
	if String.length str2 = 0 then (
		str2
	) else if str2.[String.length str2 - 1] = '/' then (
		String.sub str2 0 (String.length str2 - 1)
	) else (
		str2
	)
;;
let append_before_extension str app =
	(try
		let dot = String.rindex str '.' in
		let before = String.sub str 0 dot in
		let after = String.sub str dot (String.length str - dot) in
		before ^ app ^ after
	with
		Not_found -> str ^ app
	)
;;

(* Other file functions *)
type io_t = IO_File of string | IO_Dir of string;;
let string_of_io = function
	| IO_File x -> Printf.sprintf "File '%s'" x
	| IO_Dir x -> Printf.sprintf "Dir '%s'" x
;;
(*
let find_file x = (try
	Some (Unix.stat x).Unix.st_kind
with
	| Unix.Unix_error _ -> None
);;
*)
let find_file x = (try
	Some (Unicode.stat_utf8 x).Unix.st_kind
with
	| _ -> None
);;




(* Used for printing debug stuff *)
let nul = open_out (if Sys.os_type = "Win32" then "NUL" else "/dev/null");;

exception Loop_end;;

type 'a option3_t = Zero | One of 'a | Many of 'a option3_t array;;
type 'a onemany_t = Single of 'a | Multiple of 'a onemany_t array;;

let rec print_option3_array ?(tab="") option3_array stringify =
	Array.iteri (fun i x ->
		Printf.printf "%s%3d " tab i;
		match x with
		| Zero -> Printf.printf "NONE\n"
		| One y -> Printf.printf "ONE %S\n" (stringify y)
		| Many y -> (Printf.printf "MANY:\n"; print_option3_array ~tab:(tab ^ "  ") y stringify)
	) option3_array
;;

type lameTag_t = {
	lameRevision : int;
	lameVBRMethod : int;
	lameLowpass : int;
	lamePeakAmplitude : float;
	lameRGTrack : int;
	lameRGAlbum : int;
	lameNSPsyTune : bool;
	lameNSSafeJoint : bool;
	lameNoGapPrev : bool;
	lameNoGapNext : bool;
	lameATHType : int;
	lameABRBitrate : int;
	lameDelayStart : int;
	lameDelayEnd : int;
	lameNoiseShaping : int;
	lameStereoMode : int;
	lameUnwise : bool;
	lameSourceFrequency : int;
	lameMP3Gain : int;
	lameSurround : int;
	lamePreset : int;
	lameMusicLength : int;
	lameMusicCRC : int
};;
(*type isLame_t = IsLame of lameTag_t | IsNotLame;;*)
type xingTag_t = {
	xingRawTag : string;
	xingTagType : string;
	xingNumFrames : int option; (* No fair having MP3 files longer than 324 days *)
	xingNumBytes : int option; (* No fair having MP3 files larger than 1GB *)
	xingTOC : int array option;
	xingQuality : int option;
	xingEncoder : string;
	xingLame : lameTag_t option;
};;


type process_set_t = SSE41 | Set_base;;


(* NEW FOR THE WORKERS *)
type (*'a,'b*) queue_input_t = {
	q_silent : bool;
	q_debug_in : bool;
	q_debug_queue : bool;
	q_debug_recompress : bool;
	q_min_bitrate : int;
	q_delete_beginning_junk : bool;
	q_delete_end_junk : bool;
	q_padding : string;
	q_recompress : bool;
	q_process_set : process_set_t;
	q_zero_whole_bad_frame : bool;
	q_minimize_bit_reservoir : bool;

	q_print_in : p_type list -> unit;
	q_print_queue : p_type list -> unit;
	q_print_recompress : p_type list -> unit;
};;

class file_state =
	object

		(* Everything under this is protected by this mutex *)
		val m = Mutex.create ()
		val mutable freq_warn = true
		val mutable buffer_errors = 0
		val mutable sync_errors = 0
		val mutable crc_errors = 0
		val mutable rehuff_errors = 0
		method freq_warn = (
			Mutex.lock m;
			let got = freq_warn in
			freq_warn <- false;
			Mutex.unlock m;
			got
		)
		method add_buffer_error = (
			Mutex.lock m;
			buffer_errors <- succ buffer_errors;
			Mutex.unlock m;
		)
		method add_sync_error = (
			Mutex.lock m;
			sync_errors <- succ sync_errors;
			Mutex.unlock m;
		)
		method add_crc_error = (
			Mutex.lock m;
			crc_errors <- succ crc_errors;
			Mutex.unlock m;
		)
		method add_rehuff_error = (
			Mutex.lock m;
			rehuff_errors <- succ rehuff_errors;
			Mutex.unlock m;
		)
		method get_errors = (
			Mutex.lock m;
			let ret = (buffer_errors, sync_errors, crc_errors, rehuff_errors) in
			Mutex.unlock m;
			ret
		)
	end
;;

type worker_file_t = {
	worker_file_index : int;
	worker_file_input : string;
	worker_file_output: string;
};;
type (*'a,'b*) worker_do_t = 
	| Worker_queue of (*'a,'b*) queue_input_t
	| Worker_do of worker_file_t
	| Worker_finish
;;

type worker_ok_file_t = {
	worker_ok_index : int;
	worker_output_result : (int * int * int);
};;




(******)
(* C! *)
(******)

(* TEMP WINDOWS COUNTER *)
external get_counter_freq : unit -> int = "c_part_counter_freq" "noalloc";;
let counter_freq = get_counter_freq ();;
external counter : unit -> int = "c_part_counter" "noalloc";;


(* This function is not portable, but it won't be used with non-Windows OSes. That's what Unix.nice is for. *)
external nice_c : int -> int = "caml_nice";;

let nice = match Sys.os_type with
	| "Unix" -> Unix.nice
	| _ -> nice_c
;;

let detected_processors = try 
	match Sys.os_type with
	| "Win32" -> (
		let proc = Unix.open_process_in "echo %NUMBER_OF_PROCESSORS%" in
		let s = input_line proc in
		ignore (Unix.close_process_in proc);
		Scanf.sscanf s "%d" (fun x -> x)
	)
	| _ -> (
		let proc = Unix.open_process_in "uname" in
		let n = input_line proc in
		ignore (Unix.close_process_in proc);
		let i = match n with
			| "Linux" -> (
				let proc = Unix.open_process_in "grep -c processor /proc/cpuinfo" in
				let s = input_line proc in
				ignore (Unix.close_process_in proc);
				int_of_string s
			)
			| "Darwin" -> (
				let proc = Unix.open_process_in "sysctl hw.logicalcpu" in
				let s = input_line proc in
				ignore (Unix.close_process_in proc);
				Scanf.sscanf s "%s %d" (fun _ b -> b)
			)
			| _ -> (
				(* Other *)
				0
			)
		in
		i
	)
with
	_ -> 0
;;

type capabilities_t = {
	cap_sse : bool;
	cap_sse2 : bool;
	cap_sse3 : bool;
	cap_ssse3 : bool;
	cap_sse41 : bool;
};;
external get_capabilities : unit -> capabilities_t = "get_capabilities";;
let capabilities = get_capabilities ();;
let sse41_ok = capabilities.cap_sse && capabilities.cap_sse2 && capabilities.cap_sse3 && capabilities.cap_ssse3 && capabilities.cap_sse41;;

type os_thread_id;; (* I think this is the same as a Unix.file_descr *)
external get_os_thread_self_id : unit -> os_thread_id = "get_os_thread_self_id";;
external thread_is_alive : os_thread_id -> bool = "thread_is_alive";;

external copy_file_times : Unix.file_descr -> Unix.file_descr -> bool = "copy_file_times";;
let copy_file_times_by_name source target =
	match trap_exception (Unicode.openfile_utf8 source [Unix.O_RDONLY]) 0o600 with
	| Normal h_source -> (
		let ret =
			match trap_exception (Unicode.openfile_utf8 target [Unix.O_WRONLY]) 0o600 with
			| Normal h_target -> (
				let ret =
					if copy_file_times h_source h_target then (
						Normal ()
					) else (
						Error (Failure "copy_file_times failed")
					)
				in
				ignore @@ trap_exception Unix.close h_target;
				ret
			)
			| Error e -> Error e
		in
		ignore @@ trap_exception Unix.close h_source;
		ret
	)
	| Error e -> Error e
;;



(* C PRINTING THINGS *)
(*
Callback.register "Buffer__add_char" Buffer.add_char;;
external p_print_int_test : Buffer.t -> int -> unit = "p_print_int_test";;
*)



exception Too_many_bytes;;

(* General form of if_perhaps_t *)
type 'a good_bad_eof = Ret_eof | Ret_none | Ret_some of 'a;;

type mpeg1_t;;
type mpeg20_t;;
type mpeg25_t;;
type _ mpeg2_t =
	| MPEG20 : mpeg20_t mpeg2_t
	| MPEG25 : mpeg25_t mpeg2_t
;;
type _ mpeg_t =
	| MPEG1 : mpeg1_t mpeg_t
	| MPEG2 : 'a mpeg2_t -> 'a mpeg2_t mpeg_t
;;
type mpeg_ext = MPEG_ext : 'a mpeg_t -> mpeg_ext;;
let mpeg_equal a b = (MPEG_ext a = MPEG_ext b);;

type _ samplerate_t =
	| S48000 : mpeg1_t samplerate_t
	| S44100 : mpeg1_t samplerate_t
	| S32000 : mpeg1_t samplerate_t
	| S24000 : mpeg20_t mpeg2_t samplerate_t
	| S22050 : mpeg20_t mpeg2_t samplerate_t
	| S16000 : mpeg20_t mpeg2_t samplerate_t
	| S12000 : mpeg25_t mpeg2_t samplerate_t
	| S11025 : mpeg25_t mpeg2_t samplerate_t
	| S8000  : mpeg25_t mpeg2_t samplerate_t
;;
type samplerate_ext = Samplerate_ext : 'a samplerate_t -> samplerate_ext;;
let samplerate_equal a b = (Samplerate_ext a = Samplerate_ext b);;

type channel_mono_t;;
type channel_js_t = {js_ms : bool; js_is : bool};;
type channel_stereo_t = Stereo_simple | Stereo_joint of channel_js_t | Stereo_dual;;

type _ channel_t =
	| Mono : channel_mono_t channel_t
	| Stereo : channel_stereo_t -> channel_stereo_t channel_t
;;
type channel_ext = Channel_ext : 'chan channel_t -> channel_ext;;
let channel_equal a b = (Channel_ext a = Channel_ext b);;
let channel_mode_equal : type a b. a channel_t -> b channel_t -> bool = fun a b ->
	match (a,b) with
	| (Mono, Mono) -> true
	| (Stereo Stereo_simple, Stereo Stereo_simple) -> true
	| (Stereo Stereo_joint _, Stereo Stereo_joint _) -> true
	| (Stereo Stereo_dual, Stereo Stereo_dual) -> true
	| _ -> false
;;
let channel_count_equal : type a b. a channel_t -> b channel_t -> bool = fun a b ->
	match (a,b) with
	| (Mono, Mono) -> true
	| (Mono, _) -> false
	| (_, Mono) -> false
	| _ -> true
;;

type bitrate_t = {
	bitrate_data : int; (* This is NOT counting the CRC bytes! Subtract 2 if the frame uses CRC *)
	bitrate_size : int;
	bitrate_num : int;
	bitrate_padding : bool;
	bitrate_index : int;
};;

type emphasis_t = Emphasis_none | Emphasis_5015 | Emphasis_CCITT;;

type ('id,'chan) header_t = {
	header_raw : Ptr.Ref.ref_t;
	header_id : 'id mpeg_t;
	header_crc : bool;
	header_bitrate : bitrate_t;
	header_samplerate : 'id samplerate_t;
	header_padding : bool;
	header_private : bool;
	header_channel_mode : 'chan channel_t;
	header_copyright : bool;
	header_original : bool;
	header_emphasis : emphasis_t;
};;
type header_ext = Header_ext : ('id,'chan) header_t -> header_ext;;

type (_,_) side_bits_t =
	| Bits_1_mono   : int * int -> (mpeg1_t, channel_mono_t) side_bits_t
	| Bits_1_stereo : int * int * int * int -> (mpeg1_t, channel_stereo_t) side_bits_t
	| Bits_2_mono   : int -> (_ mpeg2_t, channel_mono_t) side_bits_t
	| Bits_2_stereo : int * int -> (_ mpeg2_t, channel_stereo_t) side_bits_t
;;

type ('id,'chan) side_t = {
	side_raw : Ptr.Ref.ref_t;
	side_offset : int;
	side_bits : ('id,'chan) side_bits_t;
	side_bytes : int;
};;


(* Now to stuff everything into a frame (of various types) *)
type ('id,'chan) input_frame_t = {
	if_raw : Ptr.Ref.ref_t;
	if_header : ('id,'chan) header_t;
	if_side_raw : Ptr.Ref.ref_t;
	if_data_raw : Ptr.Ref.ref_t;
	if_crc_ok : bool;
	mutable if_xing : xingTag_t option;
};;
type input_frame_ext = IF_ext : ('id,'chan) input_frame_t -> input_frame_ext;;

type ('id,'chan) f1_t = {
	f1_num : int;
	f1_header : ('id,'chan) header_t;
	f1_side : ('id,'chan) side_t;
	mutable f1_data : Ptr.Ref.ref_t;
	mutable f1_pad_exact : int option;
};;
type f1_ext = F1_ext : ('id,'chan) f1_t -> f1_ext;;

type ('id,'chan) f2_t = {
	f2_num : int;
	f2_header : ('id,'chan) header_t;
	f2_side : ('id,'chan) side_t;
	f2_bitrate : bitrate_t;
	f2_data : Ptr.Ref.ref_t;
	f2_pad : int;
	mutable f2_offset : int;
	mutable f2_bytes_left : int;
	mutable f2_flag : bool;
	mutable f2_check_output : bool; (* if the frame may have a gap before it (or before any previous frame, if needed) *)
};;
type f2_ext = F2_ext : ('id,'chan) f2_t -> f2_ext;;

type ('id,'chan) f3_t = {
	f3_num : int;
	f3_header_side_raw : Ptr.Ref.ref_t;
	mutable f3_output_data : Ptr.Ref.ref_t;
	f3_bitrate : bitrate_t;
	mutable f3_flag : bool;
};;
type f3_ext = F3_ext : ('id,'chan) f3_t -> f3_ext;;

type 'a req_t = Req_any | Req_equal | Req_matches of 'a list;;

(* Have to redefine this since the ids and samplerates are different *)
(* Use the _ext versions of the types to avoid nastiness *)
type reqs_t = {
	req_id            : mpeg_ext req_t;
	req_crc           : bool req_t;
	req_bitrate       : int req_t;
	req_samplerate    : samplerate_ext req_t;
	req_padding       : bool req_t;
	req_private       : bool req_t;
	req_channel_mode  : channel_ext req_t;
	req_channel_count : int req_t;
	req_copyright     : bool req_t;
	req_original      : bool req_t;
	req_emphasis      : emphasis_t req_t;
};;




(* Type-related definitions *)
let header_bytes = 4;;
let side_bytes_of_id_channel : type id chan. id mpeg_t -> chan channel_t -> int = function
	| MPEG1 -> (function
		| Mono -> 17
		| Stereo _ -> 32
	)
	| MPEG2 _ -> (function
		| Mono -> 9
		| Stereo _ -> 17
	)
;;
let side_bytes_of_header : type id chan. (id,chan) header_t -> int = fun h -> side_bytes_of_id_channel h.header_id h.header_channel_mode;;

let string_of_mpeg : type id. id mpeg_t -> string = function
	| MPEG1 -> "MPEG1"
	| MPEG2 MPEG20 -> "MPEG2"
	| MPEG2 MPEG25 -> "MPEG25"
;;
let string_of_mpeg_ext (MPEG_ext x) = string_of_mpeg x;;
let int_of_samplerate : type id. id samplerate_t -> int = function
	| S48000 -> 48000
	| S44100 -> 44100
	| S32000 -> 32000
	| S24000 -> 24000
	| S22050 -> 22050
	| S16000 -> 16000
	| S12000 -> 12000
	| S11025 -> 11025
	|  S8000 ->  8000
;;
let float_of_samplerate : type id. id samplerate_t -> float = function
	| S48000 -> 48000.
	| S44100 -> 44100.
	| S32000 -> 32000.
	| S24000 -> 24000.
	| S22050 -> 22050.
	| S16000 -> 16000.
	| S12000 -> 12000.
	| S11025 -> 11025.
	|  S8000 ->  8000.
;;
let string_of_samplerate sr = string_of_int (int_of_samplerate sr);;
let string_of_samplerate_ext (Samplerate_ext sr) = string_of_samplerate sr;;

let count_of_channel : type chan. chan channel_t -> int = function
	| Mono -> 1
	| Stereo _ -> 2
;;
let count_of_channel_ext (Channel_ext c) = count_of_channel c;;
let string_of_channel : type chan. chan channel_t -> string = function
	| Mono -> "Mono"
	| Stereo Stereo_simple -> "SimpleStereo"
	| Stereo (Stereo_joint _) -> "JointStereo"
	| Stereo Stereo_dual -> "DualStereo"
;;
let string_of_channel_ext (Channel_ext c) = string_of_channel c;;

(* Byte seconds per frame kilobit *)
let bspfk_of_samplerate : type id. id samplerate_t -> _ = function
	| S48000 -> 3.
	| S44100 -> 160. /. 49.
	| S32000 -> 9. /. 2.
	| S24000 -> 3.
	| S22050 -> 160. /. 49.
	| S16000 -> 9. /. 2.
	| S12000 -> 6.
	| S11025 -> 320. /. 49.
	| S8000  -> 9.
;;

let mpeg_of_samplerate : type id. id samplerate_t -> id mpeg_t = function
	| S48000 -> MPEG1
	| S44100 -> MPEG1
	| S32000 -> MPEG1
	| S24000 -> MPEG2 MPEG20
	| S22050 -> MPEG2 MPEG20
	| S16000 -> MPEG2 MPEG20
	| S12000 -> MPEG2 MPEG25
	| S11025 -> MPEG2 MPEG25
	| S8000  -> MPEG2 MPEG25
;;

let unpadded_frame_length : type id. id samplerate_t -> int -> int = fun samplerate bitrate ->
	match mpeg_of_samplerate samplerate with
	| MPEG1 -> 144000 * bitrate / int_of_samplerate samplerate
	| MPEG2 _ -> 72000 * bitrate / int_of_samplerate samplerate
;;

let bitrate_array_of_id : type id. id mpeg_t -> int array = function
	| MPEG1 ->   [| 0;32;40;48;56;64;80;96;112;128;160;192;224;256;320 |]
	| MPEG2 _ -> [| 0; 8;16;24;32;40;48;56; 64; 80; 96;112;128;144;160 |]
;;
let samplerate_array_of_id : type id. id mpeg_t -> id samplerate_t array = function
	| MPEG1 ->        [| S44100;S48000;S32000 |]
	| MPEG2 MPEG20 -> [| S22050;S24000;S16000 |]
	| MPEG2 MPEG25 -> [| S11025;S12000; S8000 |]
;;

let id_index_of_samplerate : type id. id samplerate_t -> int = function
	| S44100 -> 0
	| S48000 -> 1
	| S32000 -> 2
	| S22050 -> 0
	| S24000 -> 1
	| S16000 -> 2
	| S11025 -> 0
	| S12000 -> 1
	| S8000  -> 2
;;

let bitrate_of_samplerate_and_index : type id chan. id samplerate_t -> chan channel_t -> _ = fun sr chan ->
	let unpadded_frame_length_of_bitrate_int = unpadded_frame_length sr in
	let mpeg_id = mpeg_of_samplerate sr in
	let side_info_size = side_bytes_of_id_channel mpeg_id chan in
	let bitrate_array = bitrate_array_of_id mpeg_id in
	fun index padding -> (
		if index >= Array.length bitrate_array || index < 0 then failwith "bitrate_of_samplerate_and_index invalid index";
		let bitrate_int = bitrate_array.(index) in
		let frame_bytes = unpadded_frame_length_of_bitrate_int bitrate_int in
		let add_pad = if padding then 1 else 0 in
		{
			bitrate_data = frame_bytes - header_bytes - side_info_size + add_pad;
			bitrate_size = frame_bytes + add_pad;
			bitrate_num = bitrate_int;
			bitrate_padding = padding;
			bitrate_index = index;
		}
	)
;;

let string_of_emphasis = function
	| Emphasis_none -> "None"
	| Emphasis_5015 -> "50/15"
	| Emphasis_CCITT -> "CCITT"
;;


let padded_frame : type id. id samplerate_t -> int -> int -> bool =
	let l = true in
	let o = false in
	function
		| S44100 -> (fun bitrate frameno ->
			let f = frameno mod 49 in
			match bitrate with
			|  32 -> [| o;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l|].(f)
			|  40 -> [| o;l;o;l;l;o;l;o;l;l;o;l;o;l;l;o;l;l;o;l;o;l;l;o;l;o;l;l;o;l;o;l;l;o;l;l;o;l;o;l;l;o;l;o;l;l;o;l;l|].(f)
			|  48 -> [| o;l;l;o;l;l;l;o;l;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;l;o;l;l;l;o;l;l;l|].(f)
			|  56 -> [| o;l;l;l;l;l;l;o;l;l;l;l;l;l;o;l;l;l;l;l;l;o;l;l;l;l;l;l;o;l;l;l;l;l;l;o;l;l;l;l;l;l;o;l;l;l;l;l;l|].(f)
			|  64 -> [| o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l|].(f)
			|  80 -> [| o;o;o;o;l;o;o;o;l;o;o;o;o;l;o;o;o;l;o;o;o;o;l;o;o;o;l;o;o;o;o;l;o;o;o;l;o;o;o;o;l;o;o;o;l;o;o;o;l|].(f)
			|  96 -> [| o;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l|].(f)
			| 112 -> [| o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l|].(f)
			| 128 -> [| o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l|].(f)
			| 160 -> [| o;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;l|].(f)
			| 192 -> [| o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l|].(f)
			| 224 -> [| o;o;l;o;l;o;l;o;o;l;o;l;o;l;o;o;l;o;l;o;l;o;o;l;o;l;o;l;o;o;l;o;l;o;l;o;o;l;o;l;o;l;o;o;l;o;l;o;l|].(f)
			| 256 -> [| o;l;l;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;l;l;l|].(f)
			| 320 -> [| o;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l|].(f)
			|  _  -> invalid_arg "padded_frame invalid bitrate"
		)
		| S22050 -> (fun bitrate frameno ->
			let f = frameno mod 49 in
			match bitrate with
			|   8 -> [| o;o;o;o;o;o;o;o;l;o;o;o;o;o;o;o;l;o;o;o;o;o;o;o;l;o;o;o;o;o;o;o;l;o;o;o;o;o;o;o;l;o;o;o;o;o;o;o;l|].(f)
			|  16 -> [| o;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l|].(f)
			|  24 -> [| o;o;l;o;o;l;o;o;l;o;l;o;o;l;o;o;l;o;o;l;o;l;o;o;l;o;o;l;o;l;o;o;l;o;o;l;o;o;l;o;l;o;o;l;o;o;l;o;l|].(f)
			|  32 -> [| o;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l|].(f)
			|  40 -> [| o;l;o;l;l;o;l;o;l;l;o;l;o;l;l;o;l;l;o;l;o;l;l;o;l;o;l;l;o;l;o;l;l;o;l;l;o;l;o;l;l;o;l;o;l;l;o;l;l|].(f)
			|  48 -> [| o;l;l;o;l;l;l;o;l;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;l;o;l;l;l;o;l;l;l|].(f)
			|  56 -> [| o;l;l;l;l;l;l;o;l;l;l;l;l;l;o;l;l;l;l;l;l;o;l;l;l;l;l;l;o;l;l;l;l;l;l;o;l;l;l;l;l;l;o;l;l;l;l;l;l|].(f)
			|  64 -> [| o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l|].(f)
			|  80 -> [| o;o;o;o;l;o;o;o;l;o;o;o;o;l;o;o;o;l;o;o;o;o;l;o;o;o;l;o;o;o;o;l;o;o;o;l;o;o;o;o;l;o;o;o;l;o;o;o;l|].(f)
			|  96 -> [| o;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l|].(f)
			| 112 -> [| o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l|].(f)
			| 128 -> [| o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l|].(f)
			| 144 -> [| o;o;o;o;l;o;o;o;o;l;o;o;o;o;l;o;o;o;o;l;o;o;o;o;l;o;o;o;o;l;o;o;o;o;l;o;o;o;o;l;o;o;o;o;l;o;o;o;l|].(f)
			| 160 -> [| o;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;l|].(f)
			|  _  -> invalid_arg "padded_frame invalid bitrate"
		)
		| S11025 -> (fun bitrate frameno ->
			let f = frameno mod 49 in
			match bitrate with
			|   8 -> [| o;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l;o;o;o;l|].(f)
			|  16 -> [| o;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l|].(f)
			|  24 -> [| o;l;l;o;l;l;l;o;l;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;l;o;l;l;l;o;l;l;l|].(f)
			|  32 -> [| o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l|].(f)
			|  40 -> [| o;o;o;o;l;o;o;o;l;o;o;o;o;l;o;o;o;l;o;o;o;o;l;o;o;o;l;o;o;o;o;l;o;o;o;l;o;o;o;o;l;o;o;o;l;o;o;o;l|].(f)
			|  48 -> [| o;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;l;o;l;o;l;o;l|].(f)
			|  56 -> [| o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l;o;l;l;o;l;l;l|].(f)
			|  64 -> [| o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l|].(f)
			|  80 -> [| o;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;o;l;o;l;o;l;o;l;o;l|].(f)
			|  96 -> [| o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l;l|].(f)
			| 112 -> [| o;o;l;o;l;o;l;o;o;l;o;l;o;l;o;o;l;o;l;o;l;o;o;l;o;l;o;l;o;o;l;o;l;o;l;o;o;l;o;l;o;l;o;o;l;o;l;o;l|].(f)
			| 128 -> [| o;l;l;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;l;l;l|].(f)
			| 144 -> [| o;o;l;o;l;o;o;l;o;l;o;o;l;o;l;o;o;l;o;l;o;o;l;o;l;o;l;o;o;l;o;l;o;o;l;o;l;o;o;l;o;l;o;o;l;o;l;o;l|].(f)
			| 160 -> [| o;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l;o;l;l;l;l;l;l;l;l;l|].(f)
			|  _  -> invalid_arg "padded_frame invalid bitrate"
		)
		| S48000 -> (fun _ _ -> false)
		| S32000 -> (fun _ _ -> false)
		| S24000 -> (fun _ _ -> false)
		| S16000 -> (fun _ _ -> false)
		| S12000 -> (fun _ _ -> false)
		|  S8000 -> (fun _ _ -> false)
;;


