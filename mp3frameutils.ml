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

(*
mp3packer -f -z test\aps.mp3 NUL
Before: 15.328 / 15.343
*)


open Types;;
open Mp3framehuffman;;



let get_seq = if true then Ptr.Ref.get_seq_fast else Ptr.Ref.get_seq;;

let get_huffman_c = false;;



(***********)
(* C STUBS *)
(***********)

external find_best_config_base  : Ptr.t -> Ptr.t -> Ptr.t -> Ptr.t -> bool -> (int * int * int * int * int * int * int * bool) = "mfu_find_best_config_base";;
external find_best_config_sse41 : Ptr.t -> Ptr.t -> Ptr.t -> Ptr.t -> bool -> (int * int * int * int * int * int * int * bool) = "mfu_find_best_config_sse41";;



let debug_more = true;;

let tab = " ";;

(****************)
(* GLOBAL STUFF *)
(****************)

let num_quants = 576;;

(*
	The lowest frequency sample of each scalefactor band is given in this table
*)
(*
let global_scalefactors sfreq is_short = match (sfreq, is_short) with
	| (S48000, false) -> [| 0; 4; 8;12;16;20;24;30; 36; 42; 50; 60; 72; 88;106;128;156;190;230;276;330;384;576 |]
	| (S44100, false) -> [| 0; 4; 8;12;16;20;24;30; 36; 44; 52; 62; 74; 90;110;134;162;196;238;288;342;418;576 |]
	| (S32000, false) -> [| 0; 4; 8;12;16;20;24;30; 36; 44; 54; 66; 82;102;126;156;194;240;296;364;448;550;576 |]
	| (S24000, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;114;136;162;194;232;278;332;394;464;540;576 |]
	| (S22050, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;116;140;168;200;238;284;336;396;464;522;576 |]
	| (S16000, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;116;140;168;200;238;284;336;396;464;522;576 |]
	| (S12000, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;116;140;168;200;238;284;336;396;464;522;576 |]
	| (S11025, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;116;140;168;200;238;284;336;396;464;522;576 |]
	| ( S8000, false) -> [| 0;12;24;36;48;60;72;88;108;132;160;192;232;280;336;400;476;566;568;570;572;574;576 |]

	| (S48000, true ) -> [| 0;4; 8;12;16;22;28;38; 50; 64; 80;100;126;192 |]
	| (S44100, true ) -> [| 0;4; 8;12;16;22;30;40; 52; 66; 84;106;136;192 |]
	| (S32000, true ) -> [| 0;4; 8;12;16;22;30;42; 58; 78;104;138;180;192 |]
	| (S24000, true ) -> [| 0;4; 8;12;18;26;36;48; 62; 80;104;136;180;192 |]
	| (S22050, true ) -> [| 0;4; 8;12;18;24;32;42; 56; 74;100;132;174;192 |]
	| (S16000, true ) -> [| 0;4; 8;12;18;26;36;48; 62; 80;104;134;174;192 |]
	| (S12000, true ) -> [| 0;4; 8;12;18;26;36;48; 62; 80;104;134;174;192 |]
	| (S11025, true ) -> [| 0;4; 8;12;18;26;36;48; 62; 80;104;134;174;192 |]
	| ( S8000, true ) -> [| 0;8;16;24;36;52;72;96;124;160;162;164;166;192 |]
;;
let global_scalefactors_ptr =
	let get_this a b =
		let copy_array = global_scalefactors a b in
		let ptr = Ptr.make (2 * Array.length copy_array) 0 in
		Array.iteri (fun i q -> Ptr.put_16_of_int ptr (2 * i) q) copy_array;
		ptr
	in
	let long48000  = get_this S48000 false in
	let long44100  = get_this S44100 false in
	let long32000  = get_this S32000 false in
	let long24000  = get_this S24000 false in
	let long22050  = get_this S22050 false in
	let long16000  = get_this S16000 false in
	let long12000  = get_this S12000 false in
	let long11025  = get_this S11025 false in
	let long8000   = get_this  S8000 false in
	let short48000 = get_this S48000 true  in
	let short44100 = get_this S44100 true  in
	let short32000 = get_this S32000 true  in
	let short24000 = get_this S24000 true  in
	let short22050 = get_this S22050 true  in
	let short16000 = get_this S16000 true  in
	let short12000 = get_this S12000 true  in
	let short11025 = get_this S11025 true  in
	let short8000  = get_this  S8000 true  in
	fun sfreq is_short -> match (sfreq, is_short) with
	| (S48000, false) -> long48000
	| (S44100, false) -> long44100
	| (S32000, false) -> long32000
	| (S24000, false) -> long24000
	| (S22050, false) -> long22050
	| (S16000, false) -> long16000
	| (S12000, false) -> long12000
	| (S11025, false) -> long11025
	| ( S8000, false) -> long8000

	| (S48000, true ) -> short48000
	| (S44100, true ) -> short44100
	| (S32000, true ) -> short32000
	| (S24000, true ) -> short24000
	| (S22050, true ) -> short22050
	| (S16000, true ) -> short16000
	| (S12000, true ) -> short12000
	| (S11025, true ) -> short11025
	| ( S8000, true ) -> short8000
;;
*)
let global_scalefactors : type id. id samplerate_t -> bool -> int array = fun s_freq is_short -> match (s_freq, is_short) with
	| (S48000, false) -> [| 0; 4; 8;12;16;20;24;30; 36; 42; 50; 60; 72; 88;106;128;156;190;230;276;330;384;576 |]
	| (S44100, false) -> [| 0; 4; 8;12;16;20;24;30; 36; 44; 52; 62; 74; 90;110;134;162;196;238;288;342;418;576 |]
	| (S32000, false) -> [| 0; 4; 8;12;16;20;24;30; 36; 44; 54; 66; 82;102;126;156;194;240;296;364;448;550;576 |]
	| (S24000, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;114;136;162;194;232;278;332;394;464;540;576 |]
	| (S22050, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;116;140;168;200;238;284;336;396;464;522;576 |]
	| (S16000, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;116;140;168;200;238;284;336;396;464;522;576 |]
	| (S12000, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;116;140;168;200;238;284;336;396;464;522;576 |]
	| (S11025, false) -> [| 0; 6;12;18;24;30;36;44; 54; 66; 80; 96;116;140;168;200;238;284;336;396;464;522;576 |]
	| ( S8000, false) -> [| 0;12;24;36;48;60;72;88;108;132;160;192;232;280;336;400;476;566;568;570;572;574;576 |]

	| (S48000, true ) -> [| 0;4; 8;12;16;22;28;38; 50; 64; 80;100;126;192 |]
	| (S44100, true ) -> [| 0;4; 8;12;16;22;30;40; 52; 66; 84;106;136;192 |]
	| (S32000, true ) -> [| 0;4; 8;12;16;22;30;42; 58; 78;104;138;180;192 |]
	| (S24000, true ) -> [| 0;4; 8;12;18;26;36;48; 62; 80;104;136;180;192 |]
	| (S22050, true ) -> [| 0;4; 8;12;18;24;32;42; 56; 74;100;132;174;192 |]
	| (S16000, true ) -> [| 0;4; 8;12;18;26;36;48; 62; 80;104;134;174;192 |]
	| (S12000, true ) -> [| 0;4; 8;12;18;26;36;48; 62; 80;104;134;174;192 |]
	| (S11025, true ) -> [| 0;4; 8;12;18;26;36;48; 62; 80;104;134;174;192 |]
	| ( S8000, true ) -> [| 0;8;16;24;36;52;72;96;124;160;162;164;166;192 |]
;;

(* NOTE: for some reason I can't add these before a closure in the function, so I have to pollute the module's namespace *)
let gsf_get_this : type id. id samplerate_t -> _ = fun a b ->
	let copy_array = global_scalefactors a b in
	let ptr = Ptr.make (2 * Array.length copy_array) 0 in
	Array.iteri (fun i q -> Ptr.put_16_of_int ptr (2 * i) q) copy_array;
	ptr
;;
let gsf_long48000  = gsf_get_this S48000 false;;
let gsf_long44100  = gsf_get_this S44100 false;;
let gsf_long32000  = gsf_get_this S32000 false;;
let gsf_long24000  = gsf_get_this S24000 false;;
let gsf_long22050  = gsf_get_this S22050 false;;
let gsf_long16000  = gsf_get_this S16000 false;;
let gsf_long12000  = gsf_get_this S12000 false;;
let gsf_long11025  = gsf_get_this S11025 false;;
let gsf_long8000   = gsf_get_this S8000  false;;
let gsf_short48000 = gsf_get_this S48000 true;;
let gsf_short44100 = gsf_get_this S44100 true;;
let gsf_short32000 = gsf_get_this S32000 true;;
let gsf_short24000 = gsf_get_this S24000 true;;
let gsf_short22050 = gsf_get_this S22050 true;;
let gsf_short16000 = gsf_get_this S16000 true;;
let gsf_short12000 = gsf_get_this S12000 true;;
let gsf_short11025 = gsf_get_this S11025 true;;
let gsf_short8000  = gsf_get_this S8000  true;;
let global_scalefactors_ptr : type id. id samplerate_t -> bool -> Ptr.t = fun a b ->
	match (a,b) with
	| (S48000, false) -> gsf_long48000
	| (S44100, false) -> gsf_long44100
	| (S32000, false) -> gsf_long32000
	| (S24000, false) -> gsf_long24000
	| (S22050, false) -> gsf_long22050
	| (S16000, false) -> gsf_long16000
	| (S12000, false) -> gsf_long12000
	| (S11025, false) -> gsf_long11025
	| (S8000 , false) -> gsf_long8000
	| (S48000, true ) -> gsf_short48000
	| (S44100, true ) -> gsf_short44100
	| (S32000, true ) -> gsf_short32000
	| (S24000, true ) -> gsf_short24000
	| (S22050, true ) -> gsf_short22050
	| (S16000, true ) -> gsf_short16000
	| (S12000, true ) -> gsf_short12000
	| (S11025, true ) -> gsf_short11025
	| (S8000 , true ) -> gsf_short8000
;;



type block_type_t = Block_type_long | Block_type_short | Block_type_start | Block_type_stop;;

type window_normal_t = {
	normal_table_select1 : int;  (* Which Huffman table to use for each region *)
	normal_table_select2 : int;  (* etc *)
	normal_table_select3 : int;  (* etc *)
	normal_region_0_count : int; (* How many values are in the first region (starting from lowest freq) *)
	normal_region_1_count : int; (* Ditto for the next higher region (region_2_count is implicit because big_values is known beforehand) *)
};;
type window_other_t = {
	other_block_type : block_type_t; (* 01 = 1 long block start window. 10 = 3 short blocks. 11 = 1 long block stop window. 00 = invalid *)
	other_mixed_block : bool;        (* 0 = normal. 1 = lower 2 subbands are long, upper 30 subbands are short (for block_type = 10) *)
	other_table_select1 : int;       (* Which Huffman table to use for the particular region (regions are implicit) *)
	other_table_select2 : int;       (* Ditto (note that there is no region3) *)
	other_sub_block_gain1 : int;     (* Offset from the global gain of each short block, if Block_type = 10. Otherwise not used *)
	other_sub_block_gain2 : int;     (* Same for next short block *)
	other_sub_block_gain3 : int;     (* etc *)
};;

(* gc = granule-channel *)
type gc_window_t = Window_normal of window_normal_t | Window_other of window_other_t;;

type side_gc_t = {
	gc_part2_3_length : int;     (* Length in bits of parts 2 (scalefactors) and 3 (Huffman encoded data) *)
	gc_part2_3_offset : int;     (* The integral of the previous part2_3_lengths *)
	gc_big_values : int;         (* Half the number of frequency bands (starting from lowest freq) whose absolute values are greater than 1 *)
	gc_global_gain : int;        (* Logarithmic encoding of the global gain value *)
		(* xxxxxxxx11111111111111110000000000000000000000  *)
		(*        |               |                     |  *)
		(*   bigvalues*2    bv*2+count1*4              576 *)
		(* Everything in "0" region is 0 *)
		(* Everything in "1" region is -1, 0, or +1 *)
		(* "x" can be anything *)
	gc_scf_compress_index : int; (* An index determining the number of bits to use for the scale factors (For MPEG1) *)
	gc_window : gc_window_t;     (* 0 = granule / channel has only 1 block and a normal window. 1 = something else *)
	gc_pre_flag : bool;          (* High-frequency amplification shortcut; not used for MPEG2/2.5 *)
	gc_sf_scale : int;           (* Which Huffman table to use for scalefactor scaling *)
	gc_count1_table_1 : bool;    (* true if table 1 is used for the Count1 region, false if 0 *)
}

type (_,_) side_gc_selector_t =
	| GC_1_mono : side_gc_t * side_gc_t -> (mpeg1_t, channel_mono_t) side_gc_selector_t
	| GC_1_stereo : side_gc_t * side_gc_t * side_gc_t * side_gc_t -> (mpeg1_t, channel_stereo_t) side_gc_selector_t
	| GC_2_mono : side_gc_t -> (_ mpeg2_t, channel_mono_t) side_gc_selector_t
	| GC_2_stereo : side_gc_t * side_gc_t -> (_ mpeg2_t, channel_stereo_t) side_gc_selector_t
;;

type (_,_) side_scfi_t =
	| SCFI_none : (_ mpeg2_t, _) side_scfi_t
	| SCFI_mono : (bool * bool * bool * bool) -> (mpeg1_t, channel_mono_t) side_scfi_t
	| SCFI_stereo : (bool * bool * bool * bool) * (bool * bool * bool * bool) -> (mpeg1_t, channel_stereo_t) side_scfi_t
;;
let no_scfi = (false,false,false,false);;
let print_scfi (a,b,c,d) p =
	p (Char '(');
	p (Char (if a then '#' else '.'));
	p (Char (if b then '#' else '.'));
	p (Char (if c then '#' else '.'));
	p (Char (if d then '#' else '.'));
	p (Char ')');
;;

type ('id,'chan) side_internal_t = {
	side_main_data_begin : int;
	side_scfi : ('id,'chan) side_scfi_t; (* side_scfi.(channel).(scfi band) 0 = Scale factors are transmitted for each granule. 1 = One scalefactor is for both granules. Note that there are 4 scale factor bands. *)
	side_gc : ('id,'chan) side_gc_selector_t; (* (gr0ch0, gr0ch1, gr1ch0, gr1ch1) *)
};;
type side_internal_ext_t = Side_int_ext : ('id,'chan) side_internal_t -> side_internal_ext_t;;

(*
	The number of bits to use in the (low,high) frequencies of the scalefactors.
	Most of the time, the first number corresponds to the first 11 scalefactors and the last number is for the last 10 scalefactors
	Note that the last scalefactor band (#22) does not have its own scalefactor
*)
let scalefactor_compress_m1 = [|
	(0,0);
	(0,1);
	(0,2);
	(0,3);
	(3,0);
	(1,1);
	(1,2);
	(1,3);
	(2,1);
	(2,2);
	(2,3);
	(3,1);
	(3,2);
	(3,3);
	(4,2);
	(4,3);
|];;

(* This is used with MPEG2, for everything except the right channel in IS frames *)
let scalefactor_compress_m2 = Array.init 512 (fun i ->
	if i < 400 then (
		((i lsr 4 / 5), (i lsr 4 mod 5), ((i land 15) lsr 2), (i land 3))
	) else if i < 500 then (
		let j = i - 400 in
		((j lsr 2 / 5), (j lsr 2 mod 5), (j land 3), (0))
	) else (
		let j = i - 500 in
		((j / 3), (j mod 3), (0), (0))
	)
);;

(* Only for the right channel in IS frames *)
let scalefactor_compress_m2_is = Array.init 512 (fun q ->
	let i = q lsr 1 in
	if i < 180 then (
		((i / 36), (i mod 36 / 6), (i mod 6), 0)
	) else if i < 244 then (
		let j = i - 180 in
		((j land 63) lsr 4, (j land 15) lsr 2, j land 3, 0)
	) else (
		let j = i - 244 in
		(j / 3, j mod 3, 0, 0)
	)
);;

let scalefactor_bands_m2 is gc =
	if is then (
		if gc.gc_scf_compress_index < 360 then (
			match gc with
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = true}}  -> (6,15,12,0)
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = false}} -> (12,12,12,0)
			| _ ->                                                                                           (7,7,7,0)
		) else if gc.gc_scf_compress_index < 488 then (
			match gc with
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = true}}  -> (6,12,9,6)
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = false}} -> (12,9,9,6)
			| _ ->                                                                                           (6,6,6,3)
		) else (
			match gc with
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = true}}  -> (6,18,9,0)
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = false}} -> (15,12,9,0)
			| _ ->                                                                                           (8,8,5,0)
		)
	) else (
		if gc.gc_scf_compress_index < 400 then (
			match gc with
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = true}}  -> (6,9,9,9)
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = false}} -> (9,9,9,9)
			| _ ->                                                                                           (6,5,5,5)
		) else if gc.gc_scf_compress_index < 500 then (
			match gc with
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = true}}  -> (6,9,12,6)
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = false}} -> (9,9,12,6)
			| _ ->                                                                                           (6,5,7,3)
		) else (
			match gc with
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = true}}  -> (15,18,0,0)
			| {gc_window = Window_other {other_block_type = Block_type_short; other_mixed_block = false}} -> (18,18,0,0)
			| _ ->                                                                                           (11,10,0,0)
		)
	)
;;



(************************)
(* Frame internal types *)
(************************)

type _ m1_scalefactors_t =
	| M1_scalefactors_mono : int array * int array -> channel_mono_t m1_scalefactors_t
	| M1_scalefactors_stereo : int array * int array * int array * int array -> channel_stereo_t m1_scalefactors_t
;;
type _ m1_quantizers_t =
	| M1_quantizers_mono : Ptr.t * Ptr.t -> channel_mono_t m1_quantizers_t
	| M1_quantizers_stereo : Ptr.t * Ptr.t * Ptr.t * Ptr.t -> channel_stereo_t m1_quantizers_t
;;
type 'chan m1_frame_data_t = {
	m1_header : (mpeg1_t, 'chan) header_t; (* Do I need this? *)
	m1_side_info : (mpeg1_t, 'chan) side_internal_t;
	m1_scalefactors : 'chan m1_scalefactors_t; (* m1_scalefactors.(granule).(channel).(scalefactor) *)
(*	m1_quantizers : int array array array; (* m1_quantizers.(granule).(channel).(quantizer) *)*)
	m1_quantizer_ptrs : 'chan m1_quantizers_t; (* Ptr.t = int16 array *)
	m1_starting_f1 : (mpeg1_t, 'chan) f1_t;
};;

type _ m2_scalefactors_t =
	| M2_scalefactors_mono : int array -> channel_mono_t m2_scalefactors_t
	| M2_scalefactors_stereo : int array * int array -> channel_stereo_t m2_scalefactors_t
;;
type _ m2_quantizers_t =
	| M2_quantizers_mono : Ptr.t -> channel_mono_t m2_quantizers_t
	| M2_quantizers_stereo : Ptr.t * Ptr.t -> channel_stereo_t m2_quantizers_t
;;
type ('id2,'chan) m2_frame_data_t = {
	m2_header : ('id2 mpeg2_t, 'chan) header_t;
	m2_side_info : ('id2 mpeg2_t, 'chan) side_internal_t;
	m2_scalefactors : 'chan m2_scalefactors_t; (* m2_scalefactors.(channel).(scalefactor) *)
(*	m2_quantizers : int array array; (* m2_quantizers.(channel).(quantizer) *)*)
	m2_quantizer_ptrs : 'chan m2_quantizers_t;
	m2_starting_f1 : ('id2 mpeg2_t, 'chan) f1_t;
};;

type (_,_) frame_data_t =
	| M1_frame_data : 'chan m1_frame_data_t -> (mpeg1_t, 'chan) frame_data_t
	| M2_frame_data : ('id2,'chan) m2_frame_data_t -> ('id2 mpeg2_t, 'chan) frame_data_t
;;



(*let print_side pin spaces s = ();;*)

let print_side : type id chan. (Types.p_type list -> unit) -> int -> (id,chan) side_internal_t -> unit = fun pin spaces s ->
	let p x = pin (Spaces spaces :: x) in

	p [Str "Main data begin: "; Int s.side_main_data_begin];
	p [Fun (fun print ->
		print (Str "SCFI each granule:");
(*		print (Str "(later)");*)
		match s.side_scfi with
		| SCFI_none -> print (Str " none (MPEG2)")
		| SCFI_mono scfi -> (print (Char ' '); print_scfi scfi print)
		| SCFI_stereo (scfi_ch0, scfi_ch1) -> (print (Char ' '); print_scfi scfi_ch0 print; print (Char ' '); print_scfi scfi_ch1 print)
(*
		Array.iter (fun gr ->
			print (Str " (");
			Array.iter (function
				| true -> print (Char '#')
				| false -> print (Char '.')
			) gr;
			print (Str ")");
		) s.side_scfi;
*)
	)];
	p [Str "Granule / channel info:"];
	let pgrch gr ch gc =
		p [Str " Granule "; Int gr; Str ":"];
		p [Str "  Channel "; Int ch; Str ":"];
		p [Str "   Part2_3:     "; Int gc.gc_part2_3_length; Str " (offset "; Int gc.gc_part2_3_offset; Str ")"];
		p [Str "   big_values:  "; Int gc.gc_big_values];
		p [Str "   global_gain: "; Int gc.gc_global_gain];
		p [Str "   scf_comp_i:  "; Int gc.gc_scf_compress_index];
		(match gc.gc_window with
			| Window_normal w -> (
				p [Str "   Normal window"];
				p [Str "    Tables:     "; Int w.normal_table_select1; Str ","; Int w.normal_table_select2; Str ","; Int w.normal_table_select3];
				p [Str "    Region 0,1: "; Int w.normal_region_0_count; Str ","; Int w.normal_region_1_count];
			)
			| Window_other w -> (
				p [Str "   Other window"];
				p [Str "    Block type:     "; Str (match w.other_block_type with
					| Block_type_long -> "INVALID!"
					| Block_type_short -> "Short"
					| Block_type_start -> "Start"
					| Block_type_stop -> "Stop"
				)];
				p [Str "    Mixed block?    "; Bool w.other_mixed_block];
				p [Str "    Tables:         "; Int w.other_table_select1; Str ","; Int w.other_table_select2];
				p [Str "    Sub block gain: "; Int w.other_sub_block_gain1; Str ","; Int w.other_sub_block_gain2; Str ","; Int w.other_sub_block_gain3];
			)
		);
		p [Str "   HF amp:      "; Bool gc.gc_pre_flag];
		p [Str "   SF scale:    "; Int gc.gc_sf_scale];
		p [Str "   Count1 table "; Int (if gc.gc_count1_table_1 then 1 else 0)];
	in

	match s.side_gc with
	| GC_1_mono (a,b) -> (pgrch 0 0 a; pgrch 1 0 b)
	| GC_1_stereo (a,b,c,d) -> (pgrch 0 0 a; pgrch 0 1 b; pgrch 1 0 c; pgrch 1 1 d)
	| GC_2_mono (a) -> (pgrch 0 0 a)
	| GC_2_stereo (a,b) -> (pgrch 0 0 a; pgrch 0 1 b)
;;


(*
(* Frame printing functions *)
let print_side pin spaces s =

(*	let p = Printf.printf in*)
(*
	let p = (
		let h f = Printf.printf "%s%s" tabs f in
		fun a -> Printf.kprintf h a
	) in
*)
	let p x = pin (Spaces spaces :: x) in
	()
(** TODO: later
	p "Main data begin: %d\n" s.side_main_data_begin;
	p "SCFI each granule:";
	Array.iter (fun n -> Printf.printf " ("; Array.iter (fun q -> Printf.printf "%s" (if q then "#" else ".")) n; Printf.printf ")") s.side_scfi;
	p "\n";
	p "Granule / channel info:\n";
	Array.iteri (fun i granule ->
		p " Granule %d:\n" i;
		Array.iteri (fun j gc ->
			p "  Channel %d:\n" j;
			p "   Part2_3:     %d (offset %d)\n" gc.gc_part2_3_length gc.gc_part2_3_offset;
			p "   big_values:  %d\n" gc.gc_big_values;
			p "   global_gain: %d\n" gc.gc_global_gain;
			p "   scf_comp_i:  %d\n" gc.gc_scf_compress_index;
			(match gc.gc_window with
				| Window_normal w -> (
					p "   Normal window\n";
					p "    Tables:     %d,%d,%d\n" w.normal_table_select1 w.normal_table_select2 w.normal_table_select3;
					p "    Region 0,1: %d,%d\n" w.normal_region_0_count w.normal_region_1_count;
				)
				| Window_other w -> (
					p "   Other window\n";
					p "    Block type:     %s\n" (match w.other_block_type with
						| Block_type_long -> "INVALID!"
						| Block_type_short -> "Short"
						| Block_type_start -> "Start"
						| Block_type_stop -> "Stop"
					);
					p "    Mixed block?    %b\n" w.other_mixed_block;
					p "    Tables:         %d,%d\n" w.other_table_select1 w.other_table_select2;
					p "    Sub block gain: %d,%d,%d\n" w.other_sub_block_gain1 w.other_sub_block_gain2 w.other_sub_block_gain3;
				)
			);
			p "   HF amp:      %b\n" gc.gc_pre_flag;
			p "   SF scale:    %d\n" gc.gc_sf_scale;
			p "   Count1 table %d\n" (if gc.gc_count1_table_1 then 1 else 0);
		) granule;
	) s.side_gc;
*)
;;
*)


(*
Side info:
 9/17/32 BYTES = 136/256 bits
 9: main_data_begin (8 for MPEG2)
 ?: private_bits
 4: SCFI Band
 59: Side Information Granule
  12: part2_3 length (main data for this channel, granule in bits)
  9: Big values
  8: Global gain
  4: Scalefactor compress (9 for MPEG2)
  1: Window switch flag
   if 1:
    2: Block type
    1: Mix block flag
    5x2: Table Select [region]
    3x3: sub_block_gain [window]
   if 0:
    5x3: Table select [region]
    4: Region 0 count
    3: Region 1 count
  1: Pre flag (NOT FOR MPEG2)
  1: Scale factor scale
  1: Count1 table select

 MPEG1 mono:
 [9 main data] [5 privates] [4 SCFI] [59 Gr0] [59 Gr1]
 (18 - 30) (77 - 89)
 MPEG1 stereo:
 [9 main data] [3 privates] [4 SCFI0] [4 SCFI1] [59 Gr0ch1] [59 Gr0ch2] [59 Gr1ch1] [59 Gr1ch2]
 20 79 138 197
 MPEG2 mono:
 [8 main data] [1 privates] [63 Gr*]
 (9 - 21)
 MPEG2 stereo:
 [8 main data] [2 privates] [63 Gr*ch1] [63 Gr*ch2]
 (10 - 22) (73 - 85)
*)

(*******************)
(* READ QUANTIZERS *)
(*******************)
(* It ends when r_at = r_to *)
let read_quantizers state file_state k gc in_ptr r_at r_to = 
	let p = state.q_print_recompress in
	let s = Ptr.Ref.new_seq (Ptr.Ref.of_ptr in_ptr) in
	Ptr.Ref.set_seq s r_at;
(*let debug = false in*)

	if state.q_debug_recompress then (
(*		p [Str "Starting on %S at %d, going to %d\n" (String.sub (to_bin (Ptr.Ref.to_string s.Ptr.Ref.seq_ref)) s.Ptr.Ref.seq_at (r_to - s.Ptr.Ref.seq_at)) s.Ptr.Ref.seq_at r_to;*)
		let str = String.sub (to_bin (Ptr.Ref.to_string s.Ptr.Ref.seq_ref)) s.Ptr.Ref.seq_at (r_to - s.Ptr.Ref.seq_at) in
		p [Str "Starting on "; Str str; Str " at "; Int s.Ptr.Ref.seq_at; Str ", going to "; Int r_to];
	);

	let decoder_error_ref = ref false in
(*	let out_quants = Array.make num_quants 0 in*)
	let out_quants_16_ptr = Ptr.make (num_quants * 2) 16 in

	let (region0, region1, region2, table0, table1, table2) = (match gc.gc_window with
		| Window_normal w -> (
			let scfend0 = w.normal_region_0_count + 1 in
			let scfend1 = w.normal_region_1_count + 1 + scfend0 in
			let region0 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend0) lsr 1) in
			let region1 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend1) lsr 1) - region0 in
			(region0, region1, gc.gc_big_values - region0 - region1, w.normal_table_select1, w.normal_table_select2, w.normal_table_select3)
		)
		| Window_other  w -> (
			let region0 = if w.other_block_type = Block_type_short && not w.other_mixed_block then (
				(global_scalefactors k.header_samplerate true).(9 / 3) lsr 1 * 3
			) else (
				(global_scalefactors k.header_samplerate false).(8) lsr 1
			) in

			(min region0 gc.gc_big_values, max 0 (gc.gc_big_values - region0), 0, w.other_table_select1, w.other_table_select2, 0)
		)
	) in
	p [
		Str " L = (";
		Int region0; Str ","; Int region1; Str ","; Int region2; Str "), ht = (";
		Int table0; Str ","; Int table1; Str ","; Int table2; Str ")"
	];



	let index =
		let (overboard, new_ptr_loc, new_index) = Mp3framehuffman.decode_all_quants in_ptr s.Ptr.Ref.seq_at out_quants_16_ptr r_to (region0 * 2) table0 (region1 * 2) table1 (region2 * 2) table2 num_quants gc.gc_count1_table_1 decoder_error_ref in
		p [Str "Got new index "; Int new_index; Str " at bit pos "; Int new_ptr_loc; Str " after all quants decoded"];
		if new_ptr_loc > r_to then (
			p [Str "OOPS! decode_all_quants went overboard (bit "; Int new_ptr_loc; Str " > target "; Int r_to; Str ")"];
			decoder_error_ref := true;
		);
		Ptr.Ref.set_seq s new_ptr_loc;
		if overboard then (
			p [Str " Num_quants exceeded"];
			if file_state#freq_warn then (
				(* calling #freq_warn will automatically turn it off for future frames *)
				P.print_always [Str "WARNING: too many frequencies; files may be decoded differently by some players"]
			)
		);
		new_index
	in


	for i = index to num_quants - 1 do
		Ptr.put_16_of_int out_quants_16_ptr (2 * i) 0;
	done;
(*
	if s.Ptr.Ref.seq_at > r_to then (
		p [Str " OOPS! Decoding count1 went a little overboard (pos "; Int s.Ptr.Ref.seq_at; Str " > "; Int r_to; Str ")"];
		decoder_error_ref := true;
	);
*)
	if state.q_debug_recompress then (
(*
		Printf.printf "%sREAD_QUANTIZERS_M1: (%d) - [" tabs index;
		for i = 0 to num_quants - 1 do
			Printf.printf " %d" (Ptr.get_int_of_16 out_quants_16_ptr (2 * i));
		done;
		Printf.printf " ]\n";
*)
		p [
			Str "    READ_QUANTIZERS: ("; Int index; Str ") - [";
			Fun (fun print -> for i = 0 to num_quants - 1 do
				print (Str " ");
				print (Int (Ptr.get_int_of_16 out_quants_16_ptr (2 * i)));
			done);
			Str " ]"
		];
	);

	(out_quants_16_ptr, !decoder_error_ref)
;;




(*********************)
(* READ SCALEFACTORS *)
(*********************)
let read_scalefactors_m1 state scfi_tuple prev_scf_option gc s   file_state k in_ptr =
	let (scfia,scfib,scfic,scfid) = scfi_tuple in
	let p = state.q_print_recompress in
	let make_initial_array = match (prev_scf_option, scfi_tuple) with
		| (None, _) -> (fun x -> Array.make x 0)
		| (Some _, (false,false,false,false)) -> (fun x -> Array.make x 0) (* If scfi indicates to not use anything from previous frame, just recreate the array. This helps for short blocks, when the array is the wrong length anyway *)
		| (Some y, _) -> (fun x -> Array.sub y 0 x)
	in
	let (bits1,bits2) = scalefactor_compress_m1.(gc.gc_scf_compress_index) in
	let (num1,num2,scf_out) = match gc.gc_window with
		| Window_normal _ -> (
			(* Normal *)
			(11, 10, make_initial_array 21)
		)
		| Window_other x when x.other_block_type <> Block_type_short -> (
			(* Start or stop (actually the same as normal) *)
			(11, 10, make_initial_array 21)
		)
		| Window_other x when x.other_mixed_block -> (
			(* Short, mixed block *)
			(17, 18, make_initial_array 35) (* Mixed blocks seem to have 36 scalefactors, but only 33 scalefactor bands. However, the way the windows are set up, there are a total of 35 scalefactor scales *)
		)
		| Window_other _ -> (
			(* Short block *)
			(18, 18, make_initial_array 36) (* Short blocks have 39 scalefactors, but only 36 scalefactor bands *)
		)
	in
	(* Perhaps I should check for short frames somewhere? Or maybe just assume that the file has SCFSI set to false for short frames already *)
	let rec read_stuff i = (
		let num_bits = if i < num1 then bits1 else bits2 in

		p [Str "reading "; Int num_bits; Str " bits from "; Int s.Ptr.Ref.seq_at; Str " (len "; Int s.Ptr.Ref.seq_ref.Ptr.Ref.lentot; Str " bytes)"];
		p [Str "seq fast is "; Hex (8,s.Ptr.Ref.seq_get_fast_int); Str ", next bytes is "; Int s.Ptr.Ref.seq_get_fast_next_byte];

		if i >= Array.length scf_out then (
			p [Str "!"];
		) else if i < 6 then (
			if not scfia then (scf_out.(i) <- get_seq s num_bits; p [Str "0: "; Int scf_out.(i)]);
			read_stuff (succ i)
		) else if i < 11 then (
			if not scfib then (scf_out.(i) <- get_seq s num_bits; p [Str "1: "; Int scf_out.(i)]);
			read_stuff (succ i)
		) else if i < 16 then (
			if not scfic then (scf_out.(i) <- get_seq s num_bits; p [Str "2: "; Int scf_out.(i)]);
			read_stuff (succ i)
		) else if i < 21 then (
			if not scfid then (scf_out.(i) <- get_seq s num_bits; p [Str "3: "; Int scf_out.(i)]);
			read_stuff (succ i)
		) else (
			scf_out.(i) <- get_seq s num_bits; p [Str "+: "; Int scf_out.(i)];
			read_stuff (succ i)
		)
	) in

	read_stuff 0;
(*	read_stuff_s s 0;*)
(*	if debug && debug_more then Printf.printf "\n";*)

(*	if debug then Printf.printf "SCF compress (%d,%d)\n" bits1 bits2;*)
	p [
		Str "    READ_SCALEFACTORS_M1: ("; Int bits1; Str ","; Int bits2; Str ")="; Int num1; Str ","; Int num2; Str " - [";
		Fun (fun print -> Array.iter (fun n -> print (Str " "); print (Int n)) scf_out);
		Str " ] ";
(*		Fun (fun print -> Array.iter (fun n -> print (Str (if n then "#" else "."))) scfi)*)
		Fun (print_scfi scfi_tuple)
	]; (*tabs bits1 bits2 num1 num2 (Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" scf_out);*)
(*	if debug then Array.iter (fun q -> Printf.printf "%s" (if q then "#" else ".")) scfi;*)
(*	if debug then Printf.printf "\n";*)

	let r_to = gc.gc_part2_3_length + gc.gc_part2_3_offset in
	let (qp,error) = read_quantizers state file_state k gc in_ptr s.Ptr.Ref.seq_at r_to in

	(scf_out, qp, error)
;;

let read_scalefactors_m2 state is gc s =
	let p = state.q_print_recompress in

	let (bits0, bits1, bits2, bits3) = if is then (
		scalefactor_compress_m2_is.(gc.gc_scf_compress_index)
	) else (
		scalefactor_compress_m2.(gc.gc_scf_compress_index)
	) in
	let (num0, num1, num2, num3) = scalefactor_bands_m2 is gc in

	let sob1 = num0 in
	let sob2 = sob1 + num1 in
	let sob3 = sob2 + num2 in
	let num_total = sob3 + num3 in
	
	let scf_out = Array.make num_total 0 in
(*
	let rec read_stuff i = (
		if i < sob1 then (
			scf_out.(i) <- get_seq s bits0; if debug && debug_more then Printf.printf "0";
			read_stuff (succ i)
		) else if i < sob2 then (
			scf_out.(i) <- get_seq s bits1; if debug && debug_more then Printf.printf "1";
			read_stuff (succ i)
		) else if i < sob3 then (
			scf_out.(i) <- get_seq s bits2; if debug && debug_more then Printf.printf "2";
			read_stuff (succ i)
		) else if i < num_total then (
			scf_out.(i) <- get_seq s bits3; if debug && debug_more then Printf.printf "3";
			read_stuff (succ i)
		) else (
			if debug && debug_more then Printf.printf "!";
		)
	) in

	read_stuff 0;
*)
	for i = 0 to sob1 - 1 do
		scf_out.(i) <- get_seq s bits0;
(*		if debug && debug_more then Printf.printf "0";*)
	done;
	for i = sob1 to sob2 - 1 do
		scf_out.(i) <- get_seq s bits1;
(*		if debug && debug_more then Printf.printf "1";*)
	done;
	for i = sob2 to sob3 - 1 do
		scf_out.(i) <- get_seq s bits2;
(*		if debug && debug_more then Printf.printf "2";*)
	done;
	for i = sob3 to num_total - 1 do
		scf_out.(i) <- get_seq s bits3;
(*		if debug && debug_more then Printf.printf "3";*)
	done;
(*	if debug && debug_more then Printf.printf "!\n";*)

	if state.q_debug_recompress then (
		let pf from count print =
			for i = from to from + count - 1 do
				print (Str " ");
				print (Int scf_out.(i));
			done
		in
		p [
			Str "    READ_SCALEFACTORS_M2: ("; Int bits0; Str ","; Int bits1; Str ","; Int bits2; Str ","; Int bits3;
			Str ")="; Int num0; Str ","; Int num1; Str ","; Int num2; Str ","; Int num3;
			Str " - ["; Fun (pf 0 num0);
			Str " ]-["; Fun (pf sob1 num1);
			Str " ]-["; Fun (pf sob2 num2);
			Str " ]-["; Fun (pf sob3 num3);
			Str " ]";
		]
	);
(*
	if debug then (
		let str0 = Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" (Array.sub scf_out    0 num0) in
		let str1 = Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" (Array.sub scf_out sob1 num1) in
		let str2 = Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" (Array.sub scf_out sob2 num2) in
		let str3 = Array.fold_left (fun so_far gnu -> so_far ^ " " ^ (string_of_int gnu)) "" (Array.sub scf_out sob3 num3) in
		Printf.printf "%sREAD_SCALEFACTORS_M2: (%d,%d,%d,%d)=%d,%d,%d,%d - [%s ]-[%s ]-[%s ]-[%s ]\n" tabs bits0 bits1 bits2 bits3 num0 num1 num2 num3 str0 str1 str2 str3;
	);
*)

	scf_out
;;



(************************************************************************************************************************)
(* REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME REHUFF FRAME *)
(************************************************************************************************************************)
let rehuff_granule state (quant_ptr : Ptr.t) (*process_function*) gc (scf_bands_ptr : Ptr.t) = match gc.gc_window with
	| Window_other _ -> gc
	| Window_normal _ -> (
		let p = state.q_print_recompress in
		let process_function = match state.q_process_set with
			| SSE41 -> find_best_config_sse41
			| Set_base -> find_best_config_base
		in

		let (s_l1,s_l2,s_big,(*s_count1num : int*)_,s_t1,s_t2,s_t3,p1t1) = (

			(*let (s_l1,s_l2,s_big,s_count1num,s_t1,s_t2,s_t3,p1t1) =*) process_function
				quant_bits_ptr16
				quant_bits_count1_char_ptr
				scf_bands_ptr
				quant_ptr
				false (* Don't use the debug stuff yet *)
			(*in

			(s_l1,s_l2,s_big,s_count1num,s_t1,s_t2,s_t3,p1t1)*)
		) in

(*		let (s_l1,s_l2,s_big,s_t1,s_t2,s_t3,p1t1) = smallest_config in*)
		if state.q_debug_recompress then (
			p [Str "  NEW: ("; Int s_l1; Str ","; Int s_l2; Str ","; Int s_big; Str ","; Int s_t1; Str ","; Int s_t2; Str ","; Int s_t3; Str ","; Bool p1t1; Str ")"];
		);

		{gc with
			gc_big_values = s_big;
			gc_window = Window_normal {normal_table_select1 = s_t1; normal_table_select2 = s_t2; normal_table_select3 = s_t3; normal_region_0_count = s_l1; normal_region_1_count = s_l2};
			gc_count1_table_1 = p1t1;
		}
	)
;; (* rehuff_granule quants gc scf_quants *)



(*********************************************************************************)
(* DECODE FRAME DECODE FRAME DECODE FRAME DECODE FRAME DECODE FRAME DECODE FRAME *)
(*********************************************************************************)
let decode_side_info : type id chan. (id,chan) f1_t -> (id,chan) side_internal_t = fun f ->
	let s = Ptr.Ref.new_seq f.f1_side.side_raw in
	let gs = get_seq s in
	match f.f1_header.header_id with
	| MPEG1 -> (
		let read_gc part2_3_offset = (
			let part2_3_length = gs 12 in
			let big_values = gs 9 in
			let global_gain = gs 8 in
			let scf_compress = gs 4 in
			let window_flag = gs 1 in
			let window = if window_flag = 0 then (
				let huff1 = gs 5 in
				let huff2 = gs 5 in
				let huff3 = gs 5 in
				let r0 = gs 4 in
				let r1 = gs 3 in
				Window_normal {
					normal_table_select1 = huff1;
					normal_table_select2 = huff2;
					normal_table_select3 = huff3;
					normal_region_0_count = r0;
					normal_region_1_count = r1;
				}
			) else (
				let block_type_index = gs 2 in
				let mixed_block = gs 1 in
				let huff1 = gs 5 in
				let huff2 = gs 5 in
				let sb_gain1 = gs 3 in
				let sb_gain2 = gs 3 in
				let sb_gain3 = gs 3 in
				Window_other {
					other_block_type = [| Block_type_long; Block_type_start; Block_type_short; Block_type_stop |].(block_type_index);
					other_mixed_block = (mixed_block = 1);
					other_table_select1 = huff1;
					other_table_select2 = huff2;
					other_sub_block_gain1 = sb_gain1;
					other_sub_block_gain2 = sb_gain2;
					other_sub_block_gain3 = sb_gain3;
				}
			) in
			let pre_flag = gs 1 in
			let sf_scale = gs 1 in
			let count1_table = gs 1 in
			({
				gc_part2_3_length = part2_3_length;
				gc_part2_3_offset = part2_3_offset;
				gc_big_values = big_values;
				gc_global_gain = global_gain;
				gc_scf_compress_index = scf_compress;
				gc_window = window;
				gc_pre_flag = (pre_flag = 1);
				gc_sf_scale = sf_scale;
				gc_count1_table_1 = (count1_table = 1);
			}, part2_3_offset + part2_3_length)
		) in

		match f.f1_header.header_channel_mode with
		| Mono -> (
			let main_data = gs 9 in
			let _ = gs 5 in
			let a = gs 1 in
			let b = gs 1 in
			let c = gs 1 in
			let d = gs 1 in
			let side_scfi = SCFI_mono (a = 1, b = 1, c = 1, d = 1) in
			let (side_gc1, new_so_far) = read_gc 0 in
			let (side_gc2,     _     ) = read_gc new_so_far in
			{
				side_main_data_begin = main_data;
				side_scfi = side_scfi;
				side_gc = GC_1_mono (side_gc1, side_gc2);
			}
		)
		| Stereo _ -> (
			let main_data = gs 9 in
			let _ = gs 3 in
			let a = gs 1 in
			let b = gs 1 in
			let c = gs 1 in
			let d = gs 1 in
			let e = gs 1 in
			let f = gs 1 in
			let g = gs 1 in
			let h = gs 1 in
			let side_scfi = SCFI_stereo ((a = 1, b = 1, c = 1, d = 1), (e = 1, f = 1, g = 1, h = 1)) in
			let (side_gc1, new_so_far) = read_gc 0 in
			let (side_gc2, new_so_far) = read_gc new_so_far in
			let (side_gc3, new_so_far) = read_gc new_so_far in
			let (side_gc4,     _     ) = read_gc new_so_far in
			{
				side_main_data_begin = main_data;
				side_scfi = side_scfi;
				side_gc = GC_1_stereo (side_gc1, side_gc2, side_gc3, side_gc4);
			}
		)
	)
	| MPEG2 _ -> (
		let read_gc part2_3_offset = (
			let part2_3_length = gs 12 in
			let big_values = gs 9 in
			let global_gain = gs 8 in
			let scf_compress = gs 9 in
			let window_flag = gs 1 in
			let window = if window_flag = 0 then (
				let huff1 = gs 5 in
				let huff2 = gs 5 in
				let huff3 = gs 5 in
				let r0 = gs 4 in
				let r1 = gs 3 in
				Window_normal {
					normal_table_select1 = huff1;
					normal_table_select2 = huff2;
					normal_table_select3 = huff3;
					normal_region_0_count = r0;
					normal_region_1_count = r1;
				}
			) else (
				let block_type_index = gs 2 in
				let mixed_block = gs 1 in
				let huff1 = gs 5 in
				let huff2 = gs 5 in
				let sb_gain1 = gs 3 in
				let sb_gain2 = gs 3 in
				let sb_gain3 = gs 3 in
				Window_other {
					other_block_type = [| Block_type_long; Block_type_start; Block_type_short; Block_type_stop |].(block_type_index);
					other_mixed_block = (mixed_block = 1);
					other_table_select1 = huff1;
					other_table_select2 = huff2;
					other_sub_block_gain1 = sb_gain1;
					other_sub_block_gain2 = sb_gain2;
					other_sub_block_gain3 = sb_gain3;
				}
			) in
			let sf_scale = gs 1 in
			let count1_table = gs 1 in
			({
				gc_part2_3_length = part2_3_length;
				gc_part2_3_offset = part2_3_offset;
				gc_big_values = big_values;
				gc_global_gain = global_gain;
				gc_scf_compress_index = scf_compress;
				gc_window = window;
				gc_pre_flag = false; (* No pre flag for MPEG2 *)
				gc_sf_scale = sf_scale;
				gc_count1_table_1 = (count1_table = 1);
			}, part2_3_offset + part2_3_length)
		) in

		match f.f1_header.header_channel_mode with
		| Mono -> (
			let main_data = gs 8 in
			let _ = gs 1 in
			let (side_gc, _) = read_gc 0 in
			{
				side_main_data_begin = main_data;
				side_scfi = SCFI_none;
				side_gc = GC_2_mono side_gc
			}
		)
		| Stereo _ -> (
			let main_data = gs 8 in
			let _ = gs 2 in
			let (side_gc1, new_so_far) = read_gc 0 in
			let (side_gc2,     _     ) = read_gc new_so_far in
			{
				side_main_data_begin = main_data;
				side_scfi = SCFI_none;
				side_gc = GC_2_stereo (side_gc1, side_gc2);
			}
		) (* Mono / stereo *)
	)
;;

















let decode_frame : type id chan. _ -> _ -> (id,chan) f1_t -> (id,chan) frame_data_t * bool = fun state file_state f ->
	let p = state.q_print_recompress in

	let side_info = decode_side_info f in

(*	print_side side_info;*)

	let k = f.f1_header in
	(* This is needed for the new async quant readers *)
	let data_ptr = Ptr.Ref.to_ptr f.f1_data in
	let data_ptr_ref = Ptr.Ref.of_ptr data_ptr in

	let replace_side_info : type id chan. (id,chan) side_internal_t -> (id,chan) side_gc_selector_t -> (id,chan) side_internal_t = fun s r ->
		match s with
		| {side_gc = GC_1_mono _} -> {s with side_gc = r}
		| {side_gc = GC_1_stereo _} -> {s with side_gc = r}
		| {side_gc = GC_2_mono _} -> {s with side_gc = r}
		| {side_gc = GC_2_stereo _} -> {s with side_gc = r}
	in
(*	let update_frame_data : type id chan. (id,chan) Types.Types2.f1_t -> (id,chan) side_gc_selector_t -> ( *)

	match f.f1_header(*(f.f1_header.header_id, f.f1_header.header_channel_mode)*) with
	| {header_id = MPEG1; header_channel_mode = Mono}(*(MPEG1, Mono)*) -> (
		p [Str "DECODE FRAME "; Int f.f1_num; Str " (MPEG1, MONO)"];
		p [Str "  Side "; Ptrref f.f1_side.side_raw];
		p [Str "  Data "; Ptrref f.f1_data];
		if state.q_debug_recompress then print_side p 4 side_info;
		p [Str "  Gr0:"];

		let GC_1_mono (gc0,gc1) = side_info.side_gc in
		let SCFI_mono scfi = side_info.side_scfi in

		let s = Ptr.Ref.new_seq f.f1_data in
		Ptr.Ref.set_seq s gc0.gc_part2_3_offset;

		(* The rehuff does nothing for non-normal windows, so just get the scf bands for non-short blocks *)
		let scf_bands_ptr = global_scalefactors_ptr f.f1_header.header_samplerate false in

		let (scf0, qp0, error0) = read_scalefactors_m1 state no_scfi None       gc0 s  file_state k data_ptr in
		let gc0 = rehuff_granule state qp0 gc0 scf_bands_ptr in

		Ptr.Ref.set_seq s gc1.gc_part2_3_offset;
		p [Str "  Gr1:"];
		let (scf1, qp1, error1) = read_scalefactors_m1 state scfi       (Some scf0) gc1 s  file_state k data_ptr in
		let gc1 = rehuff_granule state qp1 gc1 scf_bands_ptr in

(*		let si2 =  in*)

		(
			M1_frame_data {
				m1_header = f.f1_header;
				m1_side_info = replace_side_info side_info (GC_1_mono (gc0,gc1));
				m1_scalefactors = M1_scalefactors_mono (scf0,scf1);
				m1_quantizer_ptrs = M1_quantizers_mono (qp0,qp1);
				m1_starting_f1 = f;
			}
		,
			error0 || error1
		)
	)
	| {header_id = MPEG1; header_channel_mode = Stereo _}(*(MPEG1, Stereo _)*) -> (
		p [Str "DECODE FRAME "; Int f.f1_num; Str " (MPEG1, STEREO)"];
		p [Str "  Side "; Ptrref f.f1_side.side_raw];
		p [Str "  Data "; Ptrref f.f1_data];
		if state.q_debug_recompress then print_side p 4 side_info;

		let s = Ptr.Ref.new_seq data_ptr_ref in
		let GC_1_stereo (gc00,gc01,gc10,gc11) = side_info.side_gc in
		let SCFI_stereo (scfi_ch0, scfi_ch1) = side_info.side_scfi in

		(* things for inline rehuff *)
		(* The rehuff does nothing for non-normal windows, so just get the scf bands for non-short blocks *)
		let scf_bands_ptr = global_scalefactors_ptr f.f1_header.header_samplerate false in

		p [Str "  Gr0 Ch0:"];
		Ptr.Ref.set_seq s gc00.gc_part2_3_offset;
		let (scf00, qp00, error00) = read_scalefactors_m1 state no_scfi None        gc00 s  file_state k data_ptr in
		let gc00 = rehuff_granule state qp00 gc00 scf_bands_ptr in

		p [Str "  Gr0 Ch1:"];
		Ptr.Ref.set_seq s gc01.gc_part2_3_offset;
		let (scf01, qp01, error01) = read_scalefactors_m1 state no_scfi None        gc01 s  file_state k data_ptr in
		let gc01 = rehuff_granule state qp01 gc01 scf_bands_ptr in

		p [Str "  Gr1 Ch0:"];
		Ptr.Ref.set_seq s gc10.gc_part2_3_offset;
		let (scf10, qp10, error10) = read_scalefactors_m1 state scfi_ch0       (Some scf00) gc10 s  file_state k data_ptr in
		let gc10 = rehuff_granule state qp10 gc10 scf_bands_ptr in

		p [Str "  Gr1 Ch1:"];
		Ptr.Ref.set_seq s gc11.gc_part2_3_offset;
		let (scf11, qp11, error11) = read_scalefactors_m1 state scfi_ch1       (Some scf01) gc11 s  file_state k data_ptr in
		let gc11 = rehuff_granule state qp11 gc11 scf_bands_ptr in


		p [Str "Done decoding"];

		(
			M1_frame_data {
				m1_header = f.f1_header;
				m1_side_info = {side_info with side_gc = GC_1_stereo (gc00,gc01,gc10,gc11)};
				m1_scalefactors = M1_scalefactors_stereo (scf00,scf01,scf10,scf11);
				m1_quantizer_ptrs = M1_quantizers_stereo (qp00,qp01,qp10,qp11);
				m1_starting_f1 = f;
			}
		,
			error00 || error01 || error10 || error11
		)
	)
	| {header_id = MPEG2 _; header_channel_mode = Mono}(*(MPEG2 _, Mono)*) -> (
		p [Str "DECODE FRAME "; Int f.f1_num; Str " (MPEG2, MONO)"];
		p [Str "  Side "; Ptrref f.f1_side.side_raw];
		p [Str "  Data "; Ptrref f.f1_data];
		if state.q_debug_recompress then print_side p 4 side_info;

		p [Str "Gr:"];
		let s = Ptr.Ref.new_seq f.f1_data in
		let GC_2_mono gc = side_info.side_gc in
		Ptr.Ref.set_seq s gc.gc_part2_3_offset;

		let scf_bands_ptr = global_scalefactors_ptr f.f1_header.header_samplerate false in

		let scf = read_scalefactors_m2 state false gc s in
		let ((*q,*) qp, error) = read_quantizers state file_state k gc data_ptr s.Ptr.Ref.seq_at (gc.gc_part2_3_length + gc.gc_part2_3_offset) in
		let gc = rehuff_granule state qp gc scf_bands_ptr in

		(
			M2_frame_data {
				m2_header = f.f1_header;
				m2_side_info = {side_info with side_gc = GC_2_mono gc};
				m2_scalefactors = M2_scalefactors_mono scf;
				m2_quantizer_ptrs = M2_quantizers_mono qp;
				m2_starting_f1 = f;
			}
		,
			error
		)
	)
	| {header_id = MPEG2 _; header_channel_mode = Stereo _}(*(MPEG2 _, Stereo _)*) -> (
		p [Str "DECODE FRAME "; Int f.f1_num; Str " (MPEG2, STEREO)"];
		p [Str "  Side "; Ptrref f.f1_side.side_raw];
		p [Str "  Data "; Ptrref f.f1_data];
		if state.q_debug_recompress then print_side p 4 side_info;

		(* Remember that the IS should only be set on the right channel; the left channel uses the same scalefactors as non-IS GCs *)
		p [Str "  Gr0:"];
		let s = Ptr.Ref.new_seq f.f1_data in
		let GC_2_stereo (gc0,gc1) = side_info.side_gc in

		Ptr.Ref.set_seq s gc0.gc_part2_3_offset;

		let scf_bands_ptr = global_scalefactors_ptr f.f1_header.header_samplerate false in

		let scf0 = read_scalefactors_m2 state         false         gc0 s in
		let ((*q0,*) qp0, error0) = read_quantizers state file_state k gc0 data_ptr s.Ptr.Ref.seq_at (gc0.gc_part2_3_length + gc0.gc_part2_3_offset) in
		let gc0 = rehuff_granule state qp0 gc0 scf_bands_ptr in

		let is_is = match f.f1_header.header_channel_mode with
			| Stereo Stereo_joint {js_is = is} -> is
			| _ -> false
		in

		p [Str "  Gr1:"];
		Ptr.Ref.set_seq s gc1.gc_part2_3_offset;
		let scf1 = read_scalefactors_m2 state is_is(*f.f1_header.header_is*) gc1 s in
		let ((*q1,*) qp1, error1) = read_quantizers state file_state k gc1 data_ptr s.Ptr.Ref.seq_at (gc1.gc_part2_3_length + gc1.gc_part2_3_offset) in
		let gc1 = rehuff_granule state qp1 gc1 scf_bands_ptr in

		(
			M2_frame_data {
				m2_header = f.f1_header;
				m2_side_info = {side_info with side_gc = GC_2_stereo (gc0,gc1)};
				m2_scalefactors = M2_scalefactors_stereo (scf0,scf1);
(*				m2_quantizers = [| q0; q1 |];*)
				m2_quantizer_ptrs = M2_quantizers_stereo (qp0,qp1);
				m2_starting_f1 = f;
			}
		,
			error0 || error1
		)
	)
;;





(*
## Side info:
# 9/17/32 BYTES = 136/256 bits
# 9: main_data_begin (8 for MPEG2)
# ?: private_bits
# 4: SCFI Band
# 59: Side Information Granule
#  12: part2_3 length (main data for this channel, granule in bits)
#  9: Big values
#  8: Global gain
#  4: Scalefactor compress (9 for MPEG2)
#  1: Window switch flag
#   if 1:
#    2: Block type
#    1: Mix block flag
#    5x2: Table Select [region]
#    3x3: sub_block_gain [window]
#   if 0:
#    5x3: Table select [region]
#    4: Region 0 count
#    3: Region 1 count
#  1: Pre flag (NOT FOR MPEG2)
#  1: Scale factor scale
#  1: Count1 table select

# MPEG1 mono:
# [9 main data] [5 privates] [4 SCFI] [59 Gr0] [59 Gr1]
# (18 - 30) (77 - 89)
# MPEG1 stereo:
# [9 main data] [3 privates] [4 SCFI0] [4 SCFI1] [59 Gr0ch1] [59 Gr0ch2] [59 Gr1ch1] [59 Gr1ch2]
# 20 79 138 197
# MPEG2 mono:
# [8 main data] [1 privates] [63 Gr*]
# (9 - 21)
# MPEG2 stereo:
# [8 main data] [2 privates] [63 Gr*ch1] [63 Gr*ch2]
# (10 - 22) (73 - 85)
*)

(**********************************************************************************************)
(* ENCODE FRAME ENCODE FRAME ENCODE FRAME ENCODE FRAME ENCODE FRAME ENCODE FRAME ENCODE FRAME *)
(**********************************************************************************************)
let encode_frame : type id chan. _ -> (id,chan) frame_data_t -> (id,chan) f1_t = fun state d ->
	let p = state.q_print_recompress in

	let write_granule_m1 k s (scfsia,scfsib,scfsic,scfsid) gc scf (*quants*) quants_ptr = (
		(* Scalefactors *)
		let (scf_bits1, scf_bits2) = scalefactor_compress_m1.(gc.gc_scf_compress_index) in
		let (num1, _(*num2*)) = (match gc.gc_window with
			| Window_normal _ -> (11,10)
			| Window_other x when x.other_block_type <> Block_type_short -> (11,10)
			| Window_other x when x.other_mixed_block -> (17,18)
			| Window_other _ -> (18,18)
		) in
		let rec write_scf i = (
			let num_bits = if i < num1 then scf_bits1 else scf_bits2 in
			if i >= Array.length scf then (
				p [Str "!"];
			) else if i < 6 then (
				if scfsia then () else (p [Str "0"]; Ptr.put_seq s num_bits scf.(i));
				write_scf (succ i)
			) else if i < 11 then (
				if scfsib then () else (p [Str "1"]; Ptr.put_seq s num_bits scf.(i));
				write_scf (succ i)
			) else if i < 16 then (
				if scfsic then () else (p [Str "2"]; Ptr.put_seq s num_bits scf.(i));
				write_scf (succ i)
			) else if i < 21 then (
				if scfsid then () else (p [Str "3"]; Ptr.put_seq s num_bits scf.(i));
				write_scf (succ i)
			) else (
				p [Str "+"]; Ptr.put_seq s num_bits scf.(i);
				write_scf (succ i)
			)
		) in
		write_scf 0;
(*		if debug && debug_more then Printf.printf "\n";*)

		(* Quantizers *)
		let (region0, region1, region2, table0, table1, table2) = (match gc.gc_window with
			| Window_normal w -> (
				let scfend0 = w.normal_region_0_count + 1 in
				let scfend1 = w.normal_region_1_count + 1 + scfend0 in
				let region0 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend0) lsr 1) in
				let region1 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend1) lsr 1) - region0 in
				(region0, region1, gc.gc_big_values - region0 - region1, w.normal_table_select1, w.normal_table_select2, w.normal_table_select3)
			)
			| Window_other w -> (
				let region0 = if w.other_block_type = Block_type_short && not w.other_mixed_block then (
					(global_scalefactors k.header_samplerate true).(9 / 3) lsr 1 * 3
				) else (
					(global_scalefactors k.header_samplerate false).(8) lsr 1
				) in
				(min region0 gc.gc_big_values, max 0 (gc.gc_big_values - region0), 0, w.other_table_select1, w.other_table_select2, 0)
			)
		) in
		p [Str "Writing ("; Int region0; Str ","; Int region1; Str ","; Int region2; Str ") pairs with tables ("; Int table0; Str ","; Int table1; Str ","; Int table2; Str ")"];


		let (ptr,b) = Ptr.finalize_seq s in
		let new_bits = write_all_to_frame quants_ptr region0 table0 region1 table1 region2 table2 gc.gc_count1_table_1 ptr b in
		let new_byte = new_bits lsr 3 in
		if new_bits land 7 = 0 then (
			s.Ptr.seq_now_int <- 0;
		) else (
			s.Ptr.seq_now_int <- (Ptr.get_int_of_8u ptr new_byte) lsr (8 - (new_bits land 7));
		);
		s.Ptr.seq_at <- new_bits;
		s.Ptr.seq_now_bits <- new_bits land 7;
		s.Ptr.seq_now_byte <- new_byte;
		p [Str "Ended up at "; Int s.Ptr.seq_at];

	) in

	let write_granule_m2 k s is gc scf (*quants*) quants_ptr = (
		(* Just copy the original layout; I doubt there's much savings to be had here *)
		let (bits0, bits1, bits2, bits3) = if is then (
			scalefactor_compress_m2_is.(gc.gc_scf_compress_index)
		) else (
			scalefactor_compress_m2.(gc.gc_scf_compress_index)
		) in
		let (num0, num1, num2, num3) = scalefactor_bands_m2 is gc in
		p [Str "Writing "; Int bits0; Str ","; Int bits1; Str ","; Int bits2; Str ","; Int bits3; Str " bits to "; Int num0; Str ","; Int num1; Str ","; Int num2; Str ","; Int num3; Str " bands"];

		let sob1 = num0 in
		let sob2 = sob1 + num1 in
		let sob3 = sob2 + num2 in
		let num_total = sob3 + num3 in

		let rec write_scf i = (
			if i < sob1 then (
				p [Str "0"]; Ptr.put_seq s bits0 scf.(i);
				write_scf (succ i)
			) else if i < sob2 then (
				p [Str "1"]; Ptr.put_seq s bits1 scf.(i);
				write_scf (succ i)
			) else if i < sob3 then (
				p [Str "2"]; Ptr.put_seq s bits2 scf.(i);
				write_scf (succ i)
			) else if i < num_total then (
				p [Str "3"]; Ptr.put_seq s bits3 scf.(i);
				write_scf (succ i)
			) else (
				p [Str "!"];
			)
		) in
		write_scf 0;
(*		if debug && debug_more then Printf.printf "\n";*)

		(* Quants *)
		let (region0, region1, region2, table0, table1, table2) = (match gc.gc_window with
			| Window_normal w -> (
				let scfend0 = w.normal_region_0_count + 1 in
				let scfend1 = w.normal_region_1_count + 1 + scfend0 in
				let region0 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend0) lsr 1) in
				let region1 = min gc.gc_big_values ((global_scalefactors k.header_samplerate false).(scfend1) lsr 1) - region0 in
				(region0, region1, gc.gc_big_values - region0 - region1, w.normal_table_select1, w.normal_table_select2, w.normal_table_select3)
			)
			| Window_other w -> (
				let region0 = if w.other_block_type = Block_type_short && not w.other_mixed_block then (
					(global_scalefactors k.header_samplerate true).(9 / 3) lsr 1 * 3
				) else (
					(global_scalefactors k.header_samplerate false).(8) lsr 1
				) in
				(min region0 gc.gc_big_values, max 0 (gc.gc_big_values - region0), 0, w.other_table_select1, w.other_table_select2, 0)
			)
		) in

		let (ptr,b) = Ptr.finalize_seq s in
		let new_bits = write_all_to_frame quants_ptr region0 table0 region1 table1 region2 table2 gc.gc_count1_table_1 ptr b in
		let new_byte = new_bits lsr 3 in
		if new_bits land 7 = 0 then (
			s.Ptr.seq_now_int <- 0;
		) else (
			s.Ptr.seq_now_int <- (Ptr.get_int_of_8u ptr new_byte) lsr (8 - (new_bits land 7));
		);
		s.Ptr.seq_at <- new_bits;
		s.Ptr.seq_now_bits <- new_bits land 7;
		s.Ptr.seq_now_byte <- new_byte;

		p [Str "Ended up at "; Int s.Ptr.seq_at];

	) in




	match d with
	| M1_frame_data ({m1_header = {header_channel_mode = Mono}} as m) -> (
		(* MONO MPEG1 *)
		let k = m.m1_header in

		(*************)
		(* Parts 2,3 *)
		(*************)
		(* 1931 bytes is the maximum for one frame. It fills up all 1441 bytes (minus 4 for header and 17 for side) of a 32khz 320kbps padded frame plus 511 bytes for the reservoir *)
		let out_ptr = Ptr.clearret (Ptr.make 1931 0) in (* 2881??? Where did I get that from? *)
		let s = Ptr.new_seq out_ptr in
		let GC_1_mono (gc0,gc1) = m.m1_side_info.side_gc in
		let M1_scalefactors_mono (scf0,scf1) = m.m1_scalefactors in
		let M1_quantizers_mono (qp0,qp1) = m.m1_quantizer_ptrs in
		let SCFI_mono scfi = m.m1_side_info.side_scfi in

		let r0 = s.Ptr.seq_at in
		p [Str "Doing first granule at "; Int r0; Str " (had better be 0)"];
		write_granule_m1 k s no_scfi gc0 scf0 qp0;
		let r1 = s.Ptr.seq_at in
		p [Str "First granule done; starting second at "; Int r1];
		write_granule_m1 k s scfi    gc1 scf1 qp1;
		let r2 = s.Ptr.seq_at in
		p [Str "Second granule done at "; Int r2];

		let (side_raw_ptr, side_bits, side_bytes) =
			let p = Ptr.make 17 0 in
			let s = m.m1_side_info in
			let pb = Ptr.put_bits p in
			pb 0 9 s.side_main_data_begin;
			pb 9 5 0;
			let (scfia,scfib,scfic,scfid) = scfi in
			pb 14 1 (if scfia then 1 else 0);
			pb 15 1 (if scfib then 1 else 0);
			pb 16 1 (if scfic then 1 else 0);
			pb 17 1 (if scfid then 1 else 0);
			let pack_gc gc bits o = (
				pb (o +  0) 12 bits;
				pb (o + 12)  9 gc.gc_big_values;
				pb (o + 21)  8 gc.gc_global_gain;
				pb (o + 29)  4 gc.gc_scf_compress_index;
				(match gc.gc_window with
					| Window_normal w -> (
						pb (o + 33) 1 0;
						pb (o + 34) 5 w.normal_table_select1;
						pb (o + 39) 5 w.normal_table_select2;
						pb (o + 44) 5 w.normal_table_select3;
						pb (o + 49) 4 w.normal_region_0_count;
						pb (o + 53) 3 w.normal_region_1_count;
					)
					| Window_other w -> (
						pb (o + 33) 1 1;
						pb (o + 34) 2 (match w.other_block_type with | Block_type_long -> 0 | Block_type_start -> 1 | Block_type_short -> 2 | Block_type_stop -> 3);
						pb (o + 36) 1 (if w.other_mixed_block then 1 else 0);
						pb (o + 37) 5 w.other_table_select1;
						pb (o + 42) 5 w.other_table_select2;
						pb (o + 47) 3 w.other_sub_block_gain1;
						pb (o + 50) 3 w.other_sub_block_gain2;
						pb (o + 53) 3 w.other_sub_block_gain3;
					)
				);
				pb (o + 56) 1 (if gc.gc_pre_flag then 1 else 0);
				pb (o + 57) 1 gc.gc_sf_scale;
				pb (o + 58) 1 (if gc.gc_count1_table_1 then 1 else 0);
			) in
			pack_gc gc0 (r1 - r0) 18;
			pack_gc gc1 (r2 - r1) 77;
			(p, Bits_1_mono (r1 - r0, r2 - r1), (r2 - r0 + 7) asr 3)
		in

		p [Str "Done packing the side info"];

		let (data_ptr, data_bits) = Ptr.finalize_seq s in

		{
			f1_num = m.m1_starting_f1.f1_num;
			f1_header = m.m1_starting_f1.f1_header;
			f1_side = {
				side_raw = Ptr.Ref.of_ptr side_raw_ptr;
				side_offset = 0; (* not used *)
				side_bits = side_bits;
				side_bytes = side_bytes;
			};
			f1_data = Ptr.Ref.of_subptr data_ptr 0 ((data_bits + 7) lsr 3);
			f1_pad_exact = None;
		}
	)
	| M1_frame_data ({m1_header = {header_channel_mode = Stereo _}} as m) -> (
		(* STEREO MPEG1 *)
		let k = m.m1_header in

		(*************)
		(* Parts 2,3 *)
		(*************)
		(* 1916 bytes is the maximum for one frame. It fills up all 1441 bytes (minus 4 for header and 32 for side) of a 32khz 320kbps padded frame plus 511 bytes for the reservoir *)
		let out_ptr = Ptr.clearret (Ptr.make 1916 0) in (* 2881 *)
		let s = Ptr.new_seq out_ptr in
		let GC_1_stereo (gc00,gc01,gc10,gc11) = m.m1_side_info.side_gc in
		let M1_scalefactors_stereo (scf00,scf01,scf10,scf11) = m.m1_scalefactors in
		let M1_quantizers_stereo (qp00,qp01,qp10,qp11) = m.m1_quantizer_ptrs in
		let SCFI_stereo (scfi_ch0, scfi_ch1) = m.m1_side_info.side_scfi in

		let r0 = s.Ptr.seq_at in (* had better be 0 *)
		p [Str "Doing first granule at "; Int r0; Str " (had better be 0)"];
		write_granule_m1 k s no_scfi gc00 scf00 (*m.m1_quantizers.(0).(0)*) qp00;
		let r1 = s.Ptr.seq_at in
		p [Str "First granule done; starting second at "; Int r1];
		write_granule_m1 k s no_scfi gc01 scf01 (*m.m1_quantizers.(0).(1)*) qp01;
		let r2 = s.Ptr.seq_at in
		p [Str "Second granule done; starting third at "; Int r2];
		write_granule_m1 k s scfi_ch0  gc10 scf10 (*m.m1_quantizers.(1).(0)*) qp10;
		let r3 = s.Ptr.seq_at in
		p [Str "Third granule done; starting fourth at "; Int r3];
		write_granule_m1 k s scfi_ch1  gc11 scf11 (*m.m1_quantizers.(1).(1)*) qp11;
		let r4 = s.Ptr.seq_at in
		p [Str "Fourth granule done at "; Int r4];

		(************************)
		(* Encode the side info *)
		(************************)
		let (side_raw_ptr, side_bits, side_bytes) = (
			let p = Ptr.make 32 0 in
			let s = m.m1_side_info in
			let pb = Ptr.put_bits p in
			pb  0 9 s.side_main_data_begin;
			pb  9 3 0;
			let (scfia,scfib,scfic,scfid) = scfi_ch0 in
			pb 12 1 (if scfia then 1 else 0);
			pb 13 1 (if scfib then 1 else 0);
			pb 14 1 (if scfic then 1 else 0);
			pb 15 1 (if scfid then 1 else 0);
			let (scfia,scfib,scfic,scfid) = scfi_ch1 in
			pb 16 1 (if scfia then 1 else 0);
			pb 17 1 (if scfib then 1 else 0);
			pb 18 1 (if scfic then 1 else 0);
			pb 19 1 (if scfid then 1 else 0);
			let pack_gc gc bits o = (
				pb (o +  0) 12 bits;
				pb (o + 12)  9 gc.gc_big_values;
				pb (o + 21)  8 gc.gc_global_gain;
				pb (o + 29)  4 gc.gc_scf_compress_index;
				(match gc.gc_window with
					| Window_normal w -> (
						pb (o + 33) 1 0;
						pb (o + 34) 5 w.normal_table_select1;
						pb (o + 39) 5 w.normal_table_select2;
						pb (o + 44) 5 w.normal_table_select3;
						pb (o + 49) 4 w.normal_region_0_count;
						pb (o + 53) 3 w.normal_region_1_count;
					)
					| Window_other w -> (
						pb (o + 33) 1 1;
						pb (o + 34) 2 (match w.other_block_type with | Block_type_long -> 0 | Block_type_start -> 1 | Block_type_short -> 2 | Block_type_stop -> 3);
						pb (o + 36) 1 (if w.other_mixed_block then 1 else 0);
						pb (o + 37) 5 w.other_table_select1;
						pb (o + 42) 5 w.other_table_select2;
						pb (o + 47) 3 w.other_sub_block_gain1;
						pb (o + 50) 3 w.other_sub_block_gain2;
						pb (o + 53) 3 w.other_sub_block_gain3;
					)
				);
				pb (o + 56) 1 (if gc.gc_pre_flag then 1 else 0);
				pb (o + 57) 1 gc.gc_sf_scale;
				pb (o + 58) 1 (if gc.gc_count1_table_1 then 1 else 0);
			) in
			pack_gc gc00 (r1 - r0)  20;
			pack_gc gc01 (r2 - r1)  79;
			pack_gc gc10 (r3 - r2) 138;
			pack_gc gc11 (r4 - r3) 197;
			(p, Bits_1_stereo (r1 - r0, r2 - r1, r3 - r2, r4 - r3), (r4 - r0 + 7) asr 3)
		) in

		p [Str "Done packing the side info"];

		let (data_ptr, data_bits) = Ptr.finalize_seq s in

		{
			f1_num = m.m1_starting_f1.f1_num;
			f1_header = m.m1_starting_f1.f1_header;
			f1_side = {
				side_raw = Ptr.Ref.of_ptr side_raw_ptr;
				side_offset = 0; (* not used *)
				side_bits = side_bits;
				side_bytes = side_bytes;
			};
			f1_data = Ptr.Ref.of_subptr data_ptr 0 ((data_bits + 7) lsr 3);
			f1_pad_exact = None;
		}
	)
	| M2_frame_data ({m2_header = {header_channel_mode = Mono}} as m) -> (
		
		let k = m.m2_header in

		(* 1441 bytes per frame (8khz, 160kbps, padded) - 4 byte header - 9 byte side + 255 byte reservoir *)
		let out_ptr = Ptr.clearret (Ptr.make 1683 0) in (* 5761??? *)
		let s = Ptr.new_seq out_ptr in
		let GC_2_mono gc = m.m2_side_info.side_gc in
		let M2_scalefactors_mono scf = m.m2_scalefactors in
		let M2_quantizers_mono qp = m.m2_quantizer_ptrs in

		let r0 = s.Ptr.seq_at in
		p [Str "Doing first (and only) granule at "; Int r0; Str " (had better be 0)"];
		write_granule_m2 k s false gc scf (*m.m2_quantizers.(0)*) qp;
		let r1 = s.Ptr.seq_at in
		p [Str "Granule done at "; Int r1];

		let (side_raw_ptr, side_bits, side_bytes) = (
			let p = Ptr.make 9 0 in
			let s = m.m2_side_info in
			let pb = Ptr.put_bits p in
			pb  0 8 s.side_main_data_begin;
			pb  8 2 0;
			let pack_gc gc bits o = (
				pb (o +  0) 12 bits;
				pb (o + 12)  9 gc.gc_big_values;
				pb (o + 21)  8 gc.gc_global_gain;
				pb (o + 29)  9 gc.gc_scf_compress_index;
				(match gc.gc_window with
					| Window_normal w -> (
						pb (o + 38) 1 0;
						pb (o + 39) 5 w.normal_table_select1;
						pb (o + 44) 5 w.normal_table_select2;
						pb (o + 49) 5 w.normal_table_select3;
						pb (o + 54) 4 w.normal_region_0_count;
						pb (o + 58) 3 w.normal_region_1_count;
					)
					| Window_other w -> (
						pb (o + 38) 1 1;
						pb (o + 39) 2 (match w.other_block_type with | Block_type_long -> 0 | Block_type_start -> 1 | Block_type_short -> 2 | Block_type_stop -> 3);
						pb (o + 41) 1 (if w.other_mixed_block then 1 else 0);
						pb (o + 42) 5 w.other_table_select1;
						pb (o + 47) 5 w.other_table_select2;
						pb (o + 52) 3 w.other_sub_block_gain1;
						pb (o + 55) 3 w.other_sub_block_gain2;
						pb (o + 58) 3 w.other_sub_block_gain3;
					)
				);
				pb (o + 61) 1 gc.gc_sf_scale;
				pb (o + 62) 1 (if gc.gc_count1_table_1 then 1 else 0);
			) in
			pack_gc gc (r1 - r0) 9;
			(p, Bits_2_mono (r1 - r0), (r1 - r0 + 7) asr 3)
		) in
		
		p [Str "Done packing the side info"];

		let (data_ptr, data_bits) = Ptr.finalize_seq s in

		{
			f1_num = m.m2_starting_f1.f1_num;
			f1_header = m.m2_starting_f1.f1_header;
			f1_side = {
				side_raw = Ptr.Ref.of_ptr side_raw_ptr;
				side_offset = 0; (* not used *)
				side_bits = side_bits;
				side_bytes = side_bytes;
			};
			f1_data = Ptr.Ref.of_subptr data_ptr 0 ((data_bits + 7) lsr 3);
			f1_pad_exact = None;
		}
	)
	| M2_frame_data ({m2_header = {header_channel_mode = Stereo _}} as m) -> (
		let k = m.m2_header in

		(* 1441 bytes per frame (8khz, 160kbps, padded) - 4 byte header - 17 byte side + 255 byte reservoir *)
		let out_ptr = Ptr.clearret (Ptr.make 1675 0) in (* 5761??? *)
		let s = Ptr.new_seq out_ptr in
		let GC_2_stereo (gc0,gc1) = m.m2_side_info.side_gc in
		let M2_scalefactors_stereo (scf0,scf1) = m.m2_scalefactors in
		let M2_quantizers_stereo (qp0,qp1) = m.m2_quantizer_ptrs in
		let is_is = match m.m2_header.header_channel_mode with
			| Stereo Stereo_joint {js_is = is} -> is
			| _ -> false
		in

		let r0 = s.Ptr.seq_at in
		p [Str "Doing first granule at "; Int r0; Str " (had better be 0)"];
		write_granule_m2 k s false gc0 scf0 (*m.m2_quantizers.(0)*) qp0;
		let r1 = s.Ptr.seq_at in
		p [Str "First granule done; starting second at "; Int r1];
		write_granule_m2 k s is_is gc1 scf1 (*m.m2_quantizers.(1)*) qp1;
		let r2 = s.Ptr.seq_at in
		p [Str "Second granule done at "; Int r2];

		let (side_raw_ptr, side_bits, side_bytes) = (
			let p = Ptr.make 17 0 in
			let s = m.m2_side_info in
			let pb = Ptr.put_bits p in
			pb  0 8 s.side_main_data_begin;
			pb  8 3 0;
			let pack_gc gc bits o = (
				pb (o +  0) 12 bits;
				pb (o + 12)  9 gc.gc_big_values;
				pb (o + 21)  8 gc.gc_global_gain;
				pb (o + 29)  9 gc.gc_scf_compress_index;
				(match gc.gc_window with
					| Window_normal w -> (
						pb (o + 38) 1 0;
						pb (o + 39) 5 w.normal_table_select1;
						pb (o + 44) 5 w.normal_table_select2;
						pb (o + 49) 5 w.normal_table_select3;
						pb (o + 54) 4 w.normal_region_0_count;
						pb (o + 58) 3 w.normal_region_1_count;
					)
					| Window_other w -> (
						pb (o + 38) 1 1;
						pb (o + 39) 2 (match w.other_block_type with | Block_type_long -> 0 | Block_type_start -> 1 | Block_type_short -> 2 | Block_type_stop -> 3);
						pb (o + 41) 1 (if w.other_mixed_block then 1 else 0);
						pb (o + 42) 5 w.other_table_select1;
						pb (o + 47) 5 w.other_table_select2;
						pb (o + 52) 3 w.other_sub_block_gain1;
						pb (o + 55) 3 w.other_sub_block_gain2;
						pb (o + 58) 3 w.other_sub_block_gain3;
					)
				);
				pb (o + 61) 1 gc.gc_sf_scale;
				pb (o + 62) 1 (if gc.gc_count1_table_1 then 1 else 0);
			) in
			pack_gc gc0 (r1 - r0) 10;
			pack_gc gc1 (r2 - r1) 73;
			(p, Bits_2_stereo (r1 - r0, r2 - r1), (r2 - r0 + 7) asr 3)
			
		) in
		
		p [Str "Done packing the side info"];

		let (data_ptr, data_bits) = Ptr.finalize_seq s in

		{
			f1_num = m.m2_starting_f1.f1_num;
			f1_header = m.m2_starting_f1.f1_header;
			f1_side = {
				side_raw = Ptr.Ref.of_ptr side_raw_ptr;
				side_offset = 0; (* not used *)
				side_bits = side_bits;
				side_bytes = side_bytes;
			};
			f1_data = Ptr.Ref.of_subptr data_ptr 0 ((data_bits + 7) lsr 3);
			f1_pad_exact = None;
		}
	)
;;

let recompress_frame : type id chan. _ -> _ -> (id,chan) f1_t -> (id,chan) f1_t * bool = fun state file_state f ->
	let p = state.q_print_recompress in

(*
	Printf.printf "%d\n" f.f1_num;
	Printf.printf " Input frame:  \"%s\"\n" (to_hex f.f1_string);
*)
(*	update_ref ();*)
	let (decoded, decoder_error) = decode_frame state file_state f in

	p [Str "Decoder error? "; Bool decoder_error];

	let rehuffed = (*if false then rehuff_frame ~debug:debug decoded else*) decoded in

	let encoded = encode_frame state rehuffed in

	if decoder_error then (
		p [Str "Returning original frame instead of the repacked one"]
	);

	(* I'm pretty sure I was trying to return the input frame in case of an error... *)
	let return_frame = if decoder_error then f else encoded in

	if true then (
		p [Str "  Header: "; Ptrref return_frame.f1_header.header_raw];
		p [Str "  Side:   "; Ptrref return_frame.f1_side.side_raw];
		p [Str "  Data:   "; Ptrref return_frame.f1_data];
	);

	(return_frame, decoder_error)
;;
