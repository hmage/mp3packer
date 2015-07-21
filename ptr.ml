type t;;

(* NEW PTR STYLE *)

external make : int -> int -> t = "ptr_make";;

external length : t -> int = "ptr_length" "noalloc";;
external align : t -> int = "ptr_align" "noalloc";;


(* VirtualAlloc things *)
(* Only for Windows! *)
external get_page_size : unit -> int = "ptr_get_page_size" "noalloc";;
let page_size = get_page_size ();;
external make_virtual_alloc_unsafe : int -> int -> t = "ptr_make_virtual_alloc";;
let make_page x = make_virtual_alloc_unsafe page_size x;;

(********)
(* BLIT *)
(********)

external clear : t -> unit = "ptr_clear" "noalloc";;

(* Good for doing "let x = Ptr.clearret (Ptr.make a b)" *)
let clearret x =
	clear x;
	x
;;

external blit_unsafe : t -> int -> t -> int -> int -> unit = "ptr_blit" "noalloc";;
let blit pfrom fromoff pto tooff len =
	if fromoff < 0 || fromoff + len > length pfrom || tooff < 0 || tooff + len > length pto || len < 0 then (
		invalid_arg "Ptr.blit"
	) else (
		blit_unsafe pfrom fromoff pto tooff len
	)
;;

external blit_from_string_unsafe : string -> int -> t -> int -> int -> unit = "ptr_blit_from_string" "noalloc";;
let blit_from_string s soff p poff len =
	if soff < 0 || soff + len > String.length s || poff < 0 || poff + len > length p || len < 0 then (
		invalid_arg "Ptr.blit_from_string"
	) else (
		blit_from_string_unsafe s soff p poff len
	)
;;

external blit_to_string_unsafe : t -> int -> string -> int -> int -> unit = "ptr_blit_to_string";;
let blit_to_string p poff s soff len =
	if soff < 0 || soff + len > String.length s || poff < 0 || poff + len > length p || len < 0 then (
		invalid_arg "Ptr.blit_to_string"
	) else (
		blit_to_string_unsafe p poff s soff len
	)
;;

let copy pin =
	let pout = make (length pin) (align pin) in
	blit_unsafe pin 0 pout 0 (length pin);
	pout
;;

let sub pin off len =
	if off < 0 || off + len > length pin then (
		invalid_arg "Ptr.sub"
	) else (
		let pout = make len (align pin) in
		blit_unsafe pin off pout 0 len;
		pout
	)
;;



(*********************)
(* FROM / TO STRINGS *)
(*********************)

let of_string s =
	let p = make (String.length s) 1 in
	blit_from_string_unsafe s 0 p 0 (String.length s);
	p
;;
let sub_to_string pin off len =
	if off < 0 || off + len > length pin then (
		invalid_arg "Ptr.sub_to_string"
	) else (
		let sout = String.create len in
		blit_to_string_unsafe pin off sout 0 len;
		sout
	)
;;
let to_string p =
	let s = String.create (length p) in
	blit_to_string_unsafe p 0 s 0 (length p);
	s
;;


(**************)
(* SMALL COPY *)
(**************)

(* PUT *)
let put_this name len f ptr off num =
	if off < 0 || off > length ptr - len then (
		invalid_arg name
	) else (
		f ptr off num
	)
;;

external put_8_of_int_unsafe    : t -> int -> int   -> unit = "ptr_put_8_of_int" "noalloc";;
external put_16_of_int_unsafe   : t -> int -> int   -> unit = "ptr_put_16_of_int" "noalloc";;
external put_32_of_int_unsafe   : t -> int -> int   -> unit = "ptr_put_32_of_int" "noalloc";;
external put_64_of_int_unsafe   : t -> int -> int   -> unit = "ptr_put_64_of_int" "noalloc";;
external put_64_of_int64_unsafe : t -> int -> int64 -> unit = "ptr_put_64_of_int64" "noalloc";;
external put_32_of_float_unsafe : t -> int -> float -> unit = "ptr_put_32_of_float" "noalloc";;
external put_64_of_float_unsafe : t -> int -> float -> unit = "ptr_put_64_of_float" "noalloc";;

let put_8_of_int    = put_this "Ptr.put_8_of_int"    1 put_8_of_int_unsafe;;
let put_16_of_int   = put_this "Ptr.put_16_of_int"   2 put_16_of_int_unsafe;;
let put_32_of_int   = put_this "Ptr.put_32_of_int"   4 put_32_of_int_unsafe;;
let put_64_of_int   = put_this "Ptr.put_64_of_int"   8 put_64_of_int_unsafe;;
let put_64_of_int64 = put_this "Ptr.put_64_of_int64" 8 put_64_of_int64_unsafe;;
let put_32_of_float = put_this "Ptr.put_32_of_float" 4 put_32_of_float_unsafe;;
let put_64_of_float = put_this "Ptr.put_64_of_float" 8 put_64_of_float_unsafe;;
let put_byte = put_8_of_int;;

(* GET *)
let get_this name len f ptr off =
	if off < 0 || off > length ptr - len then (
		invalid_arg name
	) else (
		f ptr off
	)
;;

(* "u" is unsigned *)
external get_int_of_8_unsafe    : t -> int -> int   = "ptr_get_int_of_8" "noalloc";;
external get_int_of_8u_unsafe   : t -> int -> int   = "ptr_get_int_of_8u" "noalloc";;
external get_int_of_16_unsafe   : t -> int -> int   = "ptr_get_int_of_16" "noalloc";;
external get_int_of_16u_unsafe  : t -> int -> int   = "ptr_get_int_of_16u" "noalloc";;
external get_int_of_32_unsafe   : t -> int -> int   = "ptr_get_int_of_32" "noalloc";;
external get_int_of_32u_unsafe  : t -> int -> int   = "ptr_get_int_of_32u" "noalloc";;
external get_int_of_64_unsafe   : t -> int -> int   = "ptr_get_int_of_64" "noalloc";;
external get_int_of_64u_unsafe  : t -> int -> int   = "ptr_get_int_of_64u" "noalloc";;
external get_int64_of_64_unsafe : t -> int -> int64 = "ptr_get_int64_of_64";;
external get_float_of_32_unsafe : t -> int -> float = "ptr_get_float_of_32";;
external get_float_of_64_unsafe : t -> int -> float = "ptr_get_float_of_64";;

let get_int_of_8    = get_this "Ptr.get_int_of_8"    1 get_int_of_8_unsafe;;
let get_int_of_8u   = get_this "Ptr.get_int_of_8u"   1 get_int_of_8u_unsafe;;
let get_int_of_16   = get_this "Ptr.get_int_of_16"   2 get_int_of_16_unsafe;;
let get_int_of_16u  = get_this "Ptr.get_int_of_16u"  2 get_int_of_16u_unsafe;;
let get_int_of_32   = get_this "Ptr.get_int_of_32"   4 get_int_of_32_unsafe;;
let get_int_of_32u  = get_this "Ptr.get_int_of_32u"  4 get_int_of_32u_unsafe;;
let get_int_of_64   = get_this "Ptr.get_int_of_64"   8 get_int_of_64_unsafe;;
let get_int_of_64u  = get_this "Ptr.get_int_of_64u"  8 get_int_of_64u_unsafe;;
let get_int64_of_64 = get_this "Ptr.get_int64_of_64" 8 get_int64_of_64_unsafe;;
let get_float_of_32 = get_this "Ptr.get_float_of_32" 4 get_float_of_32_unsafe;;
let get_float_of_64 = get_this "Ptr.get_float_of_64" 8 get_float_of_64_unsafe;;
let get_byte = get_int_of_8u;;


(* Byte swap functions *)
external put_16_of_int_bswap_unsafe : t -> int -> int -> unit = "ptr_put_16_of_int_bswap" "noalloc";;
external put_32_of_int_bswap_unsafe : t -> int -> int -> unit = "ptr_put_32_of_int_bswap" "noalloc";;
external put_32_of_float_bswap_unsafe : t -> int -> float -> unit = "ptr_put_32_of_float_bswap" "noalloc";;
let put_16_of_int_bswap = put_this "Ptr.put_16_of_int_bswap" 2 put_16_of_int_bswap_unsafe;;
let put_32_of_int_bswap = put_this "Ptr.put_32_of_int_bswap" 4 put_32_of_int_bswap_unsafe;;
let put_32_of_float_bswap = put_this "Ptr.put_32_of_float_bswap" 4 put_32_of_float_bswap_unsafe;;

external get_int_of_32u_bswap_unsafe : t -> int -> int = "ptr_get_int_of_32u_bswap" "noalloc";;
let get_int_of_32u_bswap = get_this "Ptr.get_int_of_32u_bswap" 4 get_int_of_32u_bswap_unsafe;;


(*****************)
(* BIT FUNCTIONS *)
(*****************)

(* This needs to be big-endian. I'll hard-code the bswap version here, but note that it will fail on big-endian machines *)
(* This function seems to fail over 256MiB on a 32-bit system, since that's where the number of bits will overflow to 0 *)
(* (using a negative number doesn't break since we do an lsr, not an asr *)
let get_bits ?(byte_off=0) ptr off len =
	let start_byte = off lsr 3 + byte_off in
	let end_byte = (off + len - 1) lsr 3 + byte_off in
	(* Remove the limitation of 30 bits per read in order to do some 32-bit reads in the XING tag *)
	if start_byte < 0 || end_byte >= length ptr (*|| len > 30*) then (
		invalid_arg "Ptr.get_bits";
	) else (
		let num_bytes_involved = end_byte - start_byte + 1 in
		let start_reading_byte = end_byte - 3 in (* The last byte should be set to the LSB of the read part. If we set the MSB to be the first byte then it may get chopped off by the OCaml 30-bit int limitation *)
		if false && num_bytes_involved <= 3 && start_reading_byte >= 0 then (
			(* Just do a single 32-bit read *)
			(* This breaks when accepting > 30-bit reads *)
			let raw = get_int_of_32u_bswap ptr start_reading_byte in
			let shift_by = 7 - (off + len - 1) land 7 in
			let shifted = raw lsr shift_by in
			shifted land ((1 lsl len) - 1)
		) else (
			let rec keep_reading so_far bits_left i =
(*				Printf.printf "  so_far=%d with %d bits left\n%!" so_far bits_left;*)
				if bits_left = 0 then (
					so_far
				) else if bits_left >= 8 then (
					let read_byte = get_int_of_8u ptr i in
					keep_reading ((so_far lsl 8) lor read_byte) (bits_left - 8) (succ i)
				) else (
					let read_byte = get_int_of_8u ptr i in
					(so_far lsl bits_left) lor (read_byte lsr (8 - bits_left))
				)
			in
			let first_bit_index = off land 7 in
			let got_too_much = keep_reading 0 (len + first_bit_index) start_byte in

(*			got_too_much land ((1 lsl len) - 1) (* XXX THIS IS A PROBLEM XXX *)*)
			if len >= Sys.word_size - 1 then (
				(* Everything's valid *)
				got_too_much
			) else (
				got_too_much land ((1 lsl len) - 1)
			)

(*
			let first_bits = 8 - (off land 7) in
			let first_mask = (1 lsl first_bits) - 1 in
			let first_byte = (get_int_of_8u ptr start_byte) land first_mask in
			if len >= first_bits then (
				keep_reading first_byte (len - first_bits) (succ start_byte)
			) else (
				raise Not_found
			)
*)
		)
	)
;;

let put_bits_simple ptr off len store =
	if off < 0 || off + len > length ptr lsl 3 || len > 30 then (
		invalid_arg "Ptr.put_bits";
	) else (
		for i = 0 to len - 1 do
			let byte = (off + i) lsr 3 in
			let bit =  (off + i) land 7 in
			let mask = 1 lsl (len - i - 1) in
			let out_mask = 1 lsl (7 - bit) in

			let orig = get_int_of_8u ptr byte in
			let cleared = orig land (0xFF lxor out_mask) in
			let set = ((if store land mask = 0 then 0 else 0xFF) land out_mask) in
			let gnu = cleared lor set in

			put_8_of_int ptr byte gnu;
		done
	)
;;

let put_bits ptr off len store =
	if off < 0 || off + len > length ptr lsl 3 || len > 30 then (
		invalid_arg "Ptr.put_bits";
	) else (
		let rec keep_going i =
			if i >= len then () else (
				let byte = (off + i) lsr 3 in
				let bit =  (off + i) land 7 in
				if bit = 0 && len - i >= 8 then (
					(* Do a byte at a time *)
					let mask = 0xFF lsl (len - i - 8) in
					let put_this = (mask land store) lsr (len - i - 8) in
					put_8_of_int ptr byte put_this;
					keep_going (i + 8)
				) else if (len - i) >= 8 - bit then (
					(* Do the rest of the byte *)
					let do_bits = 8 - bit in
					let out_mask = (1 lsl do_bits) - 1 in
					let mask = out_mask lsl (len - i - do_bits) in
					let put_this = (mask land store) lsr (len - i - do_bits) in

					let orig = get_int_of_8u ptr byte in
					let cleared = orig land (0xFF lxor out_mask) in
					let set = cleared lor put_this in

					put_8_of_int ptr byte set;
					keep_going (i + do_bits)
				) else (
					(* This can't overflow to the next byte since then it would have gotten caught by the above branch *)
					let do_bits = len - i in
					let bits_after = 8 - (bit + do_bits) in
					let mask = (1 lsl do_bits) - 1 in (* The mask is not shifted since this is always the end of the number *)
					let out_mask = mask lsl bits_after in

					let put_this = (mask land store) lsl bits_after in
					let orig = get_int_of_8u ptr byte in
					let cleared = orig land (0xFF lxor out_mask) in
					let set = cleared lor put_this in

					put_8_of_int ptr byte set;
				)
			)
		in
		keep_going 0
	)
;;


type seq_write_t = {
	seq_ptr : t;
	mutable seq_at : int;
	mutable seq_now_int : int;
	mutable seq_now_bits : int;
	mutable seq_now_byte : int;
};;
let new_seq p = {seq_ptr = p; seq_at = 0; seq_now_int = 0; seq_now_bits = 0; seq_now_byte = 0};;
let put_seq s num v =
	s.seq_now_int <- (s.seq_now_int lsl num) lor (v land ((1 lsl num) - 1));
	s.seq_now_bits <- s.seq_now_bits + num;
	s.seq_at <- s.seq_at + num;
	while s.seq_now_bits >= 8 do
		let shift_amount = s.seq_now_bits - 8 in
		let write_mask = 0xFF lsl shift_amount in
		let write_val = (s.seq_now_int(* land write_mask*)) lsr shift_amount in
		put_8_of_int s.seq_ptr s.seq_now_byte write_val;
		s.seq_now_int <- s.seq_now_int land (lnot write_mask);
		s.seq_now_bits <- s.seq_now_bits - 8;
		s.seq_now_byte <- succ s.seq_now_byte;
	done;
(*
	put_bits s.seq_ptr s.seq_at num v;
	s.seq_at <- s.seq_at + num;
*)
;;
let finalize_seq s =
	let ret_at = s.seq_at in
	while s.seq_now_bits > 0 do
		put_seq s 1 0;
	done;
	(s.seq_ptr, ret_at)
;;

(********)
(* MMAP *)
(********)
(* Now supports start / length arguments! *)
type map_access_t = Map_cow | Map_read_only | Map_write;;
external map_handle : Unix.file_descr -> int -> int -> map_access_t -> t = "ptr_map_handle";;
external flush_map : t -> bool = "ptr_flush_map";;
external unmap : t -> unit = "ptr_unmap";;

(*
type shared_memory_handle_t;;
external create_shared_memory : string -> int -> shared_memory_handle_t option = "ptr_create_shared_memory";;
*)

(************************)
(* UNIXY FILE FUNCTIONS *)
(************************)
external read_unsafe : Unix.file_descr -> t -> int -> int -> bool -> int -> int = "ptr_read_bytecode" "ptr_read";;
let read fh ?(pos=(-1)) ptr off len =
	if off < 0 || len < 0 || off + len > length ptr then (
		invalid_arg "Ptr.read"
	) else (
		read_unsafe fh ptr off len false pos
	)
;;
let really_read fh ?(pos=(-1)) ptr off len =
	if off < 0 || len < 0 || off + len > length ptr then (
		invalid_arg "Ptr.really_read"
	) else (
		ignore (read_unsafe fh ptr off len true pos)
	)
;;

external write_unsafe : Unix.file_descr -> t -> int -> int -> bool -> int -> int = "ptr_write_bytecode" "ptr_write";;
let write fh ?(pos=(-1)) ptr off len =
	if off < 0 || len < 0 || off + len > length ptr then (
		invalid_arg "Ptr.write"
	) else (
		write_unsafe fh ptr off len false pos
	)
;;
let really_write fh ?(pos=(-1)) ptr off len =
	if off < 0 || len < 0 || off + len > length ptr then (
		invalid_arg "Ptr.really_write"
	) else (
		ignore (write_unsafe fh ptr off len true pos)
	)
;;


(************)
(* PRINTING *)
(************)
let to_HEX =
	let x = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'A';'B';'C';'D';'E';'F'|] in
	fun p -> (
		let s = String.create (length p * 2) in
		for pi = 0 to length p - 1 do
			let c = get_int_of_8u p pi in
			s.[2 * pi + 0] <- x.(c lsr 4);
			s.[2 * pi + 1] <- x.(c land 0xF);
		done;
		s
	)
;;

module Ref =
	struct

		type node_t = {
			p : t;
			off : int;
			len : int;
		};;

		type ref_t = {
			lentot : int;
			n : node_t list;
		};;

		let print r =
			Printf.printf "Length %d\n" r.lentot;
			List.iter (fun {p = p; off = o; len = l} -> Printf.printf "  %d+%d of %d (%016X)\n" o l (length p) (Obj.magic p)) r.n;
		;;

		let mknode p o l = {p = p; off = o; len = l};;

(*		let mknew () = {lentot = 0; n = []};;*)
		let null = {lentot = 0; n = []};;



		let rec list_append_elt a = function
			| [] -> a :: []
			| hd :: tl -> hd :: list_append_elt a tl
		;;

		(* This has an integrated ptr-fuser so that if the two ptrs at the boundary are sequential it will combine them into one *)
		(* It's just as non-tail-recursive as the "@" function *)
		let rec list_append_list x y = match (x,y) with
			| (x_hd :: [], y_hd :: y_tl) when x_hd.p == y_hd.p && x_hd.off + x_hd.len = y_hd.off -> {p = x_hd.p; off = x_hd.off; len = x_hd.len + y_hd.len} :: y_tl
			| (x_hd :: x_tl, _) -> x_hd :: list_append_list x_tl y
			| ([], _) -> y
		;;



		let append r1 r2 = {
			lentot = r1.lentot + r2.lentot;
			n = list_append_list r1.n r2.n;
		};;
		let append_list rs =
			let rec flatten = function
				| {n = n} :: tl -> list_append_list n (flatten tl)
				| [] -> []
			in
			{
				lentot = List.fold_left (fun so_far {lentot = l} -> so_far + l) 0 rs;
				n = flatten rs;
			}
		;;

		let mksingle p o l = {lentot = l; n = mknode p o l :: []};;

		let compare r1 r2 =
			if r1.lentot <> r2.lentot then (
				compare r1.lentot r2.lentot
			) else (
				let rec iter_byte i1 i2 = function
					| ({len = l1} :: tl1, h2) when i1 >= l1 -> iter_byte (i1 - l1) i2 (tl1, h2)
					| (h1, {len = l2} :: tl2) when i2 >= l2 -> iter_byte i1 (i2 - l2) (h1, tl2)
					| ({p = p1; off = o1} :: _, {p = p2; off = o2} :: _) as tup -> (
						let a = get_int_of_8u p1 (o1 + i1) in
						let b = get_int_of_8u p2 (o2 + i2) in
						if a = b then (
							iter_byte (succ i1) (succ i2) tup
						) else (
							compare a b
						)
					)
					| ([], []) -> 0
					| _ -> failwith "Ptr.Ref.compare failure"(* This shouldn't happen *)
				in
				iter_byte 0 0 (r1.n,r2.n)
			)
		;;

		let of_subptr_unsafe p o l =
			if l = 0 then (
				null
			) else (
				mksingle p o l
			)
		;;
		let of_subptr p o l =
			if l = 0 then (
				null
			) else if o < 0 || l < 0 || o + l > length p then (
				invalid_arg "Ptr.Ref.of_subptr";
			) else (
				of_subptr_unsafe p o l
			)
		;;

		let append_subptr r p o l =
			if l = 0 then (
				r
			) else if o < 0 || l < 0 || o + l > length p then (
				invalid_arg "Ptr.Ref.append_ptr";
			) else (
				let rec append_elt = function
					| [] -> mknode p o l :: []
					| hd :: tl -> hd :: append_elt tl
				in
				{
					lentot = r.lentot + l;
					n = append_elt r.n;
				}
			)
		;;
		let append_ptr r p =
			let plen = length p in
			if plen = 0 then (
				r
			) else (
				let rec append_elt = function
					| [] -> mknode p 0 plen :: []
					| hd :: tl -> hd :: append_elt tl
				in
				{
					lentot = r.lentot + plen;
					n = append_elt r.n
				}
			)
		;;

		(* Removes the first o bytes of r and returns the rest *)
		let sub_last_unsafe r o =
			let rec keep_subbing o_now = function
				| rest when o_now = 0 -> rest
				| {p = from_p; off = from_ptr_off; len = from_len} :: tl when o_now < from_len -> (
					let use_off = from_ptr_off + o_now in
					let use_len = from_len - o_now in
					mknode from_p use_off use_len :: tl
				)
				| {len = from_len} :: tl -> keep_subbing (o_now - from_len) tl
				| [] -> [] (* Hmm... *)
			in
			{
				lentot = r.lentot - o;
				n = keep_subbing o r.n;
			}
		;;
		let sub_last r o =
			if o < 0 || o > r.lentot then (
				invalid_arg "Ptr.Ref.sub_last";
			) else (
				sub_last_unsafe r o
			)
		;;

		let sub_unsafe r o l =
			let rec keep_subbing o_now l_now = function
				| _ when l_now <= 0 -> []
				| ({len = from_len} as hd) :: tl when o_now = 0 && l_now >= from_len -> (
					hd :: keep_subbing 0 (l_now - from_len) tl
				)
				| {p = from_p; off = from_ptr_off; len = from_len} :: tl when o_now < from_len -> (
					let use_off = from_ptr_off + o_now in
					let use_len = min (from_len - o_now) l_now in
					mknode from_p use_off use_len :: keep_subbing 0 (l_now - use_len) tl
				)
				| {len = from_len} :: tl -> keep_subbing (o_now - from_len) l_now tl
				| [] -> [] (* Huh? *)
			in
			{
				lentot = l;
				n = keep_subbing o l r.n
			}
		;;
		let sub r o l =
			if l = 0 then (
				null
			) else if o < 0 || l < 0 || o + l > r.lentot then (
				invalid_arg "Ptr.Ref.sub";
			) else if o + l = r.lentot then (
				sub_last_unsafe r o
			) else (
				sub_unsafe r o l
			)
		;;


		(* Appends part of r2 to the end of r1 *)
		let append_sub r1 r2 o l = append r1 (sub r2 o l);;

		let of_ptr p = of_subptr_unsafe p 0 (length p);;

		(* This function (Ptr.Ref.of_string) calls Ptr.of_string... it looks a bit odd *)
		let of_string s = of_ptr (of_string s);;

		let of_substr s o l = of_string (String.sub s o l);;

		(* BLIT *)
		let blit_to_ptr_unsafe r ro p po l =
			let rec blit_list_to_ptr from_off_left to_off_left len_left n =
(*				Printf.printf " loop from_off=%d to_off=%d len=%d\n" from_off_left to_off_left len_left;*)
				match n with
				| _ when len_left <= 0 -> (
(*					Printf.printf "  done\n";*)
				)
				| [] -> (
					(* Off the end. Ignore here - this is the unsafe version *)
(*					Printf.printf "  huh?\n";*)
				)
				| {p = from_p; off = from_ptr_off; len = from_len} :: tl when from_off_left < from_len -> (
					(* Need to use part of this ptr *)
					let from_off = from_ptr_off + from_off_left in
					let use_len = min (from_len - from_off_left) len_left in
(*					Printf.printf "  blit bytes %d+%d from this ptr (ref %d+%d)\n" from_off use_len from_ptr_off from_len;*)
					blit_unsafe from_p from_off p to_off_left use_len;
					blit_list_to_ptr 0 (to_off_left + use_len) (len_left - use_len) tl
				)
				| {len = from_len} :: tl -> blit_list_to_ptr (from_off_left - from_len) to_off_left len_left tl
			in
			blit_list_to_ptr ro po l r.n
		;;
		let blit_to_ptr r ro p po l =
			if l = 0 then () else if ro < 0 || po < 0 || ro + l > r.lentot || po + l > length p then (
				invalid_arg "Ptr.Ref.blit_to_ptr";
			) else (
				blit_to_ptr_unsafe r ro p po l
			)
		;;
		let to_ptr r =
			let p = make r.lentot 0 in
			blit_to_ptr_unsafe r 0 p 0 r.lentot;
			p
		;;
		(* This uses Ptr.to_string and Ptr.Ref.to_ptr *)
		(* It is rather inefficient... *)
		let to_string r = to_string (to_ptr r);;

		let really_write_unsafe fh ?(pos=(-1)) r o l =
			(* Uses pos *)
			let rec keep_writing_at o_now l_now pos_now = function
				| _ when l_now <= 0 -> ()
				| [] -> () (* Huh? *)
				| {p = from_p; off = from_ptr_off; len = from_len} :: tl when o_now < from_len -> (
					let use_off = from_ptr_off + o_now in
					let use_len = min (from_len - o_now) l_now in
					ignore (write_unsafe fh from_p use_off use_len true pos_now);
					keep_writing_at 0 (l_now - use_len) (pos_now + use_len)	tl
				)
				| {len = from_len} :: tl -> keep_writing_at (o_now - from_len) l_now pos_now tl
			in
			(* No matter what is added to min_int, the pos will always be < 0 since min_int + max_int = -1 *)
			keep_writing_at o l (if pos < 0 then min_int else pos) r.n
		;;
		let really_write fh ?(pos=(-1)) r o l =
			if l = 0 then (
				()
			) else if o < 0 || l < 0 || o + l > r.lentot then (
				invalid_arg "Ptr.Ref.really_write";
			) else (
				really_write_unsafe fh ~pos:pos r o l
			)
		;;
		let really_write_ref fh ?(pos=(-1)) r = really_write_unsafe fh ~pos:pos r 0 r.lentot;;

		let get_bits_unsafe r o l =
			let start_byte = o lsr 3 in
			let rec keep_going num_so_far bits_left start_byte_now start_bit_now = function
				| _ when bits_left = 0 -> ((*Printf.printf "GOT %d\n%!" num_so_far;*) num_so_far)
				| {p = from_p; off = from_ptr_off; len = from_len} :: tl when start_byte_now < from_len -> (
					let max_bits = (from_len lsl 3) - start_bit_now in
					let read_bits = min max_bits bits_left in
(*					Printf.printf "Shifting %d left by %d\n%!" num_so_far read_bits;*)
(*					if read_bits <> l then Printf.printf "*" else Printf.printf "-";*)
					let next = (num_so_far lsl read_bits) lor (get_bits ~byte_off:from_ptr_off from_p (((*from_ptr_off*)0 lsl 3) + start_bit_now) read_bits) in
					keep_going next (bits_left - read_bits) 0 0 tl
				)
				| {len = from_len} :: tl -> keep_going num_so_far bits_left (start_byte_now - from_len) (start_bit_now - (from_len lsl 3)) tl
				| [] -> (num_so_far lsl bits_left) (* Let's make this overflow with 0s at the end of everything *)
			in
			keep_going 0 l start_byte o r.n
		;;
		let get_bits r o l =
			if l = 0 then (
				0
			) else if o < 0 || l < 0 || o + l > r.lentot lsl 3 then (
				invalid_arg "Ptr.Ref.get_bits";
			) else (
				get_bits_unsafe r o l
			)
		;;
		let get_bits_overflow r o l =
			if l = 0 then (
				0
			) else if o < 0 || l < 0 then (
				(* The only difference between this and get_bits is that get_bits fails if we read past the end of the ref *)
				invalid_arg "Ptr.Ref.get_bits_overflow";
			) else (
				(* get_bits_unsafe packs 0s in the end if we go over *)
				get_bits_unsafe r o l
			)
		;;

		(* Sequential bit reading *)
		(* I'll consider this a "transparent" type - programs can set seq_at themselves if needed *)
		(* Actually, it gets a lot less transparent when get_seq_fast is used... *)
		type seq_read_t = {
			seq_ref : ref_t;
			mutable seq_at : int;
			mutable seq_get_fast_int : int;
			mutable seq_get_fast_next_byte : int;
		};;
		let new_seq r = {seq_ref = r; seq_at = 0; seq_get_fast_int = 0; seq_get_fast_next_byte = 0};;
		let get_seq s num =
			let ret = get_bits s.seq_ref s.seq_at num in
			s.seq_at <- s.seq_at + num;
			ret
		;;
		let set_seq =
			fun r p -> (
				let next_byte = p lsr 3 in
(*				Printf.printf "Setting from bit %d to %d, and byte %d to %d\n" r.seq_at p r.seq_get_fast_next_byte next_byte;*)
				if p = r.seq_at then (
					(* Not doing anything; give up here *)
				) else (
					(* We can't re-use the data in get_fast_int since the data may not be any good *)
					r.seq_at <- p;
					r.seq_get_fast_int <- 0;
					r.seq_get_fast_next_byte <- next_byte;
				)
			)
		;;
		let get_seq_overflow s num =
			let ret = get_bits_overflow s.seq_ref s.seq_at num in
			s.seq_at <- s.seq_at + num;
			ret
		;;

		(* It looks like it will be difficult to load 32 bits at a time with a 32-bit compile - just do a char-wise one *)
		let ref_get_byte r o =
			let rec get_byte_list off_left = function
(*				| _ when off_left < 0 -> invalid_arg "Ptr.Ref.ref_get_byte"*)
				| {p = from_p; off = from_ptr_off; len = from_len} :: _ when off_left < from_len -> (
					(* Use this ptr *)
					(* We can use unsafe if the ref has been made with the functions here, rather than directly *)
					get_int_of_8u_unsafe from_p (from_ptr_off + off_left)
				)
				| {len = from_len} :: tl -> get_byte_list (off_left - from_len) tl
				| [] -> 0
			in
			if o >= 0 then (
				get_byte_list o r.n
			) else (
				invalid_arg "Ptr.Ref.ref_get_byte";
			)
		;;
		(* This ensures that at least num bits are available from the fast_int (and it overflows) *)
		let seq_fill s num =
			let end_byte = (s.seq_at + num - 1) asr 3 in
			let rec get_enough_bytes next_int next_next_byte =
				if end_byte >= next_next_byte then (
					let hi = next_int lsl 8 in
					let lo = ref_get_byte s.seq_ref next_next_byte in
					get_enough_bytes (hi lor lo) (succ next_next_byte)
				) else (
					s.seq_get_fast_int <- next_int;
					s.seq_get_fast_next_byte <- next_next_byte;
				)
			in
			get_enough_bytes s.seq_get_fast_int s.seq_get_fast_next_byte;
		;;
		let seq_add s num = s.seq_at <- s.seq_at + num;;
		let get_seq_fast_overflow s num =
			seq_fill s num;
			let end_bit = (s.seq_get_fast_next_byte lsl 3) - (s.seq_at + num) in
			seq_add s num;
			(s.seq_get_fast_int lsr end_bit) land ((1 lsl num) - 1)
		;;
		let get_seq_fast s num =
			let end_byte = (s.seq_at + num - 1) asr 3 in
			if end_byte >= s.seq_ref.lentot then invalid_arg "Ptr.Ref.get_seq_fast";
			get_seq_fast_overflow s num
		;;

		(* This iterates once for each ptr *)
		let iter f initial r =
			let rec iter so_far byte_now = function
				| [] -> so_far
				| {p = p; off = off; len = len} :: tl -> iter (f byte_now so_far p off len) (byte_now + len) tl
			in
			iter initial 0 r.n
		;;

		let to_HEX =
			let x = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'A';'B';'C';'D';'E';'F'|] in
			fun r -> (
				let s = String.create (r.lentot * 2) in
				let rec keep_listing outpos = function
					| [] -> ()
					| {p = from_p; off = from_off; len = from_len} :: tl -> (
						for off_add = 0 to from_len - 1 do
							let c = get_byte from_p (from_off + off_add) in
							s.[outpos + 2 * off_add + 0] <- x.(c lsr 4);
							s.[outpos + 2 * off_add + 1] <- x.(c land 0xF);
						done;
						keep_listing (outpos + 2 * from_len) tl
					)
				in
				keep_listing 0 r.n;
				s
			)
		;;

		let length r = r.lentot;;

	end
;;
