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

let to_hex s =
  let result = String.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    String.blit (Printf.sprintf "%02X" (int_of_char s.[i])) 0 result (2*i) 2;
  done;
  result;;

(******************************************************************************)
(****************************** PACK AND UNPACK *******************************)
(******************************************************************************)
(* pack *)
let packc inNum =
	let returnThis = "0" in
	if inNum < 0 then (
		returnThis.[0] <- Char.chr ((inNum + 256) land 0xFF)
	) else (
		returnThis.[0] <- Char.chr (inNum land 0xFF)
	);
	returnThis;;

let packC inNum =
	let returnThis = "0" in
	returnThis.[0] <- Char.chr (inNum land 0xFF);
	returnThis;;

let packn inNum =
	let returnThis = "01" in
	returnThis.[0] <- Char.chr ((inNum land 0xFF00) lsr 8);
	returnThis.[1] <- Char.chr (inNum land 0x00FF);
	returnThis;;

let packN inNum =
	let returnThis = "0123" in
	returnThis.[0] <- Char.chr ((inNum land 0x7F000000) lsr 24);
	returnThis.[1] <- Char.chr ((inNum land 0x00FF0000) lsr 16);
	returnThis.[2] <- Char.chr ((inNum land 0x0000FF00) lsr 8);
	returnThis.[3] <- Char.chr (inNum land 0x000000FF);
	returnThis;;

let packN32 inNum =
	let returnThis = "0123" in
	returnThis.[0] <- Char.chr (Int32.to_int (Int32.shift_right_logical (Int32.logand inNum 0xFF000000l) 24));
	returnThis.[1] <- Char.chr (Int32.to_int (Int32.shift_right_logical (Int32.logand inNum 0x00FF0000l) 16));
	returnThis.[2] <- Char.chr (Int32.to_int (Int32.shift_right_logical (Int32.logand inNum 0x0000FF00l) 8));
	returnThis.[3] <- Char.chr (Int32.to_int (Int32.shift_right_logical (Int32.logand inNum 0x000000FFl) 0));
	returnThis;;

let packv inNum =
	let returnThis = "01" in
	returnThis.[1] <- Char.chr ((inNum land 0xFF00) lsr 8);
	returnThis.[0] <- Char.chr (inNum land 0x00FF);
	returnThis;;

let packV inNum =
	let returnThis = "0123" in
	returnThis.[3] <- Char.chr ((inNum land 0x7F000000) lsr 24);
	returnThis.[2] <- Char.chr ((inNum land 0x00FF0000) lsr 16);
	returnThis.[1] <- Char.chr ((inNum land 0x0000FF00) lsr 8);
	returnThis.[0] <- Char.chr (inNum land 0x000000FF);
	returnThis;;

let packV32 inNum =
	let returnThis = "0123" in
	returnThis.[3] <- Char.chr (Int32.to_int (Int32.shift_right_logical (Int32.logand inNum 0xFF000000l) 24));
	returnThis.[2] <- Char.chr (Int32.to_int (Int32.shift_right_logical (Int32.logand inNum 0x00FF0000l) 16));
	returnThis.[1] <- Char.chr (Int32.to_int (Int32.shift_right_logical (Int32.logand inNum 0x0000FF00l) 8));
	returnThis.[0] <- Char.chr (Int32.to_int (Int32.shift_right_logical (Int32.logand inNum 0x000000FFl) 0));
	returnThis;;


(* unpack *)
let unpackc inString offset =
	let raw = Char.code inString.[offset] in
	if raw >= 128 then (
		raw - 256
	) else (
		raw
	);;

let unpackC inString offset =
	Char.code inString.[offset];;

let unpackn inString offset =
	((Char.code inString.[offset]) lsl 8) lor
	(Char.code inString.[offset + 1]);;

let unpackN inString offset =
	((Char.code inString.[offset]) lsl 24) lor
	((Char.code inString.[offset + 1]) lsl 16) lor
	((Char.code inString.[offset + 2]) lsl 8) lor
	(Char.code inString.[offset + 3]);;

let unpackN32 inString offset =
	Int32.logor
		(Int32.logor
			(Int32.shift_left (Int32.of_int (Char.code inString.[offset])) 24)
			(Int32.shift_left (Int32.of_int (Char.code inString.[offset + 1])) 16)
		)
		(Int32.logor
			(Int32.shift_left (Int32.of_int (Char.code inString.[offset + 2])) 8)
			(Int32.of_int (Char.code inString.[offset + 3]))
		);;

let unpackv inString offset =
	((Char.code inString.[offset + 1]) lsl 8) lor
	(Char.code inString.[offset]);;

let unpackV inString offset =
	((Char.code inString.[offset + 3]) lsl 24) lor
	((Char.code inString.[offset + 2]) lsl 16) lor
	((Char.code inString.[offset + 1]) lsl 8) lor
	(Char.code inString.[offset]);;

let unpackV32 inString offset =
	Int32.logor
		(Int32.logor
			(Int32.shift_left (Int32.of_int (Char.code inString.[offset + 3])) 24)
			(Int32.shift_left (Int32.of_int (Char.code inString.[offset + 2])) 16)
		)
		(Int32.logor
			(Int32.shift_left (Int32.of_int (Char.code inString.[offset + 1])) 8)
			(Int32.of_int (Char.code inString.[offset]))
		);;

(* Unpacks a bunch of bits, not necessarily aligned to byte boundaries *)
let unpackBits inString offset num =
	let startByte = offset lsr 3 in
(*	let startBit = offset mod 8 in*)
	let endByte = (offset + num - 1) lsr 3 in
	let endBit = (offset + num - 1) mod 8 in
	if num > 30 then raise (Failure "Number of bits to return from unpackBits must be 30 or fewer");
(*	Printf.printf " Num %d\n SB  %d\n Sb  %d\n EB  %d\n Eb  %d\n" num startByte startBit endByte endBit;*)

	let (msbMask, lsbMask, msbRight, lsbLeft) = [|
		(0b10000000, 0b01111111, 7, 1);
		(0b11000000, 0b00111111, 6, 2);
		(0b11100000, 0b00011111, 5, 3);
		(0b11110000, 0b00001111, 4, 4);
		(0b11111000, 0b00000111, 3, 5);
		(0b11111100, 0b00000011, 2, 6);
		(0b11111110, 0b00000001, 1, 7);
		(0b11111111, 0b00000000, 0, 8);
	|].(endBit) in
	let tempArray = Array.make (endByte - startByte + 1) 0 in
	for byteNow = startByte to endByte - 1 do
		let value = Char.code inString.[byteNow] in
		let msb = (value land msbMask) lsr msbRight in
		let lsb = (value land lsbMask) lsl lsbLeft in
		tempArray.(byteNow - startByte) <- tempArray.(byteNow - startByte) lor msb;
		tempArray.(byteNow - startByte + 1) <- lsb;
	done;
	(* Last byte *)
	(
		let value = Char.code inString.[endByte] in
		let msb = (value land msbMask) lsr msbRight in
		tempArray.(endByte - startByte) <- tempArray.(endByte - startByte) lor msb;
	);
(*
	Array.iter (fun x -> Printf.printf "%d " x) tempArray;
	Printf.printf "\n";
*)
	(* Stuff them all together *)
	let outThis = Array.fold_left (fun soFar newOne -> (soFar lsl 8) lor newOne) 0 tempArray in

(*	Printf.printf "%d (%d)\n" outThis (outThis land (1 lsl num - 1));*)
	(outThis land (1 lsl num - 1))
;;

let packBits str offset num store =
	(* Just do it bitwise because I don't want to bother with bytewise *)
(*
	let startByte = offset lsr 3 in
	let startBit = offset land 7 in
	let endByte = (offset + num - 1) lsr 3 in
	let endBit = (offset + num - 1) land 7 in
*)
	if num > 30 then raise (Failure "Number of bits to save to packBits must be 30 or fewer");
(*	Printf.printf "(%d,%d)\n(%d,%d)\n" startByte startBit endByte endBit;*)
	for i = 0 to num - 1 do
		let byte = (offset + i) lsr 3 in
		let bit  = (offset + i) land 7 in
		let mask = 1 lsl (num - i - 1) in
		let outMask = 1 lsl (7 - bit) in
(*		Printf.printf " (%d,%d) = %9d %d (%d) %d!\n" byte bit mask (if store land mask = 0 then 0 else 1) outMask ((if store land mask = 0 then 0 else 255) land outMask);*)

		let orig = (Char.code str.[byte]) in
		let cleared = orig land (255 lxor outMask) in
		let set = ((if store land mask = 0 then 0 else 255) land outMask) in
		let gnu = cleared lor set in
(*		Printf.printf " %02X %02X %02X %02X\n" orig cleared set gnu;*)

		str.[byte] <- Char.chr gnu;

(*		str.[byte] <- Char.chr ((Char.code str.[byte]) land ((if store land mask = 0 then 0 else 255) land outMask));*)
(*		Printf.printf "  %S\n" (to_hex str);*)
	done;
	()
;;

(*
This version is from 1.16 and before. The new version optimizes reads for whole bytes, which makes it ~4% faster than this
let unpackBitsOverflow inString offset num =
	if num > 30 then raise (Failure "Number of bits to save to unpackBitsOverflow must be 30 or fewer");
	let str_len_m1 = String.length inString - 1 in
	let rec add_bit so_far current_offset num_left = (
		if num_left = 0 then (
			so_far
		) else (
			let byte = current_offset lsr 3 in
			if byte > str_len_m1 then (
				so_far lsl num_left
			) else (
				let code = Char.code inString.[byte] in
				let bit = 7 - current_offset land 7 in
				let add_me = (code land (1 lsl bit)) lsr bit in
				add_bit ((so_far lsl 1) lor add_me) (succ current_offset) (pred num_left)
			)
		)
	) in
	add_bit 0 offset num
;;
*)

(* Unpacks a bunch of bits, not necessarily aligned to byte boundaries. Overflowing the string will return as though it was padded with "0" bits *)
(* Faster than the <1.16 safe version by about 32%! *)
let unpackBitsOverflowUnsafe inString offset num =
	let str_len_m1 = String.length inString - 1 in
	let rec add_bit so_far current_offset num_left = (
		if num_left = 0 then (
			so_far
		) else (
			let byte = current_offset lsr 3 in
			if byte > str_len_m1 then (
				(* Overflow! *)
				so_far lsl num_left
			) else (
				let code = Char.code inString.[byte] in
				if current_offset land 7 = 0 then (
					(* Byte boundary *)
					if num_left = 8 then (
						(so_far lsl num_left) lor code
					) else if num_left < 8 then (
						(* Shift right *)
						(so_far lsl num_left) lor (code lsr (8 - num_left))
					) else (
						(* Shift left *)
						add_bit ((so_far lsl 8) lor code) (current_offset + 8) (num_left - 8)
					)
				) else (
					(* Actually do stuff *)
					let bit = 7 - current_offset land 7 in
					let add_me = (code land (1 lsl bit)) lsr bit in
					add_bit ((so_far lsl 1) lor add_me) (succ current_offset) (pred num_left)
				)
			)
		)
	) in
	add_bit 0 offset num
;;

let unpackBitsOverflow inString offset num = (
	if num > 30 then raise (Failure "Number of bits to save to unpackBitsOverflow must be 30 or fewer");
	unpackBitsOverflowUnsafe inString offset num
);;


(* A simple, kind-of-imperative method for keeping track of which bit we're on *)
let read_bits (str, on_bits) num_bits =
(*	if debug then Printf.printf "Reading %d bits from %d on %S\n" num_bits on_bits (to_bin str);*)
	try
		if num_bits = 0 then (
			(0, (str, on_bits))
		) else (
			((unpackBits str on_bits num_bits), (str, on_bits + num_bits))
		)
	with
		_ -> raise (Failure "read_bits")
;;

let write_bits (str, on_bits) num_bits store =
(*	if debug then Printf.printf "Reading %d bits from %d on %S\n" num_bits on_bits (to_bin str);*)
	if num_bits = 0 then (
		(str, on_bits)
	) else (
		packBits str on_bits num_bits store;
		(str, on_bits + num_bits)
	)
;;

(* A simple, kind-of-imperative method for keeping track of which bit we're on *)
let read_bits_overflow_unsafe (str, on_bits) num_bits =
(*	if debug then Printf.printf "Reading %d bits from %d on %S\n" num_bits on_bits (to_bin str);*)
	if num_bits = 0 then (
		(0, (str, on_bits))
	) else (
		((unpackBitsOverflowUnsafe str on_bits num_bits), (str, on_bits + num_bits))
	)
;;
let read_bits_overflow (str, on_bits) num_bits =
(*	if debug then Printf.printf "Reading %d bits from %d on %S\n" num_bits on_bits (to_bin str);*)
	if num_bits = 0 then (
		(0, (str, on_bits))
	) else (
		((unpackBitsOverflow str on_bits num_bits), (str, on_bits + num_bits))
	)
;;
