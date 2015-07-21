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

open List2;;

let q = create ();;
append q 1;;
prepend q 2;;
append q 3;;

rev_iter (fun x -> Printf.printf "%d\n" x) q;;
(*
let a = pop_last q;;
Printf.printf ">%d\n" a;;
*)

iter (fun x -> Printf.printf "%d\n" x) q;;

Printf.printf "+%d+\n" (fold (fun a b -> Printf.printf "(%d,%d)" a b; a + b) 0 q);;
Printf.printf "+%d+\n" (rev_fold (fun a b -> Printf.printf "(%d,%d)" a b; a + b) 0 q);;

Printf.printf " #1=%d\n" (nth q 2);;

let a = to_array q;;

prepend q 666;;
ignore (take_first q);;
prepend q 666;;
ignore (take_first q);;
prepend q 666;;
ignore (take_first q);;
prepend q 666;;
ignore (take_first q);;
prepend q 666;;
ignore (take_first q);;
prepend q 666;;
ignore (take_first q);;
prepend q 666;;
ignore (take_first q);;
prepend q 666;;
ignore (take_first q);;
prepend q 666;;
ignore (take_first q);;
prepend q 666;;
ignore (take_first q);;
prepend q 666;;
ignore (take_first q);;
prepend q 666;;
ignore (take_first q);;

iter (fun x -> Printf.printf "%d\n" x) q;;

iteri (fun i x -> Printf.printf "%d: %d\n" i x) q;;
rev_iteri (fun i x -> Printf.printf "%d: %d\n" i x) q;;


ignore (take_first q);;
ignore (take_first q);;


Array.iteri (fun i x -> Printf.printf "A %d=%d\n" i x) a;;

let r = of_array a;;
Printf.printf "+%d+\n" (fold (fun a b -> Printf.printf "(%d,%d)" a b; a + b) 0 r);;
Printf.printf "+%d+\n" (rev_fold (fun a b -> Printf.printf "(%d,%d)" a b; a + b) 0 r);;


let l = to_list r;;
let s = of_list l;;
Printf.printf "+%d+\n" (fold (fun a b -> Printf.printf "(%d,%d)" a b; a + b) 0 s);;
Printf.printf "+%d+\n" (rev_fold (fun a b -> Printf.printf "(%d,%d)" a b; a + b) 0 s);;
Printf.printf " List length %d\n" (List.length l);;
