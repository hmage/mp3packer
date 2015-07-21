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

exception Empty;;

type 'a node = {
	value : 'a;
	mutable next : 'a node option;
	mutable prev : 'a node option;
};;

type 'a t = {
	mutable length : int;
	mutable head : 'a node;
	mutable tail : 'a node
};;



let create () = {
  length = 0;
  head = Obj.magic None; (* Hooray! Breaking the type system! *)
  tail = Obj.magic None
}

let clear q =
	q.length <- 0;
	q.head <- Obj.magic None;
	q.tail <- Obj.magic None;
;;

let length q = q.length;;
let is_empty q = (q.length = 0);;

let append q x =
	q.length <- succ q.length;
	if q.length = 1 then (
		let a = {
			value = x;
			next = None;
			prev = None
		} in
		q.head <- a;
		q.tail <- a;
	) else (
		let old_tail = q.tail in
		let a = {
			value = x;
			next = None;
			prev = Some old_tail
		} in
		old_tail.next <- Some a;
		q.tail <- a;
	)
;;

let prepend q x =
	q.length <- succ q.length;
	if q.length = 1 then (
		let a = {
			value = x;
			next = None;
			prev = None
		} in
		q.head <- a;
		q.tail <- a;
	) else (
		let old_head = q.head in
		let a = {
			value = x;
			next = Some old_head;
			prev = None
		} in
		old_head.prev <- Some a;
		q.head <- a;
	)
;;

let iter f q =
	if q.length = 0 then () else (
		let rec iter node =
			f node.value;
			match node.next with
			| None -> ()
			| Some x -> iter x
		in
		iter q.head
	)
;;

let rev_iter f q =
	if q.length = 0 then () else (
		let rec iter node =
			f node.value;
			match node.prev with
			| None -> ()
			| Some x -> iter x
		in
		iter q.tail
	)
;;

let iteri f q =
	if q.length = 0 then () else (
		let rec iter i node =
			f i node.value;
			match node.next with
			| None -> ()
			| Some x -> iter (succ i) x
		in
		iter 0 q.head
	)
;;

let rev_iteri f q =
	if q.length = 0 then () else (
		let rec iter i node =
			f i node.value;
			match node.prev with
			| None -> ()
			| Some x -> iter (pred i) x
		in
		iter (q.length - 1) q.tail
	)
;;

let pop_last q =
	if q.length = 0 then raise Empty;
	let old_tail = q.tail in
	match old_tail.prev with
	| None -> ( (* It's the only element in the list *)
		clear q;
		old_tail.value
	)
	| Some new_tail -> (
		new_tail.next <- None;
		old_tail.prev <- None; (* I don't think this is necessary, but I'll do it anyway *)
		q.tail <- new_tail;
		q.length <- pred q.length;
		old_tail.value
	)
;;
let take_last = pop_last;;

(* I don't like exceptions! *)
let pop_last_perhaps q =
	if q.length = 0 then (
		None
	) else (
		let old_tail = q.tail in
		match old_tail.prev with
		| None -> ( (* It's the only element in the list *)
			clear q;
			Some old_tail.value
		)
		| Some new_tail -> (
			new_tail.next <- None;
			old_tail.prev <- None; (* I don't think this is necessary, but I'll do it anyway *)
			q.tail <- new_tail;
			q.length <- pred q.length;
			Some old_tail.value
		)
	)
;;
let take_last_perhaps = pop_last_perhaps;;


let pop_first q =
	if q.length = 0 then raise Empty;
	let old_head = q.head in
	match old_head.next with
	| None -> (
		clear q;
		old_head.value
	)
	| Some new_head -> (
		new_head.prev <- None;
		old_head.next <- None;
		q.head <- new_head;
		q.length <- pred q.length;
		old_head.value
	)
;;
let take_first = pop_first;;


let pop_first_perhaps q =
	if q.length = 0 then (
		None
	) else (
		let old_head = q.head in
		match old_head.next with
		| None -> (
			clear q;
			Some old_head.value
		)
		| Some new_head -> (
			new_head.prev <- None;
			old_head.next <- None;
			q.head <- new_head;
			q.length <- pred q.length;
			Some old_head.value
		)
	)
;;
let take_first_perhaps = pop_first_perhaps;;


let fold f accu q =
	if q.length = 0 then accu else (
		let rec fold accu node =
			let accu = f accu node.value in
			match node.next with
			| None -> accu
			| Some x -> fold accu x
		in
		fold accu q.head
	)
;;
let rev_fold f accu q =
	if q.length = 0 then accu else (
		let rec fold accu node =
			let accu = f accu node.value in
			match node.prev with
			| None -> accu
			| Some x -> fold accu x
		in
		fold accu q.tail
	)
;;

let nth q n =
	if q.length = 0 then raise Empty;
	let n = (if n >= 0 then n else q.length + n) in (* Make "nth q ~-1" point to the last element *)
	if n >= q.length then raise (Invalid_argument "index out of bounds");
	let rec test i node =
		if i = n then node.value else (
			match node.next with
			| None -> raise (Invalid_argument "index out of bounds")
			| Some x -> test (succ i) x
		)
	in
	test 0 q.head
;;

let peek_first q =
	if q.length = 0 then raise Empty;
	q.head.value
;;
let head = peek_first;;
let peek_first_perhaps q =
	if q.length = 0 then (
		None
	) else (
		Some q.head.value
	)
;;
let head_perhaps = peek_first_perhaps;;

let peek_last q =
	if q.length = 0 then raise Empty;
	q.tail.value
;;
let tail = peek_last;;
let peek_last_perhaps q =
	if q.length = 0 then (
		None
	) else (
		Some q.tail.value
	)
;;
let tail_perhaps = peek_last_perhaps;;

(* To other kinds of data structures *)
let of_array a =
	let q = create () in
	Array.iter (fun x -> append q x) a;
	q
;;

let to_array q =
	if q.length = 0 then [||] else (
		let a = Array.make q.length (q.head.value) in
		iteri (fun i v -> a.(i) <- v) q;
		a
	)
;;

let of_list l =
	let q = create () in
	List.iter (fun x -> append q x) l;
	q
;;

let to_list q =
	if q.length = 0 then [] else (
		let rec make_list node list_tail =
			match node.prev with
			| None -> node.value :: list_tail
			| Some x -> make_list x (node.value :: list_tail)
		in
		make_list q.tail []
	)
;;
