type 'a t = {
	mutable expand : 'a array;
	mutable len : int;
};;

let create n = {
	expand = Array.make n (Obj.magic None); (* BREAKING THE LAW! *)
	len = 0;
};;

let get a n =
	if n >= a.len || n < 0 then (
		raise (Invalid_argument "index out of bounds")
	) else (
		a.expand.(n)
	)
;;

let set a n x =
	if n >= a.len then (
		if n >= Array.length a.expand then (
			(* Need to expand the array *)
			let rec find_new_len q = if q > n then q else find_new_len (q lsl 1 lor 1) in
			let q = find_new_len (Array.length a.expand) in
			let newarray = Array.make q (Obj.magic None) in
			Array.blit a.expand 0 newarray 0 (Array.length a.expand);
			a.expand <- newarray;
		);
		for i = a.len to n do
			a.expand.(i) <- x;
		done;
		a.len <- n + 1;
	) else (
		a.expand.(n) <- x;
	)
;;

let add a x =
	set a a.len x
;;

let length a = a.len;;

let to_array a = Array.sub a.expand 0 a.len;;

let of_array a = {
	expand = Array.copy a;
	len = Array.length a;
};;

let iter f a = (
	let rec reciter i = (
		if i >= a.len then () else (
			f a.expand.(i);
			reciter (succ i)
		)
	) in
	reciter 0
);;

let iteri f a = (
	let rec reciter i = (
		if i >= a.len then () else (
			f i a.expand.(i);
			reciter (succ i)
		)
	) in
	reciter 0
);;
