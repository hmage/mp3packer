(*
open Types;;

(* These should be defined elsewhere *)
let num_threads = detected_processors;;
*)




(****

type 'a wait_internal_t =
	| Waiting
	| Done of 'a
;;
type 'a wait_t = {
	mutable w : 'a wait_internal_t;
};;

type slot_internal_t =
	| Full
	| Processed
;;
type slot_t = {
	sw : unit -> unit;
	sc : bool ref;
};;



let list_mutex = Mutex.create ();;
let list_length_ref = ref 0;;
let list_ref = ref [];;
let list_signal_if_added = Condition.create ();;


(* This clears out all the things where !sc is true *)
let clean_list_in_mutex () =
(*	Printf.printf "%s%!" (Printf.sprintf "Cleaner starts with %d things\n" !list_length_ref);*)
	let rec keep_rebuilding = function
		| hd :: tl when !(hd.sc) -> (
			decr list_length_ref;
			keep_rebuilding tl
		)
		| hd :: tl -> hd :: keep_rebuilding tl
		| [] -> []
	in
	list_ref := keep_rebuilding !list_ref;
(*	Printf.printf "%s%!" (Printf.sprintf "Cleaner ends with %d things\n" !list_length_ref);*)
;;



let (thread_pool, first_worker_thread_id, last_worker_thread_id) =
	let first_thread_id_ref = ref max_int in
	let last_thread_id_ref = ref min_int in
	let num_threads_counted_ref = ref 0 in
	let thread_id_mutex = Mutex.create () in

	let thread_guts : int -> unit = fun thread_i ->

		(* The thread IDs are not guaranteed to be consecutive, but if they aren't it won't be a problem *)
		Mutex.lock thread_id_mutex;
		first_thread_id_ref := min !first_thread_id_ref (Thread.id (Thread.self ()));
		last_thread_id_ref := max !last_thread_id_ref (Thread.id (Thread.self ()));
		incr num_threads_counted_ref;
		Mutex.unlock thread_id_mutex;
		Thread.yield ();

		let rec loop () =

			Mutex.lock list_mutex;
(*			Printf.printf "%s%!" (Printf.sprintf "Loop sees %d things to be done\n" !list_length_ref);*)
			let rec keep_waiting () = match !list_ref with
				| hd :: tl when !(hd.sc) -> (
					(* Check really fast to see if this is done without unlocking the mutex *)
					list_ref := tl;
					decr list_length_ref;
					keep_waiting ()
				)
				| hd :: [] -> (list_ref := []; decr list_length_ref; hd)
				| hd :: tl -> (list_ref := tl; decr list_length_ref; Condition.signal list_signal_if_added; hd)
				| [] -> (Condition.wait list_signal_if_added list_mutex; keep_waiting ())
			in
			let got = keep_waiting () in
			Mutex.unlock list_mutex;

(*			Printf.printf "%s%!" (Printf.sprintf "Thread %d doing job\n" (Thread.id (Thread.self ())));*)
			got.sw ();

			loop ()
		in
		loop ()
	in
	let thread_pool = Array.init num_threads (Thread.create thread_guts) in

	(* Wait for all the threads to initialize *)
	let rec keep_waiting () =
		Mutex.lock thread_id_mutex;
		let keep_going = (!num_threads_counted_ref < num_threads) in
		Mutex.unlock thread_id_mutex;
		if keep_going then (
			Thread.yield ();
			keep_waiting ()
		)
	in
	keep_waiting ();

	Printf.printf "The threads are from %d to %d\n%!" !first_thread_id_ref !last_thread_id_ref;
	(thread_pool, !first_thread_id_ref, !last_thread_id_ref)
;;

let thread_might_be_worker () =
(*
	let id = Thread.id (Thread.self ()) in
	id >= first_worker_thread_id && id <= last_worker_thread_id
*)
	false
;;



let wrap f x w =
	let m = Mutex.create () in
	let c = Condition.create () in
	let done_ref = ref false in

	let start_processing () =
		match w.w with
		| Done _ -> () (* Already run; skip this *)
		| Waiting -> (
			if Mutex.try_lock m then (
				(match w.w with
					| Done _ -> ()
					| Waiting -> (
						(* Do it! *)
						w.w <- Done (f x);
						done_ref := true;
						Condition.broadcast c;
					)
				);
				Mutex.unlock m;
				()
			) else (
				(* Something else has the lock; it must be processing *)
				()
			)
		)
	in
	let wait_for_processing () = match w.w with
		| Done out -> out
		| Waiting -> (

			if thread_might_be_worker () then (
				(* Since this may be a worker, it shouldn't block on just this work item *)
				(* Instead try to find something else in the list *)
				let rec keep_trying_lock () =
					if Mutex.try_lock m then (
						let ret = match w.w with
							| Done out -> out
							| Waiting -> (
								let ret = f x in
								w.w <- Done ret;
								done_ref := true;
								Condition.broadcast c;
								ret
							)
						in
						Mutex.unlock m;
						ret
					) else (
						(* We can't get the lock but this MAY be a worker; check to see if there's something else to do in the meantime *)
						Mutex.lock list_mutex;
(*						Printf.printf "%s%!" (Printf.sprintf "Stealing sees %d things to be done\n" !list_length_ref);*)
						clean_list_in_mutex ();
						let do_something = match !list_ref with
							| hd :: [] -> (list_ref := []; decr list_length_ref; Some hd)
							| hd :: tl -> (list_ref := tl; decr list_length_ref; Condition.signal list_signal_if_added; Some hd)
							| [] -> None
						in
						Mutex.unlock list_mutex;

						(match do_something with
							| Some got -> (
(*								Printf.printf "%s%!" (Printf.sprintf "Thread %d doing a side job\n" (Thread.id (Thread.self ())));*)
								got.sw ();
								keep_trying_lock ()
							)
							| None -> (
								(* Just lock it *)
								Mutex.lock m;
(*								Printf.printf "%s%!" (Printf.sprintf "Thread %d found nothing to do; finishing job\n" (Thread.id (Thread.self ())));*)
								let ret = match w.w with
									| Done out -> out
									| Waiting -> (
										let ret = f x in
										w.w <- Done ret;
										done_ref := true;
										Condition.broadcast c;
										ret
									)
								in
								Mutex.unlock m;
								ret
							)
						)
					)
				in
				keep_trying_lock ()
			) else (
				(* The thread is not a worker and we just need to wait on the mutex *)
(*				Printf.printf "%s%!" (Printf.sprintf "Non-worker thread %d; block in wait_for_processing\n" (Thread.id (Thread.self ())));*)
				Mutex.lock m;
				let ret = match w.w with
					| Done out -> out
					| Waiting -> (
(*						Printf.printf "%s%!" (Printf.sprintf "Thread %d stealing\n" (Thread.id (Thread.self ())));*)
						let ret = f x in
						w.w <- Done ret;
						done_ref := true;
						Condition.broadcast c;
						ret
					)
				in
				Mutex.unlock m;
				ret
			)
		)
	in
	(done_ref, start_processing, wait_for_processing)
;;

let send_unsafe f x =
	let w = {w = Waiting} in
	let (done_ref, start_processing, wait_for_processing) = wrap f x w in
	let slot = {sc = done_ref; sw = start_processing} in

	Mutex.lock list_mutex;
	let signal = match !list_ref with
		| [] -> true
		| _ -> false
	in
	list_ref := slot :: !list_ref;
	incr list_length_ref;
	if signal then Condition.signal list_signal_if_added;

	(* DELETEME *)
	if List.length !list_ref <> !list_length_ref then Printf.printf "%s%!" (Printf.sprintf "%d <> %d!\n" (List.length !list_ref) !list_length_ref);

	Mutex.unlock list_mutex;
(*	Thread.yield ();*)

	wait_for_processing
;;

let send f x =
	let f2 x = try
		Normal (f x)
	with
		e -> Error e
	in
	send_unsafe f2 x
;;

****)

















type ('a,'b) obj_slot_t = {
	slot_parameters : 'a;
	slot_mutex : Mutex.t;
	mutable slot_ret : 'b option;
};;

let slot_is_done = function
	| {slot_ret = None} -> false
	| _ -> true
;;

class ['a,'b] per_function (f : 'a -> 'b) max_threads =
	object

		method f = f
		method max_threads = max_threads

		val l_mutex = Mutex.create ()
		val l_signal_if_added = Condition.create ()
		val mutable l = List2.create ()

		val mutable thread_pool = [||]

		method send params = (
			let slot = {
				slot_parameters = params;
				slot_mutex = Mutex.create ();
				slot_ret = None;
			} in

			Mutex.lock l_mutex;
			let signal = List2.is_empty l in
			List2.prepend l slot;
			if signal then Condition.broadcast l_signal_if_added;
			Mutex.unlock l_mutex;
			Thread.yield ();

			slot
		)

		method send_last params = (
			let slot = {
				slot_parameters = params;
				slot_mutex = Mutex.create ();
				slot_ret = None;
			} in

			Mutex.lock l_mutex;
			let signal = List2.is_empty l in
			List2.append l slot;
			if signal then Condition.broadcast l_signal_if_added;
			Mutex.unlock l_mutex;
			Thread.yield ();

			slot
		)

		method recv slot = (
			(* It would be sort of rare for this to be recursive, so don't try to check out anything else *)
			Mutex.lock slot.slot_mutex;
			let ret = match slot.slot_ret with
				| Some x -> x
				| None -> (
(*					Printf.printf "Calculate it ourselves...\n%!";*)
					let ret = f slot.slot_parameters in
					slot.slot_ret <- Some ret;
					ret
				)
			in
			Mutex.unlock slot.slot_mutex;

			ret
		)

		(* If actual processing is not needed *)
		val bypass_mutex = Mutex.create ()
		method bypass (input : 'a) (output : 'b) = (
			{
				slot_parameters = input;
				slot_mutex = bypass_mutex;
				slot_ret = Some output;
			}
		)

		initializer (
			(* Initialize the threads *)
			let rec l_keep_waiting () = match List2.take_first_perhaps l with
				| Some hd -> (
					if slot_is_done hd then (
						l_keep_waiting ()
					) else (
						if List2.is_empty l then Condition.signal l_signal_if_added;
						hd
					)
				)
				| None -> (
					Condition.wait l_signal_if_added l_mutex;
					l_keep_waiting ()
				)
			in
			let thread_guts : int -> unit = fun _ ->
				let rec loop () =
(*					Printf.printf "%s%!" (Printf.sprintf "Thread %d looping\n" (Thread.id (Thread.self ())));*)

					Mutex.lock l_mutex;
					let got = l_keep_waiting () in
					Mutex.unlock l_mutex;

					if Mutex.try_lock got.slot_mutex then (
(*						Printf.printf "%s%!" (Printf.sprintf "Thread %d got something\n" (Thread.id (Thread.self ())));*)
						(match got.slot_ret with
							| None -> got.slot_ret <- Some (f got.slot_parameters);
							| Some _ -> ()
						);
						Mutex.unlock got.slot_mutex;
						()
					); (* Otherwise just ignore it *)

					loop ()
				in
				loop ()
			in
			thread_pool <- Array.init max_threads (Thread.create thread_guts)
		)
	end
;;









