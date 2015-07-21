let pm = Mutex.create ();;
let time_ref = ref 0;;
let print_freq_next = ref true;;
let last_replace_line_length_ref = ref 0;;

(* NEW TEST STUFF *)
let next_index_ref = ref 0;;
type p_id = {
	p_handle : out_channel;
	p_header : string;
	p_buffer : Buffer.t;
	p_time_buffer : Buffer.t
};;

let new_id ?(channel=stdout) n =
	Mutex.lock pm;
	let got = !next_index_ref in
	incr next_index_ref;
	Mutex.unlock pm;

	let whitespace_str = String.make (2 * got) ' ' in
	{
		p_handle = channel;
		p_header = " " ^ whitespace_str ^ n (*^ " "*);
		p_buffer = Buffer.create 100;
		p_time_buffer = Buffer.create 26;
	}
;;

let print_spaces b total_len =
	for i = 1 to total_len do
		Buffer.add_char b ' '
	done
;;

let hex = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'A';'B';'C';'D';'E';'F'|];;
let add_subptr b p o l =
	for i = o to o + l - 1 do
		let v = Ptr.get_int_of_8 p i in
		Buffer.add_char b hex.((v lsr 4) land 0xF);
		Buffer.add_char b hex.(v land 0xF);
	done
;;
let add_ptrref b p =
	Ptr.Ref.iter (fun _ _ p o l ->
		Buffer.add_char b '[';
		add_subptr b p o l;
		Buffer.add_char b ']';
	) () p
;;

let add_hex b len x =
	for i = len - 1 downto 0 do
		let n = (x asr (4 * i)) land 0xF in
		Buffer.add_char b hex.(n)
	done
;;

(*
let rec pos_int_len_1 = function
	| x when x < 10 -> 1
	| x when x < 100 -> 2
	| x when x < 1000 -> 3
	| x when x < 10000 -> 4
	| x when x < 100000 -> 5
	| x when x < 1000000 -> 6
	| x when x < 10000000 -> 7
	| x when x < 100000000 -> 8
	| x when x < 1000000000 -> 9
	| x -> 9 + pos_int_len_1 (x / 1000000000)
;;
let rec int_len_1 = function
	| x when x = min_int -> 1 + 1 + pos_int_len_1 (~-(x / 10))
	| x when x < 0 -> 1 + pos_int_len_1 ~-x
	| x -> pos_int_len_1 x
;;
*)
let int_len =
	let rec pos_int_len = function
		| x when x < 10 -> 1
		| x when x < 100 -> 2
		| x when x < 1000 -> 3
		| x when x < 10000 -> 4
		| x when x < 100000 -> 5
		| x when x < 1000000 -> 6
		| x when x < 10000000 -> 7
		| x when x < 100000000 -> 8
		| x when x < 1000000000 -> 9
		| x -> 9 + pos_int_len (x / 1000000000)
	in
	function
	| x when x = min_int -> 1 + 1 + pos_int_len (~-(x / 10))
	| x when x < 0 -> 1 + pos_int_len ~-x
	| x -> pos_int_len x
;;
(*
let rec print_int_zeroes b total_len v =
	if v < 0 then (
		Buffer.add_char b '-';
		print_int_zeroes b (pred total_len) ~-v
	) else (
		let v_len = int_len v in
		for i = 1 to total_len - v_len do
			Buffer.add_char b '0'
		done;
		Buffer.add_string b (string_of_int v)
	)
;;
let print_int_spaces b total_len v =
	let v_len = int_len v in
	if total_len < 0 then (
		Buffer.add_string b (string_of_int v);
		print_spaces b (~-total_len - v_len);
	) else (
		print_spaces b (total_len - v_len);
		Buffer.add_string b (string_of_int v)
	)
;;
*)
(* 1_234_567 *)

(* This is a test of printing numbers without using printf *)
let print_int b v =
	let do_ref = ref false in (* Force printing of all following places - everything that gets printed sets this *)

	let rec do_to_100g y =
		if y >= 100_000 then (
			let div_me = y  /  100_000 in
			do_to_100g div_me;
			do_to_100g (y - (div_me * 100_000));
		) else (
(*			let y = if !do_ref || y >= 100_000 then (let q = y / 100_000 in Buffer.add_char b (Char.chr (q + 48)); do_ref := true; y - (q * 100_000)) else y in*)
			let y = if !do_ref || y >=  10_000 then (let q = y /  10_000 in Buffer.add_char b (Char.chr (q + 48)); do_ref := true; y - (q *  10_000)) else y in
			let y = if !do_ref || y >=   1_000 then (let q = y /   1_000 in Buffer.add_char b (Char.chr (q + 48)); do_ref := true; y - (q *   1_000)) else y in
			let y = if !do_ref || y >=     100 then (let q = y /     100 in Buffer.add_char b (Char.chr (q + 48)); do_ref := true; y - (q *     100)) else y in
			let y = if !do_ref || y >=      10 then (let q = y /      10 in Buffer.add_char b (Char.chr (q + 48)); do_ref := true; y - (q *      10)) else y in
			                                                                Buffer.add_char b (Char.chr (y + 48)); do_ref := true;
		)
	in

	if v < 0 then (
		Buffer.add_char b '-';
		if v = min_int then (
			(* Do something different since ~-min_int = min_int *)
			let div_me = v  /  100_000 in
			do_to_100g ~-div_me;
			let mod_me = v - (div_me * 100_000) in
			do_to_100g ~-mod_me;
		) else (
			do_to_100g ~-v
		)
	) else (
		do_to_100g v
	)
;;

let rec print_int_zeroes b total_len v =
	if v < 0 then (
		Buffer.add_char b '-';
		if v = min_int then (
			let div_me = v / 10 in
			print_int b ~-div_me;
			let mod_me = v - (div_me * 10) in
			print_int b ~-mod_me
		) else (
			print_int_zeroes b (pred total_len) ~-v
		)
	) else (
		let v_len = int_len v in
		for i = v_len to total_len - 1 do
			Buffer.add_char b '0'
		done;
		print_int b v
	)
;;

let print_int_spaces b total_len v =
	let v_len = int_len v in
	if total_len < 0 then (
		print_int b v;
		print_spaces b (~-total_len - v_len);
	) else (
		print_spaces b (total_len - v_len);
		print_int b v
	)
;;

let print_string_spaces b total_len s =
	let s_len = String.length s in
	if total_len < 0 then (
		Buffer.add_string b s;
		print_spaces b (~-total_len - s_len);
	) else (
		print_spaces b (total_len - s_len);
		Buffer.add_string b s
	)
;;

let rec add_one b = function
	| Types.Bool x -> Buffer.add_string b (if x then "true" else "false")
	| Types.Int x -> print_int b x
	| Types.IntN (len, x) -> print_int_spaces b len x
	| Types.Int0N (len, x) -> print_int_zeroes b len x
(*	| Types.Int64 x -> Printf.bprintf b "%Ld" x*)
	| Types.Hex (len, x) -> add_hex b len x
	| Types.Float x -> Buffer.add_string b (string_of_float x)
	| Types.FloatN (len, x) -> Printf.bprintf b "%*f" len x
	| Types.Float_N (prec, x) -> Printf.bprintf b "%.*f" prec x
	| Types.FloatN_N (len, prec, x) -> Printf.bprintf b "%*.*f" len prec x
	| Types.Str x -> Buffer.add_string b x
	| Types.StrN (len, x) -> print_string_spaces b len x
	| Types.StrS x -> Buffer.add_string b (String.escaped x)
	| Types.StrNS (len, x) -> print_string_spaces b len (String.escaped x)
	| Types.Char x -> Buffer.add_char b x
	| Types.Spaces len -> print_spaces b len
	| Types.Ptr p -> add_subptr b p 0 (Ptr.length p)
	| Types.Subptr (p,o,l) -> add_subptr b p o l
	| Types.Ptrref p -> add_ptrref b p
	| Types.Fun f -> f (add_one b)
	| Types.List l -> add_list b l
and add_list b = List.iter (add_one b);;

(*
let print_debug id plist =
	Buffer.clear id.p_buffer;
	Buffer.clear id.p_time_buffer;
	Buffer.add_string id.p_buffer id.p_header;
	add_list id.p_buffer plist;
	Buffer.add_char id.p_buffer '\n';

	Mutex.lock pm;

	let new_time = Types.counter () in
	let time_diff = if !print_freq_next then (
		print_freq_next := false;
		output_string id.p_handle "__PRINT_COUNTER_FREQUENCY__ ";
		output_string id.p_handle (string_of_int Types.counter_freq);
		output_char id.p_handle '\n';
		0
	) else (
		new_time - !time_ref
	) in
	time_ref := new_time;

	if !last_replace_line_length_ref > 0 then (
		Buffer.add_char id.p_time_buffer '\n';
		last_replace_line_length_ref := 0;
	);

(*	Printf.fprintf id.p_handle "%15d %10d\t" new_time time_diff;*)
	print_int_spaces id.p_time_buffer 15 new_time;
	Buffer.add_char id.p_time_buffer ' ';
	print_int_spaces id.p_time_buffer 10 time_diff;

	Buffer.output_buffer id.p_handle id.p_time_buffer;
	Buffer.output_buffer id.p_handle id.p_buffer;
(*	flush id.p_handle;*)
	Mutex.unlock pm;

;;

let print_always ?(channel=stdout) plist =
	let b = Buffer.create 100 in
	add_list b plist;
(*	Buffer.add_char b '\n';*)

	Mutex.lock pm;

	while Buffer.length b < !last_replace_line_length_ref do
		(* Have to pack it to cover up the last printed thing *)
		Buffer.add_char b ' ';
	done;
	last_replace_line_length_ref := 0;

	Buffer.add_char b '\n';
	Buffer.output_buffer channel b;
	Mutex.unlock pm;

	flush channel;
;;

let print_replace_always ?(channel=stdout) plist =
	let b = Buffer.create 100 in
	add_list b plist;
(*	Buffer.add_char b '\n';*)

	Mutex.lock pm;

	let before_len = Buffer.length b in
	while Buffer.length b < !last_replace_line_length_ref do
		(* Have to pack it to cover up the last printed thing *)
		Buffer.add_char b ' ';
	done;
	last_replace_line_length_ref := before_len;

	Buffer.add_char b '\r';
	Buffer.output_buffer channel b;
	Mutex.unlock pm;

	flush channel;
;;

*)

let thread_buffer_array_ref = ref [||];;
let thread_buffer_array_mutex = Mutex.create ();;
let print_debug id plist =

	(* Select buffer *)
	let tid = Thread.id (Thread.self ()) in
	let (p_buffer, p_time_buffer) =
		if Array.length !thread_buffer_array_ref <= tid then (
			Mutex.lock thread_buffer_array_mutex;
			let old_length = Array.length !thread_buffer_array_ref in
			if old_length <= tid then (
				(* It could have changed... *)
				let gnu = Array.init (tid + 1) (function
					| n when n < old_length -> !thread_buffer_array_ref.(n)
					| _ -> (Buffer.create 100, Buffer.create 26)
				) in
				thread_buffer_array_ref := gnu
			);
			Mutex.unlock thread_buffer_array_mutex;
		);
		!thread_buffer_array_ref.(tid)
	in

	Buffer.clear p_buffer;
	Buffer.clear p_time_buffer;
	Buffer.add_string p_buffer id.p_header;
	Buffer.add_char p_buffer '_';
	Buffer.add_string p_buffer (string_of_int tid);
	Buffer.add_string p_buffer " ";
	add_list p_buffer plist;
	Buffer.add_char p_buffer '\n';

	Mutex.lock pm;

	let new_time = Types.counter () in
	let time_diff = if !print_freq_next then (
		print_freq_next := false;
		output_string id.p_handle "__PRINT_COUNTER_FREQUENCY__ ";
		output_string id.p_handle (string_of_int Types.counter_freq);
		output_char id.p_handle '\n';
		0
	) else (
		new_time - !time_ref
	) in
	time_ref := new_time;

	if !last_replace_line_length_ref > 0 then (
		Buffer.add_char p_time_buffer '\n';
		last_replace_line_length_ref := 0;
	);

(*	Printf.fprintf id.p_handle "%15d %10d\t" new_time time_diff;*)
	print_int_spaces p_time_buffer 15 new_time;
	Buffer.add_char p_time_buffer ' ';
	print_int_spaces p_time_buffer 10 time_diff;

	Buffer.output_buffer id.p_handle p_time_buffer;
	Buffer.output_buffer id.p_handle p_buffer;
	flush id.p_handle;
	Mutex.unlock pm;

;;


let print_always ?(channel=stdout) plist =
	(* Select buffer *)
	let tid = Thread.id (Thread.self ()) in
	let (b, _) =
		if Array.length !thread_buffer_array_ref <= tid then (
			Mutex.lock thread_buffer_array_mutex;
			let old_length = Array.length !thread_buffer_array_ref in
			if old_length <= tid then (
				(* It could have changed... *)
				let gnu = Array.init (tid + 1) (function
					| n when n < old_length -> !thread_buffer_array_ref.(n)
					| _ -> (Buffer.create 100, Buffer.create 26)
				) in
				thread_buffer_array_ref := gnu
			);
			Mutex.unlock thread_buffer_array_mutex;
		);
		!thread_buffer_array_ref.(tid)
	in
	Buffer.clear b;

	add_list b plist;
(*	Buffer.add_char b '\n';*)

	Mutex.lock pm;

	while Buffer.length b < !last_replace_line_length_ref do
		(* Have to pack it to cover up the last printed thing *)
		Buffer.add_char b ' ';
	done;
	last_replace_line_length_ref := 0;

	Buffer.add_char b '\n';
	Buffer.output_buffer channel b;
	Mutex.unlock pm;

	flush channel;
;;

let print_replace_always ?(channel=stdout) plist =
	(* Select buffer *)
	let tid = Thread.id (Thread.self ()) in
	let (b, _) =
		if Array.length !thread_buffer_array_ref <= tid then (
			Mutex.lock thread_buffer_array_mutex;
			let old_length = Array.length !thread_buffer_array_ref in
			if old_length <= tid then (
				(* It could have changed... *)
				let gnu = Array.init (tid + 1) (function
					| n when n < old_length -> !thread_buffer_array_ref.(n)
					| _ -> (Buffer.create 100, Buffer.create 26)
				) in
				thread_buffer_array_ref := gnu
			);
			Mutex.unlock thread_buffer_array_mutex;
		);
		!thread_buffer_array_ref.(tid)
	in
	Buffer.clear b;

	add_list b plist;
(*	Buffer.add_char b '\n';*)

	Mutex.lock pm;

	let before_len = Buffer.length b in
	while Buffer.length b < !last_replace_line_length_ref do
		(* Have to pack it to cover up the last printed thing *)
		Buffer.add_char b ' ';
	done;
	last_replace_line_length_ref := before_len;

	Buffer.add_char b '\r';
	Buffer.output_buffer channel b;
	Mutex.unlock pm;

	flush channel;
;;











(* Home-grown float printing *)
let print_float_numbers =
	let max_int_float = float_of_int max_int in (* Assume min_int = -max_int; it's pretty close and this saves a constant *)
	let char_array = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'9'|] in (* Two '9' elements since something may round up to 10 *)
	let float_array = [|0.0;1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0;9.0;9.0|] in (* Same deal *)
	fun b v -> (
		if v >= ~-.max_int_float && v <= max_int_float then (
			(* We can store the integer part in a regular int *)
(*
			let int_part = int_of_float v in
			let frac_float = abs_float (float_of_int int_part -. v) in
*)
			let (frac_float,int_float) = modf v in
			let int_part = int_of_float int_float in
			print_int b int_part;
			Buffer.add_char b '.';

			(* This isn't exactly accurate, but it should be close enough *)
			let v_frac_ref = ref (abs_float frac_float) in
			Buffer.add_char b '[';
			Buffer.add_string b (Printf.sprintf "%f" !v_frac_ref);
			Buffer.add_char b ']';

			let rec keep_printing left v_frac_now = if left > 0 then (
				let use_this = 10.0 *. v_frac_now in
				let use_int = int_of_float use_this in
				Buffer.add_char b char_array.(use_int);
				keep_printing (pred left) (use_this -. float_array.(use_int))
			) in
			keep_printing 18 (abs_float frac_float)
		) else (
			(* Have to do something weird with Num *)
			Buffer.add_char b '?';
		)
	)
;;




(* TEST *)
(*
let b = Buffer.create 20;;
let test_f f =
	Buffer.clear b;
	print_float_numbers b f;
	Printf.printf "%f -> %s\n" f (Buffer.contents b);
;;
test_f 1.0;;
test_f ~-.2.5;;
test_f 0.0;;
test_f 1000000.0;;
test_f ~-.0.33333333333333333333333333;;
exit 18271;;
(***
let b1 = Buffer.create 20;;
let b2 = Buffer.create 20;;
let check len n =
	Buffer.clear b1;
(*	Buffer.clear b2;*)
	let correct = Printf.sprintf "%*d" len n in
	print_int_spaces b1 len n;
(*	print_int_spaces_new b2 len n;*)
	let s1 = Buffer.contents b1 in
(*	let s2 = Buffer.contents b2 in*)
	if s1 <> correct then (
		failwith (s1 ^ "<>" ^ correct)
	);
(*
	if s2 <> correct then (
		failwith (s2 ^ "<>" ^ correct)
	)
*)
;;

for len = -32 to 32 do
	Printf.printf "Len %d\n%!" len;
	for i = -262144 to 262144 do
		check len i
	done
done;;
Random.self_init ();;
while true do
	for i = 0 to 16777215 do
		let len = Random.int 41 - 20 in
		let v = (Random.bits () lsl 30) lxor (Random.bits ()) in
		if i = 0 then (
			Printf.printf "%d:%d\n%!" len v
		);
		check len v;
	done;
done;;

failwith "EXITED IN P.ML";;
***)
*)
