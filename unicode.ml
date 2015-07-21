type 'a t = Normal of 'a | Error of int;;

external is_win : unit -> bool = "uni_is_win" "noalloc";;
let win = is_win ();;


(***********)
(* UNICODE *)
(***********)
external set_utf8_output : unit -> unit = "uni_set_utf8_output";;

external silly_print_c : string -> unit = "uni_silly_print";;
let silly_print x =
	flush stdout;
	silly_print_c x;
;;

(* This is only good on Windows! *)
(* Unixy systems return Error for everything! *)
external utf16_of_utf8_c : string -> bool -> string t = "uni_utf16_of_utf8";;
let utf16_of_utf8 ?(include_null=false) a = utf16_of_utf8_c a include_null;;

external utf8_of_utf16_unsafe : int -> string -> string t = "uni_utf8_of_utf16";;
let utf8_of_utf16 x = utf8_of_utf16_unsafe (String.length x) x;;

external active_of_utf16_unsafe : int -> string -> string t = "uni_active_of_utf16";;
let active_of_utf16 x = active_of_utf16_unsafe (String.length x) x;;

let active_of_utf8 x8 = match utf16_of_utf8_c x8 false with
	| Normal x16 -> active_of_utf16_unsafe (String.length x16) x16
	| Error e -> Error e
;;

(* Like active_of_utf8, but returns the UTF8 string if an error occured *)
let sprint_utf8 x8 = match active_of_utf8 x8 with
	| Normal n -> n
	| Error _ -> x8
;;

(* the length value is in WCHARS, not bytes! *)
(* Unsafe since the length is not checked *)
external utf8_of_utf16_and_length_unsafe : string -> int -> string t = "uni_utf8_of_utf16_and_length";;
let utf8_of_utf16 s = utf8_of_utf16_and_length_unsafe s (String.length s / 2);;


external get_utf16_command_line : unit -> string t = "uni_get_utf16_command_line";;


external get_utf8_argv_c : unit -> string array t = "uni_get_utf8_argv";;
let argv_opt = if win then (
	get_utf8_argv_c ()
) else (
	Normal Sys.argv
);;


(* UTF8 *)
external openfile_utf16_c : string -> Unix.open_flag list -> Unix.file_perm -> Unix.file_descr = "uni_openfile_utf16";;

let openfile_utf8 = if win then (
	fun s -> (
		match utf16_of_utf8 ~include_null:true s with
		| Normal s16 -> openfile_utf16_c s16
		| Error e -> failwith (Printf.sprintf "Unicode.openfile_utf8 failed with Windows error %d" e)
	)
) else (
	Unix.openfile
);;


(* Input AND output values are zero-terminated UTF16 *)
(*external readdir_utf16_c : string -> string array = "uni_readdir_utf16";;*)
type dir_handle_t;;
external readdir_find_first_file_utf16_unsafe : string -> (dir_handle_t * string) t = "uni_readdir_find_first_file_utf16";;
external readdir_find_next_file_utf16 : dir_handle_t -> string t = "uni_readdir_find_next_file_utf16";;
external readdir_find_close : dir_handle_t -> unit t = "uni_readdir_find_close";;

let readdir_utf8 = if win then (
	fun dir_in -> (
		let read_this = dir_in ^ "\\*" in
		match utf16_of_utf8 ~include_null:true read_this with
		| Normal read_this_16 -> (
			match readdir_find_first_file_utf16_unsafe read_this_16 with
			| Normal (h, first_16) -> (
(*				Printf.printf "Got first 16: %s\n" first_16;*)
				(* This should really be "length - sizeof(WCHAR)", but this will round down correctly *)
				match utf8_of_utf16_unsafe (String.length first_16 - 1) first_16 with
				| Normal first_8 -> (
					let rec keep_reading num so_far =
						match readdir_find_next_file_utf16 h with
						| Normal next_16 -> (
(*							Printf.printf "Got next 16: %s\n" next_16;*)
							match utf8_of_utf16_unsafe (String.length next_16 - 1) next_16 with
							| Normal "." | Normal ".." -> keep_reading num so_far
							| Normal next_8 -> (
								keep_reading (succ num) (next_8 :: so_far)
							)
							| Error e -> Error e
						)
						| Error 18(*ERROR_NO_MORE_FILES*) -> Normal (num, so_far)
						| Error e -> Error e
					in
					let got_list = if first_8 = "." || first_8 = ".." then (
						keep_reading 0 []
					) else (
						keep_reading 1 [first_8]
					) in
					match got_list with
					| Normal (num, got) -> (
						let out_array = Array.make num "" in
						let rec keep_putting n = function
							| [] -> ()
							| hd :: tl -> (
								out_array.(n) <- hd;
								keep_putting (pred n) tl
							)
						in
						keep_putting (pred num) got;
						out_array
					)
					| Error e -> (
						ignore (readdir_find_close h);
						failwith (Printf.sprintf "Unicode.readdir_utf8 got Windows error %d" e)
					)
				)
				| Error e -> (
					ignore (readdir_find_close h);
					failwith (Printf.sprintf "Unicode.readdir_utf8 got Windows error %d" e)
				)
			)
			| Error 2(*ERROR_FILE_NOT_FOUND*) -> [||]
			| Error e -> failwith (Printf.sprintf "Unicode.readdir_utf8 got Windows error %d" e)
		)
		| Error e -> failwith (Printf.sprintf "Unicode.readdir_utf8 got Windows error %d" e)
	)
) else (
	Sys.readdir
);;


(* File info *)
external stat_utf16_unsafe : string -> Unix.stats t = "uni_stat_utf16";;
let stat_utf8 = if win then (
	fun name -> (
		match utf16_of_utf8 ~include_null:true name with
		| Normal name16 -> (
			match stat_utf16_unsafe name16 with
			| Normal s -> s
			| Error _ -> raise Not_found
		)
		| Error _ -> invalid_arg "Unicode.stat_utf8"
	)
) else (
	Unix.stat
);;

external file_exists_utf16_unsafe : string -> bool = "uni_file_exists_utf16";;
let file_exists_utf8 = if win then (
	fun n -> (
		match utf16_of_utf8 ~include_null:true n with
		| Normal wn -> file_exists_utf16_unsafe wn
		| Error e -> failwith (Printf.sprintf "Unicode.file_exists_utf8 got Windows error %d" e)
	)
) else (
	Sys.file_exists
);;

(* Rename *)
external rename_utf16_unsafe : string -> string -> unit t = "uni_rename_utf16";;
let rename_utf8 = if win then (
	fun n1 n2 -> (
		match (utf16_of_utf8 ~include_null:true n1, utf16_of_utf8 ~include_null:true n2) with
		| (Normal wn1, Normal wn2) -> (match rename_utf16_unsafe wn1 wn2 with
			| Normal () -> ()
			| Error e -> failwith (Printf.sprintf "Unicode.rename_utf8 got Windows error %d" e)
		)
		| (Error e, _) -> failwith (Printf.sprintf "Unicode.rename_utf8 got Windows error %d" e)
		| (_, Error e) -> failwith (Printf.sprintf "Unicode.rename_utf8 got Windows error %d" e)
	)
) else (
	Sys.rename
);;

external remove_utf16_unsafe : string -> unit t = "uni_remove_utf16";;
let remove_utf8 = if win then (
	fun f -> (match utf16_of_utf8 ~include_null:true f with
		| Normal f16 -> (
			match remove_utf16_unsafe f16 with
			| Normal () -> ()
			| Error e -> failwith (Printf.sprintf "Unicode.remove_utf8 got Windows error %d" e)
		)
		| Error e -> failwith (Printf.sprintf "Unicode.remove_utf8 got Windows error %d" e)
	)
) else (
	Sys.remove
);;
