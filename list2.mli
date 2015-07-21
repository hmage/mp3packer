exception Empty
(** Raised when a function like {!pop_last} is used on an empty list *)

type 'a t
(** The type of doubly-linked lists *)

val create : unit -> 'a t
(** Creates an empty doubly-linked list *)

val clear : 'a t -> unit
(** Discard all elements from a list *)

val length : 'a t -> int
(** The number of elements of a list *)

val is_empty : 'a t -> bool
(** [true] if the list is empty, otherwise [false] *)

val append : 'a t -> 'a -> unit
(** [append q x] adds [x] to the end of list [q] *)

val prepend : 'a t -> 'a -> unit
(** [prepend q x] adds [x] to the beginning of list [q] *)

val iter : ('a -> 'b) -> 'a t -> unit
(** [iter f q] applies [f] to all elements of [q], from beginning to end *)

val rev_iter : ('a -> 'b) -> 'a t -> unit
(** [iter f q] applies [f] to all elements of [q] backwards, from end to beginning *)

val iteri : (int -> 'a -> 'b) -> 'a t -> unit
val rev_iteri : (int -> 'a -> 'b) -> 'a t -> unit
(** Same as {!iter} and {!rev_iter}, respectively, but the first argument is the index.
	Note that the index of rev_iteri counts down *)

val pop_last : 'a t -> 'a
val take_last : 'a t -> 'a
(** Take the last element out of the list and return it *)
val pop_last_perhaps : 'a t -> 'a option
val take_last_perhaps : 'a t -> 'a option
(** Take the last element out of the list and return it, or return None *)

val pop_first : 'a t -> 'a
val take_first : 'a t -> 'a
(** Take the first element out of the list and return it *)
val pop_first_perhaps : 'a t -> 'a option
val take_first_perhaps : 'a t -> 'a option
(** Take the first element out of the list and return it, or return None *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold f i q] is the same as [(f ... (f (f i first) second) ... last)] *)

val rev_fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [rev_fold f i q] is the same as [(f ... (f (f i last) second_last) ... first)] *)

val nth : 'a t -> int -> 'a
(** [nth q n] returns the [n]th element of [q]. O([length q]) *)

val peek_first : 'a t -> 'a
val head : 'a t -> 'a
(** Returns the first element of the list without changing the contonts *)
val peek_first_perhaps : 'a t -> 'a option
val head_perhaps : 'a t -> 'a option
(** Returns the first element of the list or None if empty *)

val peek_last : 'a t -> 'a
val tail : 'a t -> 'a
(** Returns the last element of the list without changing the contonts *)
val peek_last_perhaps : 'a t -> 'a option
val tail_perhaps : 'a t -> 'a option
(** Returns the last element of the list or None if empty *)

val of_array : 'a array -> 'a t
val to_array : 'a t -> 'a array
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
(** Converts between doubly-linked lists and standard data types *)
