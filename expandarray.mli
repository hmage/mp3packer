type 'a t
val create : int -> 'a t
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val add : 'a t -> 'a -> unit
val length : 'a t -> int
val to_array : 'a t -> 'a array
val of_array : 'a array -> 'a t
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
