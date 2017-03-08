(* lab7.mli *)

type light = On | Off
type loc = int * int

val size : int
val valid_loc : int * int -> bool
val newline : unit -> unit

module type BOARD =
  sig
    type t
    exception Off_board
    val make : loc list -> t
    val get : t -> loc -> bool
    val flip : t -> loc -> t
    val is_solved : t -> bool
  end

module ArrayBoard : BOARD

module LocM : sig type t = loc val compare : t -> t -> int end

module LocSet : Set.S

module SetBoard : BOARD

module type GAME =
  sig
    type t
    
    val play : t -> loc -> t
    val play_many : t -> loc list -> t
    val from_array : light array array -> t
  end

module Game : functor (Board : BOARD) -> GAME with type t = Board.t

val test_array_board : unit -> unit
val test_set_board : unit -> unit

