(* Lab 7 solution set. *)

(* ----------------------------------------------------------------------
 * Utility types, functions and values.
 * ---------------------------------------------------------------------- *)

type light = On | Off

let size = 5  (* board size: 5x5 *)

let valid_loc (row, col) =
  let in_range n = n >= 0 && n < size in
    in_range row && in_range col

let newline () = Printf.printf "\n"

(* ----------------------------------------------------------------------
 * Board module type.
 * ---------------------------------------------------------------------- *)

type loc = int * int

module type BOARD =
  sig
    type t
    exception Off_board

    val make : loc list -> t
    val get : t -> loc -> bool
    val flip : t -> loc -> t
    val is_solved : t -> bool
  end

(* ----------------------------------------------------------------------
 * Board representation 1: an array of arrays of booleans.
 * ---------------------------------------------------------------------- *)

module ArrayBoard : BOARD =
  struct
    type t = bool array array
    exception Off_board

    let get b (row, col) =
      if valid_loc (row, col)
      then b.(row).(col)
      else raise Off_board

    let flip b (row, col) =
      if valid_loc (row, col)
      then (
        let new_array = Array.make_matrix size size false in
        for i = 0 to (size - 1) do
            new_array.(i) <- b.(i)
        done;
        if new_array.(row).(col)
        then (new_array.(row).(col) <- false;
              new_array )
        else (new_array.(row).(col) <- true;
              new_array )
        )
      else raise Off_board

    let make locs =
      let rec iter lst b =
        match lst with
        | [] -> b
        | (row, col)::tl when valid_loc (row, col) ->
          b.(row).(col) <- true;
          iter tl b
        | _ -> raise Off_board
      in
      let arr = Array.make_matrix size size false
      in
      iter locs arr

    let is_solved b =
      let s = ref true in
      let helper b s =
        try
          for i = 0 to (size - 1) do
            for j = 0 to (size - 1) do
              if b.(i).(j)
              then raise Exit
              else s := true
            done
          done
        with Exit -> s := false
      in
      helper b s;
      !s
  end

(* ----------------------------------------------------------------------
 * Board representation 2: a set of occupied locations.
 * ---------------------------------------------------------------------- *)

module LocM : Set.OrderedType with type t = loc =
  struct
    type t = loc
    let compare = Pervasives.compare
  end

module LocSet = Set.Make(LocM)

module SetBoard : BOARD =
  struct
    type t = LocSet.t
    exception Off_board

    let get b loc =
      if valid_loc loc
      then
        (if LocSet.mem loc b
        then true
        else false )
      else raise Off_board

    let flip b loc =
      if valid_loc loc
      then
        (if LocSet.mem loc b
        then LocSet.remove loc b
        else LocSet.add loc b)
      else raise Off_board

    let make locs =
      let rec iter lst b =
        match lst with
        | [] -> b
        | loc::tl when valid_loc loc -> iter tl (LocSet.add loc b)
        | _ -> raise Off_board
      in
      iter locs LocSet.empty

    let is_solved b = LocSet.is_empty b
  end

(* ----------------------------------------------------------------------
 * Game object.
 * ---------------------------------------------------------------------- *)

module type GAME =
  sig
    type t

    val play : t -> loc -> t
    val play_many : t -> loc list -> t
    val from_array : light array array -> t
  end

module Game (Board : BOARD) : GAME with type t = Board.t =
  struct
    type t = Board.t
    exception Failure

    let play b loc =
      let (row, col) = loc in
      let lst = ref [loc;(row - 1,col);(row + 1,col);(row,col - 1);(row,col + 1)] in
      let rec iter lst b =
        match lst with
        | [] -> b
        | h::tl -> iter tl (Board.flip b h)
      in
      begin
        lst := List.filter valid_loc !lst;
        iter !lst b
      end

    let play_many b locs =
      let rec iter lst b =
        match lst with
        | [] -> b
        | h::tl -> iter tl (play b h)
      in
      iter locs b

    (* Initialize a board given an array of arrays of on/off values
       by setting the corresponding lights to those values. *)
    let from_array arr =
      let lst = ref [] in
      if ((Array.length arr = size) &&
          (Array.for_all (fun x -> Array.length x = size) arr))
      then (
        for i = 0 to (size - 1) do
          for j = 0 to (size - 1) do
            match arr.(i).(j) with
            | On -> lst := (i, j)::(!lst)
            | Off -> lst := !lst
          done;
        done;
          Board.make !lst
      )
      else failwith "invalid dimension size"



  end

(* ----------------------------------------------------------------------
 * Playing the game interactively.
 * ---------------------------------------------------------------------- *)

module type INTERACT =
  sig
    type t
    type game_input = Quit | Coords of loc

    val is_digit : char -> bool
    val ok_coords_line : string -> bool
    val get_input : unit -> game_input
    val print : t -> unit
    val run : t -> unit
    val play : light array array -> unit
  end

(* Supplied to students. *)
module Interact (Board : BOARD) : INTERACT with type t = Board. t =
  struct
    (* local module *)
    module G = Game(Board)

    type t = Board.t
    type game_input = Quit | Coords of loc

    let is_digit c = c >= '0' && c <= '9'

    let ok_coords_line line =
      String.length line = 3
        && is_digit line.[0]
        && line.[1] = ' '
        && is_digit line.[2]

    let get_input () =
      let line = read_line () in
        match line with
          | "quit" -> Quit
          | _ when ok_coords_line line ->
            let row = int_of_string (Printf.sprintf "%c" line.[0]) in
            let col = int_of_string (Printf.sprintf "%c" line.[2]) in
            let loc = (row, col) in
              if valid_loc loc
                then Coords loc
                else failwith "invalid coordinates"
          | _ -> failwith "invalid input line"

    let print b =
      begin
        Printf.printf "    ";
        for col = 0 to size - 1 do
          Printf.printf "%d " col
        done;
        Printf.printf "\n   -----------\n";
        for row = 0 to size - 1 do
          Printf.printf "%d | " row;
          for col = 0 to size - 1 do
            Printf.printf "%c "
              (if (Board.get b (row, col)) then 'O' else '.')
          done;
          Printf.printf "|\n"
        done;
        Printf.printf "   -----------\n\n";
      end

    let rec run b =
      try
        begin
          Printf.printf "Enter move (row col): ";
          match get_input () with
            | Quit -> ()
            | Coords (row, col) ->
                let b' = G.play b (row, col) in
                begin
                  newline ();
                  print b';
                  if Board.is_solved b'
                    then Printf.printf "You win!\n\n"
                    else run b'
                end
        end
      with Failure msg -> (Printf.printf "ERROR: %s\n\n" msg; run b)

    let play init =
      let game = G.from_array init in
        begin
          newline ();
          print game;
          run game
        end
  end

(* ----------------------------------------------------------------------
 * Sample initial boards.
 * ---------------------------------------------------------------------- *)

module PlayA = Interact(ArrayBoard)
module PlayS = Interact(SetBoard)

let init1 =
  [|
     [| On;  On;  On;  Off; Off |];
     [| Off; Off; Off; On;  Off |];
     [| On;  On;  Off; On;  On  |];
     [| Off; Off; Off; On;  Off |];
     [| On;  On;  On;  Off; Off |]
  |]

let test_array_board () = PlayA.play init1
let test_set_board ()   = PlayS.play init1
