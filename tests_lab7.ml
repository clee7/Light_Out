(* Tests for lab7.ml *)

(*
To run this test script from the ocaml toplevel, do this:

$ ocaml
#use "topfind";;  (* may not be necessary *)
#require "oUnit";;
#load "lab7.cmo";;
#use "tests_lab7.ml";;
# run_tests ()  (* assuming this line is commented out below *)
*)

open OUnit2
open Lab7

(*** Utility functions, functors and modules. ***)

let int_of_bool = function
  | false -> 0
  | true -> 1

(* Use a specific random number seed if you need to reproduce results.
 * For instance, if a randomized test fails you might want to use this. *)
(*
let _ = Random.init 12345
*)

(* Use an arbitrary random number seed. *)
let _ = Random.self_init ()

(* Choose a random number of random locations on a 5 x 5 board. *)
let random_locs () =
  let rec iter r c current =
    match (r, c) with
      | (5, _) -> current
      | (i, 5) -> iter (i + 1) 0 current
      | (i, j) ->
          iter i (j + 1)
            (if Random.bool () then (i, j) :: current else current)
  in
    iter 0 0 []

(* Compute a list of locations on a 5 x 5 board. *)
let all_locs =
  let rec iter r c current =
    match (r, c) with
      | (5, _) -> current
      | (i, 5) -> iter (i + 1) 0 current
      | (i, j) -> iter i (j + 1) ((i, j) :: current)
  in
    iter 0 0 []

(* Compute a list of locations on a size x size board that aren't in the
 * argument list. *)
let other_locs locs =
  List.filter (fun l -> not (List.mem l locs)) all_locs

module BoardTest(B : BOARD) =
  struct
    (* Dump a board's contents to the terminal. *)
    let dump_board b =
      let rec iter r c =
        match (r, c) with
          | (5, _) -> ()
          | (i, 5) -> iter (i + 1) 0
          | (i, j) ->
            begin
              Printf.printf
                "Location: (%d,%d); contents: %b\n" i j (B.get b (i, j));
              iter i (j + 1)
            end
      in
        begin
          print_newline ();
          iter 0 0
        end

    (* Get all cells from a board object as a list of lists of ints
     * (0 for not present or 1 if present). *)
    let get_all_cells board =
      let row board r =
        List.map (fun c -> int_of_bool (B.get board (r, c))) [0; 1; 2; 3; 4]
      in
        List.map (row board) [0; 1; 2; 3; 4]

     (* Flip all cells in a board. *)
     let flip_all_cells board =
       let rec iter b r c =
         match (r, c) with
           | (5, _) -> b
           | (i, 5) -> iter b (i + 1) 0
           | (i, j) -> iter (B.flip b (i, j)) i (j + 1)
       in
         iter board 0 0

     (* Compare two boards for equality. *)
     let equal_boards b1 b2 =
       let rec iter r c =
         match (r, c) with
           | (5, _) -> true
           | (i, 5) -> iter (i + 1) 0
           | (i, j) ->
               if B.get b1 (i, j) = B.get b2 (i, j)
                 then iter i (j + 1)
                 else
                   begin
                     Printf.printf "\nERROR: not equal at: (%d,%d)\n" i j;
                     false
                   end
       in
         iter 0 0

     (* Compare two boards to see if all locations have different values. *)
     let all_unequal_boards b1 b2 =
       let rec iter r c =
         match (r, c) with
           | (5, _) -> true
           | (i, 5) -> iter (i + 1) 0
           | (i, j) ->
               if B.get b1 (i, j) <> B.get b2 (i, j)
                 then iter i (j + 1)
                 else
                   begin
                     Printf.printf "\nERROR: equal at: (%d,%d)\n" i j;
                     Printf.printf "b1: %b\tb2: %b\n" (B.get b1 (i, j)) (B.get b2 (i, j));
                     false
                   end
       in
         iter 0 0

     (*
      * The tests themselves.
      *)

     (* An empty board is solved. *)
     let test1a () =
       let empty = B.make [] in
         assert_bool "empty board is solved" (B.is_solved empty)

     (* A non-empty board is not solved. *)
     let test1b () =
       let nonempty1 = B.make [(0, 0)] in
       let nonempty2 = B.make [(0, 4)] in
       let nonempty3 = B.make [(4, 0)] in
       let nonempty4 = B.make [(4, 4)] in
       let nonempty5 = B.make [(2, 2)] in
       let msg i = Printf.sprintf "nonempty board %d is not solved" i in
         begin
           assert_bool (msg 1) (not (B.is_solved nonempty1));
           assert_bool (msg 2) (not (B.is_solved nonempty2));
           assert_bool (msg 3) (not (B.is_solved nonempty3));
           assert_bool (msg 4) (not (B.is_solved nonempty4));
           assert_bool (msg 5) (not (B.is_solved nonempty5))
         end

     (* A board made from a list of locations has those locations on. *)
     let test2 () =
       let locs = random_locs () in
         assert_bool "new board contains correct values"
           (List.for_all (fun l -> B.get (B.make locs) l) locs)

     (* A board made from a list of locations has all other locations off. *)
     let test3 () =
       let locs = random_locs () in
       let olocs = other_locs locs in
         assert_bool "new board doesn't contain incorrect values"
           (List.for_all (fun l -> not (B.get (B.make locs) l)) olocs)

     (* Flipping every square of the board once gives a board where every
      * location has a different value. *)
     let test4a () =
       let locs = random_locs () in
       let b = B.make locs in
       let fb = flip_all_cells b in
       let msg =
         "flipping every square of the board once means all locations are different"
       in
         assert_bool msg (all_unequal_boards b fb)

     (* Flipping every square of the board twice gives the original board back. *)
     let test4b () =
       let locs = random_locs () in
       let b = B.make locs in
       let fb = flip_all_cells b in
       let ffb = flip_all_cells fb in
       let msg = "flipping every square of the board twice leaves it unchanged" in
         assert_bool msg (equal_boards b ffb)

     (* Helper function to detect invalid locations. *)
     let expect_off_board f =
       try
         let _ = f () in false
       with
         B.Off_board -> true

     (* Accessing locations off the board raises an Off_board exception. *)
     let test5a () =
       let locs = random_locs () in
       let b = B.make locs in
       let msg = "accessing invalid locations raises the Off_board exception" in
       let result =
         List.for_all (fun loc -> expect_off_board (fun () -> B.get b loc))
           [(-1, 0); (0, -1); (-1, -1); (4, 5); (5, 4); (5, 5)]
       in
         assert_bool msg result

     (* Flipping locations off the board raises an Off_board exception. *)
     let test5b () =
       let locs = random_locs () in
       let b = B.make locs in
       let msg = "flipping invalid locations raises the Off_board exception" in
       let result =
         List.for_all (fun loc -> expect_off_board (fun () -> B.flip b loc))
           [(-1, 0); (0, -1); (-1, -1); (4, 5); (5, 4); (5, 5)]
       in
         assert_bool msg result

     (* Making a board with invalid locations raises an Off_board exception. *)
     let test5c () =
       let msg =
         "making a board using invalid locations raises the Off_board exception"
       in
       let result =
         List.for_all (fun loc -> expect_off_board (fun () -> B.make [loc]))
           [(-1, 0); (0, -1); (-1, -1); (4, 5); (5, 4); (5, 5)]
       in
         assert_bool msg result

  end

module ArrayBoardTest = BoardTest(ArrayBoard)
module SetBoardTest = BoardTest(SetBoard)

(*** PART 1: Board representations. ***)

let board_tests = "board_tests" >:::
[

  "Array board tests" >:: (fun c ->
     ArrayBoardTest.test1a ();
     ArrayBoardTest.test1b ();
     ArrayBoardTest.test2 ();
     ArrayBoardTest.test3 ();
     ArrayBoardTest.test4a ();
     ArrayBoardTest.test4b ();
     ArrayBoardTest.test5a ();
     ArrayBoardTest.test5b ();
     ArrayBoardTest.test5c ();
  );

  "Set board tests" >:: (fun c ->
     SetBoardTest.test1a ();
     SetBoardTest.test1b ();
     SetBoardTest.test2 ();
     SetBoardTest.test3 ();
     SetBoardTest.test4a ();
     SetBoardTest.test4b ();
     SetBoardTest.test5a ();
     SetBoardTest.test5b ();
     SetBoardTest.test5c ();
  );

]

(*** PART 2: Game object. ***)

let init1a =
  [|
     [| On;  On;  Off; Off; Off |];
     [| On;  Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |]
  |]

let init1a_locs = [(0, 0); (0, 1); (1, 0)]

let init1b =
  [|
     [| Off; Off; Off; On;  On  |];
     [| Off; Off; Off; Off; On  |];
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |]
  |]

let init1b_locs = [(0, 3); (0, 4); (1, 4)]

let init1c =
  [|
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |];
     [| On;  Off; Off; Off; Off |];
     [| On;  On;  Off; Off; Off |]
  |]

let init1c_locs = [(3, 0); (4, 0); (4, 1)]

let init1d =
  [|
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; On  |];
     [| Off; Off; Off; On;  On  |]
  |]

let init1d_locs = [(3, 4); (4, 3); (4, 4)]

let init2a =
  [|
     [| Off; On;  On;  On;  Off |];
     [| Off; Off; On;  Off; Off |];
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |]
  |]

let init2a_locs = [(0, 1); (0, 2); (0, 3); (1, 2)]

let init2b =
  [|
     [| Off; Off; Off; Off; Off |];
     [| On;  Off; Off; Off; Off |];
     [| On;  On;  Off; Off; Off |];
     [| On;  Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |]
  |]

let init2b_locs = [(1, 0); (2, 0); (2, 1); (3, 0)]

let init2c =
  [|
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; On  |];
     [| Off; Off; Off; On;  On  |];
     [| Off; Off; Off; Off; On  |];
     [| Off; Off; Off; Off; Off |]
  |]

let init2c_locs = [(1, 4); (2, 3); (2, 4); (3, 4)]

let init2d =
  [|
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; On;  Off; Off |];
     [| Off; On;  On;  On;  Off |]
  |]

let init2d_locs = [(3, 2); (4, 1); (4, 2); (4, 3)]

let init3 =
  [|
     [| Off; Off; Off; Off; Off |];
     [| Off; Off; On;  Off; Off |];
     [| Off; On;  On;  On;  Off |];
     [| Off; Off; On;  Off; Off |];
     [| Off; Off; Off; Off; Off |]
  |]

let init3_locs = [(1, 2); (2, 1); (2, 2); (2, 3); (3, 2)]

let init4 =
  [|
     [| On;  On;  On;  Off; Off |];
     [| Off; Off; Off; On;  Off |];
     [| On;  On;  Off; On;  On  |];
     [| Off; Off; Off; On;  Off |];
     [| On;  On;  On;  Off; Off |]
  |]

let init4_locs = [(0, 0); (0, 1); (0, 2); (1, 3); (2, 0); (2, 1);
                  (2, 3); (2, 4); (3, 3); (4, 0); (4, 1); (4, 2)]

module GameTest(B : BOARD) (G : GAME with type t = B.t) =
  struct
    (* Test the "play" function.
     * Playing the best move on test boards one move away from solving
     * solves them. *)
    let test1 () =
      let test_locs name init_locs move_loc =
        let b = B.make init_locs in
        let b' = G.play b move_loc in
        let (r, c) = move_loc in
        let msg =
           Printf.sprintf "Playing (%d, %d) should solve board %s\n"
             r c name
        in
          assert_bool msg (B.is_solved b')
      in
        begin
          test_locs "init1a" init1a_locs (0, 0);
          test_locs "init1b" init1b_locs (0, 4);
          test_locs "init1c" init1c_locs (4, 0);
          test_locs "init1d" init1d_locs (4, 4);
          test_locs "init2a" init2a_locs (0, 2);
          test_locs "init2b" init2b_locs (2, 0);
          test_locs "init2c" init2c_locs (2, 4);
          test_locs "init2d" init2d_locs (4, 2);
          test_locs "init3"  init3_locs  (2, 2);
        end

    (* Test the "play_many" function.
     * Playing a list of moves that is known to solve a board should
     * solve it. *)
    let test2 () =
      let test_locs init_locs move_locs =
        let b = B.make init_locs in
        let b' = G.play_many b move_locs in
        let msg = "Playing the right moves should solve board init4\n" in
          assert_bool msg (B.is_solved b')
      in
        (* Here we give a list of moves that should solve board init4: *)
        test_locs init4_locs
          [(1, 0); (1, 1); (1, 2); (2, 1);
           (3, 0); (3, 1); (3, 3); (3, 4);
           (4, 1); (4, 3); (0, 1); (1, 0);
           (1, 1); (1, 2); (2, 3); (3, 0);
           (3, 1); (3, 3); (3, 4); (4, 3) ]

     (* Test the "from_array" function.
      * Creating a board from an array should make a board with
      * the correct locations on. *)
     let test3 () =
       let correct_contents init_arr init_locs =
         let other = other_locs init_locs in
         let b = G.from_array init_arr in
         let good = List.for_all (B.get b) init_locs in
         let bad = List.for_all (fun loc -> not (B.get b loc)) other in
           good && bad
       in
       let msg name =
          Printf.sprintf
            "A board created from array %s should have the right contents\n"
              name
       in
         begin
           assert_bool (msg "init1a") (correct_contents init1a init1a_locs);
           assert_bool (msg "init1b") (correct_contents init1b init1b_locs);
           assert_bool (msg "init1c") (correct_contents init1c init1c_locs);
           assert_bool (msg "init1d") (correct_contents init1d init1d_locs);
           assert_bool (msg "init2a") (correct_contents init2a init2a_locs);
           assert_bool (msg "init2b") (correct_contents init2b init2b_locs);
           assert_bool (msg "init2c") (correct_contents init2c init2c_locs);
           assert_bool (msg "init2d") (correct_contents init2d init2d_locs);
           assert_bool (msg "init3")  (correct_contents init3 init3_locs);
           assert_bool (msg "init4")  (correct_contents init4 init4_locs);
         end
  end

module ArrayGame = Game(ArrayBoard)
module ArrayGameTest = GameTest(ArrayBoard)(ArrayGame)
module SetGame = Game(SetBoard)
module SetGameTest = GameTest(SetBoard)(SetGame)

let game_tests = "game_tests" >:::
[

  "Array game tests" >:: (fun c ->
     ArrayGameTest.test1 ();
     ArrayGameTest.test2 ();
     ArrayGameTest.test3 ();
  );

  "Set game tests" >:: (fun c ->
     SetGameTest.test1 ();
     SetGameTest.test2 ();
     SetGameTest.test3 ();
  );
]

let run_tests () =
  begin
    Printf.printf "\nRUNNING BOARD TESTS...\n\n";
    run_test_tt_main board_tests;
    Printf.printf "\nRUNNING GAME TESTS...\n\n";
    run_test_tt_main game_tests;
  end

(* Comment this out if you are running this interactively (see above). *)
let _ = run_tests ()
