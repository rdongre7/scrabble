open OUnit2
open Command
open State
open Words


(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
let cmp_demo = 
  [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ]





(** A list of [words_tests] tests. *)
let rec get_new_board' r c = 
  let rec get_new_row col = 
    if col=1 then ['0'] else '0'::(get_new_row (col-1))
  in if r=1 then [get_new_row c] else (get_new_row c)::(get_new_board' (r-1) c)

let bn15 = get_new_board' 15 15
let bn7 = get_new_board' 7 15
let bn5 = get_new_board' 5 15
let bn4 = get_new_board' 4 15
let b1 = bn7@[['0';'0';'0';'0';'0';'0';'0';'L';'E';'G';'0';'0';'0';'0';'0']]@bn7
let b2 = bn7@[['0';'0';'0';'0';'0';'0';'0';'L';'0';'0';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'E';'0';'0';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'G';'0';'0';'0';'0';'0';'0';'0']
             ]@bn5
let b3 = bn7@[['0';'0';'0';'0';'0';'0';'0';'L';'E';'G';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'I';'0';'0';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'K';'0';'0';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'E';'0';'0';'0';'0';'0';'0';'0']
             ]@bn4
let b4 = bn7@[['0';'0';'0';'0';'0';'0';'0';'L';'I';'M';'E';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'E';'0';'0';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'G';'0';'0';'0';'0';'0';'0';'0']
             ]@bn5
let b5 = bn7@[['0';'0';'0';'0';'0';'0';'0';'L';'E';'G';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'0';'G';'0';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'0';'G';'0';'0';'0';'0';'0';'0']
             ]@bn5
let b6 = bn7@[['0';'0';'0';'0';'0';'0';'0';'L';'0';'0';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'E';'0';'0';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'G';'O';'N';'E';'0';'0';'0';'0']
             ]@bn5
let b7 = bn7@[['0';'0';'0';'0';'0';'0';'0';'L';'E';'G';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'0';'0';'O';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'0';'0';'N';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'0';'0';'E';'0';'0';'0';'0';'0']
             ]@bn4
let b8 = bn7@[['0';'0';'0';'0';'0';'0';'0';'L';'0';'0';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'E';'X';'C';'E';'L';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'G';'0';'0';'0';'0';'0';'0';'0']
             ]@bn5
let b9 = bn7@[['0';'0';'0';'0';'0';'0';'0';'L';'0';'T';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'E';'0';'E';'0';'0';'0';'0';'0'];
              ['0';'0';'0';'0';'0';'0';'0';'G';'O';'N';'E';'0';'0';'0';'0']
             ]@bn5
let b10 = bn7@[['0';'0';'0';'0';'0';'0';'0';'L';'I';'T';'I';'G';'A';'T';'E'];
               ['0';'0';'0';'0';'0';'0';'0';'E';'0';'E';'0';'0';'0';'0';'0'];
               ['0';'0';'0';'0';'0';'0';'0';'G';'O';'N';'E';'0';'0';'0';'0']
              ]@bn5
let b11 =[['0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'H'];
          ['0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'E'];
          ['0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'Y']
         ]@(get_new_board' 12 15)
let b12 = (get_new_board' 14 15)@
          [['0';'0';'0';'H';'E';'Y';'0';'0';'0';'0';'0';'0';'0';'0';'0']]
let b13 =(get_new_board' 12 15)@
         [['0';'0';'0';'0';'0';'0';'0';'0';'0';'H';'0';'0';'0';'0';'0'];
          ['0';'0';'0';'0';'0';'0';'0';'0';'0';'E';'0';'0';'0';'0';'0'];
          ['0';'0';'0';'0';'0';'0';'0';'0';'0';'Y';'0';'0';'0';'0';'0']]
let b14 = [['0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'0';'H';'E';'Y']]
          @(get_new_board' 14 15)
let b15 = [['P';'O';'T';'A';'T';'O';'0';'0';'0';'0';'0';'0';'0';'0';'0'];
           ['0';'0';'0';'0';'0';'N';'0';'0';'0';'0';'0';'0';'0';'0';'0'];
           ['0';'0';'0';'0';'0';'E';'0';'0';'0';'0';'0';'0';'0';'0';'0']]
          @ (get_new_board' 12 15)
let b16 = [['0';'0';'0';'0';'0';'0';'P';'O';'T';'0';'0';'0';'0';'0';'0'];
           ['0';'0';'0';'0';'0';'0';'0';'0';'O';'0';'0';'0';'0';'0';'0'];
           ['0';'0';'0';'0';'0';'0';'0';'0';'E';'0';'0';'0';'0';'0';'0'];
           ['0';'0';'0';'0';'0';'0';'0';'0';'S';'0';'0';'0';'0';'0';'0']]
          @ (get_new_board' 11 15)
let b17 = [['0';'0';'0';'0';'0';'0';'P';'O';'T';'0';'0';'0';'0';'0';'0'];
           ['0';'0';'0';'0';'0';'0';'0';'N';'O';'0';'0';'0';'0';'0';'0'];
           ['0';'0';'0';'0';'0';'0';'0';'0';'E';'0';'0';'0';'0';'0';'0'];
           ['0';'0';'0';'0';'0';'0';'0';'0';'S';'0';'0';'0';'0';'0';'0']]
          @ (get_new_board' 11 15)

let w0 = create_word ['W'] (-1, -1123) false
let w1 = create_word ['X'] (0, 0) true
let w2 = create_word ['C'] (14, 14) true
let w3 = create_word ['A'] (14, 16) false
let w4 = create_word ['Q'] (20, 20) false
let w5 = create_word ['I'; 'O'] (4, 5) false
let w6 = create_word ['I'; 'O'] (13, 14) true
let w7 = create_word ['I'; 'O'] (13, 14) false
let w8 = create_word ['O'; 'C'; 'A'; 'M'; 'L'] (10, 7) true
let w9 = create_word ['O'; 'C'; 'A'; 'M'; 'L'] (10, 12) true
let w10 = create_word ['O'; 'C'; 'A'; 'M'; 'L'] (16, 16) false
let w11 = create_word ['T'; 'A'; 'B'] (1, 5) false

let make_place_tests
    (name : string)
    (word : char list)
    (coordinate : int * int)
    (dir : bool)
    (board : char list list)
    (output : char list list * int) : test =
  name >:: (fun _ -> assert_equal output (place word coordinate dir board))

(** [make_word_in_board_tests name word coordinate dir board output] is an OUnit 
    test named [name] that asserts the quality of 
    [word_in_board w board] with [output]. *)
let make_word_in_board_tests
    (name : string)
    (w : Words.t)
    (board : char list list)
    (output : bool) : test =
  name >:: (fun _ -> assert_equal output (word_in_board w board)) 

(** [undsiplayify_coord x] converts the tuple x, which represents a coordinate a
    player may input, into its representation as an index in a board. *)
let undisplayify_coord x = 
  (fst x - 1, snd x - 1)

(** [make_get_char_2d_test name coord board output] creates an OUnit test named 
     [name] that asserts the quality of [get_char_2d coord board] with [output]. *)
let make_get_char_2d_tests 
    (name : string)
    (coord : int * int)
    (board: char list list)
    (output: char) : test =
  name >:: (fun _ -> assert_equal output (get_char_2d coord board))

(** [make_playable_letter_tests name c coord board output] creates an OUnit test
     named [name] that asserts the quality of [playable_letter c coord board] with
     [output]. *)
let make_playable_letter_tests
    (name : string)
    (c : char)
    (coord: int * int)
    (board: char list list)
    (output: bool) : test = 
  name >:: (fun _ -> assert_equal output (playable_letter c coord board))

let make_all_new_words_test
    (name : string)
    (w : Words.t)
    (board : char list list)
    (words : string list)
    (output : bool) : test =
  name >:: (fun _ -> assert_equal output (cmp_set_like_lists (words) (all_new_words w board)))

let words_tests = 
  [  
    make_word_in_board_tests "single letter (-1, -1123)" w0 b14 false;
    make_word_in_board_tests "single letter (0, 0)" w1 b14 true;
    make_word_in_board_tests "single letter (14, 14)" w2 b14 true;
    make_word_in_board_tests "single letter (14, 15)" w3 b14 false;
    make_word_in_board_tests "single letter (20, 20)" w4 b14 false;
    make_word_in_board_tests "mult letter (4, 5)" w5 b14 true;
    make_word_in_board_tests "mult letter (13, 14) 1" w6 b14 false;
    make_word_in_board_tests "mult letter (13, 14) 2" w7 b14 true;
    make_word_in_board_tests "many letter (10, 7)" w8 b14 true;
    make_word_in_board_tests "many letter (10, 12)" w9 b14 false;
    make_word_in_board_tests "many letter (16, 16)" w10 b14 false;

    make_get_char_2d_tests "2d test 1" (undisplayify_coord (3, 3)) b1 '0';
    make_get_char_2d_tests "2d test 2" (undisplayify_coord (1, 8)) b14 '0';
    make_get_char_2d_tests "2d test 3" (undisplayify_coord (1, 14)) b11 '0';
    make_get_char_2d_tests "2d test 4" (undisplayify_coord (1, 15)) b11 'H';
    make_get_char_2d_tests "2d test 5" (undisplayify_coord(2, 15)) b11 'E';
    make_get_char_2d_tests "2d test 6" (undisplayify_coord(3, 15)) b11 'Y';
    make_get_char_2d_tests "2d test 7" (undisplayify_coord(4, 15)) b11 '0';

    make_playable_letter_tests "playable 1" 'A' (undisplayify_coord (3, 3)) b1 true;
    make_playable_letter_tests "playable 2" 'A' (undisplayify_coord (1, 14)) b11 true;
    make_playable_letter_tests "playable 3" 'H' (undisplayify_coord(1, 15)) b11 true;
    make_playable_letter_tests "playable 4" 'X' (undisplayify_coord(1, 15)) b11 false;

    (* make_all_new_words_test "words 1" w11 b15 ["TAB"; "POTATO"; "AN"; "BE"] true; *)

    make_place_tests "empty board h" ['L';'E';'G'] (7,7) true bn15 (b1,8);
    make_place_tests "empty board v" ['L';'E';'G'] (7,7) false bn15 (b2,8);

    make_place_tests "first first v" ['L';'I';'K';'E'] (7,7) false b1 (b3,16);
    make_place_tests "first first h" ['L';'I';'M';'E'] (7,7) true b2 (b4,12);
    make_place_tests "first middle v" ['E';'G';'G'] (7,8) false b1 (b5,7);
    make_place_tests "first middle h" ['E';'X';'C';'E';'L'] (8,7) true b2 (b8,22);
    make_place_tests "first end v" ['G';'O';'N';'E'] (7,9) false b1 (b7,7);
    make_place_tests "first end h" ['G';'O';'N';'E'] (9,7) true b2 (b6,7);

    make_place_tests "overlap multiples" ['L';'I';'T';'I';'G';'A';'T';'E'] (7,7) true b9 (b10,66);

    make_place_tests "right edge" ['H';'E';'Y'] (0,14) false bn15 (b11,27);
    make_place_tests "bottom edge" ['H';'E';'Y'] (14,3) true bn15 (b12,13);

    make_place_tests "ending at bottom" ['H';'E';'Y'] (12,9) false bn15 (b13,11);
    make_place_tests "ending at right" ['H';'E';'Y'] (0,12) true bn15 (b14,27);

    make_place_tests "corner case 1" ['N';'O'] (1,7) true b16 (b17,8);
    make_place_tests "corner case 2" ['O';'N'] (0,7) false b16 (b17,8)
  ]


(** [make_command_tests name input desc] constructs an OUnit
    test named [name] that asserts the quality of [input]
    with [parse input]. *)
let make_command_tests
    (name : string)
    (input: string)
    (otp: Command.command) : test = 
  name >:: (fun _ -> 
      assert_equal otp (parse input))

(** [make_command_tests_except name input exc] constructs an OUnit
    test named [name] that asserts that exception [exc] raises
    with [parse input]. *)
let make_command_tests_except
    (name : string)
    (input: string) 
    (exc): test = 
  name >:: (fun _ -> 
      assert_raises (exc) (fun () -> parse input))

(** A list of [command_tests] tests. *)
let command_tests =
  [
    make_command_tests "word stuff" "word stuff" (Word ['S';'T';'U';'F';'F']);
    make_command_tests "word stuff with spaces" "word   stuff" (Word ['S';'T';'U';'F';'F']);
    make_command_tests "word he" "word    he" (Word ['H';'E']);

    make_command_tests "score" "score" (Score);
    make_command_tests "quit" "quit" (Quit);
    make_command_tests "extrahint" "extrahint" (ExtraHint);
    make_command_tests "hint" "hint" (Hint);

    make_command_tests_except "empty_spaces" "    " Empty;
    make_command_tests_except "empty" "" Empty;
    make_command_tests_except "malformed" "potato chips" Malformed;
    make_command_tests_except "quit maformed" "quit word" Malformed;
    make_command_tests_except "hint maformed" "hint word" Malformed;
    make_command_tests_except "extrahint maformed" "extrahint word" Malformed;
    make_command_tests_except "score malformed" "score word" Malformed;
  ]

let make_state_tests_board
    (name : string)
    (input: string) 
    (otp: (char list) list) = 
  name >:: (fun _ -> 
      assert_equal otp (get_board (init_state input)))

let make_state_tests_dict
    (name : string)
    (input: string) 
    (otp: int) = 
  name >:: (fun _ -> 
      assert_equal otp (get_dict_length (init_state input)))

let get_result
    (input: string) 
    (otp: char list)
    (otp2: int*int)
    (otp3: bool) =  (place_word (init_state input) otp otp2 otp3 1)

let make_state_tests_place
    (name : string)
    (input: string) 
    (otp: char list)
    (otp2: int*int)
    (otp3: bool)
    (result: State.result) = 
  name >:: (fun _ -> 
      assert_equal result (place_word (init_state input) otp otp2 otp3 1))

let make_state_tests_place_2
    (name : string)
    (input: string) 
    (otp: char list)
    (otp2: int*int)
    (otp3: bool)
    (result: State.result)
    (boolean: bool) = 
  name >:: (fun _ -> 
      assert_equal boolean (result = (place_word (init_state input) otp otp2 otp3 1)))

let get_board
    (input: string) 
    (otp: char list)
    (otp2: int*int)
    (otp3: bool) = (place otp otp2 otp3 (get_board (init_state input)))
(** A list of [state_tests] tests. *)
let state_tests =
  [
    let x = get_board "english.txt" ['y';'e';'s'] (8,8) false in 
    let _ = print_endline (string_of_int (List.length (get_list (fst x)))) in 
    make_state_tests_board "test english" "english.txt" bn15;
    (* make_state_tests_dict "test english dict length" "english.txt" 84099; *)
    (*make_state_tests_place "test 1" "english.txt" ['A';'F';'E'] (5,5) true 
      (get_result "english.txt" [] (5,5) true);*)
    (* make_state_tests_place_2 "test 2" "english.txt" ['y';'e';'s'] (5,5) true 
       (get_result "english.txt" [] (5,5) true) false; *)


    (*make_state_tests_board "test english" "espanol.txt" bn15 FIX LATER*)
  ]

let suite =
  "test suite for A6"  >::: List.flatten [
    words_tests;
    command_tests;
    (* state_tests; *)
  ]

let _ = run_test_tt_main suite
