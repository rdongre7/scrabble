open Words
open Command
open State

(** [print_color_ch h] determines whether a special or an empty tile
    should be printed by looking at the coordinate [h]. 
    If a WORD is doubled/tripled, it is printed in RED.
    If a LETTER is doubled/tripled, it is printed in BLUE.

    Effects: prints one letter on the board.
*)
let print_color_ch h = 
  if is_double_word h then ANSITerminal.(print_string [red] "2 ") else
  if is_triple_word h then ANSITerminal.(print_string [red] "3 ") else
  if is_double_letter h then ANSITerminal.(print_string [cyan] "2 ") else
  if is_triple_letter h then ANSITerminal.(print_string [cyan] "3 ")
  else print_string "  "

(** [print_row lst row col] prints a row from [lst] while accounting for empty
    or special tiles by looking at [row] and [col].

    Effects: prints an entire row of the board.
*)
let rec print_row lst row col = 
  print_string "| ";
  match lst with
  | [] -> print_endline ""
  | h::t -> if h='0' then print_color_ch (row,col)  
    else print_string ((String.make 1 h)^" ") ; print_row t row (col+1)

(** [print_row col] prints the divider for the tiles at the top and bottom.
    Can be different lengths for the tiles on hand.

    Effects: prints an entire row of the board.
*)
let rec print_header col = if col = 0 then print_endline "+" 
  else let _ = print_string "+---" in print_header (col-1)

(** [print_number row] a number with a correct number of spaces.

    Effects: prints a number.
*)
let print_number row = 
  if row < 9 then print_string ("  "^(string_of_int (row+1))^" ")
  else print_string (" "^(string_of_int (row+1))^" ")

(** [print_board' board row col] prints the board stred in [board] while accounting 
    for empty or special tiles.

    Effects: prints the entire board on the terminal screen.
*)
let rec print_board' board row col pr = 
  if pr then print_string "    " else print_string "";
  print_header col;
  match board with
  | [] -> print_endline ""
  | h::t -> if pr then print_number row else print_string "";
    print_row h row 0; print_board' t (row+1) col pr

(** [print_board board] prints the board stred in [board] while accounting 
    for empty or special tiles.

    Effects: prints the entire board on the terminal screen.
*)
let print_board board = 
  print_endline "      1   2   3   4   5   6   7   8   9  10  11  12  13  14  15";
  print_board' board 0 15 true

(** [print_current_letters lst] prints the tiles that the player curretly has
    stored in [lst].

    Effects: prints the tiles that the user currently has.
*)
let print_current_letters lst = print_board' [lst] 1 (List.length lst) false

(** [split_coor str] splits the string into a tuple of coordinates. Prompts 
    to put in the coordinates again if they are invalid.

    Example:
    - [split_coor "6 5"] is [(6,5)]
    - [split_coor " 78 9 10"] is ["Invalid coordinates. Try again:"]
*)
let rec split_coor turn length dir = 
  let str = read_line() in
  if str = "q" then failwith "want to quit" else
    match List.filter (fun x -> x<>"") (String.split_on_char ' ' str) with
    | h1::h2::_ -> 
      (try
         let r = int_of_string h1 in 
         let c = int_of_string h2 in 
         if (turn <> 1) then ((r-1), (c-1))
         else
         if dir then 
           if c <= 8 && (c + length) > 8 && (r = 8) then ((r-1), (c-1))
           else (ANSITerminal.(print_string [red]  
                                 "\nInvalid coordinates. Try again:\n> ");
                 split_coor turn length dir)
         else 
         if r <= 8 && (r + length) > 8 && c = 8 then ((r-1), (c-1))
         else ( ANSITerminal.(print_string [red]  
                                "\nInvalid coordinates. Try again:\n> ");
                split_coor turn length dir)

       with Failure s -> (ANSITerminal.(print_string [red]  
                                          "\nInvalid coordinates. Try again:\n> ");
                          split_coor turn length dir))
    | _ -> (ANSITerminal.(print_string [red]  
                            "\nInvalid coordinates. Try again:\n> ");
            split_coor turn length dir)

let rec direction_input d = 
  match String.uppercase_ascii (read_line()) with 
  | "Q" -> failwith "want to quit"
  | "V" -> false
  | "H" -> true 
  | _ -> ANSITerminal.(print_string [red] "\nInvalid direction. Try again.\n> "); 
    direction_input d

(** [sleep i] the execution stops for [i] seconds.
*)
let rec sleep i = 
  Unix.sleepf 0.5; 
  if i=0 then print_string "." else (print_string "."; sleep (i-1))

(** [subscore state new_state] is the amount of points added to the player's score
    after one round of the game.
*)
let subscore state new_state = 
  let old_s = fst (State.get_score state) in 
  let new_s = fst (State.get_score new_state) in 
  string_of_int (new_s - old_s)

(** [t] is a record that stores the information about the new word from the user.
    [dir] component is [true] for horizontal, [false] for vertical.

    Example: 
    - [let stuff = {word=['S';'T';'U';'F';'F'];coordinate=(7,8);dir=true}]
*)
type t = {
  word : char list;
  coordinate : int*int;
  dir : bool
}

(** [continue strs] recursive method that prompts the user to to continue playing
    the game and put in a new word.

     Requires: a valid [state] of type State.t *)
let rec continue (state: State.t) turn = 
  print_endline "\nTiles on your rack: ";
  print_current_letters (State.get_letters state);                                         
  print_endline 
    "\nWhat word would you like to put on the board next? 
    (Start with 'word '/'hint'/'extrahint'/'score')";
  print_string  "> ";
  let new_line = read_line() in
  let _ = print_endline  "" in

  try
    match parse new_line with
    | Quit -> let _ =  ANSITerminal.(print_string [red] "Ending the game...\n") 
      in exit 0

    | Score -> 
      let (p1,p2) = State.get_score state in
      let _ = ANSITerminal.(print_string [green] 
                              ("You: "^(string_of_int p1)
                               ^" vs "^"AI: "^(string_of_int p2))) 
      in continue state turn

    | Hint -> 
      (match get_hint_coord state turn with 
       | None -> ANSITerminal.(print_string [red] 
                                 "No hints available :(\n - we will replace your letters"); 
         (match (replace_letters_2 state turn) with 
          |Legal new_state -> continue new_state (turn+1)
          |Finished -> print_endline ("You won with a score of "
                                      ^(string_of_int(fst (get_score state)))^"!")
          |Illegal-> failwith "impossible")
       | Some hint -> ANSITerminal.(print_string [green] 
                                      ("Hint: "^(fst hint))); continue state turn) 

    | ExtraHint -> (match get_hint_coord state turn with 
        | None -> ANSITerminal.(print_string [red] 
                                  "No hints available :(\n - we will replace your letters"); 
          (match (replace_letters_2 state turn) with 
           |Legal new_state -> continue new_state (turn+1)
           |Finished -> print_endline ("You won with a score of "
                                       ^(string_of_int(fst (get_score state)))^"!")
           |Illegal-> failwith "impossible")
        | Some hint -> 
          let coor = snd hint in 
          ANSITerminal.(print_string [green] 
                          ("Hint: "^(fst hint)^" at "
                           ^(string_of_int (if turn <> 1 then fst coor + 1 
                                            else fst coor))^" "
                           ^(string_of_int 
                               (if turn <> 1 then snd coor + 1 else snd coor)))); 
          continue state turn)

    | Word lst -> if (List.length lst) = 1 
      then (ANSITerminal.(print_string [red] "Place more than 1 letter"); 
            continue state turn) 
      else
        let _ = print_string "\nDirection 'V' (vertical) or 'H' (horizontal): \n> " in

        (* (try *)
        let dir = direction_input "zero" in
        let _ = print_string "\nFirst tile where the word starts, ex. '0 0': \n> " in
        let coord = split_coor turn (List.length lst) dir
        (* with Failure s -> print_endline "Changed your mind? try a new word!"; continue state turn)  *)
        in
        (match place_word state lst coord dir turn with                                                                    
         | Illegal -> 
           let _ = ANSITerminal.(print_string [red] 
                                   "The word cannot be placed on the board. Try again. \n") in
           continue state turn

         | Legal new_state ->  
           ANSITerminal.(print_string [green] 
                           ("Success! +"^
                            (subscore state new_state)^" points added to your score.\n"));
           print_string "AI is placing a word"; 
           (* sleep 5;  *)
           print_endline "";
           print_board (get_board new_state);
           continue new_state (turn+1)

         | Finished -> 
           print_endline ("You won with a score of "^(string_of_int(fst (get_score state)))^"!"))

  with 
  | Malformed -> 
    let _ = print_endline "Invalid command. Please enter another command. \n" in
    continue state turn
  | Empty -> 
    let _ = print_endline "This command is empty. Please enter another command. \n" in
    continue state turn 

let rec play_ai_game state turn num = 
  try 
    let new_line = read_line () in 
    if new_line = "quit" then let _ = ANSITerminal.(print_string [red] "Ending the game...\n") in exit 0
    else 
      let _ = print_endline "Press any key to play the next move!" in              
      let _ = print_board (get_board state) in 
      let x = (get_ai_game state turn) in 
      match x with 
      |None -> (match (replace_letters_2 state turn) with 
          |Legal new_state -> if (num < 5) then play_ai_game new_state (turn+1) (num+1)
            else let _ = print_endline "Game over!" in exit 0 
          |Finished -> let _ = print_endline "Game over!" in exit 0
          |_ -> let _ = print_endline "Error" in exit 0)
      |Some y -> 
        (match place_word state (fst y) (snd (snd y)) (fst (snd y)) turn with 
         |Legal z -> play_ai_game z (turn+1) 0 
         |_ -> let _ = print_endline "Game over!" in exit 0)
  with _ -> let _ = print_endline "Game over!" in 
    exit 0

(** [play_game f] starts the game with the given language [f]. 
    Checks if [f] language is supported by the game. If not, exits.

    Examples: 
    - [play_game "english"] gives the instuctions, prints the board, and continues

    - [play_game "japanese"] is ["Sorry, this language is not supported."] and exits*)
let play_game f m =
  try                              
    let init = init_state (f^".txt") in
    let _ = print_board (get_board init)   in         
    let _ = print_endline " *** \n Welcome! 
    \n You will be playing a lonely game of Scrabble. 
    \n Key to the board: \n - RED refers to doubling/trippling a WORD
    \n - BLUE refers to doubling/trippling a LETTER
    \n Valid commands are 'word [word]' followed by coordinates '0 0', 
    followed by direction 'h' or 'v', 'score', 'hint', 'extrahint', or 'quit'. \n 
    \n Good luck! \n ***" in

    if (m <> "ai") then continue init 1
    else play_ai_game init 1 0 
  with _ -> let _ = print_endline "Sorry, this language is not supported." in 
    let _ = print_endline "Try starting the game again." in
    exit 0

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.resize 80 45;
  ANSITerminal.(print_string [green]
                  "\n\nWelcome to this walmart version of Scrabble.\n");
  print_endline "Please enter the language you would like to use.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> 
    ANSITerminal.(print_string [green]
                    "\nPlease enter the mode you would like to play. 'ai' or 'player'\n\n"); 
    match read_line () with 
    |exception End_of_file -> () 
    |mode -> play_game file_name mode

(* Execute the game engine. *)
let _ = main ()


