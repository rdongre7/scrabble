open Words

(* Abstract type [t] keeps track of the state,
    which includdes the [dictionary] used, letters let in the [bag],
    scores and current tiles on the rack of [player_one] and [ai],
    and the words on the [board] *)
type t = {
  dictionary: string list; 
  bag:  char list;  
  player_one: int * (char list); 
  ai: int * (char list); 
  board: (char list) list; 
}

(* Type [letter] keeps track of the letter by including 
    [space1] and [space2] as [coord] and saving it's direction. *)
type letter = {
  space1: int; 
  space2: int; 
  coord: int*int; 
  dir: bool; 
}

(**  [result] is all possible outcomes of each round.*)
type result = 
  | Legal of t
  | Finished
  | Illegal 

(**  [get_new_board' r c] is the board initializer with [r] rows and [c] columns.

      Example:
     - [get_new_board 2 2] is [['0';'0'];['0';'0']]*)
let rec get_new_board' r c = 
  let rec get_new_row col = 
    if col=1 then ['0'] else '0'::(get_new_row (col-1))
  in if r=1 then [get_new_row c] else (get_new_row c)::(get_new_board' (r-1) c)

(**  [get_new_board] is the board initializer 15 by 15.*)
let get_new_board = get_new_board' 15 15

(**  [remove_character rem bag acc] is the bag with remaining letters 
      without [char].

      Example:
     - [remove_character  'A' ['A';'B']] is [['B']]

      Requires: [rem] is in [bag]. *)
let rec remove_character (rem:char) bag acc = 
  match bag with 
  |[] -> acc
  |h::t -> if h = rem then remove_character '~' t acc 
    else remove_character rem t (h::acc)

(**  [remove_all_characters word inventory] is the bag with all letters in [word] 
      removed. 

      Example: 
     - [remove_all_characters ['Y';'A';'Y'] ['Y';'A';'Y';'Y';'S']] is ['Y'; 'S'] 

      Requires: all letters in [word] are in [inventory]. *)
let rec remove_all_characters word inventory = 
  match word with 
  |[] -> inventory
  |h::t -> remove_all_characters t (remove_character h inventory [])

(**  [to_char_list word acc index] converts the [word] to a list of characters.

      Example:
     - [to_char_list "stuff"] is [['S';'T';'U';'F';'F']]*)
let rec to_char_list word = 
  List.init (String.length word) (String.get word)

(**  [check_over bag inventory] determines the winning state of the game, if 
      the players do not have any letters left on their racks.

      Example:
     - [check_over [] []] is [true]
     - [check_over ['A';'C'] ['B';'E']] is [false]*)
let check_over bag inventory = 
  ((List.length bag) = 0) && ((List.length inventory) = 0)

(** [check_direction_of_char row pos] is [true] if the characters preceding and
     following the [pos]th position, which is not an edge, in [row] are empty. 
     [false] otherwise. *)
let check_direction_of_char row pos = 
  if (pos = 14 || pos = 1) then false(** THIS IS N O T RIGHT FIX THIS BRO*)
  else 
  if List.nth row (pos-1) = '0' && List.nth row (pos+1) = '0' then true 
  else false 

(** [transpose_matrix matrix] is the lists of lists represented by the lists of
      lists [matrix] after the transpose operation.

      Example:
    - [transpose_matrix []] is [[]]
    - [transpose_matrix [['a'; 'b']; ['c'; 'd']]] is [[['a'; 'c']; ['b'; 'd']]] *)
let rec transpose_matrix matrix = 
  match matrix with
  | [] -> []
  | []::xss  -> transpose_matrix xss 
  | (x::xs)::xss -> (x :: List.map List.hd xss) :: transpose_matrix (xs :: List.map List.tl xss)

(** [print_list lst] prints the contents of [lst]. *)
let rec print_list lst = 
  match lst with 
  |[] -> ()
  |h::t -> let _ = print_endline (Char.escaped h) in print_list t

(** [get_spaces num row coord dir] is the number of spaces allowed 
    for the ai to place a letter. *)
let get_spaces (num: int) (row: char list) (coord: int*int) (dir: bool): letter = 
  let rec get_spaces_helper num curr row space1 space2 temp = 
    match row with 
    |[] -> {space1 = space1; space2 = space2; coord = coord; dir = dir}
    |h::t -> if curr < num then 
        if (h = '0') 
        then get_spaces_helper num (curr + 1) t (space1 + 1) space2 temp
        else get_spaces_helper num (curr + 1) t 0 space2 temp
      else if (curr = num) 
      then get_spaces_helper num (curr + 1) t space1 space2 temp
      else  if (h = '0' && temp) 
      then get_spaces_helper num (curr + 1) t space1 (space2+1) temp
      else get_spaces_helper num (curr + 1) [] space1 space2 false 
  in get_spaces_helper num 0 row 0 0 true 

(** [get_list board] gets the list of letters that
    could be placed by AI on the board. *)
let rec get_list (board: (char list) list) = 
  let transposed = transpose_matrix board in 
  let rec get_list_1 idx ters board2 = 
    match board2 with 
    |[] -> ters 
    |h1::t1 -> 
      get_list_1 (idx + 1)
        ((let rec get_list_2 num acc (row: char list) = 
            match row with 
            |[] -> acc
            |h::t -> if (h <> '0') 
              then get_list_2 (num+1) 
                  ((if (check_direction_of_char h1 num) 
                    then (get_spaces num h1 (idx, num) (check_direction_of_char h1 num)) 
                    else (get_spaces idx (List.nth transposed num) 
                            (idx, num))(check_direction_of_char h1 num) )::acc) t 
              else get_list_2 (num+1) acc t in 
          get_list_2 0 [] h1) @ ters) t1
  in get_list_1 0 [] board

(** [contains_minus_1 word inventory] checks if the letter is in the AI's
    inventory and if it can be placed on the board.
    In addition, it returns the letter and its position if it can be placed. *)
let contains_minus_1 word inventory = 
  let rec contains_minus_1_helper word1 inventory1 pepito pos =
    match word1 with 
    |[] -> (true, (pepito, pos))
    |h::t ->  if List.mem (Char.uppercase_ascii h) inventory1 then 
        let x = (remove_character (Char.uppercase_ascii h) inventory1 []) 
        in contains_minus_1_helper t x
          pepito ((if (pepito = '0' && h <> '0') then (pos+1) else pos))
      else if (pepito = '0' && h <> '0') 
      then contains_minus_1_helper t inventory1 h pos 
      else (false, (pepito, pos)) in 
  contains_minus_1_helper word inventory '0' 0 

(** [print_list_string lst] prints a list of strings *)
let rec print_list_string lst = 
  match lst with 
  |[] -> ()
  |h::t -> let _ = print_endline h in print_list_string t

(** [print_list_letters lst board] prints the letters that are being
    analyzed by AI. *)
let rec print_list_letters lst board = 
  match lst with 
  |[] -> ()
  |h::t -> let x = (get_char_2d ((fst h.coord), (snd h.coord)) board) in 
    let _ = print_endline (Char.escaped x) in 
    let _ = print_endline (string_of_int h.space1) in 
    let _ = print_endline (string_of_int h.space2) in print_list_letters t board

(** [pepitototo ter dict acc] is the list of strings that are valid given the 
    board and the letter to search for. *)
let rec pepitototo ter dict acc = 
  match dict with 
  |[] -> acc
  |h::t -> if (String.length h > 0) then 
      if (Char.uppercase_ascii h.[0]) = ter then pepitototo ter t (h::acc) 
      else pepitototo ter t acc
    else pepitototo ter t acc

(** [get_coord dir coord pos] gets the next coordinate depending on 
    the direction of the word. *)
let get_coord dir coord pos = 
  if dir then (fst coord, (snd coord - pos))
  else (fst coord - pos, snd coord)

(** [check_surroundings dir coord len board temp] checks
    if the word intersects with any other word
    or if it is going ouside the bounds. *)
let rec check_surroundings dir coord len board temp = 
  if len = 0 then true
  else
  if dir then if (get_char_2d (fst coord - 1, snd coord) board) = '0' 
              && (get_char_2d (fst coord + 1, snd coord) board) = '0' 
    then check_surroundings dir (fst coord, snd coord + 1) (len - 1) board temp 
    else if temp 
    then check_surroundings dir (fst coord, snd coord + 1) (len - 1) board false
    else false
  else 
  if (get_char_2d (fst coord, snd coord - 1) board) = '0' 
  && (get_char_2d (fst coord, snd coord + 1) board) = '0'
  then check_surroundings dir (fst coord + 1, snd coord) (len - 1) board temp
  else if temp 
  then check_surroundings dir (fst coord + 1, snd coord) (len - 1) board false 
  else false

(** [find_board_letters c pos len board] finds letters on the board
    so that AI can orient itself in which direction it needs to go. *)
let rec find_board_letters c pos len board = function 
  |[] -> (false, (true,(0,0)))
  |h::t1 -> 
    if ((Char.uppercase_ascii c) = (get_char_2d h.coord board)) 
    && (h.space1 > pos) && (h.space2 > (len - 1 - pos)) 
    then let t = get_coord h.dir h.coord pos in 
      if check_surroundings h.dir t len board true then (true, (h.dir, t)) 
      else find_board_letters c pos len board t1
    else find_board_letters c pos len board t1

(** [find_word board words board_ters inventory] is the full algorithm
    of whether or not there is a word available for AI to be placed
    on the board. It takes into account the inventory and the current 
    letters on the board. *)
let find_word board words board_ters inventory = 
  let rec find_word_helper board words (board_ters: letter list) inventory curr : (string * (bool * (int*int))) option =
    match words with 
    |[] -> curr
    |h::t -> 
      let x = contains_minus_1 (to_char_list h) inventory 
      in if fst x && (String.length h <> 1) then 
        let y = (find_board_letters (fst (snd x)) 
                   (snd (snd x)) (String.length h) board board_ters) 
        in if fst y then 
          (match curr with
           |None -> find_word_helper board t board_ters inventory
                      (Some (String.uppercase_ascii h, snd y))
           |Some z -> if (String.length h > String.length (fst z)) 
             then find_word_helper board t board_ters inventory
                 (Some (String.uppercase_ascii h, snd y))
             else find_word_helper board t board_ters inventory (Some z))
        else find_word_helper board t board_ters inventory curr
      else find_word_helper board t board_ters inventory curr
  in find_word_helper board words board_ters inventory None

(** [get_ai_word board words board_ters inventory] is the full algorithm
    of whether or not there is a word available for AI to be placed
    on the board. It takes into account the inventory and the current 
    letters on the board. *)
let get_ai_word board ai dict = 
  let character_list = get_list board in 
  let rec get_ai_word_helper dict inventory curr = 
    match inventory with 
    |[] -> curr
    |h::t -> 
      match find_word board (pepitototo h dict []) character_list (snd(ai)) with 
      |None -> get_ai_word_helper dict t curr  
      |Some x -> match curr with 
        |None -> get_ai_word_helper dict t (Some x) 
        |Some y -> if (String.length (fst x) > String.length (fst y)) 
          then get_ai_word_helper dict t (Some x) 
          else get_ai_word_helper dict t curr
  in get_ai_word_helper dict (snd (ai)) None

(** [get_new_bag st word] is the set of letters returned after removing all letters
    in the played word.

    Example:
    - [get_new_bag ['A';'B'] ['A']] is [['B']]
*)
let get_new_bag st word = 
  let rec get_new_bag_helper bag characters = 
    match characters with 
    |[] -> bag
    |h::t -> get_new_bag_helper (remove_character h bag []) t in 
  get_new_bag_helper st.bag (to_char_list word)

(** [get_random_letter bag] is a random letter from the bag of letters.

    Example:
    - [get_random_letter ['N';'O']] is [['O']]
*)
let get_random_letter (bag: char list) = 
  Random.self_init ();
  let rand = Random.int (List.length bag) in 
  List.nth bag rand 

(** [replace_letters_input bag num acc] is a new rack of letters
    which is done when no words are available for the AI. *)
let rec replace_letters_input bag num acc = 
  if num = -1 then (bag, acc) else 
    let temp = get_random_letter bag in 
    replace_letters_input (remove_character temp bag []) (num - 1) (temp::acc)

(** [replace_letters_input2 bag num acc] is a new rack of letters
    which is done when no words are available for the AI. *)
let rec replace_letters_input_2 bag num acc = 
  if num = -1 then (bag, acc) else 
    let temp = get_random_letter bag in 
    replace_letters_input (remove_character temp bag []) (num - 1) (temp::acc)

(** [replace_letters bag num] returns the new bag, after removing [num] letters, 
    along with a set of [num] random letters.

    Example:
    - [replace_letters ['N';'O'] 1 ] is [['O']]
*)
let replace_letters bag num = 
  replace_letters_input bag num []

(** [init_state s] is the initial state of the game. 
    The bag of letters and a rack of tiles are initialized. *)
let init_state file = 
  let init_letters = get_init_letters in 
  let dict = get_dictionary file in 
  let player_one_bag_words = replace_letters init_letters 6 in 
  let player_two_bag_words = replace_letters (fst player_one_bag_words) 6 in
  {dictionary = dict;
   bag = fst (player_two_bag_words);
   player_one = (0, snd (player_one_bag_words)); 
   ai = (0, snd (player_two_bag_words)); 
   board = get_new_board}

(** [contains_log dict word] performs a binary search through an ordered dictionary 
    to see if the word is contained within it, returning true if the word is contained
    and false otherwise.

    Example:
    - [contains_log ["hello";"world"]  "hello"] is [true]
    - [contains_log ["hello";"world"]  "hi"] is [false]
*)
let contains_log dict word =
  let rec search dict word min max =
    let mid = (min + max) / 2 in
    if max < min then false else
    if (String.compare (List.nth dict mid) word = 0) then true else 
    if (String.compare (List.nth dict mid) word = 1) then (search dict word (mid+1) max) else
      search dict word min (mid-1) in 
  search dict word 0 ((List.length dict) - 1) 

(** [cl2s cl] is the string returned from a list of characters  *)
let cl2s cl = String.concat "" (List.map (String.make 1) cl)

(** [get_missing_letter_pos] returns the index of the missing letter in the word. *)
let get_missing_letter_pos (word:char list) (dir: bool) (curr: int*int) (board: (char list) list)= 
  let rec helper word num dir board = 
    match word with 
    |[] -> (-1)
    |h::t -> if dir then 
        if (get_char_2d ((fst curr), ((snd curr) + num)) board) <> '0' then num
        else helper t (num+1) dir board
      else 
      if (get_char_2d (((fst curr) + num), snd curr) board) <> '0' then num
      else helper t (num+1) dir board
  in helper word 0 dir board

(** [get_missing_letter dir board curr pos] is a missing
    character that is not on AI's rack but is on the board *)
let get_missing_letter dir board curr pos = 
  if dir then get_char_2d ((fst curr), ((snd curr) + pos)) board
  else get_char_2d (((fst curr) + pos), snd curr) board

(** [board_word_works word board pos dir curr turn ] is given the position of 
    the letter in the word, and you have the initial coordinate
     of the word, and you wanna make sure that the missing letter's position 
    in the board is equal to the letter that's currently there in the board.*)
let board_word_works (word:char list) (board: (char list) list) 
    (pos: int) (dir: bool) (curr: int*int) turn = 
  if turn = 1 then true 
  else
  if (get_missing_letter dir board curr pos = (List.nth word pos)) then true 
  else failwith "yikes"

(** For a word: you need to get the index of the missing letter (that isn't in 
    the word), and see if the word obtained from adding that letter to that 
    sposition in the word is in the dictionary. *)

(** [is_valid] is a string*bool tuple, with the first element representing the 
    new inventory after using up some characters to make a word, and the second 
    element representing whether or not the word is valid (i.e. whether it exists 
    in the dictionary and whether the player has the necessary characters in 
    his/her inventory.) *)
let is_valid (word:char list) st pos = 
  let rec is_valid_helper word inventory num pos temp = match word with 
    |[] -> (true, (inventory, (num+temp))) 
    |h::t -> if (List.mem h inventory) then is_valid_helper t 
          (if (h <> pos || temp <> 0) then (remove_character h inventory []) 
           else inventory) num pos (if h = pos then 1 else temp)   
      else if (num = 0) then is_valid_helper t inventory 1 pos temp 
      else (false, (inventory, num)) in 
  let temp = (is_valid_helper word (snd (st.player_one)) 0 pos 0) in 
  (snd temp, (contains_log st.dictionary (String.lowercase_ascii (cl2s word)) 
              && fst temp))

(** [contains_first lst1 lst2] finds if any of the words formed from chars are 
    valid words from [lst2] *)
let rec contains_first lst1 lst2 = 
  match lst1 with 
  |[] -> true
  |h::t -> if List.mem h lst2 
    then contains_first t (remove_character h lst2 []) else false

(** [find_word_first board words inventory] finds the word for the first time
    when the board is just initiated. Since there aren't any letters on the board
    the words is constructed just of letters on the rack. *)
let find_word_first  board words inventory = 
  let rec find_word_helper_first board words inventory 
      curr : ((string * (bool * (int*int))) option) =
    match words with 
    |[] -> curr
    |h::t -> 
      let x = contains_first (to_char_list (String.uppercase_ascii h)) inventory in 
      if x && (String.length h <> 1) then match curr with
        |None -> find_word_helper_first board t inventory
                   (Some (String.uppercase_ascii h, (true, (8,8))))
        |Some y -> if (String.length h > String.length (fst y)) 
          then find_word_helper_first board t inventory
              (Some (String.uppercase_ascii h, (true, (8,8))))
          else find_word_helper_first board t inventory (Some y)
      else find_word_helper_first board t inventory curr
  in find_word_helper_first board words inventory None

(** [first_turn_ai board ai dict] places the word for the first time
    when the board is just initiated. Since there aren't any letters on the board
    the words is constructed just of letters on the rack. *)
let first_turn_ai board ai dict =  
  let rec get_ai_word_helper_first dict inventory curr = 
    match inventory with 
    |[] -> curr
    |h::t -> match find_word_first board (pepitototo h dict []) (snd(ai)) with 
      |None -> get_ai_word_helper_first dict t curr  
      |Some x -> match curr with 
        |None -> get_ai_word_helper_first dict t (Some x) 
        |Some y -> if (String.length (fst x) > String.length (fst y)) 
          then get_ai_word_helper_first dict t (Some x) 
          else get_ai_word_helper_first dict t curr
  in get_ai_word_helper_first dict (snd (ai)) None


(** Idea: go through dictionary and find first instance that contains all the letters, minus one, 
    and the minus one letter is on the board with an open spot for the word*)

(** [place_word st lst coor dir] places the letter that the player
    provided on the board. Updates the score, board, 

    Requires: [st] to be a valid state of type [t]*)
let place_word st word coordinate dir turn = 
  try 
    (* let dict = st.dictionary in  *)
    let pos = get_missing_letter_pos word dir coordinate st.board in 
    (** THIS WILL CAUSE AN ERROR WITH S AND STUFF, IF YOU'RE CREATING A BRAND NEW WORD*)
    let valid = is_valid word st (if turn <> 1 then (List.nth word pos) else '0') in 
    if snd valid then try 
        (* let test =  board_word_works word st.board pos dir coordinate turn in   *)
        let ters = st.bag in 
        let player_one_bag_words = replace_letters_input ters 
            ((List.length word) - 1 - snd (fst valid)) (fst (fst valid)) in 
        let new_board_score = place word coordinate dir st.board in
        let ai_word = match get_ai_word (fst new_board_score) st.ai st.dictionary with 
          |Some x -> x
          |None -> ("", (false, (0,0))) in 
        let new_board_2 = place (to_char_list (fst ai_word)) (snd (snd ai_word)) 
            (fst (snd ai_word)) (fst new_board_score) in 
        let ai_bag_words = if (fst ai_word = "") then replace_letters 
              ((snd st.ai)@(fst player_one_bag_words)) 6 
          else replace_letters_input (fst player_one_bag_words) 
              (List.length (to_char_list (fst ai_word)) - 2) 
              (remove_all_characters (to_char_list (fst ai_word)) (snd st.ai)) in
        if check_over (fst (ai_bag_words)) (snd (player_one_bag_words)) then 
          Finished else Legal
            {dictionary = st.dictionary;
             bag = fst (ai_bag_words);
             player_one = (snd new_board_score + (fst st.player_one), snd (player_one_bag_words)); 
             ai = (snd new_board_2 + (fst st.ai), snd (ai_bag_words));
             board = fst new_board_2} 
      with 
      | Failure x -> let _ = print_endline x in Illegal
      | Invalid_argument x -> Illegal 
    else Illegal
  with Invalid_argument x -> Illegal 

(** [replace_letters_2 st turn] is the new state that replaces
    the words in ai's rack if there are no words that can be placed on the 
    board. The entire state is updated since the inventory is new.*)
let replace_letters_2 st turn = 
  let yeet = replace_letters ((snd st.player_one)@(st.bag)) 6 in
  let ai_word_yeet = match get_ai_word st.board st.ai st.dictionary with 
    |Some x -> x
    |None -> if turn = 1 then (match first_turn_ai st.board st.ai st.dictionary 
                               with |None -> ("", (false, (0,0))) 
                                    |Some x -> x ) 
      else ("", (false, (0,0))) in 
  let new_board_yeet = place (to_char_list (fst ai_word_yeet)) 
      (snd (snd ai_word_yeet)) (fst (snd ai_word_yeet)) (st.board) in 
  let ai_bag_words_yeet = if (fst ai_word_yeet = "") 
    then replace_letters ((snd st.ai)@(fst yeet)) 6 
    else replace_letters_input (fst yeet) 
        (List.length (to_char_list (fst ai_word_yeet)) - 2) 
        (remove_all_characters (to_char_list (fst ai_word_yeet)) (snd st.ai)) in
  if check_over (fst (ai_bag_words_yeet)) (snd (yeet)) then 
    Finished else Legal
      {dictionary = st.dictionary;
       bag = fst ai_bag_words_yeet;
       player_one = (fst st.player_one, snd yeet);
       ai = (snd new_board_yeet, snd (ai_bag_words_yeet));
       board = fst new_board_yeet} 

(** [get_hints st turn] forms the hint for the user
    with just the word. It is the first instance of the word. *)
let get_hints st turn = 
  let x = if (turn <> 1) then get_ai_word st.board st.player_one st.dictionary 
    else first_turn_ai st.board st.player_one st.dictionary in 
  match x with 
  |Some y -> Some (fst y)
  |None -> None 

(** [get_ai_game st turn] sets the game of AI against AI
    in a step by step fashion. *)
let get_ai_game st turn = 
  let x = if (turn <> 1) then get_ai_word st.board st.player_one st.dictionary 
    else first_turn_ai st.board st.player_one st.dictionary in 
  match x with 
  |Some y -> Some ((to_char_list (fst y)), (fst (snd y), (snd (snd y))))
  |None -> None

(** [get_score s] a tuple with both player's and AI's current scores. 

    Example:
    - [get_score state] is [(15,0)]*)
let get_score st = ((fst st.player_one), (fst st.ai))

(** [get_letters s] is the letters that the player has on the rack. 

    Example:
    - [get_letters state] is [['S';'F';'P';'E']]*)
let get_letters st = snd (st.player_one)

(** [get_board s] is the current board with words. *)
let get_board st = st.board 

(** [get_dict_length st] is the length of the dictionary. *)
let get_dict_length st = (List.length st.dictionary) 

(** [get_bag st] is the current bag of words. *)
let get_bag st = st.bag

(** [get_hint_coord st turn] forms the hint for the user
    with both the word and coordinate. It is the first instance of the word. *)
let get_hint_coord st turn = 
  let x = if (turn <> 1) then get_ai_word st.board st.player_one st.dictionary 
    else first_turn_ai st.board st.player_one st.dictionary in 
  match x with 
  |Some y -> Some (fst y, (snd (snd y)))
  |None -> None 