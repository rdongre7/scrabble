open Unix 
open Str

(** [close_dir_helper dir acc] is a tuple with [dir] and [acc].
         Effects: closes the directory [dir]. *)
let close_dir_helper dir acc = 
  (closedir dir, acc)

(** [close_file_helper file acc] creates a tuple with [file] and [acc].
         Effects: closes the channel of the file [file]. *)
let close_file_helper file acc = 
  (close_in file, acc)

(** [split_regex acc pos regex str] is a set-like list of strings that match 
         the regular expression [regex] in [str], starting at the position [pos].*)
let rec split_regex acc pos regex str = 
  try
    let st = Str.search_forward regex str pos in
    let en = Str.match_end() in 
    split_regex ((String.sub str st (en - st))::acc) en regex str
  with Not_found -> acc

(** [get_words ch acc] is a list of words, converted to lowercase, in [ch]. *)
let rec get_words ch acc = 
  try 
    let str = input_line ch in 
    let x = (String.lowercase_ascii str |> split_regex [] 0 (Str.regexp "[a-z0-9][^ \n\t\r]*[a-z0-9]\\|[a-z0-9]"))in 
    get_words ch ((List.hd x)::acc)
  with End_of_file -> snd (close_file_helper ch acc)

(** [get_dictionary file] is a list of words in [file]. *)
let get_dictionary file = 
  try get_words (open_in ("dictionaries"^Filename.dir_sep^file)) []
  with Unix_error (a, b, c) -> let _ = print_endline "vat" in raise Not_found 

(** [is_word w file] is [true] if the string [w] is in the dictionary 
    [file] represents. *)
let is_word w file =
  let d = get_dictionary file in  
  List.mem w d

(** [get_init_letters] is the list of letters at the start of the game. *)
let get_init_letters = 
  ['A'; 'A'; 'A'; 'A'; 'A'; 'A'; 'A'; 'A'; 'A';
   'B'; 'B'; 'C'; 'C'; 'D'; 'D'; 'D'; 'D'; 'E'; 
   'E'; 'E'; 'E'; 'E'; 'E'; 
   'E'; 'E'; 'E'; 'E'; 'E'; 'E'; 'F'; 'F'; 'G'; 'G'; 'G'; 'H'; 'H'; 'I'; 
   'I'; 'I'; 'I'; 'I'; 'I'; 'I'; 'I'; 'I'; 'J'; 'K'; 'L'; 'M'; 'M'; 'N'; 
   'N'; 'N'; 'N'; 'N'; 'N'; 'O'; 'O'; 'O'; 'O'; 'O'; 'O'; 'O'; 'O'; 'P'; 
   'P'; 'Q'; 'R'; 'R'; 'R'; 'R'; 'R'; 'R'; 'S'; 'S'; 'S'; 'S'; 'T'; 'T'; 
   'T'; 'T'; 'T'; 'T'; 'U'; 'U'; 'U'; 'U'; 'V'; 'V'; 'W'; 'W'; 'X'; 'Y'; 
   'Y'; 'Z' ]

(** The type representing a word to be placed on a board. *)
type t = {
  word : char list;
  coordinate : int*int;
  dir :bool
}

(** [letter_score c] is the amount of points for a character [c]. *)
let letter_score c = 
  match c with
  | 'E'|'A'|'I'|'O'|'N'|'R'|'T'|'L'|'S'|'U' -> 1
  | 'D'|'G' -> 2
  | 'B'|'C'|'M'|'P' -> 3
  | 'F'|'H'|'V'|'W'|'Y' -> 4
  | 'K' -> 5
  | 'J'|'X' -> 8
  | 'Q'|'Z' -> 10
  | _ -> 0

(** [is_double_word] is [true] if the coordinate is a double word space *)
let is_double_word  = function
  | (1,1)|(2,2)|(3,3)|(4,4)|(1,13)|(2,12)|(3,11)|(4,10)|(13,1)|(12,2)|(11,3)
  |(10,4)|(10,10)|(11,11)|(12,12)|(13,13)|(7,7) -> true
  | _ -> false

(**[is_triple_word] is [true] if the coordinate is a triple word space *)
let is_triple_word  = function
  | (0,0)|(0,7)|(0,14)|(7,0)|(7,14)|(14,0)|(14,7)|(14,14) -> true
  | _ -> false

(** [is_double_letter] is [true] if the coordinate is a double letter space *)
let is_double_letter = function
  | (0,3)|(0,11)|(2,6)|(2,8)|(3,0)|(3,7)|(3,14)|(6,2)|(6,6)|(6,8)
  |(6,12)|(7,3)|(7,11)|(8,2)|(8,6)|(8,8)|(8,12)|(11,0)|(11,7)
  |(11,14)|(12,6)|(12,8)|(14,3)|(14,11) -> true
  | _ -> false

(** [is_triple_letter] is [true] if the coordinate is a triple letter space *)
let is_triple_letter = function
  | (5,1)|(9,1)|(1,5)|(5,5)|(9,5)|(13,5)
  |(1,9)|(5,9)|(9,9)|(13,9)|(5,13)|(9,13) -> true
  | _ -> false

(** [next_coor x dir] is the next coordinate after the coordinate [x], 
    as determined by [dir]. *)
let next_coor x dir =
  if dir then (fst x, snd x + 1) else (fst x + 1, snd x)

(** [next_word_segment w] is the word containing all but the first letter in
    [w], whose coordinate is the coordinate of the second letter of [w]. *)
let next_word_segment w =
  match w.word with
  | [] -> failwith "no letters in w"
  | h::t ->  {word = t; coordinate = next_coor w.coordinate w.dir; dir = w.dir}

(** [tile_score x c] is the score of the letter [c] multiplied accordingly
     if [x] is a double letter or triple letter tile*)
let tile_score x c = 
  if is_double_letter x then 2 * (letter_score c)
  else if is_triple_letter x then 3 * (letter_score c)
  else letter_score c


(** [score_helper acc w] is the score of [w] without considering 
    whole-word multiplier tiles*)

let rec score_helper acc w =
  match w.word with
  | [] -> acc
  | h::t -> score_helper (acc + tile_score w.coordinate h) (next_word_segment w)

(** [word_multiplier x] is [2] if the letter at [x] is a double word tile, [3] if 
     the letter at [x] is a triple word tile, [1] otherwise. *)
let word_multiplier x =
  if is_double_word x then 2
  else if is_triple_word x then 3
  else 1 

(** [multiplier acc w] is the factor by which the score of the word [w] will be 
    multiplied by. *)
let rec multiplier acc w =
  match w.word with
  | [] -> acc
  | h::t -> multiplier (acc * word_multiplier w.coordinate) (next_word_segment w)

(**returns the score of w.word when placed at w.coordinate in w.dir *)
let score w =
  (multiplier 1 w) * (score_helper 0 w)

(** [board_max b] is the tuple (h, v) where h is the maximum width of [b] 
    and v is the maximum height of [b].
    Requires: all lists in [b] have the same length 
    Example: 
    - [board_max b] is (13, 10) when [b] is a 13 x 10 list *)
let board_max b =
  let h = List.length b in
  let v = List.length (List.nth b 0) in
  (h, v)

let board_horizontal_max b = 
  fst (board_max b)

let board_vertical_max b = 
  snd (board_max b)

(** [letter_in_board x b] is [true] if the position [x] is contained within 
    the board [b]. [false] otherwise. *)
let letter_in_board x b =
  (fst x < board_horizontal_max b) && (fst x >= 0) 
  && (snd x < board_vertical_max b) && (snd x >= 0)

(** [word_in_board_helper acc w b] is [true] if the word [w] is contained within 
    the board [b]. [false] otherwise.*)
let rec word_in_board_helper acc w b = 
  match w.word with
  | [] -> acc
  | h :: t -> (word_in_board_helper (letter_in_board w.coordinate b) (next_word_segment w) b)

(** [word_in_board w b] is [true] if the entirety of word [w] is contained within 
    the board [b], i.e. does not run off the board [b]. [false] otherwise. *)
let word_in_board w b = 
  word_in_board_helper true w b

(**[list_index lst pos cnt] returns the element at position [pos] in [lst] *)
let rec list_index lst pos cnt =
  match lst with
  | [] -> None
  | h::t when cnt = pos -> Some h
  | h::t -> list_index t pos (cnt+1)

(**[get_char lst pos] is the character at position [pos] in [lst], '0' if
   [lst] is empty *)
let rec get_char lst pos =
  match list_index lst pos 0 with
  | None -> '0'
  | Some h -> h

(** [flatten x b] is the int value that represents the one-dimensional 
    coordinate of [x] in board [b]. 

    Note that the board [b] is indexed at 0.

    Examples: 
    - [flatten (0,6) b] = [6], where [b] is 15 x 15
    - [flatten (4, 5) b] = [65] ,where [b] is 15 x 15 *)
let flatten_coord (x : int * int) b = 
  board_horizontal_max b * (fst x) + (snd x)

(** [get_char_2d x b] is the character at the coordinate [x] in board [b]. 

     Requires: [x] is a valid coordinate for [b]. *)
let get_char_2d (x : int * int) b=
  let board_flat = List.concat b in
  let x_flat = flatten_coord x b in
  get_char board_flat x_flat

(**[get_string acc s] returns a string made of the characters
   in char list [s]. *)
let rec get_string acc = function
  | [] -> acc
  | h::t -> (String.make 1 h)^(get_string acc t)

(** [playable_letter c x b] is [true] if the letter on the board [b] at 
    coordinate [x] is the same letter as [c] or if it is empty. 
    [false] otherwise. *)
let playable_letter c x b =
  (* design choice: e.g. when do we want to disallow the player from putting 
     down 'a' when 'b' is already there? *)
  let board_letter = get_char_2d x b in 
  (board_letter = '0') || (board_letter = c)

(** [board_letters_helper w b] is the ordered list of letters that are currently 
    on the board [b], starting at position [w.coordinate] and moving in [w.dir]. *)
let rec board_letters_helper acc w b =
  match w.word with
  | [] -> List.filter (fun x -> x <> '0') acc
  | h::t -> 
    board_letters_helper (if playable_letter h w.coordinate b then h::acc else acc ) 
      (next_word_segment w) b

(** [remove_single_element x lst] removes a single instance of [x] in [lst].*)
let rec remove_single_element x lst =
  match lst with
  | [] -> raise Not_found
  | h::t when h = x -> t
  | _::t -> remove_single_element x t

(** [missing_letters_helper w lst] is the list of letters needed to complete the 
    string represented by list [w] given the letters in [lst]. *)
let rec missing_letters_helper (w : 'a list) lst =
  match lst with
  | [] -> w
  | h::t -> 
    if List.mem h w
    then missing_letters_helper (remove_single_element h w) (remove_single_element h lst)
    else missing_letters_helper w (List.filter (fun x -> x <> h) lst)

(** [missing_letters w b] is the list of letters needed to complete [w] in 
    the board [b]. *)
let missing_letters w b =
  let board_letters = board_letters_helper [] w b in
  missing_letters_helper w.word board_letters

(** [enough_letters miss player] is [true] if the contents of the list [miss] 
    are contained in the list [player]. *)
let rec enough_letters miss player =
  match miss with
  | [] -> true
  | h::t ->  
    if List.mem h player 
    then enough_letters t (remove_single_element h player)
    else false

(** [valid_letters w b p] is [true] if the letters in the list [p] could form 
    the word [w] in the board [b]. [false] otherwise. *)
let valid_letters w b p =
  let missing = missing_letters w b in
  enough_letters missing p

(* find the start of that word segment on the board given direction *)
let find_start = 
  (0, 0)

(** [word_at_start w d b acc] is the list of letters starting at [x] and in 
    the direction of [d] in board [b]. Not tail-recursive. *)
let rec word_at_start x d b =
  let letter = get_char_2d x b in
  if letter = '0' then []
  else letter :: word_at_start (next_coor x d) d b

(* not tail recursive yet*)
let rec all_new_words_helper w b  =
  match w.word with
  | [] -> []
  | h :: t -> (get_string "" (word_at_start w.coordinate (not (w.dir)) b)) :: all_new_words_helper (next_word_segment w) b 

(** [all_new_words w b p] is the list of all words placed in [b] with [w] 
    and [p], which may or may not be valid words. 

    Example:
                                 S
            U S                  U S
                                 R
    P O T A T O          P O T A T O
              N    ->            A N 
              E                  X E   
    gives a list with the same contents as ["S"; "SURTAX"; "US"; "R"; "POTATO"; "AN"; "XE"]. 

    (* more examples to come*)


    Requires: placing word [w] in [b] is a valid move. *)
let rec all_new_words w b =
  let lst = all_new_words_helper w b in
  get_string "" (word_at_start w.coordinate w.dir b) :: lst

(** [valid_new_words] is [true] if each new letter connects a valid word together. 
    Example:
    P O T A T O          P O T A T O
          N    ->            A N   is [true], as "TAB", "AN", and "BE" are valid.
          E                  B E   
    Requires: placing word [w] in [b] is a valid move.
*)
let valid_new_words w b p file =
  let words = all_new_words w b in
  let rec check acc ws = 
    match ws with
    | [] -> acc
    | h::t -> (List.mem h file) && check true t in
  check true words

(** [verify_word w b p file] is [true] if the word [w] is valid word in board [b] 
    with dictionary [file] given that the player has the letters in [p]. [false] otherwise. *)
let verify_word w b p file = 
  word_in_board w b && (List.mem (get_string "" w.word) file) 
  && (valid_letters w b p) && (valid_new_words w b p file)

(**[row_helper wstring len start r cnt] is the [r] with word [wstring]
   inserted at position [start] *)
let rec row_helper wstring len start r cnt = 
  match r with
  | [] -> r
  | h::t when cnt < (start + len) && cnt >= start -> 
    (String.get wstring (cnt - start))::(row_helper wstring len start t (cnt + 1))
  | h::t -> h::(row_helper wstring len start t (cnt + 1))

(**[place horizontal b w cnt] is board [b] with word [w] placed in it. Used 
   when [w] is a horizontal word.  *)
let rec place_horizontal b w cnt =
  match b with
  | [] -> []
  | h::t when cnt = (fst w.coordinate) -> 
    (row_helper (get_string "" w.word) (List.length w.word) 
       (snd w.coordinate) h 0)::t
  | h::t -> h::(place_horizontal t w (cnt + 1))

(**[vertical_helper row pos c cnt] returns a row with the character at 
   position [pos] changed to [c] *)
let rec vertical_helper row pos c cnt =
  match row with 
  | [] -> []
  | h::t when cnt = pos -> c::t
  | h::t -> h::(vertical_helper t pos c (cnt + 1))

(**[place vertical b w] returns a board with [w] placed in it. *)
let rec place_vertical b w length cnt =
  match b with
  | [] -> []
  | h::t when cnt >= (fst w.coordinate) && cnt < (fst w.coordinate) + length ->
    (vertical_helper h (snd w.coordinate) 
       (String.get  (get_string "" w.word) 
          (cnt - (fst w.coordinate))) 0)::(place_vertical t w length (cnt+1))
  | h::t -> h::(place_vertical t w length (cnt+1))

(**[get_vertical_str b pos] returns the vertical string of characters in [b] 
   whose y coordinate is [pos]*)
let rec get_vertical_str b pos =
  match b with 
  | [] -> ""
  | h::t -> let c = list_index h pos 0 in match c with
    | None -> ""
    | Some x -> (String.make 1 x)^(get_vertical_str t pos)

(**[substr_helper str] returns a substring of [str] with any leading 0's
   removed *)
let rec substr_helper str =
  match str with
  | "" -> ""
  | c -> if (String.get c 0) == '0' 
    then substr_helper (String.sub str 1 (String.length str - 1))
    else str

(**[index_helper str] returns the index of '0' in [str] and the length
   of [str] if there is no '0'. *)
let rec index_helper str =
  try String.index str '0' with
  | _ -> String.length str


(**[substr_with_c str c pos bm] returns a tuple whose first element is the 
   substring in [str] that contains c and no 0's and whose second element 
   is the position of the first letter in the word containing [c] in [str]*)
let rec substr_with_c str c pos bm =
  let x = substr_helper str in
  let y = String.sub x 0 (index_helper x) in
  if (String.contains y c && (pos >= bm - String.length x) && 
      (pos < bm - String.length x + String.length y)) || String.length y != 1 
  then (y, bm - String.length x) 
  else substr_with_c (String.sub x (index_helper x) 
                        (String.length x - String.length y)) c pos bm


(**[get_list s acc] is a char list of all the characters in [s] *)
let rec get_list s acc =
  match s with
  | "" -> acc
  | s -> let last = String.length s -1 in get_list (String.sub s 0 last) 
      ((String.get s ((String.length s) - 1))::acc)

(**[get_substrs str acc n b] is a list of words made of the substrs in
   [str]. [n] is the coordinate that is not changing (i.e. first coordinate for
   horizontal strings) and [b] is the direction of the word.*)
let rec get_substrs str acc n b bm =
  match str with
  | "" -> acc
  | s -> let x = substr_helper str in
    let i = index_helper x in
    let y = String.sub x 0 (i) in
    if String.length y >=2 then if b 
      then get_substrs (String.sub x i (String.length x - String.length y)) 
          ({word = get_list y []; coordinate = (n,bm - String.length x); 
            dir = b}::acc) n b bm
      else get_substrs (String.sub x i (String.length x - String.length y)) 
          ({word = get_list y []; coordinate = (bm - String.length x,n); 
            dir = b}::acc) n b bm
    else get_substrs (String.sub x i (String.length x - String.length y)) acc n b bm

(**[present_words_v b cnt acc] is a list of the vertical words present in [b] *)
let rec present_words_v b cnt acc =
  if cnt = 0 then (get_substrs (get_vertical_str b cnt) [] 
                     cnt false (board_horizontal_max b))@acc
  else present_words_v b (cnt - 1) ((get_substrs (get_vertical_str b cnt) [] 
                                       cnt false (board_horizontal_max b))@acc)


(**[vertical_word_score b coor c] returns the score of the vertical word that
   contains character [c] at coordinate [coor]. If [c] is not in a word (i.e.
   it's not directly below or above any other letters), returns 0)
*)
let vertical_word_score b coor c words=
  let s = get_vertical_str b (snd coor) in
  let tup = substr_with_c s c (snd coor) (board_vertical_max b) in 
  let word = {word = get_list (fst tup) []; coordinate = (snd tup, snd coor); 
              dir = false} in
  if String.length (fst tup) = 1 || List.mem word words then 0 else score word

(**[check_for_v_words row start finish cnt b] is the score of all vertical
   words that contain letters from [start] to [finish] in [row] *)
let rec check_for_v_words row start finish cnt b words =
  match row with
  | [] -> 0
  | h::t when cnt < (snd finish) && cnt >= snd start -> 
    (vertical_word_score b (fst start, cnt) h words) 
    + (check_for_v_words t start finish (cnt+1) b words)
  | h::t -> check_for_v_words t start finish (cnt+1) b words

(**[score_horizontal b w cnt] returns the score created by placing [w] in [b] *)
let rec score_horizontal b ori_b w cnt words=
  match b with
  | [] -> 0
  | h::t when cnt = (fst w.coordinate) -> 
    check_for_v_words h w.coordinate (fst w.coordinate, 
                                      (snd w.coordinate) + List.length w.word)
      0 ori_b words
  | h::t -> score_horizontal t ori_b w (cnt + 1) words

(**[present_words_h b cnt acc] is the list of horizontal words in b *)
let rec present_words_h b cnt acc =
  match b with
  | [] -> acc
  | h::t -> present_words_h t (cnt+1) 
              (get_substrs (get_string "" h) [] cnt true (board_vertical_max b))@acc

(**[horizontal_word_score row c coor words] gives the score of the word in [row]
   containing character [c] at coordinate [coor]. If [c] is not directly to 
   the right or left of any characters, returns 0.*)
let horizontal_word_score row c coor words=
  let x = substr_with_c (get_string "" row) c (snd coor) (List.length row) in
  let word = {word = get_list (fst x) []; coordinate = (fst coor, snd x); 
              dir = true} in
  if String.length (fst x) == 1 || List.mem word words then 0 else score word

(**[score_vertical b w cnt words] returns the score created by placing [w] 
   in [b]*)
let rec score_vertical b w cnt words=
  if cnt = 14 then 0 else
    match b with
    | [] -> 0
    | h::t when cnt >= (fst w.coordinate)  && 
                cnt < (fst w.coordinate + (List.length w.word)) -> 
      (horizontal_word_score h (get_char w.word (cnt - (fst w.coordinate))) 
         (cnt, snd w.coordinate) words)+(score_vertical t w (cnt+1) words)
    | h::t -> score_vertical t w (cnt+1) words

(**[place word coor dir b] returns a tuple with [b] after placing word
   [word] in [b] at position [coor] in [dir] and the score of that word. 
   Example:
   [place "HELLO" (7,7) true b], where [b] is a 15x15 empty board, is [((a 
   board with the middle row 
   ['0';'0';'0';'0';'0';'0';'0';'H';'E';'L';'L';'O';'0';'0';'0']),18)]*)
let place word coord dir b =
  let w = {word = word; coordinate = coord; dir = dir} in 
  (*if verify_word w b then*) if w.dir 
  then let newboard = place_horizontal b w 0 in 
    (newboard,score w + score_horizontal newboard newboard w 0 
                (present_words_v b (board_horizontal_max b) []))
  else let newboard = place_vertical b w (List.length w.word) 0 in 
    (newboard,score w + score_vertical newboard w 0 (present_words_h b 0 []))
(*else failwith "ok"*)

(** [create_word w coord dir] is a word with the word represented by [w], the beginning coordinate [coord], and the direction [dir]. *)
let create_word word coord dir = {word = word; coordinate = coord; dir = dir}