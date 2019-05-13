type t = 
  {
    word : char list;
    coordinate : int*int;
    dir : bool
  }

(** [get_init_letters] is the list of letters at the start of the game. *)
val get_init_letters: char list

(** [get_dictionary file] is a list of words in [file]. *)
val get_dictionary: string -> string list 

(** [is_double_word] is [true] if the coordinate is a double word space *)
val is_double_word: int*int -> bool 

(**[is_triple_word] is [true] if the coordinate is a triple word space *)
val is_triple_word: int*int -> bool 

(** [is_triple_letter] is [true] if the coordinate is a triple letter space *)
val is_double_letter: int*int -> bool 

(** [is_triple_letter] is [true] if the coordinate is a triple letter space *)
val is_triple_letter: int*int -> bool 

(** [word_in_board w b] is [true] if the entirety of word [w] is contained within 
    the board [b], i.e. does not run off the board [b]. [false] otherwise. *)
val word_in_board: t -> (char list) list -> bool

(** [get_char_2d x b] is the character at the coordinate [x] in board [b]. 

     Requires: [x] is a valid coordinate for [b]. *)
val get_char_2d: int*int -> (char list) list -> char

(** [playable_letter c x b] is [true] if the letter on the board [b] at 
    coordinate [x] is the same letter as [c] or if it is empty. 
    [false] otherwise. *)
val playable_letter: char -> int * int -> (char list) list -> bool

(** [missing_letters w b] is the list of letters needed to complete [w] in the board [b]. *)
val missing_letters: t -> (char list) list -> char list 

val all_new_words :  t -> (char list) list -> string list

(** [verify_word w b p file] is [true] if the word [w] is valid word in board [b] with dictionary [file] given that the player has the letters in [p]. [false] otherwise. *)
val verify_word: t -> (char list) list -> char list -> string list -> bool

(**[place word coord dir b] returns a tuple with a player's updated score and the updated board.
   If [w] will not go in board, it returns the score and board unchanged. 

   Requires: playing a word with string [w], tuple [coord], and direction [dir] in the board [b] is a valid move. *)
val place: char list -> int*int -> bool -> (char list) list -> (char list) list * int

(** [create_word w coord dir] is a word with the word represented by [w], the beginning coordinate [coord], and the direction [dir]. *)
val create_word: char list -> int*int -> bool -> t