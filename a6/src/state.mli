(**  This abstract type represents a dynamic state after each round of the game.*)
type t 

type letter

(**  [result] is all possible outcomes of each round.*)
type result =  Legal of t | Finished | Illegal 

(** [init_state s] is the initial state of the game. 
    The bag of letters and a rack of tiles are initialized. *)
val init_state : string -> t

(** [get_board s] is the current board with words. *)
val get_board: t -> (char list) list 

(** [get_letters s] is the letters that the player has on the rack. *)
val get_letters: t -> char list 

(** [get_score s] a tuple with both player's and AI's current scores. *)
val get_score: t -> int * int

(** [place_word s lst coor dir] places the letter that the player
    provided on the board. *)
val place_word: t -> char list -> int*int -> bool -> int -> result 

(** [get_dict_length st] is the length of the dictionary in state.*)
val get_dict_length: t -> int

(** [get_list board] gets the list of letters that could be placed by AI on the board. *)
val get_list: (char list) list -> letter list 

(** [get_hints st turn] forms the hint for the user
    with just the word. It is the first instance of the word. *)
val get_hints: t -> int ->  string option

(** [replace_letters_2 st turn] is the new state that replaces
    the words in ai's rack if there are no words that can be placed on the 
    board. The entire state is updated since the inventory is new.*)
val replace_letters_2: t -> int -> result

(** [get_ai_game st turn] sets the game of AI against AI
    in a step by step fashion. *)
val get_ai_game: t -> int -> (char list * (bool * (int * int))) option

(** [get_bag st] is the current bag of words. *)
val get_bag: t -> char list

(** [get_hints st turn] forms the hint for the user
    with the word and coordinates. It is the first instance of the word. *)
val get_hint_coord: t -> int -> (string * (int * int)) option