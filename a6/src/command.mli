(** [word] is the word put in by the user broken down into characters. *)
type word = char list

(** [command] is the possible command that a user can use in the game. *)
type command = 
  | Word of word
  | Score
  | Hint
  | Quit
  | ExtraHint

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command].
    It gets converted to either [Word _], [Quit], or [Score].
    Examples: 
    - [parse "word stuff"] is [Word ['S';'T';'U';'F';'F']]
    - [parse ""] is [Raise Empty]
    - [parse "quit no"] is [Raise Malformed]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] is raise when a string is parsed and it
    does not match any of [command]'s.*)
val parse : string -> command
