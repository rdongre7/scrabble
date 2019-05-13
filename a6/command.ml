(** [word] is a word put in by a player broken down into characters.*)
type word = char list

(** [command] is all the possible commands that people can call
    while they are playing the game. *)
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

let rec split_str acc = function
  | "" -> acc
  | str -> split_str ((Char.uppercase_ascii(String.get str 0)::acc)) 
             (String.sub str 1 ((String.length str)-1))

(** [word_comm lst] handles the the [Word _] case to produce [Word of char list].

    Examples: 
    - [word_comm ["stuff"]] is [Word ['S';'T';'U';'F';'F']]
    - [word_comm []] is [Malfomed]

    Requires: [lst] to be a list of objects strings 
    Raises: [Malformed] if [lst] is the empty.*)
let word_comm lst = 
  match lst with
  | [] -> raise Malformed
  | _ -> Word (List.rev(split_str [] (List.fold_left (^) "" lst)))

(** [parse str] parses a player's input into a [command].
    It gets converted to either [Word _], [Quit], or [Score]. 

    Examples: 
    - [parse "word stuff"] is [Word ['S';'T';'U';'F';'F']]
    - [parse ""] is [Raise Empty]
    - [parse "quit no"] is [Raise Malformed]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is not valid or does not match
    any of the commands.*)
let parse str = 
  let lst_str = List.filter ((<>) "") ( String.split_on_char ' ' str) in
  match lst_str with
  | [] -> raise Empty
  | h::t -> match h with 
    | "quit" -> if t=[] then Quit else raise Malformed
    | "word" -> if t <> [] then (word_comm t) else raise Malformed
    | "score" -> if t=[] then Score else raise Malformed
    | "extrahint" -> if t=[] then ExtraHint else raise Malformed 
    | "hint" -> if t=[] then Hint else raise Malformed 
    | _ -> raise Malformed