let rec list_len (l: 'a list) : int = 
  match l with 
  | [] -> 0 
  | h::t -> 1 + list_len t 

let rec prog_str_list_after_name (l: string list list) (name : string) : (string list list) = 
  match l with 
  | [] -> []
  | h :: t -> match h with 
              | [] -> prog_str_list_after_name t name 
              | n :: args -> if n=name then t else prog_str_list_after_name t name
 
(* TYPE DEFINITIONS -----------------------------------------------------------------------------*)

type const = 
  Int of string 
  | String of string 
  | Undefined

type command = 
  Push of const 
  | Pop 
  | Add 
  | Sub 
  | Mul 
  | Div
  | Swap
  | Neg 
  | Concat 
  | Quit
  | Crash
  | SoftCrash
  | And 
  | Or 
  | BeginEnd of command list 

type status = Ok | Err 

type var = string * const 

let (|>) a f = f a 

(* REGEX ----------------------------------------------------------------------------------------*)
let str_regex = Str.regexp "\"[a-zA-z]*\"[ ]*$"
let int_regex = Str.regexp "-?[0-9]+[ ]*$"

let str_is_str (s : string) : bool = Str.string_match str_regex s 0 
let str_is_int (s: string) : bool = Str.string_match int_regex s 0 

let const_of_str (s: string) = 
  if str_is_str s then 
    String(s)
  else if str_is_int s then 
    Int(s)
  else
    Undefined

let str_is_bool (s : string) : bool = (s="0" || s="1")

let rec map (f: 'a->'b) (ls: 'a list): 'b list = 
  match ls with 
  | [] -> [] 
  | h::t -> (f h)::(map f t)

(* COMMAND CONSTRUCTORS -------------------------------------------------------------------------*)
let push_constructor (args : string list) : command = 
  match list_len args with 
  | 1 -> Push((const_of_str (List.hd args)))
  | n -> Crash

let pop_constructor (args : string list) : command = 
  match list_len args with 
  | 0 -> Pop
  | n -> Crash

let add_constructor (args : string list) : command = 
  match list_len args with 
  | 0 -> Add
  | n -> Crash

let sub_constructor (args : string list) : command = 
  match list_len args with 
  | 0 -> Sub
  | n -> Crash

let mul_constructor (args : string list) : command = 
  match list_len args with 
  | 0 -> Mul
  | n -> Crash  

let div_constructor (args : string list) : command = 
  match list_len args with 
  | 0 -> Div
  | n -> Crash  

let swap_constructor (args : string list) : command = 
  match list_len args with 
  | 0 -> Swap
  | n -> Crash

let neg_constructor (args : string list) : command = 
  match list_len args with 
  | 0 -> Neg
  | n -> Crash

let concat_constructor (args : string list) : command = 
  match list_len args with 
  | 0 -> Concat
  | n -> Crash

let quit_constructor (args : string list) : command = 
  match list_len args with 
  | 0 -> Quit
  | n -> Crash

let and_constructor (args : string list) : command = 
  match list_len args with 
  | 0 -> And
  | n -> Crash
 
let or_constructor (args : string list) : command = 
  match list_len args with 
  | 0 -> Or
  | n -> Crash  

let crash_constructor (args : string list) : command = 
  Crash

let str_to_com_constructor (str: string) : (string list -> command) = 
  match str with 
  | "Push" -> push_constructor 
  | "Pop" -> pop_constructor 
  | "Add" -> add_constructor 
  | "Sub" -> sub_constructor 
  | "Mul" -> mul_constructor 
  | "Div" -> div_constructor 
  | "Swap" -> swap_constructor 
  | "Neg" -> neg_constructor 
  | "Concat" -> concat_constructor
  | "Quit" -> quit_constructor 
  | "And" -> and_constructor
  | "Or" -> or_constructor
  | s -> crash_constructor  

let rec map_strs_to_prog (strs: string list list) : command list = 
  match strs with 
  | [] -> [SoftCrash]
  | h :: t -> 
    match h with 
    | [] -> Crash :: map_strs_to_prog t 
    | name :: args -> 
      match name with 
      | "Begin" -> BeginEnd(code_block_until_end_com (List.tl strs))::(map_strs_to_prog (prog_str_list_after_name t "End"))
      | name -> let constructor = str_to_com_constructor name in (constructor args)::(map_strs_to_prog t)
and code_block_until_end_com (strs: string list list): command list =
  match strs with 
  | [] -> [Crash]
  | h :: t -> 
    match h with 
    | [] -> [Crash]
    | name :: args -> 
      match name with 
      | "End" -> [] 
      | "Begin" -> BeginEnd(code_block_until_end_com (List.tl strs))::(code_block_until_end_com (prog_str_list_after_name t "End"))
      | name -> let constructor = str_to_com_constructor name in (constructor args)::(code_block_until_end_com t)

let extract_str_from_const (c : const) : string = 
  match c with 
  | Undefined -> ""
  | Int(c) -> c 
  | String(c) -> c

let split_by_newline (s: string): string list = Str.split (Str.regexp "[\n]+") s
let split_by_spaces (str: string) = Str.split (Str.regexp "[ ]+") str

let interpreter (src : string) (output_file_path: string): unit =
  let _ = src 
  |> split_by_newline
  |> (map split_by_spaces)
  in ()