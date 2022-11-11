(* Honor code comes here:

   First Name: Joseph
   Last Name: Bubb
   BU ID: U91516134

   I pledge that this program represents my own program code and that I have
   coded on my own. I received help from no one in designing and debugging my
   program. I have read the course syllabus of CS 320 and have read the sections
   on Collaboration and Academic Misconduct. I also understand that I may be
   asked to meet the instructor or the TF for a follow up interview on Zoom. I
   may be asked to explain my solution in person and may also ask you to solve a
   related problem. *)

(*NOTE: There are no restrictions on what you can use*)
open Str 


(*Writing a line to a file*)
let write_file_example (file_path: string) : unit =
  let fp = open_out file_path in
  let () = Printf.fprintf fp "writing this line!" in
    close_out fp

let (|>) a f = f a 

let rec map f ls = 
  match ls with 
  | [] -> [] 
  | h::t -> (f h)::(map f t)

type const = 
  Int of string 
  | String of string 

type com = 
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

let push c = 
  match c with 
  | None -> Crash 
  | Some(c) -> Push(c)
  
let pop o = 
  match o with 
  | None -> Pop 
  | Some(o) -> Crash

let add o = 
  match o with 
  | None -> Add 
  | Some(o) -> Crash 

let sub o = 
  match o with 
  | None -> Sub 
  | Some(o) -> Crash 

let mul o = 
  match o with 
  | None -> Mul 
  | Some(o) -> Crash 

let div o = 
  match o with 
  | None -> Div 
  | Some(o) -> Crash 
  
let swap o = 
  match o with 
  | None -> Swap 
  | Some(o) -> Crash 

let neg o = 
  match o with 
  | None -> Neg 
  | Some(o) -> Crash 
  
let concat o = 
  match o with 
  | None -> Concat
  | Some(o) -> Crash 

let quit o = 
  match o with 
  | None -> Quit 
  | Some(o) -> Crash  

let crash o = 
  Crash
  
let str_to_com_constructor s = 
  match s with 
  | "Push" -> push 
  | "Pop" -> pop 
  | "Add" -> add 
  | "Sub" -> sub 
  | "Mul" -> mul 
  | "Div" -> div 
  | "Swap" -> swap 
  | "Neg" -> neg 
  | "Concat" -> concat
  | "Quit" -> quit 
  | s -> crash 

let next ls = 
  match ls with 
  | [] -> None 
  | h::t -> Some(h)

let split_src_to_com_strings src = split (regexp "[\n]+") src 

let trim_all ls = 
  map (global_replace (regexp "[ ]+") "") ls
   

let rec map_com_strings_to_coms str_ls = 
  match str_ls with 
  | [] -> [Crash]
  | h::t -> 
    match h with 
    | [] -> Crash :: map_com_strings_to_coms t 
    | name::args -> ((str_to_com_constructor name) (next args))::(map_com_strings_to_coms t) 

(*When it comes to parsing src, you should find it useful that fold_left can be
  defined and used with strings (like String.fold_left in new OCaml version).
  See String.get. These are suggestions though and you are welcome
  to use what you want :)  *)
let interpreter (src : string) (output_file_path: string): unit =
  (* Split the string by lines *)
  let stack = [] in 
  let coms = 
    split (regexp "[\n]+") src
    |> trim_all 
    |> map_com_strings_to_coms 
    (* |> (interpret stack output_file_path) *)
  in () 
  