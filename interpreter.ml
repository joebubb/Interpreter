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

let str_regex = regexp "\"[a-zA-z]*\"[ ]*$"
let int_regex = regexp "-?[0-9]+[ ]*$"

let str_is_str s = string_match str_regex s 0 
let str_is_int s = string_match int_regex s 0 

let str_is_bool s = (s="0" || s="1")

(*Writing a line to a file*)
let write_file_example (file_path: string) : unit =
  let fp = open_out file_path in
  let () = Printf.fprintf fp "writing this line!" in
    close_out fp

let write_nothing = () 

let (|>) a f = f a 

let rec map f ls = 
  match ls with 
  | [] -> [] 
  | h::t -> (f h)::(map f t)

type const = 
  Int of string 
  | String of string 
  | Undefined

let extract_const c = 
  match c with 
  | Undefined -> ""
  | Int(c) -> c 
  | String(c) -> c 

let rec str_of_stack stack = 
  match stack with 
  | [] -> ""
  | h::t -> (extract_const h) ^ "\n" ^ (str_of_stack t)

let write_stack stack output = 
  let s = str_of_stack stack in 
  let fp = open_out output in 
  let _ = Printf.fprintf fp "%s" s in 
  close_out fp
let write_err output = 
  let fp = open_out output in 
  let _ = Printf.fprintf fp "\"Error\"" in 
  close_out fp

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
  | SoftCrash
  | And 
  | Or 

type status = Ok | Err 

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

let and_constr o = 
  match o with 
  | None -> And
  | Some(o) -> Crash

let or_constr o = 
  match o with 
  | None -> Or 
  | Some(o) -> Crash 

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
  | "And" -> and_constr
  | "Or" -> or_constr
  | s -> crash 

let const_of_str s = 
  if str_is_str s then 
    String(s)
  else if str_is_int s then 
    Int(s)
  else
    Undefined

let next ls = 
  match ls with 
  | [] -> None 
  | h::t -> Some(const_of_str h)

let split_by_newline s = split (regexp "[\n]+") s

let split_src_to_com_strings src = split_by_newline src 
   
let split_by_spaces str = split (regexp "[ ]+") str

let rec map_com_strings_to_coms str_ls = 
  match str_ls with 
  | [] -> [SoftCrash]
  | h::t -> 
    match h with 
    | [] -> Crash :: map_com_strings_to_coms t 
    | name::args -> ((str_to_com_constructor name) (next args))::(map_com_strings_to_coms t) 

let concat_strings s1 s2 = 
  let r = regexp "[\"]" in 
  let f = global_replace r "" s1 in 
  let s = global_replace r "" s2 in 
  "\"" ^ f ^ s ^ "\""

let rec stack_len stack = 
  match stack with 
  | [] -> 0 
  | h::t -> 1 + (stack_len t)

let top_two stack = 
  let first = List.nth stack 0 in 
  let second = List.nth stack 1 in 
  (first, second)

let stack_without_top_two stack = 
  let t = List.tl stack in 
  List.tl t 

let both_ints f s = 
  match f with 
  | String(c) -> false 
  | Undefined -> false
  | Int(c) -> match s with 
              | String(c) -> false
              | Undefined -> false
              | Int(c) -> true 

let both_strs f s = 
  match f with 
  | Int(c) -> false 
  | Undefined -> false
  | String(c) -> match s with 
              | Int(c) -> false
              | Undefined -> false
              | String(c) -> true 

let both_bools f s = 
  let a = extract_const f in 
  let b = extract_const s in 
  (str_is_bool a) && (str_is_bool b)

let push_to_stack c stack = (Ok, c :: stack)
let pop_from_stack stack = 
  match stack with 
  | [] -> (Err, [])
  | h::t -> (Ok, t)

let add_with_stack stack = 
  if stack_len stack < 2 then (Err, stack) else 
  match top_two stack with 
  | (f, s) -> if both_ints f s then 
                let i1 = (int_of_string (extract_const f)) in 
                let i2 = (int_of_string (extract_const s)) in 
                let sum = i1 + i2 in 
                (Ok, (Int(string_of_int sum))::(stack_without_top_two stack))
              else
                (Err, stack)

let sub_with_stack stack = 
  if stack_len stack < 2 then (Err, stack) else 
    match top_two stack with 
    | (f, s) -> if both_ints f s then 
                  let i1 = (int_of_string (extract_const f)) in 
                  let i2 = (int_of_string (extract_const s)) in 
                  let diff = i1 - i2 in 
                  (Ok, (Int(string_of_int diff))::(stack_without_top_two stack))
                else
                  (Err, stack)

let mul_with_stack stack = 
  if stack_len stack < 2 then (Err, stack) else 
    match top_two stack with 
    | (f, s) -> if both_ints f s then 
                  let i1 = (int_of_string (extract_const f)) in 
                  let i2 = (int_of_string (extract_const s)) in 
                  let prod = i1 * i2 in 
                  (Ok, (Int(string_of_int prod))::(stack_without_top_two stack))
                else
                  (Err, stack)

let div_with_stack stack = 
  if stack_len stack < 2 then (Err, stack) else 
    match top_two stack with 
    | (f, s) -> if (both_ints f s) && (not ((int_of_string (extract_const s))=0)) then 
                  let i1 = (int_of_string (extract_const f)) in 
                  let i2 = (int_of_string (extract_const s)) in 
                  let quo = i1 / i2 in 
                  (Ok, (Int(string_of_int quo))::(stack_without_top_two stack))
                else
                  (Err, stack)

let swap_stack stack = 
  if stack_len stack < 2 then (Err, stack) else 
    match top_two stack with 
    | (f, s) -> (Ok, s::f::(stack_without_top_two stack))   
  
let neg_stack stack = 
  if stack_len stack < 1 then (Err, stack) else 
    let top = List.nth stack 0 in 
    if (str_is_int (extract_const top)) then 
      let num = -1 * (int_of_string (extract_const top)) in 
      let bottom = List.tl stack in 
      let new_const = Int(string_of_int num) in
      (Ok, new_const::bottom)
    else 
      (Err, stack)

let concat_stack stack = 
  if stack_len stack < 2 then (Err, stack) else 
    match top_two stack with 
    | (f, s) -> if both_strs f s then 
      let s1 = extract_const f in 
      let s2 = extract_const s in 
      let ans = concat_strings s1 s2 in 
      (Ok, String(ans)::(List.tl (List.tl stack)))
    else
      (Err, stack) 


let and_stack stack = 
  if stack_len stack < 2 then (Err, stack) else 
    match top_two stack with 
    | (f, s) -> 
      if both_bools f s then 
        if (extract_const f = "1") && (extract_const s = "1") then 
          (Ok, Int("1")::(List.tl (List.tl stack)))
        else
          (Ok, Int("0")::(List.tl (List.tl stack))) 
      else 
        (Err, stack)


let rec interpret stack output comms = 
  match comms with 
  | [] -> write_stack stack output
  | h::t -> 
    match h with
    | Push(c) -> let result = push_to_stack c stack in process_result result output t
    | Pop -> let result = pop_from_stack stack in process_result result output t 
    | Add -> let result = add_with_stack stack in process_result result output t 
    | Sub -> let result = sub_with_stack stack in process_result result output t 
    | Mul -> let result = mul_with_stack stack in process_result result output t 
    | Div -> let result = div_with_stack stack in process_result result output t 
    | Swap -> let result = swap_stack stack in process_result result output t
    | Neg -> let result = neg_stack stack in process_result result output t  
    | Concat -> let result = concat_stack stack in process_result result output t 
    | Quit -> write_stack stack output 
    | Crash -> write_err output 
    | SoftCrash -> write_nothing 
    | And -> let result = and_stack stack in process_result result output t

and process_result r output comms = 
    match r with 
    | (Err, s) -> write_err output
    | (Ok, s) -> interpret s output comms

(*When it comes to parsing src, you should find it useful that fold_left can be
  defined and used with strings (like String.fold_left in new OCaml version).
  See String.get. These are suggestions though and you are welcome
  to use what you want :)  *)
let interpreter (src : string) (output_file_path: string): unit =
  src
  |> split_by_newline
  |> (map split_by_spaces)
  |> map_com_strings_to_coms 
  |> (interpret [] output_file_path)
