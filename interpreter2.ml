(* Misc ---------------------------------------------------------------------------------------- *)
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

let not (b: bool) = if b then false else true 

let rec get_from_dict (d: ('a * 'b) list) (key: 'a): 'b option = 
  match d with 
  | [] -> None 
  | h::t -> 
    match h with 
    (k, v) -> if key=k then Some(v) else get_from_dict t key
 
(* TYPE DEFINITIONS ---------------------------------------------------------------------------- *)

type const = 
  Int of string 
  | String of string 
  | Undefined

type command = 
  Push of string
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
  | Not 
  | Equal
  | Lte
  | Local of string
  | Global of string
  | BeginEnd of command list 
  | IfElse of command list * command list

type status = Ok | Err 
type var = string * const 
type env = const list * var list * var list * command list * string (* Stack, Globals, Locals, program, output *)
type result = status * env 

let new_env (stack: const list) (global_vars: var list) (local_vars: var list) (program: command list) (output: string): env = 
  (stack, global_vars, local_vars, program, output)

let new_local_env (parent_env: env) = 
  match parent_env with 
  | (stk, glb, loc, p, o) -> new_env [] glb [] p o

let (|>) a f = f a 

let get_local (n: string) (e: env): const option = 
  match e with (s, g, l, p, o) -> 
  get_from_dict l n 

let get_global (n: string) (e: env): const option = 
  match e with (s, g, l, p, o) -> 
    get_from_dict g n

let get_var (n: string) (e: env): const = 
  let o = get_local n e in 
  match o with 
  | Some(o) -> o 
  | None -> let o = get_global n e in 
    match o with 
    | Some(o) -> o 
    | None -> Undefined

(* REGEX --------------------------------------------------------------------------------------- *)
let str_regex = Str.regexp "\"[a-zA-z]*\"[ ]*$"
let int_regex = Str.regexp "-?[0-9]+[ ]*$"
let name_regex = Str.regexp "[a-z]+[a-zA-z0-9_]*$"

let str_is_str (s : string) : bool = Str.string_match str_regex s 0 
let str_is_int (s: string) : bool = Str.string_match int_regex s 0 
let str_is_name (s: string) = Str.string_match name_regex s 0

let str_is_bool (s : string) : bool = (s="0" || s="1")

let extract_str_from_const (c: const): string = 
  match c with 
  | Undefined -> ""
  | Int(s) -> s
  | String(s) -> s

let rec all_ints (l: const list): bool = 
  match l with 
  | [] -> true 
  | h::t -> 
    let s = extract_str_from_const h in 
    (str_is_int s) && (all_ints t)

let all_strings (l: const list): bool = 
  match l with 
  | [] -> true 
  | h::t -> 
    let s = extract_str_from_const h in 
    (str_is_str s) && (all_ints t)

let rec all_bools (l: const list): bool = 
  match l with 
  | [] -> true 
  | h::t -> 
    let s = extract_str_from_const h in 
    (s="0"||s="1") && all_bools t

let concat_strings s1 s2 = 
  let r = Str.regexp "[\"]" in 
  let f = Str.global_replace r "" s1 in 
  let s = Str.global_replace r "" s2 in 
  "\"" ^ f ^ s ^ "\""

let rec map (f: 'a->'b) (ls: 'a list): 'b list = 
  match ls with 
  | [] -> [] 
  | h::t -> (f h)::(map f t)

(* Output -------------------------------------------------------------------------------------- *)
let rec str_of_stack stack = 
  match stack with 
  | [] -> ""
  | h::t -> (extract_str_from_const h) ^ "\n" ^ (str_of_stack t)

let write_stack stack output = 
  let s = str_of_stack stack in 
  let fp = open_out output in 
  let _ = Printf.fprintf fp "%s" s in 
  close_out fp

let write_err output = 
  let fp = open_out output in 
  let _ = Printf.fprintf fp "\"Error\"" in 
  close_out fp

(* COMMAND CONSTRUCTORS ------------------------------------------------------------------------ *)
let push_constructor (args : string list) : command = 
  match list_len args with 
  | 1 -> Push(List.hd args)
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

let not_constructor (args: string list): command = 
  match list_len args with 
  | 0 -> Not 
  | n -> Crash 

let equal_constructor (args: string list): command = 
  match list_len args with 
  | 0 -> Equal
  | n -> Crash 

let lte_constructor (args: string list): command = 
  match list_len args with 
  | 0 -> Lte
  | n -> Crash 

let local_constructor (args: string list): command = 
  match list_len args with 
  | 1 -> Local(List.hd args)
  | n -> Crash

let global_constructor (args: string list): command = 
  match list_len args with 
  | 1 -> Global(List.hd args)
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
  | "Not" -> not_constructor
  | "Equal" -> equal_constructor
  | "Lte" -> lte_constructor
  | "Local" -> local_constructor
  | "Global" -> global_constructor
  | s -> crash_constructor  

let rec map_strs_to_prog (strs: string list list) : command list = 
  match strs with 
  | [] -> [SoftCrash]
  | h :: t -> 
    match h with 
    | [] -> Crash :: map_strs_to_prog t 
    | name :: args -> 
      match name with 
      | "Begin" -> BeginEnd(code_block_until_com (List.tl strs) "End")::(map_strs_to_prog (prog_str_list_after_name t "End"))
      | "IfThen" -> 
        let block1 = code_block_until_com (List.tl strs)  "Else" in 
        let block2 = code_block_until_com (prog_str_list_after_name t "Else") "End" in 
        IfElse(block1, block2)::(map_strs_to_prog (prog_str_list_after_name t "End"))
      | name -> let constructor = str_to_com_constructor name in (constructor args)::(map_strs_to_prog t)
and code_block_until_com (strs: string list list) (comname: string): command list =
  match strs with 
  | [] -> [Crash]
  | h :: t -> 
    match h with 
    | [] -> [Crash]
    | name :: args -> 
      if name=comname then []
      else let constructor = str_to_com_constructor name in (constructor args)::(code_block_until_com t comname)

let extract_str_from_const (c : const) : string = 
  match c with 
  | Undefined -> ""
  | Int(c) -> c 
  | String(c) -> c

let split_by_newline (s: string): string list = Str.split (Str.regexp "[\n]+") s
let split_by_spaces (str: string) = Str.split (Str.regexp "[ ]+") str

(* Command Functions --------------------------------------------------------------------------- *)
let pop_env (e: env): result = 
  match e with 
  | (s, g, l, p, o) -> if list_len s < 1 then (Err, e) else (Ok, new_env (List.tl s) g l p o)

let push_env (str: string) (e: env): result = 
  if str_is_int str then 
    match e with 
    | (s, g, l, p, o) -> (Ok, new_env (Int(str)::s) g l p o)
  else if str_is_str str then  
    match e with 
    | (s, g, l, p, o) -> (Ok, new_env ((String(str))::s) g l p o)
  else if str_is_name str then 
    let c = get_var str e in 
    if Undefined=c then 
      (Err, e) 
    else 
      match e with (s,g,l,p,o)->
      (Ok, new_env (c::s) g l p o)
  else
    (Err, e)

let add_env (e: env): result = 
  match e with 
  | (s, g, l, p, o) ->
    if list_len s < 2 then (Err, e) else 
    let a = List.hd s in 
    let b = List.hd (List.tl s) in 
    if all_ints [a;b] then 
      let n1 = int_of_string (extract_str_from_const a) in 
      let n2 = int_of_string (extract_str_from_const b) in 
      let resulting_const = Int(string_of_int (n1 + n2)) in 
      let s = List.tl (List.tl s) in 
      (Ok, new_env (resulting_const::s) g l p o)
    else 
      (Err, e)

let sub_env (e: env): result = 
  match e with 
  | (s, g, l, p, o) ->
    if list_len s < 2 then (Err, e) else 
    let a = List.hd s in 
    let b = List.hd (List.tl s) in 
    if all_ints [a;b] then 
      let n1 = int_of_string (extract_str_from_const a) in 
      let n2 = int_of_string (extract_str_from_const b) in 
      let resulting_const = Int(string_of_int (n1 - n2)) in 
      let s = List.tl (List.tl s) in 
      (Ok, new_env (resulting_const::s) g l p o)
    else 
      (Err, e)

let mul_env (e: env): result = 
  match e with 
  | (s, g, l, p, o) ->
    if list_len s < 2 then (Err, e) else 
    let a = List.hd s in 
    let b = List.hd (List.tl s) in 
    if all_ints [a;b] then 
      let n1 = int_of_string (extract_str_from_const a) in 
      let n2 = int_of_string (extract_str_from_const b) in 
      let resulting_const = Int(string_of_int (n1 * n2)) in 
      let s = List.tl (List.tl s) in 
      (Ok, new_env (resulting_const::s) g l p o)
    else 
      (Err, e)

let div_env (e: env): result = 
  match e with 
  | (s, g, l, p, o) ->
    if list_len s < 2 then (Err, e) else 
    let a = List.hd s in 
    let b = List.hd (List.tl s) in 
    if all_ints [a;b] then 
      let n1 = int_of_string (extract_str_from_const a) in 
      let n2 = int_of_string (extract_str_from_const b) in 
      if n2=0 then (Err, e) else 
      let resulting_const = Int(string_of_int (n1 / n2)) in 
      let s = List.tl (List.tl s) in 
      (Ok, new_env (resulting_const::s) g l p o)
    else 
      (Err, e)

let swap_env (e: env): result = 
  match e with 
  | (s, g, l, p, o) -> 
    if list_len s < 2 then (Err, e) else 
    let a = List.hd s in 
    let b = List.hd (List.tl s) in 
    let base = List.tl (List.tl s) in 
    (Ok, new_env (b::a::base) g l p o)

let neg_env (e: env): result = 
  match e with 
  | (s, g, l, p, o) -> 
    if s=[] then 
      (Err, e) 
    else 
      let a = List.hd s in 
      if all_ints [a] then 
        let n = -1 * (int_of_string (extract_str_from_const a)) in 
        let rc = Int(string_of_int n) in 
        (Ok, new_env (rc::(List.tl s)) g l p o)
      else
        (Err, e)

let concat_env (e: env): result = 
  match e with 
  | (s, g, l, p, o) -> 
    if list_len s < 2 then 
      (Err, e) 
    else 
      let a = List.hd s in 
      let b = List.hd (List.tl s) in 
      if all_strings [a;b] then 
        let s1 = extract_str_from_const a in 
        let s2 = extract_str_from_const b in 
        let rc = String(concat_strings s1 s2) in 
        (Ok, new_env (rc::(List.tl (List.tl s))) g l p o) 
      else 
        (Err, e)

let and_env (e: env): result = 
  match e with 
  | (s, g, l, p, o) -> 
    if list_len s < 2 then (Err, e)
    else 
      let a = List.hd s in 
      let b = List.hd (List.tl s) in 
      if not (all_bools [a;b]) then (Err, e)
      else 
        let s = List.tl (List.tl s) in 
        let s1 = extract_str_from_const a in 
        let s2 = extract_str_from_const b in 
        if (s1="1") && (s2="1") then 
          let rc = Int("1") in 
          (Ok, new_env (rc::s) g l p o)
        else 
          let rc = Int("0") in 
          (Ok, new_env (rc::s) g l p o)

let or_env (e: env): result = 
  match e with 
  | (s, g, l, p, o) -> 
    if list_len s < 2 then (Err, e)
    else 
      let a = List.hd s in 
      let b = List.hd (List.tl s) in 
      if not (all_bools [a;b]) then (Err, e)
      else 
        let s = List.tl (List.tl s) in 
        let s1 = extract_str_from_const a in 
        let s2 = extract_str_from_const b in 
        if (s1="1") || (s2="1") then 
          let rc = Int("1") in 
          (Ok, new_env (rc::s) g l p o)
        else 
          let rc = Int("0") in 
          (Ok, new_env (rc::s) g l p o)

let not_env (e: env): result = 
  match e with 
  | (s, g, l, p, o) -> 
    if list_len s < 1 then (Err, e)
    else 
      let a = List.hd s in 
      if not (all_bools [a]) then (Err, e)
      else 
        let s = List.tl s in 
        let s1 = extract_str_from_const a in 
        if (s1="1") then 
          let rc = Int("0") in 
          (Ok, new_env (rc::s) g l p o)
        else 
          let rc = Int("1") in 
          (Ok, new_env (rc::s) g l p o)

let equal_env (e: env): result = 
  match e with 
  | (s, g, l, p, o) -> 
    if list_len s < 2 then (Err, e) else 
      let a = List.hd s in 
      let b = List.hd (List.tl s) in 
      if not (all_ints [a;b]) then (Err, e) else 
        let i1 = int_of_string (extract_str_from_const a) in 
        let i2 = int_of_string (extract_str_from_const b) in 
        if i1=i2 then 
          let rc = Int("1") in 
          let s = List.tl (List.tl s) in 
          (Ok, new_env (rc::s) g l p o)
        else 
          let rc = Int("0") in 
          let s = List.tl (List.tl s) in 
          (Ok, new_env (rc::s) g l p o)

let lte_env (e: env): result = 
  match e with 
  | (s, g, l, p, o) -> 
    if list_len s < 2 then (Err, e) else 
      let a = List.hd s in 
      let b = List.hd (List.tl s) in 
      if not (all_ints [a;b]) then (Err, e) else 
        let i1 = int_of_string (extract_str_from_const a) in 
        let i2 = int_of_string (extract_str_from_const b) in 
        if i1=i2 || i1<i2 then 
          let rc = Int("1") in 
          let s = List.tl (List.tl s) in 
          (Ok, new_env (rc::s) g l p o)
        else 
          let rc = Int("0") in 
          let s = List.tl (List.tl s) in 
          (Ok, new_env (rc::s) g l p o)

let add_local_to_env (name: string) (e: env): result = 
  if not (str_is_name name) then (Err, e) else 
  match e with 
  | (s, g, l, p, o) -> if s=[] then (Err, e) else 
  let c = List.hd s in 
  (Ok, new_env (List.tl s) g ((name, c)::l) p o)

let add_global_to_env (name: string) (e: env): result = 
  if not (str_is_name name) then (Err, e) else 
    match e with 
    | (s, g, l, p, o) -> if s=[] then (Err, e) else 
    let c = List.hd s in 
    (Ok, new_env (List.tl s) ((name, c)::g) l p o)

let if_else_env (t: command list) (f: command list) (e: env): result = 
  match e with (s, g, l, p, o) -> 
  if list_len s < 1 then (Err, e) else 
  if not (str_is_bool (extract_str_from_const (List.hd s))) then (Err, e) else 
  let b = extract_str_from_const (List.hd s) in 
  if b="1" then 
    (Ok, new_env (List.tl s) g l (t@p) o)
  else 
    (Ok, new_env (List.tl s) g l (f@p) o)

let func_for_command (com: command): (env -> result) = 
  match com with 
  | Push(c) -> push_env c
  | Pop -> pop_env 
  | Add -> add_env 
  | Sub -> sub_env 
  | Mul -> mul_env
  | Div -> div_env
  | Swap -> swap_env 
  | Neg -> neg_env
  | Concat -> concat_env 
  | And -> and_env
  | Or -> or_env
  | Not -> not_env
  | Equal -> equal_env
  | Lte -> lte_env
  | Local(n) -> add_local_to_env n 
  | Global(n) -> add_global_to_env n
  | IfElse(t, f) -> if_else_env t f 
  | c -> failwith "invalid com in func_for_command"

(* Interpretation ------------------------------------------------------------------------------ *)
let rec interp_program (e: env): unit = 
  match e with (s, g, l, p, o) -> 
    match p with 
    | [] -> () 
    | com::t -> 
      match com with 
      | Quit -> write_stack s o 
      | SoftCrash -> () 
      | Crash -> write_err o 
      | com -> 
        let f = func_for_command com in 
        let r = f e in 
        process_result r  
and process_result  (r: result): unit = 
    match r with 
    | (Err, e) -> (match e with (s, g, l, p, o) -> write_err o)
    | (Ok, e) -> 
      match e with 
      (s, g, l, p, o) -> interp_program (new_env s g l (List.tl p) o)

let interpreter (src : string) (output_file_path: string): unit =
  let src = src
  |> split_by_newline
  |> (map split_by_spaces)
  |> map_strs_to_prog 
  in (interp_program (new_env [] [] [] src output_file_path) )