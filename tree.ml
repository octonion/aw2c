(* tree.ml 

--

This file is part of aw2c. Copyright 2008 Glyn Webster.

aw2c is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

aw2c is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with aw2c.  If not, see <http://www.gnu.org/licenses/>.

*)
(** Algol-W parse tree. Returned by [Parser.program]. *)

type id = Table.Id.t
type loc = Location.t

type t = 
  (* Constants *)
  | Integer        of loc * string
  | Bits           of loc * string
  | String         of loc * string
  | Real           of loc * string * string  (** Stored as decimal and exponent strings, to avoid rounding errors. *)
  | Imaginary      of loc * string * string  
  | LongReal       of loc * string * string
  | LongImaginary  of loc * string * string  
  | TRUE           of loc
  | FALSE          of loc
  | NULL           of loc
  | C_code         of loc * t option * string (* simple type, C code *)

  (* statements *)
  | IF_else    of loc * t * t * t
  | IF         of loc * t * t
  | CASE       of loc * t * t list
  | CASE_expr  of loc * t * t list
  | WHILE      of loc * t * t
  | FOR        of loc * id * t * t * t
  | FOR_step   of loc * id * t * t * t * t
  | FOR_list   of loc * id * t list * t
  | GOTO       of loc * id
  | ASSERT     of loc * t
  | Empty      of loc
  | BEGIN      of loc * t list * t list

  | Label     of loc * id  (** Labels in the bodies of blocks. *)

  | Assignment of loc * t * t
  | Variable    of loc * id
  | Dereference of loc * id * t list
  | Substring   of loc * t * t * int

  | STAR of loc (** appears in array designators *)

  | Binary of loc * t * t * t
  | EQ 
  | NE 
  | GT 
  | LT 
  | GE 
  | LE 
  | IS 
  | ADD 
  | SUB 
  | OR 
  | MUL 
  | RDIV
  | IDIV
  | REM 
  | AND 
  | PWR 
  | SHL 
  | SHR 

  (* Unary expression and operators *)
  | Unary of loc * t * t
  | LONG
  | SHORT
  | ABS
  | NOT
  | NEG
  | IDENTITY

  (* declarations *)
  | Simple     of loc * t * id list
  | RECORD     of loc * id * t list
  | ARRAY      of loc * t * id list * (t * t) list
  | PROCEDURE  of loc * t option * id * t list * t

  (* Simple types *)
  | INTEGER
  | BITS
  | STRING of int option
  | STRING_star
  | REAL
  | COMPLEX
  | LONG_REAL
  | LONG_COMPLEX
  | LOGICAL
  | REFERENCE of loc * id list

  (* formal parameters *)
  | Name_formal         of loc * t * id list
  | VALUE_formal        of loc * t * id list
  | RESULT_formal       of loc * t * id list
  | VALUE_RESULT_formal of loc * t * id list
  | PROCEDURE_formal    of loc * t option * id list * t list  (* loc, return type, ids, formals *)
  | ARRAY_formal        of loc * t * id list * int

  (* external Foreign interface procedure body *)
  | Foreign of loc * Table.Id.t * string
;;


(* end *)

let sprintf = Printf.sprintf


let algolw_string_literal s =
  let b = Buffer.create 32 in
  Buffer.add_char b '"';
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '"' -> Buffer.add_string b "\"\""
    | c   -> Buffer.add_char b c
  done;
  Buffer.add_char b '"';
  Buffer.contents b


let rec str =
  function
  | Integer (_, s) -> s
  | Bits (_, s) -> sprintf "#%s" s
  | String (_, s) -> algolw_string_literal s
  | Real          (_, r, "") -> r
  | Imaginary     (_, r, "") -> r ^ "I"
  | LongReal      (_, r, "") -> r ^ "L"
  | LongImaginary (_, r, "") -> r ^ "IL"
  | Real          (_, r, e)  -> sprintf "%s'%s" r e
  | Imaginary     (_, r, e)  -> sprintf "%s'%sI" r e
  | LongReal      (_, r, e)  -> sprintf "%s'%sL" r e
  | LongImaginary (_, r, e)  -> sprintf "%s'%sIL" r e
  | TRUE (_) -> "TRUE"
  | FALSE (_) -> "FALSE"
  | NULL (_) -> "NULL"
  | C_code (_, _, c) -> c

  | Assignment (_, d, value)         -> sprintf "(%s := %s)" (str d) (str value)
  | Variable (_, id)                 -> Table.Id.to_string id
  | Dereference (_, id, actuals )     -> sprintf "%s(%s)" (Table.Id.to_string id) (comma_strs actuals)
  | Substring (_, src, start, length ) -> sprintf "%s(%s | %i)" (str src) (str start) length
  | STAR _  -> "*"


  | Binary (_, left, op, right) -> sprintf "(%s %s %s)" (str left) (str op) (str right)
  | EQ -> "="
  | NE -> "~="
  | GT -> ">"
  | LT -> "<"
  | GE -> ">="
  | LE -> "<="
  | IS -> "IS"
  | ADD -> "+"
  | SUB -> "-"
  | OR -> "OR"
  | MUL -> "*"
  | RDIV -> "/"
  | IDIV -> "DIV"
  | REM -> "REM"
  | AND -> "AND"
  | PWR -> "**"
  | SHL -> "SHL"
  | SHR -> "SHR"

  | Unary (_, op, right) -> sprintf "(%s %s)" (str op) (str right)
  | IDENTITY -> "+"
  | NEG -> "-"
  | NOT-> "~"
  | LONG -> "LONG"
  | SHORT -> "SHORT"
  | ABS -> "ABS"
      
  | IF_else (_, condition, then_branch, else_branch) -> 
        sprintf 
          "(IF %s THEN %s ELSE %s)" 
          (str condition) 
          (str then_branch) 
          (str else_branch)
  | IF (_, condition, then_branch) -> 
      sprintf 
        "(IF %s THEN %s)"
        (str condition)
        (str then_branch)
  | CASE (_, selector, branches) -> 
      ( match branches with
        | [] -> sprintf "CASE %s OF BEGIN END" (str selector)
        | _  -> 
            sprintf 
              "CASE %s OF BEGIN %s END"
              (str selector) 
              (String.concat "; " (List.map str branches))
      )
  | CASE_expr (_, selector, branches) -> 
      sprintf 
        "CASE %s OF (%s)"
        (str selector) 
        (comma_strs branches)
  | WHILE (_, condition, body) -> 
      sprintf "(WHILE %s DO %s)" (str condition) (str body)
  | FOR (_, counter, first, last, body) -> 
      sprintf 
        "(FOR %s := %s UNTIL %s DO %s)" 
        (Table.Id.to_string counter) 
        (str first) 
        (str last) 
        (str body)
  | FOR_step (_, counter, first, step, last, body) -> 
      sprintf 
        "(FOR %s := %s STEP %s UNTIL %s DO %s)" 
        (Table.Id.to_string counter) 
        (str first) 
        (str step) 
        (str last) 
        (str body)
  | FOR_list (_, counter, values, body) -> 
      sprintf 
        "(FOR %s := %s DO %s)" 
        (Table.Id.to_string counter) 
        (comma_strs values)
        (str body)
  | GOTO (_, label) ->
      sprintf 
        "(GOTO %s)" 
        (Table.Id.to_string label)
  | ASSERT (_, condition) ->
      sprintf 
        "(ASSERT %s)" 
        (str condition)
  | Empty (_) -> 
      "(*empty*)"    
        
  | BEGIN (_, ds, ss ) -> 
      ( match (ds, ss) with
        | ([], []) -> "BEGIN END" 
        | ([], _) -> sprintf "BEGIN %s END" (block_body_items ss)
        | (_, _) -> 
            sprintf 
              "BEGIN %s; %s END" 
              (semicolon_strs ds)
              (block_body_items ss)
      )
        
  | INTEGER -> "INTEGER"
  | BITS -> "BITS"
  | STRING (Some len) -> sprintf "STRING(%i)" len
  | STRING None -> sprintf "STRING"
  | STRING_star -> "STRING(*)"
  | REAL -> "REAL"
  | COMPLEX -> "COMPLEX"
  | LONG_REAL -> "LONG REAL"
  | LONG_COMPLEX -> "LONG COMPLEX"
  | LOGICAL -> "LOGICAL"
  | REFERENCE (_, ids) -> sprintf "REFERENCE(%s)" (String.concat ", " (List.map Table.Id.to_string ids))

  | Simple (_, simpletype, identifiers ) -> 
      sprintf 
        "%s %s" 
        (str simpletype) 
        (comma_ids identifiers)
  | ARRAY (_, simpletype, identifiers, dimensions_list ) ->
      sprintf 
        "%s ARRAY %s (%s)" 
        (str simpletype) 
        (comma_ids identifiers)
        (String.concat ", " (List.map (fun(l, h) -> sprintf "%s :: %s" (str l) (str h)) dimensions_list))
        
  | RECORD (_, recordclass, field_declarations ) ->
      sprintf 
        "RECORD %s (%s)" 
        (Table.Id.to_string recordclass) 
        (semicolon_strs field_declarations)
  | PROCEDURE (_, simpletype, identifier, parameters, procedure_body) ->
      let h =
        match simpletype with
        | None -> "PROCEDURE"
        | Some t -> (str t) ^ " PROCEDURE"
      in
      sprintf "%s %s%s; %s" h (Table.Id.to_string identifier) (formals parameters) (str procedure_body)
        
  | Foreign (_, language, link ) -> 
      sprintf "%s \"%s\"" (Table.Id.to_string language) link
        
  | Name_formal         (_, t, ids) -> sprintf "%s %s" (str t) (comma_ids ids)
  | VALUE_formal        (_, t, ids) -> sprintf "%s VALUE %s" (str t) (comma_ids ids)
  | RESULT_formal       (_, t, ids) -> sprintf "%s RESULT %s" (str t) (comma_ids ids)
  | VALUE_RESULT_formal (_, t, ids) -> sprintf "%s VALUE RESULT %s" (str t) (comma_ids ids)
  | PROCEDURE_formal    (_, None, ids, fs) -> sprintf "PROCEDURE %s%s" (comma_ids ids) (formals fs)
  | PROCEDURE_formal    (_, Some t, ids, fs) -> sprintf "%s PROCEDURE %s%s" (str t) (comma_ids ids) (formals fs)
      
  | ARRAY_formal        (_, t, ids, dim) -> sprintf "%s ARRAY %s (%s)" (str t) (comma_ids ids) (stars dim)

  | Label (_,_) -> failwith "shouldn't be labels here"

and comma_strs trees      = (String.concat ", " (List.map str trees))    
and semicolon_strs trees  = (String.concat "; " (List.map str trees))    
and comma_ids ids         = (String.concat ", " (List.map Table.Id.to_string ids))    
and block_body_items = 
  function
  | []                      -> ""
  | Label (_, id ) :: []    -> sprintf "%s:" (Table.Id.to_string id)
  | Label (_, id ) :: items -> sprintf "%s: " (Table.Id.to_string id) ^ block_body_items items
  | e              :: []    -> str e
  | e              :: items -> str e ^ "; " ^ block_body_items items
and stars = 
  function
  | 1 -> "*"
  | n -> "*, " ^ stars (n - 1)
and formals fs =
  match fs with
  | [] -> ""
  | _ -> sprintf " (%s)" (semicolon_strs fs)
;;
      

let rec last (xs : 'a list) : 'a =
  match xs with 
  | []       -> failwith "last"
  | [x]      -> x
  | x :: xs' -> last xs'


let rec to_loc =
  function
  | Integer (loc, _) -> loc
  | Bits (loc, _) -> loc
  | String (loc, _) -> loc
  | Real (loc, _, _) -> loc
  | Imaginary (loc, _, _  ) -> loc
  | LongReal (loc, _, _) -> loc
  | LongImaginary (loc, _, _  ) -> loc
  | TRUE (loc) -> loc
  | FALSE (loc) -> loc
  | NULL (loc) -> loc
  | IF_else (loc, _, _, _) -> loc
  | IF (loc, _, _) -> loc
  | CASE (loc, _, _) -> loc
  | CASE_expr (loc, _, _) -> loc
  | WHILE (loc, _, _) -> loc
  | FOR (loc, _, _, _, _) -> loc
  | FOR_step (loc, _, _, _, _, _) -> loc
  | FOR_list (loc, _, _, _) -> loc
  | GOTO (loc, _) -> loc
  | ASSERT (loc, _) -> loc
  | Empty (loc) -> loc
  | BEGIN (_, _, ss) -> to_loc (last ss)  (* error messages are about the type of the last expression *)
  | Label (loc, _) -> loc
  | Assignment (loc, _, _) -> loc
  | Variable (loc, _) -> loc
  | Dereference (loc, _, _) -> loc
  | Substring (loc, _, _, _) -> loc
  | Binary (loc, _, _, _) -> loc
  | Unary (loc, _, _) -> loc
  | Simple (loc, _, _) -> loc
  | RECORD (loc, _, _) -> loc
  | ARRAY (loc, _, _, _) -> loc
  | PROCEDURE (loc, _, _, _, _) -> loc
  | Name_formal (loc, _, _) -> loc
  | VALUE_formal (loc, _, _) -> loc
  | RESULT_formal (loc, _, _) -> loc
  | VALUE_RESULT_formal (loc, _, _) -> loc
  | PROCEDURE_formal (loc, _, _, _) -> loc
  | ARRAY_formal (loc, _, _, _) -> loc
  | Foreign (loc, _, _) -> loc
  | C_code (loc, _, _) -> loc
  | symbol -> failwith (sprintf "Tree.to_loc: %s has no location" (str symbol))
;;


(* end *)
