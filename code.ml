(* code.ml 

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

(* The idea here is to leave all the work until the code is output.
   We particularly don't want to concatenate tens of thousands of
   increasing large strings as we go. *)

exception TemplateError of string

type t =
  | Empty
  | String of string
  | Add of t * t
  | Concat of string * t list
  | Template of string * t list

let empty = Empty

let string s = String s

let add a b = Add (a, b)

let separate separator code_list = 
  Concat (separator, code_list)

let rseparate separator code_list = 
  Concat (separator, List.rev code_list)

let concat code_list = 
  Concat ("", code_list)

let template template code_list = 
  Template (template, code_list)

let rec output_code output code =
   match code with
   | Empty -> ()
   | String s -> 
       output_string output s
   | Add (a, b) ->
       ( output_code output a ; output_code output b )
   | Concat (seperator, code_list) -> 
       let rec loop =
         function
         | [] -> ()
         | [c] -> output_code output c
         | c :: cs -> 
             output_code output c ;
             output_string output seperator ;
             loop cs
       in
       loop code_list
   | Template (template, code_list) ->
       let rec loop i cs =
         if i < String.length template then 
           if template.[i] = '$' then
             match cs with
             | [] -> raise (TemplateError template)
             | c :: cs' -> ( output_code output c ; loop (i + 1) cs' )
           else
             ( output_char output template.[i] ; loop (i + 1) cs )
         else
           match cs with
           | [] -> ()
           | _ -> raise (TemplateError template)
       in
       loop 0 code_list 

let rec is_empty =
  function
  | Empty -> true
  | String "" -> true
  | Add (x, y) -> is_empty x && is_empty y
  | Concat (_, xs) -> List.for_all is_empty xs
  | Template ("", _) -> true
  | _ -> false


(* end *)
