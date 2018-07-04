(* location.ml 

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

(* There's going to be a lot of these. *)

(* Ocaml strings are assigned by reference, so I expect this to be compiled as a pointer and an integer: *)
type t = Location of string * int

let filename = ref ""

let set_filename new_filename = filename := new_filename

let create file line char = 
  assert (char < 10000);
  Location (file, line * 10000 + char)

let of_position position = 
  let line = position.Lexing.pos_lnum in
  let char = position.Lexing.pos_cnum - position.Lexing.pos_bol in
  create !filename line char

let contents loc = 
  let Location (file, n) = loc in (file, n / 10000, n mod 10000)

let to_string loc = 
  let file, line, char = contents loc in
  Printf.sprintf "%s:%i:%i:" file line (char + 1)

(* 2008-07-12 Lines directives were causing Hendrik Boom trouble with gdb *)

let to_line_directive loc = ""
(*
let to_line_directive loc = 
  let file, line, char = contents loc in
  Printf.sprintf "#line %i %S" line file
*)


(* end *)
