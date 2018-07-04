(* class.mli -- record class identifiers 

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

type t = int  (* classes are identified by their index in the global class array. *)

let compare a b = a - b

let exception_class = (Table.Id.create "alw_0000_exception", "exception")

let global_class_array = DynArray.create 0 (Table.Id.dummy, "")

let create loc id = 
  let name = Table.Id.to_string id in
  let (_, line, _) = Location.contents loc in
  let global_id = Table.Id.create (Printf.sprintf "alw_%04i_%s" line name) in
  DynArray.push global_class_array (global_id, name) ; 
  DynArray.length global_class_array - 1

let to_id c     = fst (DynArray.get global_class_array c)

let to_string c = snd (DynArray.get global_class_array c)

let contents () = 
  let rec loop i xs = 
    if i < 1 then xs 
    else loop (i - 1) (DynArray.get global_class_array i :: xs)
  in
  loop (DynArray.length global_class_array - 1) []
      

(* end *)
