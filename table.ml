(* table.ml 

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

module Id = struct
  type t = int

  let id2string : string DynArray.t = DynArray.create 20 "" ;;

  let string2id : (string, int) Hashtbl.t = Hashtbl.create 83 ;;

  let create (str : string) : t =
    let str' = String.lowercase str in
    try
      Hashtbl.find string2id str'
    with Not_found ->
      let id = DynArray.length id2string in
      begin
        DynArray.push id2string str' ;
        Hashtbl.add string2id str' id ;
        id
      end

  let to_string (id : t) : string = DynArray.get id2string id ;;

  let hash (id : t) : int = id ;;

  let compare (id1 : t) (id2 : t) : int = id1 - id2 ;;

  let eq (id1 : t) (id2 : t) : bool = (id1 = id2) ;;

  let clear () : unit = begin  DynArray.clear id2string ; Hashtbl.clear string2id end ;;

  let dummy = -1
end


(** Maps of identifiers to any type. Used in {!Scope}. *)
module IdMap = Map.Make(Id) ;;


(** Sets of class identifiers. *)
module IdSet = Set.Make(Id) ;;


(* end *)
