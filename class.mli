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

type t

val compare : t -> t -> int

val create : Location.t -> Table.Id.t -> t

val to_id : t -> Table.Id.t  (* global identifier for class *)

val to_string : t -> string  (* printable name for class *)

val contents : unit -> (Table.Id.t * string) list  (* global identifier and printable name for class *)

(* end *)
