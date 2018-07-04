(* table.mli 

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
(** The global symbol table. *)

(** Algol W identifiers, These are case insensitive; they are always
    output in lowercase.  This interface is compatible with
    [Set.OrderedType] and [Hashtbl.HashableType]. *)
module Id : sig
  (** An identifier in an Algol W program. *)
  type t
    (** [create string] creates a new identifier. 
        @return an [Id.t] for [string] *)
  val create    : string -> t
    
  (** [to_string identifier] returns the string representation of an identifier.
      @return the identifier as a lowercase string *)
  val to_string : t -> string
    
  (** Clear the global symbol table. *)
  val clear     : unit -> unit
    
  val hash      : t -> int
  val compare   : t -> t -> int
  val eq        : t -> t -> bool

  val dummy : t  (* an identifer that is never used *)
end


(** Maps of identifiers to any type. Used in {!Scope}. *)
module IdMap : Map.S with type key = Id.t


(** Sets of class identifiers. *)
module IdSet : Set.S with type elt = Id.t


(* end *)
