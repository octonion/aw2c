(* scope.mli 

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
(** Declaration scopes for blocks, PROCEDURE formal parameter lists
    and FOR statements. (The control identifier of a FOR statement is
    local to its body.) *)

(** Raised by [get] or [pos] an identifier is not defined anywhere in
    the global scope. *)
exception Undefined of Table.Id.t

(** [Redefined (identifier, definition)] is raise by [set]
    if it was about to be redefine [identifier] in the local
    local. [definition] is the existing definition for
    [identifier] *)
exception Redefined of Table.Id.t * Type.definition_t


(** Local scopes: a map of identifiers to definitions.
    No identifier may be defined twice in a local scope. *)
module Local : 
sig 
  type t
  val empty : t
  val get : t -> Table.Id.t -> Type.definition_t option
  val set : t -> Table.Id.t -> Type.definition_t -> t
end


(** Global scopes: a stack of nested local scopes.  *)
type t = Local.t list
  
(** A empty global scope. *)
val empty : t

(** [push scope] pushes a new local scope onto a scope.
    @return the modified scope *)
val push : t -> t
    
(** [outside scope] pops a local scope from a scope.
    I.e. it reveals the outside declarartions.
    @return the modified scope *)
val pop : t -> t
    
(** [get identifier scope] returns the  definition of [identifier] in [scope]. 
    @raise Undefined if [identifier] not in [scope] *)
val get : t -> Table.Id.t -> Type.definition_t        

(** [set scope identifier definition] gives [identifier] a [definition] of in the local scope.  
    @return the modified scope
    @raise Redefined if [identifier] is already defined in the local scope *)
val set : t -> Table.Id.t -> Type.definition_t -> t

(** [redefine scope identifier definition] redefines [identifier] with [definition] of in the local scope.  
    @return the modified scope *)
val redefine : t -> Table.Id.t -> Type.definition_t -> t

(* end *)
