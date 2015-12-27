open Syntax

(* Representation of a choice point.
    (db    : database)    a list of assertions
    (env   : environment) a list of variable bindings
    (goal  : clause)      the current goal
    (depth : int)         the search depth
 *)

type choice = { db    : database,
                env   : environment,
                goal  : clause,
                depth : int }

(* Global database *)
val base = ref ([] : database)

(* assert fact to end of databse *)
fun assertz a = base := !base @ [a]

exception Unprovable

(* Used to renumber variable instances in a term to match a given level n *)
fun numberTermVars n t = case t of VAR (v,_) => VAR (v,n)
                                 | CON _     => t
                                 | CMP p     => CMP p
and numberAtomicProp n (r,ts) = (r, List.map (numberTermVars n) ts)

(* This function is coppied blindly from my reference material. But it seems to
    me a lousy mixture of concerns.
    TODO: !!! *)
(** [display_solution ch env] displays the solution of a goal encoded
       by [env]. It then gives the user the option to search for other
       solutions, as described by the list of choice points [ch], or to abort
       the current proof search. *)
(* fun displaySolution choices env = *)
(*   case *)
