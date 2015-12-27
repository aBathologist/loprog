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
