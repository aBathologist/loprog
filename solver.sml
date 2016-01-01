signature DATA =
sig
  val base : Syntax.database ref
end

structure TestData : DATA =
struct
val base = ref ([
                   (("father",[CON "adam",CON "cain"]), []),
                   (("father",[CON "cain",CON "enoch"]), [])
                ]
                : Syntax.database);
end


functor Solver(Db:DATA) =
struct

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
val base = Db.base

(* assert fact to end of databse *)
fun assertz a = base := !base @ [a]

exception NoSolution

(* Used to renumber variable instances in a term to match a given level n *)
fun numberTermVars n t =
  case t of VAR (v,_) => VAR (v,n)
          | CON _     => t
          | CMP p     => CMP (numberAtomicProp n (p:atomic_prop))

and numberAtomicProp n (r,ts)  = (r, List.map (numberTermVars n) ts)

fun printLn str = print (str ^ "\n")

fun promptForInput str = ( print (str ^ " ") ;
                           TextIO.inputLine TextIO.stdIn
                         )

(* This function is coppied blindly from my reference material. But it seems to
    me a lousy mixture of concerns.
    TODO: !!! *)
(** [display_solution ch env] displays the solution of a goal encoded
       by [env]. It then gives the user the option to search for other
       solutions, as described by the list of choice points [ch], or to abort
       the current proof search. *)
fun displaySolution (choices : choice list) env =
  case (envToString env, choices)
   of ("Succeeds.", _) => printLn "Succeeds."
    | (answer, [])     => printLn answer
    | (answer, choice) =>
      case promptForInput (answer)
       of SOME ";" => continueSearch choices
        | _        => raise NoSolution

(* search for other soluctions beginning with first choice in choices *)
and continueSearch [] = raise NoSolution
  | continueSearch (choice::choices) = solve choices choice

(* main solving algorithm
   TODO: - annotate
         - refactor
         - clean
*)
and solve choices ({db, env, goal, depth}:choice) =
    let
        (** [reduce_atom a asrl] reduces atom [a] to subgoals by using the
            first assertion in the assetion list [asrl] whose conclusion matches
            [a]. It returns [None] if the atom cannot be reduced, or the
            remaining assertions, the new environment and the list of subgoals.
        *)
        fun reduceAtomicProp p [] = NONE
          | reduceAtomicProp p ((head,body)::db') =
            let
                val env' = Unify.unifyAtomicProps env (p, numberAtomicProp depth head)
            in
                SOME (db', env', List.map (numberAtomicProp depth) body)
            end
    in
        case goal
         of []       => displaySolution choices env
          | g::goal' =>
            case reduceAtomicProp g db
             of NONE => continueSearch choices
              | SOME (db', env', body) =>
                let
                    val goals = body @ goal'
                    val depth' = depth + 1
                    val choices' = {db=db', env=env, goal=goal, depth=depth}::choices
                    val choice'  = {db=(!base), env=env', goal=goals, depth=depth'}
                in
                    solve choices' choice'
                end
    end

fun solveTopLevel g =
  solve [] {db=(!base), env=[], goal=g, depth=1}
  handle NoSolution => printLn "Fails"

end

structure S = Solver(TestData)
open S
