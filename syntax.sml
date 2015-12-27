(* When testing this file for dev purposes, you'll need to run *)
(* it after calling `CM.make("../utils/sources.cm")`, since it *)
(* relies on the functions available in the utils library. *)

structure Syntax =
struct

type constant = string

(* The int allows for disambiguating alphabetically identical variables. *)
type variable = string * int

datatype term = VAR of variable             (* X1, Y2, Z3     *)
              | CON of constant             (* a, b, c        *)
              | CMP of constant * term list (* f(t_1,...,t_2) *)

type atomic_prop = constant * term list

(* The empty list represents true. *)
type clause = atomic_prop list

(* An assertion (p, clause) is a Horn formula: p <= clause. *)
type assertion = atomic_prop * clause

(* An environment carries the current bindings of variables. *)
type environment = (variable * term) list

(* dev: Example enivronment *)
val env = [(("X",0), CON "a"), (("Y",0), CMP ("ass", [VAR ("Y",1), CON "cookie", CON "butts"]))] : environment

(* (* A database is a list of assertions *) *)
type database = assertion list

datatype toplevel_cmd = ASSERT of assertion
                      | GOAL of clause      (* Query: ?- a *)
                      | USE of string       (* Importing files *)
                      | QUIT

(* Given a variable and an environment, it returns the value to which
   the variable is bound, or else the variable itself if it is unbound *)
fun lookup env x =
  case Dict.lookup env x of NONE    => VAR x
                          | SOME y  => y

(* Takes an environment env and a term t, and substitutes any variables in t
   for their bindings in env. *)
fun subInTerm env t =
  case t of CON _ => t
          | CMP (name, terms) => CMP (name, List.map (subInTerm env) terms)
          | VAR x => let val e = lookup env x
                     in if e = t
                        then e
                        else subInTerm env e
                     end

fun concatSep _   []      = ""
  | concatSep sep (x::[]) = x
  | concatSep sep (x::xs) = x ^ sep ^ concatSep sep xs

fun toString t =
  case t of VAR (v,0) => v
          | VAR (v,n) => v ^ Int.toString n
          | CON c => c
          | CMP (name, terms) => name ^ "(" ^ concatSep "," (List.map toString terms) ^ ")"

fun envToString env =
  let
      fun bindingToString ((v,_), term) = v ^ " = " ^ toString term
      fun topLevelBinding ((_,n),_) = n = 0
  in
      case List.filter topLevelBinding env
       of []       => "Succeeds."
        | bindings => (concatSep "\n" o List.map bindingToString) bindings
  end

(* The infamous occurs check *)
fun occurs var t =
  case t of VAR var' => var = var'
          | CON _    => false
          | CMP (_, ts) => List.exists (occurs var) ts


end
