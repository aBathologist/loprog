
signature UNIFIER =
sig
  include SYNTAX
  val unifyTerms :
      environment -> (term * term) -> environment option
  val unifyAtomicProps :
      environment -> (atomic_prop * atomic_prop) -> environment option
end

structure UnifierChk =
struct

open Syntax

exception Cyclical

fun unifyTermsChk NONE _ =  NONE
  | unifyTermsChk (SOME env) (t1,t2) =
    let
        fun occursCheck (v,t) =
          if occurs v t
          then (raise Cyclical)
          else (v,t)
    in
        if (t1 = t2)
        then SOME env
        else
            case (t1, t2)
             of (VAR v, t)       => SOME (occursCheck (v,t) :: env)
              | (t, VAR v)       => SOME (occursCheck (v,t) :: env)
              | (CMP p1, CMP p2) => unifyAtomicPropsChk (SOME env) (p1,p2)
              | _                => NONE
    end

and unifyAtomicPropsChk env ((name1,ts1), (name2,ts2)) =
    let
        fun sameLength xs ys = (List.length xs = List.length ys)
        fun unif (t1,t2,env) = unifyTermsChk env (t1,t2)
    in
        if (name1 = name2) andalso (sameLength ts1 ts2)
        then
            (ListPair.foldl unif env (ts1,ts2))
        else
            NONE
    end

(* TODO: make use of occursCheck option by parameterizing module around
         a parameter val check : bool or maybe just two different modules? *)

fun unifyTerms env = unifyTermsChk (SOME env)
fun unifyAtomicProps env = unifyAtomicPropsChk (SOME env)

end

(* open Unify *)
(* structure Constructor = *)
(* struct *)

(* fun ? id = VAR (id, 0) *)
(* infix -- *)
(* fun p -- ts = CMP (p, ts) *)
