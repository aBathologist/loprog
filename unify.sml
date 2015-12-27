structure Unify =
struct

open Syntax

exception Ununifiable
exception Cyclical

fun unifyTermsChk env (t1,t2) =
  let
      fun occursCheck (v,t) = if occurs v t
                              then (raise Cyclical)
                              else (v,t)
  in
      if (t1 = t2)
      then env
      else
          case (t1, t2)
           of (VAR v, t)       => occursCheck (v,t) :: env
            | (t, VAR v)       => occursCheck (v,t) :: env
            | (CMP p1, CMP p2) => unifyAtomicPropsChk env (p1,p2)
            | _                => raise Ununifiable
  end

and unifyAtomicPropsChk env ((name1,ts1), (name2,ts2)) =
    let
        fun sameLength xs ys = (List.length xs = List.length ys)
        fun unif (t1,t2,env) = unifyTermsChk env (t1,t2)
    in
        if (name1 = name2) andalso (sameLength ts1 ts2)
        then
            ListPair.foldl unif env (ts1,ts2)
        else
            raise Ununifiable
    end

(* TODO: make use of occursCheck option by paramterizing module around
         a parameter val check : bool or maybe just two different modules? *)

val unifyTerms = unifyTermsChk
val unifyAtomicProps = unifyAtomicPropsChk

end
