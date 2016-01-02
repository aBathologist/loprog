
signature UNIFIER =
sig
  include SYNTAX
  val unifyTerms :
      environment -> (term * term) -> environment option
  val unifyAtomicProps :
      environment -> (atomic_prop * atomic_prop) -> environment option
end

structure Unifier : UNIFIER =
struct
  open Syntax

  fun unifyTerms' NONE _ = NONE
    | unifyTerms' (SOME env) (t1,t2) =
      if (t1 = t2)
      then SOME env
      else
          case (t1,t2)
           of (VAR v, t)       => SOME ((v,t) :: env)
            | (t, VAR v)       => SOME ((v,t) :: env)
            | (CMP p1, CMP p2) => unifyAtomicProps' (SOME env) (p1,p2)
            | _                => NONE

  and unifyAtomicProps' optEnv ((name1,ts1),(name2,ts2)) =
      let
          fun unify (t1,t2,optEnv) = unifyTerms' optEnv (t1,t2)
      in
          if (name1 = name2) andalso (ListExtra.sameLength ts1 ts2)
          then
              ListPair.foldl unify optEnv (ts1,ts2)
          else
              NONE

      end

  fun unifyTerms env = unifyTerms' (SOME env)
  fun unifyAtomicProps env = unifyAtomicProps' (SOME env)

end

structure UnifierChk : UNIFIER =
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

  and unifyAtomicPropsChk optEnv ((name1,ts1), (name2,ts2)) =
      let
          fun unify (t1,t2,optEnv) = unifyTermsChk optEnv (t1,t2)
      in
          if (name1 = name2) andalso (ListExtra.sameLength ts1 ts2)
          then
              ListPair.foldl unify optEnv (ts1,ts2)
          else
              NONE
      end

  fun unifyTerms env = unifyTermsChk (SOME env)
  fun unifyAtomicProps env = unifyAtomicPropsChk (SOME env)

end

    (* open Unify *)
    (* structure Constructor = *)
    (* struct *)

    (* fun ? id = VAR (id, 0) *)
    (* infix -- *)
    (* fun p -- ts = CMP (p, ts) *)
