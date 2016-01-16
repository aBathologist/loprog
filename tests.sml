(* use "../utils/test.sml"; *)

structure SyntaxSetup =
struct

  open Test.Setup
  open Syntax
  val suiteName = "syntax.sml"

  val exVariable : term = VAR ("X", 0)
  val exConstant : term = CON "A"
  val exCompound : term = CMP ("father", [CON "adam", CON "cain"])

  val exAtomicProp : atomic_prop =
      ("father", [CON "adam", CON "cain"])
  val exClause : clause =
      [("father", [VAR ("X",0), VAR ("Y",0)]),
       ("father", [VAR ("Y",0), VAR ("Z",0)])]
  val exAssertion : assertion =
      (("grandfather", [VAR ("X",0), VAR ("Z",0)]),
       [ ("father", [VAR ("X",0), VAR ("Y",0)]),
         ("father", [VAR ("Y",0), VAR ("Z",0)])] )
  val exEnvironment : environment =
      [(("X",0), CON "adam"), (("Z",0), CON "enoch")]
  val exDatabase : database =
      [(("father", [CON "cain", CON "enoch"]),[]),
       (exAtomicProp, []),
       exAssertion]

  fun tests () =
    [
      test "substition of environment bindings in a term"
           let val partialTerm  =
                   CMP ("grandfather", [VAR ("X",0), VAR ("Z",0)])
               val termWithSubs =
                   CMP ("grandfather", [CON "adam",  CON "enoch"])
           in subInTerm exEnvironment partialTerm = termWithSubs
           end,

      test "conversion of a variable term to a string"
           (toString exVariable = "X"),

      test "converison of an environment to a string"
           (envToString exEnvironment = "X = adam\nZ = enoch"),

      test "a variable occurs in a term"
           (occurs ("X",0) (CMP ("father", [CON "adam", VAR ("X",0)]))),

      test "a variable doesn't occur in a term"
           (not (occurs ("X",0) exCompound))
    ]

end

structure UnifierSetup : SETUP =
struct

  open Test.Setup
  open Unifier
  val suiteName = "Unifier in unifier.sml"

  fun tests () =
    [
      test "successfull unification"
           let val t1 = CMP ("father", [VAR ("X",0), CON "enoch"])
               val t2 = CMP ("father", [CON "adam",  VAR ("Y",0)])
               val envShouldBe = [(("X",0), CON "adam"),
                                  (("Y",0), CON "enoch")]
               val SOME resultingEnv = unifyTerms [] (t1,t2)
           in ListExtras.eqMembers resultingEnv envShouldBe
           end,

      test "unsuccessfull unificaiton"
           let val t1 = CMP ("father", [VAR ("X",0), CON "able"])
               val t2 = CMP ("father", [CON "adam",  CON "cain"])
           in unifyTerms [] (t1,t2) = NONE
           end,

      test "successfull unification of nest compound terms"
           let val t1 =
                   CMP ("p", [CMP ("q", [VAR ("A",0)]), CON "b", CON "c"])
               val t2 =
                   CMP ("p", [VAR ("X", 0), VAR ("A",0), VAR ("A",0)])
               val envShouldBe = SOME [(("Y",1),CON "b"),
                                       (("X",0),CMP ("q",[VAR ("A",0)]))]
           in unifyTerms [] (t1,t2) = SOME 

    ]


end

structure UnifierTests = Test.Run(structure Setup = UnifierSetup)
structure SyntaxTests = Test.Run(structure Setup = SyntaxSetup);

UnifierTests.detail ();
