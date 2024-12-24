MODULE Test;
  CONST C = 10;                (*global constant C - shadowed in Q*)
  TYPE G = REAL;               (*global type G - NOT shadowed in Q*)
    T = REAL;                  (*global type T - shadowed in Q*)
  VAR A,                       (*global variable A - NOT shadowed in Q*)
    B: INTEGER;                (*global variable B - shadowed in Q*)

  PROCEDURE P;                 (*global procedure P*)

    PROCEDURE Q;               (*intermediate procedure Q*)
      CONST C = 20;            (*intermediate constant C - shadows global constant C*)
      TYPE T = INTEGER;        (*intermediate type T - shadows global type T*)
      VAR B: INTEGER;          (*intermediate variable B - shadows global variable B*)

      PROCEDURE R(x: T): T;    (*access to intermediate type T allowed in original compiler, NOT allowed in modified compiler*)
        VAR i: INTEGER;
          q: T;                (*access to intermediate type T allowed in original compiler, NOT allowed in modified compiler*)
          g: G;                (*access to global type G (which is NOT shadowed) allowed in BOTH compilers*)
      BEGIN (*R*)
        i := C;                (*access to intermediate constant C allowed in original compiler, NOT allowed in modified compiler*)
        P;                     (*access to global procedure P allowed in BOTH compilers*)
        Q;                     (*access to intermediate procedure Q allowed in BOTH compilers*)
        i := A;                (*access to global variable A (which is NOT shadowed) allowed in BOTH compilers*)
        i := B;                (*access to intermediate variable B NOT allowed in BOTH compilers*)
        RETURN i
      END R;

    BEGIN (*Q*)
    END Q;

  BEGIN (*P*)
  END P;

END Test.

ORP.Compile Test.Mod ~           # compile the test program with the ORIGINAL compiler (1 error)

ORP.Compile ORG.Mod/s ORP.Mod ~  # build the MODIFIED compiler
System.Free ORP ORG ~            # unload the ORIGINAL compiler

ORP.Compile Test.Mod ~           # compile the test program with the MODIFIED compiler (4 errors)


