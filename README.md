# Oberon-no-access-to-intermediate-objects
Dissallow access to all intermediate objects (except procedures) in the Oberon-07 programming language.

Note: In this repository, the term "Project Oberon 2013" refers to a re-implementation of the original "Project Oberon" on an FPGA development board around 2013, as published at www.projectoberon.com.

**PREREQUISITES**: A current version of Project Oberon 2013 (see http://www.projectoberon.com). If you use Extended Oberon (see http://github.com/andreaspirklbauer/Oberon-extended), the functionality is already implemented.

------------------------------------------------------
The official Oberon-07 **language report** (http://www.inf.ethz.ch/personal/wirth/Oberon/Oberon07.Report.pdf, as of May 3, 2016) disallows access to intermediate *constants*, *types* and *variables* within nested scopes.

The modified Oberon-07 compiler provided in **this** repository brings the compiler in line with the language report, and also disallows access to intermediate *constants* and *types* within nested scopes, not just access to intermediate *variables*.

Like the official Oberon-07 compiler, the revised compiler implements **shadowing through scope** when accessing named objects. This means when two objects share the *same* name, the one declared at the narrower scope hides, or *shadows*, the one declared at the wider scope. In such a situation, the shadowed element is not available in the narrower scope. If the shadowing element is itself declared at an intermediate scope, it is only available at that scope level, but not in narrower scopes (as access to intermediate objects is disallowed)

The *official* Oberon-07 compiler already issues an error message, if intermediate **variables** are accessed within nested scopes (line 25 of the program below), *regardless* of whether a global variable with the same name exists (line 7) or not. With the revised compiler, the same error message is now *also* issued for intermediate **constants** (line 21) and **types** (lines 16 and 18).

------------------------------------------------------
**Example**

      1   MODULE Test;
      2     CONST C = 10;               (*global constant C, shadowed in Q and therefore not available in R*)
      3
      4     TYPE G = REAL;              (*global type G, not shadowed in Q and therefore available in R*)
      5       T = REAL;                 (*global type T, shadowed in Q and therefore not available in R*)
      6     VAR A,                      (*global variable A, not shadowed in Q and therefore available in R*)
      7       B: INTEGER;               (*global variable B, shadowed in Q and therefore not available in R*)
      8
      9     PROCEDURE P;                (*global procedure P*)
     10
     11       PROCEDURE Q;              (*intermediate procedure Q, contains shadowing elements C, T and B*)
     12         CONST C = 20;           (*intermediate constant C which shadows the global constant C*)
     13         TYPE T = INTEGER;       (*intermediate type T which shadows the global type T*)
     14         VAR B: INTEGER;         (*intermediate variable B which shadows the global variable B*)
     15
     16         PROCEDURE R(x: T): T;   (*access to intermediate type T allowed in original, not allowed in modified compiler*)
     17           VAR i: INTEGER;
     18             q: T;               (*access to intermediate type T allowed in original, not allowed in modified compiler*)
     19             g: G;               (*access to global type G (not shadowed) allowed in both compilers*)
     20          BEGIN (*R*)
     21            i := C;              (*access to interm. constant C allowed in original, not allowed in modified compiler*)
     22            P;                   (*access to global (unshadowed) procedure P allowed in both compilers*)
     23            Q;                   (*access to intermediate procedure Q allowed in both compilers*)
     24            i := A;              (*access to global (unshadowed) variable A allowed in both compilers*)
     25            i := B;              (*access to intermediate variable B not allowed in both compilers*)
     26            RETURN i
     27          END R;
     28        END Q;
     29      END P;
     30
     31    END Test.

Disallowing access to intermediate objects from within nested scopes while at the same time implementing *shadowing through scope* raises the question whether one should *relax* the shadowing rules and *allow* access to the *global* scope level, when an object with the same name as a global object is re-declared at an *intermediate* level, but *not* at the strictly local level ("piercing through the shadow").

In the above example, such an approach would allow access to the global variable B (line 7) in procedure R (line 25), effectively *ignoring* any intermediate-level variables B that may also exist (line 14). It would make nested procedures self-contained in the sense that they can be moved around freely. For example, procedure R can be made local to procedure Q *without* having to be concerned about whether one can still access the global variable B (line 7).

We have opted not to adopt this approach for two main reasons. First, a nested procedure may also call the *surrounding* procedure that contains it (a frequent case) and is thus not necessarily self-contained anyway. Second, we didn’t want to break with a long language tradition (in *appendix B*, a possible implementation of such relaxed shadowing rules is provided).

------------------------------------------------------
**Preparing your system to use the modified Oberon compiler**

If *Extended Oberon* is used, "no access to intermediate objects" is already implemented on your system.

If *Project Oberon 2013* is used, follow the instructions below:

------------------------------------------------------

Convert the downloaded files to Oberon format (Oberon uses CR as line endings) using the command [**dos2oberon**](dos2oberon), also available in this repository (example shown for Mac or Linux):

     for x in *.Mod ; do ./dos2oberon $x $x ; done

Import the files to your Oberon system. If you use an emulator (e.g., **https://github.com/pdewacht/oberon-risc-emu**) to run the Oberon system, click on the *PCLink1.Run* link in the *System.Tool* viewer, copy the files to the emulator directory, and execute the following command on the command shell of your host system:

     cd oberon-risc-emu
     for x in *.Mod ; do ./pcreceive.sh $x ; sleep 0.5 ; done

Compile the provided test program with the ORIGINAL and then with the MODIFIED Oberon compiler:

     ORP.Compile Test.Mod ~           # compile the test program with the ORIGINAL compiler (1 error)
     ORP.Compile ORG.Mod/s ORP.Mod ~  # build the MODIFIED compiler
     System.Free ORP ORG ~            # unload the ORIGINAL compiler

     ORP.Compile Test.Mod ~           # compile the test program with the MODIFIED compiler (4 errors)

The difference is in the number of error messages produced - 1 for the original, 4 for the modified compiler.

------------------------------------------------------
**Differences to the official Oberon-07 compiler**

1. First, we recall that when a string is parsed, a string item x is created in *ORP.factor* using *ORG.MakeStringItem*, where *x.a* is set to the string buffer position (*strx*) and *x.b* to the string length (*len*). There are two cases: *declared* and *anonymous* string constants.

         MODULE M;
           CONST s* = "declared string";  (*creates a named type in the symbol table of the compiler*)
           VAR a: ARRAY 32 OF CHAR;
         BEGIN a := "anonymous string"
         END M.

2. The field *obj.lev* is no longer "abused" to hold the length (*len*) of string constants. Instead, the length of a string constant is now encoded and stored together with its string buffer position (*strx*) in the field *obj.val*. This adds a single line to procedure *ORP.Declarations*

        IF x.type.form = ORB.String THEN obj.val := x.a (*strx*) + x.b (*len*) * 100000H ELSE obj.val := x.a END

     We have chosen to use the the 20 least significant bits (bits 0-19) to hold the string buffer position *strx* and the 12 most significant bits (bits 20-31) to hold the string length *len* (allowing for a maximum string length of 2^12 = 4096). This allows us to use the *same* code for decoding *obj.val* in procedure *ORG.MakeItem*, regardless of whether the object *obj* represents a global string (in which case the 20 lowest-order bits represent the string buffer position *strx*) or an imported string (in which case these bits represent the exporting module's export number *exno*):

        x.a := y.val MOD 100000H; (*strx/exno*) x.b := y.val DIV 100000H (*len*)

3. The field *obj.lev* holding the scope level is now consistently set for *all* declared objects, i.e. also for *constants*, *types* and *procedures*, not just for *variables* (by convention, *obj.lev = 0* for *global* objects, *obj.lev = level > 0* for local objects, and *obj.lev = -mno < 0* for *imported* objects, see *ORB.Import*). This makes it possible to check *all* identifiers for a valid scope.

4. The check for a valid scope level has been moved from *ORG.MakeItem* to *ORP.qualident*. This disallows access to *all* intermediate identifiers, and also in *all* cases when an item is *used*, not just when it is initially *created*.

5. Intermediate *procedures* are excluded from the check, as we *want* them to remain accessible in nested scopes.

**[1]** We could have chosen to use 8 bits for the string length *len* (for a maximum string length of 2^8 = 256 characters, which would be in line with the current limitation *ORS.stringBufSize = 256* in the official Oberon-07 compiler) and 24 bits for the string buffer position *strx*. However, using 24 bits for the string buffer position *strx* (string address relative to the static base SB) is not needed, as memory instructions on RISC have an offset of only 20 bits anyway, limiting any offset from the SB to 2^20 bytes = 1 MB. By choosing to use 12 instead of 8 bits for the string length *len*, we maintain the option to either increase the maximum string length or to reuse 4 bits for other purposes, without imposing any restrictions on the offset from the SB (on RISC).

In the output of the following Unix-style *diff* commands, lines that begin with "<" are the old lines (i.e. code from the official Oberon-07 compiler), while lines that begin with ">" are the modified lines (i.e. code from *this* repository).

**$ diff FPGAOberon2013/ORG.Mod Oberon-no-access-to-intermediate-objects/Sources/FPGAOberon2013/ORG.Mod**

*ORG.MakeStringItem:*

```diff
240c240
<   BEGIN x.mode := ORB.Const; x.type := ORB.strType; x.a := strx; x.b := len; i := 0;
---
>   BEGIN x.mode := ORB.Const; x.type := ORB.strType; x.a := strx; x.b := len; x.r := 0; i := 0;
```

*ORG.MakeItem:*

```diff
252c252,253
<     ELSIF (y.class = ORB.Const) & (y.type.form = ORB.String) THEN x.b := y.lev  (*len*)
---
>     ELSIF (y.class = ORB.Const) & (y.type.form = ORB.String) THEN x.r := y.lev;
>       x.a := y.val MOD 100000H; (*strx / exno*) x.b := y.val DIV 100000H (*len*)
254,255c255
<     END ;
<     IF (y.lev > 0) & (y.lev # curlev) & (y.class # ORB.Const) THEN ORS.Mark("level error, not accessible") END
---
>     END
```

**$ diff FPGAOberon2013/ORP.Mod Oberon-no-access-to-intermediate-objects/Sources/FPGAOberon2013/ORP.Mod**

*ORP.qualident:*

```diff
39a40,41
>     ELSIF (obj.lev > 0) & (obj.lev # level) &
>       ((obj.class # ORB.Const) OR (obj.type.form # ORB.Proc)) THEN ORS.Mark("not accessible")
```
*ORP.Declarations:*

```diff
797,798c797,799
<         ORB.NewObj(obj, id, ORB.Const); obj.expo := expo;
<         IF x.mode = ORB.Const THEN obj.val := x.a; obj.lev := x.b; obj.type := x.type
---
>         ORB.NewObj(obj, id, ORB.Const); obj.expo := expo; obj.lev := level;
>         IF x.mode = ORB.Const THEN obj.type := x.type;
>           IF x.type.form = ORB.String THEN obj.val := x.a (*strx*) + x.b (*len*) * 100000H ELSE obj.val := x.a END
```

**APPENDIX A: An alternative approach (not implemented)**

The implementation described above is preferred if the modified value of *obj.val* needs to be written to the symbol file for other purposes. For an example use case see http://github.com/andreaspirklbauer/Oberon-importing-string-constants. An alternative implementation, which does *not* modify the semantics of the field *obj.val*, can be obtained as follows:

1. Instead of encoding and storing the length of a string constant together with its string buffer position (*strx*) in the field *obj.val*, the length of a string constant could be stored in the (byte-sized) field *obj.exno*. This is acceptable because the field *obj.exno* is not used for string constants and the length of a string is limited to 256 bytes (*ORS.stringBufSize* = 256).

2. The semantics of the field *obj.val* is not modified.

However, choosing this approach will no longer allow to implement importing string constants (where the field *obj.exno* is actually needed). Thus, we don't recommend this approach.

**$ diff FPGAOberon2013/ORG.Mod ORG.Mod**

*ORG.MakeItem:*

```diff
253c253
<     ELSIF (y.class = ORB.Const) & (y.type.form = ORB.String) THEN x.b := y.lev  (*len*)
---
>     ELSIF (y.class = ORB.Const) & (y.type.form = ORB.String) THEN x.b := y.exno  (*len*)
255,256c255
<     END ;
<     IF (y.lev > 0) & (y.lev # curlev) & (y.class # ORB.Const) THEN ORS.Mark("level error, not accessible") END
---
>     END
```

**$ diff FPGAOberon2013/ORP.Mod ORP.Mod**

*ORP.qualident:*

```diff
39a40,41
>     ELSIF (obj.lev > 0) & (obj.lev # level) &
>       ((obj.class # ORB.Const) OR (obj.type.form # ORB.Proc)) THEN ORS.Mark("level error, not accessible")
```

*ORP.Declarations:*

```diff
799,800c801,802
<         ORB.NewObj(obj, id, ORB.Const); obj.expo := expo;
<         IF x.mode = ORB.Const THEN obj.val := x.a; obj.lev := x.b; obj.type := x.type
---
>         ORB.NewObj(obj, id, ORB.Const); obj.expo := expo; obj.lev := level;
>         IF x.mode = ORB.Const THEN obj.val := x.a; obj.exno := x.b (*len*); obj.type := x.type
```

**APPENDIX B: Implementing relaxed shadowing rules**

To implement the relaxed *shadowing* rules outlined above, i.e. to allow access to the global level *if* a referenced object is declared at an intermediate, but not at the strictly local level, the following changes need to be made:

First, revert back to the original procedure *ORP.qualident* (by removing the two lines inserted above) to arrive at:

     PROCEDURE qualident(VAR obj: ORB.Object);
     BEGIN obj := ORB.thisObj(); ORS.Get(sym);
       IF obj = NIL THEN ORS.Mark("undef"); obj := dummy END ;
       IF (sym = ORS.period) & (obj.class = ORB.Mod) THEN
         ORS.Get(sym);
         IF sym = ORS.ident THEN obj := ORB.thisimport(obj); ORS.Get(sym);
           IF obj = NIL THEN ORS.Mark("undef"); obj := dummy END
         ELSE ORS.Mark("identifier expected"); obj := dummy
         END
       END
     END qualident;

Second, change procedure *ORB.thisObj* to the following code (innermost IF statement added):

     PROCEDURE thisObj*(): Object;
      VAR s, x: Object;
     BEGIN s := topScope;
       REPEAT x := s.next;
         WHILE (x # NIL) & (x.name # ORS.id) DO x := x.next END ;
         IF (x # NIL) & (s # topScope) & (x.lev > 0) & (x.class # Mod) &        (*<------- added*)
            ((x.class # Const) OR (x.type.form # Proc)) THEN
           x := NIL
         END ;
         s := s.dsc
       UNTIL (x # NIL) OR (s = NIL);
       RETURN x
     END thisObj;

