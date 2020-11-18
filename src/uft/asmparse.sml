(* A parser for assembly language *)

(* You'll get a partially complete version of this file, 
  which you'll need to complete. *)

structure AsmParse :>
  sig
    val parse  : AsmLex.token list list -> AssemblyCode.instr list Error.error
    val unparse1 : AssemblyCode.instr -> string 
    val unparse : AssemblyCode.instr list -> string list (* use me if there are functions *)
    (* type line = string one line of assembly code *)
  end
  =
struct
  (* visualize list of tokens using Unicode middle dot as separator *)
  fun showTokens ts = "[" ^ String.concatWith "\194\183" (map AsmLex.unparse ts) ^ "]"

  structure P = MkListProducer (val species = "parser"
                                type input = AsmLex.token
                                val show = showTokens
                               )
      (* P for parser; builds a module that takes AsmLex.token as input *)

  structure L = AsmLex
  structure A = AssemblyCode
  structure O = ObjectCode

  (* Operations on producers: Wishing for Modula-style FROM IMPORT here ... *)
  infix 3 <*>      val op <*> = P.<*>
  infixr 4 <$>     val op <$> = P.<$>
  infix 3 <~>      val op <~> = P.<~>
  infix 1 <|>      val op <|> = P.<|>
  infix 3 >>        val op >> = P.>>

  val succeed = P.succeed
  val curry = P.curry
  val curry3 = P.curry3
  val id = P.id
  val fst = P.fst
  val snd = P.snd
  val many = P.many
  val many1 = P.many1
  val sat = P.sat
  val one = P.one
  val notFollowedBy = P.notFollowedBy
  val eos = P.eos
  fun flip f x y = f y x

  (* utilities *)

  fun eprint s = TextIO.output (TextIO.stdErr, s)

  type 'a parser = 'a P.producer

  (* always-error parser; useful for messages *)
  fun expected what =
    let fun bads ts = Error.ERROR ("looking for " ^ what ^
                                   ", got this input: " ^ showTokens ts)
    in  P.check ( bads <$> many one )
    end

  (* always-succeed parser; prints msg when run *)
  fun debug msg =
      P.ofFunction (fn ts => (app eprint ["@> ", msg, "\n"]; SOME (Error.OK (), ts)))

  (* make another parser chatter on entry *)
  val verbose : string -> 'a parser -> 'a parser
   = (fn msg => fn p => debug msg >> p)

  val veryVerbose : string -> 'a parser -> 'a parser
      = (fn what => fn p =>
           let fun shout s = app eprint ["looking for ", what, s, "\n"]
           in  P.ofFunction (fn ts =>
                                let val _ = shout "..."
                                    val answer = P.asFunction p ts
                                    val _ =
                                        case answer
                                          of NONE => shout ": failed"
                                           | SOME (Error.ERROR _, _) => shout ": errored"
                                           | SOME (Error.OK _, _) => shout ": succeeded"
                                in  answer
                                end)
           end)

  (****************************************************************************)

  (**** parsers for common tokens ****)

      (* These are your workhorse parsers---the analog of the `get`
         functions from the `tokens.h` interface in the SVM *)

(* TODO: '() not one token so cannot use name to parse *)
(* TODO: Test all kinds of literals *)

  val bool       = P.maybe (fn (L.NAME "true")    => SOME true
                               | (L.NAME "false") => SOME false
                               | _ => NONE) one
  val emptylist       = P.maybe (fn (L.NAME "'()") => SOME O.EMPTYLIST
                               | _ => NONE) one

  val nil'       = P.maybe (fn (L.NAME "nil") => SOME O.NIL
                               | _ => NONE) one
  val real       = P.maybe (fn (L.INT n) => SOME (Real.fromInt n)
                               | _ => NONE) one
  val int       = P.maybe (fn (L.INT   n)    => SOME n  | _ => NONE) one
  val name      = P.maybe (fn (L.NAME  n)    => SOME n  | _ => NONE) one
  val string    = P.maybe (fn (L.STRING s)   => SOME s  | _ => NONE) one
  val reg       = P.maybe (fn (L.REGISTER n) => SOME n  | _ => NONE) one
  val eol       = P.maybe (fn (L.EOL)        => SOME () | _ => NONE) one

  val literal : O.literal P.producer
   = O.INT <$> int <|> O.BOOL <$> bool <|> O.REAL <$> real <|> emptylist <|> O.STRING <$> string <|> nil'
   
  (* turn any single-token string into a parser for that token *)
  fun the "\n" = eol
    | the s =
        case AsmLex.tokenize s
          of Error.OK [t, AsmLex.EOL] => sat (P.eq t) one >> succeed ()
           | _ => Impossible.impossible "non-token in assembler parser"

  fun kw s = sat (P.eq s) name  (* keyword; example: kw "goto" *)


  (***** instruction-building functions for parsers ****)

  fun regs operator operands = A.OBJECT_CODE (O.REGS (operator, operands))
  fun regs_lit operator operands lit = A.OBJECT_CODE (O.REGSLIT (operator, operands, lit))
  fun def_label lab = A.DEFLABEL lab
  fun goto_label lab = A.GOTO_LABEL lab
  fun if_goto_label r1 lab = A.IF_GOTO_LABEL (r1, lab)
  fun loadfunc r1 arity instrs = A.LOADFUNC (r1, arity, instrs)

     (* curried instruction builder *)

  fun eR0 operator          = regs operator []
  fun eR1 operator r1       = regs operator [r1]
  fun eR2 operator r1 r2    = regs operator [r1, r2]
  fun eR3 operator r1 r2 r3 = regs operator [r1, r2, r3]
  fun eR1U16 operator r1 lit = regs_lit operator [r1] lit
  fun eR1U16_switch operator lit r1 = regs_lit operator [r1] lit

  (***** toy parser for you to extend ****)
  (* <$> : ('a -> 'b) * 'a producer -> 'b producer
  <*> : ('a -> 'b) producer * 'a producer -> 'b producer
 eR3 "+" : 'a -> 'c -> 'd -> instr
 eR3 "+" <$> : ('c -> 'd -> instr) producer 
 ('d -> instr) producer
 eR1 "print": 'a -> instruction 
 
eR1 "print" <$> reg : instruction producer *)

(* 'a -> 'b -> instr
'b -> instr producer *)

  (* parser to read an instruction /without/ reading end of line *)
  val one_line_instr : A.instr P.producer 
     =  kw "@" >> regs <$> name <*> many int  (* "escape hatch" syntax *)
    <|> eR3 "+" <$> reg <~> the ":=" <*> reg <~> the "+" <*> reg
    <|> eR3 "-" <$> reg <~> the ":=" <*> reg <~> the "-" <*> reg
   <|> eR3 "/" <$> reg <~> the ":=" <*> reg <~> the "/" <*> reg
   <|> eR3 "*" <$> reg <~> the ":=" <*> reg <~> the "*" <*> reg
    <|> eR3 "=" <$> reg <~> the ":=" <*> reg <~> the "=" <*> reg

    <|> eR3 "call" <$> reg <~> the ":=" <~> the "call" <*> reg <~> the "(" <~> reg <~> the "," <~> the "..." <~> the "," <*> reg <~> the ")"

   <|> the "return" >> eR1 "return" <$> reg
   <|> eR2 "mov" <$> reg <~> the ":="  <*> reg 

   <|> the "print" >> eR1 "print" <$> reg

   <|> the "!" >> eR1 "!" <$> reg
   <|> the "zero" >> eR1 "zero" <$> reg
   <|> the "makeconscell" >> eR1 "makeconscell" <$> reg
   <|> the "projectbool" >> eR1 "projectbool" <$> reg
   <|> the "halt" >> succeed (eR0 "halt") 
   <|> def_label <$> name <~> the ":"
   <|> the "goto" >> goto_label <$> name
   <|> the "if" >> if_goto_label <$> reg <~> the "goto" <*> name 
   <|> eR1U16 "loadliteral" <$> reg <~> the ":=" <*> literal
   <|> the "check" >> eR1U16_switch "check" <$> literal <~> the "," <*> reg
   <|> the "expect" >> eR1U16_switch "expect" <$> literal <~> the "," <*> reg
   <|> eR1U16 "getglobal" <$> reg <~> the ":=" <~> the "globals" <~> the "[" <*> literal <~> the "]"
   <|> the "globals" >> the "[" >> eR1U16_switch "setglobal" <$> literal <~> the "]" <~> the ":=" <*> reg
   <|> the "tailcall" >> eR2 "tailcall" <$> reg <~> the "(" <~> reg <~> the "," <~> the "..." <~> the "," <*> reg <~> the ")"
   <|> eR2 "function?" <$> reg <~> the ":=" <~> the "function?" <*> reg 
   <|> eR2 "pair?" <$> reg <~> the ":=" <~> the "pair?" <*> reg 
   <|> eR2 "symbol?" <$> reg <~> the ":=" <~> the "symbol?" <*> reg 
   <|> eR2 "number?" <$> reg <~> the ":=" <~> the "number?" <*> reg 
   <|> eR2 "boolean?" <$> reg <~> the ":=" <~> the "boolean?" <*> reg 
   <|> eR2 "null?" <$> reg <~> the ":=" <~> the "null?" <*> reg 
   <|> eR2 "nil?" <$> reg <~> the ":=" <~> the "nil?" <*> reg 

   <|> eR2 "cdr" <$> reg <~> the ":=" <~> the "cdr" <*> reg 
   <|> eR2 "car" <$> reg <~> the ":=" <~> the "car" <*> reg 
   <|> eR2 "hash" <$> reg <~> the ":=" <~> the "hash" <*> reg 

   <|> eR3 "cons" <$> reg <~> the ":=" <*> reg <~> the "cons" <*> reg
   <|> eR3 "=" <$> reg <~> the ":=" <*> reg <~> the "=" <*> reg
   <|> eR3 ">" <$> reg <~> the ":=" <*> reg <~> the ">" <*> reg
   <|> eR3 "<" <$> reg <~> the ":=" <*> reg <~> the "<" <*> reg
   <|> eR3 "idiv" <$> reg <~> the ":=" <*> reg <~> the "idiv" <*> reg

   <|> eR3 "mkclosure" <$> reg <~> the ":=" <~> the "closure" <~> the "[" <*> reg <~> the "," <*> reg <~> the "]"
   <|> eR3 "getclslot" <$> reg <~> the ":=" <*> reg <~> the "." <*> reg
   <|> eR3 "getclslot" <$> reg <~> the "." <*> reg <~> the ":=" <*> reg

   <|> the "error" >> eR1 "error" <$> reg
   <|> the "printu" >> eR1 "printu" <$> reg
   <|> the "println" >> eR1 "println" <$> reg

    (* You add cases here.  Put cases with longer inputs first, e.g.,
          r1 := r2 + r3
       before
          r1 := r3
     *)    

   (**** recursive parser that handles end-of-line and function loading ****)

   (* Parsers for start and end of "load function", for you to write.
      Designing syntax so each one terminates with `eol` is recommended. *)

   fun loadfunc (reg, arity) body = A.LOADFUNC (reg, arity, body)
   val loadfunStart : (int * int) parser = (* fill in with (reg * arity) parser *)
        P.pair <$> reg <~> the ":=" <~> the "function" <*> int <~> the "{"
   val loadfunEnd : unit parser =
         the "}" >> eol

   (* grammar :   <instruction> ::= <one_line_instruction> EOL
                                 | <loadfunStart> {<instruction>} <loadfunEnd> *)

   (* simple parser with no error detection *)
   val instruction : A.instr Error.error parser
     = Error.OK <$>
       P.fix (fn instruction : A.instr parser =>
                one_line_instr <~> many1 eol
            <|> loadfunc <$> loadfunStart <*> many instruction <~> loadfunEnd
        )

   (* A better parser is juiced up with extra error detection *)

   fun badTokens ts = Error.ERROR ("unrecognized assembly line: " ^ showTokens ts)
   val nonEOL = sat (curry op <> L.EOL) one  (* any token except EOL *)

   val instruction : A.instr Error.error parser
     = P.fix (fn instruction =>
           Error.OK <$> one_line_instr <~> eol
       <|> Error.OK <$>
           (loadfunc <$> loadfunStart <*> 
                         P.check (Error.list <$> many instruction) <~>
                         loadfunEnd)
       <|> P.notFollowedBy loadfunEnd >>
           (* gobble to end of line, then succeed by producing error message: *)
           badTokens <$> many nonEOL <~> eol  
        )


val parse =
  Error.join o
  P.produce (Error.list <$> (many eol >> many instruction)) o
  List.concat            

  (*************************** unparsing *****************************)

  val int = Int.toString
  fun reg r = "$r" ^ int r
  fun label l = "L" ^ int l ^ ":"
  val spaceSep = String.concatWith " "

(* TODO: clean up parser *)

  fun unparse_lit (O.STRING s) = StringEscapes.quote s
    | unparse_lit (x) = String.concat (ObjectUnparser.literal x)


  fun unparse1 (A.OBJECT_CODE (O.REGS ("+", [x, y, z]))) =
        spaceSep [reg x, ":=", reg y, "+", reg z]
    | unparse1 (A.OBJECT_CODE (O.REGS ("-", [x, y, z]))) =
            spaceSep [reg x, ":=", reg y, "-", reg z]
    | unparse1 (A.OBJECT_CODE (O.REGS ("/", [x, y, z]))) =
            spaceSep [reg x, ":=", reg y, "/", reg z]
    | unparse1 (A.OBJECT_CODE (O.REGS ("*", [x, y, z]))) =
            spaceSep [reg x, ":=", reg y, "*", reg z]
    | unparse1 (A.OBJECT_CODE (O.REGS ("call", [x, y, z]))) =
            spaceSep [reg x, ":=", "call", reg x, "(", reg y, "...", reg z, ")"]
    | unparse1 (A.OBJECT_CODE (O.REGS ("tailcall", [x, y]))) =
            spaceSep ["tailcall", reg x, "(", reg x, "...", reg y, ")"]
    | unparse1 (A.OBJECT_CODE (O.REGS ("print", [x]))) =
            spaceSep ["print", reg x]
    | unparse1 (A.OBJECT_CODE (O.REGS ("return", [x]))) =
            spaceSep ["return", reg x]
    | unparse1 (A.OBJECT_CODE (O.REGS ("halt", []))) = 
            "halt"
    | unparse1 (A.OBJECT_CODE (O.REGS ("!", [x]))) = 
            spaceSep ["!", reg x]
    | unparse1 (A.OBJECT_CODE (O.REGS ("zero", [x]))) = 
            spaceSep ["zero", reg x]
    | unparse1 (A.OBJECT_CODE (O.REGS ("makeconscell", [x]))) = 
            spaceSep ["makeconscell", reg x]
    | unparse1 (A.OBJECT_CODE (O.REGS ("projectbool", [x]))) = 
            spaceSep ["projectbool", reg x]
   | unparse1 (A.OBJECT_CODE (O.REGSLIT ("loadliteral", [x], y))) = 
            spaceSep [reg x, ":=", unparse_lit y]
    | unparse1 (A.OBJECT_CODE (O.REGSLIT ("check", [x], y))) = 
            spaceSep ["check", unparse_lit y, ",", reg x]
    | unparse1 (A.OBJECT_CODE (O.REGSLIT ("expect", [x], y))) = 
            spaceSep ["expect", unparse_lit y, ",", reg x]
    | unparse1 (A.OBJECT_CODE (O.REGSLIT ("getglobal", [x], y))) = 
            spaceSep [String.concat [reg x, ":=", "globals[", unparse_lit y, "]"]] 
    | unparse1 (A.OBJECT_CODE (O.REGSLIT ("setglobal", [x], y))) = 
            spaceSep [String.concat ["globals[", unparse_lit y, "]"], ":=", reg x] 
    | unparse1 (A.OBJECT_CODE (O.GOTO x)) =
            spaceSep ["goto", label x]
    | unparse1 (A.OBJECT_CODE (O.REGINT ("regint", x, y, i))) =
            Impossible.impossible "no regint"
    | unparse1 (A.GOTO_LABEL s) =
            spaceSep ["goto", s]
    | unparse1 (A.DEFLABEL l) =
            spaceSep [l, ":"]
    | unparse1 (A.IF_GOTO_LABEL (x, l)) =
            spaceSep ["if", reg x, "goto", l]
    | unparse1 (A.OBJECT_CODE (O.REGS ("mov", [x, y]))) =
            spaceSep [reg x, ":=", reg y]
    | unparse1 (A.OBJECT_CODE (O.REGS ("function?", [x, y]))) =
            spaceSep [reg x, ":=", "function?", reg y]
    | unparse1 (A.OBJECT_CODE (O.REGS ("pair?", [x, y]))) =
            spaceSep [reg x, ":=", "pair?", reg y]
   | unparse1 (A.OBJECT_CODE (O.REGS ("symbol?", [x, y]))) =
            spaceSep [reg x, ":=", "symbol?", reg y]
   | unparse1 (A.OBJECT_CODE (O.REGS ("number?", [x, y]))) =
            spaceSep [reg x, ":=", "number?", reg y]
   | unparse1 (A.OBJECT_CODE (O.REGS ("boolean?", [x, y]))) =
            spaceSep [reg x, ":=", "boolean?", reg y]
   | unparse1 (A.OBJECT_CODE (O.REGS ("null?", [x, y]))) =
            spaceSep [reg x, ":=", "null?", reg y]
   | unparse1 (A.OBJECT_CODE (O.REGS ("nil?", [x, y]))) =
            spaceSep [reg x, ":=", "nil?", reg y]
   | unparse1 (A.OBJECT_CODE (O.REGS ("cdr", [x, y]))) =
            spaceSep [reg x, ":=", "cdr", reg y]
   | unparse1 (A.OBJECT_CODE (O.REGS ("car", [x, y]))) =
            spaceSep [reg x, ":=", "car", reg y]
   | unparse1 (A.OBJECT_CODE (O.REGS ("hash", [x, y]))) =
            spaceSep [reg x, ":=", "hash", reg y]

    | unparse1 (A.OBJECT_CODE (O.REGINT ("mkclosure", x, y, k))) =
            spaceSep [reg x, ":=", "closure", "[", reg y, ",", Int.toString k, "]"]
    | unparse1 (A.OBJECT_CODE (O.REGINT ("getclslot", x, y, k))) =
            spaceSep [reg x, ":=", reg y, ".", Int.toString k]
    | unparse1 (A.OBJECT_CODE (O.REGINT ("setclslot", x, y, k))) =
            spaceSep [reg x, ".", Int.toString k, ":=", reg y]

    | unparse1 (A.OBJECT_CODE (O.REGS ("cons", [x, y, z]))) =
            spaceSep [reg x, ":=", reg y, "cons", reg z]
   | unparse1 (A.OBJECT_CODE (O.REGS ("=", [x, y, z]))) =
            spaceSep [reg x, ":=", reg y, "=", reg z]
   | unparse1 (A.OBJECT_CODE (O.REGS (">", [x, y, z]))) =
            spaceSep [reg x, ":=", reg y, ">", reg z]
   | unparse1 (A.OBJECT_CODE (O.REGS ("<", [x, y, z]))) =
            spaceSep [reg x, ":=", reg y, "<", reg z]
   | unparse1 (A.OBJECT_CODE (O.REGS ("idiv", [x, y, z]))) =
            spaceSep [reg x, ":=", reg y, "idiv", reg z]
    | unparse1 (A.OBJECT_CODE (O.REGS ("error", [x]))) =
            spaceSep ["error", reg x]
   | unparse1 (A.OBJECT_CODE (O.REGS ("printu", [x]))) =
            spaceSep ["printu", reg x]
   | unparse1 (A.OBJECT_CODE (O.REGS ("println", [x]))) =
            spaceSep ["println", reg x]
    | unparse1 _ = "an unknown assembly-code instruction"
  

               
  (* val unparse : AssemblyCode.instr list -> string list *)

                (* LOADFUNC (r, k, body) means:
                    - body describes a function
                      that expects k parameters
                    - capture those instructions and insert the function
                      into the literal pool
                    - emit an instruction to load that literal into register r
               *)
fun unparse [] = []
| unparse (A.OBJECT_CODE (O.LOADFUNC (r, k, instr)) :: ks) = [spaceSep [reg r, ":=", "function", int k, String.concat ["{",String.concat ( map (fn i => unparse1 (A.OBJECT_CODE i)) instr), "}"]]] @ unparse ks 
| unparse (A.LOADFUNC (r, k, instr) :: ks) = [spaceSep [reg r, ":=", "function", int k, String.concat ["{", String.concat (unparse instr), "}"]]] @ unparse ks
| unparse (l :: ls) = [String.concat [unparse1 l, "\n"]] @ unparse ls


end