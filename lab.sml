S f <*> S a = S (f a)
E   <*> S a = E
S f <*> E   = E
E   <*> E   = E

f <$> S a =  succeed f <*> S a
f <$> E   =  succeed f <*> E

map f (S a) = S b
map f E     = E

join E         = E
join (S E)     = succeed E
join (S (S a)) = succeed succeed a


E   >>= k  =  E
S a >>= k  =  k a

(f >=> g) a =  g b when f a = succeed b
(f >=> g) a =  E when f a = E

___LAB________

'a producer: input list -> ('a error * input list) option

6.
val <|> : 'a producer * 'a producer -> 'a producer
 
(p1 <|> p2) ts == SOME(OK a, ts'), when p1 ts succeeds
(p1 <|> p2) ts == SOME(OK a, ts'), when p1 fails and p2 succeeds
(p1 <|> p2) ts == SOME(ERROR msg, ts'), when p1 errors
(p1 <|> p2) ts == NONE, when both fail

sat predicate p ts = SOME(OK a, ts'), when p ts succeeds and predicate a is true
sat predicate p ts = NONE, when p ts succeeds and predicate a is false
sat predicate p ts = NONE , when p ts fails
sat predicate p ts = SOME(ERROR msg, ts'), when p ts errors



val succeed : 'a -> 'a producer
val cons: 'a * 'a list -> 'a list
val <$> : ('a -> 'b) * 'a producer -> 'b producer
val <*>  : ('a -> 'b) producer * 'a producer -> 'b producer
'a producer: input list -> ('a error * input list) option
val curry cons: 'a list -> 'a list
val many : 'a producer -> 'a list producer

7.
many p == cons <$> p <*> many p <|> succeed []
 
many1 p == cons <$> p <*> many p

optional p == succeed [] <|> cons <$> p

count 

8.

curry op :: : 

many p
== curry op :: 



<instr> ::= <objcode> \n
		   | <labelop> \n
		   | <loadfun> \n
<objcode> ::= halt
		   | print $<reg>
		   | $<reg> := <lit>
	 	   | check <lit>, <reg>
		   | expect <lit>, <reg>
		   | $<reg> := $<reg> + $<reg>
		   | $<reg> := $<reg> - $<reg>
		   | $<reg> := $<reg> / $<reg>
		   | $<reg> := $<reg> * $<reg>

		   | $<reg> := function? $<reg>
		   | $<reg> := pair? $<reg>
		   | $<reg> := symbol? $<reg>
		   | $<reg> := number? $<reg>
		   | $<reg> := boolean? $<reg>
		   | $<reg> := null? $<reg>
		   | $<reg> := nil? $<reg>

		   | $<reg> := cdr $<reg>
		   | $<reg> := car $<reg>
		   
		   | $<reg> := $<reg> cons $<reg>
		   | $<reg> := $<reg> = $<reg>
		   | $<reg> := $<reg> > $<reg>
		   | $<reg> := $<reg> < $<reg>
		   | $<reg> := $<reg> < $<reg>
		   | $<reg> := $<reg> idiv $<reg>
		   | $<reg> := $<reg> idiv $<reg>
		   | $<reg> := hash $<reg>

		   | error $<reg>
		   | printu $<reg>
		   | println $<reg>

		   | ! $<reg>
		   | zero $<reg>
		   | globals[<lit>] := $<reg>
		   | $<reg> := globals[<lit>]
		   | makeconscell $<reg>
		   | projectbool $<reg>

<labelop> ::= <label> :
		   | goto <label>
		   | if $<reg> goto <label>
<loadfun> ::= $<reg> := function arity {instructions}
  <label> ::= STRING
    <reg> ::= rK, where K is an interger literal in range 0-255
    <lit> ::= INT | REAL | BOOL | STRING | EMPTYLIST | NIL