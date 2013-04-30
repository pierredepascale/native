;;; fixnum
("fx+1" (%fx+1 4) "5")
("fixnum? on fixnum" (%fixnum? 1) "#t")
("fixnum? on char" (%fixnum? #\a) "#f")
("fx-1+" (%fx-1+ 5) "4")
("fx= of same fx" (%fx= 1 1) "#t")
("fx= of diff fx" (%fx= 1 2) "#f")
("fxzero? of zero" (%fxzero? 0) "#t")
("fxzero? of one" (%fxzero? 1) "#f")
("fx< correct" (%fx< 1 2) "#t")
("fx< incorrect" (%fx< 2 1) "#f")
("fx< of same number" (%fx< 2 2) "#f")
("fx+" (%fx+ 1 2) "3")
("fx-" (%fx- 3 2) "1")

;;; boolean
("not on fx" (%not 0) "#f")
("not false" (%not #f) "#t")
("not true" (%not #t) "#f")
("boolean? on true" (%boolean? #t) "#t")
("boolean? on false" (%boolean? #f) "#t")
("boolean? on fixnum" (%boolean? 123) "#f")

;;; character
("char? on char" (%char? #\a) "#t")
("char? on fixnum" (%char? 123) "#f")

("fixnum to char" (%fixnum->char 65) "#\\A")

;;; pair
("null? on null" (%null? '()) "#t")
("null? on fixnum" (%null? 123) "#f")

("pair? on pair" (%pair? '(1 2)) "#t")
("pair? on fixnum" (%pair? 12) "#f")
("pair? on null" (%pair? '()) "#f")

("cons primitive" (%cons 1 '()) "(1)")

("car primitive" (%car '(1 2)) "1")
("cdr primitive" (%cdr '(1 2)) "(2)")

;;; vector
("vector? on vector" (%vector? '#(1 2 3)) "#t")
("vector? on fixnum" (%vector? 123) "#f")

;;; closure

("closure? on closure" (%closure? (lambda () 1)) "#t")
("closure? on fixnum" (%closure? 123) "#f")

;;; string

;;; symbol
("symbol? on symbol" (%symbol? 'abc) "#t")
("symbol? on fixnum" (%symbol? 1234) "#f")

