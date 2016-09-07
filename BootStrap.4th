1024 DUP * 256 * MEM_SIZE !
restart!

: IMMEDIATE LAST 8 + DUP C@ 1 OR SWAP C! ;
: \ (source) @ C@ >IN ! ; IMMEDIATE
: ] 1 STATE ! ; : [ 0 STATE ! ; IMMEDIATE : ?] STATE @ ;
: NIP SWAP DROP ; : TUCK SWAP OVER ; : -ROT ROT ROT ;

: 0= 0 = ; : 0< 0 < ; : 0> 0 > ;
: <> = 0= ; : <= > 0= ; : >= < 0= ;
: 4* 2* 2* ; : 4/ 2/ 2/ ; 
: 2+ 1+ 1+ ; : 2- 1- 1- ;
: BETWEEN 2 PICK >= -ROT >= = ; 
: TRUE -1 ; : FALSE 0 ;

: I_FETCH 1 ; : I_STORE 2 ; : I_LITERAL 3 ; : I_DUP 4 ; : I_SWAP 5 ;
: I_DROP 6 ; : I_PICK 7 ; : I_ROT 8 ; : I_ONEPLUS 9 ; : I_PLUS 10 ;
: I_MINUS 11 ; : I_MULT 12 ; : I_DIV 13 ; : I_EQ 14 ; : I_CALL 15 ;
: I_LT 16 ; : I_GT 17 ; : I_TO_R 18 ; : I_R_FROM 19 ; : I_R_AT 20 ;
: I_JMPZ 21 ; : I_LOOKUP 22 ; : I_X86 23 ; : I_JMPNZ 24 ; : I_SLITERAL 25 ;
: I_DEPTH 26 ; : I_ONEMINUS 27 ; : I_AND 28 ; : I_OR 29 ; : I_XOR 30 ;
: I_DIVMOD 31 ; : I_EMIT 32 ; : I_DICTP 33 ; : I_TWOTIMES 34 ; : I_TWODIV 35 ;
: I_FOPEN 36 ; : I_FCLOSE 37 ; : I_FREAD 38 ; : I_FWRITE 39 ; : I_FGETC 40 ;
: I_FETCH_8 41 ; : I_STORE_8 42 ; : I_LITERAL_8 43 ; : I_COMMA 44 ; : I_COMMA_8 45 ;
: I_OVER 46 ; : I_STRCMP 47 ; : I_STRCMPI 48 ; : I_CREATE 49 ; : I_WORD 50 ;
: I_INC 51 ; : I_GOTO 52 ; : I_EXECUTE 53 ; : I_DOT 54 ; : I_ABORT 55 ; : I_TYPE 56 ;
: I_COLON 57 ; : I_SEMICOLON 58 ; : I_PLUSSTORE 59 ; : I_MINUSSTORE 60 ; 
: I_BREAK 98 ; : I_RETURN 99 ;

: :NONAME HERE 1 STATE ! ; : NONAME; I_RETURN C, 0 STATE ! ; IMMEDIATE

: BEGIN HERE ; IMMEDIATE : AGAIN I_GOTO C, , ; IMMEDIATE
: WHILE I_JMPNZ C, , ; IMMEDIATE : UNTIL I_JMPZ C, , ; IMMEDIATE

: DO I_SWAP C, HERE I_TO_R C, I_TO_R C, ; IMMEDIATE
: I I_R_AT C, ; IMMEDIATE
: LOOP I_R_FROM C, I_ONEPLUS C, I_R_FROM C, I_OVER C, I_OVER C, I_LT C, I_JMPZ C, HERE SWAP 0 , I_GOTO C, , HERE SWAP ! I_DROP C, I_DROP C, ; IMMEDIATE
: LEAVE I_R_FROM C, I_R_FROM C, I_DROP C, I_DUP C, I_TO_R C, I_TO_R C, ; IMMEDIATE

: IF I_JMPZ C, HERE 0 , ; IMMEDIATE
: ELSE I_GOTO C, HERE SWAP 0 , HERE SWAP ! ; IMMEDIATE
: THEN HERE SWAP ! ; IMMEDIATE

: NEGATE 0 SWAP - ; : ABS DUP 0 < IF NEGATE THEN ;
: OFF 0 SWAP ! ; : ON -1 SWAP ! ;
: BINARY 2 BASE ! ; : OCTAL 8 BASE ! ; : DECIMAL 10 BASE ! ; : HEX 16 BASE ! ;
: 2DROP DROP DROP ; : 2DUP OVER OVER ; : ?DUP DUP IF DUP THEN ;
: MOD 2DUP / * - ; : SQ DUP * ;

: .incb. DUP C@ 1+ SWAP C! ;
: >S DUP .incb. DUP C@ + C! ; \ ( c addr -- ) - append c to counted string at addr
: collect PAD2 OFF >IN .INC. \ ( char -- )
	BEGIN (SOURCE) @ 1+ >IN @ + C@ DUP 0=
		IF 2DROP EXIT THEN   \ END OF LINE
		>IN .INC. 2DUP =
		IF 2DROP EXIT THEN   \ found closing char
		PAD2 >S
	AGAIN ;

: compile-num DUP 0 255 between IF I_LITERAL_8 C, C, ELSE I_LITERAL C, , THEN  ;
: compile-call I_CALL C, , ;

: CHAR WORD 1+ C@ ; : [CHAR] CHAR ?] IF compile-num THEN ; IMMEDIATE
: ( [CHAR] ) COLLECT ; IMMEDIATE

: ALLOT ( n -- ) HERE + (HERE) ! ;
: CREATE word create.empty I_LITERAL C, HERE 0 , I_RETURN C, HERE SWAP ! ;
: >DOES> HERE 6 - (HERE) ! ;
: VARIABLE CREATE 0 , ;
: CONSTANT ( n -- ) word create.empty compile-num I_RETURN C, ;
: FIELD + DUP CONSTANT ;
0 CELL+ CONSTANT CELL
: CELLS CELL * ; CELL NEGATE CONSTANT -CELL

: .dec. ( addr -- ) -1 SWAP +! ; : .dec4. -4 swap +! ; 
: .inc4. 4 SWAP +! ;

VARIABLE tSP
CREATE tStack 64 CELLS ALLOT
: tStack.Reset tStack tSP ! ;
: T@ ( -- n ) tSP @ @ ;
: T! ( n -- ) tSP @ ! ;
: >T ( n -- ) CELL tSP +! T! ;
: T> ( -- n ) T@ -CELL tSP +! ;
: tDEPTH ( -- n ) tSP @ tStack - 2/ 2/ ;
tStack.Reset

CREATE cStack 64 CELLS ALLOT
: cStack.Reset cStack dup ! ;
: CS@ ( -- n ) cStack @ @ ;
: CS! ( n -- ) cStack @ ! ;
: >CS ( n -- ) CELL cStack +! CS! ;
: CS> ( -- n ) CS@ -CELL cStack +! ;
: csDEPTH ( -- n ) cStack DUP @ SWAP - 2/ 2/ ;
cStack.Reset

: COUNT DUP 1+ SWAP C@ ; : CTYPE COUNT TYPE ;

: BL 32 ; : TAB 9 ; : LF 10 ;
: SPACE BL EMIT ; : .TAB TAB EMIT ; : CR 13 EMIT LF EMIT ;
: SPACES 0 DO SPACE LOOP ;

: Word.Body 4 + @ ;
: Word.Flags 8 + ;
: Word.Name 9 + ;
: Word.Name? Word.name CTYPE ;
: Word.?Immediate Word.Flags C@ 1 AND ;
: Words.GetNext @ ;
: Words.Count 0 >T LAST BEGIN T@ 1+ T! Words.GetNext DUP Words.GetNext WHILE DROP T> ;
: Words.Find WORD lookup ;
: Words LAST BEGIN DUP Word.Name? 2 SPACES Words.GetNext DUP Words.GetNext WHILE DROP ;

: mem_start (memory) @ ; : mem_end mem_start mem_size @ + ;

: Last? LAST Word.name? ;
: Words.Start 0 >T LAST BEGIN DUP T! Words.GetNext DUP Words.GetNext WHILE DROP T> ;
: Code.Start mem_start ;
: Dict.Stats Words.Start Words.GetNext LAST - DUP Words.Count / SWAP ;
: Code.Stats HERE Code.Start - DUP Words.Count / SWAP ;
: Words.Stats Code.Stats NIP Dict.Stats NIP + Words.Count 2DUP / SWAP ROT ;

: CMOVE 0 DO OVER C@ OVER C! 1+ SWAP 1+ SWAP LOOP 2DROP ; \ ( from to count -- )
: MOVE  0 DO OVER @ OVER ! 4 + SWAP 4 + SWAP LOOP 2DROP ;

: COPY.STRING OVER C@ 1+ CMOVE ; \ ( from to -- )
: string, ( addr num -- ) DUP C, 0 DO DUP C@ C, 1+ LOOP DROP 0 C, ;

: FORGET Words.Find ?DUP IF DUP Word.Body (HERE) ! Words.GetNext (LAST) ! THEN ;
: ' Words.Find ?dup if Word.body then ;
: ['] ' ; IMMEDIATE
: ForgetLast LAST Word.Body (HERE) ! LAST Words.GetNext (LAST) ! ;

: .( [CHAR] ) COLLECT PAD2 CTYPE ; IMMEDIATE
 
: S" [CHAR] " collect PAD2
	?] IF 
		I_SLITERAL C, COUNT string,
	THEN ; IMMEDIATE

: ." [ I_CALL c, ' S" , ]
	?] IF 
		I_CALL c, [ I_LITERAL c, ' CTYPE , ] , 
	ELSE
		CTYPE
	THEN ; IMMEDIATE

: doNum ( n -- ) ?] IF compile-num THEN ;
: try-num DROP TRUE ;
: doWord DUP Word.?Immediate SWAP Word.Body SWAP
	?] IF 
		IF EXECUTE 
		ELSE compile-call 
		THEN
	ELSE DROP execute 
	THEN ;
: (interpret-word) LOOKUP DUP 
	IF 
		doWord FALSE
	ELSE 
		try-num
	THEN ;
: parse 
	BEGIN 
		WORD DUP C@ 0= 
		IF DROP EXIT THEN     \ end of line
		(interpret-word)
		IF
			PAD CTYPE ."  ??" 
			EXIT
		THEN 
	AGAIN ;
	
: SOURCE (source) @ DUP C@ SWAP 1+ SWAP ;

\ File stuff ...
HEX 
\ Access modes
: GENERIC_ALL     10 ;
: GENERIC_EXECUTE 20 ;
: GENERIC_WRITE   40 ;
: GENERIC_READ    80 ;

\ Dispositions
: CREATE_NEW        1 ;
: CREATE_ALWAYS     2 ;
: OPEN_EXISTING     3 ;
: OPEN_ALWAYS       4 ;
: TRUNCATE_EXISTING 5 ;
DECIMAL

VARIABLE #(isNeg) 
VARIABLE #(count) 
VARIABLE #(buf) 32 ALLOT
: <# 0 #(count) ! ;
: HOLD >T #(count) .INC. ;
: SIGN #(isNeg) @ IF [CHAR] - HOLD THEN ;
: # BASE @ /MOD SWAP DUP 9 > IF 55 ELSE [CHAR] 0 THEN + HOLD ;
: #(toBuf) 0 #(buf) C! #(buf) #(count) @ 0 DO T> OVER >S LOOP DROP ;
: #> DROP #(toBuf) #(buf) COUNT ;
: #S BEGIN # DUP WHILE 0 ;
: . DUP 0 < #(isNeg) ! ABS <# #S DROP SIGN #> TYPE SPACE ;
: U. #(isNeg) OFF <# #S DROP #> TYPE SPACE ;
: HEX. [CHAR] $ EMIT BASE @ HEX SWAP U. BASE ! ;

VARIABLE #(padChar)
VARIABLE #(padWidth)
: #(padOne) #(count) @ #(padWidth) @ < IF #(padChar) @ HOLD 1 ELSE 0 THEN ;
: #(padIt) BEGIN #(padOne) WHILE ;
: ./w ( num padChar width -- ) 
	#(padWidth) ! #(padChar) ! <# #S DROP #(padIt) SIGN #> TYPE ;
: ./p <# #S DROP #(padIt) SIGN #> TYPE ;

: ? @ . ; : C? C@ . ;
: DUMP ( addr n -- ) #(isNeg) OFF BASE @ >T HEX 
	0 DO DUP C@ [CHAR] 0 2 ./w SPACE 1+ LOOP 
	DROP T> BASE ! ;

: .S ." <" DEPTH . 8 EMIT ." > " DEPTH 
	IF
		DEPTH 0 DO I PICK >T LOOP 
		DEPTH 0 DO T> . LOOP 
		." <-TOS"
	THEN ;

\ ( disposition access-mode name -- handle ) FOPEN
OPEN_ALWAYS GENERIC_READ S" BootStrap.4th" FOPEN .S DROP
t1 ! here 10 + t2 !
t2 @ 1000 t1 @ fread t2 @ swap type
t1 @ fclose

: .Stats
	Code.Stats . ." bytes used, avg code length: " . CR 
	Dict.Stats . ." bytes used, avg entry length: " . CR 
	Words.Stats . ." bytes used, " . ." words, avg word length: " . CR 
	LAST HERE - . ." bytes available."
	CR CR ;

: FILL ( addr count val -- ) -ROT OVER + SWAP DO DUP I C! LOOP DROP ;
\ ( size -- addr result )
: ALLOCATE HERE TUCK + (HERE) ! 0 ;
: dl last dup word.name? ." - " word.body here over - dump ;
: FL ForgetLast ;
: FL? FL LAST? ;
: ascii BASE @ >T DECIMAL DO I DUP HEX. DUP . EMIT CR LOOP T> BASE ! ;

CHAR A BL 1+ ascii CR WORDS CR CR .Stats ." So, how does everything look?"
