1024 DUP * 32 * MEM_SIZE !
restart!

LAST 12 - T1 ! LAST T1 @ ! HERE T1 @ 4 + ! 1 T1 @ 9 + C! 58 T1 @ 10 + C! T1 @ (LAST) !
:noname word create.empty 1 STATE ! ; DROP

: IMMEDIATE LAST 8 + DUP C@ 1 OR SWAP C! ;
: \ (source) @ c@ >IN ! ; IMMEDIATE

: ] 1 STATE ! ;
: [ 0 STATE ! ; IMMEDIATE
: ?] STATE @ ;

: 0= 0 = ;
: <> = 0= ;
: <= > 0= ;
: >= < 0= ;
: 4* 2* 2* ;
: 4/ 2/ 2/ ; 
: 2+ 1+ 1+ ;

: NIP SWAP DROP ; : TUCK SWAP OVER ; : -ROT ROT ROT ;

: I_FETCH 1 ; : I_STORE 2 ; : I_LITERAL 3 ; : I_DUP 4 ; : I_SWAP 5 ;
: I_DROP 6 ; : I_PICK 7 ; : I_ROT 8 ; : I_ONEPLUS 9 ; : I_PLUS 10 ;
: I_MINUS 11 ; : I_MULT 12 ; : I_DIV 13 ; : I_EQ 14 ; : I_CALL 15 ;
: I_LT 16 ; : I_GT 17 ; : I_TO_R 18 ; : I_R_FROM 19 ; : I_R_AT 20 ;
: I_JMPZ 21 ; : I_LOOKUP 22 ; : I_X86 23 ; : I_JMPNZ 24 ; : I_25 25 ;
: I_DEPTH 26 ; : I_ONEMINUS 27 ; : I_AND 28 ; : I_OR 29 ; : I_XOR 30 ;
: I_DIVMOD 31 ; : I_EMIT 32 ; : I_DICTP 33 ; : I_TWOTIMES 34 ; : I_TWODIV 35 ;
: I_FOPEN 36 ; : I_FCLOSE 37 ; : I_FREAD 38 ; : I_FWRITE 39 ; : I_FGETC 40 ;
: I_FETCH_8 41 ; : I_STORE_8 42 ; : I_LITERAL_8 43 ; : I_COMMA 44 ; : I_COMMA_8 45 ;
: I_OVER 46 ; : I_STRCMP 47 ; : I_STRCMPI 48 ; : I_CREATE 49 ; : I_WORD 50 ;
: I_GOTO 52 ; : I_EXECUTE 53 ; : I_DOT 54 ; 
: I_BREAK 98 ; : I_RETURN 99 ;

: BEGIN HERE ; IMMEDIATE
: AGAIN I_GOTO C, , ; IMMEDIATE
: WHILE I_JMPNZ C, , ; IMMEDIATE
: UNTIL I_JMPZ C, , ; IMMEDIATE
: DO I_SWAP C, HERE I_TO_R C, I_TO_R C, ; IMMEDIATE
: I I_R_AT C, ; IMMEDIATE
: LOOP I_R_FROM C, I_ONEPLUS C, I_R_FROM C, I_OVER C, I_OVER C, I_LT C, I_JMPZ C, HERE SWAP 0 , I_GOTO C, , HERE SWAP ! I_DROP C, I_DROP C, ; IMMEDIATE
: LEAVE I_R_FROM C, I_R_FROM C, I_DROP C, I_DUP C, I_TO_R C, I_TO_R C, ; IMMEDIATE

: IF I_JMPZ C, HERE 0 , ; IMMEDIATE
: ELSE I_GOTO C, HERE SWAP 0 , HERE SWAP ! ; IMMEDIATE
: THEN HERE SWAP ! ; IMMEDIATE

: NEGATE 0 SWAP - ;
: ABS DUP 0 < IF NEGATE THEN ;

: OFF 0 SWAP ! ; : ON -1 SWAP ! ;

: BINARY 2 BASE ! ; : OCTAL 8 BASE ! ; : DECIMAL 10 BASE ! ; : HEX 16 BASE ! ;

: 2DROP DROP DROP ; : 2DUP OVER OVER ;
: ?DUP DUP IF DUP THEN ;

: MOD 2dup / * - ;
: SQ DUP * ;

: ALLOT HERE + (HERE) ! ;
: VARIABLE word create.empty I_LITERAL C, HERE 0 , I_RETURN C, HERE SWAP ! 0 , ;
: CONSTANT word create.empty I_LITERAL C, , I_RETURN C, ;
: FIELD + DUP CONSTANT ;

: ? @ . ; : C? C@ . ;

: .inc.  DUP @ 1+  SWAP ! ;
: .inc4. DUP @ 4 + SWAP ! ;
: .dec.  DUP @ 1-  SWAP ! ;
: .dec4. DUP @ 4 - SWAP ! ;

variable tStack 256 allot 
: tStack.Reset tStack dup ! ;
: T! tStack @ ! ;
: >T tStack .inc4. T! ;
: T@ tStack @ @ ;
: T> T@ tStack .dec4. ;
tStack.Reset

: COUNT DUP 1+ SWAP C@ ;
: TYPE BEGIN SWAP DUP C@ EMIT 1+ SWAP 1- DUP WHILE DROP DROP ;
: CTYPE count type ;

: BL 32 ; : TAB 9 ; : LF 10 ;
: .BL BL emit ; : .TAB TAB emit ; : CR 13 emit LF emit ;
: SPACES 0 DO .BL LOOP ;

: Word.Body 4 + @ ;
: Word.Flags 8 + ;
: Word.Name 9 + ;
: Word.Name? Word.name CTYPE ;
: Word.?Immediate Word.Flags C@ 1 AND ;
: Words.GetNext @ ;
: Words.Count 0 >T LAST BEGIN T@ 1+ T! Words.GetNext DUP Words.GetNext WHILE DROP T> ;
: Words.Find WORD lookup ;
: Words LAST BEGIN DUP Word.Name? .BL .BL Words.GetNext DUP Words.GetNext WHILE DROP ;
: Word.size 25 ;

: mem_start (memory) @ ; : mem_end mem_start mem_size @ + ;

: Last? LAST Word.name? ;
: Words.Start 0 >T LAST BEGIN DUP T! Words.GetNext DUP Words.GetNext WHILE DROP T> ;
: Code.Start mem_start ;
: Dict.Stats Words.Start Words.GetNext LAST - DUP Words.Count / SWAP ;
: Code.Stats HERE Code.Start - DUP Words.Count / SWAP ;
: Words.Stats Code.Stats NIP Dict.Stats NIP + Words.Count 2DUP / SWAP ROT ;

: CMOVE 0 DO 2DUP SWAP C@ SWAP C! SWAP 1+  SWAP 1+  LOOP 2DROP ; \ ( from to count -- )
: MOVE  0 DO 2DUP SWAP  @ SWAP  ! SWAP 4 + SWAP 4 + LOOP 2DROP ;

: COPY.STRING OVER C@ 1+ CMOVE ; \ ( from to -- )
: string, DUP C, 0 DO DUP C@ C, 1+ LOOP DROP 0 C, ;
: >S DUP .inc. DUP C@ + C! ; \ ( c addr -- ) - append c to counted string at addr

\ variable cStack 256 allot 
\ : CS! cStack @ ! ; : >CS cStack .inc4. CS! ; 
\ : CS@ cStack @ @ ; : CS> CS@ cStack .dec4. ;
\ : cStack.Reset cStack dup !  cStack.Reset

: ascii BASE @ >T DO I DUP . HEX DUP . DECIMAL EMIT CR LOOP T> BASE ! ;

: FORGET Words.Find ?DUP IF DUP Word.Body (HERE) ! Words.GetNext (LAST) ! THEN ;
: ' Words.Find ?dup if Word.body then ;
: ['] ' ; IMMEDIATE
: ForgetLast LAST Word.Body (HERE) ! LAST Words.GetNext (LAST) ! ;

: collect pad2 off >in .inc.
	begin (source) @ 1+ >in @ + C@ dup 0=
		if 2drop exit then   \ End of line
		>in .inc. 2dup =
		if 2drop exit then   \ found closing char
		pad2 >S
	again ;

: ( 41 COLLECT ; IMMEDIATE
: DUMP 0 DO DUP C@ . 1+ LOOP DROP ;
 
: S" 34 collect pad2
	?] IF 
		I_LITERAL C, HERE 9 + , I_GOTO C, HERE 0 , 
		SWAP COUNT string, HERE SWAP ! 
	THEN ; IMMEDIATE

: ." [ I_CALL c, ' s" , ]
	?] IF 
		I_CALL c, [ I_LITERAL c, ' ctype , ] , 
	ELSE
		ctype
	THEN ; IMMEDIATE

: .S ." <" DEPTH . 8 emit ." > " DEPTH 
	IF
		DEPTH 0 DO I PICK >T LOOP 
		DEPTH 0 DO T> . LOOP 
		." <-TOS"
	THEN ;

: doNum
	?] IF
		I_LITERAL C, ,
	THEN ;
: tryNum 0 ;
: doWord DUP Word.?Immediate SWAP Word.Body SWAP
	?] IF 
		IF EXECUTE 
		ELSE I_CALL C, , 
		THEN
	ELSE DROP execute 
	THEN ;
: parse 
	BEGIN 
		WORD DUP C@ 0= 
		IF DROP EXIT THEN     \ end of line
		LOOKUP ?DUP 
		IF
			doWord 
		ELSE 
			tryNum 0=
			IF
				PAD CTYPE ."  ??" 
			THEN
		THEN 
	AGAIN ;
	
: SOURCE (source) @ DUP C@ SWAP 1+ SWAP ;

: FL? ForgetLast LAST? ;

: .Stats
	Code.Stats . ." bytes used, avg code length: " . CR 
	Dict.Stats . ." bytes used, avg entry length: " . CR 
	Words.Stats . ." bytes used, " . ." words, avg word length: " . CR 
	LAST HERE - . ." bytes available."
	CR CR ;

91 65 ascii
CR WORDS CR CR .Stats ForgetLast
\ So, how does everything look?
