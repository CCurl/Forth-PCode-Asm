\ *************************************************************************************************
FORGET _SEE_
: _SEE_ ;

\ *************************************************************************************************
256 CONSTANT mcode.num
CREATE mcode mcode.num CELLS ALLOT
variable see-ip
variable last-opcode
: mcode! CELLS mcode + ! ;
: mcode@ CELLS mcode + @ ;
: mcode.exec dup last-opcode ! mcode@ ?dup
	if execute 
	else ." ; invalid opcode: " last-opcode @ hex.
	then ;
mcode mcode.num CELLS 0 FILL

: .arg4 see-ip @ @ hex. see-ip .inc4. ;
: .arg1 see-ip @ C@ hex. see-ip .inc. ;
: .wn ( dict-addr -- ) ."  ; " word.name? ;
: .rn ( call-addr -- ) dup C@ I_DICTP = IF 1+ @ ."  ; " word.name? ELSE DROP THEN ;
:NONAME ." @" ; I_FETCH mcode!
:NONAME ." !" ; I_STORE mcode!
:NONAME ." LIT " .arg4 ; I_LITERAL mcode!
:NONAME ." DUP" ; I_DUP mcode!
:NONAME ." SWAP" ; I_SWAP mcode!
:NONAME ." DROP" ; I_DROP mcode!
:NONAME ." PICK" ; I_PICK mcode!
:NONAME ." ROT" ; I_ROT mcode!
:NONAME ." 1+" ; I_ONEPLUS mcode!
:NONAME ." +" ; I_PLUS mcode!
:NONAME ." -" ; I_MINUS mcode!
:NONAME ." *" ; I_MULT mcode!
:NONAME ." /" ; I_DIV mcode!
:NONAME ." =" ; I_EQ mcode!
:NONAME ." CALL " see-ip @ @ dup hex. .rn see-ip .inc4. ; I_CALL mcode!
:NONAME ." <" ; I_LT mcode!
:NONAME ." >" ; I_GT mcode!
:NONAME ." >R" ; I_TO_R mcode!
:NONAME ." R>" ; I_R_FROM mcode!
:NONAME ." R@" ; I_R_AT mcode!
:NONAME ." JMPZ " .arg4 ; I_JMPZ mcode!
:NONAME ." LOOKUP" ; I_LOOKUP mcode!
:NONAME ." X86" ; I_X86 mcode!
:NONAME ." JMPNZ " .arg4 ; I_JMPNZ mcode!
:NONAME ." SLITERAL ; " 34 emit see-ip @ dup CTYPE 34 emit C@ 2+ see-ip +! ; I_SLITERAL mcode!
:NONAME ." DEPTH" ; I_DEPTH mcode!
:NONAME ." 1-" ; I_ONEMINUS mcode!
:NONAME ." AND" ; I_AND mcode!
:NONAME ." OR" ; I_OR mcode!
:NONAME ." XOR" ; I_XOR mcode!
:NONAME ." /MOD" ; I_DIVMOD mcode!
:NONAME ." EMIT" ; I_EMIT mcode!
:NONAME ." DICTP " see-ip @ @ dup hex. .wn see-ip .inc4. ; I_DICTP mcode!
:NONAME ." 2*" ; I_TWOTIMES mcode!
:NONAME ." 2/" ; I_TWODIV mcode!
:NONAME ." FOPEN" ; I_FOPEN mcode!
:NONAME ." FCLOSE" ; I_FCLOSE mcode!
:NONAME ." FREAD" ; I_FREAD mcode!
:NONAME ." FWRITE" ; I_FWRITE mcode!
:NONAME ." FGETC" ; I_FGETC mcode!
:NONAME ." C@" ; I_FETCH_8 mcode!
:NONAME ." C!" ; I_STORE_8 mcode!
:NONAME ." LIT_8 " see-ip @ C@ hex. see-ip .inc. ; I_LITERAL_8 mcode!
:NONAME ." ," ; I_COMMA mcode!
:NONAME ." C," ; I_COMMA_8 mcode!
:NONAME ." OVER" ; I_OVER mcode!
:NONAME ." STRCMP" ; I_STRCMP mcode!
:NONAME ." STRCMPI" ; I_STRCMPI mcode!
:NONAME ." CREATE" ; I_CREATE mcode!
:NONAME ." WORD" ; I_WORD mcode!
:NONAME ." ++" ; I_INC mcode!
:NONAME ." GOTO " .arg4 ; I_GOTO mcode!
:NONAME ." EXECUTE" ; I_EXECUTE mcode!
:NONAME ." DOT" ; I_DOT mcode!
:NONAME ." ABORT" ; I_ABORT mcode!
:NONAME ." TYPE" ; I_TYPE mcode!
:NONAME ." COLON" ; I_COLON mcode!
:NONAME ." SEMICOLON" ; I_SEMICOLON mcode!
:NONAME ." +!" ; I_PLUSSTORE mcode!
:NONAME ." -!" ; I_MINUSSTORE mcode!
\ :NONAME ." LOADFROM" ; I_LOADFROM mcode!
:NONAME ." BREAK" ; I_BREAK mcode!
:NONAME ." RETURN" ; I_RETURN mcode!

: .( [char] ( emit ; : .) [char] ) emit ; : .bl bl emit ;
: .: [char] : emit ; : .; [char] ; emit ; 

: see-instr ( instr -- ) see-ip @ hex. .bl see-ip @ C@ see-ip .inc. mcode.exec ;
: see-header ( addr -- ) .; .bl word.name? ;
: see-def ( addr -- ) see-ip ! 
	begin
		see-instr cr
		see-ip @ c@ I_DICTP = if exit then
		see-ip @ HERE >= if exit then
	again
 ;
: see-word ( word-addr -- ) word.body see-def ;
: see words.find ?dup if see-word else ." not found." then ;
