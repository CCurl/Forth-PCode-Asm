; ---------------------------------------------------------------------------------------------------------
; A dictionary entry looks like this:
;
;		BYTE	next, prev		; The distance to the next or previous word (absolute value)
;		BYTE	imm, 0			; 1=Immediate, reserved byte.
;		DWORD	eax				; The memory location of word's definition (XT).
;		BYTE	len, <Name>, 0	; The word's name, including length and null-terminator.
;
; ---------------------------------------------------------------------------------------------------------

var_HERE		DWORD 	0
var_LAST		DWORD 	0
var_STATE		DWORD 	0
var_toIN		DWORD 	0
var_SOURCE		DWORD 	0
var_MemSZ		DWORD 	0
var_BASE		DWORD 	0
var_T1			DWORD 	0
var_T2			DWORD 	0
var_T3			DWORD 	0
var_T4			DWORD 	0

var_InpBuf		BYTE 	256 DUP (0)
var_PAD			BYTE 	256 DUP (0)
var_PAD2		BYTE 	256 DUP (0)

dStack_MIN		DWORD 	0
dStack_MAX		DWORD 	0
; var_cStack		DWORD 	cStack_SZ DUP (0)
var_rStack		DWORD 	rStack_SZ DUP (0)

;memoryMin		DWORD	var_HERE
;memoryMax		DWORD	?


; For the code that generates the constants
code_GenConst	BYTE	I_LITERAL, 0, 0, 0, 0, I_RETURN, COMPILE_STOP

; Needed by addWords
code_HERE_Addr	BYTE	I_LITERAL
				DWORD	var_HERE
				BYTE	I_RETURN, COMPILE_STOP
code_LAST_Addr	BYTE	I_LITERAL
				DWORD	var_LAST
				BYTE	I_RETURN, COMPILE_STOP

; General words
code_HERE		BYTE	I_LITERAL
				DWORD	var_HERE
				BYTE	I_FETCH, I_RETURN, COMPILE_STOP
code_LAST		BYTE	I_LITERAL
				DWORD	var_LAST
				BYTE	I_FETCH, I_RETURN, COMPILE_STOP
code_Source		BYTE	I_LITERAL
				DWORD	var_SOURCE
				BYTE	I_CALL
				DWORD	code_COUNT
				BYTE	I_RETURN, COMPILE_STOP  ; xt(SOURCE) xt_Count

code_Inc		BYTE	I_DUP, I_FETCH, I_ONEPLUS,  I_SWAP, I_STORE, I_RETURN, COMPILE_STOP
code_Dec		BYTE	I_DUP, I_FETCH, I_ONEMINUS, I_SWAP, I_STORE, I_RETURN, COMPILE_STOP
code_Inc_4		BYTE	I_DUP, I_FETCH, I_LITERAL_8, 4, I_PLUS,  I_SWAP, I_STORE, I_RETURN, COMPILE_STOP
code_Dec_4		BYTE	I_DUP, I_FETCH, I_LITERAL_8, 4, I_MINUS, I_SWAP, I_STORE, I_RETURN, COMPILE_STOP
code_Count		BYTE	I_DUP, I_FETCH_8, I_SWAP, I_ONEPLUS, I_SWAP, I_RETURN, COMPILE_STOP
code_Immediate	BYTE	I_LITERAL
				DWORD	var_LAST
				BYTE	I_FETCH, I_LITERAL_8, 4, I_PLUS, I_DUP, I_FETCH_8, I_LITERAL_8, 1, I_OR,
						I_SWAP, I_STORE_8, I_RETURN, COMPILE_STOP

; For the words that represent the primitives
code_GenOp		BYTE	I_LITERAL
				DWORD	var_STATE
				BYTE	I_FETCH,
						I_JMPZ, 0, 0, 0, 0,
						I_LITERAL_8, 0,					; Stick OpCode at +9
						I_RETURN,
						0,								; Stick OpCode at +13
						I_RETURN, COMPILE_STOP

code_NoName		BYTE	I_LITERAL
				DWORD	var_HERE
				BYTE	I_FETCH, I_LITERAL_8, 1, I_LITERAL
				DWORD	var_STATE
				BYTE	I_STORE, I_RETURN, COMPILE_STOP
code_SemiColon	BYTE	I_LITERAL_8, I_RETURN, I_COMMA_8,  I_LITERAL_8, 0, I_LITERAL
				DWORD	var_STATE
				BYTE	I_STORE, I_RETURN, COMPILE_STOP
; code_Execute	BYTE	I_EXECUTE, I_RETURN, COMPILE_STOP
;code_Hex		BYTE	I_LITERAL_8, 16, I_LITERAL
;				DWORD	var_BASE
;				BYTE	I_STORE_8, I_RETURN, COMPILE_STOP
;code_Decimal	BYTE	I_LITERAL_8, 10, I_LITERAL
;				DWORD	var_BASE
;				BYTE	I_STORE_8, I_RETURN, COMPILE_STOP
;code_BINARY	BYTE	I_LITERAL_8, 2, I_LITERAL
;				DWORD	var_BASE
;				BYTE	I_STORE_8, I_RETURN, COMPILE_STOP
;code_BRANCHB	BYTE	I_LITERAL_8, I_GOTO, I_COMMA_8, I_LITERAL
;				DWORD	var_HERE
;				BYTE	I_FETCH, I_MINUS, I_COMMA_8, I_RETURN, COMPILE_STOP

code_XXXXX		BYTE	I_RETURN, COMPILE_STOP

; ---------------------------------------------------------------------------------------------------------

xt_HERE			DWORD	?
xt_LAST			DWORD	?
xt_Count		DWORD	?

; ---------------------------------------------------------------------------------------------------------
name_HERE_Addr	BYTE	6, "(HERE)", 0
name_LAST_Addr	BYTE	6, "(LAST)", 0
name_State		BYTE	5, "STATE", 0
name_toIN		BYTE	3, ">IN", 0
name_Source_Addr BYTE	8, "(SOURCE)", 0
name_T1			BYTE	2, "T1", 0
name_T2			BYTE	2, "T2", 0
name_T3			BYTE	2, "T3", 0
name_T4			BYTE	2, "T4", 0

name_HERE		BYTE	4, "HERE", 0
name_LAST		BYTE	4, "LAST", 0
name_Inc		BYTE	5, ".inc.", 0
name_Dec		BYTE	5, ".dec.", 0
name_Inc_4		BYTE	6, ".inc4.", 0
name_Dec_4		BYTE	6, ".dec4.", 0
name_Count		BYTE	5, "COUNT", 0
name_Source		BYTE	6, "SOURCE", 0
name_Pad		BYTE	3, "PAD", 0
name_Pad2		BYTE	4, "PAD2", 0
name_Immediate	BYTE	9, "IMMEDIATE", 0
; name_Word		BYTE	4, "WORD", 0
name_NoName		BYTE	7, ":NONAME", 0
name_SemiColon	BYTE	1, ";", 0
name_Execute	BYTE	7, "EXECUTE", 0
name_StdIn		BYTE	5, "StdIn", 0
name_StdOut		BYTE	6, "StdOut", 0
name_BASE		BYTE	4, "BASE", 0
name_PrimVs		BYTE	8, "pVectors", 0
name_BRANCHB	BYTE	7, "BRANCHB", 0
name_MEMSIZE	BYTE	8, "MEM_SIZE", 0
name_MEMSTART	BYTE	8, "(memory)", 0

; IMMEDIATE "macro-assembler" type words for primitives

; Primitives
prim_FETCH		BYTE	I_FETCH,		01, "@", 0
prim_STORE		BYTE	I_STORE,		01, "!", 0
; prim_LITERAL	BYTE	I_LITERAL,		07, "LITERAL", 0
prim_DUP		BYTE	I_DUP,			03, "DUP", 0
prim_SWAP		BYTE	I_SWAP,			04, "SWAP", 0
prim_DROP		BYTE	I_DROP,			04, "DROP", 0
prim_PICK		BYTE	I_PICK,			04, "PICK", 0
prim_ROT		BYTE	I_ROT,			03, "ROT", 0
prim_ONEPLUS	BYTE	I_ONEPLUS,		02, "1+", 0
prim_PLUS		BYTE	I_PLUS,			01, "+", 0
prim_MINUS		BYTE	I_MINUS,		01, "-", 0
prim_MULT		BYTE	I_MULT,			01, "*", 0
prim_DIV		BYTE	I_DIV,			01, "/", 0
prim_EQ			BYTE	I_EQ,			01, "=", 0
prim_CALL		BYTE	I_CALL,			04, "CALL", 0
prim_LT			BYTE	I_LT,			01, "<", 0
prim_GT			BYTE	I_GT,			01, ">", 0
prim_TO_R		BYTE	I_TO_R,			02, ">R", 0
prim_R_FROM		BYTE	I_R_FROM,		02, "R>", 0
prim_R_AT		BYTE	I_R_AT,			02, "R@", 0
prim_DEPTH		BYTE	I_DEPTH,		05, "DEPTH", 0
prim_LOOKUP		BYTE	I_LOOKUP,		06, "LOOKUP", 0
prim_X86		BYTE	I_X86,			03, "x86", 0
prim_ONEMINUS	BYTE	I_ONEMINUS,		02, "1-", 0
prim_AND		BYTE	I_AND,			03, "AND", 0
prim_OR			BYTE	I_OR,			02, "OR", 0
prim_XOR		BYTE	I_XOR,			03, "XOR", 0
prim_DIVMOD		BYTE	I_DIVMOD,		04, "/MOD", 0
prim_EMIT		BYTE	I_EMIT,			04, "EMIT", 0
prim_DICTP		BYTE	I_DICTP,		05, "DICTP", 0
prim_TWOTIMES	BYTE	I_TWOTIMES,		02, "2*", 0
prim_TWODIV		BYTE	I_TWODIV,		02, "2/", 0
prim_FOPEN		BYTE	I_FOPEN,		05, "FOPEN", 0
prim_FCLOSE		BYTE	I_FCLOSE,		06, "FCLOSE", 0
prim_FREAD		BYTE	I_FREAD,		05, "FREAD", 0
prim_FWRITE		BYTE	I_FWRITE,		06, "FWRITE", 0
prim_FGETC		BYTE	I_FGETC,		04, "FGETC", 0
prim_FETCH_8	BYTE	I_FETCH_8,		02, "C@", 0
prim_STORE_8	BYTE	I_STORE_8,		02, "C!", 0
; prim_LITERAL_8	BYTE	I_LITERAL_8,	09, "LITERAL_8", 0
prim_COMMA		BYTE	I_COMMA,		01, ",", 0
prim_COMMA_8	BYTE	I_COMMA_8,		02, "C,", 0
prim_OVER		BYTE	I_OVER,			04, "OVER", 0
prim_STRCMP		BYTE	I_STRCMP,		06, "STRCMP", 0
prim_STRCMPI	BYTE	I_STRCMPI,		07, "STRCMPI", 0
prim_CREATE		BYTE	I_CREATE,		12, "CREATE.EMPTY", 0
prim_WORD		BYTE	I_WORD,			04, "WORD", 0
; prim_BRANCHF	BYTE	I_BRANCHF,		07, "BRANCHF", 0
; prim_BRANCHB	BYTE	I_GOTO,			07, "BRANCHB", 0
prim_EXECUTE	BYTE	I_EXECUTE,		07, "EXECUTE", 0
prim_DOT		BYTE	I_DOT,			01, ".", 0
prim_BREAK		BYTE	I_BREAK,		03, "BRK", 0
prim_RETURN		BYTE	I_RETURN,		04, "EXIT", 0
prim_XXXXX		BYTE	0000000,		00, "XXXXX", 0

; ---------------------------------------------------------------------------------------------------------
array_Vars_8	DWORD	name_HERE_Addr,		var_HERE,
						name_LAST_Addr,		var_LAST,
						name_STATE,			var_STATE,
						name_toIN,			var_toIN,
						name_Source_Addr,	var_SOURCE,
						name_PAD,			var_PAD,
						name_PAD2,			var_PAD2,
						name_BASE,			var_BASE

array_Vars_82	DWORD	name_T1,			var_T1,
						name_T2,			var_T2,
						name_T3,			var_T3,
						name_T4,			var_T4,
						name_MEMSIZE,		req_MEMSIZE,
						name_MEMSTART,		theMemory,
						name_PrimVs,		primVectors

array_Vars_83	DWORD	name_StdOut,		hStdOut,
						name_StdIn,			hStdIn,
						0, 0

array_Words		DWORD	offset code_Inc, offset name_Inc, 0,
						offset code_Dec, offset name_Dec, 0,
						offset code_Inc_4, offset name_Inc_4, 0,
						offset code_Dec_4, offset name_Dec_4, 0,
						offset code_Count, offset name_Count, offset xt_Count
;						offset code_Immediate, offset name_Immediate, 0

array_Words2	DWORD	offset code_NoName, offset name_NoName, 0,
						offset code_SemiColon, offset name_SemiColon, 1
;						offset code_Word, offset name_Word, 0

array_Words3	DWORD	offset code_SOURCE, offset name_SOURCE, 0,
						0, 0, 0 



a_prim_0	DWORD	offset prim_FETCH,
					offset prim_STORE,
					offset prim_DUP,
					offset prim_SWAP,
					offset prim_DROP,
					offset prim_PICK,
					offset prim_ROT,
					offset prim_ONEPLUS,
					offset prim_ONEMINUS,
					offset prim_PLUS

;					offset prim_LITERAL,
;					offset prim_LITERAL_8,
;					offset prim_DICTP,
			
a_prim_1	DWORD	offset prim_MINUS,
					offset prim_MULT,
					offset prim_DIV,
					offset prim_EQ,
					offset prim_CALL,
					offset prim_LT,
					offset prim_GT,
					offset prim_TO_R,
					offset prim_R_FROM,
					offset prim_R_AT
			
a_prim_2	DWORD	offset prim_AND,
					offset prim_OR,
					offset prim_XOR,
					offset prim_DEPTH,
					offset prim_TWOTIMES,
					offset prim_TWODIV
			
a_prim_3	DWORD	offset prim_DIVMOD,
					offset prim_EMIT,
					offset prim_FOPEN,
					offset prim_FCLOSE,
					offset prim_FREAD,
					offset prim_FWRITE,
					offset prim_FGETC
			
a_prim_4	DWORD	offset prim_FETCH_8,
					offset prim_STORE_8,
					offset prim_COMMA,
					offset prim_COMMA_8,
					offset prim_OVER,
					offset prim_STRCMP,
					offset prim_STRCMPI,
					offset prim_CREATE,
					offset prim_WORD
			
a_prim_5	DWORD	offset prim_EXECUTE,
					offset prim_DOT,
					offset prim_BREAK,
					offset prim_RETURN,
					offset prim_LOOKUP,
					offset prim_X86,
					0
		
a_x86_1		DWORD	offset x86_TEST1,
					offset x86_TEST2,
					0
