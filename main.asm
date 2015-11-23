; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; A P-Code implementation of Forth
;
; Written by Chris Curl

; ---------------------------------------------------------------------------------------------------------
StackSize	EQU		4096
include irvine32.inc

VirtualAlloc PROTO,
	pMem:DWORD,				; optional, NULL lets the system figure it out
	dwSize:DWORD,			; requested size
	fAllocType:DWORD,		; allocation type
	fProtect:DWORD			; protection flags

VirtualFree PROTO,
  lpAddress:DWORD,
  dwSize:DWORD,
  dwFreeType:DWORD

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 1 - CONSTANTS
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------

; For VirtualAlloc and VirtualFree
PAGE_EXECUTE_READWRITE	EQU 00040h
MEM_COMMIT				EQU 01000h
MEM_RESERVE				EQU 02000h
MEM_RELEASE				EQU 08000h

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------

oneKilo			EQU 400h
oneMB			EQU oneKilo * oneKilo

SIZE_DWORD		EQU 4
SIZE_WORD		EQU 2
SIZE_CHAR		EQU 1

memorySize		EQU oneMB
dStackSize		EQU oneKilo*4
rStack_SZ		EQU oneKilo*2

inpBuf_SZ		EQU	 0100h

; For Dictionary entries
IMM_BIT				EQU 01h
READY_BIT			EQU 02h
x86_BIT				EQU 10h
COMPILE_ONLY_BIT	EQU 80h
NEXT_OFFSET			EQU  0
XT_OFFSET			EQU  4
FLAGS_OFFSET		EQU  8
NAME_OFFSET			EQU  9
ENTRY_OVERHEAD		EQU 10

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; VM opcodes

	; opcode 0 is unused
	I_FETCH			EQU   1
	I_STORE			EQU   2
	I_LITERAL		EQU   3
	I_DUP			EQU   4
	I_SWAP			EQU   5
	I_DROP			EQU   6
	I_PICK			EQU   7
	I_ROT			EQU   8
	I_ONEPLUS		EQU   9
	I_PLUS			EQU  10		; 0Ah
	I_MINUS			EQU  11
	I_MULT			EQU  12		; 0Ch
	I_DIV			EQU  13
	I_EQ			EQU  14		; 0Eh
	I_CALL			EQU  15
	I_LT			EQU  16		; 10h
	I_GT			EQU  17
	I_TO_R			EQU  18		; 12h
	I_R_FROM		EQU  19
	I_R_AT			EQU  20		; 14h
	I_JMPZ			EQU  21
	I_LOOKUP		EQU  22		; 16h
	I_X86			EQU  23
	I_JMPNZ			EQU  24		; 18h
	I_UNUSED_25		EQU  25
	I_DEPTH			EQU  26		; 1ah
	I_ONEMINUS		EQU  27
	I_AND			EQU  28		; 1ch
	I_OR			EQU  29
	I_XOR			EQU  30		; 1eh
	I_DIVMOD		EQU  31
	I_EMIT			EQU  32		; 20h
	I_DICTP			EQU  33
	I_TWOTIMES		EQU  34		; 22h
	I_TWODIV		EQU  35
	I_FOPEN			EQU  36		; 24h
	I_FCLOSE		EQU  37
	I_FREAD			EQU  38		; 26h
	I_FWRITE		EQU  39
	I_FGETC			EQU  40		; 28h
	I_FETCH_8		EQU  41
	I_STORE_8		EQU  42		; 2ah
	I_LITERAL_8		EQU  43
	I_COMMA			EQU  44		; 2ch
	I_COMMA_8		EQU  45
	I_OVER			EQU  46		; 2eh
	I_STRCMP		EQU  47
	I_STRCMPI		EQU  48		; 30h
	I_CREATE		EQU  49
	I_WORD			EQU  50		; 32h
	I_BRANCHF		EQU  51
	I_GOTO			EQU  52		; 34h
	I_EXECUTE		EQU  53
	I_DOT			EQU  54		; 36h
	; opcodes 55 -> 97 are unused
	I_BREAK			EQU  98		; 62h
	I_RETURN		EQU  99		; 63h
	; opcodes 100 -> 255 are unused

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 2 - Macros
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; The stack pointer is EBP

m_Push		macro	Val

			sub ebp, SIZE_DWORD
			mov	DWORD PTR [ebp], Val

			endm

; ---------------------------------------------------------------------------------------------------------
m_cPush		macro	Val

			m_Push			0
			mov BYTE PTR	[ebp], Val

			endm

; ---------------------------------------------------------------------------------------------------------
m_pushVal	macro	theVar

			mov		edx, theVar
			m_Push	edx

			endm

; ---------------------------------------------------------------------------------------------------------
m_getTOS	macro	tgtReg

			mov	tgtReg, [ebp]

			endm

; ---------------------------------------------------------------------------------------------------------
m_setTOS	macro	fromReg

			mov DWORD PTR [ebp], fromReg

			endm

; ---------------------------------------------------------------------------------------------------------
m_Drop		macro

			add	ebp, SIZE_DWORD

			endm

; ---------------------------------------------------------------------------------------------------------
m_Pop		macro	toReg

			m_getTOS	toReg
			m_Drop

			endm

; ---------------------------------------------------------------------------------------------------------
m_isStackEmpty		macro

			cmp	ebp, dStack_MAX
			jg	stackEmpty

			endm

; ---------------------------------------------------------------------------------------------------------
m_execO		macro	opCode

			mov  edx, opCode
			call executeOpCode

			endm

; ---------------------------------------------------------------------------------------------------------
m_execXT	macro codeAddr

			mov  edx, codeAddr
			call executeWord_SAFE

			endm

; ---------------------------------------------------------------------------------------------------------
m_pushExecO	macro	theVal, opCode

			m_Push	theVal
			m_execO	opCode

			endm

; ---------------------------------------------------------------------------------------------------------
val2mem	macro		addrTo, theVal

		push	theVal
		pop		addrTo

		endm
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 3 - Data
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
.data

req_MEMSIZE	DWORD memorySize
cur_MEMSIZE	DWORD memorySize

primVectors	DWORD 256 DUP (0)

welcome		BYTE	"Hello, Chris.", 0
msgOK		BYTE	" ", 0
msgPrompt	BYTE	13, 10, "ok. ", 0
cmdBye		BYTE	3, "bye", 0
cmdRestart	BYTE	8, "restart!", 0
resetOK		BYTE	"restarted.", 0
msgBye		BYTE	"Good bye.", 0
msgBad_PRE	BYTE	"Sorry, I don't know '", 0
msgBad_POST	BYTE	"'.", 0
msgMemOR	BYTE	"Memory reference out of range.", 0
msgStackE	BYTE	"Stack empty.", 0
msgRStackE	BYTE	"Return stack empty.", 0
msgStackU	BYTE	"Stack underflow.", 0
msgStackO	BYTE	"Stack overflow.", 0
msgDivByZero BYTE	"Divide by zero.", 0
msgNoName	BYTE	"Cannot create dictionary entry with no name.", 0
msgBadOP	BYTE	"Invalid opcode encountered.", 0
msgNoMem	BYTE	"Fatal error - memory allocation failed!", 0
msgBadMem	BYTE	"Fatal error - memory addresses out of expected range!", 0
char2Num	BYTE	"0123456789ABCDEF", 0

theMemory	DWORD	0

bytesRead	DWORD ?
hStdIn		HANDLE ?
hStdOut		HANDLE ?
SizeReadWrite DWORD 0

initialESPVal	DWORD ?


; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section XXX - Variables for creating the built-in words
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
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
var_rStack		DWORD 	rStack_SZ DUP (0)

; For the code that generates the constants
code_GenVar		BYTE	6, I_LITERAL, 0, 0, 0, 0, I_RETURN

; Needed by addWords
code_HERE_Addr	BYTE	6, I_LITERAL
				DWORD	var_HERE
				BYTE	I_RETURN
code_LAST_Addr	BYTE	6, I_LITERAL
				DWORD	var_LAST
				BYTE	I_RETURN

; General words
code_HERE		BYTE	7, I_LITERAL
				DWORD	var_HERE
				BYTE	I_FETCH, I_RETURN
code_LAST		BYTE	7, I_LITERAL
				DWORD	var_LAST
				BYTE	I_FETCH, I_RETURN

code_NoName		BYTE	15, I_LITERAL
				DWORD	var_HERE
				BYTE	I_FETCH, I_LITERAL_8, 1, I_LITERAL
				DWORD	var_STATE
				BYTE	I_STORE, I_RETURN
code_SemiColon	BYTE	12, I_LITERAL_8, I_RETURN, I_COMMA_8,  I_LITERAL_8, 0, I_LITERAL
				DWORD	var_STATE
				BYTE	I_STORE, I_RETURN

; ---------------------------------------------------------------------------------------------------------
name_HERE_Addr	BYTE	6, "(HERE)", 0
name_LAST_Addr	BYTE	6, "(LAST)", 0
name_State		BYTE	5, "STATE", 0
name_toIN		BYTE	3, ">IN", 0
name_Source_Addr BYTE	8, "(SOURCE)", 0
name_Pad		BYTE	3, "PAD", 0
name_Pad2		BYTE	4, "PAD2", 0
name_BASE		BYTE	4, "BASE", 0

name_T1			BYTE	2, "T1", 0
name_T2			BYTE	2, "T2", 0
name_T3			BYTE	2, "T3", 0
name_T4			BYTE	2, "T4", 0
name_MEMSIZE	BYTE	8, "MEM_SIZE", 0
name_MEMSTART	BYTE	8, "(memory)", 0
name_PrimVs		BYTE	8, "pVectors", 0

name_StdOut		BYTE	6, "StdOut", 0
name_StdIn		BYTE	5, "StdIn", 0

name_NoName		BYTE	7, ":NONAME", 0
name_SemiColon	BYTE	1, ";", 0

name_HERE		BYTE	4, "HERE", 0
name_LAST		BYTE	4, "LAST", 0


; IMMEDIATE "macro-assembler" type words for primitives

; Primitives
prim_FETCH		BYTE	I_FETCH,		01, "@", 0
prim_STORE		BYTE	I_STORE,		01, "!", 0
prim_DUP		BYTE	I_DUP,			03, "DUP", 0
prim_SWAP		BYTE	I_SWAP,			04, "SWAP", 0
prim_DROP		BYTE	I_DROP,			04, "DROP", 0
prim_PICK		BYTE	I_PICK,			04, "PICK", 0
prim_ROT		BYTE	I_ROT,			03, "ROT", 0
prim_ONEPLUS	BYTE	I_ONEPLUS,		02, "1+", 0
prim_ONEMINUS	BYTE	I_ONEMINUS,		02, "1-", 0
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

prim_AND		BYTE	I_AND,			03, "AND", 0
prim_OR			BYTE	I_OR,			02, "OR", 0
prim_XOR		BYTE	I_XOR,			03, "XOR", 0
prim_DEPTH		BYTE	I_DEPTH,		05, "DEPTH", 0
prim_TWOTIMES	BYTE	I_TWOTIMES,		02, "2*", 0
prim_TWODIV		BYTE	I_TWODIV,		02, "2/", 0

prim_DIVMOD		BYTE	I_DIVMOD,		04, "/MOD", 0
prim_EMIT		BYTE	I_EMIT,			04, "EMIT", 0
prim_FOPEN		BYTE	I_FOPEN,		05, "FOPEN", 0
prim_FCLOSE		BYTE	I_FCLOSE,		06, "FCLOSE", 0
prim_FREAD		BYTE	I_FREAD,		05, "FREAD", 0
prim_FWRITE		BYTE	I_FWRITE,		06, "FWRITE", 0
prim_FGETC		BYTE	I_FGETC,		04, "FGETC", 0

prim_FETCH_8	BYTE	I_FETCH_8,		02, "C@", 0
prim_STORE_8	BYTE	I_STORE_8,		02, "C!", 0
prim_COMMA		BYTE	I_COMMA,		01, ",", 0
prim_COMMA_8	BYTE	I_COMMA_8,		02, "C,", 0
prim_OVER		BYTE	I_OVER,			04, "OVER", 0
prim_STRCMP		BYTE	I_STRCMP,		06, "STRCMP", 0
prim_STRCMPI	BYTE	I_STRCMPI,		07, "STRCMPI", 0
prim_CREATE		BYTE	I_CREATE,		12, "CREATE.EMPTY", 0
prim_WORD		BYTE	I_WORD,			04, "WORD", 0

prim_EXECUTE	BYTE	I_EXECUTE,		07, "EXECUTE", 0
prim_DOT		BYTE	I_DOT,			01, ".", 0
prim_BREAK		BYTE	I_BREAK,		03, "BRK", 0
prim_RETURN		BYTE	I_RETURN,		04, "EXIT", 0
prim_LOOKUP		BYTE	I_LOOKUP,		06, "LOOKUP", 0
prim_X86		BYTE	I_X86,			03, "x86", 0

; ---------------------------------------------------------------------------------------------------------
array_Vars_1	DWORD	name_HERE_Addr,		var_HERE,
						name_LAST_Addr,		var_LAST,
						name_STATE,			var_STATE,
						name_toIN,			var_toIN,
						name_Source_Addr,	var_SOURCE,
						name_PAD,			var_PAD,
						name_PAD2,			var_PAD2,
						name_BASE,			var_BASE

array_Vars_2	DWORD	name_T1,			var_T1,
						name_T2,			var_T2,
						name_T3,			var_T3,
						name_T4,			var_T4,
						name_MEMSIZE,		req_MEMSIZE,
						name_MEMSTART,		theMemory,
						name_PrimVs,		primVectors

array_Vars_3	DWORD	name_StdOut,		hStdOut,
						name_StdIn,			hStdIn,
						0, 0

array_Words		DWORD	offset code_NoName, offset name_NoName, 0,
						offset code_SemiColon, offset name_SemiColon, 1,
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
		

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 4 - Code
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
.code

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section XXX - YYY
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
;
; In this implementation:
;
;		EBP is the stack base
;		EDI is the depth
;		EAX is the top of the stack, 
;		EBX is the 2nd element on the stack.
; 
; [EBP] is also the depth
; The day will come when these rules are known to be followed.
; When that dqy comes, fPush and fPop will become obsolete
; ---------------------------------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------------------------------
; This is the primary FORTH data stack.   ** TESTED **
; In this implementation, EAX IS the top of the stack, and [EDI] is the 2nd element on the stack.
; ---------------------------------------------------------------------------------------------------------
fDrop proc			

		cmp			ebp, dStack_MAX
		jg			stackEmpty
		add			ebp, SIZE_DWORD
		ret

fDrop endp

; ---------------------------------------------------------------------------------------------------------
stackEmpty proc

		mov				edx, offset msgStackE
		jmp				doError

stackEmpty endp

; ---------------------------------------------------------------------------------------------------------
stackUnderFlow proc			; If we get here, the stack could be hosed ...

		mov				edx, offset msgStackU
		jmp				doError

stackUnderFlow endp

; ---------------------------------------------------------------------------------------------------------
stackOverFlow proc			; If we get here, the stack could be hosed ...

		mov				edx, offset msgStackO
		jmp				doError

stackOverFlow endp

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
rPush proc					; pushes EDX onto the return stack

		push		ecx

		mov			ecx, var_rStack
		inc			ecx
		mov			var_rStack, ecx
		mov			var_rStack[ecx*4], edx

		pop			ecx
		ret

rPush endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
rPop proc					; pops the return stack into EDX

		push		ecx

		mov			ecx, var_rStack
		test		ecx, ecx
		jz			isEmpty
		mov			edx, var_rStack[ecx*4]
		dec			ecx
		mov			var_rStack, ecx

		pop			ecx
		ret

isEmpty:
		mov				edx, offset msgRStackE
		jmp				doError

rPop endp

; ---------------------------------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
;
;
;			FORTH PRIMITIVES
;
;
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
; Comma - 
fCOMMA proc PUBLIC 

		m_Pop	eax
		mov		edx, var_HERE
		mov		[edx], eax			; Set the DWORD
		add		edx, 4				; update (HERE) .. 32 bits
		mov		var_HERE, edx

		ret

fCOMMA endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
; Comma_8 - 
fCOMMA_8 proc PUBLIC 

		m_Pop	eax
		mov		edx, var_HERE
		mov		[edx], al			; Set the BYTE
		inc		edx					; update (HERE) .. 8 bits
		mov		var_HERE, edx

		ret

fCOMMA_8 endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
; FETCH (@) - this is the 32 bit version
fFETCH proc

		m_getTOS	eax
		mov			edx, [eax]
		m_setTOS	edx
		ret

isBad:
		mov		edx, offset msgMemOR
		jmp				doError

fFETCH endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
; FETCH_8 (C@)
fFETCH_8 proc

		m_getTOS	eax
		xor			edx, edx
		mov			dl, [eax]
		m_setTOS	edx
		ret

isBad:
		mov		edx, offset msgMemOR
		jmp				doError

fFETCH_8 endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
; STORE (!) - this is the 32 bit version
fSTORE proc

		m_Pop		edx
		m_Pop		eax
		mov			[edx], eax
		ret

fSTORE endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
; fSTORE_8 (C!)
fSTORE_8 proc

		m_Pop		edx
		m_Pop		eax
		mov			[edx], al
		ret

fSTORE_8 endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fLITERAL proc

		mov		edx,[esi]
		m_Push	edx
		add		esi, SIZE_DWORD
		ret

fLITERAL endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fLITERAL_8 proc

		mov		dl,[esi]
		m_cPush	dl
		inc		esi
		ret

fLITERAL_8 endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fDUP proc

		m_getTOS eax
		m_Push eax
		ret

fDUP endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fSWAP proc

		m_Pop ebx
		m_getTOS eax
		m_SetTOS ebx
		m_Push eax
		ret

fSWAP endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fPICK proc

		m_Pop		edx
		shl			edx, 2
		mov			eax, ebp
		add			eax, edx
		cmp			eax, dStack_MAX
		jg			stackUnderFlow
		mov			eax, [eax]
		m_Push eax
		ret

fPICK endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fROT proc

		m_Pop		ecx
		m_Pop		ebx
		m_Pop		eax

		m_Push		ebx
		m_Push		ecx
		m_Push		eax
		
		ret

fROT endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fONEPLUS proc

		m_getTOS eax
		inc  eax
		m_setTOS eax
		ret

fONEPLUS endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fONEMINUS proc

		m_getTOS eax
		dec  eax
		m_setTOS eax
		ret

fONEMINUS endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fPLUS proc

		m_Pop		ebx
		m_getTOS	eax
		add			eax, ebx
		m_setTOS	eax
		ret

fPLUS endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fMINUS proc

		m_Pop		ebx
		m_getTOS	eax
		sub			eax, ebx
		m_setTOS	eax
		ret

fMINUS endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fMULT proc

		m_Pop		ebx
		m_getTOS	eax
		mul			ebx
		m_setTOS	eax
		ret

fMULT endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
f_DIV proc

		m_Pop		ebx		; Divisor
		m_getTOS	eax		; Dividend

		test		ebx, ebx
		jz			divByZero

		xor			edx, edx	; DIV uses EDX and EAX
		test		eax, eax
		jns			doDiv
		dec			edx

doDiv:	idiv		ebx			; EAX gets quotient, EDX gets remainder
		m_setTOS	eax
		ret

divByZero:
		mov			edx, offset msgDivByZero
		jmp			doError

f_DIV endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fEQ proc

		m_Pop		ebx		; Divisor
		m_getTOS	eax		; Dividend
		.IF eax == ebx
			mov		eax, -1
		.ELSE
			mov		eax, 0
		.ENDIF
		m_setTOS	eax
		ret

fEQ endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fCALL proc

		mov		edx, [esi]		; The address here is the actual code address (XT)
		push	edx				; Save it
		add		esi, 4			; Move the IP to the next instruction
		mov		edx, esi		; Put the current IP (ESI) on the return stack
		call	rPush
		pop		esi				; Get new address into the IP (ESI)
		ret

fCALL endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fLT proc

		xor			edx, edx
		dec			edx
		m_Pop		ebx
		m_getTOS	eax
		cmp			eax, ebx
		jl			allDone
		inc			edx
allDone:
		m_setTOS	edx
		ret

fLT endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fGT proc

		xor			edx, edx
		dec			edx
		m_Pop		ebx
		m_getTOS	eax
		cmp			eax, ebx
		jg			allDone
		inc			edx
allDone:
		m_setTOS	edx
		ret

fGT endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fTO_R proc

	m_Pop	edx
	call	rPush
	ret

fTO_R endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fR_FROM proc

	call	rPop
	m_Push	edx
	ret

fR_FROM endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fR_AT proc

	call	rPop
	call	rPush
	m_Push	edx
	ret

fR_AT endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fJMPZ proc

		m_Pop		eax
		.IF	(EAX == 0)
			mov			esi, [esi]		; The address here is an absolute 32 bit address
		.ELSE
			add			esi, 4			; Move the IP
		.ENDIF

done:	ret

fJMPZ endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fJMPNZ proc

		m_Pop		eax
		.IF	(EAX != 0)
			mov			esi, [esi]		; The address here is an absolute 32 bit address
		.ELSE
			add			esi, 4			; Move the IP
		.ENDIF

done:	ret

fJMPNZ endp

; ---------------------------------------------------------------------------------------------------------
; Dictionary entries look like this:
;
; Offset
; 00-03		DWORD		Next		; Address of next entry
; 04-07		DWORD		XT			; Execution Token (HERE)
; 08-08		BYTE 		Flags		; Flags ... see below.
; 09-09		BYTE		Len			; Length byte
; 10-xx		BYTE(?)		Name		; Word name, ZERO delimited
;
; Flags:
;		Bit 0 (Flags AND 0x01) is the immediate flag.		A ONE here means the word is immediate.
;		Bit 1 (Flags AND 0x02) is the ready bit.			A ONE here means the word is ready for use.
;		Bit 1 (Flags AND 0x04) is not currently used.
;		Bit 1 (Flags AND 0x08) is not currently used.
;		Bit 1 (Flags AND 0x10) is the primitive bit.		A ONE here means the word is machine code.
;		Bit 1 (Flags AND 0x20) is not currently used.
;		Bit 4 (Flags AND 0x40) is not currently used.
;		Bit 7 (Flags AND 0x80) is the compile_only bit.		A ONE here means the word is only for compile time (STATE=1).
;
; ---------------------------------------------------------------------------------------------------------
fLOOKUP proc			; ( addr -- head|0 )

			m_Pop			ebx							; ( addr -- )
			mov				ecx, var_LAST				; [ecx] -> current dictionary entry

L1:
			mov				edx, ecx
			add				edx, NAME_OFFSET
			m_Push			ebx							; ( -- addr )
			m_Push			edx							; ( addr  -- addr word-name )
			call			fSTRCMPi					; ( s1 s2 -- bool )
			m_Pop			eax							; ( bool -- )
			test			eax, eax
			jz				notThis
			m_Push			ecx							; Found it. Leave the head address on the stack.
			ret

notThis:	mov				ecx, [ecx+NEXT_OFFSET]		; Go to the next word
			mov				eax, [ecx+NEXT_OFFSET]		; Get that word's NEXT pointer
			test			eax, eax					; If the Next pointer is ZERO, we are at the end.
			jnz				L1

			; If we get here, then we didn't find it, put ZERO is on the stack.
			m_Push			eax
			ret

fLOOKUP endp

; ---------------------------------------------------------------------------------------------------------
fX86 proc

		mov			ecx, [esi]
		add			esi, SIZE_DWORD
		call		ecx
		ret

fX86 endp

; ---------------------------------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------------------------------
fDEPTH proc

	mov		edx, dStack_MAX
	; add		edx, 4
	sub		edx, ebp
	shr		edx, 2
	m_Push	edx
	ret

fDEPTH endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
f22 proc

	ret

f22 endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
f23 proc

	ret

f23 endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
f24 proc

	ret

f24 endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
f25 proc

	ret

f25 endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fAND proc

		m_Pop		ebx
		m_getTOS	eax
		and			eax, ebx
		m_setTOS	eax
		ret

fAND endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
f_OR proc

		m_Pop		ebx
		m_getTOS	eax
		or			eax, ebx
		m_setTOS	eax
		ret

f_OR endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fXOR proc

		m_Pop		ebx
		m_getTOS	eax
		xor			eax, ebx
		m_setTOS	eax
		ret

fXOR endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fDIVMOD proc				; ( n1 n2 -- rem quot )

		m_Pop		ebx		; Divisor
		m_getTOS	eax		; Dividend

		test		ebx, ebx
		jz			divByZero

		xor			edx, edx	; DIV uses EDX and EAX
		div			ebx			; EAX gets quotient, EDX gets remainder
		m_setTOS	edx			; Remainder goes on first
		m_Push		eax			; Then quotient
		ret

divByZero:
		mov			edx, offset msgDivByZero
		jmp			doError

fDIVMOD endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fEMIT proc

		m_Pop	eax
		mov		edx, hStdOut
		.IF		edx != 0
			call	WriteChar
		.ENDIF

		ret

fEMIT endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fDICTP proc

	add  esi, SIZE_DWORD
	ret

fDICTP endp

; ---------------------------------------------------------------------------------------------------------
fTwoTimes proc

			m_getTOS	eax
			shl			eax, 2
			m_setTOS	eax
			ret

fTwoTimes endp

; ---------------------------------------------------------------------------------------------------------
fTwoDiv proc

			m_getTOS	eax
			shr			eax, 2
			m_setTOS	eax
			ret

fTwoDiv endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fSPUSH proc					; string push (append char) ( c addr -- )

	; push edi

	; call	fPop			; get string offset (relative to theMemory)
	; push	eax				; remember it
	; call	fPop			; get the byte from the dataStack
	; pop		edx				; 
	; vmToAbsolute edi, edx	; resolve string actual location
	; call stkPush_8			; append the byte

	; pop  edi
	ret

fSPUSH endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fSPOP proc					; string pop (remove last char) ( addr -- c )

	; push	edi

	; call	fPop			; get string offset (relative to theMemory)
	; vmToAbsolute edi, eax	; resolve string actual location
	; call	stkPop_8		; get the character (decrements string length)
	; call	fPush			; put the char on the dataStack

	; pop		edi
	ret

fSPOP endp

; ---------------------------------------------------------------------------------------------------------
;CreateFile PROTO,		; create new file
;	lpFilename:PTR BYTE,				; ptr to filename (EAX)
;	dwDesiredAccess:DWORD,				; access mode;		(EDX)
;	dwShareMode:DWORD,					; share mode;
;	lpSecurityAttributes:DWORD,  		; pointer to security attributes
;	dwCreationDisposition:DWORD,		; file creation options (ECX)
;	dwFlagsAndAttributes:DWORD,			; file attributes
;	hTemplateFile:DWORD					; handle to template file
;
; ** NOT TESTED **
fFOPEN	proc

		m_Pop		eax
		m_Pop		edx
		m_Pop		ecx
		invoke		CreateFile, eax, GENERIC_READ or GENERIC_WRITE, \
						FILE_SHARE_READ or FILE_SHARE_WRITE, \
						NULL, 4, FILE_ATTRIBUTE_NORMAL, \
						NULL
		m_Push		eax
		call GetLastError
	ret

fFOPEN endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fFCLOSE proc

	m_Pop		edx
	invoke CloseHandle, edx
	ret

fFCLOSE endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fFREAD proc

	; ( addr count hFile -- numRead )
	; invoke ReadFile,hFile,pMemory,MEMSIZE-1,ADDR SizeReadWrite,NULL 
	push		ebx

	m_Pop		eax
	m_Pop		ecx
	m_Pop		edx
	; mov			ebx, offset SizeReadWrite
	invoke ReadFile, eax, edx, ecx, offset SizeReadWrite, NULL 
	mov			eax, SizeReadWrite
	m_Push		eax

	pop			ebx
	ret

fFREAD endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fFWRITE proc

	; invoke WriteFile,hFile,pMemory,eax,ADDR SizeReadWrite,NULL
	ret

fFWRITE endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fFGETC proc

	ret

fFGETC endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fOVER proc

		m_Pop		ebx
		m_getTOS	eax
		m_Push		ebx
		m_Push		eax
		ret

fOVER endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fSTRCMP proc

	push		esi
	push		ecx
	push		ebx
	push		edi

	m_Pop		esi
	m_Pop		edi
	call		strCmpC
	m_Push		eax

	pop			edi
	pop			ebx
	pop			ecx
	pop			esi
	
	ret

fSTRCMP endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fSTRCMPi proc

	push		esi
	push		ecx
	push		ebx
	push		edi

	m_Pop		esi
	m_Pop		edi
	call		strCmpCi
	m_Push		eax

	pop			edi
	pop			ebx
	pop			ecx
	pop			esi
	ret

fSTRCMPI endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fBREAK proc

	;mov	edx, offset var_dStack
	ret

fBREAK endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fCreate proc		; (CREATE) ... address of name is on the stack

		push			esi

		m_Pop			esi
		mov				dl, [esi]
		test			dl, dl
		jz				NameNeeded

		call			addWord				; Name in ESI
		; TODO: if a needed register changed, push and pop it

		pop				esi
		ret

NameNeeded:
		mov				edx, offset msgNoName
		jmp				doError

fCreate endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fWord2 proc		; Puts the next word from the input stream into PAD

		push			eax
		push			esi
		push			edi

		mov				ecx, var_toIN
		mov				esi, var_SOURCE
		inc				esi						; Skip count byte
		xor				edx, edx
		mov				edi, offset var_PAD

skipWS:	; Skip whitespace
		mov				dl, [esi][ecx]
		test			dl, dl
		jz				allDone
		call			isWS
		jnz				cWord
		inc				ecx
		jmp				skipWS

cWord:	; Collect word
		mov				dl, [esi][ecx]
		test			dl, dl
		jz				allDone
		call			isWS
		jz				allDone
		inc				dh
		inc				edi
		inc				ecx
		mov				[edi], dl
		jmp				cWord

allDone:
		inc				edi					; Add the NULL terminator
		mov	BYTE PTR	[edi], 0
		mov				var_PAD, dh
		mov				var_toIN, ecx

		pop				edi
		pop				esi
		pop				eax
		m_Push			offset var_PAD
		ret

fWord2 endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fBRANCHF proc		; Branch forward relative

	mov		esi, [esi]
	ret

fBRANCHF endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fGOTO proc		; Branch backward

	mov		esi, [esi]
	ret

fGOTO endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fEXECUTE proc		; Execute XT

	m_Pop		eax
	test		eax, eax		; Make sure it is not ZERO
	jz			allDone
	mov			esi, eax		; Put the current IP (ESI) on the return stack

allDone:
	ret

fEXECUTE endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
fDOT proc

			m_Pop		eax
			mov			ebx, var_BASE
			m_Push		20h
			xor			edx, edx
			mov			ecx, 1

			; is this a negative number?
			push		edx
			test		eax, eax
			jns			doDiv
			pop			edx					; yes, it is negative
			inc			edx
			push		edx
			xor			eax, 0ffffffffh
			inc			eax

doDiv:		xor			edx, edx			; DIV uses EDX:EAX
			div			ebx					; EAX gets quotient, EDX gets remainder
			.IF edx < 10
				add edx, 48
			.ELSE
				add	edx, 55
			.ENDIF
			m_Push		edx					; Push the digit
			inc			ecx					; Number of digits
			test		eax, eax
			jnz			doDiv

			pop			edx					; Start negative number with a '-'
			test		edx, edx
			jz			doOut
			m_push		'-'
			inc			ecx

doOut:		call		fEMIT
			loop		doOut
			ret

fDOT endp

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section XXX - Helpers
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
isWS proc				; Sets the Z/Equals flag if the char in DL is whitespace

			cmp			dl, 9
			je			allDone
			cmp			dl, 10
			je			allDone
			cmp			dl, 13
			je			allDone
			cmp			dl, 32
			je			allDone
			; TODO: decide what to do about other chars that are not printable
			jg			notWS
			cmp			dl, dl		; just set the Z flag for now

allDone:	ret

notWS:		cmp			dl, 32		; set the flags for "not equal"
			ret

isWS endp

; -----------------------------------------------------------------------------------
; Case sensitive counted string compare - string 1 in [EDI], string 2 in [ESI]
; First BYTE is the count
; Returns edx = 0 if not the same
strCmpC proc PUBLIC 

	xor  ecx, ecx
	mov  cl, [edi]			; max is 255 chars
	inc  ecx				; include the count byte
	; inc  ecx				; include the NULL terminator
L1:
	mov  dl, [esi]			; get a character from source
	cmp  dl, [edi]			; compare it to the target
	jne	 notEqual
	inc  esi				; move to next character
	inc  edi
	loop L1					; repeat for entire string
	mov  edx, 1
	ret

notEqual:
	xor  edx, edx
	ret

strCmpC endp

; ---------------------------------------------------------------------------------------------------------
tolower proc PUBLIC
	
			cmp		dl, 'A'
			jl		allDone
			cmp		dl, 'Z'
			jg		allDone
			add		dl, 32

allDone:	ret

tolower endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
; Case insensitive counted string compare - string 1 in [EDI], string 2 in [ESI]
; First BYTE is the count
; Returns EAX = 1 if the same, else EAX = 0
strCmpCi proc PUBLIC 

			xor		eax, eax
			xor		ecx, ecx
			mov		cl, [edi]			; max is 255 chars
			inc		ecx					; include the count byte
			; inc  ecx					; We are NOT including the NULL terminator

L1:			mov		dl, [esi]			; get a character from source
			call	tolower
			mov		al, dl
			mov		dl, [edi]			; compare it to the target
			call	toLower
			cmp		al, dl
			jnz		notEqual
			inc		esi					; move to next characters
			inc		edi
			loop	L1					; repeat for entire string
			mov		eax, 1
			ret

notEqual:	xor  eax, eax
			ret

strCmpCi endp

; ---------------------------------------------------------------------------------------------------------
; Copies counted strings - source string in [ESI], destination string in [EDI]
; First BYTE is the count
strCpyC proc PUBLIC 
	
	push ecx

	xor  ecx, ecx
	mov  cl, [esi]
	add ecx, 2				; include the count and trailing null
	cld						; make sure the direction flag is correct
	rep movsb

	pop ecx
	ret

strCpyC endp

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section XXX -	Initialization
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
PutVector	macro nOpcode, pProc

		mov  esi, nOpcode
		mov  edx, offset pProc
		mov  [edi][esi*4], edx

		endm

; ---------------------------------------------------------------------------------------------------------
unkOpcode proc

		mov		edx, offset msgBadOP
		jmp		doError

unkOpcode endp

; ---------------------------------------------------------------------------------------------------------
bsVectors	proc

	mov  edi, offset primVectors

	mov		ecx, 255
	mov		edx, offset unkOpcode
	mov		[edi], edx
L1:
	mov		[edi][ecx*4], edx
	loop	L1

	PutVector I_FETCH, fFETCH
	PutVector I_STORE, fSTORE
	PutVector I_LITERAL, fLITERAL
	PutVector I_DUP, fDUP
	PutVector I_SWAP, fSWAP
	PutVector I_DROP, fDrop
	PutVector I_PICK, fPICK
	PutVector I_ROT, fROT
	PutVector I_ONEPLUS, fONEPLUS
	PutVector I_PLUS, fPLUS
	PutVector I_MINUS, fMINUS
	PutVector I_MULT, fMULT
	PutVector I_DIV, f_DIV
	PutVector I_EQ, fEQ
	PutVector I_CALL, fCALL
	PutVector I_LT, fLT
	PutVector I_GT, fGT
	PutVector I_TO_R, fTO_R
	PutVector I_R_FROM, fR_FROM
	PutVector I_R_AT, fR_AT
	PutVector I_JMPZ, fJMPZ
	PutVector I_LOOKUP, fLOOKUP
	PutVector I_X86, fX86
	PutVector I_JMPNZ, fJMPNZ
	PutVector I_DEPTH, fDEPTH
	PutVector I_ONEMINUS, fONEMINUS
	PutVector I_AND, fAND
	PutVector I_OR, f_OR
	PutVector I_XOR, fXOR
	PutVector I_DIVMOD, fDIVMOD
	PutVector I_EMIT, fEMIT
	PutVector I_DICTP, fDICTP
	PutVector I_TWOTIMES, fTwoTimes
	PutVector I_TWODIV, fTwoDiv
	PutVector I_FOPEN, fFOPEN
	PutVector I_FCLOSE, fFCLOSE
	PutVector I_FREAD, fFREAD
	PutVector I_FWRITE, fFWRITE
	PutVector I_FGETC, fFGETC
	PutVector I_FETCH_8, fFETCH_8
	PutVector I_STORE_8, fSTORE_8
	PutVector I_LITERAL_8, fLITERAL_8
	PutVector I_COMMA, fCOMMA
	PutVector I_COMMA_8, fCOMMA_8
	PutVector I_OVER, fOVER
	PutVector I_STRCMP, fSTRCMP
	PutVector I_STRCMPI, fSTRCMPI
	PutVector I_CREATE, fCreate
	PutVector I_WORD, fWord2
	PutVector I_BRANCHF, fBRANCHF
	PutVector I_GOTO, fGOTO
	PutVector I_EXECUTE, fEXECUTE
	PutVector I_DOT, fDOT

	; Opcodes 51 -> 97 are not used ... yet

	PutVector I_BREAK, fBREAK
	; PutVector I_RETURN, fRETURN

	ret

bsVectors	endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
bootStrap proc

	call bsVectors

	invoke GetStdHandle, STD_INPUT_HANDLE
	mov  hStdIn, eax

    invoke GetStdHandle, STD_OUTPUT_HANDLE
	mov  hStdOut, eax

	.IF theMemory != 0
		mov			edx, cur_MEMSIZE
		INVOKE		VirtualFree, theMemory, edx, MEM_RELEASE
		mov			theMemory, 0
	.ENDIF

	mov		edx, req_MEMSIZE
	mov		cur_MEMSIZE, edx
	invoke			VirtualAlloc, 0, edx, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE
	.IF eax == 0
		mov			edx, offset msgNoMem
		call		WriteString
		INVOKE	ReadConsole, hStdIn, edx, 10, ADDR bytesRead, 0
		INVOKE	ExitProcess,1
	.ENDIF
	mov				theMemory, eax

	; A little test to make sure we can execute code in the allocated memory
	mov				ecx, theMemory
	add				ecx, 4
	mov	byte ptr	[ecx], 00bah
	mov	DWORD ptr	[ecx+1], 11223344h
	mov	byte ptr	[ecx+5], 00c3h
	call			ecx					; EDX should get 11223344h in it
	; End of the little test

	; Initialize (HERE), (LAST), (SOURCE)
	; HERE starts at the beginning.
	mov				var_HERE, eax

	; LAST starts at the end.
	add				eax, cur_MEMSIZE
	sub				eax, SIZE_DWORD
	mov				var_LAST, eax
	xor				edx, edx
	mov				[eax], edx

	mov				var_SOURCE, offset var_InpBuf

	; Allocate the stack
	invoke			VirtualAlloc, 0, dStackSize, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE

	.IF eax == 0
		mov			edx, offset msgNoMem
		call		WriteString
		INVOKE	ReadConsole, hStdIn, edx, 10, ADDR bytesRead, 0
		invoke ExitProcess,0
	.ENDIF
	mov				dStack_MIN, eax
	add				eax, dStackSize
	mov				ebp, eax
	sub				eax, SIZE_DWORD
	mov				dStack_MAX, eax

	; Initialize the stacks
	xor				edi, edi
	mov				var_rStack, edi

	; Built in words ...
	call initializeWords
	
	ret

bootStrap endp

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section XXX -	Built in words
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Dictionary entries look like this:
;
; Offset
; 00-03		DWORD		Next		; Address of next entry: the current LAST.
; 04-07		DWORD		XT			; Execution Token: the current HERE.
; 08-08		BYTE 		Flags		; Flags ... see below.
; 09-09		BYTE		Len			; Length byte
; 10-xx		BYTE(?)		Name		; Word name, ZERO delimited
;
; Flags:
;		Bit 0 (Flags AND 0x01) is the immediate flag.		A ONE here means the word is immediate.
;		Bit 1 (Flags AND 0x02) is the ready bit.			A ONE here means the word is ready for use.
;		Bit 2 (Flags AND 0x04) is not currently used.
;		Bit 3 (Flags AND 0x08) is not currently used.
;		Bit 4 (Flags AND 0x10) is the primitive bit.		A ONE here means the word is machine code.
;		Bit 5 (Flags AND 0x20) is not currently used.
;		Bit 6 (Flags AND 0x40) is not currently used.
;		Bit 7 (Flags AND 0x80) is the compile_only bit.		A ONE here means the word is only for compile time (STATE=1).
;
; ---------------------------------------------------------------------------------------------------------

addWord	proc	; Name in ESI

			push			edi
			push			esi
			push			ecx

			; Calculate the new starting address
addIt:		mov				eax, var_LAST		; EAX (LAST) is used later as well
			xor				ecx, ecx
			mov				cl, [esi]
			inc				cl
			add				cl, ENTRY_OVERHEAD
			mov				edi, eax
			sub				edi, ecx
			mov				var_LAST, edi

			STOSD								; EAX has the old LAST
			mov				eax, var_HERE		; Initial XT is HERE
			STOSD
			mov				al, READY_BIT		; Initial value of FLAGS
			STOSB

			; EDI now points to where the word name goes
			xor				ecx, ecx
			mov				cl, [esi]
			inc				ecx					; include the count
			cld
			REP MOVSB
			xor				al, al				; NULL terminator
			STOSB

allDone:	pop				ecx
			pop				esi
			pop				edi
			ret

addWord	endp

; ---------------------------------------------------------------------------------------------------------
; ** TESTED **
compileCode		proc	; source in ESI

			push	esi
			push	edi
			push	ecx

			xor		ecx, ecx		; Get the number of bytes to move
			mov		cl, [esi]
			inc		esi
			mov		edi, var_HERE	; Destination

			rep		movsb

			mov		eax, var_HERE
			mov		var_HERE, edi

allDone:	
			pop		ecx
			pop		edi
			pop		esi
			ret

compileCode		endp

; ---------------------------------------------------------------------------------------------------------
; * NOT TESTED **
initializeWords	proc

			; First word: HERE
			mov		esi, offset name_HERE		; ESI used by addWord - Name
			call	addWord
			mov		esi, offset code_HERE
			call	compileCode

			; 2nd word: LAST
			mov		esi, offset name_LAST		; ESI used by addWord - Name
			call	addWord
			mov		esi, offset code_LAST
			call	compileCode

			; Constants
			mov		ecx, offset array_Vars_1	; Generate 32-bit constant values ... (HERE), (LAST), STATE, ...
nextVar:	mov		esi, [ecx]					; Name
			test	esi, esi
			jz		addWords
			push	ecx
			call	addWord
			pop		ecx
			mov		edx, [ecx+4]				; variable address
			mov		esi, offset code_GenVar
			mov		[esi+2], edx				; Set the address
			push	ecx
			call	compileCode
			pop		ecx
			add		ecx, 8
			jmp		nextVar

			; General words
addWords:	mov		ecx, offset array_words
nxtWord: 	mov		esi, [ecx]
			test	esi, esi
			jz		addPrims
			mov		esi, [ecx+4]
			push	ecx
			call	addWord
			pop		ecx
			mov		edx, [ecx+8]
			.IF		EDX == 1				; A ONE here means IMMEDIATE
				mov		edx, var_LAST
				add		edx, FLAGS_OFFSET
				mov		al, [edx]
				or		al, IMM_BIT
				mov		[edx], al
			.ELSEIF EDX > 1
				mov		[edx], eax
			.ENDIF
			mov		esi, [ecx]
			push	ecx
			call	compileCode
			pop		ecx
			add		ecx, 12
			jmp		nxtWord

addPrims:	; These are the primitives
			; These words have an OpCode inserted
			mov		ecx, offset a_prim_0
nxtPrim:	mov		var_T1, ecx
			mov		esi, [ecx]
			test	esi, esi
			jz		addx86
			inc		esi
			call	addWord

			mov		edx, var_LAST					; Make it immediate
			add		edx, FLAGS_OFFSET
			mov		al, [edx]
			or		al, IMM_BIT
			mov		[edx], al

			; Generate the code for the words that represent the VM's instructions
			m_pushExecO I_LITERAL, I_COMMA_8
			mov		edx, offset var_STATE
			m_pushExecO edx, I_COMMA
			m_pushExecO I_FETCH, I_COMMA_8
			m_pushExecO I_JMPZ, I_COMMA_8
			mov		edx, var_HERE
			add		edx, 8
			m_pushExecO edx, I_COMMA
			m_pushExecO I_LITERAL_8, I_COMMA_8
			mov		ecx, var_T1							; Stick opcode HERE
			mov		ecx, [ecx]
			mov		dl, [ecx]
			m_cPush dl
			m_execO	I_COMMA_8
			m_pushExecO I_COMMA_8, I_COMMA_8
			m_pushExecO I_RETURN, I_COMMA_8
			mov		ecx, var_T1							; Stick opcode HERE
			mov		ecx, [ecx]
			mov		dl, [ecx]
			m_cPush dl
			m_execO	I_COMMA_8
			m_pushExecO I_RETURN, I_COMMA_8

			mov		ecx, var_T1							; Stick opcode HERE
			add		ecx, 4
			jmp		nxtPrim

addx86:			; Other words that are implemented in machine code
addOther:		; There are currently no special words
			ret

initializeWords	endp

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section XXX - The run time interpreter
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; This is used to execute one VM instruction.

executeOpcode proc						; EDX = opcode, ESI = IP

	and		edx, 00ffh					; ensure range 0-255 ... all 256 entries have handlers.
	mov		ecx, primVectors[edx*4]		; most cause a jump to the invalid opcode handler.
	jmp		ecx							; jump to the handler.

executeOpcode endp

; ---------------------------------------------------------------------------------------------------------
; This is the main execution loop for the interpreter.

executeWord proc	; ESI = IP
	
L1:
	mov				dl, [esi]
	inc				esi
	cmp				dl, I_RETURN
	je				doRet

	call			executeOpcode
	jmp				L1

doRet:
	mov		edx, var_rStack		; If the return stack is empty, then we are done
	test	edx, edx
	jz		allDone

	call rPop
	mov  esi, edx
	jmp  L1

allDone:
	ret

executeWord endp

; ---------------------------------------------------------------------------------------------------------
; This is used during initialization ...

executeWord_SAFE	proc		; word address in EDX

	push	esi
	mov		esi, edx
	call	executeWord
	pop		esi

	ret

executeWord_SAFE	endp

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section XXX - The user interaction loop
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
dlToNum proc

			; dl is the char, retyrn the num in dh
			; returns carry flag == 1 if it it a number
			
			push			ecx
			push			esi

			xor				dh, dh
			mov				ecx, var_BASE
			mov				esi, offset char2Num

L1:			cmp				dl, [esi]
			je				isNum
			inc				esi
			inc				dh
			loop			L1

notNum:		xor				dh, dh
			clc
			jmp				allDone
			
isNum:		mov				dl, dh
			xor				dh, dh
			stc
			jmp				allDone
	
allDone:	pop				esi
			pop				ecx
			ret

dlToNum endp

; ---------------------------------------------------------------------------------------------------------
isNumber proc			; If the word in PAD is a number, puts said number on the stack and sets the carry flag.
;						; Else, clears the carry flag and does NOT put a number on the stack.

			push			esi
			push			ecx
			push			ebx

			mov				ebx, var_BASE

			xor				edx, edx
			push			edx						; flag for negative
			mov				esi, offset var_PAD
			mov				dl, [esi]				; Get the length
			test			dl, dl
			jz				notNumber				; Buffer is empty

			m_Push			0						; 0 on the stack means not negative
			inc				esi						; Move to first char
			mov				dl, [esi]				; check for '-'
			cmp				dl, '-'
			jne				L1
			pop				edx						; 1 on the stack means negative
			inc				edx
			push			edx
			inc				esi
L1:
			mov				dl, [esi]				; Get the char
			test			dl, dl					; End of buffer?
			jz				chkNeg

ckDigit:	call			dlToNum
			jnc				notNumber
			m_pushExecO		edx, I_SWAP				; ( A -- B A )
			m_PushVal		var_BASE
			m_execO			I_MULT					; TODO: decimal only for now ( A B -- A B*BASE )
			m_execO			I_PLUS					; ( A B -- A+B )
			inc				esi						; Move to next char
			jmp				L1

isOverFlow:	mov				edx, 0					; TODO: what to do about overflow?
notNumber:	call			fDrop					; ( num -- )
			pop				edx
			clc
			jmp				allDone

chkNeg:		pop				edx
			test			edx, edx
			jz				notNeg
			m_Pop			eax
			dec				eax
			xor				eax, 0ffffffffh
			m_Push			eax
notNeg:		stc

allDone:	pop				ebx
			pop				ecx
			pop				esi
			ret

isNumber endp

; ---------------------------------------------------------------------------------------------------------
; The current word or number is in PAD
;
doWord proc

			m_Push			offset var_PAD			; ( -- PAD )
			call			fLOOKUP					; ( PAD -- head|0 )
			m_Pop			esi						; ( head|0 -- )
			test			esi, esi
			jz				tryNum

			mov				ebx, esi				; Remember the address
			mov				dl, [ebx+FLAGS_OFFSET]	; Get the flags
			mov				esi, [ebx+XT_OFFSET]	; Get the XT

			mov				dh, dl					; Is it immediate?
			and				dl, IMM_BIT
			jnz				executeIt
			cmp				var_STATE, 0			; Are we interpreting?
			je				executeIt
			m_pushExecO		I_CALL, I_COMMA_8		; We are compiling, and it is NOT an immediate word
			m_pushExecO		esi, I_COMMA			; Compile a call to it
			jmp				allDone

			; Either we are not compiling, or it is an immediate word
executeIt:	and				dh, x86_BIT				; x86_BIT on means it is real code, not bytecode.
			jnz				isRealCode
			jmp				executeWord

isRealCode:
			jmp				esi

tryNum:
			call			isNumber				; Not a word ... maybe it's a number ...
			jnc				soSorry

			; It is a number, and that number is on the stack.
			cmp				var_STATE, 0			; Compiling?
			je				allDone					; No, all done.

			; Compile it into the definition. Numbers < 256 can use I_LITERAL_8
			.IF			ax < 100h
				m_pushExecO		I_LITERAL_8, I_COMMA_8	
				m_execO			I_COMMA_8
			.ELSE
				m_pushExecO		I_LITERAL, I_COMMA_8
				m_execO			I_COMMA
			.ENDIF
allDone:	ret

soSorry:	mov				edx, offset msgBad_PRE		; not a number either ... 
			call			WriteString
			mov				edx, offset var_PAD
			inc				edx							; skip count byte
			call			WriteString
			mov				edx, offset msgBad_POST
			jmp				doError

doWord endp

; ---------------------------------------------------------------------------------------------------------
;
;	ESI points to the input text, a counted string. It is preserved.
;   CL keeps the the >IN value, not sure if this is needed at this point
;	EBX is used for the transfer to PAD. It is preserved.
;	EAX and EDX are used freely
;
; ---------------------------------------------------------------------------------------------------------
processLine proc

	; ** TEMP debug
	; MemGet		eax, MemLoc_LAST
	; add			eax, ebp
	; mov			edx, Addr_CodeStart
	; add			edx, ebp
	; ** TEMP debug

	mov				var_toIN, 0

doOne:
	call		fWord2				; Puts current word address on the stack.
	m_Pop		eax
	mov			dl, [eax]			; PAD is empty when at EOL
	test		dl, dl
	jz			allDone

	call		doWord
	cmp			ebp, dStack_MIN		; See if the stack is OK
	jl			stackOverFlow
	mov			eax, dStack_MAX		; See if the stack is OK
	cmp			ebp, eax			; See if the stack is OK
	jg			stackUnderFlow
	jmp			doOne

allDone:
	ret

processLine endp

; ---------------------------------------------------------------------------------------------------------
doError	proc			; Error message in EDX

		call		WriteString

doError endp

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section XXX - YYY
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
mainLoop	proc

	; Initialize the stacks
	mov				ebp, dStack_MAX
	; add				ebp, SIZE_DWORD
	xor				edi, edi
	mov				var_rStack, 0
	; mov				var_cStack, 0

	mov				var_STATE, 0
	mov				var_BASE, 10

	mov				esp, initialESPVal

loopStart:
	; Display prompt
	mov  edx, offset msgPrompt				; string addr in edx
	call WriteString
	; invoke locate,10,3

	; Wait for commands from console
	mov		edx, offset var_InpBuf
	inc		edx								; save room for the count BYTE

	INVOKE	ReadConsole, hStdIn, edx, inpBuf_SZ, ADDR bytesRead, 0

	mov				edx, bytesRead			; count includes CR and LF
	sub				edx, 2					; remove CR/LF
	mov				eax, offset var_InpBuf
	mov				[eax], dl				; store the count
	mov				[eax+edx+1], dh			; NULL terminator

	; is the command "bye"?
	mov				edi, offset cmdBye
	mov				esi, offset var_InpBuf
	call			strCmpC

	test  edx, edx							; strCmpX returns result in EDX
	jnz  goodBye

	; is the command "restart!"?
	mov				edi, offset cmdRestart
	mov				esi, offset var_InpBuf
	call			strCmpC

	test  edx, edx							; strCmpX returns result in EDX
	jz  doIt

	call bootStrap
	mov  edx, offset resetOK				; string addr in edx
	call WriteString
	jmp mainLoop

doIt:
	call processLine

	mov  edx, offset msgOK					; string addr in EDX
	call WriteString

	jmp loopStart

goodBye:
	mov  edx, offset msgBye					; string addr in edx
	call WriteString

	invoke ExitProcess,0

mainLoop endp

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section XXX - YYY
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
main		proc

	mov			edx, offset welcome		; string addr in edx
	call		WriteString

	mov			initialESPVal, esp
	call		bootStrap 
	jmp			mainLoop
	; NO return here ... fall through 

main	endp
end main

; ---------------------------------------------------------------------------------------------------------
