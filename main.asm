; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; A bare bones Forth virtual machine
;
; Written by Chris Curl
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
;
; There are 2 stacks in this virtual machine: the data stack, and the return stack. EBP is the pointer for
; the data stack. The return stack is a counted array of DWORDs.
;
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
include irvine32.inc

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 1 - Constants
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------

bytes256		EQU 100h
oneKilo			EQU bytes256 * 4
oneMB			EQU oneKilo * oneKilo


SIZE_DWORD		EQU 4
SIZE_WORD		EQU 2
SIZE_CHAR		EQU 1
CELL_SIZE		EQU SIZE_DWORD

memorySize		EQU oneKilo*8		; The default memory size
dStackSize		EQU bytes256		; The size of the data stack
rStack_SZ		EQU bytes256		; The size of the return stack

; NB: the memory size can be changed as follows:
; This is done in the beginning of BootStrap.4th
; 1024 DUP * 32 * MEM_SIZE !
; restart!

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; VM opcodes

	; opcode 0 is unused
    I_LITERAL EQU 1
    I_FETCH EQU 2
    I_STORE EQU 3
    I_SWAP EQU 4
    I_DROP EQU 5
    I_DUP EQU 6
    I_SLITERAL EQU 7
    I_JMP EQU 8
    I_JMPZ EQU 9
    I_JMPNZ EQU 10
    I_CALL EQU 11
    I_RET EQU 12
    I_ZTYPE EQU 13
    I_CLITERAL EQU 14
    I_CFETCH EQU 15
    I_CSTORE EQU 16
    I_ADD EQU 17
    I_SUB EQU 18
    I_MUL EQU 19
    I_DIV EQU 20
    I_LT EQU 21
    I_EQ EQU 22
    I_GT EQU 23
    I_DICTP EQU 24
    I_EMIT EQU 25
    I_OVER EQU 26
    I_COMPARE EQU 27
    I_FOPEN EQU 28
    I_FREAD EQU 29
    I_FREADLINE EQU 30
    I_FWRITE EQU 31
    I_FCLOSE EQU 32
    I_DTOR EQU 33
    I_RFETCH EQU 34
    I_RTOD EQU 35
    I_ONEPLUS EQU 36
    I_PICK EQU 37
    I_DEPTH EQU 38
    I_GETCH EQU 39
    I_LSHIFT EQU 40
    I_RSHIFT EQU 41
    I_AND EQU 42
    I_OR EQU 43
    I_BRANCH EQU 44
    I_BRANCHZ EQU 45
    I_BRANCHNZ EQU 46
    I_COMPAREI EQU 47
    I_BREAK EQU 253
    I_RESET EQU 254
    I_BYE EQU 255



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
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 3 - Program data
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
.data

req_MEMSIZE	DWORD memorySize
cur_MEMSIZE	DWORD memorySize

; Each primitive is implemented as a procedure. A pointer the that routine is stored here.
primVectors	DWORD 256 DUP (0)

msgBadOP	BYTE	"Invalid opcode encountered.", 0
msgNoMem	BYTE	"Fatal error - memory allocation failed!", 0
msgBadMem	BYTE	"Fatal error - memory addresses out of expected range!", 0
bsFile		BYTE	"PC4th.fs",0

theMemory	DWORD ?
bytesRead	DWORD ?

hStdIn		HANDLE ?
hStdOut		HANDLE ?
SizeReadWrite DWORD 0

initialESPVal	DWORD ?


; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 4 - Variables for creating the built-in words
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------

dStack_MIN		DWORD 	0
dStack_MAX		DWORD 	0
var_rStack		DWORD 	rStack_SZ DUP (0)

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 5 - Code
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
.code

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 6 - Primitives
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
;
; In this implementation:
;
;		EBP is defined to be the stack pointer for the data stack. Similar to ESP, PUSH decrements the
;		pointer, and POP increments it. [EBP] can be used to refer to the element at the top of the stack.
;		
;		The return stack is implemented as a counted array of DWORDs, with the ZEROth element being the
;		count.
; 
; ---------------------------------------------------------------------------------------------------------
fDrop proc			

		; cmp			ebp, dStack_MAX
		; jg			stackEmpty
		m_Drop
		ret

fDrop endp

; ---------------------------------------------------------------------------------------------------------
stackEmpty proc

		; mov				edx, offset msgStackE
		jmp				doError

stackEmpty endp

; ---------------------------------------------------------------------------------------------------------
stackUnderFlow proc			; If we get here, the stack could be hosed ...

		; mov				edx, offset msgStackU
		jmp				doError

stackUnderFlow endp

; ---------------------------------------------------------------------------------------------------------
stackOverFlow proc			; If we get here, the stack could be hosed ...

		; mov				edx, offset msgStackO
		jmp				doError

stackOverFlow endp

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
rPush proc					; pushes EDX onto the return stack

		push		ecx

		add DWORD PTR var_rStack, 4
		mov			ecx, var_rStack
		mov			[ecx], edx

		pop			ecx
		ret

rPush endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
rPop proc					; pops the return stack into EDX

		push		ecx

;		mov			ecx, var_rStack
;		test		ecx, ecx
;		jz			isEmpty
;		mov			edx, var_rStack[ecx*4]
;		dec			ecx
;		mov			var_rStack, ecx

		mov			ecx, var_rStack
		; cmp			ecx, offset var_rStack
		; jng			isEmpty
		mov			edx, [ecx]
		sub			ecx, 4
		mov			var_rStack, ecx

		pop			ecx
		ret

isEmpty:
		; mov				edx, offset msgRStackE
		jmp				doError

rPop endp

doError:

; ---------------------------------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
;
;
; Section 6 - VM Instructions
;
;
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------


; ------------------------------------------------------------------------
f_LITERAL proc

	lodsd
	m_push	eax	
	ret

f_LITERAL endp

; ------------------------------------------------------------------------
f_FETCH proc

	m_pop	eax
	mov		edx, theMemory[eax]
	m_push	edx
	ret

f_FETCH endp

; ------------------------------------------------------------------------
f_STORE proc

	m_pop	eax
	m_pop	ecx
	mov		theMemory[eax], ecx
	ret

f_STORE endp

; ------------------------------------------------------------------------
f_SWAP proc

	m_pop	eax
	m_getTOS	edx
	m_setTOS	eax
	m_push		edx
	ret

f_SWAP endp

; ------------------------------------------------------------------------
f_DROP proc

	m_pop	eax
	ret

f_DROP endp

; ------------------------------------------------------------------------
f_DUP proc

	m_pop	eax
	ret

f_DUP endp

; ------------------------------------------------------------------------
f_SLITERAL proc

	m_pop	eax
	ret

f_SLITERAL endp

; ------------------------------------------------------------------------
f_JMP proc

	m_pop	eax
	ret

f_JMP endp

; ------------------------------------------------------------------------
f_JMPZ proc

	m_pop	eax
	ret

f_JMPZ endp

; ------------------------------------------------------------------------
f_JMPNZ proc

	m_pop	eax
	ret

f_JMPNZ endp

; ------------------------------------------------------------------------
f_CALL proc

	m_pop	eax
	ret

f_CALL endp

; ------------------------------------------------------------------------
f_RET proc

	m_pop	eax
	ret

f_RET endp

; ------------------------------------------------------------------------
f_ZTYPE proc

	m_pop	eax
	ret

f_ZTYPE endp

; ------------------------------------------------------------------------
f_CLITERAL proc

	m_pop	eax
	ret

f_CLITERAL endp

; ------------------------------------------------------------------------
f_CFETCH proc

	m_pop	eax
	ret

f_CFETCH endp

; ------------------------------------------------------------------------
f_CSTORE proc

	m_pop	eax
	ret

f_CSTORE endp

; ------------------------------------------------------------------------
f_ADD proc

	m_pop	eax
	ret

f_ADD endp

; ------------------------------------------------------------------------
f_SUB proc

	m_pop	eax
	ret

f_SUB endp

; ------------------------------------------------------------------------
f_MUL proc

	m_pop	eax
	ret

f_MUL endp

; ------------------------------------------------------------------------
f_DIV proc

	m_pop	eax
	ret

f_DIV endp

; ------------------------------------------------------------------------
f_LT proc

	m_pop	eax
	ret

f_LT endp

; ------------------------------------------------------------------------
f_EQ proc

	m_pop	eax
	ret

f_EQ endp

; ------------------------------------------------------------------------
f_GT proc

	m_pop	eax
	ret

f_GT endp

; ------------------------------------------------------------------------
f_DICTP proc

	lodsd
	ret

f_DICTP endp

; ------------------------------------------------------------------------
f_EMIT proc

	m_pop	eax
	ret

f_EMIT endp

; ------------------------------------------------------------------------
f_OVER proc

	m_pop	eax
	ret

f_OVER endp

; ------------------------------------------------------------------------
f_COMPARE proc

	m_pop	eax
	ret

f_COMPARE endp

; ------------------------------------------------------------------------
f_FOPEN proc

	m_pop	eax
	ret

f_FOPEN endp

; ------------------------------------------------------------------------
f_FREAD proc

	m_pop	eax
	ret

f_FREAD endp

; ------------------------------------------------------------------------
f_FREADLINE proc

	m_pop	eax
	ret

f_FREADLINE endp

; ------------------------------------------------------------------------
f_FWRITE proc

	m_pop	eax
	ret

f_FWRITE endp

; ------------------------------------------------------------------------
f_FCLOSE proc

	m_pop	eax
	ret

f_FCLOSE endp

; ------------------------------------------------------------------------
f_DTOR proc

	m_pop	eax
	ret

f_DTOR endp

; ------------------------------------------------------------------------
f_RFETCH proc

	m_pop	eax
	ret

f_RFETCH endp

; ------------------------------------------------------------------------
f_RTOD proc

	m_pop	eax
	ret

f_RTOD endp

; ------------------------------------------------------------------------
f_ONEPLUS proc

	m_pop	eax
	ret

f_ONEPLUS endp

; ------------------------------------------------------------------------
f_PICK proc

	m_pop	eax
	ret

f_PICK endp

; ------------------------------------------------------------------------
f_DEPTH proc

	m_pop	eax
	ret

f_DEPTH endp

; ------------------------------------------------------------------------
f_GETCH proc

	m_pop	eax
	ret

f_GETCH endp

; ------------------------------------------------------------------------
f_LSHIFT proc

	m_pop	eax
	ret

f_LSHIFT endp

; ------------------------------------------------------------------------
f_RSHIFT proc

	m_pop	eax
	ret

f_RSHIFT endp

; ------------------------------------------------------------------------
f_AND proc

	m_pop	eax
	ret

f_AND endp

; ------------------------------------------------------------------------
f_OR proc

	m_pop	eax
	ret

f_OR endp

; ------------------------------------------------------------------------
f_BRANCH proc

	m_pop	eax
	ret

f_BRANCH endp

; ------------------------------------------------------------------------
f_BRANCHZ proc

	m_pop	eax
	ret

f_BRANCHZ endp

; ------------------------------------------------------------------------
f_BRANCHNZ proc

	m_pop	eax
	ret

f_BRANCHNZ endp

; ------------------------------------------------------------------------
f_COMPAREI proc

	m_pop	eax
	ret

f_COMPAREI endp

; ------------------------------------------------------------------------
f_BREAK proc

	m_pop	eax
	ret

f_BREAK endp

; ------------------------------------------------------------------------
f_RESET proc

	m_pop	eax
	ret

f_RESET endp

; ------------------------------------------------------------------------
f_BYE proc

	m_pop	eax
	ret

f_BYE endp


; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 7 - Helpers
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
			; Section 3.4.1.1 says non-printable chars can be considered whitespace
			jg			notWS
			cmp			dl, dl		

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
;
; Section 8 - Initialization
;
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

	; Initialize all vectors to "unknown opcode"
	mov		ecx, 255
	mov		edx, offset unkOpcode
	mov		[edi], edx
L1:
	mov		[edi][ecx*4], edx
	loop	L1

	; Fill in known opcodes
	PutVector I_LITERAL, f_LITERAL
	PutVector I_FETCH, f_FETCH
	PutVector I_STORE, f_STORE
	PutVector I_SWAP, f_SWAP
	PutVector I_DROP, f_DROP
	PutVector I_DUP, f_DUP
	PutVector I_SLITERAL, f_SLITERAL
	PutVector I_JMP, f_JMP
	PutVector I_JMPZ, f_JMPZ
	PutVector I_JMPNZ, f_JMPNZ
	PutVector I_CALL, f_CALL
	PutVector I_RET, f_RET
	PutVector I_ZTYPE, f_ZTYPE
	PutVector I_CLITERAL, f_CLITERAL
	PutVector I_CFETCH, f_CFETCH
	PutVector I_CSTORE, f_CSTORE
	PutVector I_ADD, f_ADD
	PutVector I_SUB, f_SUB
	PutVector I_MUL, f_MUL
	PutVector I_DIV, f_DIV
	PutVector I_LT, f_LT
	PutVector I_EQ, f_EQ
	PutVector I_GT, f_GT
	PutVector I_DICTP, f_DICTP
	PutVector I_EMIT, f_EMIT
	PutVector I_OVER, f_OVER
	PutVector I_COMPARE, f_COMPARE
	PutVector I_FOPEN, f_FOPEN
	PutVector I_FREAD, f_FREAD
	PutVector I_FREADLINE, f_FREADLINE
	PutVector I_FWRITE, f_FWRITE
	PutVector I_FCLOSE, f_FCLOSE
	PutVector I_DTOR, f_DTOR
	PutVector I_RFETCH, f_RFETCH
	PutVector I_RTOD, f_RTOD
	PutVector I_ONEPLUS, f_ONEPLUS
	PutVector I_PICK, f_PICK
	PutVector I_DEPTH, f_DEPTH
	PutVector I_GETCH, f_GETCH
	PutVector I_LSHIFT, f_LSHIFT
	PutVector I_RSHIFT, f_RSHIFT
	PutVector I_AND, f_AND
	PutVector I_OR, f_OR
	PutVector I_BRANCH, f_BRANCH
	PutVector I_BRANCHZ, f_BRANCHZ
	PutVector I_BRANCHNZ, f_BRANCHNZ
	PutVector I_COMPAREI, f_COMPAREI
	PutVector I_BREAK, f_BREAK
	PutVector I_RESET, f_RESET
	PutVector I_BYE, f_BYE

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
	mov				var_rStack, offset var_rStack

	ret

bootStrap endp

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 10 - The run time interpreter
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; This is used to execute one VM instruction.

executeOpcode proc						; EDX = opcode, ESI = IP

	and		eax, 00ffh					; ensure range 0-255 ... all 256 entries have handlers.
	shl		eax, 2
	mov		ecx, primVectors[eax]		; most cause a jump to the invalid opcode handler.
	jmp		ecx							; jump to the handler.

executeOpcode endp


; ---------------------------------------------------------------------------------------------------------
; This is the main execution loop for the interpreter.

; ESI = IP
runForthVM proc	
	
L1:
	; NOTE: the direction flag is ASSUMED to be clear.
	; Anything that sets it MUST clear it before exiting.
	lodsb
	cmp				al, I_RET
	je				doRet

	cmp				al, I_BYE
	je				allDone

	call			executeOpcode
	jmp				L1

doRet:
	mov		edx, var_rStack			; If the return stack is empty, then we are done
	cmp		edx, offset var_rStack
	jle		allDone

	call rPop
	mov  esi, edx
	jmp  L1

allDone:
	ret

runForthVM endp

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 13 - Program entry point
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
main		proc

	mov			initialESPVal, esp
	call		bootStrap 
	jmp			runForthVM

main	endp
end main

; ---------------------------------------------------------------------------------------------------------
