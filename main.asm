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

memorySize		EQU oneKilo * 16	; The default memory size
stackSize		EQU oneKilo
dStackSize		EQU bytes256 * 2	; The size of the data stack
rStack_SZ		EQU bytes256 * 2	; The size of the return stack

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
m_TOS		macro

			DWORD PTR [ebp]

			endm

; ---------------------------------------------------------------------------------------------------------
m_cTOS		macro

			BYTE PTR [ebp]

			endm

; ---------------------------------------------------------------------------------------------------------
abs2vm		macro	tgt, absolute

			mov tgt, absolute
			sub tgt, theMemory

			endm

; ---------------------------------------------------------------------------------------------------------
vm2abs		macro	tgt, offset

			mov tgt, theMemory
			add tgt, offset

			endm

; ---------------------------------------------------------------------------------------------------------
m_Store		macro	offset, val

			push edi
			mov edi, theMemory
			mov DWORD PTR [edi][offset], val
			pop edi

			endm

; ---------------------------------------------------------------------------------------------------------
m_cStore	macro	offset, val

			push edi
			mov edi, theMemory
			mov BYTE PTR [edi][offset], val
			pop edi

			endm

; ---------------------------------------------------------------------------------------------------------
m_Fetch		macro	tgt, offset

			push edi
			mov edi, theMemory
			mov DWORD PTR tgt, DWORD PTR [edi][offset]
			pop edi

			endm

; ---------------------------------------------------------------------------------------------------------
m_cFetch	macro	tgt, offset

			push edi
			mov edi, theMemory
			mov BYTE PTR tgt, BYTE PTR [edi][offset]
			pop edi

			endm

; ---------------------------------------------------------------------------------------------------------
m_instr macro inst

	mov al, inst
	stosb

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
theStacks	DWORD ?
bytesRead	DWORD ?

hStdIn		HANDLE ?
hStdOut		HANDLE ?
SizeReadWrite DWORD 0

initialESPVal	DWORD ?

dStack_MIN		DWORD 	0
dStack_MAX		DWORD 	0
var_rStack		DWORD 	rStack_SZ DUP (0)

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 4 - Variables for creating the built-in words
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------

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
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
stackEmpty proc

		; mov			edx, offset msgStackE
		; jmp			doError

stackEmpty endp

; ---------------------------------------------------------------------------------------------------------
stackUnderFlow proc			; If we get here, the stack could be hosed ...

		; mov			edx, offset msgStackU
		; jmp			doError

stackUnderFlow endp

; ---------------------------------------------------------------------------------------------------------
stackOverFlow proc			; If we get here, the stack could be hosed ...

		; mov			edx, offset msgStackO
		; jmp			doError

stackOverFlow endp

; ---------------------------------------------------------------------------------------------------------
; pushes EDX onto the return stack

rPush:

		push		ecx

		add DWORD PTR var_rStack, 4
		mov			ecx, var_rStack
		mov			[ecx], edx

		pop			ecx
		ret

; ---------------------------------------------------------------------------------------------------------
; pops the return stack into EDX

rPop:

		push		ecx

		mov			ecx, var_rStack
		mov			edx, [ecx]
		sub			ecx, 4
		mov			var_rStack, ecx

		pop			ecx
		ret

isEmpty:
		; mov			edx, offset msgRStackE
		; jmp			doError

doError:
		ret


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
f_LITERAL:

	lodsd
	m_push	eax	
	ret

; ------------------------------------------------------------------------
f_FETCH:

	m_getTOS	edx
	m_Fetch		eax, edx
	m_setTOS	eax
	ret

; ------------------------------------------------------------------------
f_STORE:

	m_pop		edx
	m_pop		eax
	m_Store		edx, eax
	ret

; ------------------------------------------------------------------------
f_SWAP:

	m_pop		eax
	m_getTOS	edx
	m_setTOS	eax
	m_push		edx
	ret

; ------------------------------------------------------------------------
f_DROP:

	m_drop
	ret

; ------------------------------------------------------------------------
f_DUP:

	m_getTOS	eax
	m_push		eax
	ret

; ------------------------------------------------------------------------
f_SLITERAL:

		mov		eax, esi			; push the string start address
		sub		eax, [theMemory]
		m_push	eax
		lodsb						; Count byte
		xor		ecx, ecx
		mov		cl, al
		rep		lodsb				; skip string data
		mov		al, [esi]
		cmp		al, 0				; if it ends with NULL, skip that too
		jne		sl1
		lodsb
sl1:	ret

; ------------------------------------------------------------------------
f_JMP:

		lodsd
		vm2abs	esi, eax
		ret

; ------------------------------------------------------------------------
f_JMPZ:

	m_pop	eax
	cmp		eax, 0
	je		f_JMP
	lodsd
	ret

; ------------------------------------------------------------------------
f_JMPNZ:

	m_pop	eax
	cmp		eax, 0
	jne		f_JMP
	lodsd
	ret

; ------------------------------------------------------------------------
f_CALL:

	lodsd
	abs2vm		edx, esi
	call		rPush
	vm2abs		esi, eax
	ret

; ------------------------------------------------------------------------
f_RET:

	call		rPop
	vm2abs		esi, edx
	ret

; ------------------------------------------------------------------------
f_ZTYPE:

	m_pop	eax
	; TODO
	ret

; ------------------------------------------------------------------------
f_CLITERAL:

	lodsb
	m_cPush	al
	ret

; ------------------------------------------------------------------------
f_CFETCH:

	m_getTOS	ecx
	m_cFetch	al, ecx
	and			eax, 000000ffh
	m_setTOS	eax
	ret

; ------------------------------------------------------------------------
f_CSTORE:

		m_pop		ecx
		m_pop		eax
		m_cStore	ecx, al
		ret

; ------------------------------------------------------------------------
f_ADD:

		m_pop		edx
		m_getTOS	eax
		add			eax, edx
		m_setTOS	eax
		ret

; ------------------------------------------------------------------------
f_SUB:

		m_pop		edx
		m_getTOS	eax
		sub			eax, edx
		m_setTOS	eax
		ret

; ------------------------------------------------------------------------
f_MUL:

		m_pop		ecx
		m_getTOS	eax
		imul		eax, ecx
		m_setTOS	eax
		ret

; ------------------------------------------------------------------------
f_DIV:

		m_pop		ecx
		m_getTOS	eax
		; cmp			ecx, 0
		; je			divByZero
		idiv		ecx
		m_setTOS	eax
		ret

; ------------------------------------------------------------------------
f_LT:

		xor			edx, edx
		m_pop		ecx
		m_getTOS	eax
		.IF (eax < ecx)
			dec			edx
		.ENDIF
		m_setTOS	edx

		ret

; ------------------------------------------------------------------------
f_EQ:

		xor			edx, edx
		m_pop		ecx
		m_getTOS	eax
		.IF (eax == ecx)
			dec			edx
		.ENDIF
		m_setTOS	edx
		ret

; ------------------------------------------------------------------------
f_GT:

		xor			edx, edx
		m_pop		ecx
		m_getTOS	eax
		.IF (eax > ecx)
			dec			edx
		.ENDIF
		m_setTOS	edx
		ret

; ------------------------------------------------------------------------
f_DICTP:

		LODSD
		ret

; ------------------------------------------------------------------------
f_EMIT:

		; TODO
		ret

; ------------------------------------------------------------------------
f_OVER:

		; TODO
		ret

; ------------------------------------------------------------------------
f_COMPARE:

		; TODO
		ret

; ------------------------------------------------------------------------
f_FOPEN:

		; TODO
		ret

; ------------------------------------------------------------------------
f_FREAD:

		; TODO
		ret

; ------------------------------------------------------------------------
f_FREADLINE:

		; TODO
		ret

; ------------------------------------------------------------------------
f_FWRITE:

		; TODO
		ret

; ------------------------------------------------------------------------
f_FCLOSE:

		; TODO
		ret

; ------------------------------------------------------------------------
f_DTOR:

	m_pop		edx
	call		rPush
	ret

; ------------------------------------------------------------------------
f_RFETCH:

	call		rPop
	call		rPush
	m_push		edx
	ret

; ------------------------------------------------------------------------
f_RTOD:

	call		rPop
	m_push		edx
	ret

; ------------------------------------------------------------------------
f_ONEPLUS:

		inc DWORD PTR [ebp]
		; m_getTOS	eax
		; inc			eax
		; m_setTOS	eax
		ret

; ------------------------------------------------------------------------
f_PICK:

		; TODO
		ret

; ------------------------------------------------------------------------
f_DEPTH:

		; TODO
		ret

; ------------------------------------------------------------------------
f_GETCH:

		; TODO
		ret

; ------------------------------------------------------------------------
f_LSHIFT:

		m_pop		ecx
		m_getTOS	eax
		shl			eax, cl
		m_setTOS	edx
		ret

; ------------------------------------------------------------------------
f_RSHIFT:

		m_pop		ecx
		m_getTOS	eax
		shr			eax, cl
		m_setTOS	edx
		ret

; ------------------------------------------------------------------------
f_AND:

		m_pop		ecx
		m_getTOS	eax
		and			eax, ecx
		m_setTOS	edx
		ret

; ------------------------------------------------------------------------
f_OR:

		m_pop		ecx
		m_getTOS	eax
		or			eax, ecx
		m_setTOS	edx
		ret

; ------------------------------------------------------------------------
f_BRANCH:

		; TODO
		ret

; ------------------------------------------------------------------------
f_BRANCHZ:

		; TODO
		ret

; ------------------------------------------------------------------------
f_BRANCHNZ:

		; TODO
		ret

; ------------------------------------------------------------------------
f_COMPAREI:

		; TODO
		ret

; ------------------------------------------------------------------------
f_BREAK:

		; TODO
		ret

; ------------------------------------------------------------------------
f_RESET:

		cld
		mov		esi, theMemory
		ret

; ------------------------------------------------------------------------
f_BYE:

		; TODO
		ret

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 7 - Helpers
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------------------------------
; Sets the CARRY flag and AL=the number

isBase10Ch: 

		cmp			al, '0'
		jl			ncNo
		cmp			al, '9'
		jg			ncNo
		sub			al, '0'

ncYes:	stc
		ret
ncNo:	clc
		ret

; ---------------------------------------------------------------------------------------------------------
; Sets the CARRY flag and AL=the number

isBase16Ch:
		call		isBase10Ch
		jc			ncYes
		cmp			al, 'F'
		jle			b16U
		sub			al, 32
b16U:	cmp			al, 'A'
		jl			ncNo
		cmp			al, 'F'
		jg			ncNo
		sub			al, 'A'
		jmp			ncYes

; ---------------------------------------------------------------------------------------------------------
; Sets the CARRY flag if the char in DL is whitespace

isWS:

			cmp			dl, ' '
			jle			ncYes
			jmp			ncNo

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
tolower:
	
			cmp		dl, 'A'
			jl		tlExit
			cmp		dl, 'Z'
			jg		tlExit
			add		dl, 32

tlExit:	ret

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

	mov [edi][nOpCode*4], offset pProc

	endm

; ---------------------------------------------------------------------------------------------------------
unkOpcode:

	jmp f_RESET

; ---------------------------------------------------------------------------------------------------------
bsVectors:

	mov  edi, offset primVectors

	; Initialize all vectors to "unknown opcode"
	mov		ecx, 255
	mov		eax, offset unkOpcode
	rep		stosd

	; Fill in known opcodes
	mov  edi, offset primVectors
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
	invoke			VirtualAlloc, 0, stackSize, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE

	.IF eax == 0
		mov			edx, offset msgNoMem
		call		WriteString
		INVOKE	ReadConsole, hStdIn, edx, 10, ADDR bytesRead, 0
		invoke ExitProcess,0
	.ENDIF
	mov				theStacks, eax

	; Initialize the stacks
	mov				dStack_MIN, eax
	add				eax, dStackSize
	mov				var_rStack, eax

	; leave a buffer of 4 CELLS around each stack
	mov				eax, dStack_MIN
	add				eax, 4*CELL_SIZE
	mov				dStack_MIN, eax

	mov				eax, var_rStack
	sub				eax, 4*CELL_SIZE
	mov				dStack_MAX, eax
	mov				ebp, eax

	mov				eax, var_rStack
	add				eax, 4*CELL_SIZE
	mov				var_rStack, eax

	; 
	mov				edi, theMemory
	m_instr I_CLITERAL
	m_instr 1
	m_instr I_CLITERAL
	m_instr 2
	m_instr I_ADD
	m_instr I_JMPNZ
	m_instr 0
	m_instr 0
	m_instr 0
	m_instr 0

	ret

bootStrap endp

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; Section 10 - The run time interpreter
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; This is the main execution loop for the interpreter.

runCPULoop proc			; ESI = IP
	
cpuLoop:
	; NOTE: the direction flag is ASSUMED to be clear.
	; Anything that sets it MUST clear it before exiting.
	lodsb
	cmp			al, I_BYE
	je			cpuDone

	and			eax, 00ffh					; ensure range 0-255 ... all 256 entries have handlers.
	shl			eax, 2
	mov			ecx, primVectors[eax]		; most cause a jump to the invalid opcode handler.
	call		ecx							; jump to the handler.
	jmp			cpuLoop

cpuDone:
	ret

runCPULoop endp

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
	cld
	mov			esi, theMemory
	jmp			runCPULoop

main	endp
end main

; ---------------------------------------------------------------------------------------------------------
