; Forth in assembler

; ---------------------------------------------------------------------------------------------------------
; .686P
; .model flat, stdcall
; .stack 4096
; ExitProcess PROTO, dwExitCode:dword

; Constants
include constants.inc

; ---------------------------------------------------------------------------------------------------------
StackSize	EQU		fourKilo
include irvine32.inc

; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; MACROS
include macros.inc

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
msgWordLen	BYTE	"Maximum word length is 15 characters.", 0
msgBadOP	BYTE	"Invalid opcode encountered.", 0
msgNoMem	BYTE	"Fatal error - memory allocation failed!", 0
msgBadMem	BYTE	"Fatal error - memory addresses out of expected range!", 0
wsChars		BYTE	4, 9, 10, 13, 32
char2Num	BYTE	"0123456789ABCDEF", 0

theMemory	DWORD	0

bytesRead	DWORD ?
hStdIn		HANDLE ?
hStdOut		HANDLE ?
SizeReadWrite DWORD 0

initialESPVal	DWORD ?

include variables.inc

; ---------------------------------------------------------------------------------------------------------
.code

include primitives.inc
include helpers.inc
include words.inc
include bootstrap.inc
include runtime.inc
; include test.inc

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
	call			fWord2			; Puts current word address on the stack.
	m_Pop			eax
	mov				dl, [eax]		; PAD is empty when at EOL
	test			dl, dl
	jz				allDone

	call			doWord
	jmp				doOne

allDone:
	ret

processLine endp

; ---------------------------------------------------------------------------------------------------------
doError	proc			; Error message in EDX

		call		WriteString

doError endp

; ---------------------------------------------------------------------------------------------------------
mainLoop	proc

	; Initialize the stacks
	mov				ebp, dStack_MAX
	add				ebp, SIZE_DWORD
	xor				edi, edi
	mov				var_rStack, 0
	; mov				var_cStack, 0

	mov				var_STATE, 0
	mov				var_BASE, 10

	mov				esp, initialESPVal

loopStart:
	; Display prompt
	mov  edx, offset msgPrompt		; string addr in edx
	call WriteString
	; invoke locate,10,3

	; Wait for commands from console
	mov		edx, offset var_InpBuf
	inc		edx							; save room for the count BYTE

	INVOKE	ReadConsole, hStdIn, edx, inpBuf_SZ, ADDR bytesRead, 0

	mov				edx, bytesRead			; count includes CR and LF
	sub				edx, 2					; remove CR/LF
	mov				var_InpBuf, dl			; store the count
	m_Push			0
	m_Push			offset var_InpBuf
	m_execXT		offset code_Count
	m_execO			I_PLUS
	m_execO			I_STORE_8

	; is the command "bye"?
	mov				edi, offset cmdBye
	mov				esi, offset var_InpBuf
	call			strCmpC

	test  edx, edx						; strCmpX returns result in EDX
	jnz  goodBye

	; is the command "restart!"?
	mov				edi, offset cmdRestart
	mov				esi, offset var_InpBuf
	call			strCmpC

	test  edx, edx						; strCmpX returns result in EDX
	jz  doIt

	call bootStrap
	mov  edx, offset resetOK		; string addr in edx
	call WriteString
	jmp mainLoop

doIt:
	call processLine

	mov  edx, offset msgOK				; string addr in EDX
	call WriteString

	jmp loopStart

goodBye:
	mov  edx, offset msgBye		; string addr in edx
	call WriteString

	invoke ExitProcess,0

mainLoop endp

; ---------------------------------------------------------------------------------------------------------
main		proc

	mov			edx, offset welcome		; string addr in edx
	call		WriteString

	mov			initialESPVal, esp
	call		bootStrap 
	jmp			mainLoop
	; call fTest
	; NO return here ... fall through 

main	endp
end main

; ---------------------------------------------------------------------------------------------------------
