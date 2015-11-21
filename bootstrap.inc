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
	PutVector I_25, fI_25
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

	mov				ecx, theMemory
	add				ecx, 4
	mov	byte ptr	[ecx], 00bah
	mov	DWORD ptr	[ecx+1], 11223344h
	mov	byte ptr	[ecx+5], 00c3h
	call			ecx
	; EDX should get 11223344h in it

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

	; VM initialize
	;mov				memoryMin, offset var_HERE
	;mov				memoryMax, eax
	;add				memoryMax, cur_MEMSIZE


	; Initialize the stacks
	xor				edi, edi
	mov				var_rStack, edi
	; mov				var_cStack, edi

	; Built in words ...
	call initializeWords
	
	ret

bootStrap endp

; ---------------------------------------------------------------------------------------------------------
