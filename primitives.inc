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
fI_24 proc

		ret

fI_24 endp

; ---------------------------------------------------------------------------------------------------------
fI_25 proc

		ret

fI_25 endp

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
x86_TEST1 proc

		jmp		wordEnd

		call	fDUP
		call	fONEPLUS
		call	fPLUS
wordEnd:
		ret

x86_TEST1 endp

; ---------------------------------------------------------------------------------------------------------
x86_TEST2 proc

wordEnd:
		ret

x86_TEST2 endp

