; ---------------------------------------------------------------------------------------------------------
; source in ESI
stringComma		proc

		push		esi

		xor			ecx, ecx
		mov			cl, [esi]
		add			ecx, 2			; for the count and null terminator
		xor			edx, edx

L1:
		mov			dl, [esi]
		m_cPush		dl
		call		fComma_8
		inc			esi
		loop		L1

		pop			esi
		ret

stringComma		endp

; ---------------------------------------------------------------------------------------------------------
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

			; First 4 bytes is the XT & Flags
			STOSD								; EAX has the old LAST
			mov				eax, var_HERE		; Initial XT is HERE
			STOSD
			xor				al, al				; Flags: ready
			or				al, READY_BIT
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

			push	var_HERE		; return the XT
			push	esi

L1:			
			mov		dl, [esi]
			cmp		dl, COMPILE_STOP
			je		allDone
			m_cPush	dl
			call	fComma_8
			inc		esi
			jmp		L1

allDone:	
			pop		esi
			pop		eax
			ret

compileCode		endp

; ---------------------------------------------------------------------------------------------------------
compile8Bit		proc



compile8Bit		endp
; ---------------------------------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------------------------------
; * NOT TESTED **
initializeWords	proc

			; First word: HERE
			mov		esi, offset name_HERE		; ESI used by addWord - Name
			call	addWord
			val2mem	xt_HERE, var_HERE
			mov		esi, offset code_HERE
			call	compileCode

			; First word NEXT pointer cleanup
			; mov		edx, var_LAST
			; mov	 DWORD PTR	[edx], 0
			
			; 2nd word: LAST
			mov		esi, offset name_LAST		; ESI used by addWord - Name
			call	addWord
			val2mem	xt_LAST, var_HERE
			mov		esi, offset code_LAST
			call	compileCode

			; Constants
			mov		ecx, offset array_Vars_8	; Generate 32-bit constant values ... (HERE), (LAST), STATE, ...
nextCon:	mov		esi, [ecx]					; Name
			test	esi, esi
			jz		addWords
			push	ecx
			call	addWord
			pop		ecx
			mov		edx, [ecx+4]				; variable address
			mov		esi, offset code_GenConst
			mov		[esi+1], edx				; Set the address
			push	ecx
			call	compileCode
			pop		ecx
			add		ecx, 8
			jmp		nextCon

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

			; For the words that represent the primitives
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

addx86:			; Words that are implemented in machine code
addOther:		; There are currently no special words
			ret

initializeWords	endp

; ---------------------------------------------------------------------------------------------------------
