; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
fABORT proc
	ret
fABORT endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
executeOpcode proc						; EDX = opcode, ESI = IP

	and		edx, 00ffh					; ensure range 0-255 ... all 255 entries have handlers.
	mov		ecx, primVectors[edx*4]		; most cause a jump to the invalid opcode handler.
	jmp		ecx							; jump to the handler.


executeOpcode endp

; ---------------------------------------------------------------------------------------------------------
; ** NOT TESTED **
; ESI = IP
executeWord proc
	
L1:
	xor				edx, edx
	mov				dl, [esi]
	inc				esi
	cmp				dl, I_RETURN
	je				doRet

	mov				ecx, primVectors[edx*4]		; primVectors is a big jump table
	call			ecx							; all 255 entries have handlers, most of which are "invalid opcode"
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
executeWord_SAFE	proc		; word address in EDX

	push	esi
	mov		esi, edx
	call	executeWord
	pop		esi

	ret

executeWord_SAFE	endp

; ---------------------------------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------------------------------
