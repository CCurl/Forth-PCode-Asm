; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------
; ---------------------------------------------------------------------------------------------------------

; ----------------------; ---------------------------------------------------------------------------------------------------------
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
; ** TESTED **
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
	
			cmp		dl, 65			; < 'A' = OK
			jl		allDone

			cmp		dl, 90			; > 'Z' = OK
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
; ** NOT TESTED **
; Copies counted strings - source string in [ESI], destination string in [EDI]
; First BYTE is the count
strCpyC proc PUBLIC 
	
	push ecx

	xor  ecx, ecx
	mov  cl, [esi]
	inc  ecx				; include the count and trailing null
	inc  ecx
	cld						; make sure the direction flag is correct
	rep movsb

	pop ecx
	ret

strCpyC endp

; ---------------------------------------------------------------------------------------------------------

locate proc x:DWORD,y:DWORD

    ; LOCAL xyVar    :DWORD

  ; -----------------------------------
  ; make both co-ordinates into a DWORD
  ; -----------------------------------
    mov  ecx, x
    mov  eax, y
    shl  eax, 16
    mov  ax, cx

	push		eax
	push		hStdOut
	call SetConsoleCursorPosition

    ;invoke SetConsoleCursorPosition, hStdOut, eax

    ret

locate endp

; ---------------------------------------------------------------------------------------------------------
