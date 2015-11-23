; ---------------------------------------------------------------------------------------------------------
; This is a PCode based implementation of Forth.
;
; The data stack is in its own allocated memory space. EBP is the stack pointer. Like the x86 stack pointer,
; PUSH decrements the pointer, and POP increments it. [EBP] can be used to refer to the element on the top
; of the stack. Every entry (e.g. the value from C@) on the stack is 32 bits.
; 
; A dictionary entry looks like this:
;
; Offset
; 00-03		DWORD		Prev		; Address of previous dictionary entry: the current LAST.
; 04-07		DWORD		XT			; The "execution token" ... the address where the word's code starts.
; 08-08		BYTE 		Flags		; Flags ... see below.
; 09-09		BYTE		Len			; Length byte.
; 10-??		BYTE(?)		Name		; Word name, ZERO delimited
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
