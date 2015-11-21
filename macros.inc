m_initVar	macro	varName, memOffset

			mov varname, ebp
			add varname, memOffset

			endm

; ---------------------------------------------------------------------------------------------------------
m_Push		macro	Val

			sub			ebp, SIZE_DWORD
			cmp			ebp, dStack_MIN
			jl			stackOverFlow
			mov	DWORD PTR [ebp], Val

			endm

; ---------------------------------------------------------------------------------------------------------
m_cPush		macro	Val

			m_Push		0
			mov	BYTE PTR [ebp], Val

			endm

; ---------------------------------------------------------------------------------------------------------
m_pushVal	macro	theVar

			mov		edx, theVar
			m_Push	edx

			endm

; ---------------------------------------------------------------------------------------------------------
m_getTOS	macro	tgtReg

			cmp			ebp, dStack_MAX
			jg			stackEmpty
			mov			tgtReg, [ebp]

			endm

; ---------------------------------------------------------------------------------------------------------
m_setTOS	macro	fromReg

			mov DWORD PTR [ebp], fromReg

			endm

; ---------------------------------------------------------------------------------------------------------
m_Drop		macro

			add			ebp, SIZE_DWORD

			endm

; ---------------------------------------------------------------------------------------------------------
m_Pop		macro	toReg

			m_getTOS toReg
			m_Drop

			endm

; ---------------------------------------------------------------------------------------------------------
m_isStackEmpty		macro	numNeeded

			cmp			ebp, dStack_MAX
			jg			stackEmpty

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

			m_Push		theVal
			m_execO		opCode

			endm

; ---------------------------------------------------------------------------------------------------------
m_Advance_CELL	macro	theReg		; this is assumed to be a register

		ADD		theReg, SIZE_DWORD
		endm

; ---------------------------------------------------------------------------------------------------------
m_Advance_CHAR	macro	theReg		; this is assumed to be a register

		ADD		theReg, SIZE_CHAR
		
		endm

; ---------------------------------------------------------------------------------------------------------
m_MACRO_TEMPL	macro	param1		; macro comments

		; macro guts here ...

		endm

; ---------------------------------------------------------------------------------------------------------
val2mem		macro		addrTo, theVal

		push	theVal
		pop		addrTo

			endm
; ---------------------------------------------------------------------------------------------------------
