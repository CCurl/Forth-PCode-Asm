; ---------------------------------------------------------------------------------------------------------
; constants ...

; For VirtualAlloc and VirtualFree
PAGE_EXECUTE_READWRITE	EQU 0040h
MEM_COMMIT				EQU 01000h
MEM_RESERVE				EQU 02000h
MEM_RELEASE				EQU 08000h

; ---------------------------------------------------------------------------------------------------------

oneKilo			EQU 400h
twoKilo			EQU oneKilo * 2
fourKilo		EQU oneKilo * 4
eightKilo		EQU oneKilo * 8

oneMB			EQU oneKilo * oneKilo
twoMB			EQU oneMB * 2
fourMB			EQU oneMB * 4
eightMB			EQU oneMB * 8
sixteenMB		EQU oneMB * 16

SIZE_DWORD		EQU 4
SIZE_WORD		EQU 2
SIZE_CHAR		EQU 1

memorySize		EQU sixteenMB
dStackSize		EQU fourKilo
cStack_SZ		EQU twoKilo
rStack_SZ		EQU twoKilo

; Pointers
; MemLoc_dsPTR	EQU 0000h
; MemLoc_HERE		EQU 0004h
; MemLoc_LAST		EQU 0008h
; MemLoc_STATE	EQU 000Ch
; MemLoc_TOIN		EQU 0010h
; MemLoc_SOURCE	EQU 0014h
; MemLoc_MemSZ	EQU 0018h
; MemLoc_BASE		EQU 001ch

inpBuf_SZ		EQU	 0100h

; VmLoc_CodeStart	EQU  oneKilo * 4
; VmLoc_DictStart	EQU  VmLoc_CodeStart

COMPILE_STOP	EQU -99

; For Dictionary entries
IMM_BIT				EQU 01h
READY_BIT			EQU 02h
x86_BIT				EQU 10h
COMPILE_ONLY_BIT	EQU 80h
NEXT_OFFSET			EQU  0
XT_OFFSET			EQU  4
FLAGS_OFFSET		EQU  8
NAME_OFFSET			EQU  9
ENTRY_OVERHEAD		EQU 10

; ---------------------------------------------------------------------------------------------------------
; VM opcodes

	I_FETCH			EQU   1
	I_STORE			EQU   2
	I_LITERAL		EQU   3
	I_DUP			EQU   4
	I_SWAP			EQU   5
	I_DROP			EQU   6
	I_PICK			EQU   7
	I_ROT			EQU   8
	I_ONEPLUS		EQU   9
	I_PLUS			EQU  10
	I_MINUS			EQU  11
	I_MULT			EQU  12
	I_DIV			EQU  13
	I_EQ			EQU  14
	I_CALL			EQU  15
	I_LT			EQU  16		; 10h
	I_GT			EQU  17
	I_TO_R			EQU  18		; 12h
	I_R_FROM		EQU  19
	I_R_AT			EQU  20		; 14h
	I_JMPZ			EQU  21
	I_LOOKUP		EQU  22		; 16h
	I_X86			EQU  23
	I_JMPNZ			EQU  24		; 18h
	I_25			EQU  25
	I_DEPTH			EQU  26		; 1ah
	I_ONEMINUS		EQU  27
	I_AND			EQU  28		; 1ch
	I_OR			EQU  29
	I_XOR			EQU  30		; 1eh
	I_DIVMOD		EQU  31
	I_EMIT			EQU  32		; 20h
	I_DICTP			EQU  33
	I_TWOTIMES		EQU  34		; 22h
	I_TWODIV		EQU  35
	I_FOPEN			EQU  36		; 24h
	I_FCLOSE		EQU  37
	I_FREAD			EQU  38		; 26h
	I_FWRITE		EQU  39
	I_FGETC			EQU  40		; 28h
	I_FETCH_8		EQU  41
	I_STORE_8		EQU  42		; 2ah
	I_LITERAL_8		EQU  43
	I_COMMA			EQU  44		; 2ch
	I_COMMA_8		EQU  45
	I_OVER			EQU  46		; 2eh
	I_STRCMP		EQU  47
	I_STRCMPI		EQU  48		; 30h
	I_CREATE		EQU  49
	I_WORD			EQU  50		; 32h
	I_BRANCHF		EQU  51
	I_GOTO			EQU  52		; 34h
	I_EXECUTE		EQU  53
	I_DOT			EQU  54		; 36h
	; 54 -> 97 unused
	I_BREAK			EQU  98		; 62h
	I_RETURN		EQU  99		; 63h

; ---------------------------------------------------------------------------------------------------------
; 
; x86_TEST1 PROTO
; x86_TEST2 PROTO
