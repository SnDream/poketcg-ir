IF !DEF(MACROS_INC)
DEF MACROS_INC EQU 1

; 

MACRO lb ; r, hi, lo
	ld \1, ((\2) & $ff) << 8 | ((\3) & $ff)
ENDM

; 简单定义 db dw的方法，一般用于编写地址

MACRO dbw
	db \1
	dw \2
ENDM

; 简单定义符号位置的方法

MACRO dba
	db BANK(\1)
	dw \1
ENDM

; copy from pret/poketcg

MACRO ldtx
	IF _NARG == 2
		ld \1, \2_
	ELSE
		ld \1, \2_ \3
	ENDC
ENDM

MACRO bank1call
	rst $18
	dw \1
ENDM

MACRO farcall
	rst $28
	IF _NARG == 1
		db BANK(\1)
		dw \1
	ELSE
		db \1
		dw \2
	ENDC
ENDM

ENDC ; MACROS_INC
