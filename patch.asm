include "include/hardware.inc"
include "include/overlay.inc"
include "include/macros.inc"
include "include/constants.inc"
include "include/charmap_w.inc"

; hardware.inc包含各种硬件和寄存器的定义，建议使用里面的方式标注寄存器或者硬件参数
; overlay.inc包含org（代码定位）的用法和cleartill的用法
; charmap_w.inc定义了一种实现中文编码的方法 (见https://github.com/SnDream/charmap_w.inc)
; macros.inc建议定义一些自己编写的宏或者其他的内容
; constants.inc建议定义一些代码的常数

; 可以往后继续编写补丁代码

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

	org $077b
oSetROMBank::
	; push af
	; ldh [ohSRAMBank], a
	; ld [rRAMB], a
	; xor a
	; ld [rRAMG], a
	; pop af
	; ret
	push af
	ldh [ohSRAMBank], a
	ld [rRAMB], a
	jr oEnableSRAM.entry
	nop
	nop
	pop af
	ret

	org $0787
oEnableSRAM::
	push af
.entry
	ld a, CART_SRAM_ENABLE
	ld [rRAMG], a
	pop af
	ret
	
	org $078f
oDisableSRAM::
	; push af
	; farcall oDisableSRAMEmpty
	; xor a
	; ld [rRAMG], a
	; pop af
	; ret
	push af
	farcall oDisableSRAMEmpty
	jr oEnableSRAM.entry
	nop
	nop
	pop af
	ret

	org $66a0, $06
oDisableSRAMEmpty::
	ret

	org $577a, $06
oDisableIR::
	push af
	xor a
	jr oEnableIR.entry

	org $577e, $06
oEnableIR::
	push af
	ld a, $00 ; ld a, $0e
.entry
	ld [rRAMG], a
	pop af
	ret

	org $6633, $06
oUnusedSaveDataValidation::

	org $668c, $06
oUnusedSaveDataValidation.entry
	; ld a, CART_SRAM_ENABLE
	; ld [rRAMG], a
	; xor a
	; ldh [ohSRAMBank], a
	; ld [rRAMB], a
	; ld [_SRAM], a
	; ld [rRAMG], a
	; jp oReset

	org $04fd
oReset::
