IF !DEF(CONSTANTS_INC)
DEF CONSTANTS_INC EQU 1


DEF CART_DETECT_0 EQU $A5
DEF CART_DETECT_1 EQU $3C

DEF CART_TYPE_HUC EQU $00
DEF CART_TYPE_MBC EQU $0A

DEF CART_IR_ENABLE EQU $0E

; console types (wConsole)
DEF CONSOLE_DMG EQU $00
DEF CONSOLE_SGB EQU $01
DEF CONSOLE_CGB EQU $02

MACRO const_def
	IF _NARG > 0
		DEF const_value = \1
	ELSE
		DEF const_value = 0
	ENDC
ENDM

MACRO const
	DEF \1 EQU const_value
	DEF const_value = const_value + 1
ENDM

; commands transmitted through IR to be
; executed by the other device
; (see ExecuteReceivedIRCommands)
	const_def
	const IRCMD_CLOSE             ; $0
	const IRCMD_RETURN_WO_CLOSING ; $1
	const IRCMD_TRANSMIT_DATA     ; $2
	const IRCMD_RECEIVE_DATA      ; $3
	const IRCMD_CALL_FUNCTION     ; $4
DEF NUM_IR_COMMANDS EQU const_value

; parameters for IR communication
; (see InitIRCommunications)
	const_def 1
	const IRPARAM_CARD_POP    ; $1
	const IRPARAM_SEND_CARDS  ; $2
	const IRPARAM_SEND_DECK   ; $3

DEF rJOYP       EQU $ff00 ; Joypad (R/W)
DEF P15              EQU %00100000
DEF JOY_BTNS_SELECT  EQU P15
DEF P14              EQU %00010000
DEF JOY_DPAD_SELECT  EQU P14
DEF JOY_INPUT_MASK   EQU %00001111
DEF P13              EQU %00001000
DEF P12              EQU %00000100
DEF P11              EQU %00000010
DEF P10              EQU %00000001

DEF STAT_LCDC_STATUS  EQU %00000011
DEF STAT_ON_HBLANK    EQU %00000000 ; H-Blank
DEF STAT_ON_VBLANK    EQU %00000001 ; V-Blank
DEF STAT_ON_OAM       EQU %00000010 ; OAM-RAM is used by system
DEF STAT_ON_LCD       EQU %00000011 ; Both OAM and VRAM used by system

DEF TAC_START     EQU $04
DEF TAC_STOP      EQU $00
DEF TAC_4096_HZ   EQU $00
DEF TAC_262144_HZ EQU $01
DEF TAC_65536_HZ  EQU $02
DEF TAC_16384_HZ  EQU $03

ENDC ; CONSTANTS_INC
