IF !DEF(CONSTANTS_INC)
DEF CONSTANTS_INC EQU 1


DEF CART_DETECT_0 EQU $A5
DEF CART_DETECT_1 EQU $3C

DEF CART_IR_ENABLE EQU $0E

; copy from pret/poketcg

DEF NAME_BUFFER_LENGTH          EQU 16

DEF PLAYER_TURN   EQUS "HIGH(wPlayerDuelVariables)"
DEF OPPONENT_TURN EQUS "HIGH(wOpponentDuelVariables)"

DEF NUM_DECKS                EQU 4
DEF DECK_NAME_SIZE           EQU 24
DEF DECK_NAME_SIZE_WO_SUFFIX EQU 21 ; name part before "deck"
DEF DECK_SIZE                EQU 60
DEF DECK_STRUCT_SIZE         EQU DECK_NAME_SIZE + DECK_SIZE
DEF DECK_CONFIG_BUFFER_SIZE  EQU 80
DEF MAX_NUM_SAME_NAME_CARDS  EQU 4
DEF MAX_UNNAMED_DECK_NUM     EQU 999

; max number of player names that
; can be written to sCardPopNameList
DEF CARDPOP_NAME_LIST_MAX_ELEMS EQU 16
DEF CARDPOP_NAME_LIST_SIZE EQUS "CARDPOP_NAME_LIST_MAX_ELEMS * NAME_BUFFER_LENGTH"

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

	const_def
	const SCENE_TITLE_SCREEN                  ; $00
	const SCENE_COLOSSEUM_BOOSTER             ; $01
	const SCENE_EVOLUTION_BOOSTER             ; $02
	const SCENE_MYSTERY_BOOSTER               ; $03
	const SCENE_LABORATORY_BOOSTER            ; $04
	const SCENE_CHARIZARD_INTRO               ; $05
	const SCENE_SCYTHER_INTRO                 ; $06
	const SCENE_AERODACTYL_INTRO              ; $07
	const SCENE_GRADIENT_BLACK_AND_RED        ; $08
	const SCENE_GRADIENT_WHITE_AND_RED        ; $09
	const SCENE_GRADIENT_BLACK_AND_GREEN      ; $0a
	const SCENE_GRADIENT_WHITE_AND_GREEN      ; $0b
	const SCENE_COLOR_WHEEL                   ; $0c
	const SCENE_COLOR_TEST                    ; $0d
	const SCENE_GAMEBOY_LINK_CONNECTING       ; $0e
	const SCENE_GAMEBOY_LINK_TRANSMITTING     ; $0f
	const SCENE_GAMEBOY_LINK_NOT_CONNECTED    ; $10
	const SCENE_GAMEBOY_PRINTER_TRANSMITTING  ; $11
	const SCENE_GAMEBOY_PRINTER_NOT_CONNECTED ; $12
	const SCENE_CARD_POP                      ; $13
	const SCENE_CARD_POP_ERROR                ; $14
	const SCENE_JAPANESE_TITLE_SCREEN         ; $15
	const SCENE_NINTENDO                      ; $16
	const SCENE_COMPANIES                     ; $17
	const SCENE_JAPANESE_TITLE_SCREEN_2       ; $18
	const SCENE_COPYRIGHT                     ; $19
	const SCENE_JAPANESE_TITLE_SCREEN_2_COPY  ; $1a
	const SCENE_COLOR_PALETTE                 ; $1b

DEF NUM_SCENES EQU const_value

	const_def
	const MUSIC_STOP           ; $00
	const MUSIC_TITLESCREEN    ; $01
	const MUSIC_DUEL_THEME_1   ; $02
	const MUSIC_DUEL_THEME_2   ; $03
	const MUSIC_DUEL_THEME_3   ; $04
	const MUSIC_PAUSE_MENU     ; $05
	const MUSIC_PC_MAIN_MENU   ; $06
	const MUSIC_DECK_MACHINE   ; $07
	const MUSIC_CARD_POP       ; $08
	const MUSIC_OVERWORLD      ; $09
	const MUSIC_POKEMON_DOME   ; $0a
	const MUSIC_CHALLENGE_HALL ; $0b
	const MUSIC_CLUB_1         ; $0c
	const MUSIC_CLUB_2         ; $0d
	const MUSIC_CLUB_3         ; $0e
	const MUSIC_RONALD         ; $0f
	const MUSIC_IMAKUNI        ; $10
	const MUSIC_HALL_OF_HONOR  ; $11
	const MUSIC_CREDITS        ; $12
	const MUSIC_UNUSED_13      ; $13
	const MUSIC_UNUSED_14      ; $14
	const MUSIC_MATCH_START_1  ; $15
	const MUSIC_MATCH_START_2  ; $16
	const MUSIC_MATCH_START_3  ; $17
	const MUSIC_MATCH_VICTORY  ; $18
	const MUSIC_MATCH_LOSS     ; $19
	const MUSIC_MATCH_DRAW     ; $1a
	const MUSIC_UNUSED_1B      ; $1b
	const MUSIC_BOOSTER_PACK   ; $1c
	const MUSIC_MEDAL          ; $1d
	const MUSIC_UNUSED_1E      ; $1e

DEF NUM_SONGS EQU const_value


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

; buttons
DEF A_BUTTON_F EQU 0
DEF B_BUTTON_F EQU 1
DEF SELECT_F   EQU 2
DEF START_F    EQU 3
DEF D_RIGHT_F  EQU 4
DEF D_LEFT_F   EQU 5
DEF D_UP_F     EQU 6
DEF D_DOWN_F   EQU 7

DEF A_BUTTON   EQU 1 << A_BUTTON_F ; $01
DEF B_BUTTON   EQU 1 << B_BUTTON_F ; $02
DEF SELECT     EQU 1 << SELECT_F   ; $04
DEF START      EQU 1 << START_F    ; $08
DEF D_RIGHT    EQU 1 << D_RIGHT_F  ; $10
DEF D_LEFT     EQU 1 << D_LEFT_F   ; $20
DEF D_UP       EQU 1 << D_UP_F     ; $40
DEF D_DOWN     EQU 1 << D_DOWN_F   ; $80

DEF BUTTONS    EQU A_BUTTON | B_BUTTON | SELECT | START  ; $0f
DEF D_PAD      EQU D_RIGHT  | D_LEFT   | D_UP   | D_DOWN ; $f0

ENDC ; CONSTANTS_INC
