include "include/hardware.inc"
include "include/overlay.inc"
include "include/macros.inc"
include "include/constants.inc"
include "include/charmap_w.inc"
include "include/charmap.inc"

; hardware.inc包含各种硬件和寄存器的定义，建议使用里面的方式标注寄存器或者硬件参数
; overlay.inc包含org（代码定位）的用法和cleartill的用法
; charmap_w.inc定义了一种实现中文编码的方法 (见https://github.com/SnDream/charmap_w.inc)
; macros.inc建议定义一些自己编写的宏或者其他的内容
; constants.inc建议定义一些代码的常数

; 可以往后继续编写补丁代码

; MBC Type in ROM
	org $0147
	; db CART_ROM_MBC3_RAM_BAT

;        | 0x0 | 0xA | 0xE |
;--------+-----+-----+-----+
; HUC1   | RW  | RW  | IR  |
; MBCx   | XX  | RW  | XX  |
; BROKEN | RW  | RW  | RW  |
; BGB?   | RO? | RW  | RO? |

	org $0070
CartridgeDetect::
	push hl
	ld hl, wCartridgeDetect
	ld a, CART_IR_ENABLE
	ld [rRAMG], a
	call SRAMWriteTest
	jr z, .broken
	xor a
	ld [rRAMG], a
	call SRAMWriteTest
	jr z, .huc
.broken
.mbc
	ld a, CART_SRAM_ENABLE
	ldh [hSRAMEnValue], a
.huc
	ld a, CART_SRAM_ENABLE ; 对所有情况适用
	ld [rRAMG], a
	xor a
	ld [hl], a
	ld [rRAMG], a
	pop hl
	
	; xor a
	jp oSetSRAMBank

SRAMWriteTest::
	ld a, CART_DETECT_0
	ld [hl], a
	cp a, [hl]
	ret nz
	ld a, CART_DETECT_1
	ld [hl], a
	cp a, [hl]
	ret

	org $0162
InitSetSRAMBank_Patch::
	call CartridgeDetect
	nop

	org $077b
oSetSRAMBank::
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
	jr oDisableSRAM.entry ; 实际上做En，见下面说明

	org $0787
EnableSRAM::
oEnableSRAM::
; 	push af
; .entry
; 	ld a, CART_SRAM_ENABLE
; .entry2
; 	ld [rRAMG], a
; 	pop af
; 	ret

	org $078f
; 对于HUC1，保持原有的写0的方式（理论上没用）
; 对于MBCx，关闭也是打开，否则运行异常
; 因此还是取hSRAMEnValue
DisableSRAM::
oDisableSRAM::
	; push af
	; farcall oDisableSRAMEmpty
	; xor a
	; ld [rRAMG], a
	; pop af
	; ret
	push af
.entry
	ldh a, [hSRAMEnValue]
	; xor a
	ld [rRAMG], a
	pop af
	ret

IF 0
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
; 	push af
; 	ld a, CART_IR_ENABLE
; .entry
; 	ld [rRAMG], a
; 	pop af
; 	ret

	push af
	ld a, CART_SRAM_DISABLE
.entry
	ld [rRAMG], a
	pop af
	ret
ENDC

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
	; jp Reset
IF 0
	ld a, CART_SRAM_ENABLE
	ld [rRAMG], a
	xor a
	ldh [ohSRAMBank], a
	ld [rRAMB], a
	ld [_SRAM], a
	nop
	nop
	nop
	jp Reset
ENDC

; 	org $5a3a, 6
; oClearRPAndRestoreVBlankFunction::
; 	jp nClearRPAndRestoreVBlankFunction

; copy from pret/poketcg

	org $77c0, 6

; if carry flag is set, only delays
; if carry not set:
; - set rRP to $c1, wait;
; - set rRP to $c0, wait;
; - return
Func_19674:
	jr c, .delay_once
	ld [hl], $c1
	ld a, 5
	jr .loop_delay_1 ; jump to possibly to add more cycles?
.loop_delay_1
	dec a
	jr nz, .loop_delay_1
	ld [hl], $c0
	ld a, 14
	jr .loop_delay_2 ; jump to possibly to add more cycles?
.loop_delay_2
	dec a
	jr nz, .loop_delay_2
	ret

.delay_once
	ld a, 21
	jr .loop_delay_3 ; jump to possibly to add more cycles?
.loop_delay_3
	dec a
	jr nz, .loop_delay_3
	nop
	ret

; input a = byte to transmit through IR
TransmitByteThroughIR:
	push hl
	ld hl, rRP
	push de
	push bc
	ld b, a
	scf  ; carry set
	call Func_19674
	or a ; carry not set
	call Func_19674
	ld c, 8
	ld c, 8 ; number of input bits
.loop
	ld a, $00
	rr b
	call Func_19674
	dec c
	jr nz, .loop
	pop bc
	pop de
	pop hl
	ldh a, [rJOYP]
	bit 1, a ; P11
	jr z, ReturnZFlagUnsetAndCarryFlagSet
	xor a ; return z set
	ret

; same as ReceiveByteThroughIR but
; returns $0 in a if there's an error in IR
ReceiveByteThroughIR_ZeroIfUnsuccessful:
	call ReceiveByteThroughIR
	ret nc
	xor a
	ret

; returns carry if there's some time out
; and output in register a of $ff
; otherwise returns in a some sequence of bits
; related to how rRP sets/unsets bit 1
ReceiveByteThroughIR:
	push de
	push bc
	push hl

; waits for bit 1 in rRP to be unset
; up to $100 loops
	ld b, 0
	ld hl, rRP
.wait_ir
	bit 1, [hl]
	jr z, .ok
	dec b
	jr nz, .wait_ir
	; looped around $100 times
	; return $ff and carry set
	pop hl
	pop bc
	pop de
	scf
	ld a, $ff
	ret

.ok
; delay for some cycles
	ld a, 15
.loop_delay
	dec a
	jr nz, .loop_delay

; loop for each bit
	ld e, 8
.loop
	ld a, $01
	; possibly delay cycles?
	ld b, 9
	ld b, 9
	ld b, 9
	ld b, 9

; checks for bit 1 in rRP
; if in any of the checks it is unset,
; then a is set to 0
; this is done a total of 9 times
	bit 1, [hl]
	jr nz, .asm_196ec
	xor a
.asm_196ec
	bit 1, [hl]
	jr nz, .asm_196f1
	xor a
.asm_196f1
	dec b
	jr nz, .asm_196ec
	; one bit received
	rrca
	rr d
	dec e
	jr nz, .loop
	ld a, d ; has bits set for each "cycle" that bit 1 was not unset
	pop hl
	pop bc
	pop de
	or a
	ret

ReturnZFlagUnsetAndCarryFlagSet:
	ld a, $ff
	or a ; z not set
	scf  ; carry set
	ret

; called when expecting to transmit data
Func_19705:
	ld hl, rRP
.asm_19708
	ldh a, [rJOYP]
	bit 1, a
	jr z, ReturnZFlagUnsetAndCarryFlagSet
	ld a, $aa ; request
	call TransmitByteThroughIR
	push hl
	pop hl
	call ReceiveByteThroughIR_ZeroIfUnsuccessful
	cp $33 ; acknowledge
	jr nz, .asm_19708
	xor a
	ret

; called when expecting to receive data
Func_1971e:
	ld hl, rRP
.asm_19721
	ldh a, [rJOYP]
	bit 1, a
	jr z, ReturnZFlagUnsetAndCarryFlagSet
	call ReceiveByteThroughIR_ZeroIfUnsuccessful
	cp $aa ; request
	jr nz, .asm_19721
	ld a, $33 ; acknowledge
	call TransmitByteThroughIR
	xor a
	ret

ReturnZFlagUnsetAndCarryFlagSet2:
	jp ReturnZFlagUnsetAndCarryFlagSet

TransmitIRDataBuffer:
	call Func_19705
	jr c, ReturnZFlagUnsetAndCarryFlagSet2
	ld a, $49
	call TransmitByteThroughIR
	ld a, $52
	call TransmitByteThroughIR
	ld hl, wIRDataBuffer
	ld c, 8
	jr TransmitNBytesFromHLThroughIR

ReceiveIRDataBuffer:
	call Func_1971e
	jr c, ReturnZFlagUnsetAndCarryFlagSet2
	call ReceiveByteThroughIR
	cp $49
	jr nz, ReceiveIRDataBuffer
	call ReceiveByteThroughIR
	cp $52
	jr nz, ReceiveIRDataBuffer
	ld hl, wIRDataBuffer
	ld c, 8
	jr ReceiveNBytesToHLThroughIR

; hl = start of data to transmit
; c = number of bytes to transmit
TransmitNBytesFromHLThroughIR:
	ld b, $0
.loop_data_bytes
	ld a, b
	add [hl]
	ld b, a
	ld a, [hli]
	call TransmitByteThroughIR
	jr c, .asm_1977c
	dec c
	jr nz, .loop_data_bytes
	ld a, b
	cpl
	inc a
	call TransmitByteThroughIR
.asm_1977c
	ret

; hl = address to write received data
; c = number of bytes to be received
ReceiveNBytesToHLThroughIR:
	ld b, 0
.loop_data_bytes
	call ReceiveByteThroughIR
	jr c, ReturnZFlagUnsetAndCarryFlagSet2
	ld [hli], a
	add b
	ld b, a
	dec c
	jr nz, .loop_data_bytes
	call ReceiveByteThroughIR
	add b
	or a
	jr nz, ReturnZFlagUnsetAndCarryFlagSet2
	ret

; disables interrupts, and sets joypad and IR communication port
; switches to CGB normal speed
StartIRCommunications:
	di
	; call SwitchToCGBNormalSpeed
	ld a, P14
	ldh [rJOYP], a
	ld a, $c0
	ldh [rRP], a
	ret

; reenables interrupts, and switches CGB back to double speed
CloseIRCommunications:
	ld a, P14 | P15
	ldh [rJOYP], a
.wait_vblank_on
	ldh a, [rSTAT]
	and STAT_LCDC_STATUS
	cp STAT_ON_VBLANK
	jr z, .wait_vblank_on
.wait_vblank_off
	ldh a, [rSTAT]
	and STAT_LCDC_STATUS
	cp STAT_ON_VBLANK
	jr nz, .wait_vblank_off
	; call SwitchToCGBDoubleSpeed
	ei
	ret

; set rRP to 0
ClearRP:
	ld a, $00
	ldh [rRP], a
	ret

; expects to receive a command (IRCMD_* constant)
; in wIRDataBuffer + 1, then calls the subroutine
; corresponding to that command
ExecuteReceivedIRCommands:
	call StartIRCommunications
.loop_commands
	call ReceiveIRDataBuffer
	jr c, .error
	jr nz, .loop_commands
	ld hl, wIRDataBuffer + 1
	ld a, [hl]
	ld hl, .CmdPointerTable
	cp NUM_IR_COMMANDS
	jr nc, .loop_commands ; invalid command
	call .JumpToCmdPointer ; execute command
	jr .loop_commands
.error
	call CloseIRCommunications
	xor a
	scf
	ret

.JumpToCmdPointer
	add a ; *2
	add l
	ld l, a
	ld a, 0
	adc h
	ld h, a
	ld a, [hli]
	ld h, [hl]
	ld l, a
.jp_hl
	jp hl

.CmdPointerTable
	dw .Close                ; IRCMD_CLOSE
	dw .ReturnWithoutClosing ; IRCMD_RETURN_WO_CLOSING
	dw .TransmitData         ; IRCMD_TRANSMIT_DATA
	dw .ReceiveData          ; IRCMD_RECEIVE_DATA
	dw .CallFunction         ; IRCMD_CALL_FUNCTION

; closes the IR communications
; pops hl so that the sp points
; to the return address of ExecuteReceivedIRCommands
.Close
	pop hl
	call CloseIRCommunications
	or a
	ret

; returns without closing the IR communications
; will continue the command loop
.ReturnWithoutClosing
	or a
	ret

; receives an address and number of bytes
; and transmits starting at that address
.TransmitData
	call Func_19705
	ret c
	call LoadRegistersFromIRDataBuffer
	jp TransmitNBytesFromHLThroughIR

; receives an address and number of bytes
; and writes the data received to that address
.ReceiveData
	call LoadRegistersFromIRDataBuffer
	ld l, e
	ld h, d
	call ReceiveNBytesToHLThroughIR
	jr c, .asm_19812
	sub b
	call TransmitByteThroughIR
.asm_19812
	ret

; receives an address to call, then stores
; the registers in the IR data buffer
.CallFunction
	call LoadRegistersFromIRDataBuffer
	call .jp_hl
	call StoreRegistersInIRDataBuffer
	ret

; returns carry set if request sent was not acknowledged
TrySendIRRequest:
	call StartIRCommunications
	ld hl, rRP
	ld c, 4
.send_request
	ld a, $aa ; request
	push bc
	call TransmitByteThroughIR
	push bc
	pop bc
	call ReceiveByteThroughIR_ZeroIfUnsuccessful
	pop bc
	cp $33 ; acknowledgement
	jr z, .received_ack
	dec c
	jr nz, .send_request
	scf
	jr .close

.received_ack
	xor a
.close
	push af
	call CloseIRCommunications
	pop af
	ret

; returns carry set if request was not received
TryReceiveIRRequest:
	call StartIRCommunications
	ld hl, rRP
.wait_request
	call ReceiveByteThroughIR_ZeroIfUnsuccessful
	cp $aa ; request
	jr z, .send_ack
	ldh a, [rJOYP]
	cpl
	and P10 | P11
	jr z, .wait_request
	scf
	jr .close

.send_ack
	ld a, $33 ; acknowledgement
	call TransmitByteThroughIR
	xor a
.close
	push af
	call CloseIRCommunications
	pop af
	ret

; sends request for other device to close current communication
RequestCloseIRCommunication:
	call StartIRCommunications
	ld a, IRCMD_CLOSE
	ld [wIRDataBuffer + 1], a
	call TransmitIRDataBuffer
;	fallthrough

; calls CloseIRCommunications while preserving af
SafelyCloseIRCommunications:
	push af
	call CloseIRCommunications
	pop af
	ret

; sends a request for data to be transmitted
; from the other device
; hl = start of data to request to transmit
; de = address to write data received
; c = length of data
RequestDataTransmissionThroughIR:
	ld a, IRCMD_TRANSMIT_DATA
	call TransmitRegistersThroughIR
	push de
	push bc
	call Func_1971e
	pop bc
	pop hl
	jr c, SafelyCloseIRCommunications
	call ReceiveNBytesToHLThroughIR
	jr SafelyCloseIRCommunications

; transmits data to be written in the other device
; hl = start of data to transmit
; de = address for other device to write data
; c = length of data
RequestDataReceivalThroughIR:
	ld a, IRCMD_RECEIVE_DATA
	call TransmitRegistersThroughIR
	call TransmitNBytesFromHLThroughIR
	jr c, SafelyCloseIRCommunications
	call ReceiveByteThroughIR
	jr c, SafelyCloseIRCommunications
	add b
	jr nz, .asm_1989e
	xor a
	jr SafelyCloseIRCommunications
.asm_1989e
	call ReturnZFlagUnsetAndCarryFlagSet
	jr SafelyCloseIRCommunications

; first stores all the current registers in wIRDataBuffer
; then transmits it through IR
TransmitRegistersThroughIR:
	push hl
	push de
	push bc
	call StoreRegistersInIRDataBuffer
	call StartIRCommunications
	call TransmitIRDataBuffer
	pop bc
	pop de
	pop hl
	ret nc
	inc sp
	inc sp
	jr SafelyCloseIRCommunications

; ir_functions

; ; hl = text ID
; LoadLinkConnectingScene:
; 	push hl
; 	call SetSpriteAnimationsAsVBlankFunction
; 	ld a, SCENE_GAMEBOY_LINK_CONNECTING
; 	lb bc, 0, 0
; 	call LoadScene
; 	pop hl
; 	call DrawWideTextBox_PrintText
; 	call EnableLCD
; 	ret

; shows Link Not Connected scene
; then asks the player whether they want to try again
; if the player selects "no", return carry
; input:
;  - hl = text ID
LoadLinkNotConnectedSceneAndAskWhetherToTryAgain:
	push hl
	call RestoreVBlankFunction
	call SetSpriteAnimationsAsVBlankFunction
	ld a, SCENE_GAMEBOY_LINK_NOT_CONNECTED
	lb bc, 0, 0
	call LoadScene
	pop hl
	call DrawWideTextBox_WaitForInput
	ld hl, $0197 ; ldtx hl, WouldYouLikeToTryAgainText
	call YesOrNoMenuWithText_SetCursorToYes
;	fallthrough

ClearRPAndRestoreVBlankFunction:
	push af
	call ClearRP
	call RestoreVBlankFunction
	pop af
	ret

; prepares IR communication parameter data
; a = a IRPARAM_* constant for the function of this connection
InitIRCommunications:
	ld hl, wOwnIRCommunicationParams
	ld [hl], a
	inc hl
	ld [hl], $50
	inc hl
	ld [hl], $4b
	inc hl
	ld [hl], $31
	ld a, $ff
	ld [wIRCommunicationErrorCode], a
	ld a, PLAYER_TURN
	ldh [hWhoseTurn], a
; clear wNameBuffer and wOpponentName
	xor a
	ld [wNameBuffer], a
	ld hl, wOpponentName
	ld [hli], a
	ld [hl], a
; loads player's name from SRAM
; to wDefaultText
	call oEnableSRAM
	ld hl, sPlayerName
	ld de, wDefaultText
	ld c, NAME_BUFFER_LENGTH
.loop
	ld a, [hli]
	ld [de], a
	inc de
	dec c
	jr nz, .loop
	call oDisableSRAM
	ret

; returns carry if communication was unsuccessful
; if a = 0, then it was a communication error
; if a = 1, then operation was cancelled by the player
PrepareSendCardOrDeckConfigurationThroughIR:
	call InitIRCommunications

; pressing A button triggers request for IR communication
.loop_frame
	call DoFrame
	ldh a, [hKeysPressed]
	bit B_BUTTON_F, a
	jr nz, .b_btn
	ldh a, [hKeysHeld]
	bit A_BUTTON_F, a
	jr z, .loop_frame
; a btn
	call TrySendIRRequest
	jr nc, .request_success
	or a
	jr z, .loop_frame
	xor a
	scf
	ret

.b_btn
	; cancelled by the player
	ld a, $01
	scf
	ret

.request_success
	call ExchangeIRCommunicationParameters
	ret c
	ld a, [wOtherIRCommunicationParams + 3]
	cp $31
	jr nz, SetIRCommunicationErrorCode_Error
	or a
	ret

; exchanges player names and IR communication parameters
; checks whether parameters for communication match
; and if they don't, an error is issued
ExchangeIRCommunicationParameters:
	ld hl, wOwnIRCommunicationParams
	ld de, wOtherIRCommunicationParams
	ld c, 4
	call RequestDataTransmissionThroughIR
	jr c, .error
	ld hl, wOtherIRCommunicationParams + 1
	ld a, [hli]
	cp $50
	jr nz, .error
	ld a, [hli]
	cp $4b
	jr nz, .error
	ld a, [wOwnIRCommunicationParams]
	ld hl, wOtherIRCommunicationParams
	cp [hl] ; do parameters match?
	jr nz, SetIRCommunicationErrorCode_Error

; receives wDefaultText from other device
; and writes it to wNameBuffer
	ld hl, wDefaultText
	ld de, wNameBuffer
	ld c, NAME_BUFFER_LENGTH
	call RequestDataTransmissionThroughIR
	jr c, .error
; transmits wDefaultText to be
; written in wNameBuffer in the other device
	ld hl, wDefaultText
	ld de, wNameBuffer
	ld c, NAME_BUFFER_LENGTH
	call RequestDataReceivalThroughIR
	jr c, .error
	or a
	ret

.error
	xor a
	scf
	ret

SetIRCommunicationErrorCode_Error:
	ld hl, wIRCommunicationErrorCode
	ld [hl], $01
	ld de, wIRCommunicationErrorCode
	ld c, 1
	call RequestDataReceivalThroughIR
	call RequestCloseIRCommunication
	ld a, $01
	scf
	ret

SetIRCommunicationErrorCode_NoError:
	ld hl, wOwnIRCommunicationParams
	ld [hl], $00
	ld de, wIRCommunicationErrorCode
	ld c, 1
	call RequestDataReceivalThroughIR
	ret c
	call RequestCloseIRCommunication
	or a
	ret

; makes device receptive to receive data from other device
; to write in wDuelTempList (either list of cards or a deck configuration)
; returns carry if some error occurred
TryReceiveCardOrDeckConfigurationThroughIR:
	call InitIRCommunications
.loop_receive_request
	xor a
	ld [wDuelTempList], a
	call TryReceiveIRRequest
	jr nc, .receive_data
	bit 1, a
	jr nz, .cancelled
	jr .loop_receive_request
.receive_data
	call ExecuteReceivedIRCommands
	ld a, [wIRCommunicationErrorCode]
	or a
	ret z ; no error
	xor a
	scf
	ret

.cancelled
	ld a, $01
	scf
	ret

; returns carry if card(s) wasn't successfully sent
_SendCard:
	call StopMusic
	ld hl, $019a ; ldtx hl, SendingACardText
	call LoadLinkConnectingScene
	ld a, IRPARAM_SEND_CARDS
	call PrepareSendCardOrDeckConfigurationThroughIR
	jr c, .fail

	; send cards
	xor a
	ld [wDuelTempList + DECK_SIZE], a
	ld hl, wDuelTempList
	ld e, l
	ld d, h
	ld c, DECK_SIZE + 1
	call RequestDataReceivalThroughIR
	jr c, .fail
	call SetIRCommunicationErrorCode_NoError
	jr c, .fail
	call ExecuteReceivedIRCommands
	jr c, .fail
	ld a, [wOwnIRCommunicationParams + 1]
	cp $4f
	jr nz, .fail
	call PlayCardPopSong
	xor a
	call ClearRPAndRestoreVBlankFunction
	ret

.fail
	call PlayCardPopSong
	ld hl, $019e ; ldtx hl, CardTransferWasntSuccessful1Text
	call LoadLinkNotConnectedSceneAndAskWhetherToTryAgain
	jr nc, _SendCard ; loop back and try again
	; failed
	scf
	ret

PlayCardPopSong:
	ld a, MUSIC_CARD_POP
	jp PlaySong

_ReceiveCard:
	call StopMusic
	ld hl, $019b ; ldtx hl, ReceivingACardText
	call LoadLinkConnectingScene
	ld a, IRPARAM_SEND_CARDS
	call TryReceiveCardOrDeckConfigurationThroughIR
	ld a, $4f
	ld [wOwnIRCommunicationParams + 1], a
	ld hl, wOwnIRCommunicationParams
	ld e, l
	ld d, h
	ld c, 4
	call RequestDataReceivalThroughIR
	jr c, .fail
	call RequestCloseIRCommunication
	jr c, .fail
	call PlayCardPopSong
	or a
	call ClearRPAndRestoreVBlankFunction
	ret

.fail
	call PlayCardPopSong
	ld hl, $019f ; ldtx hl, CardTransferWasntSuccessful2Text
	call LoadLinkNotConnectedSceneAndAskWhetherToTryAgain
	jr nc, _ReceiveCard
	scf
	ret

_SendDeckConfiguration:
	call StopMusic
	ld hl, $019c; ldtx hl, SendingADeckConfigurationText
	call LoadLinkConnectingScene
	ld a, IRPARAM_SEND_DECK
	call PrepareSendCardOrDeckConfigurationThroughIR
	jr c, .fail
	ld hl, wDuelTempList
	ld e, l
	ld d, h
	ld c, DECK_STRUCT_SIZE
	call RequestDataReceivalThroughIR
	jr c, .fail
	call SetIRCommunicationErrorCode_NoError
	jr c, .fail
	call PlayCardPopSong
	call ClearRPAndRestoreVBlankFunction
	or a
	ret

.fail
	call PlayCardPopSong
	ld hl, $01a0 ; ldtx hl, DeckConfigurationTransferWasntSuccessful1Text
	call LoadLinkNotConnectedSceneAndAskWhetherToTryAgain
	jr nc, _SendDeckConfiguration
	scf
	ret

_ReceiveDeckConfiguration:
	call StopMusic
	ld hl, $019d ; ldtx hl, ReceivingDeckConfigurationText
	call LoadLinkConnectingScene
	ld a, IRPARAM_SEND_DECK
	call TryReceiveCardOrDeckConfigurationThroughIR
	jr c, .fail
	call PlayCardPopSong
	call ClearRPAndRestoreVBlankFunction
	or a
	ret

.fail
	call PlayCardPopSong
	ld hl, $01a1 ; ldtx hl, DeckConfigurationTransferWasntSuccessful2Text
	call LoadLinkNotConnectedSceneAndAskWhetherToTryAgain
	jr nc, _ReceiveDeckConfiguration ; loop back and try again
	scf
	ret

_DoCardPop:
; loads scene for Card Pop! screen
; then checks if console is SGB
; and issues an error message in case it is
	call SetSpriteAnimationsAsVBlankFunction
	ld a, SCENE_CARD_POP
	lb bc, 0, 0
	call LoadScene
	ld hl, $018a ; ldtx hl, AreYouBothReadyToCardPopText
	call PrintScrollableText_NoTextBoxLabel
	call RestoreVBlankFunction
	ld hl, $00dd ; ldtx hl, CardPopCannotBePlayedWithTheGameBoyText
	ld a, [wConsole]
	cp CONSOLE_SGB
	jr z, .error

; initiate the communications
	call PauseSong
	call SetSpriteAnimationsAsVBlankFunction
	ld a, SCENE_GAMEBOY_LINK_CONNECTING
	lb bc, 0, 0
	call LoadScene
	ld hl, $018d ; ldtx hl, PositionGameBoyColorsAndPressAButtonText
	call DrawWideTextBox_PrintText
	call EnableLCD
	call HandleCardPopCommunications
	push af
	push hl
	call ClearRP
	call RestoreVBlankFunction
	pop hl
	pop af
	jr c, .error

; show the received card detail page
; and play the corresponding song
	ld a, [wLoadedCard1ID]
	call AddCardToCollectionAndUpdateAlbumProgress
	ld hl, wLoadedCard1Name
	ld a, [hli]
	ld h, [hl]
	ld l, a
	call LoadTxRam2
	ld a, PLAYER_TURN
	ldh [hWhoseTurn], a
	ld a, $5d ; SFX_5D
	call PlaySFX
.wait_sfx
	call AssertSFXFinished
	or a
	jr nz, .wait_sfx
	ld a, [wCardPopCardObtainSong]
	call PlaySong
	ld hl, $018e ; ldtx hl, ReceivedThroughCardPopText
	bank1call _DisplayCardDetailScreen
	call ResumeSong
	lb de, $38, $9f
	call SetupText
	bank1call OpenCardPage_FromHand
	ret

.error
; show Card Pop! error scene
; and print text in hl
	push hl
	call ResumeSong
	call SetSpriteAnimationsAsVBlankFunction
	ld a, SCENE_CARD_POP_ERROR
	lb bc, 0, 0
	call LoadScene
	pop hl
	call PrintScrollableText_NoTextBoxLabel
	call RestoreVBlankFunction
	ret

; handles all communications to the other device to do Card Pop!
; returns carry if Card Pop! is unsuccessful
; and returns in hl the corresponding error text ID
HandleCardPopCommunications:
; copy CardPopNameList from SRAM to WRAM
	call EnableSRAM
	ld hl, sCardPopNameList
	ld de, wCardPopNameList
	ld bc, CARDPOP_NAME_LIST_SIZE
	call CopyDataHLtoDE
	call DisableSRAM

	ld a, IRPARAM_CARD_POP
	call InitIRCommunications
.loop_request
	call TryReceiveIRRequest ; receive request
	jr nc, .execute_commands
	bit 1, a
	jr nz, .fail
	call TrySendIRRequest ; send request
	jr c, .loop_request

; do the player name search, then transmit the result
	call ExchangeIRCommunicationParameters
	jr c, .fail
	ld hl, wCardPopNameList
	ld de, wOtherPlayerCardPopNameList
	ld c, 0 ; $100 bytes = CARDPOP_NAME_LIST_SIZE
	call RequestDataTransmissionThroughIR
	jr c, .fail
	call LookUpNameInCardPopNameList
	ld hl, wCardPopNameSearchResult
	ld de, wCardPopNameSearchResult
	ld c, 1
	call RequestDataReceivalThroughIR
	jr c, .fail
	call SetIRCommunicationErrorCode_NoError
	jr c, .fail
	call ExecuteReceivedIRCommands
	jr c, .fail
	jr .check_search_result

.execute_commands
; will receive commands to send card pop name list,
; and to receive the result of the name list search
	call ExecuteReceivedIRCommands
	ld a, [wIRCommunicationErrorCode]
	or a
	jr nz, .fail
	call RequestCloseIRCommunication
	jr c, .fail

.check_search_result
	ld a, [wCardPopNameSearchResult]
	or a
	jr z, .success
	; not $00, means the name was found in the list
	ld hl, $018c ; ldtx hl, CannotCardPopWithFriendPreviouslyPoppedWithText
	scf
	ret

.success
	call DecideCardToReceiveFromCardPop

; increment number of times Card Pop! was done
; and write the other player's name to sCardPopNameList
; the spot where this is written in the list is derived
; from the lower nybble of sTotalCardPopsDone
; that means that after 16 Card Pop!, the older
; names start to get overwritten
	call EnableSRAM
	ld hl, sTotalCardPopsDone
	ld a, [hl]
	inc [hl]
	and $0f
	swap a ; *NAME_BUFFER_LENGTH
	ld l, a
	ld h, $0
	ld de, sCardPopNameList
	add hl, de
	ld de, wNameBuffer
	ld c, NAME_BUFFER_LENGTH
.loop_write_name
	ld a, [de]
	inc de
	ld [hli], a
	dec c
	jr nz, .loop_write_name
	call DisableSRAM
	or a
	ret

.fail
	ld hl, $018b ; ldtx hl, ThePopWasntSuccessfulText
	scf
	ret

; looks up the name in wNameBuffer in wCardPopNameList
; used to know whether this save file has done Card Pop!
; with the other player already
; returns carry and wCardPopNameSearchResult = $ff if the name was found;
; returns no carry and wCardPopNameSearchResult = $00 otherwise
LookUpNameInCardPopNameList:
; searches for other player's name in this game's name list
	ld hl, wCardPopNameList
	ld c, CARDPOP_NAME_LIST_MAX_ELEMS
.loop_own_card_pop_name_list
	push hl
	ld de, wNameBuffer
	call oLookUpNameInCardPopNameList.CompareNames
	pop hl
	jr nc, .found_name
	ld de, NAME_BUFFER_LENGTH
	add hl, de
	dec c
	jr nz, .loop_own_card_pop_name_list

; name was not found in wCardPopNameList

; searches for this player's name in the other game's name list
; this is useless since it discards the result from the name comparisons
; as a result this loop will always return no carry
	call EnableSRAM
	ld hl, wOtherPlayerCardPopNameList
	ld c, CARDPOP_NAME_LIST_MAX_ELEMS
.loop_other_card_pop_name_list
	push hl
	ld de, sPlayerName
	call oLookUpNameInCardPopNameList.CompareNames ; discards result from comparison
	pop hl
	ld de, NAME_BUFFER_LENGTH
	add hl, de
	dec c
	jr nz, .loop_other_card_pop_name_list
	xor a
	jr .no_carry

.found_name
	ld a, $ff
	scf
.no_carry
	call DisableSRAM
	ld [wCardPopNameSearchResult], a ; $00 if name was not found, $ff otherwise
	ret


IRHwisMissing::
	push hl
	call StopMusic
	call SetSpriteAnimationsAsVBlankFunction
	ld a, SCENE_GAMEBOY_LINK_CONNECTING
	lb bc, 0, 0
	call LoadScene
	call EnableLCD

	call PlayCardPopSong
	call RestoreVBlankFunction
	call SetSpriteAnimationsAsVBlankFunction
	ld a, SCENE_GAMEBOY_LINK_NOT_CONNECTED
	lb bc, 0, 0
	call LoadScene
	pop hl
	call DrawWideTextBox_WaitForInput
	call RestoreVBlankFunction
	scf
	ret

MACRO IRHwSelect
	ldh a, [hSRAMEnValue]
	and a
	jp z, o\1
	ld a, [wConsole]
	cp a, CONSOLE_CGB
	jp z, \1
ENDM

n_SendCard::
	IRHwSelect _SendCard
	call Prepare_IRHwisMissingText ; ld hl, $019e
	jp IRHwisMissing

n_ReceiveCard::
	IRHwSelect _ReceiveCard
	call Prepare_IRHwisMissingText ; ld hl, $019f
	jp IRHwisMissing

n_SendDeckConfiguration::
	IRHwSelect _SendDeckConfiguration
	call Prepare_IRHwisMissingText ; ld hl, $01a0
	jp IRHwisMissing

n_ReceiveDeckConfiguration::
	IRHwSelect _ReceiveDeckConfiguration
	call Prepare_IRHwisMissingText ; ld hl, $01a1
	jp IRHwisMissing

n_DoCardPop::
	IRHwSelect _DoCardPop
	call SetSpriteAnimationsAsVBlankFunction
	ld a, SCENE_CARD_POP
	lb bc, 0, 0
	call LoadScene
	ld hl, $018a ; ldtx hl, AreYouBothReadyToCardPopText
	call PrintScrollableText_NoTextBoxLabel
	call RestoreVBlankFunction
	call ResumeSong
	call SetSpriteAnimationsAsVBlankFunction
	ld a, SCENE_CARD_POP_ERROR
	lb bc, 0, 0
	call LoadScene
	call Prepare_IRHwisMissingText ; ld hl, $018d
	; call PrintScrollableText_NoTextBoxLabel
	call DrawWideTextBox_WaitForInput
	call RestoreVBlankFunction
	ret

Prepare_IRHwisMissingText:
	push de
	push bc
	ld hl, IRHwisMissingText
	ld de, wDefaultText
	ld c, IRHwisMissingText.end - IRHwisMissingText
.loop
	ld a, [hli]
	ld [de], a
	inc de
	dec c
	jr nz, .loop
	pop bc
	pop de
	ld hl, $0000
	ret

IRHwisMissingText:
	db_w "找不到红外硬件支持，", $0a
	db_w "无法使用该功能。"    , $00
.end

	org $7355, 1
ReceiveDeckConfiguration:
	farcall n_ReceiveDeckConfiguration
	ret

SendDeckConfiguration:
	farcall n_SendDeckConfiguration
	ret

ReceiveCard:
	farcall n_ReceiveCard
	ret

SendCard:
	farcall n_SendCard
	ret

DoCardPop:
	farcall n_DoCardPop
	ret
