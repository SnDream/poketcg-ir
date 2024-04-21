include "include/overlay.inc"

; 建议使用这个文件存放游戏原有的符号入口，以便调用使用

	org $ff81
ohSRAMBank:: db

	org $55e1, 6
oTransmitByteThroughIR:: ; static

	org $5621, 6
oReceiveByteThroughIR:: ; static

	org $5671, 6
oReturnZFlagUnsetAndCarryFlagSet:: ; static

	org $56a1, 6
oFunc_19705:: ; static 美日差异较大

	org $56ca, 6
oFunc_1971e:: ; static 美日差异较大

	org $56f3, 6
oReturnZFlagUnsetAndCarryFlagSet2:: ; static

	org $56f6, 6
oTransmitIRDataBuffer:: ; static

	org $570c, 6
oReceiveIRDataBuffer:: ; static

	org $5726, 6
oTransmitNBytesFromHLThroughIR:: ; static

	org $573b, 6
oReceiveNBytesToHLThroughIR:: ; static

	org $5750, 6
oStartIRCommunications:: ; static

	org $575d, 6
oCloseIRCommunications:: ; static

	org $5786, 6
oExecuteReceivedIRCommands:: ; extern

	org $57e2, 6
oTrySendIRRequest:: ; extern

	org $5805, 6
oTryReceiveIRRequest:: ; extern

	org $5823, 6
oRequestCloseIRCommunication:: ; extern

	org $582e, 6
oSafelyCloseIRCommunications:: ; static

	org $5834, 6
oRequestDataTransmissionThroughIR:: ; extern

	org $5847, 6
oRequestDataReceivalThroughIR:: ; extern

	org $5861, 6
oTransmitRegistersThroughIR:: ; extern

	org $5a21, 6
oLoadLinkNotConnectedSceneAndAskWhetherToTryAgain::

	org $5a3a, 6
oClearRPAndRestoreVBlankFunction::

	org $5a40, 6
oInitIRCommunications::

	org $5a6b, 6
oPrepareSendCardOrDeckConfigurationThroughIR::

	org $5a8c, 6
oExchangeIRCommunicationParameters::

	org $5ad1, 6
oSetIRCommunicationErrorCode_NoError::

	org $5ae4, 6
oTryReceiveCardOrDeckConfigurationThroughIR::

	org $5b05, 6
o_SendCard::

	org $5b44, 6
o_ReceiveCard::

	org $5b68, 6
o_SendDeckConfiguration::

	org $5b9e, 6
o_ReceiveDeckConfiguration::

	org $5bc3, 6
o_DoCardPop::

	org $5cdd, 6
oLookUpNameInCardPopNameList::

	org $5d11, 6
oLookUpNameInCardPopNameList.CompareNames::

; copy from pret/poketcg

	org $0261
EnableLCD::
	org $04fd
Reset::
	org $070e
CopyDataHLtoDE::
	org $2213
SetupText::
	org $2b8c
PrintScrollableText_NoTextBoxLabel::
	org $3686
ResumeSong::
	org $0521
DoFrame::
	org $296a
DrawWideTextBox_PrintText::
	org $29bc
DrawWideTextBox_WaitForInput::
	org $29fc
YesOrNoMenuWithText_SetCursorToYes::
	org $2da1
LoadTxRam2::
	org $366a
StopMusic::
	org $366b
PlaySong::
	org $3675
AssertSFXFinished::
	org $367c
PlaySFX::
	org $3681
PauseSong::
	org $391a
AddCardToCollectionAndUpdateAlbumProgress::
	org $3d22
LoadScene::

	org $56b8, 1
OpenCardPage_FromHand::
	org $5d19, 1
_DisplayCardDetailScreen::

	org $5875, 6
StoreRegistersInIRDataBuffer::
	org $588e, 6
LoadRegistersFromIRDataBuffer::
	org $58a5, 6
SetSpriteAnimationsAsVBlankFunction::
	org $58c5, 6
RestoreVBlankFunction::
	org $5a0d, 6
LoadLinkConnectingScene::
	org $5b3f, 6
PlayCardPopSong::



	org $5d20, 6
DecideCardToReceiveFromCardPop::
	org $5d7b, 6
CreateCardPopCandidateList::
	org $5db6, 6
CalculateNameHash::

	org $a005
sTotalCardPopsDone::

	org $a010
sPlayerName::

	org $bb00
sCardPopNameList::

	org $c100
wCardPopNameList::

	org $c200
wPlayerDuelVariables::
wOtherPlayerCardPopNameList::

	org $c8b0
wDefaultText::

	org $cb46
wOpponentName::
	org $cb4b
wNameBuffer::
	org $cb5b
wDuelTempList::
	org $cbdf
wLoadedCard1Name::
	org $cbe3
wLoadedCard1ID::

	org $ce37
wIRCommunicationErrorCode:: ds 1
	org $ce38
wOwnIRCommunicationParams:: ds $4
	org $ce3b
wOtherIRCommunicationParams::
	org $ce40
wCardPopNameSearchResult::
	org $ce46
wCardPopCardObtainSong::

	org $c8f4
wConsole:: ds $1

	org $ce22
wIRDataBuffer:: ds $8

	org $ff90
hKeysHeld:: ds 1
hKeysPressed:: ds 1
	org $ff97
hWhoseTurn:: ds $1

