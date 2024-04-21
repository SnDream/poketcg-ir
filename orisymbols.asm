include "include/overlay.inc"

; 建议使用这个文件存放游戏原有的符号入口，以便调用使用

	org $ff81
ohSRAMBank:: db

	org $c8f4
wConsole::
	ds $1

	org $ce22
wIRDataBuffer:: ds $8
