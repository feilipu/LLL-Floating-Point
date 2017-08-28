;==================================================================================
; Contents of this file are copyright Phillip Stevens
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
; Converted to z88dk z80asm YAZ180 by
; Phillip Stevens @feilipu https://feilipu.me
; August 2017
;
;
; https://github.com/feilipu/
;
; https://feilipu.me/
;

SECTION     data_align_256
ORG         $2500

;SCRPG      $2800   Scratch page is located here.
;                   Contains the operands and result area.

SECTION     code_driver
ORG         $2900

SECTION     data_driver
ORG         -1

SECTION     code_user
ORG         $A000

SECTION     data_user
ORG         -1
