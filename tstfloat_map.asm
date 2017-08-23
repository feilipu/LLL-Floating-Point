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

INCLUDE "config_yaz180_private.inc"

SECTION     apu_data_align_256
ORG         $2500

SECTION     apu_data
ORG         $2700

SECTION     apu_driver
ORG         $2800

SECTION     apu_library
ORG         $3000

SECTION     code_user
ORG         $8200

