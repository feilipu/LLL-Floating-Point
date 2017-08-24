;
; Modified to run on the RC2014 and the YAZ180 by
; Phillip Stevens @feilipu https://feilipu.me
; February / March 2017
;
; Converted to z88dk z80asm for RC2014 and YAZ180 by
; Phillip Stevens @feilipu https://feilipu.me
; August 2017
;

INCLUDE "config_yaz180_private.inc"

EXTERN  APU_ISR_NMI
EXTERN  APU_INIT, APU_CHK_IDLE
EXTERN  APU_OP_LD, APU_CMD_LD

EXTERN  INPUT, CVRT
EXTERN  LADD, LSUB, LMUL, LDIV, DSQRT

EXTERN  APUStatus, APUError, APUIntCount

;
;==============================================================================
;       RC2014 & YAZ180 DEFINES
;

DEFC    DEINT   =   $0C47       ;Function DEINT to get (IX+USR) into DE registers
DEFC    ABPASS  =   $13BD       ;Function ABPASS to put output into AB register for return

DEFC    STACKTOP    =   $3FFE   ; start of a global stack (any pushes pre-decrement)

;==============================================================================
;       SIMPLE EXERCISE PROGRAM
;

DEFC    SCRPG   =   $28         ; SCRATCH PAGE IS 2800H
DEFC    OP1     =   $00         ; STARTING LOCATION OF OPERAND 1
DEFC    OP2     =   OP1+$04     ; STARTING LOCATION OF OPERAND 2
DEFC    RSULT   =   OP2+$04     ; STARTING LOCATION OF RESULT
DEFC    SCR     =   RSULT+$04   ; STARTING LOCATION OF SCRATCH AREA


SECTION code_user               ; ORIGIN FOR YAZ180 DURING TESTING
                                ; start from 'X' jump, Basic prompt

PUBLIC _main

_main:
                                ; Am9511A I/O is from $Cn00 to $Cn01

                                ; assume the operand byte code in function call
                                ; return 16 bit result (if relevant)
                                ; NOS, TOS, poked to relevant addresses
                                ; Result peeked from relevant address

        call DEINT              ; get the USR(x) argument in de

TEST:
        LD (STACKTOP), sp       ; store the old stack top, at top of new SP
        LD sp, STACKTOP         ; set new Stack Pointer, before decrement

        LD HL, APU_HELLO        ;LOAD HL ADDRESS OF HELLO
        CALL pstring            ;PRINT IT
        call pnewline

        LD      HL, PYTHAGORAS  ;LOAD HL ADDRESS OF PYTHAGORAS
        CALL    pstring         ;PRINT IT

                                ;EXAMPLE CODE - ONE OPERAND COMMAND

;        LD      H,SCRPG         ;SET H REGISTER TO RAM SCRATCH PAGE
;        LD      L,OP1           ;POINTER TO OPERAND 1
;        LD      C,SCR           ;SCRATCH AREA

;        CALL    INPUT           ;INPUT OPERAND 1 FROM TTY

                                ;EXAMPLE CODE - TWO OPERAND COMMAND

;        LD      H,SCRPG        ;SET H REGISTER TO RAM SCRATCH PAGE
;        LD      L,OP2          ;POINTER TO OPERAND 2
;        LD      C,SCR          ;SCRATCH AREA

;        CALL    INPUT          ;INPUT OPERAND 2 FROM TTY

;        LD      L,OP1          ;OPERAND 1 POINTER IN (H)L
;        LD      B,OP2          ;OPERAND 2 POINTER IN (H)B
;        LD      C,RSULT        ;RESULT TO (H)C POINTER

;        CALL    LDIV           ;DIVIDE OP1 BY OP2 AND PLACE RESULT IN RSULT
;        CALL    LMUL           ;MULTIPLY OP1 BY OP2 AND PLACE RESULT IN RSULT

                                ;EXAMPLE CODE - ONE OPERAND COMMAND

;        LD      L,OP1           ;OPERAND 1 POINTER IN (H)L
;        LD      B,RSULT         ;RESULT TO (H)B POINTER
;        LD      C,SCR           ;SCRATCH AREA

;        CALL    DSQRT           ;SQUARE ROOT OF OP1 AND PLACE RESULT IN RSULT

                                ;EXAMPLE CODE - APU ONE OPERAND COMMAND

        CALL APU_INIT           ;INITIALISE THE APU

        XOR A                   ;clear A register to 0
        LD H, SCRPG             ;SET H REGISTER TO RAM SCRATCH PAGE
        LD L, OP1               ;POINTER TO OPERAND 1
        LD (HL), A              ;CLEAR SCRATCH AREA
        LD D, H
        LD E, L
        INC DE
        LD BC, 10H              ;CLEAR 16 BYTES
        LDIR

        LD H, SCRPG             ;SET H REGISTER TO RAM SCRATCH PAGE
        LD L, OP1               ;POINTER TO OPERAND 1
        LD C, SCR               ;SCRATCH AREA

        CALL INPUT              ;INPUT OPERAND 1 FROM TTY

        LD D, SCRPG             ;SET D REGISTER TO RAM SCRATCH PAGE
        LD E, OP1               ;POINTER TO OPERAND 1
        ld a, __IO_APU_OP_ENT32 ;ENTER 32 bit (floating point from INPUT)
        CALL APU_OP_LD          ;POINTER TO OPERAND IN OPERAND BUFFER
        
                                ;EXAMPLE CODE - APU TWO OPERAND COMMAND

        LD H, SCRPG             ;SET H REGISTER TO RAM SCRATCH PAGE
        LD L, OP2               ;POINTER TO OPERAND 2
        LD C, SCR               ;SCRATCH AREA

        CALL INPUT              ;INPUT OPERAND 2 FROM TTY

        LD D, SCRPG             ;SET D REGISTER TO RAM SCRATCH PAGE
        LD E, OP2               ;POINTER TO OPERAND 2
        LD A, __IO_APU_OP_ENT32 ;ENTER 32 bit (floating point from INPUT)
        CALL APU_OP_LD          ;POINTER TO OPERAND IN OPERAND BUFFER

        LD A, 17h               ;COMMAND for PTOF (push floating )
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, 12h               ;COMMAND for FMUL (floating multiply)
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, 19h               ;COMMAND for XCHF (swap float)
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, 17h               ;COMMAND for PTOF (push floating )
        CALL APU_CMD_LD         ;ENTER a COMMAND

;        LD A, 13h               ;COMMAND for FDIV (floating divide)
;        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, 12h               ;COMMAND for FMUL (floating multiply)
        CALL APU_CMD_LD         ;ENTER a COMMAND

;        LD A, 11h               ;COMMAND for FSUB (floating subtract)
;        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, 10h               ;COMMAND for FADD (floating add)
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, 01h               ;COMMAND for SQRT (floating square root)
        CALL APU_CMD_LD         ;ENTER a COMMAND

;        LD A, 1Ah               ;COMMAND for PUPI (push Pi)
;        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD D, SCRPG             ;SET D REGISTER TO RAM SCRATCH PAGE
        LD E, RSULT             ;(D)E POINTER NOW RSULT
        LD A, __IO_APU_OP_REM32 ;REMOVE 32 bit OPERAND (floating point in this case)
        CALL APU_OP_LD

        CALL APU_ISR_NMI        ;KICK OFF APU PROCESS, WHICH THEN INTERRUPTS

        CALL APU_CHK_IDLE       ;check, because it could be doing a last command

                                ;EXAMPLE CODE - OUTPUT

        LD H, SCRPG             ;SET H REGISTER TO RAM SCRATCH PAGE
        LD L, RSULT             ;(H)L POINTER NOW RSULT
        LD C, SCR               ;SCRATCH AREA

        call CVRT               ;OUTPUT NUMBER STARTING IN LOCATION RSULT TO TTY
        call pnewline           ;print newline

        ld a, (APUIntCount)     ; print how many times the interrupt was entered      
        call phex 
        call pnewline

        LD SP, (STACKTOP)       ;reenable old SP
        
        ld a, (APUError)        ;any errors ?
        ld b, a
        xor a
        jp ABPASS               ;output them
        
;        JP TEST                 ;START AGAIN

;==============================================================================
;       OUTPUT SUBROUTINE
;

    ;print string
pstring:
    ld a, (hl)          ;Get character from HL
    or a                ;Is it $00 ?
    ret z               ;Then RETurn on terminator
    rst 08H             ;Print IT
    inc hl              ;Point to next character 
    jp pstring            ;Continue until $00


    ;print CR/LF
pnewline:
    ld a, CHAR_CR
    rst 08
    ld a, CHAR_LF
    rst 08
    ret

    ;print contents of HL as 16 bit number in ASCII HEX
phex16:
    push af
    ld a, h
    call phex
    ld a, l
    call phex
    pop af
    ret

    ;print contents of A as 8 bit number in ASCII HEX
phex:
    push af             ;store the binary value
    rlca                ;shift accumulator left by 4 bits
    rlca
    rlca
    rlca
    and $0F             ;now high nibble is low position
    cp 10
    jr c, phex_b        ;jump if high nibble < 10
    add a, 7            ;otherwise add 7 before adding '0'
phex_b:
    add a, '0'          ;add ASCII 0 to make a character
    rst 08              ;print high nibble
    pop af              ;recover the binary value
phex1:
    and $0F
    cp 10
    jr c, phex_c        ;jump if low nibble < 10
    add a, 7
phex_c:
    add a, '0'
    rst 08              ;print low nibble
    ret

;------------------------------------------------------------------------------

;SECTION  data_user

HELLO:      DEFM    CHAR_CR,CHAR_LF,"LLL Float ",0
APU_HELLO:  DEFM    CHAR_CR,CHAR_LF,"Am9511A Float ",0
PYTHAGORAS: DEFM    CHAR_CR,CHAR_LF,"SQRT[ a^2 + b^2 ] ",0

;==============================================================================
;
;==============================================================================

