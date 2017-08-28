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
        call DEINT              ; get the USR(x) argument in de

        LD (STACKTOP), sp       ; store the old stack top, at top of new SP
        LD sp, STACKTOP         ; set new Stack Pointer, before decrement

        LD HL, APU_HELLO        ;LOAD HL ADDRESS OF HELLO
        CALL pstring            ;PRINT IT
        call pnewline

                                ;EXAMPLE CODE - LLLF ONE OPERAND COMMAND

;        LD      H,SCRPG         ;SET H REGISTER TO RAM SCRATCH PAGE
;        LD      L,OP1           ;POINTER TO OPERAND 1
;        LD      C,SCR           ;SCRATCH AREA

;        CALL    INPUT           ;INPUT OPERAND 1 FROM TTY

                                ;EXAMPLE CODE - LLLF TWO OPERAND COMMAND

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

        LD HL, PYTHAGORAS       ;LOAD HL ADDRESS OF PYTHAGORAS
        CALL pstring            ;PRINT IT


                                ;EXAMPLE CODE - APU ONE OPERAND COMMAND

        CALL APU_INIT           ;INITIALISE THE APU
                                ;Am9511A I/O is from $Cn00 to $Cn01

        ld hl, INPUT_DWD_PROMPT
        call pstring

        LD H, SCRPG             ;SET H REGISTER TO RAM SCRATCH PAGE
        LD L, OP1               ;POINTER TO OPERAND 1
        LD C, SCR               ;SCRATCH AREA

;        CALL INPUT              ;INPUT OPERAND 1 FROM TTY
        call rhexdwd

        LD D, SCRPG             ;SET D REGISTER TO RAM SCRATCH PAGE
        LD E, OP1               ;POINTER TO OPERAND 1
        LD a, __IO_APU_OP_ENT32 ;ENTER 32 bit (double word from INPUT)
        CALL APU_OP_LD          ;POINTER TO OPERAND IN OPERAND BUFFER

                                ;EXAMPLE CODE - APU TWO OPERAND COMMAND
        ld hl, INPUT_DWD_PROMPT
        call pstring

        LD H, SCRPG             ;SET H REGISTER TO RAM SCRATCH PAGE
        LD L, OP2               ;POINTER TO OPERAND 2
        LD C, SCR               ;SCRATCH AREA

;        CALL INPUT              ;INPUT OPERAND 2 FROM TTY
        call rhexdwd

        LD D, SCRPG             ;SET D REGISTER TO RAM SCRATCH PAGE
        LD E, OP2               ;POINTER TO OPERAND 2
        LD A, __IO_APU_OP_ENT32 ;ENTER 32 bit (double word from INPUT)
        CALL APU_OP_LD          ;POINTER TO OPERAND IN OPERAND BUFFER

        LD A, __IO_APU_OP_FLTD  ;COMMAND for FLTD (float double)
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, __IO_APU_OP_PTOF  ;COMMAND for PTOF (push float)
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, __IO_APU_OP_FMUL  ;COMMAND for FMUL (floating multiply)
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, __IO_APU_OP_XCHF  ;COMMAND for XCHF (swap float)
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, __IO_APU_OP_FLTD  ;COMMAND for FLTD (float double)
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, __IO_APU_OP_PTOF  ;COMMAND for PTOF (push floating)
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, __IO_APU_OP_FMUL  ;COMMAND for FMUL (floating multiply)
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, __IO_APU_OP_FADD  ;COMMAND for FADD (floating add)
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, __IO_APU_OP_SQRT  ;COMMAND for SQRT (floating square root)
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD A, __IO_APU_OP_FIXD  ;COMMAND for FIXD (fix double)
        CALL APU_CMD_LD         ;ENTER a COMMAND

        LD D, SCRPG             ;SET D REGISTER TO RAM SCRATCH PAGE
        LD E, RSULT             ;(D)E POINTER NOW RSULT
        LD A, __IO_APU_OP_REM32 ;REMOVE 32 bit OPERAND
        CALL APU_OP_LD

        CALL APU_ISR_NMI        ;KICK OFF APU PROCESS, WHICH THEN INTERRUPTS

        CALL APU_CHK_IDLE       ;CHECK, because it could be doing a last command

                                ;EXAMPLE CODE - OUTPUT
        call pnewline
        ld hl, OUTPUT_DWD_PROMPT
        call pstring

        LD H, SCRPG             ;SET H REGISTER TO RAM SCRATCH PAGE
        LD L, RSULT             ;(H)L POINTER NOW RSULT
        LD C, SCR               ;SCRATCH AREA

;        call CVRT               ;OUTPUT NUMBER STARTING IN LOCATION RSULT TO TTY
        call phexdwd
        call pnewline           ;print newline

        ld a, (APUIntCount)     ; print how many times the interrupt was entered      
        call phex 
        call pnewline

        LD SP, (STACKTOP)       ;reenable old SP
        
        ld a, (APUError)        ;any errors ?
        ld b, a
        xor a
        jp ABPASS               ;output them

;==============================================================================
;       INPUT SUBROUTINES
;

rhexdwd:                        ; returns 4 bytes LE, from address in hl
        push af
        inc hl
        inc hl
        inc hl
        call rhex
        ld (hl), a
        call phex
        dec hl
        call rhex
        ld (hl), a
        call phex
        dec hl
        call rhex
        ld (hl), a
        call phex
        dec hl
        call rhex
        ld (hl), a
        call phex     
        pop af
        ret

rhexwd:                         ; returns 2 bytes LE, from address in hl
        push af
        inc hl
        call rhex
        ld (hl), a
        call phex
        dec hl
        call rhex
        ld (hl), a
        call phex
        pop af
        ret

rhex:                           ; Returns byte in a
        push bc
        rst 10H                 ; Rx byte
        sub '0'
        cp 10
        jr c, rhexnbl2          ; if a<10 read the second nibble
        sub 7                   ; else subtract 'A'-'0' (17) and add 10
rhexnbl2:
        rlca                    ; shift accumulator left by 4 bits
        rlca
        rlca
        rlca
        ld c, a                 ; temporarily store the first nibble in c
        rst 10H                 ; Rx byte
        sub '0'
        cp 10
        jr c, rhexend           ; if a<10 finalize
        sub 7                   ; else subtract 'A' (17) and add 10
rhexend:
        or c                    ; assemble two nibbles into one byte in a
        pop bc
        ret                     ; return the byte read in a

;==============================================================================
;       OUTPUT SUBROUTINES
;

    ;print string
pstring:
    ld a, (hl)          ;Get character from HL
    or a                ;Is it $00 ?
    ret z               ;Then RETurn on terminator
    rst 08H             ;Print IT
    inc hl              ;Point to next character 
    jp pstring          ;Continue until $00

    ;print CR/LF
pnewline:
    ld a, CHAR_CR
    rst 08
    ld a, CHAR_LF
    rst 08
    ret

    ;print Double Word at address HL as 32 bit number in ASCII HEX
phexdwd:
    push af
    inc hl
    inc hl
    inc hl
    ld a, (hl)
    call phex
    dec hl
    ld a, (hl)
    call phex
    dec hl
    ld a, (hl)
    call phex
    dec hl
    ld a, (hl)
    call phex
    pop af
    ret

    ;print Word at address HL as 16 bit number in ASCII HEX
phexwd:
    push af
    inc hl
    ld a, (hl)
    call phex
    dec hl
    ld a, (hl)
    call phex
    pop af
    ret

    ;print contents of DEHL as 32 bit number in ASCII HEX
phexdwdreg:
    push af
    ld a, d
    call phex
    ld a, e
    call phex
    ld a, h
    call phex
    ld a, l
    call phex
    pop af
    ret

    ;print contents of HL as 16 bit number in ASCII HEX
phexwdreg:
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
INPUT_WD_PROMPT:    DEFM    CHAR_CR,CHAR_LF,"WD  Input  0x",0
INPUT_DWD_PROMPT:   DEFM    CHAR_CR,CHAR_LF,"DWD Input  0x",0
OUTPUT_WD_PROMPT:   DEFM    CHAR_CR,CHAR_LF,"WD  Output 0x",0
OUTPUT_DWD_PROMPT:  DEFM    CHAR_CR,CHAR_LF,"DWD Output 0x",0

;==============================================================================
;
;==============================================================================

