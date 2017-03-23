;
; Modified to run on the RC2014 and the YAZ180 by
; Phillip Stevens @feilipu https://feilipu.me
; February / March 2017
;

#include "d:/yaz180.h"          ;include the yaz180 header

#include "d:/am9511a.asm"       ;include the Am9511A-1 library
#include "d:/z80_lllf.asm"      ;include the LLL float library

;
;==============================================================================
;       RC2014 & YAZ180 DEFINES
;

DEINT       .EQU    0C47H           ;Function DEINT to get (IX+USR) into DE registers
ABPASS      .EQU    13BDH           ;Function ABPASS to put output into AB register for return

USRSTART    .EQU    $4000           ; start of USR(x) asm code

;==============================================================================
;       SIMPLE EXERCISE PROGRAM
;


SCRPG   .EQU    28H             ;SCRATCH PAGE IS 2800H
OP1     .EQU    00H             ;STARTING LOCATION OF OPERAND 1
OP2     .EQU    OP1+4           ;STARTING LOCATION OF OPERAND 2
RSULT   .EQU    OP2+4           ;STARTING LOCATION OF RESULT
SCR     .EQU    RSULT+4         ;STARTING LOCATION OF SCRATCH AREA


                                ; ORIGIN FOR YAZ180 DURING TESTING
        .org USRSTART           ; start from 'X' jump, Basic prompt

                                ; Am9511A I/O is from $C000 to $C001

                                ; assume the operand byte code in function call
                                ; return 16 bit result (if relevant)
                                ; NOS, TOS, poked to relevant addresses
                                ; Result peeked from relevant address

        call DEINT              ; get the USR(x) argument in de     

TEST:

        ld (STACKTOP), sp       ; store the old stack top, at top of new SP
        ld sp, STACKTOP         ; set new Stack Pointer

        LD      HL, APU_HELLO   ;LOAD HL ADDRESS OF HELLO
        CALL    PRINT           ;PRINT IT

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
        
        call APU_INIT           ;INITIALISE THE APU
         
        LD H, SCRPG             ;SET H REGISTER TO RAM SCRATCH PAGE
        LD L, OP1               ;POINTER TO OPERAND 1
        LD C, SCR               ;SCRATCH AREA

        CALL INPUT              ;INPUT OPERAND 1 FROM TTY

        LD D, SCRPG             ;SET D REGISTER TO RAM SCRATCH PAGE
        LD E, OP1               ;POINTER TO OPERAND 1
        ld a, APU_OP_ENT32_CMD  ;ENTER 32 bit (floating point from INPUT)
        call APU_OP_LD          ;POINTER TO OPERAND IN OPERAND BUFFER
        
                                ;EXAMPLE CODE - APU TWO OPERAND COMMAND

        LD H, SCRPG             ;SET H REGISTER TO RAM SCRATCH PAGE
        LD L, OP2               ;POINTER TO OPERAND 2
        LD C, SCR               ;SCRATCH AREA

        CALL INPUT              ;INPUT OPERAND 2 FROM TTY

        LD D, SCRPG             ;SET D REGISTER TO RAM SCRATCH PAGE
        LD E, OP2               ;POINTER TO OPERAND 2
        ld a, APU_OP_ENT32_CMD  ;ENTER 32 bit (floating point from INPUT)
        call APU_OP_LD          ;POINTER TO OPERAND IN OPERAND BUFFER
        
        ld a, $10               ;COMMAND for FADD (floating add)
        call APU_CMD_LD         ;ENTER a COMMAND

        LD D, SCRPG             ;SET D REGISTER TO RAM SCRATCH PAGE
        LD E, RSULT             ;(D)E POINTER NOW RSULT
        ld a, APU_OP_REM32_CMD  ;REMOVE 32 bit OPERAND (floating point in this case)
        call APU_OP_LD
        
        call APU_ISR            ;KICK OFF APU PROCESS INTERRUPTS
;        call APU_ISR            ;KICK OFF APU PROCESS WITHOUT INTERRUPTS
;        call APU_ISR            ;KICK OFF APU PROCESS WITHOUT INTERRUPTS        
;        call APU_ISR            ;KICK OFF APU PROCESS WITHOUT INTERRUPTS
;        call APU_ISR            ;KICK OFF APU PROCESS WITHOUT INTERRUPTS


        call APU_CHK_IDLE       ; one final check, because it could be doing a last command

                                ;EXAMPLE CODE - OUTPUT

        LD H, SCRPG             ;SET H REGISTER TO RAM SCRATCH PAGE
        LD L, RSULT             ;(H)L POINTER NOW RSULT
        LD C, SCR               ;SCRATCH AREA

        CALL CVRT               ;OUTPUT NUMBER STARTING IN LOCATION RSULT TO TTY
        
        ld sp, (STACKTOP)       ;recover the old Stack Pointer
        
        JP TEST                 ;START AGAIN

;==============================================================================
;       OUTPUT SUBROUTINE
;

PRINT:
        LD A, (HL)              ;Get character from HL
        OR A                    ;Is it $00 ?
        RET Z                   ;Then RETurn on terminator
        CALL TX0                ;PRINT IT
        INC HL                  ;Point to next character 
        JP PRINT                ;Continue until $00

HERE:
        .BYTE   CR,LF
        .BYTE   "here!",0

HELLO:
        .BYTE   CR,LF
        .BYTE   "LLL Float ",0

APU_HELLO:
        .BYTE   CR,LF
        .BYTE   "Am9511A Float ",0

;==============================================================================
;
                .END
;
;==============================================================================

