; original LLL code restored Herb Johnson Feb 2015
; original LLL code from "Floating Point Package for
; Intel 8008 and 8080 Microprocessors" by Maples Oct 24 1975
; URCL-51940 publication from Lawrence Livermore Laboratory
; 171286.PDF 
;
; fixes:
; 0) many lines had space in column 1! labels become operands!
; 1) added LLL square root routine
; 2) added missing "CALL SIGN" in CVRT routine
; 3) replaced ERR routine with one from LLL document
; 4) commented out "ORA A" at end of SAVEN
;
; ###S
; MODIFIED BY TONY GOLD FOR NON-MACRO ASSEMBLER
; CHANGES WITHIN ;###S AND ;###E LINES
; ALL ORIGINAL CODE RETAINED AS COMMENTS
; ###E
;
; //// FLOATING POINT PACKAGE FOR THE MCS8
; //// BY DAVID MEAD
; //// MODIFIED BY HAL BRAND 9/6/74
; //// MODIFIED FOR 24 BIT MANTISSAS
; //// PLUS ADDED I/O CONVERSION ROUTINES
; //// MODIFIED BY FRANK OLKEN 6/28/75
;
;
; Modified to run on the RC2014 and the YAZ180 by
; Phillip Stevens @feilipu https://feilipu.me
; February / March 2017
;
; RC2014 and YAZ180 have TX0 and RX0 and RX0_CHK located at RST jumps.
;
;TX0            RST 1
;RX0            RST 2
;RX0_CHK        RST 3
;

MINCH   .EQU    300Q             ;MINIMUM CHARACTERISTIC WITH SIGN EXTENDED
MAXCH   .EQU    077Q             ;MAXIMUM CHARACTERISTIC WITH SIGN EXTENDED

;
;
;******************************************************
;       //// SIMPLE EXERCISE PROGRAM
;******************************************************
;

                                ; RC2014 Function Calls from Nascom Basic Symbol Tables
DEINT    .EQU    0B57H          ; Function DEINT to get USR(x) into DE registers
ABPASS   .EQU    12CCH          ; Function ABPASS to put output into AB register for return

                                ; YAZ180 Function Calls from Nascom Basic Symbol Tables
;DEINT   .EQU    0C3FH          ; Function DEINT to get USR(x) into DE registers
;ABPASS  .EQU    13B4H          ; Function ABPASS to put output into AB register for return


SCRPG   .EQU    23H             ;SCRATCH PAGE IS 2300H
OP1     .EQU    00H             ;STARTING LOCATION OF OPERAND 1
OP2     .EQU    OP1+4           ;STARTING LOCATION OF OPERAND 2
RSULT   .EQU    OP2+4           ;STARTING LOCATION OF RESULT
SCR     .EQU    RSULT+4         ;STARTING LOCATION OF SCRATCH AREA

        .ORG    3000H
        
TEST:
        MVI     H, SCRPG        ;SET H REGISTER TO RAM SCRATCH PAGE
        MVI     L, OP1          ;POINTER TO OPERAND 1
        MVI     C, SCR          ;SCRATCH AREA
        CALL    INPUT           ;INPUT OPERAND 1 FROM TTY
        MVI     L, OP2          ;POINTER TO OPERAND 2
        MVI     C, SCR          ;SCRATCH AREA
        CALL    INPUT           ;INPUT OPERAND 2 FROM TTY
        MVI     L, OP1          ;OPERAND 1 POINTER IN (H)L
        MVI     B, OP2          ;OPERAND 2 POINTER IN (H)B
        MVI     C, RSULT        ;RESULT TO (H)C POINTER
        CALL    LDIV            ;DIVIDE OP1 BY OP2 AN PLACE RESULT IN RSULT
        MVI     L, RSULT        ;(H)L POINTER NOW RSULT
        MVI     C, SCR          ;SCRATCH AREA
        CALL    CVRT            ;OUTPUT NUMBER STARTING IN LOCATION RSULT TO TTY
        JP      TEST            ;START AGAIN

;
;
;******************************************************
;       //// OUTPUT SUBROUTINES
;******************************************************
;
OUTR:
	PUSH	B		;SAVE REG B
	MVI	B, 1		;PAD ONCE
	CALL	PAD		;DO IT
	POP	B		;RESTORE B AND RET
	RET

PAD:
	PUSH	B		;SAVE THE COUNT AND C
	MOV	C, A		;SAVE CHARACTER IN C
P1:
	RST	1		;OUTPUT THE CHARACTER TO TX0
	DCR	B		;DECREMENT NUMB. CHARACTERS
	MOV	A, C		;GET THE CHARACTER BACK
	JNZ	P1		;GO TO P1 IF ANY CHARACTERS LEFT
	POP	B		;RESTORE B AND C
	RET			;TRANSMITTED CHARACTER STILL IN A

;
;
;******************************************************
;       //// INPUT SUBROUTINES
;******************************************************
;
INP:
	RST	2		;INPUT A CHARACTER FROM RX0
	RET			;RETURN

;
;
;******************************************************
;       //// DIVIDE SUBROUTINE
;******************************************************
;
LDIV:   CALL    CSIGN           ;COMPUTE SIGN OF RESULT
        CALL    ZCHK            ;CHECK IF DIVIDEND = ZERO
        JNZ     DTST2           ;IF DIVIDEND .NE. 0 CHECK DIVISOR
        CALL    BCHK            ;CHECK FOR ZERO/ZERO
        JZ      INDFC           ;ZERO/ZERO = INDEFINITE
        JMP     WZERC           ;ZERO/NONZERO = ZERO
DTST2:  CALL    BCHK            ;COME HERE IF DIVIDEND .NE. 0
        JZ      OFLWC           ;NONZERO/ZERO = OVERFLOW
                                ;IF WE GET HERE, THINGS LOOK OKAY
        MOV     E,L             ;SAVE BASE IN E
        MOV     L,C             ;BASE 6 TO L
        CALL    DCLR            ;CLEAR QUOTIENT MANTISSA SLOT
        MOV     L,E             ;RESTORE BASE IN L
        CALL    ENT1            ;DO FIRST CYCLE
        MOV     L,C             ;BASE 6 TO L
        CALL    DLST            ;MOVE QUOTIENT OVER ONE PLACE
        MVI     D,23            ;NUMBER OF ITERATIONS TO D
REP3:   MOV     L,E
        CALL    ENT2
        DCR     D               ;DEC D
        JZ      GOON
        MOV     A,L
        MOV     L,C             ;BASE 6 TO L
        MOV     C,A
        CALL    DLST            ;MOVE QUOTIENT MANT OVER
        MOV     A,L             ;CPTR TO A
        MOV     E,C             ;LPTR TO E
        MOV     C,A             ;CPTR TO C
        JMP     REP3
;
GOON:   CALL    AORS            ;CHECK IF RESULT IS NORMALIZED
        JM      CRIN
        MOV     A,L             ;LPTR TO A
        MOV     L,C             ;CPTR TO L
        MOV     C,A             ;LPTR TO C
        CALL    DLST            ;SHIFT QUOTIENT LEFT
        MOV     C,L
        MOV     L,E
        CALL    LDCP            ;COMPUTE THE CHARACTERISTIC OF RESULT
        RET
;
CRIN:   CALL    CFCHE           ;GET A=CHAR(H,L), E=CHAR(H,B)
        SUB     E               ;NEW CHAR = CHAR(DIVIDEND) - CHAR(DVISIOR)
        CPI     177Q            ;CHECK MAX POSITIVE NUMBER
        JZ      OFLWC           ;JUMP ON OVERFLOW
        ADI     1               ;ADD 1 SINCE WE DID NOT LEFTSHIFT
        CALL    CCHK            ;CHECK AND STORE CHARACTERISTIC
        RET                     ;RETURN
;
;
;
;******************************************************
;       //// ADDITION SUBROUTINE
;******************************************************
;
;
LADD:   XRA     A               ;/***SET UP TO ADD
        JMP     LADS            ;NOW DO IT
;
;
;******************************************************
;       //// SUBTRACTION SUBROUTINE
;******************************************************
;
;       SUBROUTINE LADS
;
;       FLOATING POINT ADD OR SUB
;       A 128 ON ENTRY SUB
;       A 0 ON ENTRY ADD
;       F-S F,FIRST OPER DESTROYED
;       BASE 11 USED FOR SCRATCH
;
LSUB:   MVI     A,200Q          ;/****SET UP TO SUBTRACT
;
LADS:   CALL    ACPR            ;SAVE ENTRY PNT AT BASE 6
        CALL    BCHK            ;CHECK ADDEND/SUBTRAHEND = ZERO
        RZ                      ;IF SO, RESULT=ARG SO RETURN
                                ;THIS WILL PREVENT UNDERFLOW INDICATION ON
                                ;ZERO + OR - ZERO
        CALL    CCMP
        JZ      EQ02            ;IF EQUAL, GO ON
        MOV     D,A             ;SAVE LPTR CHAR IN D
        JC      LLTB
        SUB     E               ;L.GT.B IF HERE
        ANI     127
        MOV     D,A             ;DIFFERENCE TO D
        MOV     E,L             ;SAVE BASE IN E
        MOV     L,C             ;C PTR TO L
        INR     L               ;C PTR 1 TO L
        MOV     M,E             ;SAVE BASE IN C PTR\1
        MOV     L,B             ;B PTR TO L
        JMP     NCHK
LLTB:   MOV     A,E             ;L.LT.B IF HERE,BPTR TO A
        SUB     D               ;SUBTRACT LPTR CHAR FROM BPTR CHAR
        ANI     127
        MOV     D,A             ;DIFFERENCE TO D
NCHK:   MVI     A,24
        CMP     D
        JNC     SH10
        MVI     D,24
SH10:   ORA     A
        CALL    DRST
        DCR     D
        JNZ     SH10
EQUL:   MOV     A,L
        CMP     B
        JNZ     EQ02            ;F.GT.S IF L.NE.B
        MOV     L,C             ;C PTR TO L
        INR     L               ;C PTR\1 TO L
        MOV     L,M             ;RESTORE L
EQ02:   CALL    LASD            ;CHECK WHAT TO
        CALL    ACPR            ;SAVE ANSWER
        CPI     2               ;TEST FOR ZERO ANSWER
        JNZ     NOT0
        JMP     WZER            ;WRITE FLOATING ZERO AND RETURN
;
NOT0:   MVI     D,1             ;WILL TEST FOR SUB
        ANA     D
        JZ      ADDZ            ;LSB 1 IMPLIES SUB
        CALL    TSTR            ;CHECK NORMAL/REVERSE
        JZ      SUBZ            ;IF NORMAL,GO SUBZ
        MOV     A,L             ;OTHERWISE REVERSE
        MOV     L,B             ;ROLES
        MOV     B,A             ;OF L AND B
;
SUBZ:   CALL    DSUB            ;SUBTRACT SMALLER FROM BIGGER
        CALL    MANT            ;SET UP SIGN OF RESULT
        CALL    TSTR            ;SEE IF WE NEED TO INTERCHANGE
                                ;BPTR AND LPTR
        JZ      NORM            ;NO INTERCHANGE NECESSARY, SO NORMALIZE
                                ;AND RETURN
        MOV     A,L             ;INTERCHANGE
        MOV     L,B             ;L
        MOV     B,A             ;AND B
        MOV     A,C             ;CPTR  TO A
        MOV     C,B             ;BPTR TO C
        MOV     E,L             ;LPTR TO E
        MOV     B,A             ;CPTR TO B
        CALL    LXFR            ;MOVE BPTR> TO LPTR>
        MOV     A,B
        MOV     B,C
        MOV     C,A
        MOV     L,E
        JMP     NORM            ;NORMALIZE RESULT AND RETURN
;
;   COPY THE LARGER CHARACTERISTIC TO THE RESULT
;
ADDZ:   CALL    CCMP            ;COMPARE THE CHARACTERISTICS
        JNC     ADD2            ;IF CHAR(H,L) .GE. CHAR(H,B) CONTINUE
        CALL    BCTL            ;IF CHAR(H,L) .LT. CHAR(H,B) THE COPY
                                ;CHAR(H,B) TO CHAR(H,L)
ADD2:   CALL    MANT            ;COMPUTE SIGN OF RESULT
        CALL    DADD            ;ADD MANTISSAS
        JNC     SCCFG           ;IF THERE IS NO OVFLW - DONE
        CALL    DRST            ;IF OVERFLOW SHIFT RIGHT
        CALL    INCR            ;AND INCREMENT CHARACTERISTIC
        RET                     ;ALL DONE, SO RETURN
;
;   THIS ROUTINE STORES THE MANTISSA SIGN IN THE RESULT
;   THE SIGN HAS PREVIOUSLY BEEN COMPUTED BY LASD.
;
MANT:   MOV     E,L             ;SAVE L PTR
        MOV     L,C             ;C PTR TO L
        MOV     A,M             ;LOAD INDEX WORD
        ANI     128             ;SCARF SIGN
        MOV     L,E             ;RESTORE L PTR
        INR     L               ;L PTR 2
        INR     L
        INR     L               ;TO L
        MOV     E,A             ;SAVE SIGN IN E
        MOV     A,M
        ANI     127             ;SCARF CHAR
        ADD     E               ;ADD SIGN
        MOV     M,A             ;STORE IT
        DCR     L               ;RESTORE
        DCR     L
        DCR     L               ;L PTR
        RET
;
;
;   SUBROUTINE LASD
;
;       UTILITY ROUTINE FOR LADS
;       CALCULATES TRUE OPER AND SGN
;       RETURNS ANSWER IN
;
LASD:   CALL    MSFH            ;FETCH MANT SIGNS, F IN A,D
        CMP     E               ;COMPARE SIGNS
        JC      ABCH            ;F\,S- MEANS GO TO A BRANCH
        JNZ     BBCH            ;F- S\ MEANS GO TO B BRANCH
        ADD     E               ;SAME SIGN IF HERE, ADD SIGNS
        JC      BMIN            ;IF BOTH MINUS, WILL OVERFLOW
        CALL    AORS            ;BOTH POS IF HERE
        JP      L000            ;IF AN ADD, LOAD 0
COM1:   CALL    DCMP            ;COMPARE F WITH S
        JC      L131            ;S.GT.F,SO LOAD 131
        JNZ     L001            ;F.GT.S,SO LOAD 1
L002:   MVI     A,2             ;ERROR CONDITION, ZERO ANSWER
        RET
BMIN:   CALL    AORS            ;CHECK FOR ADD OR SUB
        JP      L128            ;ADD, SO LOAD 128
COM2:   CALL    DCMP            ;COMPARE F WITH S
        JC      L003            ;S.GT.F,SO LOAD 3
        JNZ     L129            ;FGT.S.SO LOAD 129
        JMP     L002            ;ERROR
ABCH:   CALL    AORS            ;FT,S- SO TEST FOR A/S
        JM      L000            ;SUBTRACT, SO LOAD 0
        JMP     COM1            ;ADD, SO GO TO DCMP
BBCH:   CALL    AORS            ;F-,S\,SO TEST FOR A/S
        JM      L128            ;SUB
        JMP     COM2            ;ADD
L000:   XRA     A
        RET
L001:   MVI     A,1
        RET
L003:   MVI     A,3
        RET
L128:   MVI     A,128
        RET
L129:   MVI     A,129
        RET
L131:   MVI     A,131
        RET
;
;       SUBROUTINE LMCM
;       COMPARES THE MAGNITUDE OF
;       TWO FLOATING PNT NUMBERS
;       Z[1 IF [,C[1 IF F.LT.S.
;
LMCM:   CALL    CCMP            ;CHECK CHARS
        RNZ                     ;RETURN IF NOT EQUAL
        CALL    DCMP            ;IF EQUAL, CHECK MANTS
        RET
;
;
;
;***************************************************
;       //// MULTIPLY SUBROUTINE
;***************************************************
;
;   SUBROUTINE LMUL
;
;       FLOATING POINT MULTIPLY
;       L PTR X B PTR TO C PTR
;
LMUL:   CALL    CSIGN           ;COMPUTE SIGN OF RESULT AND STORE IT
        CALL    ZCHK            ;CHECK FIRST OPERAND FOR ZERO
        JZ      WZERC           ;ZERO * ANYTHING = ZERO
        CALL    BCHK            ;CHECK SECOND OPERAND FOR ZERO
        JZ      WZERC           ;ANYTHING * ZERO = ZERO
        MOV     E,L             ;SAVE L PTR
        MOV     L,C             ;C PTR TO L
        CALL    DCLR            ;CLR PRODUCT MANT LOCS
        MOV     L,E             ;L PTR TO L
        MVI     D,24            ;LOAD NUMBER ITERATIONS
KPGO:   CALL    DRST            ;SHIFT L PTR RIGHT
        JC      MADD            ;WILL ADD B PTR IF C[1
        MOV     A,L             ;INTERCHANGE
        MOV     L,C             ;L AND
        MOV     C,A             ;C PTRS
INTR:   CALL    DRST            ;SHIFT PRODUCT OVER
        MOV     A,L             ;INTERCHANGE
        MOV     L,C             ;L AND C PTRS_BACK TO
        MOV     C,A             ;ORIGINAL>
        DCR     D
        JNZ     KPGO            ;MORE CYCLES IF Z[0
        CALL    AORS            ;TEST IF RESULT IS NORMALIZED
        JM      LMCP            ;IF NORMALIZED GO COMPUTE CHAR
        MOV     E,L             ;SAVE LPTR IN E
        MOV     L,C             ;SET L=CPTR
        CALL    DLST            ;LEFT SHIFT RESULT TO NORMALIZE
        MOV     L,E             ;RESTORE LPTR
        CALL    CFCHE           ;OTHERWISE SET A=CHAR(H,L), E=CHAR(H,B)
        ADD     E               ;CHAR(RESULT) = CHAR(H,L) + CHAR(H,B)
        CPI     200Q            ;CHECK FOR SMALLEST NEGATIVE NUMBER
        JZ      UFLWC           ;IF SO THEN UNDERFLOW
        SUI     1               ;SUBTRACT 1 TO COMPENSATE FOR NORMALIZE
        CALL    CCHK            ;CHECK CHARACTERISTIC AND STORE IT
        RET                     ;RETURN
;
MADD:   MOV     A,L             ;INTERCHANGE
        MOV     L,C             ;L AND
        MOV     C,A             ;C PTRS
        CALL    DADD            ;ACCUMULATE PRODUCT
        JMP     INTR
;
;   SUBROUTINE NORM
;
;       THIS SUBROUTINE WILL NORMALIZE A FLOATING POINT
;       NUMBER, PRESERVING ITS ORIGINAL SIGN.
;       WE CHECK FOR UNDERFLOW AND SET THE CONDITION
;       FLAG APPROPRIATELY.  (SEE ERROR RETURNS).
;       THERE IS AN ENTRY POINT TO FLOAT A SIGNED INTEGER
;       (FLOAT) AND AN ENTRY POINT TO FLOAT AN UNSIGNED
;       INTEGER.
;
;   ENTRY POINTS:
;
;       NORM  - NORMALIZE FLOATING PT NUMBER AT (H,L)
;       FLOAT - FLOAT TRIPLE PRECISION INTEGER AT (H,L)
;               PRESERVING SIGN BIT IN (H,L)+3
;       DFXL  - FLOAT UNSIGNED (POSITIVE) TRIPLE PRECISION
;               AT (H,L)
;
;   REGISTERS ON EXIT:
;
;       A = CONDITION FLAG (SEE ERROR RETURNS)
;       D,E = GARBAGE
;       B,C,H,L = SAME AS ON ENTRY
;
NORM:   MOV     E,L             ;SAVE L IN E
NORM1:  CALL    GCHAR           ;GET CHAR(H,L) IN A WITH SIGN EXTENDED
        MOV     D,A             ;SAVE CHAR IN D
FXL1:   MOV     L,E             ;RESTORE L
FXL2:   CALL    ZMCHK           ;CHECK FOR ZERO MANTISSA
        JZ      WZER            ;IF ZERO MANTISSA THEN ZERO RESULT
REP6:   MOV     A,M             ;GET MOST SIGNIFICANT BYTE OF
                                ;MANTISSA
        ORA     A               ;SET FLAGS
        JM      SCHAR           ;IF MOST SIGNIFICANT BIT = 1 THEN
                                ;NUMBER IS NORMALIZED AND WE GO TO
                                ;STORE THE CHARACTERISTIC
        MOV     A,D             ;OTHERWISE CHECK FOR UNDERFLOW
        CPI     MINCH           ;COMPARE WITH MINIMUM CHAR
        JZ      WUND            ;IF EQUAL THEN UNDERFLOW
        CALL    DLST            ;SHIFT MANTISSA LEFT
        DCR     D               ;DECREMENT CHARACTERISTIC
        JMP     REP6            ;LOOP AN TEST NEXT BIT
SCHAR:  JMP     INCR3           ;STORE THE CHARACTERISTIC USING
                                ;THE SAME CODE AS THE INCREMENT
;
DFXL:   MOV     E,L             ;ENTER HERE TO FLOAT UNSIGNED
                                ;INTEGER
                                ;FIRST SAVE L IN E
        INR     L               ;MAKE (H,L) POINT TO CHAR
        INR     L               ;MAKE (H,L) POINT TO CHAR
        INR     L               ;MAKE (H,L) POINT TO CHAR
        XRA     A               ;ZERO ACCUMULATOR
        MOV     M,A             ;STORE A PLUS (+) SIGN
        MOV     L,E             ;RESTORE L
FLOAT:  MVI     D,24            ;ENTER HERE TO FLOAT INTEGER
                                ;PRESERVING ORIGINAL SIGN IN (H,L)+3
                                ;SET UP CHARACTERISTIC
        JMP     FXL2            ;GO FLOAT THE NUMBER
;
;
;
;
;   SUBROUTINE ZCHK
;
;       THIS ROUTINE SETS THE ZERO FLAG IF IT DETECTS
;       A FLOATING ZERO AT (H,L).
;
;   SUBROUTINE ZMCHK
;
;       THIS ROUTINE SETS THE ZERO FLAG IF IT DETECTS A
;       ZERO MANTISSA AT (H,L)
;
ZCHK:
ZMCHK:  INR     L               ;SET L TO POINT LAST BYTE OF MANTISSA
        INR     L               ;SET L TO POINT TO LAST BYTE OF MANTISSA
        MOV     A,M             ;LOAD LEAST SIGNIFICANT BYTE
        DCR     L               ;L POINTS TO MIDDLE BYTE
        ORA     M               ;OR WITH LEAST SIGNIFICANT BYTE
        DCR     L               ;L POINTS TO MOST SIGNIFICANT BYTE
                                ;OF MANTISSA (ORIGINAL VALUE)
        ORA     M               ;OR IN MOST SIGNIFICANT BYTE
        RET                     ;RETURNS WITH ZERO FLAG SET APPROPRIATELY
;
;  SUBROUTINE BCHK
;
;       THIS ROUTINE CHECKS (H,B) FOR FLOATING PT ZERO
;
BCHK:   MOV     E,L             ;SAVE LPTR IN E
        MOV     L,B             ;SET L=BPTR
        CALL    ZCHK            ;CHECK FOR ZERO
        MOV     L,E             ;RESTORE L=LPTR
        RET                     ;RETURN
;
;
;   SUBROUTINE DLST
;
;       SHIFTS DBL WORD ONE PLACE LF
DLST:   INR     L
        INR     L               ;/***TP
        MOV     A,M             ;LOAD IT
        ORA     A               ;KILL CARRY
        RAL                     ;SHIFT IT LEFT
        MOV     M,A             ;STORE IT
        DCR     L
        MOV     A,M             ;LOAD IT
        RAL                     ;SHIFT IT LEFT
;       IF CARRY SET BY FIRST SHIFT
;       IT WILL BE IN LSB OF SECOND
        MOV     M,A
        DCR     L               ;/***TP EXTENSION
        MOV     A,M
        RAL
        MOV     M,A             ;/***ALL DONE TP
        RET
;
;
;   SUBROUTINE DRST
;
;       SHIFTS DOUBLE WORD ONE PLACE
;       TO THE RIGHT
;           DOES NOT AFFECT D
;
DRST:   MOV     E,L             ;/***TP MODIFIED RIGHT SHIFT TP
        MOV     A,M             ;LOAD FIRST WORD
        RAR                     ;ROTATE IT RIGHT
        MOV     M,A             ;STORE IT
        INR     L               ;/*** TP
        MOV     A,M             ;LOAD SECOND WORD
        RAR                     ;SHIFT IT RIGHT
        MOV     M,A             ;STORE IT
        INR     L               ;/*** TP EXTENSION
        MOV     A,M
        RAR
        MOV     M,A
        MOV     L,E             ;/***TP - ALL DONE TP
        RET
;
;
;   SUBROUTINE DADD
;
;       ADDS TWO DOUBLE PRECISION
;       WORDS, C=1 IF THERE IS OVRFLW
;
DADD:   MOV     E,L             ;SAVE BASE IN E
        MOV     L,B             ;BASE \3 TO L
        INR     L               ;BASE \4 TO L
        INR     L               ;/***TP
        MOV     A,M             ;LOAD S MANTB
        MOV     L,E             ;BASE TO L
        INR     L               ;BASE \1 TO L
        INR     L               ;/***TP
        ADD     M               ;ADD TWO MANTB]S
        MOV     M,A             ;STORE ANSWER
        MOV     L,B             ;/***TP EXTENSION
        INR     L
        MOV     A,M
        MOV     L,E
        INR     L
        ADC     M
        MOV     M,A             ;/***TP - ALL DONE
        MOV     L,B             ;BASE \3 TO L
        MOV     A,M             ;MANTA OF S TO A
        MOV     L,E             ;BASE TO L
        ADC     M               ;ADD WITH CARRY
        MOV     M,A             ;STORE ANSWER
        RET
;
;
;   SUBROUTINE DCLR
;
;       CLEARS TWO SUCCESSIVE
;       LOCATIONS OF MEMORY
;
DCLR:   XRA     A
        MOV     M,A
        INR     L
        MOV     M,A
        INR     L               ;/***TP EXTENSION
        MOV     M,A             ;/***TP ZERO 3
        DCR     L               ;/***TP - ALL DONE
        DCR     L
        RET
;
;
;       /*****ALL NEW DSUB - SHORTER***
;
;   SUBROUTINE DSUB
;
;       DOUBLE PRECISION SUBTRACT
;
DSUB:   MOV     E,L             ;SAVE BASE IN E
        INR     L               ;/***TP EXTENSION
        INR     L               ;/START WITH LOWS
        MOV     A,M             ;/GET ARG
        MOV     L,B             ;/NOW SET UP TO SUB
        INR     L
        INR     L
        SUB     M               ;/NOW DO IT
        MOV     L,E             ;/NOW MUST PUT IT BACK
        INR     L
        INR     L
        MOV     M,A             ;/PUT BACK
        DCR     L               ;/***TP - ALL DONE
        MOV     A,M             ;/GET LOW OF LOP
        MOV     L,B             ;/SET TO BOP
        INR     L               ;/SET TO BOP LOW
        SBB     M               ;/GET DIFF. OF LOWS
        MOV     L,E             ;/SAVE IN LOP LOW
        INR     L               ;/TO LOP LOW
        MOV     M,A             ;/INTO RAM
        DCR     L               ;/BACK UP TO LOP HIGH
        MOV     A,M             ;/GET LOP HIGH
        MOV     L,B             ;/SET TO BOP HIGH
        SBB     M               ;/SUB. WITH CARRY
        MOV     L,E             ;/SAVE IN LOP HIGH
        MOV     M,A             ;/INTO RAM
        RET                     ;/ALL DONE - MUCH SHORTER
;
;   SUBROUTINE GCHAR
;
;       THIS SUBROUTINE RETURNS THE CHARACTERISTIC OF
;       THE FLOATING POINT NUMBER POINTED TO BY (H,L)
;       IN THE A REGISTER WITH ITS SIGN EXTENDED INTO THE
;       LEFTMOST BIT.
;
;   REGISTERS ON EXIT:
;
;       A = CHARACTERISTIC OF (H,L) WITH SIGN EXTENDED
;       L = (ORIGINAL L) + 3
;       B,C,D,E,H = SAME AS ON ENTRY
;
GCHAR:  INR     L               ;MAKE (H,L) POINT TO CHAR
        INR     L               ;MAKE (H,L) POINT TO CHAR
        INR     L               ;MAKE (H,L) POINT TO CHAR
        MOV     A,M             ;SET A=CHAR + MANTISSA SIGN
        ANI     177Q            ;GET RID OF MANTISSA SIGN BIT
        ADI     100Q            ;PROPAGATE CHAR SIGN INTO LEFTMOST BIT
        XRI     100Q            ;RESTORE ORIGINAL CHAR SIGN BIT
        RET                     ;RETURN WITH (H,L) POINTING TO THE
                                ;CHAR = ORIGINAL (H,L)+3
                                ;SOMEONE ELSE WILL CLEAN UP
;
;
;   SUBROUTINE CFCHE
;
;       THIS SUBROUTINE RETURNS THE CHARACTERISTICS OF THE
;       FLOATING POINT NUMBERS POINTED TO BY (H,L) AND
;       (H,B) IN THE A AND E REGISTERS RESPECTIVELY,
;       WITH THEIR SIGNS EXTENDED INTO THE LEFTMOST BIT.
;
;   REGISTERS ON EXIT:
;
;       A = CHARACTERISTIC OF (H,L) WITH SIGN EXTENDED
;       E = CHARACTERISTIC OF (H,B) WITH SIGN EXTENDED
;       B,C,H,L = SAME AS ON ENTRY
;       D = A
;
CFCHE:  MOV     E,L             ;SAVE LPTR IN E
        MOV     L,B             ;SET L = BPTR
        CALL    GCHAR           ;GET CHAR(H,B) WITH SIGN EXTENDED IN A
        MOV     L,E             ;RESTORE L = LPTR
        MOV     E,A             ;SET E=CHAR(H,B) WITH SIGN EXTENDED
        CALL    GCHAR           ;SET A=CHAR(H,L) WITH SIGN EXTENDED
        DCR     L               ;RESTORE L = LPTR
        DCR     L               ;RESTORE L = LPTR
        DCR     L               ;RESTORE L = LPTR
        MOV     D,A             ;SET D=A=CHAR(H,L) WITH SIGN EXTENDED
        RET
;
;
;   SUBROUTINE CCMP
;
;       THIS SUBROUTINE COMPARES THE CHARACTERISTICS OF
;       FLOATING POINT NUMBERS POINTED TO BY (H,L) AND (H,B).
;       THE ZERO FLIP-FLOP IS SET IF CHAR(H,L) EQUALS
;       CHAR(H,B).  IF CHAR(H,L) IS LESS THAN CHAR(H,B) THEN
;       THE CARRY BIT WILL BE SET.
;
;   REGISTERS ON EXIT:
;
;       A = CHARACTERISTIC OF (H,L) WITH SIGN EXTENDED
;       E = CHARACTERISTIC OF (H,B) WITH SIGN EXTENDED
;       D = A
;       B,C,H,L = SAME AS ON ENTRY
;
CCMP:   CALL    CFCHE           ;FETCH CHARACTERISTICS WITH SIGN EXTENDED
                                ;INTO A (CHAR(H,L)) AND E (CHAR(H,B)) REGISTERS
        MOV     D,A             ;SAVE CHAR (H,L)
        SUB     E               ;SUBTRACT E (CHAR(H,B))
        RAL                     ;ROTATE SIGN BIT INTO CARRY BIT
        MOV     A,D             ;RESTORE A=CHAR(H,L)
        RET                     ;RETURN
;
;   ERROR RETURNS
;
;       THE FOLLOWING CODE IS USED TO RETURN VARIOUS
;       ERROR CONDITIONS.  IN EACH CASE A FLOATING POINT
;       NUMBER IS STORED IN  THE 4 WORDS POINTED TO BY (H,L)
;       AND A FLAG IS STORED IN THE ACCUMULATOR.
;
;       CONDITION   FLAG   RESULT (+)        RESULT (-)
;
;       UNDERFLOW    377   000 000 000 100   000 000 000 300
;       OVERFLOW     177   377 377 377 077   377 377 377 277
;       INDEFINITE   077   377 377 377 077   377 377 377 277
;       NORMAL       000   XXX XXX XXX XXX   XXX XXX XXX XXX
;       NORMAL ZERO  000   000 000 000 100   (ALWAYS RETURNS +0)
;
;   ENTRY POINTS:
;
;       WUND - WRITE UNDERFLOW
;       WOVR - WRITE OVERFLOW
;       WIND - WRITE INDEFINITE
;       WZER - WRITE NORMAL ZERO
;
;###S
;WFLT     MACRO   VMANT,VCHAR,VFLAG,LABEL  ;WRITE FLOATING NUMBER
;
;         MVI     D,VCHAR     ;LOAD CHARACTERISTIC INTO D REGISTER
;         CALL    WCHAR       ;WRITE CHARACTERISTIC
;LABEL::  MVI     A,VMANT     ;LOAD MANTISSA VALUE
;                             ;WE ASSUME HERE THAT ALL BYTES OF MANTISSA
;                             ;ARE THE SAME
;         CALL    WMANT       ;WRITE THE MANTISSA
;         MVI     A,VFLAG     ;SET ACCUMULATOR TO FLAG
;         ORA     A           ;SET FLAGS PROPERLY
;         RET                 ;RETURN (WMANT RESTORED (H,L))
;         ENDM
;
;WUND:   WFLT    0,100Q,377Q,UFLW1  ;WRITE UNDERFLOW
WUND:   MVI     D,100Q          ;LOAD CHARACTERISTIC INTO D REGISTER
        CALL    WCHAR           ;WRITE CHARACTERISTIC
UFLW1:  MVI     A,0             ;LOAD MANTISSA VALUE
                                ;WE ASSUME HERE THAT ALL BYTES OF MANTISSA
                                ;ARE THE SAME
        CALL    WMANT           ;WRITE THE MANTISSA
        MVI     A,377Q          ;SET ACCUMULATOR TO FLAG
        ORA     A               ;SET FLAGS PROPERLY
        RET                     ;RETURN (WMANT RESTORED (H,L))
;WOVR:   WFLT    377Q,77Q,177Q,OFLW1  ;WRITE OVERFLOW
WOVR:   MVI     D,77Q           ;LOAD CHARACTERISTIC INTO D REGISTER
        CALL    WCHAR           ;WRITE CHARACTERISTIC
OFLW1:  MVI     A,377Q          ;LOAD MANTISSA VALUE
                                ;WE ASSUME HERE THAT ALL BYTES OF MANTISSA
                                ;ARE THE SAME
        CALL    WMANT           ;WRITE THE MANTISSA
        MVI     A,177Q          ;SET ACCUMULATOR TO FLAG
        ORA     A               ;SET FLAGS PROPERLY
        RET                     ;RETURN (WMANT RESTORED (H,L))
;WIND:   WFLT    377Q,77Q,77Q,INDF1  ;WRITE INDEFINITE
WIND:   MVI     D,77Q           ;LOAD CHARACTERISTIC INTO D REGISTER
        CALL    WCHAR           ;WRITE CHARACTERISTIC
INDF1:  MVI     A,377Q          ;LOAD MANTISSA VALUE
                                ;WE ASSUME HERE THAT ALL BYTES OF MANTISSA
                                ;ARE THE SAME
        CALL    WMANT           ;WRITE THE MANTISSA
        MVI     A,77Q           ;SET ACCUMULATOR TO FLAG
        ORA     A               ;SET FLAGS PROPERLY
        RET                     ;RETURN (WMANT RESTORED (H,L))
;###E
;
WZER:   INR     L               ;WRITE NORMAL ZERO
        INR     L               ;
        INR     L               ;
        MVI     M,100Q          ;STORE CHARACTERISTIC FOR ZERO
        XRA     A               ;ZERO ACCUMULATOR
        CALL    WMANT           ;STORE ZERO MANTISSA
        ORA     A               ;SET FLAGS PROPERLY
        RET                     ;RETURN
;
; ROUTINE TO WRITE MANTISSA FOR ERROR RETURNS
;
WMANT:  DCR     L               ;POINT LEAST SIGNIFICANT BYTE
                                ;OF MANTISSA
        MOV     M,A             ;STORE LSBYTE OF MANTISSA
        DCR     L               ;POINT TO NEXT LEAST SIGNIFICANT BYTE
                                ;OF MANTISSA
        MOV     M,A             ;STORE NLSBYTE OF MANTISSA
        DCR     L               ;POINT TO MOST SIGNIFICANT BYTE
                                ;OF MANTISSA
        MOV     M,A             ;STORE MSBYTE OF MANTISSA
        RET                     ;RETURN (H,L) POINTS TO BEGINNING OF
                                ;FLOATING POINT RESULT
;
; ROUTINE TO WRITE CHARACTERISTIC FOR ERROR RETURNS
; NOTE:  WE PRESERVE ORIGINAL MANTISSA SIGN
; ON ENTRY D CONTAINS NEW CHARACTERISTIC TO BE STORED.
;
WCHAR:  INR     L               ;SET (H,L) TO POINT TO CHARACTERISTIC
        INR     L               ;PART OF ABOVE
        INR     L               ;PART OF ABOVE
        MOV     A,M             ;LOAD CHARACTERISTIC A
                                ;AND MANTISSA SIGN
        ANI     200Q            ;JUST KEEP MANTISSA SIGN
        ORA     D               ;OR IN NEW CHARACTERISTIC
        MOV     M,A             ;STORE IT BACK
        RET                     ;RETURN WITH (H,L) POINT TO CHARACTERISTIC
                                ;OF RESULT
                                ;SOMEONE ELSE WILL FIX UP (H,L)
;
;   SUBROUTINE INDFC
;
;       THIS ROUTINE WRITES A FLOATING INDEFINITE, SETS
;       THIS WRITES WRITES A FLOATING POINT INDEFINITE
;       AT (H,C), SETS THE CONDITION FLAG AND RETURNS
;
;
INDFC:  MOV     E,L             ;SAVE LPTR IN E
        MOV     L,C             ;SET L=CPTR SO (H,L)-ADDR OF RESULT
        CALL    WIND            ;WRITE INDEFINITE
        MOV     L,E             ;RESTORE L=LPTR
        RET                     ;RETURN
;
;
;   SUBROUTINE WZERC
;
;       THIS ROUTINE WRITES A NORMAL FLOATING POINT ZERO
;       AT (H,C), SETS THE CONDITION FLAG AND RETURNS
;
WZERC:  MOV     E,L             ;SAVE LPTR IN E
        MOV     L,C             ;SETL=CPTR SO (H,L)=ADDR OF RESULT
        CALL    WZER            ;WRITE NORMAL ZERO
        MOV     L,E             ;RESTORE L=LPTR
        RET                     ;RETURN
;
;   SUBROUTINE INCR
;
;       THIS SUBROUTINE INCREMENTS THE CHARACTERISTIC
;       OF THE FLOATING POINT NUMBER POINTED TO BY (H,L).
;       WE TEST FOR OVERFLOW AND SET APPROPRIATE FLAG.
;       (SEE ERROR RETURNS).
;
;   REGISTERS ON EXIT:
;
;        A = CONDITION FLAG (SEE ERROR RETURNS)
;        D = CLOBBERED
;        B,C,H,L = SAME AS ON ENTRY
;
INCR:   CALL    GCHAR           ;GET CHAR WITH SIGN EXTENDED
        CPI     MAXCH           ;COMPARE WITH MAX CHAR PERMITTED
        JZ      OFLW1           ;INCREMENT WOULD CAUSE OVERFLOW
        MOV     D,A             ;SAVE IT IN D
        INR     D               ;INCREMENT IT
        JMP     INCR2           ;JUMP AROUND ALTERNATE ENTRY POINT
INCR3:  INR     L               ;COME HERE TO STORE CHARACTERISTIC
        INR     L               ;POINT (H,L) TO CHAR
        INR     L               ;POINT (H,L) TO CHAR
INCR2:  MVI     A,177Q
        ANA     D               ;KILL SIGN BIT
        MOV     D,A             ;BACK TO D
        MOV     A,M             ;NOW SIGN IT
        ANI     200Q            ;GET MANTISSA SIGN
        ORA     D               ;PUT TOGETHER
        MOV     M,A             ;STORE IT BACK
        DCR     L               ;NOW BACK TO BASE
        DCR     L               ;/***TP
        DCR     L
SCCFG:  XRA    A                ;SET SUCCESS FLAG
        RET
;
;   SUBROUTINE DECR
;
;       THIS SUBROUTINE DECREMENTS THE CHARACTERISTIC
;       OF THE FLOATING POINT NUMBER POINTED TO BY (H,L).
;       WE TEST FOR UNDERFLOW AND SET APPROPRIATE FLAG.
;       (SEE ERROR RETURNS).
;
;   REGISTERS ON EXIT:
;
;        A = CONDITION FLAG (SEE ERROR RETURNS)
;        D = CLOBBERED
;        B,C,H,L = SAME AS ON ENTRY
;
DECR:   CALL    GCHAR           ;GET CHAR WITH SIGN EXTENDED
        CPI     MINCH           ;COMPARE WITH MIN CHAR PERMITTED
        JZ      UFLW1           ;DECREMENT WOULD CAUSE UNDERFLOW
        MOV     D,A             ;SAVE CHARACTERISTIC IN D
        DCR     D               ;DECREMENT CHARACTERISTIC
        JMP     INCR2           ;GO STORE IT BACK
;
;   SUBROUTINE AORS
;
;       RETURN S=1 IF BASE 6
;       HAS A 1 IN MSB
;
AORS:   MOV     E,L             ;SAVE BASE
        MOV     L,C             ;BASE 6 TO L
        MOV     A,M             ;LOAD IT
        ORA     A               ;SET FLAGS
        MOV     L,E             ;RESTORE BASE
        RET
;
;
;   SUBROUTINE TSTR
;
;       CHECKS C PTR TO SEE IF
;       NLSB !
;       RETURNS Z=1 IF NOT
;       DESTROYS E,D
;
TSTR:   MOV     E,L             ;SAVE BASE
        MOV     L,C             ;C PTR TO L
        MVI     D,2             ;MASK TO D
        MOV     A,M             ;LOAD VALUE
        MOV     L,E             ;RESTORE BASE
        ANA     D               ;AND VALUE WITH MASK
        RET
;
;
;   SUBROUTINE ACPR
;
;       STORES A IN LOCATION OF CPTR
;       LPTR IN E
;
ACPR:   MOV     E,L             ;SAVE LPTR
        MOV     L,C             ;CPTR TO L
        MOV     M,A             ;STORE A
        MOV     L,E             ;RESTORE BASE
        RET
;
;
;   SUBROUTINE DCMP
;
;       COMPARES TWO DOUBLE LENGTH
;       WORDS
;
DCMP:   MOV     A,M             ;NUM MANTA TO A
        MOV     E,L             ;SAVE BASE IN E
        MOV     L,B             ;BASE 3 TO L
        CMP     M               ;COMPARE WITH DEN MANTA
        MOV     L,E             ;RETURN BASE TO L
        RNZ                     ;RETURN IF NOT THE SAME
        INR     L               ;L TO NUM MANTB
        MOV     A,M             ;LOAD IT
        MOV     L,B             ;DEN MANTB ADD TO L
        INR     L               ;BASE 4 TO L
        CMP     M
        MOV     L,E
        RNZ                     ;/***TP EXTENSION
        INR     L               ;NOW CHECK BYTE 3
        INR     L
        MOV     A,M             ;GET FOR COMPARE
        MOV     L,B
        INR     L
        INR     L               ;BYTE 3 NOW
        CMP     M               ;COMPARE
        MOV     L,E             ;/***TP - ALL DONE
        RET
;
;
;   SUBROUTINE DIVC
;
;       PERFORMS ONE CYCLE OF DOUBLE
;       PRECISION FLOATING PT DIVIDE
;       ENTER AT ENT1 ON FIRST CYCLE
;       ENTER AT ENT2 ALL THEREAFTER
;
ENT2:   CALL    DLST            ;SHIFT MOVING DIVIDEND
        JC      OVER            ;IF CARRY=1,NUM.GT.D
ENT1:   CALL    DCMP            ;COMPARE NUM WITH DEN
        JNC     OVER            ;IF CARRY NOT SET,NUM.GE.DEN
        RET
OVER:   CALL    DSUB            ;CALL DOUBLE SUBTRACT
        MOV     E,L             ;SAVE BASE IN E
        MOV     L,C             ;BASE 6 TO L
        INR     L               ;BASE 7 TO L
        INR     L               ;/***TP
        MOV     A,M
        ADI     1               ;ADD 1
        MOV     M,A             ;PUT IT BACK
        MOV     L,E             ;RESTORE BASE TO L
        RET
;
;
;   SUBROUTINE LXFR
;
;       MOVES CPTR TO EPTR
;       MOVES 3 WORDS IF ENTER AT LXFR
;
LXFR:   MVI     D,4             ;MOVE 4 WORDS
REP5:   MOV     L,C             ;CPTR TO L
        MOV     A,M             ;CPTR> TO A
        MOV     L,E             ;EPTR TO L
        MOV     M,A
        INR     C               ;INCREMENT C
        INR     E               ;INCREMENT E TO NEXT
        DCR     D               ;TEST FOR DONE
        JNZ     REP5            ;GO FOR FOR TILL D=0
        MOV     A,E             ;NOW RESET C AND E
        SUI     4               ;RESET BACK BY 4
        MOV     E,A             ;PUT BACK IN E
        MOV     A,C             ;NOW RESET C
        SUI     4               ;BY 4
        MOV     C,A             ;BACK TO C
        RET                     ;DONE
;
;   SUBROUTINE LDCP
;
;       THIS SUBROUTINE COMPUTES THE CHARACTERISTIC
;       FOR THE FLOATING DIVIDE ROUTINE
;
;   REGISTERS ON EXIT:
;
;       A = CONDITION FLAG (SEE ERROR RETURNS)
;       D,E = GARBAGE
;       B,C,H,L = SAME AS ON ENTRY
;
;   REGISTERS ON ENTRY:
;
;       (H,B) = ADDRESS OFF DIVISOR
;       (H,C) = ADDRESS OF QUOTIENT
;       (H,L) = ADDRESS OF DIVIDEND
;
LDCP:   CALL    CFCHE           ;SET E=CHAR(H,B), A=CHAR(H,L)
        SUB     E               ;SUBTRACT TO GET NEW CHARACTERISTIC
        JMP     CCHK            ;GO CHECK FOR OVER/UNDERFLOW
                                ;AND STORE CHARACTERISTIC
;
;
;   SUBROUTINE LMCP
;
;       THIS SUBROUTINE COMPUTES THE CHARACTERISTIC
;       FOR THE FLOATING MULTIPLY ROUTINE.
;
;   REGISTERS ON EXIT:
;
;       A = CONDITION FLAG (SEE ERROR RETURNS)
;       D,E = GARBAGE
;       B,C,H,L = SAME AS ON ENTRY
;
;   REGISTERS ON ENTRY:
;
;       (H,B) = ADDRESS OFF MULTIPLICAND
;       (H,C) = ADDRESS OF PRODUCT
;       (H,L) = ADDRESS OF MULTIPLIER
;
LMCP:   CALL    CFCHE           ;SET E=CHAR(H,B), A=CHAR(H,L)
        ADD     E               ;ADD TO GET NEW CHARACTERISTIC
                                ;NOW FALL INTO THE ROUTINE
                                ;WHICH CHECKS FOR OVER/UNDERFLOW
                                ;AND STORE CHARACTERISTIC
;
;
;   SUBROUTINE CCHK
;
;       THIS SUBROUTINE CHECKS A CHARACTERISTIC IN
;       THE ACCUMULATOR FOR OVERFLOW OR UNDERFLOW.
;       IT THEN STORES THE CHARACTERISTIC, PRESERVING
;       THE PREVIOUSLY COMPUTED MANTISSA SIGN.
;
;  REGISTERS ON ENTRY:
;
;       (H,L) = ADDRESS OF ONE OPERAND
;       (H,B) = ADDRESS OF OTHER OPERAND
;       (H,C) = ADDRESS OF RESULT
;       A     = NEW CHARACTERISTIC OF  RESULT
;
;   REGISTERS ON EXIT:
;
;       A = CONDITION FLAG (SEE ERROR RETURNS)
;       D,E = GARBAGE
;       B,C,H,L = SAME AS ON ENTRY
;
CCHK:                           ;ENTER HERE TO CHECK CHARACTERISTIC
        CPI     100Q            ;CHECK FOR 0 TO +63
        JC      STORC           ;JUMP IF OKAY
        CPI     200Q            ;CHECK FOR +64 TO +127
        JC      OFLWC           ;JUMP IF OVERFLOW
        CPI     300Q            ;CHECK FOR -128 TO -65
        JC      UFLWC           ;JUMP IF UNDERFLOW
STORC:  MOV     E,L             ;SAVE L IN E
        MOV     L,C             ;LET L POINT TO RESULT
        MOV     D,A             ;SAVE CHARACTERISTIC IN D
        CALL    INCR3           ;STORE CHARACTERISTIC
        MOV     L,E             ;RESTORE L
        RET                     ;RETURN
;
;   SUBROUTINE OFLWC
;
;       THIS ROUTINE WRITES A FLOATING POINT OVERFLOW AT (H,C)
;       SETS THE CONDITION FLAG, AND RETURNS.
;
OFLWC:  MOV     E,L             ;SAVE L IN E
        MOV     L,C             ;SET L=CPTR, SO (H,L)=ADDR OF RESULT
        CALL    WOVR            ;WRITE OUT OVERFLOW
        MOV     L,E             ;RESTORE L
        RET                     ;RETURN
;
;   SUBROUTINE UFLWC
;
;       THIS ROUTINE WRITES A FLOATING POINT UNDERFLOW AT (H,C)
;       SETS THE CONDITION FLAG, AND RETURNS.
;
UFLWC:  MOV     E,L             ;SAVE L IN E
        MOV     L,C             ;SET L=CPTR, SO (H,L)=ADDR OF RESULT
        CALL    WUND            ;WRITE OUT UNDERFLOW
        MOV     L,E             ;RESTORE L
        RET                     ;RETURN
;
;
;   SUBROUTINE CSIGN
;
;       THIS SUBROUTINE COMPUTES AND STORE THE MANTISSA
;       SIGN FOR THE FLOATING MULTIPLY AND DIVIDE ROUTINES
;
;   REGISTERS ON ENTRY:
;
;       (H,L) = ADDRESS OF ONE OPERAND
;       (H,B) = ADDRESS OF OTHER OPERAND
;       (H,C) = ADDRESS OF RESULT
;
;   REGISTERS ON EXIT:
;
;       A,D,E = GARBAGE
;       B,C,H,L = SAME AS ON ENTRY
;
;
CSIGN:  CALL    MSFH            ;SET A=SIGN(H,L), E=SIGN(H,B)
        XRA     E               ;EXCLUSIVE OR SIGNS TO GET NEW SIGN
        CALL    CSTR            ;STORE SIGN INTO RESULT
        RET                     ;RETURN
;
;
;   SUBROUTINE CSTR
;
;       STORES VALUE IN A IN
;       CPTR 2
;       PUTS LPTR IN E
;
CSTR:   MOV     E,L             ;SAVE LPTR IN E
        MOV     L,C             ;CPTR TO L
        INR     L               ;CPTR\2
        INR     L               ;TO L
        INR     L               ;/***TP
        MOV     M,A             ;STORE ANSWER
        MOV     L,E             ;LPTR BACK TO L
        RET
;
;   SUBROUTINE MSFH
;
;       THIS SUBROUTINE FETCHES THE SIGNS OF THE MANTISSAS
;       OF THE FLOATING POINT NUMBERS POINTED TO BY (H,L)
;       AND (H,B) INTO THE A AND E REGISTERS RESPECTIVELY.
;
;   REGISTERS ON EXIT:
;
;       A = SIGN  OF MANTISSA OF (H,L)
;       E = SIGN OF MANTISSA OF (H,B)
;       B,C,D,H,L = SAME AS ON ENTRY
;
MSFH:   MOV     E,L             ;SAVE LPTR
        MOV     L,B             ;BPTR TO L
        INR     L               ;BPTR\2
        INR     L               ;/***TP
        INR     L               ;TO L
        MOV     A,M             ;BPTR 2>TO A
        ANI     128             ;SAVE MANT SIGN
        MOV     L,E             ;LPTR BACK TO L
        MOV     E,A             ;STORE BPTR MANT SIGN
        INR     L               ;LPTR\2
        INR     L               ;/***TP
        INR     L               ;TO L
        MOV     A,M             ;LPTR\2>TO A
        ANI     128             ;SAVE LPTR MANT SIGN
        DCR     L               ;LPTR BACK
        DCR     L               ;TO L
        DCR     L               ;/***TP
        RET
;
;
;   SUBROUTINE BCTL
;
;           MOVES BPTR CHAR TO LPTR CHAR
;           DESTROYS E
;
BCTL:   MOV     E,L             ;LPTR TO E
        MOV     L,B             ;BPTR TO L
        INR     L               ;BPTR 2
        INR     L               ;/***TP
        INR     L               ;TO L
        MOV     A,M             ;BPTR CHAR TO A
        MOV     L,E             ;LPTR TO L
        INR     L               ;LPTR 2
        INR     L               ;TO L
        INR     L               ;/***TP
        MOV     M,A             ;STORE BPTR CHAR IN LPTR CHAR
        MOV     L,E             ;LPTR TO L
        RET
;
;HRJ for some reason the square root routine was not included
;
;
;       SUBROUTINE DSQRT
;
;       THE L REG PTS TO THE    TO BE
;       OPERATED ON.
;       THE B REG PTS TO THE LOC WHERE
;       THE RESULT IS TO BE STORED
;       THE C REG PTS TO 17(10) SCRATCH 
;       AREA.
;       WHERE:
;       C = ITERATION COUNT
;       C+1 = L REG
;       C+2 = B REG
;       C+3 TO C+6 = INTRL REG 1
;       C+7 TO C+10 = INTRL REG 2
;       C+11 TO C+14 = INTRL REG 3
;       C+15 = 
;
DSQRT:	MOV     A,L	        ;STORE L IN
	MOV     L,C	        ;2ND WRD SCRATCH
	MVI     M,0             ;INITIALIZE ITER COUNT
	INR     L
	MOV     M,A
	INR     L               ;STR B IN 3RD
	MOV     M,B             ;WRD OF SCRATCH
	INR     L               ;SET C TO INTRL
	MOV     C,L             ;REG I
	MOV     L,A             ; SET L PRT AT
	MOV     A,H             ;SET REGS FOR COPY
	CALL    COPY		; COPY TC INTRL REG1
	CALL    GCHR		;PUT CHR IN A
	MOV     B,A	        ;MAKE COPY
	ANI     200Q		;OK NEG
	JNZ     ERSQ
	MOV     A,B
	ANI     100Q		;OK NEG EXP
	MOV     A,B
	JZ      EPOS
	RAR			;DIV BY 2
	ANI     177Q
	ORI     100Q		;SET SIGN BIT
	MOV     M,A	        ;SAVE 1ST APPROX
	JMP     AGN4
EPOS:	RAR			;DIV BY 2
	ANI     177Q
	MOV     M,A		;SAVE 1ST APPROX
AGN4:	MOV     L,C		;SET REGS
	MOV     A,C		;TO COPY 1ST
	ADI     4		;APPROX
	MOV     C,A		;INTO INTRL REG 2
	MOV     A,H		;FRM INTRL REG1
	CALL    COPY
	MOV     A,C
	SUI     4		;MULTIPLY INTRL REG 1
	MOV     L,A
	MOV     B,C		;TIME INTRL REG2
	ADI     10Q		;PLACE RESULT IN
	MOV     C,A		;INMTRL REG 3
	CALL    LMUL
	MOV     A,C
	SUI     10Q		;COPY .ORG INTO
	MOV     C,A		;INTRL REG 1
	SUI     2
	MOV     L,A
	MOV     L,M
	MOV     A,H
	CALL    COPY
	MOV     A,C
	ADI     10Q		;ADD INTRL
	MOV     L,A		;REG3 OT
	MOV     B,C		;INTRL REG1
	ADI     4		;ANS TO INTRL
	MOV     C,A		;REG3
	CALL    LADD
	MOV     A,L
	SUI     4		;DIV INTRL REG 3
	MOV     B,A		;BY INTRL REG 2
	SUI     4		;PUT ANSR IN INTRL
	MOV     C,A		;REG1
	CALL    LDIV
	CALL    GCHR
	SUI     1
	ANI     177Q
	MOV     M,A
	MOV     A,C
	SUI     3		;C PTS TO INTRL REG 1
	MOV     L,A		;GET INTR
	MOV     B,M		;COUNT NOW INCR
	INR     B
	MOV     M,B
	MOV     A,B
	CPI     5		;IF = 5 RTN ANS
	JNZ     AGN4		;OTHERWISE CONT
	MOV     L,C
ALDN:	DCR     L		;COPY ANS INTO
	MOV     C,M		;LOC REQUESTED
	INR     L
	MOV     A,H
	CALL    COPY
	RET
ERSQ:	MOV     L,C
	CALL    WZER		;WRITE A FLOATING ZERO
	JMP     ALDN
;                        ; C+1 = L REG
;
;******************************************************
;       //// 5 DIGIT FLOATING PT. OUTPUT
;******************************************************
;
;
;       ROUTINE TO CONVERT FLOATING PT.
;       NUMBERS TO ASCII AND OUTPUT THEM VIA A SUBROUTINE
;       CALLED OUTR  -  NOTE: THIS IS CURRENTLY SET
;       TO ODT'S OUTPUT ROUTINE
;
CVRT:   CALL    ZCHK            ;CHECK FOR NEW ZERO
        JNZ     NNZRO           ;NOT ZERO
        INR     C               ;IT WAS, OFFSET C BY 2
        INR     C
        MOV     L,C
        CALL    WZER            ;WRITE ZERO
        CALL    SIGN            ;SEND SPACE ON POS ZERO [HRJ: was missing]
        INR     L               ;PNT TO DECIMAL EXPONENT
        INR     L
        INR     L
        INR     L
        XRA     A               ;SET IT TO ZERO
        MOV     M,A
        JMP     MDSKP           ;OUTPUT IT
NNZRO:  MOV     D,M             ;GET THE NUMBER TO CONVERT
        INR     L
        MOV     B,M
        INR     L
        MOV     E,M
        INR     L               ;4 WORD***TP
        MOV     A,M             ;/***TP
        INR     C               ;OFFSET SCRATCH POINTER BY 2
        INR     C
        MOV     L,C             ;L NOT NEEDED ANY MORE
        MOV     M,D             ;SAVE NUMBER IN SCRATCH
        INR     L
        MOV     M,B
        INR     L
        MOV     M,E             ;/***TP
        INR     L               ;/***TP
        MOV     B,A             ;SAVE COPY OF CHAR & SIGN
        ANI     177Q            ;GET ONLY CHAR.
        MOV     M,A             ;SAVE ABS(NUMBER)
        CPI     100Q            ;CK FOR ZERO
        JZ      NZRO
        SUI     1               ;GET SIGN OF DEC. EXP
        ANI     100Q            ;GET SIGN OF CHAR.
NZRO:   RLC                     ;MOVE IT TO SIGN POSITION
        INR     L               ;MOVE TO DECIMAL EXP.
        MOV     M,A             ;SAVE SIGN OF EXP.
        MOV     A,B             ;GET MANT. SIGH BACK
        CALL    SIGN            ;OUTPUT SIGN
        MVI     L,TEN5 & 377Q   ;TRY MULT. OR DIV. BY 100000 FIRST
        CALL    COPT            ;MAKE A COPY IN RAM
TST8:   CALL    GCHR            ;GET CHAR. OF NUMBER
        MOV     B,A             ;SAVE A COPY
        ANI     100Q            ;GET ABSOLUTE VALUE OF CHAR
        MOV     A,B             ;IN CASE PLUS
        JZ      GOTV            ;ALREADY PLUS
        MVI     A,200Q          ;MAKE MINUS INTO PLUS
        SUB     B               ;PLUS=200B-CHAR
GOTV:   CPI     22Q             ;TEST FOR USE OF 100000
        JM      TRY1            ;WONT GO
        CALL    MORD            ;WILL GO SO DO IT
        ADI     5               ;INCREMENT DEC. EXPONENT BY 5
        MOV     M,A             ;UPDATE MEM
        JMP     TST8            ;GO TRY AGAIN
TRY1:   MVI     L,TEN & 377Q   ;NOW USE JUST TEN
        CALL    COPT            ;PUT IT IN RAM
TST1:   CALL    GCHR            ;GET CHARACTERISTIC
        CPI     1               ;MUST GET IN RANGE 1 TO 6
        JP      OK1             ;AT LEAST ITS 1 OR BIGGER
MDGN:   CALL    MORD            ;MUST MUL OF DIV BY 10
        ADI     1               ;INCREMENT DECIMAL EXP.
        MOV     M,A             ;UPDATE MEM
        JMP     TST1            ;NOW TRY AGAIN
OK1:    CPI     7               ;TEST FOR LESS THAN 7
        JP      MDGN            ;NOPE - 7 OR GREATER
MDSKP:  MOV     L,C             ;SET UP DIGIT COUNT
        DCR     L
        DCR     L               ;IN 1ST WORD OF SCRATCH
        MVI     M,5             ;5 DIGITS
        MOV     E,A             ;SAVE CHAR. AS LEFT SHIFT COUNT
        CALL    LSFT            ;SHIFT LEFT PROPER NUMBER
        CPI     12Q             ;TEST FOR 2 DIGITS HERE
        JP      TWOD            ;JMP IF 2 DIGITS TO OUTPUT
        CALL    DIGO            ;OUTPUT FIRST DIGIT
POPD:   CALL    MULTT           ;MULTIPLY THE NUMBER BY 10
INPOP:  CALL    DIGO            ;PRINT DIGIT IN A
        JNZ     POPD            ;MORE DIGITS?
        MVI     A,305Q          ;NO SO PRINT E
        CALL    OUTR            ;BASIC CALL TO OUTPUT
        CALL    GETEX           ;GET DECIMAL EXP
        MOV     B,A             ;SAVE A COPY
        CALL    SIGN            ;OUTPUT SIGN
        MOV     A,B             ;GET EXP BACK
        ANI     77Q             ;GET GOOD BITS
        CALL    CTWO            ;GO CONVERT 2 DIGITS
DIGO:   ADI     260Q            ;MAKE A INTO ASCII
        CALL    OUTR            ;OUTPUT DIGIT
        MOV     L,C             ;GET DIGIT COUNT
        DCR     L               ;BACK UP TO DIGIT COUNT
        DCR     L
        MOV     A,M             ;TEST FOR DECIMAL PT
        CPI     5               ;PRINT . AFTER 1ST DIGIT
        MVI     A,256Q          ;JUST IN CASE
        CZ      OUTR            ;OUTPUT . IF 1ST DIGIT
        MOV     D,M             ;NOW DECREMENT DIGIT COUNT
        DCR     D
        MOV     M,D             ;UPDATE MEM AND LEAVE FLOPS SET
        RET                     ;SERVES AS TERM FOR DIGO & CVRT
MULTT:  MVI     E,1             ;MULT. BY 10 (START WITH X2)
        CALL    LSFT            ;LEFT SHIFT 1 = X2
        MOV     L,C             ;SAVE X2 IN "RESULT"
        DCR     L               ;SET TO TOP OF NUMBER
        MOV     A,C             ;SET C TO RESULT
        ADI     11Q
        MOV     C,A             ;NOW C SET RIGHT
        MOV     A,H             ;SHOW RAM TO RAM TRANSFER
        CALL    COPY            ;SAVE X2 FINALLY
        MOV     A,C             ;MUST RESET C
        SUI     11Q             ;BACK TO NORMAL
        MOV     C,A
        MVI     E,2             ;NOW GET (X2)X4=X8
        MOV     L,C             ;BUT MUST SAVE OVERFLOW
        DCR     L
        CALL    TLP2            ;GET X8
        MOV     L,C             ;SET UP TO CALL DADD
        MOV     A,C             ;SET B TO X2
        ADI     12Q             ;TO X2
        MOV     B,A
        CALL    DADD            ;ADD TWO LOW WORDS
        DCR     L               ;BACK UP TO OVERFLOW
        MOV     A,M             ;GET IT
        MOV     L,B             ;NOW SET TO X2 OVERFLOW
        DCR     L               ;ITS AT B-1
        ADC     M               ;ADD WITH CARRY - CARRY WAS PRESERVED
        RET                     ;ALL DONE, RETURN OVERFLOW IN A
LSFT:   MOV     L,C             ;SET PTR FOR LEFT SHIFT OF NUMBER
        DCR     L               ;BACK UP TO OVERFLOW
        XRA     A               ;OVERFLOW=0 1ST TIME
TLOOP:  MOV     M,A             ;SAVE OVERFLOW
TLP2:   DCR     E               ;TEST FOR DONE
        RM                      ;DONE WHEN E MINUS
        INR     L               ;MOVE TO LOW
        INR     L
        INR     L               ;/***TP EXTENSION
        MOV     A,M             ;SHIFT LEFT 4 BYTES
        RAL
        MOV     M,A             ;PUT BACK
        DCR     L               ;/***TP - ALL DONE
        MOV     A,M             ;GET LOW
        RAL                     ;SHIFT LEFT 1
        MOV     M,A             ;RESTORE IT
        DCR     L               ;BACK UP TO HIGH
        MOV     A,M             ;GET HIGH
        RAL                     ;SHIFT IT LEFT WITH CARRY
        MOV     M,A             ;PUT IT BACK
        DCR     L               ;BACK UP TO OVERFLOW
        MOV     A,M             ;GET OVERFLOW
        RAL                     ;SHIFT IT LEFT
        JMP     TLOOP           ;GO FOR MORE
SIGN:   ANI     200Q            ;GET SIGN BIT
        MVI     A,240Q          ;SPACE INSTEAD OF PLUS
        JZ      PLSV            ;TEST FOR +
        MVI     A,255Q          ;NEGATIVE
PLSV:   CALL    OUTR            ;OUTPUT SIGN
        RET
GCHR:   MOV     L,C             ;GET CHARACTERISTIC
GETA:   INR     L               ;MOVE TO IT
        INR     L
        INR     L               ;/***TP
        MOV     A,M             ;FETCH INTO A
        RET                     ;DONE
MORD:   CALL    GETEX           ;MUL OR DIV DEPENDING ON EXP
        MOV     E,A             ;SAVE DECIMAL EXP
        MOV     B,L             ;SET UP TO MULT OR DIV
        INR     B               ;NOW BOP POINTER SET
        MOV     L,C             ;L POINTS TO NUMBER TO CONVERT
        MOV     A,C             ;POINT C AT "RESULT" AREA
        ADI     11Q             ;IN SCRATCH
        MOV     C,A             ;NOW C SET RIGHT
        MOV     A,E             ;NOW TEST FOR MUL
        ANI     200Q            ;TEST NEGATIVE DEC. EXP.
        JZ      DIVIT           ;IF EXP IS + THEN DIVIDE
        CALL    LMUL            ;MULT.
FINUP:  MOV     A,C             ;SAVE LOC. OF RESULT
        MOV     C,L             ;C=LOC OF NUMBER (IT WAS DESTROYED)
        MOV     L,A             ;SET L TO LOC. OF RESULT
        MOV     A,H             ;SHOW RAM TO RAM TRANSFER
        CALL    COPY            ;MOVE RESULT TO NUMBER
GETEX:  MOV     L,C             ;NOW GET DECIMAL EXP
        INR     L
        JMP     GETA            ;USE PART OF GCHR
DIVIT:  CALL    LDIV            ;DIVIDE
        JMP     FINUP
TWOD:   CALL    CTWO            ;CONVERT TO 2 DIGITS
        MOV     B,A             ;SAVE ONES DIGIT
        CALL    GETEX           ;GET DECIMAL EXP
        MOV     E,A             ;SAVE A COPY
        ANI     200Q            ;TEST FOR NEGATIVE
        JZ      ADD1            ;BUMP EXP BY 1 SINCE 2 DIGITS
        DCR     E               ;DECREMENT NEGATIVE EXP SINCE 2 DIGITS
FINIT:  MOV     M,E             ;RESTORE EXP WITH NEW VALUE
        MOV     A,B             ;NOW DO 2ND DIGIT
        JMP     INPOP           ;GO OUT 2ND AND REST OF DIGITS
ADD1:   INR     E               ;COMPENSATE FOR 2 DIGITS
        JMP     FINIT
CTWO:   MVI     E,377Q          ;CONVERT 2 DIGIT BIN TO BCD
LOOP:   INR     E               ;ADD UP TENS DIGIT
        SUI     12Q             ;SUBTRACT 10
        JP      LOOP            ;TILL NEGATIVE RESULT
        ADI     12Q             ;RESTORE ONES DIGIT
        MOV     B,A             ;SAVE ONES DIGIT
        MOV     A,E             ;GET TENS DIGIT
        CALL    DIGO            ;OUTPUT IT
        MOV     A,B             ;SET A TO 2ND DIGIT
        RET
COPT:   MOV     A,C             ;COPY FROM 10N TO RAM
        ADI     5
        MOV     C,A             ;SET C TO PLACE TO PUT
        MVI     A,TEN5/256
        CALL    COPY            ;COPY IT
        MOV     A,C             ;NOW RESET C
        SUI     5
        MOV     C,A             ;ITS RESET
        RET
COPY:   MOV     B,H             ;SAVE RAM H
        MOV     H,A             ;SET TO SOURCE H
        MOV     A,M             ;GET 4 WORDS INTO THE REGS.
        INR     L
        MOV     D,M
        INR     L
        MOV     E,M
        INR     L
        MOV     L,M             ;LAST ONE ERASES L
        MOV     H,B             ;SET TO DESTINATION RAM
        MOV     B,L             ;SAVE 4TH WORD IN B
        MOV     L,C             ;SET TO DESTINATION
        MOV     M,A             ;SAVE FIRST WORD
        INR     L
        MOV     A,M             ;SAVE THIS WORD IN A (INPUT SAVES C HERE
        MOV     M,D             ;NOW PUT 2ND WORD
        INR     L
        MOV     M,E
        INR     L
        MOV     M,B             ;ALL 4  COPIED NOW
        RET                     ;ALL DONE
;
;
TEN5:   .DB      303Q,120Q,0Q,21Q  ;303240(8) = 100000.
TEN:    .DB      240Q,0Q,0Q,4Q  ;12(8) = 10
;
;       SCRATCH MAP FOR I/O CONVERSION ROUTINES
;
;       RELATIVE TO (C+2)USE
;       C-2             DIGIT COUNT
;       C-1             OVERFLOW
;       C               HIGH NUMBER - MANTISSA
;       C+1             LOW NUMBER
;       C+2             CHARACTERISTIC
;       C+3             DECIMAL EXPONENT (SIGN & MAG.)
;       C+4             TEN**N
;       C+5             TEN**N
;       C+6             TEN**N
;       C+7             RESULT OF MULT & DIV
;       C+8             AND TEMP FOR X2
;       C+9             "       "
;       C+10            L FOR NUMBER TO GO INTO (INPUT ONLY)
;       C+11            DIGIT JUST INPUT (INPUT ONLY)
;
;
;       /*****BEGIN INPUT*************
;
;
;HRJ was:
;ERR:
;       STC                     ;ERROR FLAG
;       RET                     ;AND RETURN
; replaced with code in (PDF) document HRJ

;
;
;   SUBROUTINE ERR
;
ERR:    MVI     A,277Q          ;ERROR IN INPUT
        CALL    OUTR            ;SEND A ?(SPACE)
        MVI     A,240Q          ;
        CALL    OUTR            ;OUTPUT SPACE
        JMP     PRMT            ;GO PROMPT USER AND RESTART
;HRJ  end replacing code
;
;********************************************************
;       //// 4 1/2 DIGIT INPUT ROUTINE
;*******************************************************
;
;
;       /L POINTS TO WHERE TO PUT INPUT NUMBER
;       /C POINTS TO 13(10) WORDS OF SCRATCH
;
INPUT:  MOV     B,L             ;SAVE ADDRESS WHERE DATA IS TO GO
        MOV     A,C             ;IN SCRATCH
        ADI     17Q             ;COMPUTE LOC. IN SCRATCH
        MOV     L,A
        MOV     M,B             ;PUT IT
        INR     C               ;OFFSET SCRATCH POINTER
        INR     C               ;BY 2
PRMT:   MVI     A,272Q          ;PROMPT USER WITH : HRJ added
        CALL    OUTR            ;OUTPUT HRJ added
        CALL    ZROIT           ;ZERO NUMBER
        INR     L               ;AND ZERO
        MOV     M,A             ;DECIMAL EXPONENT
        CALL    GNUM            ;GET INTEGER PART OF NUM
        CPI     376Q            ;TERM=.?
        JZ      DECPT           ;YES
TSTEX:  CPI     25Q             ;TEST FOR E
        JZ      INEXP           ;YES - HANDLE EXP
        CPI     360Q            ;TEST FOR SPACE TERM (240B-260B)
        JNZ     ERR             ;NOT LEGAL TERM
        CALL    FLTSGN          ;FLOAT # AND SIGN IT
SCALE:  CALL    GETEX           ;GET DECIMAL EXP
        ANI     177Q            ;GET GOOD BITS
        MOV     E,A             ;SAVE COPY
        ANI     100Q            ;GET SIGN OF EXP
        RLC                     ;INTO SIGN BIT
        ORA     A               ;SET FLOPS
        MOV     B,A             ;SAVE SIGN
        MOV     A,E             ;GET EXP BACK
        JZ      APLS            ;JMP IS +
        MVI     A,200Q          ;MAKE MINUS +
        SUB     E               ;NOW ITS +
APLS:   ADD     B               ;SIGN NUMBER
        MOV     M,A             ;SAVE EXP (SIGN & MAG.)
        MVI     L,TEN5 & 377Q   ;TRY MORD WITH 10**5 FIRST
        CALL    COPT            ;TRANSFER TO RAM
        CALL    GETEX           ;GET DECIMAL EXP
INT5:   ANI     77Q             ;GET MAG. OF EXP
        CPI     5Q              ;TEST FOR USE OF 10**5
        JM      TRYTN           ;WONT GO - TRY 10
        CALL    MORD            ;WILL GO SO DO IT
        SUI     5Q              ;MAG = MAG -5
        MOV     M,A             ;UPDATE DEC. EXP IN MEM
        JMP     INT5            ;GO TRY AGAIN
TRYTN:  MVI     L,TEN & 377Q    ;PUT TEN IN RAM
        CALL    COPT
        CALL    GETEX           ;SET UP FOR LOOP
INT1:   ANI     77Q             ;GET MAGNITUDE
        ORA     A               ;TEST FOR 0
        JZ      SAVEN           ;DONE, MOVE NUM OUT AND GET OUT
        CALL    MORD            ;NOT DONE - DO 10
        SUI     1Q              ;EXP = EXP -1
        MOV     M,A             ;UPDATE MEM
        JMP     INT1            ;TRY AGAIN
DECPT:  MOV     L,C             ;ZERO DIGIT COUNT
        DCR     L               ;SINCE ITS NECESSARY
        DCR     L               ;TO COMPUTE EXP.
        MVI     M,0             ;ZEROED
        CALL    EP1             ;GNUM IN MIDDLE
        MOV     E,A             ;SAVE TERMINATOR
        MOV     L,C             ;MOVE DIGIT COUNT TO EXP
        DCR     L               ;BACK UP TO DIGIT COUNT
        DCR     L
        MOV     B,M             ;GOT DIGIT COUNT
        CALL    GETEX           ;SET L TO DEC. EXP
        MOV     M,B             ;PUT EXP
        MOV     A,E             ;TERM BACK TO A
        JMP     TSTEX           ;TEST FOR E+OR-XX
INEXP:  CALL    FLTSGN          ;FLOAT AND SIGN NUMBER
        CALL    SAVEN           ;SAVE NUMBER IN (L) TEMP
        CALL    ZROIT           ;ZERO OUT NUM. FOR INPUTTING EXP
        CALL    GNUM            ;NOW INPUT EXPONENT
        CPI     360Q            ;TEST FOR SPACE TERM.
        JNZ     ERR             ;NOT LEGAL - TRY AGAIN
        MOV     L,C             ;GET EXP OUT OF MEM
        INR     L               ;/***TP
        INR     L               ;EXP LIMITED TO 5 BITS
        MOV     A,M             ;GET LOWEST 8 BITS
        ANI     37Q             ;GET GOOD BITS
        MOV     B,A             ;SAVE THEM
        INR     L               ;GET SIGN OF EXP
        MOV     A,M             ;INTO A
        ORA     A               ;SET FLOPS
        MOV     A,B             ;IN CASE NOTHING TO DO
        JM      USEIT           ;IF NEG. USE AS +
        MVI     A,0Q            ;IF + MAKE -
        SUB     B               ;0-X = -X
USEIT:  INR     L               ;POINT AT EXP
        ADD     M               ;GET REAL DEC. EXP
        MOV     M,A             ;PUT IN MEM
        MOV     A,C             ;NOW GET NUMBER BACK
        ADI     15Q             ;GET ADD OF L
        MOV     L,A             ;L POINTS TO L OF NUMBER
        MOV     L,M             ;NOW L POINTS TO NUMBER
        MOV     A,H             ;RAM TO RAM COPY
        CALL    COPY            ;COPY IT BACK
        JMP     SCALE           ;NOW ADJUST FOR EXP
GNUM:   CALL    INP             ;GET A CHAR
        CPI     240Q            ;IGNORE LEADING SPACES
        JZ      GNUM
        CPI     255Q            ;TEST FOR -
        JNZ     TRYP            ;NOT MINUS
        MOV     L,C             ;MINUS SO SET SIGN
        INR     L               ;IN CHAR LOC.
        INR     L               ;/***TP
        INR     L
        MVI     M,200Q          ;SET - SIGN
        JMP     GNUM
TRYP:   CPI     253Q            ;IGNORE +
        JZ      GNUM
TSTN:   SUI     260Q            ;STRIP ASCII
        RM                      ;RETURN IF TERM
        CPI     12Q             ;TEST FOR NUMBER
        RP                      ;ILLEGAL
        MOV     E,A             ;SAVE DIGIT
        CALL    GETN            ;LOC. OF DIGIT STORAGE TO L
        MOV     M,E             ;SAVE DIGIT
        CALL    MULTT           ;MULT NUMBER BY 10
        ORA     A               ;TEST FOR TOO MANY DIGITS
        RNZ                     ;TOO MANY DIGITS
        CALL    GETN            ;GET DIGIT
        MOV     L,C             ;SET L TO NUMBER
        INR     L
        INR     L               ;/***TP
        ADD     M               ;ADD IN THE DIGIT
        MOV     M,A             ;PUT RESULT BACK
        DCR     L               ;NOW DO HIGH
        MOV     A,M             ;GET HIGH TO ADD IN CARRY
        ACI     0Q              ;ADD IN CARRY
        MOV     M,A             ;UPDATE HIGH
        DCR     L               ;/***TP EXTENSION
        MOV     A,M
        ACI     0Q              ;ADD IN CARRY
        MOV     M,A             ;/***TP ALL DONE
        RC                      ;OVERFLOW ERROR
        DCR     L               ;BUMP DIGIT COUNT NOW
        DCR     L
        MOV     B,M             ;GET DIGIT COUNT
        INR     B               ;BUMP DIGIT COUNT
        MOV     M,B             ;UPDATE DIGIT COUNT
EP1:    CALL    INP             ;GET NEXT CHAR
        JMP     TSTN            ;MUST BE NUM. OR TERM
FLTSGN: MOV     L,C             ;POINT L AT NUMBER TO FLOAT
        JMP     FLOAT           ;GO FLOAT IT
SAVEN:  MOV     A,C             ;PUT NUMBER IN (L)
        ADI     15Q             ;GET ADD OF L
        MOV     L,A
        MOV     E,M             ;GET L OF RESULT
        MOV     L,E             ;POINT L AT (L)
        INR     L               ;SET TO 2ND WORD TO SAVE C
        MOV     M,C             ;SAVE C IN (L) +1 SINCE IT WILL BE DESTROYED
        MOV     L,C             ;SET UP TO CALL COPY
        MOV     C,E             ;NOW L&C SET
        MOV     A,H             ;RAM TO RAM COPY
        CALL    COPY            ;COPY TO L
        MOV     C,A             ;(L)+1 RETURNED HERE SO SET AS C
        ;ORA    A               ;MAKE SURE CY=0 (NO ERROR) 
                                ;HRJ ORA above not in LLL document
        RET                     ;NOW EVERYTHING HUNKY-DORRY
GETN:   MOV     A,C             ;GET DIGIT
        ADI     16Q             ;LAST LOC. IN SCRATCH
        MOV     L,A             ;PUT IN L
        MOV     A,M             ;GET DIGIT
        RET
ZROIT:  MOV     L,C             ;ZERO NUMBER
        XRA     A
        MOV     M,A             ;/***TP
        INR     L               ;/***TP
        MOV     M,A
        INR     L
        MOV     M,A
        INR     L               ;NOW SET SIGN TO +
        MOV     M,A
        RET                     ;DONE
;
;
; END of code from LLNL PDF document
;
        .END

