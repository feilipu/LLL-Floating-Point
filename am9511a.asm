;==================================================================================
; Contents of this file are copyright Phillip Stevens
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
;
; https://github.com/feilipu/
;
; https://feilipu.me/
;
;==================================================================================
;
; Z180 Register Mnemonics
;

IO_BASE         .EQU    $00     ; Internal I/O Base Address (ICR) <<< SET THIS AS DESIRED >>>

CNTLA0          .EQU    IO_BASE+$00     ; ASCI Control Reg A Ch 0
CNTLA1          .EQU    IO_BASE+$01     ; ASCI Control Reg A Ch 1
CNTLB0          .EQU    IO_BASE+$02     ; ASCI Control Reg B Ch 0
CNTLB1          .EQU    IO_BASE+$03     ; ASCI Control Reg B Ch 1
STAT0           .EQU    IO_BASE+$04     ; ASCI Status  Reg   Ch 0
STAT1           .EQU    IO_BASE+$05     ; ASCI Status  Reg   Ch 1
TDR0            .EQU    IO_BASE+$06     ; ASCI Tx Data Reg   Ch 0
TDR1            .EQU    IO_BASE+$07     ; ASCI Tx Data Reg   Ch 1
RDR0            .EQU    IO_BASE+$08     ; ASCI Rx Data Reg   Ch 0
RDR1            .EQU    IO_BASE+$09     ; ASCI Rx Data Reg   Ch 1

ASEXT0          .EQU    IO_BASE+$12     ; ASCI Extension Control Reg Ch 0 (Z8S180 & higher Only)
ASEXT1          .EQU    IO_BASE+$13     ; ASCI Extension Control Reg Ch 1 (Z8S180 & higher Only)

ASTC0L          .EQU    IO_BASE+$1A     ; ASCI Time Constant Ch 0 Low (Z8S180 & higher Only)
ASTC0H          .EQU    IO_BASE+$1B     ; ASCI Time Constant Ch 0 High (Z8S180 & higher Only)
ASTC1L          .EQU    IO_BASE+$1C     ; ASCI Time Constant Ch 1 Low (Z8S180 & higher Only)
ASTC1H          .EQU    IO_BASE+$1D     ; ASCI Time Constant Ch 1 High (Z8S180 & higher Only)

CNTR            .EQU    IO_BASE+$0A     ; CSI/O Control Reg
TRDR            .EQU    IO_BASE+$0B     ; CSI/O Tx/Rx Data Reg

TMDR0L          .EQU    IO_BASE+$0C     ; Timer Data Reg Ch 0 Low
TMDR0H          .EQU    IO_BASE+$0D     ; Timer Data Reg Ch 0 High
RLDR0L          .EQU    IO_BASE+$0E     ; Timer Reload Reg Ch 0 Low
RLDR0H          .EQU    IO_BASE+$0F     ; Timer Reload Reg Ch 0 High
TCR             .EQU    IO_BASE+$10     ; Timer Control Reg

TMDR1L          .EQU    IO_BASE+$14     ; Timer Data Reg Ch 1 Low
TMDR1H          .EQU    IO_BASE+$15     ; Timer Data Reg Ch 1 High
RLDR1L          .EQU    IO_BASE+$16     ; Timer Reload Reg Ch 1 Low
RLDR1H          .EQU    IO_BASE+$17     ; Timer Reload Reg Ch 1 High

FRC             .EQU    IO_BASE+$18     ; Free-Running Counter

CMR             .EQU    IO_BASE+$1E     ; CPU Clock Multiplier Reg (Z8S180 & higher Only)
CCR             .EQU    IO_BASE+$1F     ; CPU Control Reg (Z8S180 & higher Only)

SAR0L           .EQU    IO_BASE+$20     ; DMA Source Addr Reg Ch0-Low
SAR0H           .EQU    IO_BASE+$21     ; DMA Source Addr Reg Ch0-High
SAR0B           .EQU    IO_BASE+$22     ; DMA Source Addr Reg Ch0-Bank
DAR0L           .EQU    IO_BASE+$23     ; DMA Dest Addr Reg Ch0-Low
DAR0H           .EQU    IO_BASE+$24     ; DMA Dest Addr Reg Ch0-High
DAR0B           .EQU    IO_BASE+$25     ; DMA Dest ADDR REG CH0-Bank
BCR0L           .EQU    IO_BASE+$26     ; DMA Byte Count Reg Ch0-Low
BCR0H           .EQU    IO_BASE+$27     ; DMA Byte Count Reg Ch0-High
MAR1L           .EQU    IO_BASE+$28     ; DMA Memory Addr Reg Ch1-Low
MAR1H           .EQU    IO_BASE+$29     ; DMA Memory Addr Reg Ch1-High
MAR1B           .EQU    IO_BASE+$2A     ; DMA Memory Addr Reg Ch1-Bank
IAR1L           .EQU    IO_BASE+$2B     ; DMA I/O Addr Reg Ch1-Low
IAR1H           .EQU    IO_BASE+$2C     ; DMA I/O Addr Reg Ch2-High
BCR1L           .EQU    IO_BASE+$2E     ; DMA Byte Count Reg Ch1-Low
BCR1H           .EQU    IO_BASE+$2F     ; DMA Byte Count Reg Ch1-High
DSTAT           .EQU    IO_BASE+$30     ; DMA Status Reg
DMODE           .EQU    IO_BASE+$31     ; DMA Mode Reg
DCNTL           .EQU    IO_BASE+$32     ; DMA/Wait Control Reg

IL              .EQU    IO_BASE+$33     ; INT Vector Low Reg
ITC             .EQU    IO_BASE+$34     ; INT/TRAP Control Reg

RCR             .EQU    IO_BASE+$36     ; Refresh Control Reg

CBR             .EQU    IO_BASE+$38     ; MMU Common Base Reg
BBR             .EQU    IO_BASE+$39     ; MMU Bank Base Reg
CBAR            .EQU    IO_BASE+$3A     ; MMU Common/Bank Area Reg

OMCR            .EQU    IO_BASE+$3E     ; Operation Mode Control Reg
ICR             .EQU    IO_BASE+$3F     ; I/O Control Reg


;==================================================================================
;
; Interrupt vectors (offsets) for Z180/HD64180 internal interrupts
;

VECTOR_BASE     .EQU   $80      ; Vector Base address (IL) <<< SET THIS AS DESIRED >>>

VECTOR_INT1     .EQU   VECTOR_BASE+$00    ; external /INT1 
VECTOR_INT2     .EQU   VECTOR_BASE+$02    ; external /INT2 
VECTOR_PRT0     .EQU   VECTOR_BASE+$04    ; PRT channel 0 
VECTOR_PRT1     .EQU   VECTOR_BASE+$06    ; PRT channel 1 
VECTOR_DMA0     .EQU   VECTOR_BASE+$08    ; DMA channel 0 
VECTOR_DMA1     .EQU   VECTOR_BASE+$0A    ; DMA Channel 1 
VECTOR_CSIO     .EQU   VECTOR_BASE+$0C    ; Clocked serial I/O 
VECTOR_ASCI0    .EQU   VECTOR_BASE+$0E    ; Async channel 0 
VECTOR_ASCI1    .EQU   VECTOR_BASE+$10    ; Async channel 1

;==================================================================================
;
; Some bit definitions used with the Z-180 on-chip peripherals:
;

; ASCI Control Reg A (CNTLAn)

SER_MPE         .EQU   $80    ; Multi Processor Enable
SER_RE          .EQU   $40    ; Receive Enable
SER_TE          .EQU   $20    ; Transmit Enable
SER_RTS0        .EQU   $10    ; _RTS Request To Send
SER_EFR         .EQU   $08    ; Error Flag Reset

SER_7N1         .EQU   $00    ; 7 Bits No Parity 1 Stop Bit
SER_7N2         .EQU   $01    ; 7 Bits No Parity 2 Stop Bits
SER_7P1         .EQU   $02    ; 7 Bits    Parity 1 Stop Bit
SER_7P2         .EQU   $03    ; 7 Bits    Parity 2 Stop Bits
SER_8N1         .EQU   $04    ; 8 Bits No Parity 1 Stop Bit
SER_8N2         .EQU   $05    ; 8 Bits No Parity 2 Stop Bits
SER_8P1         .EQU   $06    ; 8 Bits    Parity 1 Stop Bit
SER_8P2         .EQU   $07    ; 8 Bits    Parity 2 Stop Bits

; ASCI Control Reg B (CNTLBn)
                              ; BAUD Rate = PHI / PS / SS / DR

SER_MPBT        .EQU   $80    ; Multi Processor Bit Transmit
SER_MP          .EQU   $40    ; Multi Processor
SER_PS          .EQU   $20    ; Prescale PHI by 10 (PS 0) or 30 (PS 1)
SER_PEO         .EQU   $10    ; Parity Even or Odd
SER_DR          .EQU   $08    ; Divide SS by 16 (DR 0) or 64 (DR 1)

SER_SS_DIV_1    .EQU   $00    ; Divide PS by  1
SER_SS_DIV_2    .EQU   $01    ; Divide PS by  2
SER_SS_DIV_4    .EQU   $02    ; Divide PS by  4
SER_SS_DIV_8    .EQU   $03    ; Divide PS by  8
SER_SS_DIV_16   .EQU   $04    ; Divide PS by 16
SER_SS_DIV_32   .EQU   $05    ; Divide PS by 32
SER_SS_DIV_64   .EQU   $06    ; Divide PS by 64
SER_SS_EXT      .EQU   $07    ; External Clock Source <= PHI / 40

; ASCI Status Reg (STATn)

SER_RDRF        .EQU   $80    ; Receive Data Register Full
SER_OVRN        .EQU   $40    ; Overrun (Received Byte)
SER_PE          .EQU   $20    ; Parity Error (Received Byte)
SER_FE          .EQU   $10    ; Framing Error (Received Byte)
SER_RIE         .EQU   $08    ; Receive Interrupt Enabled
SER_DCD0        .EQU   $04    ; _DCD0 Data Carrier Detect USART0
SER_CTS1        .EQU   $04    ; _CTS1 Clear To Send USART1
SER_TDRE        .EQU   $02    ; Transmit Data Register Empty
SER_TIE         .EQU   $01    ; Transmit Interrupt Enabled

; CPU Clock Multiplier Reg (CMR) (Z8S180 & higher Only)

CMR_X2          .EQU   $80    ; CPU x2 XTAL Multiplier Mode
CMR_LN_XTAL     .EQU   $40    ; Low Noise Crystal 

; CPU Control Reg (CCR) (Z8S180 & higher Only)

CCR_XTAL_X2     .EQU   $80    ; PHI = XTAL Mode
CCR_STANDBY     .EQU   $40    ; STANDBY after SLEEP
CCR_BREXT       .EQU   $20    ; Exit STANDBY on BUSREQ
CCR_LNPHI       .EQU   $10    ; Low Noise PHI (30% Drive)
CCR_IDLE        .EQU   $08    ; IDLE after SLEEP
CCR_LNIO        .EQU   $04    ; Low Noise I/O Signals (30% Drive)
CCR_LNCPUCTL    .EQU   $02    ; Low Noise CPU Control Signals (30% Drive)
CCR_LNAD        .EQU   $01    ; Low Noise Address and Data Signals (30% Drive)

; DMA/Wait Control Reg (DCNTL)

DCNTL_MWI1      .EQU   $80    ; Memory Wait Insertion 1 (1 Default)
DCNTL_MWI0      .EQU   $40    ; Memory Wait Insertion 0 (1 Default)
DCNTL_IWI1      .EQU   $20    ; I/O Wait Insertion 1 (1 Default)
DCNTL_IWI0      .EQU   $10    ; I/O Wait Insertion 0 (1 Default)
DCNTL_DMS1      .EQU   $08    ; DMA Request Sense 1
DCNTL_DMS0      .EQU   $04    ; DMA Request Sense 0
DCNTL_DIM1      .EQU   $02    ; DMA Channel 1 I/O & Memory Mode
DCNTL_DIM0      .EQU   $01    ; DMA Channel 1 I/O & Memory Mode


; INT/TRAP Control Register (ITC)

ITC_ITE2        .EQU   $04    ; Interrupt Enable #2
ITC_ITE1        .EQU   $02    ; Interrupt Enable #1
ITC_ITE0        .EQU   $01    ; Interrupt Enable #0 (1 Default)

; Refresh Control Reg (RCR)

RCR_REFE        .EQU   $80    ; DRAM Refresh Enable
RCR_REFW        .EQU   $40    ; DRAM Refresh 2 or 3 Wait states

; Operation Mode Control Reg (OMCR)

OMCR_M1E        .EQU   $80    ; M1 Enable (0 Disabled)
OMCR_M1TE       .EQU   $40    ; M1 Temporary Enable
OMCR_IOC        .EQU   $20    ; IO Control (1 64180 Mode)

;==================================================================================
;
; Some definitions used with the YAZ-180 on-board peripherals:
;

; BREAK for Single Step Mode
BREAK           .EQU    $2000      ; Any value written to $2000, halts CPU

; 82C55 PIO Port Definitions

PIO             .EQU    $4000      ; Base Address for 82C55
PIOA            .EQU    PIO+$00    ; Address for Port A
PIOB            .EQU    PIO+$01    ; Address for Port B
PIOC            .EQU    PIO+$02    ; Address for Port C
PIOCNTL         .EQU    PIO+$03    ; Address for Control Byte

; PIO Mode Definitions

; Mode 0 - Basic Input / Output

PIOCNTL00       .EQU    $80        ; A->, B->, CH->, CL->
PIOCNTL01       .EQU    $81        ; A->, B->, CH->, ->CL
PIOCNTL0        .EQU    $82        ; A->, ->B, CH->, CL->
PIOCNTL03       .EQU    $83        ; A->, ->B, CH->, ->CL

PIOCNTL04       .EQU    $88        ; A->, B->, ->CH, CL->
PIOCNTL05       .EQU    $89        ; A->, B->, ->CH, ->CL
PIOCNTL06       .EQU    $8A        ; A->, ->B, ->CH, CL->
PIOCNTL07       .EQU    $8B        ; A->, ->B, ->CH, ->CL

PIOCNTL08       .EQU    $90        ; ->A, B->, CH->, CL->
PIOCNTL09       .EQU    $91        ; ->A, B->, CH->, ->CL
PIOCNTL10       .EQU    $92        ; ->A, ->B, CH->, CL->
PIOCNTL11       .EQU    $83        ; ->A, ->B, CH->, ->CL

PIOCNTL12       .EQU    $98        ; ->A, B->, ->CH, CL-> (Default Setting)
PIOCNTL13       .EQU    $99        ; ->A, B->, ->CH, ->CL
PIOCNTL14       .EQU    $9A        ; ->A, ->B, ->CH, CL->
PIOCNTL15       .EQU    $9B        ; ->A, ->B, ->CH, ->CL

; Mode 1 - Strobed Input / Output
; TBA Later

; Mode 2 - Strobed Bidirectional Bus Input / Output
; TBA Later

; Am9511A-1 APU Port Address

APU             .EQU    $C000      ; Base Address for Am9511A
APUDATA         .EQU    APU+$00    ; APU Data Port
APUCNTL         .EQU    APU+$01    ; APU Control Port - Write Only
APUSTAT         .EQU    APU+001    ; APU Status Port - Read Only

;==================================================================================
;
; DEFINES SECTION
;

ROMSTART        .EQU     $0000 ; Bottom of FLASH
ROMSTOP         .EQU     $1FFF ; Top of FLASH

RAMSTART_CA0    .EQU     $2000 ; Bottom of Common 0 RAM
RAMSTOP_CA0     .EQU     $3FFF ; Top of Common 0 RAM

RAMSTART_BANK   .EQU     $4000 ; Bottom of Banked RAM
RAMSTOP_BANK    .EQU     $7FFF ; Top of Banked RAM

RAMSTART_CA1    .EQU     $8000 ; Bottom of Common 1 RAM
RAMSTOP_CA1     .EQU     $FFFF ; Top of Common 1 RAM

RAMSTART        .EQU     RAMSTART_CA0
RAMSTOP         .EQU     RAMSTOP_CA1

APU_BUFF_START  .EQU     $2300 ; START AT RAM 0x2300 TO CLEAR SER BUFFER
APU_STACKTOP    .EQU     $25FE ; Keep RAM usage below 0x25FF

INT0_APU        .EQU     $3800 ; the APU IM1 INT0 jump (RAM)
INT0_APU_ISR    .EQU     $3810 ; start of the APU ISR asm code (RAM)

APU_CMD_BUFSIZE .EQU     $FF    ; FIXED CMD buffer size, 256 CMDs
APU_PTR_BUFSIZE .EQU     $FF    ; FIXED DATA POINTER buffer size, 128 POINTERs

OP_ENT_CMD      .EQU     $40
OP_REM_CMD      .EQU     $50
OP_ENT16_CMD    .EQU     $40
OP_ENT32_CMD    .EQU     $41
OP_REM16_CMD    .EQU     $50
OP_REM32_CMD    .EQU     $51

;
;
;******************************************************
;       //// AM9511A-1 LIBRARY VARIABLES
;******************************************************
;

APUCMDBuf       .EQU     APU_BUFF_START ; must start on 0xnn00 for low byte roll-over
APUPTRBuf       .EQU     APUCMDBuf+APU_CMD_BUFSIZE+1
APUCMDInPtr     .EQU     APUPTRBuf+APU_PTR_BUFSIZE+1
APUCMDOutPtr    .EQU     APUCMDInPtr+2
APUPTRInPtr     .EQU     APUCMDOutPtr+2
APUPTROutPtr    .EQU     APUPTRInPtr+2
APUCMDBufUsed   .EQU     APUPTROutPtr+2
APUPTRBufUsed   .EQU     APUCMDBufUsed+1

;==================================================================================
;
; MACRO SECTION
;

; #DEFINE 

;==================================================================================
;
; ORIGIN
;

        .ORG INT0_APU_ISR      ;LIBRARY ORIGIN FOR RC2014 AND YAZ180 DURING TESTING


;==================================================================================
;
; INTERRUPT SECTION
;
; Interrupt Service Routine for the Am9511A-1
; 
; Initially called once the required operand pointers and commands are loaded
; Following calls generated by END signal whenever a single APU command is completed
; Sends a new command (with operands if needed) to the APU

APU_ISR:
        di                      ; we're called directly, the first time
        ld (APU_STACKTOP), sp   ; store the old stack top, at top of new SP
        ld sp, APU_STACKTOP     ; set new Stack Pointer
        push af                 ; store AF, etc, so we don't clobber it
        push bc
        push hl
        
        xor a                   ; Set internal clock = crystal x 1 = 18.432MHz
                                ; That makes the PHI 9.216MHz
        out0 (CMR), a           ; CPU Clock Multiplier Reg (CMR)

APU_ISR_ENTRY:
        ld a, (APUCMDBufUsed)   ; check whether we have a command to do
        or a                    ; zero?
        jr z, APU_ISR_END       ; if so then exit

        ld hl, (APUCMDOutPtr)   ; get the pointer to place where we pop the COMMAND
        ld a, (hl)              ; get the COMMAND byte
        push af                 ; save the COMMAND
        
        inc l                   ; move the COMMAND pointer low byte along, 0xFF rollover
        ld (APUCMDOutPtr), hl   ; write where the next byte should be popped

        ld hl, APUCMDBufUsed
        dec (hl)                ; atomically decrement current COMMAND count remaining
        
        and $F0                 ; mask MSB of COMMAND
        cp OP_ENT_CMD           ; check whether it is OPERAND entry COMMAND
        jr z, APU_ISR_OP_ENT    ; load an OPERAND
        
        cp OP_REM_CMD           ; check whether it is OPERAND removal COMMAND
        jr z, APU_ISR_OP_REM    ; remove an OPERAND

        pop af                  ; recover the COMMAND
        ld bc, APUCNTL          ; the address of the APU control port in BC
        out (c), a              ; load the COMMAND <<<<<<<<<<<<<<<<<<<<< THIS LINE HANGS
        
        in0 a, (ITC)            ; Get INT/TRAP Control Register (ITC)
        or  ITC_ITE0            ; Mask in INT0
        out0 (ITC), a           ; Enable external interrupt INT0
        
APU_ISR_EXIT:
        pop hl                  ; recover HL, etc
        pop bc
        pop af
        ld sp, (APU_STACKTOP)   ;recover the old Stack Pointer
        ei
        reti

APU_ISR_OP_ENT:
        push de
        
        ld hl, (APUPTROutPtr)   ; get the pointer to where we pop OPERAND POINTER
        ld e, (hl)              ; read the POINTER low byte to the APUPTRInPtr
        inc l                   ; move the POINTER low byte along, 0xFF rollover
        ld d, (hl)              ; read the POINTER high byte to the APUPTRInPtr
        inc l
        ld (APUPTROutPtr), hl   ; write where the next POINTER should be read
                 
        ld hl, APUPTRBufUsed    ; atomic decrement of POINTER count
        dec (hl)
        dec (hl)

        ld bc, APUDATA+$0300    ; the address of the APU data port in BC
        ex de, hl               ; move the base address of the OPERAND to HL
        
        pop de

        outi                    ; output 16 bit OPERAND
        outi

        pop af                  ; get the command back
        cp OP_ENT16_CMD         ; is it a 2 byte OPERAND
        jr z, APU_ISR_ENTRY     ; yes? then go back to get another COMMAND

        outi                    ; output last two bytes of 32 bit OPERAND
        outi
        
        jr APU_ISR_ENTRY        ; go back to get another COMMAND

APU_ISR_OP_REM:
        push de

        ld hl, (APUPTROutPtr)   ; get the pointer to where we pop OPERAND POINTER
        ld e, (hl)              ; read the POINTER low byte to the APUPTRInPtr
        inc l                   ; move the POINTER low byte along, 0xFF rollover
        ld d, (hl)              ; read the POINTER high byte to the APUPTRInPtr
        inc l
        ld (APUPTROutPtr), hl   ; write where the next POINTER should be read
                 
        ld hl, APUPTRBufUsed    ; atomic decrement of OPERAND POINTER count
        dec (hl)
        dec (hl)

        ld bc, APUDATA+$0300    ; the address of the APU data port in BC
        ex de, hl               ; move the base address of the OPERAND to HL

        pop de

        inc hl                  ; reverse the OPERAND bytes to load

        pop af                  ; get the command back
        cp OP_REM16_CMD         ; is it a 2 byte OPERAND
        jr z, APU_ISR_REM16     ; yes then skip

        inc hl                  ; increment two more bytes for 32bit OPERAND
        inc hl
        ind                     ; get the higher two bytes of 32bit OPERAND
        ind

APU_ISR_REM16:
        ind                     ; get 16 bit OPERAND
        ind
                                ; There is only ever one result
APU_ISR_END:                    ; We're done
        in0 a, (ITC)            ; Get INT/TRAP Control Register (ITC)
        and ~ITC_ITE0           ; Mask out INT0   
        out0 (ITC), a           ; Disable external interrupt INT0

                                ; Set internal clock = crystal x 2 = 36.864MHz
        ld a, CMR_X2            ; Set Hi-Speed flag
        out0 (CMR), a           ; CPU Clock Multiplier Reg (CMR)

        jr APU_ISR_EXIT         ; We're done here

        
;==================================================================================
;
; CODE SECTION
;

;------------------------------------------------------------------------------
;       APU_INIT
;       Initialises the APU buffers

APU_INIT:
        push af
        push bc
        push de
        push hl

        LD  HL, APUCMDBuf       ; Initialise COMMAND Buffer
        LD (APUCMDInPtr), HL
        LD (APUCMDOutPtr), HL

        LD HL, APUPTRBuf        ; Initialise OPERAND POINTER Buffer
        LD (APUPTRInPtr), HL
        LD (APUPTROutPtr), HL

        XOR A                   ; 0 both Buffer counts
        LD (APUCMDBufUsed), A
        LD (APUPTRBufUsed), A
        
        XOR A
        LD (APUCMDBuf), A       ; Clear COMMANDj Buffer
        LD HL, APUCMDBuf
        LD D, H
        LD E, L
        INC DE
        LD BC, APU_CMD_BUFSIZE
        LDIR

        XOR A
        LD (APUPTRBuf), A       ; Clear OPERAND POINTER Buffer
        LD HL, APUPTRBuf
        LD D, H
        LD E, L
        INC DE
        LD BC, APU_PTR_BUFSIZE
        LDIR

        ld hl, INT0_APU         ; load the address of the APU INT0 jump
                                ; initially there is a RET 0xC9 there.
        ld (hl), $c3            ; load a JP to 0x3810
        inc hl
        ld (hl), $10
        inc hl
        ld (hl), $38

;        ld (hl), $c3           ; load a JP to 0x0020
;        inc hl
;        ld (hl), $20
;        inc hl
;        ld (hl), $00

APU_INIT_LOOP:
        ex (sp), hl             ; a short delay
        ex (sp), hl

        ld bc, APUSTAT          ; the address of the APU status port in bc
        in a, (c)               ; read the APU
        and $80                 ; Busy?
        jr nz, APU_INIT_LOOP

        pop hl
        pop de
        pop bc
        pop af
        ret

;------------------------------------------------------------------------------
;       APU_CHK_IDLE
;       Confirms whether the APU is idle
;       Loop until it returns ready
;       For Interrupt driven, not useful.
;       Operand Entry and Removal takes little time,
;       and we'll be interrupted for Command entry.

APU_CHK_IDLE:
        push af
        push bc

APU_CHK_LOOP:
        ex (sp), hl             ; a short delay
        ex (sp), hl

        ld a, (APUCMDBufUsed)   ; get the usage of the COMMAND buffer
        and a                   ; check it is zero
        jr nz, APU_CHK_LOOP     ; otherwise wait
        
        ex (sp), hl             ; a short delay
        ex (sp), hl

        ld bc, APUSTAT          ; the address of the APU status port in bc
        in a, (c)               ; read the APU
        and $80                 ; Busy?
        jr nz, APU_CHK_LOOP     ; then wait

        pop bc
        pop af
        ret

;------------------------------------------------------------------------------
;       APU_OP_LD
;       POINTER to OPERAND in DE
;       APU COMMAND in A

APU_OP_LD:
        push hl                 ; store HL so we don't clobber it
        push af                 ; store COMMAND so we don't clobber it

        ld a, (APUCMDBufUsed)   ; Get the number of bytes in the COMMAND buffer
        cp APU_CMD_BUFSIZE      ; check whether there is space in the buffer
        
        pop af                  ; pop the operand entry COMMAND        
        jr z, APU_OP_EXIT       ; COMMAND buffer full, so pop the COMMAND and exit

        ld hl, (APUCMDInPtr)    ; get the pointer to where we poke
        ld (hl), a              ; write the COMMAND byte to the APUCMDInPtr   

        inc l                   ; move the COMMAND pointer low byte along, 0xFF rollover
        ld (APUCMDInPtr), hl    ; write where the next byte should be poked

        ld hl, APUCMDBufUsed
        inc (hl)                ; atomic increment of COMMAND count

        ld a, (APUPTRBufUsed)   ; Get the number of bytes in the OPERAND POINTER buffer
        cp APU_PTR_BUFSIZE-1    ; check whether there is space for a POINTER
        jr z, APU_OP_EXIT       ; buffer full, so exit
        
        ld hl, (APUPTRInPtr)    ; get the pointer to where we poke
        ld (hl), e              ; write the low byte of POINTER to the APUPTRInPtr   
        inc l                   ; move the POINTER low byte along, 0xFF rollover
        ld (hl), d              ; write the high byte of POINTER to the APUPTRInPtr   
        inc l
        ld (APUPTRInPtr), hl    ; write where the next POINTER should be poked

        ld hl, APUPTRBufUsed
        inc (hl)
        inc (hl)

APU_OP_EXIT:
        pop hl                  ; recover HL      
        ret

;------------------------------------------------------------------------------
;       APU_CMD_LD
;       APU COMMAND in A

APU_CMD_LD:
        push hl                 ; store HL so we don't clobber it
        push af                 ; store COMMAND so we don't clobber it

        ld a, (APUCMDBufUsed)   ; Get the number of bytes in the COMMAND buffer
        cp APU_CMD_BUFSIZE      ; check whether there is space in the buffer
        
        pop af                  ; pop the operand entry COMMAND        
        jr z, APU_CMD_EXIT      ; COMMAND buffer full, so pop the COMMAND and exit

        ld hl, (APUCMDInPtr)    ; get the pointer to where we poke
        ld (hl), a              ; write the COMMAND byte to the APUCMDInPtr   

        inc l                   ; move the COMMAND pointer low byte along, 0xFF rollover
        ld (APUCMDInPtr), hl    ; write where the next byte should be poked

        ld hl, APUCMDBufUsed
        inc (hl)                ; atomic increment of COMMAND count

APU_CMD_EXIT:
        pop hl                  ; recover HL
        ret

        .end
