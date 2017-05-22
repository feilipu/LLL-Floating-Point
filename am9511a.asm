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

;==============================================================================
;
; INCLUDES SECTION
;

;==============================================================================
;
; DEFINES SECTION
;

NMI_APU_ISR .EQU    $2900 ; start of the APU ISR asm code

APU_LIBRARY .EQU    $3000 ; start of APU and LLLF library functions

;==================================================================================
;
; MACRO SECTION
;

; #DEFINE 


;==================================================================================
;
; INTERRUPT SECTION
;
; Interrupt Service Routine for the Am9511A-1
; 
; Initially called once the required operand pointers and commands are loaded
; Following calls generated by END signal whenever a single APU command is completed
; Sends a new command (with operands if needed) to the APU

        .ORG NMI_APU_ISR      ;DRIVER ORIGIN FOR YAZ180 DURING TESTING

APU_ISR:
        push af                 ; store AF, etc, so we don't clobber them
        push bc
        push de
        push hl

        xor a                   ; set internal clock = crystal x 1 = 18.432MHz
                                ; that makes the PHI 9.216MHz
        out0 (CMR), a           ; CPU Clock Multiplier Reg (CMR)

APU_ISR_ENTRY:
        ld a, (APUCMDBufUsed)   ; check whether we have a command to do
        or a                    ; zero?
        jr z, APU_ISR_END       ; if so then clean up and END

        ld bc, APUCNTL          ; the address of the APU control port in BC
        in a, (c)               ; read the APU
        and APU_CNTL_ERROR      ; any errors?
        call nz, APU_ISR_ERROR  ; then capture error in APUError

        ld hl, (APUCMDOutPtr)   ; get the pointer to place where we pop the COMMAND
        ld a, (hl)              ; get the COMMAND byte
        ld (APUStatus), a       ; save the COMMAND (as a status byte)

        inc l                   ; move the COMMAND pointer low byte along, 0xFF rollover
        ld (APUCMDOutPtr), hl   ; write where the next byte should be popped

        ld hl, APUCMDBufUsed
        dec (hl)                ; atomically decrement COMMAND count remaining
        
        and $F0                 ; mask MSB of COMMAND
        cp APU_OP_ENT           ; check whether it is OPERAND entry COMMAND
        jr z, APU_ISR_OP_ENT    ; load an OPERAND

        cp APU_OP_REM           ; check whether it is OPERAND removal COMMAND
        jr z, APU_ISR_OP_REM    ; remove an OPERAND

        ld a, (APUStatus)       ; recover the COMMAND from Status
        ld bc, APUCNTL          ; the address of the APU control port in BC
        out (c), a              ; load the COMMAND, and do it

APU_ISR_EXIT:
        pop hl                  ; recover HL, etc
        pop de
        pop bc
        pop af
        retn

APU_ISR_END:                    ; we've finished with no errors
        xor a
        ld (APUStatus), a       ; set the COMMAND status to idle (NOP)

                                ; set internal clock = crystal x 2 = 36.864MHz
        ld a, CMR_X2            ; set Hi-Speed flag
        out0 (CMR), a           ; CPU Clock Multiplier Reg (CMR)

        jr APU_ISR_EXIT         ; we're done here

APU_ISR_OP_ENT:      
        ld hl, (APUPTROutPtr)   ; get the pointer to where we pop OPERAND PTR
        ld e, (hl)              ; read the OPERAND PTR low byte from the APUPTROutPtr
        inc l                   ; move the POINTER low byte along, 0xFF rollover
        ld d, (hl)              ; read the OPERAND PTR high byte from the APUPTROutPtr
        inc l
        ld (APUPTROutPtr), hl   ; write where the next POINTER should be read

        ld hl, APUPTRBufUsed    ; decrement of POINTER count remaining
        dec (hl)
        dec (hl)

        ld bc, APUDATA+$0300    ; the address of the APU data port in BC
        ex de, hl               ; move the base address of the OPERAND to HL

        outi                    ; output 16 bit OPERAND
        outi

        ld a, (APUStatus)       ; recover the COMMAND status
        cp APU_OP_ENT16         ; is it a 2 byte OPERAND
        jp z, APU_ISR_ENTRY     ; yes? then go back to get another COMMAND

        outi                    ; output last two bytes of 32 bit OPERAND
        outi

        jp APU_ISR_ENTRY        ; go back to get another COMMAND

APU_ISR_OP_REM:
        ld hl, (APUPTROutPtr)   ; get the pointer to where we pop OPERAND PTR
        ld e, (hl)              ; read the OPERAND PTR low byte from the APUPTROutPtr
        inc l                   ; move the POINTER low byte along, 0xFF rollover
        ld d, (hl)              ; read the OPERAND PTR high byte from the APUPTROutPtr
        inc l
        ld (APUPTROutPtr), hl   ; write where the next POINTER should be read

        ld hl, APUPTRBufUsed    ; decrement of OPERAND POINTER count remaining
        dec (hl)
        dec (hl)

        ld bc, APUDATA+$0300    ; the address of the APU data port in BC
        ex de, hl               ; move the base address of the OPERAND to HL

        inc hl                  ; reverse the OPERAND bytes to load

        ld a, (APUStatus)       ; recover the COMMAND status
        cp APU_OP_REM16         ; is it a 2 byte OPERAND
        jr z, APU_ISR_REM16     ; yes then skip

        inc hl                  ; increment two more bytes for 32bit OPERAND
        inc hl
        ind                     ; get the higher two bytes of 32bit OPERAND
        ind

APU_ISR_REM16:
        ind                     ; get 16 bit OPERAND
        ind

        jp APU_ISR_ENTRY        ; go back to get another COMMAND

APU_ISR_ERROR:                  ; we've an error to notify
        ld hl, APUError         ; collect any previous errors
        and (hl)                ; and we add any new error types
        ld (hl), a              ; set the APUError status
        
        ld bc, PIOB             ; output errors onto Port B FIXME
        out (c), a              ; APUStatus onto Port B

        ret                     ; we're done here

;==================================================================================
;
; CODE SECTION
;

;------------------------------------------------------------------------------
;       APU_INIT
;       Initialises the APU buffers

        .ORG APU_LIBRARY        ;CODE ORIGIN FOR YAZ180 DURING TESTING

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

        XOR A                   ; clear A register to 0

        LD (APUCMDBufUsed), A   ; 0 both Buffer counts
        LD (APUPTRBufUsed), A

        LD (APUCMDBuf), A       ; clear COMMAND Buffer
        LD HL, APUCMDBuf
        LD D, H
        LD E, L
        INC DE
        LD BC, APU_CMD_BUFSIZE+1
        LDIR

        LD (APUPTRBuf), A       ; clear OPERAND POINTER Buffer
        LD HL, APUPTRBuf
        LD D, H
        LD E, L
        INC DE
        LD BC, APU_PTR_BUFSIZE+1
        LDIR

        ld (APUStatus), a       ; set APU status to idle (NOP)
        ld (APUError), a        ; clear APU errors
        
        ld a, DCNTL_IWI0|DCNTL_IWI1 ; DMA/Wait Control Reg Set I/O Wait States
        out0 (DCNTL), a         ; 0 Memory Wait & 4 I/O Wait

        ld hl, APU_ISR          ; load our origin into the jump table 
                                ; initially there is a RETN there
        ld (INT_NMI_ADDR), hl   ; load the address of the APU NMI jump

APU_INIT_LOOP:
        ld bc, APUCNTL          ; the address of the APU Control port in bc
        in a, (c)               ; read the APU
        and APU_CNTL_BUSY       ; busy?
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
;       Operand Entry and Removal takes little time,
;       and we'll be interrupted for Command entry.
;       Use after the APU_ISR call.

APU_CHK_IDLE:
        push af
        push bc

APU_CHK_LOOP:
        ld a, (APUStatus)       ; get the status of the APU
        or a                    ; check it is zero (NOP)
        jr nz, APU_CHK_LOOP     ; otherwise wait

        ld bc, APUCNTL          ; the address of the APU control port in bc
        in a, (c)               ; read the APU
        and APU_CNTL_BUSY       ; busy?
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
        ld l, a                 ; store COMMAND so we don't clobber it

        ld a, (APUCMDBufUsed)   ; Get the number of bytes in the COMMAND buffer
        cp APU_CMD_BUFSIZE      ; check whether there is space in the buffer
        jr nc, APU_OP_EXIT      ; COMMAND buffer full, so exit

        ld a, l                 ; recover the operand entry COMMAND
        ld hl, (APUCMDInPtr)    ; get the pointer to where we poke
        ld (hl), a              ; write the COMMAND byte to the APUCMDInPtr   

        inc l                   ; move the COMMAND pointer low byte along, 0xFF rollover
        ld (APUCMDInPtr), hl    ; write where the next byte should be poked

        ld hl, APUCMDBufUsed
        inc (hl)                ; atomic increment of COMMAND count

        ld a, (APUPTRBufUsed)   ; Get the number of bytes in the OPERAND PTR buffer
        cp APU_PTR_BUFSIZE-1    ; check whether there is space for a OPERAND PTR
        jr nc, APU_OP_EXIT      ; buffer full, so exit
        
        ld hl, (APUPTRInPtr)    ; get the pointer to where we poke
        ld (hl), e              ; write the low byte of OPERAND PTR to the APUPTRInPtr   
        inc l                   ; move the POINTER low byte along, 0xFF rollover
        ld (hl), d              ; write the high byte of OPERAND PTR to the APUPTRInPtr   
        inc l
        ld (APUPTRInPtr), hl    ; write where the next POINTER should be poked

        ld hl, APUPTRBufUsed
        inc (hl)                ; increment of OPERAND PTR count
        inc (hl)

APU_OP_EXIT:
        pop hl                  ; recover HL
        ret

;------------------------------------------------------------------------------
;       APU_CMD_LD
;       APU COMMAND in A

APU_CMD_LD:
        push hl                 ; store HL so we don't clobber it
        ld l, a                 ; store COMMAND so we don't clobber it

        ld a, (APUCMDBufUsed)   ; Get the number of bytes in the COMMAND buffer
        cp APU_CMD_BUFSIZE      ; check whether there is space in the buffer
        jr nc, APU_CMD_EXIT     ; COMMAND buffer full, so exit

        ld a, l                 ; recover the operand entry COMMAND
        ld hl, (APUCMDInPtr)    ; get the pointer to where we poke
        ld (hl), a              ; write the COMMAND byte to the APUCMDInPtr   

        inc l                   ; move the COMMAND pointer low byte along, 0xFF rollover
        ld (APUCMDInPtr), hl    ; write where the next byte should be poked

        ld hl, APUCMDBufUsed
        inc (hl)                ; atomic increment of COMMAND count

APU_CMD_EXIT:
        pop hl                  ; recover HL
        ret

;==============================================================================
;
        .END
;
;==============================================================================

