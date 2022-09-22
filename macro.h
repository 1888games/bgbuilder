

            MAC SLEEP            ;usage: SLEEP n (n>1)
.CYCLES     SET {1}

                IF .CYCLES < 2
                    ECHO "MACRO ERROR: 'SLEEP': Duration must be > 1"
                    ERR
                ENDIF

                IF .CYCLES & 1
                    IFNCONST NO_ILLEGAL_OPCODES
                        nop 0
                    ELSE
                        bit VSYNC
                    ENDIF
.CYCLES             SET .CYCLES - 3
                ENDIF
            
                REPEAT .CYCLES / 2
                    nop
                REPEND
            ENDM

;-------------------------------------------------------------------------------
; VERTICAL_SYNC
; revised version by Edwin Blink -- saves bytes!
; Inserts the code required for a proper 3 scanline vertical sync sequence
; Note: Alters the accumulator

; OUT: A = 0

             MAC VERTICAL_SYNC
                lda #%1110          ; each '1' bits generate a VSYNC ON line (bits 1..3)
.VSLP1          sta WSYNC           ; 1st '0' bit resets Vsync, 2nd '0' bit exit loop
                sta VSYNC
                lsr
                bne .VSLP1          ; branch until VYSNC has been reset
             ENDM

;-------------------------------------------------------------------------------
; CLEAN_START
; Original author: Andrew Davie
; Standardised start-up code, clears stack, all TIA registers and RAM to 0
; Sets stack pointer to $FF, and all registers to 0
; Sets decimal mode off, sets interrupt flag (kind of un-necessary)
; Use as very first section of code on boot (ie: at reset)
; Code written to minimise total ROM usage - uses weird 6502 knowledge :)

            MAC CLEAN_START
                sei
                cld
            
                ldx #0
                txa
                tay
.CLEAR_STACK    dex
                txs
                pha
                bne .CLEAR_STACK     ; SP=$FF, X = A = Y = 0

            ENDM

;-------------------------------------------------------
; SET_POINTER
; Original author: Manuel Rotschkar
;
; Sets a 2 byte RAM pointer to an absolute address.
;
; Usage: SET_POINTER pointer, address
; Example: SET_POINTER SpritePTR, SpriteData
;
; Note: Alters the accumulator, NZ flags
; IN 1: 2 byte RAM location reserved for pointer
; IN 2: absolute address

            MAC SET_POINTER
.POINTER    SET {1}
.ADDRESS    SET {2}

                LDA #<.ADDRESS  ; Get Lowbyte of Address
                STA .POINTER    ; Store in pointer
                LDA #>.ADDRESS  ; Get Hibyte of Address
                STA .POINTER+1  ; Store in pointer+1

            ENDM

;-------------------------------------------------------
; BOUNDARY byte#
; Original author: Denis Debro (borrowed from Bob Smith / Thomas)
;
; Push data to a certain position inside a page and keep count of how
; many free bytes the programmer will have.
;
; eg: BOUNDARY 5    ; position at byte #5 in page

.FREE_BYTES SET 0   
   MAC BOUNDARY
      REPEAT 256
         IF <. % {1} = 0
            MEXIT
         ELSE
.FREE_BYTES SET .FREE_BYTES + 1
            .byte $00
         ENDIF
      REPEND
   ENDM

;-------------------------------------------------------
; SKIP_SCANLINES #lines
;
; Skip a given # of scanlines.
; Sets the X register to zero.

	MAC SKIP_SCANLINES
.LINES  SET {1}
	ldx #.LINES
.vblank	sta WSYNC
	dex
	bne .vblank
	ENDM

; EOF
