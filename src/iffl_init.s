;-------------------------------------------------------------------------------
; Disposable portion of the IFFL system (routines only needed when initializing)
;-------------------------------------------------------------------------------

                processor 6502

;-------------------------------------------------------------------------------
; Include your loader configuration file by this point!
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Other defines
;-------------------------------------------------------------------------------

        ;Loader defines

RETRIES         = $05                   ;Retries when reading a sector
MAXFILES        = $7f                   ;Max.127 files in the IFFL file
MW_DATA_LENGTH  = $20                   ;Bytes in one M/W command

        ;C64 defines

status          = $90                   ;Kernal zeropage variables
messages        = $9d
fa              = $ba

ciout           = $ffa8                 ;Kernal routines
listen          = $ffb1
second          = $ff93
unlsn           = $ffae
acptr           = $ffa5
chkin           = $ffc6
chkout          = $ffc9
chrin           = $ffcf
chrout          = $ffd2
close           = $ffc3
open            = $ffc0
setmsg          = $ff90
setnam          = $ffbd
setlfs          = $ffba
clrchn          = $ffcc
getin           = $ffe4
load            = $ffd5
save            = $ffd8

        ;1541 defines

drvlentbllo     = $0300                 ;File length lobytes
drvlentblhi     = $0380                 ;File length hibytes
drvbuf          = $0400                 ;Sector data buffer

drvtrklinktbl   = $0420                 ;Track link table for fast scanning
drvsctlinktbl   = $0440                 ;Sector link table for fast scanning

drvtrktbl       = $0300                 ;Start track of files (overwrites lentbl)
drvscttbl       = $0380                 ;Start sector of files (overwrites lentbl)
drvoffstbl      = $0780                 ;Start offset of files

buf1cmd         = $01                   ;Buffer 1 command
buf1trk         = $08                   ;Buffer 1 track
buf1sct         = $09                   ;Buffer 1 sector
buf2cmd         = $02                   ;Buffer 2 command
buf2trk         = $0a                   ;Buffer 2 track
buf2sct         = $0b                   ;Buffer 2 sector
drvtemp         = $0c                   ;Temp variable
drvtemp2        = $0d                   ;Temp variable, IFFL file number
drvcntl         = $0e                   ;IFFL byte counter for scanning
drvcnth         = $0f
iddrv0          = $12                   ;Disk drive ID
id              = $16                   ;Disk ID
buflo           = $30                   ;Buffer address lowbyte
bufhi           = $31                   ;Buffer address highbyte
sectors         = $43                   ;Amount of sectors on current track

returnok        = $f505                 ;Return from job exec with OK code
waitsync        = $f556                 ;Wait for SYNC
decode          = $f7e8                 ;Decode 5 GCR bytes, bufferindex in Y
initialize      = $d005                 ;Initialize routine (for return to diskdrive OS)

;-------------------------------------------------------------------------------
; INITLOADER
;
; Uploads the IFFL drivecode to disk drive memory and starts it.
;
; Remarks: If the IFFL loader requires relocation to page 2 and/or 3, then
;          call initloader1, relocate the loader, and then call initloader2.
;
; Parameters: -
; Returns: C=0 IFFL initialized OK
;          C=1 Error
; Modifies: A,X,Y
;-------------------------------------------------------------------------------

initloader1:
                lda #$4c                ;OP for JMP: disable execution of the
                sta il_retearly         ;second half of the initialization as
                                        ;the IFFL loader cannot be relocated yet

initloader:     lda #<il_nmi            ;Set NMI vector (let NMI trigger once
                sta $0318               ;so RESTORE keypresses won't interfere
                sta $fffa               ;with loading and PAL/NTSC detection)
                lda #>il_nmi
                sta $0319
                sta $fffb
                lda #$81
                sta $dd0d               ;Timer A interrupt source
                lda #$01                ;Timer A count ($0001)
                sta $dd04
                lda #$00
                sta $dd05
                lda #%00011001          ;Run Timer A in one-shot mode
                sta $dd0e

                lda #<drivecode_c64
                ldx #>drivecode_c64
                ldy #(drivecodeend_drv-drivecode_drv+MW_DATA_LENGTH-1)/MW_DATA_LENGTH
il_begin:       sta il_senddata+1
                stx il_senddata+2
                sty loadtempreg         ;Number of "packets" to send
                lda #>drivecode_drv
                sta il_mwstring+1
                ldy #$00
;                sty il_mwstring+2       ;Drivecode starts at lowbyte 0
                beq il_nextpacket
il_sendmw:      lda il_mwstring,x       ;Send M-W command (backwards)
                jsr ciout
                dex
                bpl il_sendmw
                ldx #MW_DATA_LENGTH
il_senddata:    lda drivecode_c64,y     ;Send one byte of drivecode
                jsr ciout
                iny
                bne il_notover
                inc il_senddata+2
il_notover:     inc il_mwstring+2       ;Also, move the M-W pointer forward
                bne il_notover2
                inc il_mwstring+1
il_notover2:    dex
                bne il_senddata
                jsr unlsn               ;Unlisten to perform the command
il_nextpacket:  lda fa                  ;Set drive to listen
                jsr listen
                lda #$6f
                jsr second
                ldx #$05
                dec loadtempreg         ;All "packets" sent?
                bpl il_sendmw
                dex
il_sendme:      lda il_mestring,x       ;Send M-E command (backwards)
                jsr ciout
                dex
                bpl il_sendme
il_retearly:    jsr unlsn               ;Start drivecode

initloader2:
                if TWOBIT_PROTOCOL=0
il_wait:        bit $dd00               ;Wait for 1541 to signal drv_init
                bvs il_wait             ;started by setting CLK low
                else
                sei                     ;Sokrates' variant for video std detection
                lda $d011               ;Make sure NMI was caught first
                and #$10
                bne il_detect
                lda #$8e                ;Correct cycle count if screen is blanked
                sta il_midcycles+1
il_detect:      ldx #$00
il_waitraster0: lda $d012
il_waitraster1: cmp $d012
                beq il_waitraster1
                bmi il_waitraster0
                and #$03
                cmp #$03
                bne il_isntsc
                tay
il_countcycles: inx
                lda $d012
                bpl il_countcycles
il_midcycles:   cpx #$5e                ;VICE values: $6C for Drean, $50 for PAL
                                        ;(with screen blanked: $9C and $80)
                bcs il_isdrean          ;so choose middle value, $5E
                iny
                lda #$30                ;Adjust 2-bit fastload transfer
                sta getbyte_delay       ;delay only for PAL
il_isdrean:     tya                     ;$03 for Drean, $04 for PAL
il_isntsc:      cli                     ;$01 for old NTSC, $02 for NTSC
                endif

                jsr getbyte             ;Get a byte from the drive
                cmp #$02                ;Error or OK?
                rts

il_nmi:         rti

;-------------------------------------------------------------------------------
; M-W and M-E command strings
;-------------------------------------------------------------------------------

il_mwstring:    dc.b MW_DATA_LENGTH,$00,$00,"W-M"

il_mestring:    dc.b >drv_init, <drv_init, "E-M"

;-------------------------------------------------------------------------------
; Drivecode
;-------------------------------------------------------------------------------

drivecode_c64:
                rorg $500
drivecode_drv:

;-------------------------------------------------------------------------------
; Subroutine: scan the IFFL file (executed via jobcode $e0)
;-------------------------------------------------------------------------------

drv_scan:       lda #>drvbuf
                sta bufhi
                lda sectors
                sta drvtemp
drv_scansector: lda #<drvbuf            ;Read sector header to $0400
                jsr drv_scan5bytes
                lda drvbuf              ;Check for correct header
                cmp #$52
                bne drv_scansector
                lda #<drvbuf+5          ;Read data beginning to $0405
                jsr drv_scan5bytes
                ldy #$00
                sty buflo
                jsr decode              ;Decode the header
                lda $54                 ;Take sectornumber
                pha
                ldy #$05
                jsr decode              ;Decode the data beginning
                pla
                tax
                lda $53                 ;Store T/S link to our linktable
                sta drvtrklinktbl,x     ;so that we can "virtually" loop
                lda $54                 ;through the sectors later
                sta drvsctlinktbl,x
                dec drvtemp             ;Loop until all sectors scanned
                bne drv_scansector

drv_scanfile:   ldy drvtemp2            ;Current file number
                lda drvcnth             ;See if the byte counter is less than
                bne drv_scannext        ;254, otherwise go to next sector
                lda drvcntl
                cmp #254
                bcs drv_scannext
drv_scanfileok: sta drvoffstbl,y        ;Store file offset
                adc drvlentbllo,y       ;Now add this file's length to the
                sta drvcntl             ;byte counter
                lda drvcnth
                adc drvlentblhi,y
                sta drvcnth
                lda buf2trk             ;Store file track/sector
                sta drvtrktbl,y         ;(file length gets overwritten but
                lda buf2sct             ;it's no longer needed at this point)
                sta drvscttbl,y
                inc drvtemp2            ;Increment file counter
                bpl drv_scanfile        ;Fill up the table, then exit
                lda #$00
                beq drv_scandone

drv_scannext:   lda drvcntl             ;Now subtract 254 bytes from the counter
                sec                     ;as we go to the next sector
                sbc #254
                sta drvcntl
                bcs drv_scannextok
                dec drvcnth
drv_scannextok: ldx buf2sct
                lda drvsctlinktbl,x     ;Get next sector from our linktable
                sta buf2sct
                lda drvtrklinktbl,x     ;Get next track from our linktable
                cmp buf2trk
                beq drv_scanfile        ;If same track, go back to loop for
drv_scandone:   sta buf2trk             ;files
                jmp returnok            ;Otherwise, have to return from the
                                        ;job execution, and execute again
drv_scan5bytes: sta buflo
                jsr waitsync            ;Wait for SYNC (clears Y)
drv_5byteloop:  bvc drv_5byteloop       ;Wait for data from the R/W head
                clv
                lda $1c01
                sta (buflo),y
                iny
                cpy #$05                ;Read 5 GCR bytes
                bne drv_5byteloop
                rts

;-------------------------------------------------------------------------------
; Drive main code
;-------------------------------------------------------------------------------

drv_init:
                if TWOBIT_PROTOCOL=0
                lda #$08                ;Set CLK=low to tell C64 there's no data to
                sta $1800               ;be read yet
                endif

                lda #18                 ;Read first dir sector
                ldx #1
drv_dirsctloop: jsr drv_readsector
                bcs drv_initfail
                ldy #$02
drv_fileloop:   lda drvbuf,y            ;File type must be PRG
                and #$83
                cmp #$82
                bne drv_nextfile
                sty drv_namecmp+1
                ldx #$03
                lda #$a0                ;Make an endmark at the 16th letter
                sta drvbuf+19,y
drv_namecmp:    lda drvbuf,x
                cmp drv_filename-3,x    ;Check against each letter of filename
                bne drv_namedone        ;until at the endzero
                inx
                bne drv_namecmp
drv_namedone:   cmp #$a0                ;If got to a $a0, name correct
                beq drv_found
drv_nextfile:   tya
                clc
                adc #$20                ;Go to next file
                tay
                bcc drv_fileloop
drv_nextdirsct: ldx drvbuf+1
                lda drvbuf              ;Any more dir sectors?
                bne drv_dirsctloop      ;Errorcode $10 not used by 1541
                lda #$10                ;so use it as "IFFL file not found"
drv_initfail:   jsr drv_sendbyte        ;Send error code & exit through
drv_quit:       jmp initialize          ;INITIALIZE

drv_found:      lda drvbuf+1,y          ;IFFL datafile found, get its start
                ldx drvbuf+2,y          ;track & sector
                jsr drv_readsector
                bcs drv_initfail
                ldy #MAXFILES-1
drv_copylentbl: lda drvbuf+2,y          ;First sector contains the file lengths.
                sta drvlentbllo,y       ;Copy them to the length tables
                lda drvbuf+2+MAXFILES,y
                sta drvlentblhi,y
                dey
                bpl drv_copylentbl
                lda #$00                ;Clear the length of the last file
                sta drvlentbllo+MAXFILES ;in case we have full 127 files
                sta drvlentblhi+MAXFILES
                sta drvtemp2            ;Clear the scanning file counter
                sta drvcntl             ;Clear the 16bit IFFL byte counter
;                sta drvcnth             ;TODO: Already 0 after a reset
                lda drvbuf              ;Now get the next T/S (where actual
                sta buf2trk             ;data starts) and perform the scanning
                lda drvbuf+1            ;with the seek/execute jobcode
                sta buf2sct             ;(drv_scan at $500 gets executed)
drv_scanloop:   lda #$e0
                sta buf2cmd
                cli
drv_scanwait:   lda buf2cmd
                bmi drv_scanwait
                sei
                cmp #$02                ;If error, abort
                bcs drv_initfail
                lda buf2trk             ;Keep calling the job until the file is
                bne drv_scanloop        ;at an end
                jsr drv_sendbyte        ;Now A=0, send the byte so that C64
                                        ;knows the file was scanned successfully

drv_mainloop:
                if TWOBIT_PROTOCOL=0
                lda $1800               ;Set CLK=High
                and #$f7
                sta $1800
                lda #$04
drv_mainfilewait:
                bit $1800               ;Wait for CLK=High
                bne drv_mainfilewait
                ldy #$00                ;Set DATA=High
                sty $1800
                endif

                cli                     ;Allow interrupts so drive may stop
                jsr drv_getbyte         ;Get file number from C64
                tay                     ;(file number also now in drvtemp2)

                if TWOBIT_PROTOCOL=0
                lda #$08                ;Set CLK=low to tell C64 there's no data to
                sta $1800               ;be read yet
                endif

                lda drvoffstbl,y        ;Get file start offset
                sta drv_mainsendstart+1
                lda drvtrktbl,y         ;Get file start track & sector
                sta buf1trk
                lda drvscttbl,y
                sta buf1sct

drv_mainsctloop:ldy drvtemp2            ;Get the file number back
                ldx #$fe                ;Assume we'll send a full sector (254b.)
                lda buf1trk             ;If we're on the startsector of the
                cmp drvtrktbl+1,y       ;next file, we can only send up to the
                bne drv_mainnotlast     ;next file's startoffset
                lda buf1sct
                cmp drvscttbl+1,y
                bne drv_mainnotlast
                ldx drvoffstbl+1,y      ;If endoffset = startoffset, we're
                cpx drv_mainsendstart+1 ;already on the next file and can't
                beq drv_mainfiledone    ;send anything
drv_mainnotlast:stx drv_mainsendend+1
                jsr drv_readsector2     ;Read sector, abort if failed
                bcs drv_mainfilefail

                if RECEIVE_BUFFER=0
                lda drv_mainsendend+1
                cmp #$fe
                php
                sec
                sbc drv_mainsendstart+1 ;Get amount of bytes to send
                jsr drv_sendbyte
drv_mainsendstart:
                ldy #$00
drv_mainsendloop:
                iny
                lda drvbuf+1,y          ;Send buffer
                jsr drv_sendbyte
drv_mainsendend:
                cpy #$00
                bne drv_mainsendloop
                plp                     ;See if it was a partial sector
                bne drv_mainfiledone    ;(last one)

                else

drv_mainsendend:ldy #$00
                cpy #$fe
                php
                tya
                sec
                sbc drv_mainsendstart+1 ;Get amount of bytes to send
                jsr drv_sendbyte
drv_mainsendloop:
                lda drvbuf+1,y          ;Send buffer backwards
                jsr drv_sendbyte
                dey
drv_mainsendstart:
                cpy #$00
                bne drv_mainsendloop
                plp                     ;See if it was a partial sector
                bne drv_mainfiledone    ;(last one)
                endif

drv_mainnextsct:lda #$00
                sta drv_mainsendstart+1 ;Startoffset for next sector is 0
                lda drvbuf+1            ;Follow the T/S link
                sta buf1sct
                lda drvbuf
                sta buf1trk             ;Go back to send the next sector,
                bne drv_mainsctloop     ;unless IFFL file end encountered
drv_mainfiledone:
                lda #$00                ;Errorcode 0: all OK
drv_mainfilefail:
                pha                     ;File end: send $00 and errorcode
                lda #$00
                jsr drv_sendbyte
                pla
                jsr drv_sendbyte

                jmp drv_mainloop        ;Then go back to main to wait for
                                        ;file number

;-------------------------------------------------------------------------------
; Subroutine: send byte in A to C64; with 2-bit protocol no IRQs are allowed.
;-------------------------------------------------------------------------------

drv_sendbyte:
                if TWOBIT_PROTOCOL>0
                sta drvtemp
                lsr
                lsr
                lsr
                lsr
                tax
                lda #$04
drv_sendwait:   bit $1800               ;Wait for CLK==low
                beq drv_sendwait
                lsr                     ;Set DATA=low
                sta $1800
                lda drv_sendtbl,x       ;Get the CLK,DATA pairs for low nybble
                pha
                lda drvtemp
                and #$0f
                tax
                lda #$04
drv_sendwait2:  bit $1800               ;Wait for CLK==high (start of high speed transfer)
                bne drv_sendwait2
                lda drv_sendtbl,x       ;Get the CLK,DATA pairs for high nybble
                sta $1800
                asl
                and #$0f
                sta $1800
                pla
                sta $1800
                asl
                and #$0f
                sta $1800
                nop
                nop
                lda #$00
                sta $1800               ;Finish send: DATA & CLK both high

                else

                sta drvtemp             ;Store the byte to a temp variable
                tya                     ;Store Y-register contents
                pha
                ldy #$04
                lda $1800
                and #$f7
                sta $1800
                tya
s1:             asl drvtemp             ;Rotate bit to carry and "invert"
                ldx #$02
                bcc s2
                ldx #$00
s2:             bit $1800
                bne s2
                stx $1800
                asl drvtemp
                ldx #$02
                bcc s3
                ldx #$00
s3:             bit $1800
                beq s3
                stx $1800
                dey
                bne s1
                txa
                ora #$08
                sta $1800
                pla
                tay
                endif

                rts

;-------------------------------------------------------------------------------
; 2-bit send table
;-------------------------------------------------------------------------------

                if TWOBIT_PROTOCOL>0
drv_sendtbl:    dc.b $0f,$07,$0d,$05
                dc.b $0b,$03,$09,$01
                dc.b $0e,$06,$0c,$04
                dc.b $0a,$02,$08,$00
                endif

;-------------------------------------------------------------------------------
; Subroutine: get byte from C64 in A
;-------------------------------------------------------------------------------

drv_getbyte:    ldy #$08                ;Counter: receive 8 bits
drv_recvbit:    lda #$85
                and $1800               ;Wait for CLK==low || DATA==low
                bmi drv_gotatn          ;Quit if ATN was asserted
                beq drv_recvbit
                lsr                     ;Read the data bit
                lda #2                  ;Prepare for CLK=high, DATA=low
                bcc drv_rskip
                lda #8                  ;Prepare for CLK=low, DATA=high
drv_rskip:      sta $1800               ;Acknowledge the bit received
                ror drvtemp2            ;and store it
drv_rwait:      lda $1800               ;Wait for CLK==high || DATA==high
                and #5
                eor #5
                beq drv_rwait
                lda #0
                sta $1800               ;Set CLK=DATA=high
                dey
                bne drv_recvbit         ;Loop until all bits have been received
                lda drvtemp2            ;Return the data to A
                rts
drv_gotatn:     pla                     ;If ATN gets asserted, exit to the operating
                pla                     ;system. Discard the return address and
                jmp drv_quit            ;jump to the INITIALIZE routine

;-------------------------------------------------------------------------------
; Subroutine: read sector
;-------------------------------------------------------------------------------

drv_readsector: sta buf1trk
                stx buf1sct
drv_readsector2:
                if LED_FLASHING>0
                jsr drv_led
                endif
                ldy #RETRIES            ;Retry counter
drv_readsectorretry:
                lda #$80
                sta buf1cmd
                cli
drv_readsectorpoll:
                lda buf1cmd
                bmi drv_readsectorpoll
                sei
                cmp #$02                ;Errorcode
                bcc drv_readsectorok
                ldx id                  ;Handle possible disk ID change
                stx iddrv0
                ldx id+1
                stx iddrv0+1
                dey                     ;Decrement retry counter and try again
                bne drv_readsectorretry
drv_readsectorok:
                if LED_FLASHING>0
drv_led:        lda #$08                ;Flash the drive LED
drv_ledac1:     eor $1c00
drv_ledac2:     sta $1c00
                endif
                rts

;-------------------------------------------------------------------------------
; IFFL filename
;-------------------------------------------------------------------------------

drv_filename:   dc.b "IFFLDATA",0

drivecodeend_drv:
                rend
drivecodeend_c64:
