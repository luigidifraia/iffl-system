;-------------------------------------------------------------------------------
; Disposable portion of the IFFL system (routines only needed when initializing)
;-------------------------------------------------------------------------------

                processor 6502

;-------------------------------------------------------------------------------
; Make sure you included your loader configuration file by this point!
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
talk            = $ffb4
tksa            = $ff96
untlk           = $ffab
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

        ;Common drive defines

drvlentbllo     = $0300                 ;File length lobytes
drvlentblhi     = drvlentbllo+MAXFILES+1 ;File length hibytes
drvbuf          = $0400                 ;Sector data buffer
drvstart        = $0500                 ;Start of drivecode

drvtrktbl       = drvlentbllo           ;Start track of files (overwrites lentbl)
drvscttbl       = drvtrktbl+MAXFILES+1  ;Start sector of files (overwrites lentbl)

drvoffstbl      = $0780                 ;Start offset of files

;-------------------------------------------------------------------------------
; INITLOADER
;
; Uploads the IFFL drivecode to disk drive memory and starts it.
;
; Remarks: If the IFFL loader requires relocation to page 2 and/or 3, then
;          call initloader1, relocate the loader, and then call initloader2.
;
; Pre: Kernal disk routines expect RS-232 interrupts to be disabled ($02a1 
;      = 0x00) and the STOP key flag to signal no key pressed ($91 = 0xff)
;
; Parameters: -
; Returns: C=0 IFFL initialized OK
;          C=1 Error
; Modifies: A,X,Y
;-------------------------------------------------------------------------------

initloader1:    lda #<il_nmi            ;Set NMI vector (let NMI trigger once
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

il_detectdrive: lda #<il_drivecode
                ldx #>il_drivecode
                ldy #(il_driveend-il_drivecode+MW_DATA_LENGTH-1)/MW_DATA_LENGTH
                jsr il_begin             ;Upload test-drivecode
                ;lda status              ;If serial error here, not a
                ;cmp #$c0                ;serial device
                ;beq il_noserial
                ldx #$00
                ldy #$00
il_delay:       inx                     ;Delay to make sure the test-
                bne il_delay            ;drivecode executed to the end
                iny
                bpl il_delay
                lda fa                  ;Set drive to listen
                jsr listen
                lda #$6f
                jsr second
                ldx #$05
il_ddsendmr:    lda il_mrstring,x       ;Send M-R command (backwards)
                jsr ciout
                dex
                bpl il_ddsendmr
                jsr unlsn
                lda fa
                jsr talk
                lda #$6f
                jsr tksa
                lda #$00
                jsr acptr               ;First byte: test value
                pha
                jsr acptr               ;Second byte: drive type
                tax
                jsr untlk
                pla
                cmp #$aa                ;Drive can execute code, so can
                beq il_fastloadok       ;use fastloader
il_unsupported: rts

il_fastloadok:  cpx #$01
                bcs il_drv2mhz

                lda #<dr_init_1mhz
                sta il_mestring+1
                lda #>dr_init_1mhz
                sta il_mestring

                lda #<drivecode_c64_drv_1mhz
                ldx #>drivecode_c64_drv_1mhz
                ldy #(drivecodeend_drv_1mhz-drivecode_drv_1mhz+MW_DATA_LENGTH-1)/MW_DATA_LENGTH
                bne il_supported

il_drv2mhz:     cpx #$03
                bcs il_unsupported

                lda #<dr_init_2mhz
                sta il_mestring+1
                lda #>dr_init_2mhz
                sta il_mestring

                cpx #$02
                bcc il_drv1581

                ldx #$a5                ;Patch dir start track/sector for FD2000
                stx drv_dirtrk-drvstart+drivecode_c64_drv_2mhz
                inx
                stx drv_dirsct-drvstart+drivecode_c64_drv_2mhz
                lda #$54
                sta drv_dirtrk+1-drvstart+drivecode_c64_drv_2mhz
                lda #$56
                sta drv_dirsct+1-drvstart+drivecode_c64_drv_2mhz

il_drv1581:     lda #<drivecode_c64_drv_2mhz
                ldx #>drivecode_c64_drv_2mhz
                ldy #(drivecodeend_drv_2mhz-drivecode_drv_2mhz+MW_DATA_LENGTH-1)/MW_DATA_LENGTH

il_supported:   inc usefastload

il_begin:       sta il_senddata+1
                stx il_senddata+2
                sty loadtempreg         ;Number of "packets" to send
                lda #>drvstart
                sta il_mwstring+1
                ldy #$00
                sty il_mwstring+2       ;Drivecode starts at lowbyte 0
                beq il_nextpacket
il_sendmw:      lda il_mwstring,x       ;Send M-W command (backwards)
                jsr ciout
                dex
                bpl il_sendmw
                ldx #MW_DATA_LENGTH
il_senddata:    lda $bdbd,y             ;Send one byte of drivecode
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
                jmp unlsn               ;Start drivecode

initloader:     jsr initloader1

initloader2:    lda usefastload
                bne il_ok
                lda #$11
                sec
                rts

il_ok:
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

il_mestring:    dc.b >drvstart, <drvstart, "E-M"

;-------------------------------------------------------------------------------
; IL_DRIVECODE - Drivecode used to detect drive type & test if drivecode
; execution works OK
;-------------------------------------------------------------------------------

il_drivecode:
                rorg drvstart

                asl ild_return1         ;Modify first returnvalue to prove
                                        ;we've executed something :)
                lda $fea0               ;Recognize drive family
                ldx #3                  ;(from Dreamload)
ild_floop:      cmp ild_family-1,x
                beq ild_ffound
                dex                     ;If unrecognized, assume 1541
                bne ild_floop
                beq ild_idfound
ild_ffound:     lda ild_idloclo-1,x
                sta ild_idlda+1
                lda ild_idlochi-1,x
                sta ild_idlda+2
ild_idlda:      lda $fea4               ;Recognize drive type
                ldx #3                  ;3 = CMD HD
ild_idloop:     cmp ild_id-1,x          ;2 = CMD FD
                beq ild_idfound         ;1 = 1581
                dex                     ;0 = 1541
                bne ild_idloop
ild_idfound:    stx ild_return2
                rts

ild_family:     dc.b $43,$0d,$ff
ild_idloclo:    dc.b $a4,$c6,$e9
ild_idlochi:    dc.b $fe,$e5,$a6
ild_id:         dc.b "8","F","H"

ild_return1:    dc.b $55
ild_return2:    dc.b 0

                rend

il_driveend:

;-------------------------------------------------------------------------------
; M-R command string
;-------------------------------------------------------------------------------

il_mrstring:    dc.b 2,>ild_return1,<ild_return1,"R-M"

;-------------------------------------------------------------------------------
; Loader configuration
;-------------------------------------------------------------------------------

usefastload:    dc.b 0                          ;If nonzero, fastloading will
                                                ;be used (autoconfigured)

;-------------------------------------------------------------------------------
; Drive type-specific defines
;-------------------------------------------------------------------------------

                subroutine drv_1mhz

        ;1541/1571 defines

.drvtrklinktbl  = $0420                 ;Track link table for fast scanning
.drvsctlinktbl  = $0440                 ;Sector link table for fast scanning

.buf1cmd        = $01                   ;Buffer 1 command
.buf1trk        = $08                   ;Buffer 1 track
.buf1sct        = $09                   ;Buffer 1 sector
.buf2cmd        = $02                   ;Buffer 2 command
.buf2trk        = $0a                   ;Buffer 2 track
.buf2sct        = $0b                   ;Buffer 2 sector
.drvtemp        = $0c                   ;Temp variable
.drvtemp2       = $0d                   ;Temp variable, IFFL file number
.drvcntl        = $0e                   ;IFFL byte counter for scanning
.drvcnth        = $0f
.iddrv0         = $12                   ;Disk drive ID
.id             = $16                   ;Disk ID
.buflo          = $30                   ;Buffer address lowbyte
.bufhi          = .buflo+1              ;Buffer address highbyte
.sectors        = $43                   ;Amount of sectors on current track

.busport        = $1800

returnok        = $f505                 ;Return from job exec with OK code
waitsync        = $f556                 ;Wait for SYNC
decode          = $f7e8                 ;Decode 5 GCR bytes, bufferindex in Y
initialize      = $d005                 ;Initialize routine (for return to diskdrive OS)

;-------------------------------------------------------------------------------
; Drivecode for 1MHz drives
;-------------------------------------------------------------------------------

drivecode_c64_drv_1mhz:
                rorg drvstart
drivecode_drv_1mhz:

;-------------------------------------------------------------------------------
; Subroutine: scan the IFFL file (executed via jobcode $e0)
;-------------------------------------------------------------------------------

.dr_scan:       lda #>drvbuf
                sta .bufhi
                lda .sectors
                sta .drvtemp
.dr_scansector: lda #<drvbuf            ;Read sector header to $0400
                jsr .dr_scan5bytes
                lda drvbuf              ;Check for correct header
                cmp #$52
                bne .dr_scansector
                lda #<drvbuf+5          ;Read data beginning to $0405
                jsr .dr_scan5bytes
                ldy #$00
                sty .buflo
                jsr decode              ;Decode the header
                lda $54                 ;Take sectornumber
                pha
                ldy #$05
                jsr decode              ;Decode the data beginning
                pla
                tax
                lda $53                 ;Store T/S link to our linktable
                sta .drvtrklinktbl,x    ;so that we can "virtually" loop
                lda $54                 ;through the sectors later
                sta .drvsctlinktbl,x
                dec .drvtemp            ;Loop until all sectors scanned
                bne .dr_scansector

.dr_scanfile:   ldy .drvtemp2           ;Current file number
                lda .drvcnth            ;See if the byte counter is less than
                bne .dr_scannext        ;254, otherwise go to next sector
                lda .drvcntl
                cmp #254
                bcs .dr_scannext
.dr_scanfileok: sta drvoffstbl,y        ;Store file offset
                adc drvlentbllo,y       ;Now add this file's length to the
                sta .drvcntl            ;byte counter
                lda .drvcnth
                adc drvlentblhi,y
                sta .drvcnth
                lda .buf2trk            ;Store file track/sector
                sta drvtrktbl,y         ;(file length gets overwritten but
                lda .buf2sct            ;it's no longer needed at this point)
                sta drvscttbl,y
                inc .drvtemp2           ;Increment file counter
                bpl .dr_scanfile        ;Fill up the table, then exit
                lda #$00
                beq .dr_scanok

.dr_scannext:   lda .drvcntl            ;Now subtract 254 bytes from the counter
                sec                     ;as we go to the next sector
                sbc #254
                sta .drvcntl
                bcs .dr_scannextok
                dec .drvcnth
.dr_scannextok: ldx .buf2sct
                lda .drvsctlinktbl,x    ;Get next sector from our linktable
                sta .buf2sct
                lda .drvtrklinktbl,x    ;Get next track from our linktable
                cmp .buf2trk
                beq .dr_scanfile        ;If same track, go back to loop for
.dr_scanok:     sta .buf2trk            ;files
                jmp returnok            ;Otherwise, have to return from the
                                        ;job execution, and execute again
.dr_scan5bytes: sta .buflo
                jsr waitsync            ;Wait for SYNC (clears Y)
.dr_5byteloop:  bvc .dr_5byteloop       ;Wait for data from the R/W head
                clv
                lda $1c01
                sta (.buflo),y
                iny
                cpy #$05                ;Read 5 GCR bytes
                bne .dr_5byteloop
                rts

;-------------------------------------------------------------------------------
; Drive main code
;-------------------------------------------------------------------------------

dr_init_1mhz:
                if TWOBIT_PROTOCOL=0
                lda #$08                ;Set CLK=low to tell C64 there's no data to
                sta .busport            ;be read yet
                endif

                lda #18                 ;Read first dir sector
                ldx #1
.dr_dirsctloop: jsr .dr_readsector
                bcs .dr_initfail
                ldy #$02
.dr_fileloop:   lda drvbuf,y            ;File type must be PRG
                and #$83
                cmp #$82
                bne .dr_nextfile
                sty .dr_namecmp+1
                ldx #$03
                lda #$a0                ;Make an endmark at the 16th letter
                sta drvbuf+19,y
.dr_namecmp:    lda drvbuf,x
                cmp .dr_filename-3,x    ;Check against each letter of filename
                bne .dr_namedone        ;until at the endzero
                inx
                bne .dr_namecmp
.dr_namedone:   cmp #$a0                ;If got to a $a0, name correct
                beq .dr_found
.dr_nextfile:   tya
                clc
                adc #$20                ;Go to next file
                tay
                bcc .dr_fileloop
.dr_nextdirsct: ldx drvbuf+1
                lda drvbuf              ;Any more dir sectors?
                bne .dr_dirsctloop      ;Errorcode $10 not used by 1541
                lda #$10                ;so use it as "IFFL file not found"
.dr_initfail:   jsr .dr_sendbyte        ;Send error code & exit through
.dr_quit:       jmp initialize          ;INITIALIZE

.dr_found:      lda drvbuf+1,y          ;IFFL datafile found, get its start
                ldx drvbuf+2,y          ;track & sector
                jsr .dr_readsector
                bcs .dr_initfail
                ldy #MAXFILES-1
.dr_copylentbl: lda drvbuf+2,y          ;First sector contains the file lengths.
                sta drvlentbllo,y       ;Copy them to the length tables
                lda drvbuf+2+MAXFILES,y
                sta drvlentblhi,y
                dey
                bpl .dr_copylentbl
                lda #$00                ;Clear the length of the last file
                sta drvlentbllo+MAXFILES ;in case we have full 127 files
                sta drvlentblhi+MAXFILES
                sta .drvtemp2           ;Clear the scanning file counter
                sta .drvcntl            ;Clear the 16bit IFFL byte counter
                sta .drvcnth
                lda drvbuf              ;Now get the next T/S (where actual
                sta .buf2trk            ;data starts) and perform the scanning
                lda drvbuf+1
                sta .buf2sct
.dr_scanloop:   jsr .dr_scanexec
                bcs .dr_initfail        ;If error, abort
                lda .buf2trk            ;Keep calling the job until the file is
                bne .dr_scanloop        ;at an end
                jsr .dr_sendbyte        ;Now A=0, send the byte so that C64
                                        ;knows the file was scanned successfully

.dr_mainloop:
                if TWOBIT_PROTOCOL=0
                lda .busport            ;Set CLK=High
                and #$f7
                sta .busport
                lda #$04
.dr_mainfilewait:
                bit .busport            ;Wait for CLK=High
                bne .dr_mainfilewait
                ldy #$00                ;Set DATA=High
                sty .busport
                endif

                cli                     ;Allow interrupts so drive may stop
                jsr .dr_getbyte         ;Get file number from C64
                tay                     ;(file number also now in .drvtemp2)

                if TWOBIT_PROTOCOL=0
                lda #$08                ;Set CLK=low to tell C64 there's no data to
                sta .busport            ;be read yet
                endif

                lda drvoffstbl,y        ;Get file start offset
                sta .dr_mainsendstart+1
                lda drvtrktbl,y         ;Get file start track & sector
                sta .buf1trk
                lda drvscttbl,y
                sta .buf1sct

.dr_mainsctloop:ldy .drvtemp2           ;Get the file number back
                ldx #$fe                ;Assume we'll send a full sector (254b.)
                lda .buf1trk            ;If we're on the startsector of the
                cmp drvtrktbl+1,y       ;next file, we can only send up to the
                bne .dr_mainnotlast     ;next file's startoffset
                lda .buf1sct
                cmp drvscttbl+1,y
                bne .dr_mainnotlast
                ldx drvoffstbl+1,y      ;If endoffset = startoffset, we're
                cpx .dr_mainsendstart+1 ;already on the next file and can't
                beq .dr_mainfiledone    ;send anything
.dr_mainnotlast:stx .dr_mainsendend+1
                jsr .dr_readsector2     ;Read sector, abort if failed
                bcs .dr_mainfilefail

                if RECEIVE_BUFFER=0
                lda .dr_mainsendend+1
                cmp #$fe
                php
                sec
                sbc .dr_mainsendstart+1 ;Get amount of bytes to send
                jsr .dr_sendbyte
.dr_mainsendstart:
                ldy #$00
.dr_mainsendloop:
                iny
                lda drvbuf+1,y          ;Send buffer
                jsr .dr_sendbyte
.dr_mainsendend:
                cpy #$00
                bne .dr_mainsendloop
                plp                     ;See if it was a partial sector
                bne .dr_mainfiledone    ;(last one)

                else

.dr_mainsendend:ldy #$00
                cpy #$fe
                php
                tya
                sec
                sbc .dr_mainsendstart+1 ;Get amount of bytes to send
                jsr .dr_sendbyte
.dr_mainsendloop:
                lda drvbuf+1,y          ;Send buffer backwards
                jsr .dr_sendbyte
                dey
.dr_mainsendstart:
                cpy #$00
                bne .dr_mainsendloop
                plp                     ;See if it was a partial sector
                bne .dr_mainfiledone    ;(last one)
                endif

.dr_mainnextsct:lda #$00
                sta .dr_mainsendstart+1 ;Startoffset for next sector is 0
                lda drvbuf+1            ;Follow the T/S link
                sta .buf1sct
                lda drvbuf
                sta .buf1trk            ;Go back to send the next sector,
                bne .dr_mainsctloop     ;unless IFFL file end encountered
.dr_mainfiledone:
                lda #$00                ;Errorcode 0: all OK
.dr_mainfilefail:
                pha                     ;File end: send $00 and errorcode
                lda #$00
                jsr .dr_sendbyte
                pla
                jsr .dr_sendbyte

                jmp .dr_mainloop        ;Then go back to main to wait for
                                        ;file number

.dr_scanexec:   lda #$e0                ;Use the seek/execute jobcode
                sta .buf2cmd            ;(.dr_scan at $0500 gets executed)
                cli
.dr_scanwait:   lda .buf2cmd
                bmi .dr_scanwait
                sei
                cmp #$02
                rts

;-------------------------------------------------------------------------------
; Subroutine: send byte in A to C64; no IRQs are allowed.
;-------------------------------------------------------------------------------

.dr_sendbyte:
                if TWOBIT_PROTOCOL>0
                sta .drvtemp
                lsr
                lsr
                lsr
                lsr
                tax
                lda #$04
.dr_sendwait:   bit .busport            ;Wait for CLK==low
                beq .dr_sendwait
                lsr                     ;Set DATA=low
                sta .busport
                lda .dr_sendtbl,x       ;Get the CLK,DATA pairs for low nybble
                pha
                lda .drvtemp
                and #$0f
                tax
                lda #$04
.dr_sendwait2:  bit .busport            ;Wait for CLK==high (start of high speed transfer)
                bne .dr_sendwait2
                lda .dr_sendtbl,x       ;Get the CLK,DATA pairs for high nybble
                sta .busport
                asl
                and #$0f
                sta .busport
                pla
                sta .busport
                asl
                and #$0f
                sta .busport
                nop
                nop
                lda #$00
                sta .busport            ;Finish send: DATA & CLK both high

                else

                sta .drvtemp            ;Store the byte to a temp variable
                tya                     ;Store Y-register contents
                pha
                ldy #$04
                lda .busport
                and #$f7
                sta .busport
                tya
.s1:            asl .drvtemp            ;Rotate bit to carry and "invert"
                ldx #$02
                bcc .s2
                ldx #$00
.s2:            bit .busport
                bne .s2
                stx .busport
                asl .drvtemp
                ldx #$02
                bcc .s3
                ldx #$00
.s3:            bit .busport
                beq .s3
                stx .busport
                dey
                bne .s1
                txa
                ora #$08
                sta .busport
                pla
                tay
                endif

                rts

;-------------------------------------------------------------------------------
; 2-bit send table
;-------------------------------------------------------------------------------

                if TWOBIT_PROTOCOL>0
.dr_sendtbl:    dc.b $0f,$07,$0d,$05
                dc.b $0b,$03,$09,$01
                dc.b $0e,$06,$0c,$04
                dc.b $0a,$02,$08,$00
                endif

;-------------------------------------------------------------------------------
; Subroutine: get byte from C64 in A
;-------------------------------------------------------------------------------

.dr_getbyte:    ldy #$08                ;Counter: receive 8 bits
.dr_recvbit:    lda #$85
                and .busport            ;Wait for CLK==low || DATA==low
                bmi .dr_gotatn          ;Quit if ATN was asserted
                beq .dr_recvbit
                lsr                     ;Read the data bit
                lda #2                  ;Prepare for CLK=high, DATA=low
                bcc .dr_rskip
                lda #8                  ;Prepare for CLK=low, DATA=high
.dr_rskip:      sta .busport            ;Acknowledge the bit received
                ror .drvtemp2           ;and store it
.dr_rwait:      lda .busport            ;Wait for CLK==high || DATA==high
                and #5
                eor #5
                beq .dr_rwait
                lda #0
                sta .busport            ;Set CLK=DATA=high
                dey
                bne .dr_recvbit         ;Loop until all bits have been received
                lda .drvtemp2           ;Return the data to A
                rts
.dr_gotatn:     pla                     ;If ATN gets asserted, exit to the operating
                pla                     ;system. Discard the return address and
                jmp .dr_quit            ;jump to the INITIALIZE routine

;-------------------------------------------------------------------------------
; Subroutine: read sector
;-------------------------------------------------------------------------------

.dr_readsector: sta .buf1trk
                stx .buf1sct
.dr_readsector2:
                if LED_FLASHING>0
                jsr .dr_led
                endif
                ldy #RETRIES            ;Retry counter
.dr_readsectorretry:
                jsr .dr_readexec
                bcc .dr_readsectorok
                jsr .dr_idchange
                dey                     ;Decrement retry counter and try again
                bne .dr_readsectorretry
.dr_readsectorok:
                if LED_FLASHING>0
.dr_led:        lda #$08                ;Flash the drive LED
.dr_ledac1:     eor $1c00
.dr_ledac2:     sta $1c00
                endif
                rts

.dr_readexec:   lda #$80                ;Job code: read sector
                sta .buf1cmd            ;Buffer 1 job
                cli
.dr_readsectorpoll:
                lda .buf1cmd
                bmi .dr_readsectorpoll
                sei
                cmp #$02                ;Errorcode
                rts

.dr_idchange:   ldx .id                 ;Handle possible disk ID change
                stx .iddrv0
                ldx .id+1
                stx .iddrv0+1
                rts

;-------------------------------------------------------------------------------
; IFFL filename
;-------------------------------------------------------------------------------

.dr_filename:   dc.b "IFFLDATA",0

drivecodeend_drv_1mhz:
                rend
drivecodeend_c64_drv_1mhz:

;-------------------------------------------------------------------------------
; Drive type-specific defines
;-------------------------------------------------------------------------------

                subroutine drv_2mhz

        ;1581/FD/HD defines

.drvtrklinktbl  = $0800                 ;Track link table for fast scanning
.drvsctlinktbl  = $0900                 ;Sector link table for fast scanning

.buf1cmd        = $03                   ;Buffer 1 command
.buf1trk        = $0d                   ;Buffer 1 track
.buf1sct        = $0e                   ;Buffer 1 sector
.buf2trk        = $0f                   ;Buffer 2 track
.buf2sct        = $10                   ;Buffer 2 sector
.drvtemp        = $70                   ;Temp variable
.drvtemp2       = $71                   ;Temp variable, IFFL file number
.drvcntl        = $72                   ;IFFL byte counter for scanning
.drvcnth        = $73

.busport        = $4001

;-------------------------------------------------------------------------------
; Drivecode for 2MHz drives
;-------------------------------------------------------------------------------

drivecode_c64_drv_2mhz:
                rorg drvstart
drivecode_drv_2mhz:

;-------------------------------------------------------------------------------
; Subroutine: scan the IFFL file
;-------------------------------------------------------------------------------

.dr_scan:       ldx .buf2sct            ;Make a copy of start sector
                stx .drvtemp
.dr_scansector: lda .buf2trk
                ldx .drvtemp
                jsr .dr_readsector      ;Read sector to $0400
                bcs .dr_scandone        ;Return error
                ldx .drvtemp
                lda drvbuf+1            ;Store T/S link to our linktable
                sta .drvsctlinktbl,x    ;so that we can "virtually" loop
                sta .drvtemp
                lda drvbuf              ;through the sectors later
                sta .drvtrklinktbl,x
                cmp .buf2trk
                beq .dr_scansector      ;Loop until EOF or track step required

.dr_scanfile:   ldy .drvtemp2           ;Current file number
                lda .drvcnth            ;See if the byte counter is less than
                bne .dr_scannext        ;254, otherwise go to next sector
                lda .drvcntl
                cmp #254
                bcs .dr_scannext
.dr_scanfileok: sta drvoffstbl,y        ;Store file offset
                adc drvlentbllo,y       ;Now add this file's length to the
                sta .drvcntl            ;byte counter
                lda .drvcnth
                adc drvlentblhi,y
                sta .drvcnth
                lda .buf2trk            ;Store file track/sector
                sta drvtrktbl,y         ;(file length gets overwritten but
                lda .buf2sct            ;it's no longer needed at this point)
                sta drvscttbl,y
                inc .drvtemp2           ;Increment file counter
                bpl .dr_scanfile        ;Fill up the table, then exit
                lda #$00
                beq .dr_scanok

.dr_scannext:   lda .drvcntl            ;Now subtract 254 bytes from the counter
                sec                     ;as we go to the next sector
                sbc #254
                sta .drvcntl
                bcs .dr_scannextok
                dec .drvcnth
.dr_scannextok: ldx .buf2sct
                lda .drvsctlinktbl,x    ;Get next sector from our linktable
                sta .buf2sct
                lda .drvtrklinktbl,x    ;Get next track from our linktable
                cmp .buf2trk
                beq .dr_scanfile        ;If same track, go back to loop for
.dr_scanok:     sta .buf2trk            ;files
                clc                     ;Otherwise, have to return and execute
.dr_scandone:   rts                     ;again

;-------------------------------------------------------------------------------
; Drive main code
;-------------------------------------------------------------------------------

dr_init_2mhz:
                if TWOBIT_PROTOCOL=0
                lda #$08                ;Set CLK=low to tell C64 there's no data to
                sta .busport            ;be read yet
                endif

drv_dirtrk:     lda #40                 ;Read first dir sector
drv_dirsct:     ldx #3
.dr_dirsctloop: jsr .dr_readsector
                bcs .dr_initfail
                ldy #$02
.dr_fileloop:   lda drvbuf,y            ;File type must be PRG
                and #$83
                cmp #$82
                bne .dr_nextfile
                sty .dr_namecmp+1
                ldx #$03
                lda #$a0                ;Make an endmark at the 16th letter
                sta drvbuf+19,y
.dr_namecmp:    lda drvbuf,x
                cmp .dr_filename-3,x    ;Check against each letter of filename
                bne .dr_namedone        ;until at the endzero
                inx
                bne .dr_namecmp
.dr_namedone:   cmp #$a0                ;If got to a $a0, name correct
                beq .dr_found
.dr_nextfile:   tya
                clc
                adc #$20                ;Go to next file
                tay
                bcc .dr_fileloop
.dr_nextdirsct: ldx drvbuf+1
                lda drvbuf              ;Any more dir sectors?
                bne .dr_dirsctloop      ;Errorcode $10 not used by 1541
                lda #$10                ;so use it as "IFFL file not found"
.dr_initfail:   jsr .dr_sendbyte        ;Send error code & exit
.dr_quit:       rts

.dr_found:      lda drvbuf+1,y          ;IFFL datafile found, get its start
                ldx drvbuf+2,y          ;track & sector
                jsr .dr_readsector
                bcs .dr_initfail
                ldy #MAXFILES-1
.dr_copylentbl: lda drvbuf+2,y          ;First sector contains the file lengths.
                sta drvlentbllo,y       ;Copy them to the length tables
                lda drvbuf+2+MAXFILES,y
                sta drvlentblhi,y
                dey
                bpl .dr_copylentbl
                lda #$00                ;Clear the length of the last file
                sta drvlentbllo+MAXFILES ;in case we have full 127 files
                sta drvlentblhi+MAXFILES
                sta .drvtemp2           ;Clear the scanning file counter
                sta .drvcntl            ;Clear the 16bit IFFL byte counter
                sta .drvcnth
                lda drvbuf              ;Now get the next T/S (where actual
                sta .buf2trk            ;data starts) and perform the scanning
                lda drvbuf+1
                sta .buf2sct
.dr_scanloop:   jsr .dr_scan
                bcs .dr_initfail        ;If error, abort
                lda .buf2trk            ;Keep calling the job until the file is
                bne .dr_scanloop        ;at an end
                jsr .dr_sendbyte        ;Now A=0, send the byte so that C64
                                        ;knows the file was scanned successfully

.dr_mainloop:
                if TWOBIT_PROTOCOL=0
                lda .busport            ;Set CLK=High
                and #$f7
                sta .busport
                lda #$04
.dr_mainfilewait:
                bit .busport            ;Wait for CLK=High
                bne .dr_mainfilewait
                ldy #$00                ;Set DATA=High
                sty .busport
                endif

                cli                     ;Allow interrupts so drive may stop
                jsr .dr_getbyte         ;Get file number from C64
                tay                     ;(file number also now in .drvtemp2)

                if TWOBIT_PROTOCOL=0
                lda #$08                ;Set CLK=low to tell C64 there's no data to
                sta .busport            ;be read yet
                endif

                lda drvoffstbl,y        ;Get file start offset
                sta .dr_mainsendstart+1
                lda drvtrktbl,y         ;Get file start track & sector
                sta .buf1trk
                lda drvscttbl,y
                sta .buf1sct

.dr_mainsctloop:ldy .drvtemp2           ;Get the file number back
                ldx #$fe                ;Assume we'll send a full sector (254b.)
                lda .buf1trk            ;If we're on the startsector of the
                cmp drvtrktbl+1,y       ;next file, we can only send up to the
                bne .dr_mainnotlast     ;next file's startoffset
                lda .buf1sct
                cmp drvscttbl+1,y
                bne .dr_mainnotlast
                ldx drvoffstbl+1,y      ;If endoffset = startoffset, we're
                cpx .dr_mainsendstart+1 ;already on the next file and can't
                beq .dr_mainfiledone    ;send anything
.dr_mainnotlast:stx .dr_mainsendend+1
                jsr .dr_readsector2     ;Read sector, abort if failed
                bcs .dr_mainfilefail

                if RECEIVE_BUFFER=0
                lda .dr_mainsendend+1
                cmp #$fe
                php
                sec
                sbc .dr_mainsendstart+1 ;Get amount of bytes to send
                jsr .dr_sendbyte
.dr_mainsendstart:
                ldy #$00
.dr_mainsendloop:
                iny
                lda drvbuf+1,y          ;Send buffer
                jsr .dr_sendbyte
.dr_mainsendend:
                cpy #$00
                bne .dr_mainsendloop
                plp                     ;See if it was a partial sector
                bne .dr_mainfiledone    ;(last one)

                else

.dr_mainsendend:ldy #$00
                cpy #$fe
                php
                tya
                sec
                sbc .dr_mainsendstart+1 ;Get amount of bytes to send
                jsr .dr_sendbyte
.dr_mainsendloop:
                lda drvbuf+1,y          ;Send buffer backwards
                jsr .dr_sendbyte
                dey
.dr_mainsendstart:
                cpy #$00
                bne .dr_mainsendloop
                plp                     ;See if it was a partial sector
                bne .dr_mainfiledone    ;(last one)
                endif

.dr_mainnextsct:lda #$00
                sta .dr_mainsendstart+1 ;Startoffset for next sector is 0
                lda drvbuf+1            ;Follow the T/S link
                sta .buf1sct
                lda drvbuf
                sta .buf1trk            ;Go back to send the next sector,
                bne .dr_mainsctloop     ;unless IFFL file end encountered
.dr_mainfiledone:
                lda #$00                ;Errorcode 0: all OK
.dr_mainfilefail:
                pha                     ;File end: send $00 and errorcode
                lda #$00
                jsr .dr_sendbyte
                pla
                jsr .dr_sendbyte

                jmp .dr_mainloop        ;Then go back to main to wait for
                                        ;file number

;-------------------------------------------------------------------------------
; Subroutine: send byte in A to C64; no IRQs are allowed.
;-------------------------------------------------------------------------------

.dr_sendbyte:
                if TWOBIT_PROTOCOL>0
                sta .drvtemp
                lsr
                lsr
                lsr
                lsr
                tax
                lda #$04
.dr_sendwait:   bit .busport            ;Wait for CLK==low
                beq .dr_sendwait
                lsr                     ;Set DATA=low
                sta .busport
                lda .dr_sendtbl,x       ;Get the CLK,DATA pairs for low nybble
                pha
                lda .drvtemp
                and #$0f
                tax
                lda #$04
.dr_sendwait2:  bit .busport            ;Wait for CLK==high (start of high speed transfer)
                bne .dr_sendwait2
                lda .dr_sendtbl,x       ;Get the CLK,DATA pairs for high nybble
                sta .busport
                jsr .dr_delay18
                nop
                asl
                and #$0f
                sta .busport
                cmp ($00,x)
                nop
                pla
                sta .busport
                cmp ($00,x)
                nop
                asl
                and #$0f
                sta .busport
                ldx #$00
                nop
                jsr .dr_delay12
                stx .busport            ;Finish send: DATA & CLK both high

                else

                sta .drvtemp            ;Store the byte to a temp variable
                tya                     ;Store Y-register contents
                pha
                ldy #$04
                lda .busport
                and #$f7
                sta .busport
                tya
.s1:            asl .drvtemp            ;Rotate bit to carry and "invert"
                ldx #$02
                bcc .s2
                ldx #$00
.s2:            bit .busport
                bne .s2
                stx .busport
                asl .drvtemp
                ldx #$02
                bcc .s3
                ldx #$00
.s3:            bit .busport
                beq .s3
                stx .busport
                dey
                bne .s1
                txa
                ora #$08
                sta .busport
                pla
                tay
                endif

                rts

;-------------------------------------------------------------------------------
; 2-bit send table
;-------------------------------------------------------------------------------

                if TWOBIT_PROTOCOL>0
.dr_delay18:    cmp ($00,x)
.dr_delay12:    rts

.dr_sendtbl:    dc.b $0f,$07,$0d,$05
                dc.b $0b,$03,$09,$01
                dc.b $0e,$06,$0c,$04
                dc.b $0a,$02,$08,$00
                endif

;-------------------------------------------------------------------------------
; Subroutine: get byte from C64 in A
;-------------------------------------------------------------------------------

.dr_getbyte:    ldy #$08                ;Counter: receive 8 bits
.dr_recvbit:    lda #$85
                and .busport            ;Wait for CLK==low || DATA==low
                bmi .dr_gotatn          ;Quit if ATN was asserted
                beq .dr_recvbit
                lsr                     ;Read the data bit
                lda #2                  ;Prepare for CLK=high, DATA=low
                bcc .dr_rskip
                lda #8                  ;Prepare for CLK=low, DATA=high
.dr_rskip:      sta .busport            ;Acknowledge the bit received
                ror .drvtemp2           ;and store it
.dr_rwait:      lda .busport            ;Wait for CLK==high || DATA==high
                and #5
                eor #5
                beq .dr_rwait
                lda #0
                sta .busport            ;Set CLK=DATA=high
                dey
                bne .dr_recvbit         ;Loop until all bits have been received
                lda .drvtemp2           ;Return the data to A
                rts
.dr_gotatn:     pla                     ;If ATN gets asserted, exit to the operating
                pla                     ;system. Discard the return address and
                jmp .dr_quit            ;return

;-------------------------------------------------------------------------------
; Subroutine: read sector
;-------------------------------------------------------------------------------

.dr_readsector: sta .buf1trk
                stx .buf1sct
.dr_readsector2:
                if LED_FLASHING>0
                jsr .dr_led
                endif
                ldy #RETRIES            ;Retry counter
.dr_readsectorretry:
                jsr .dr_readexec
                bcc .dr_readsectorok
                ;jsr .dr_idchange
                dey                     ;Decrement retry counter and try again
                bne .dr_readsectorretry
.dr_readsectorok:
                if LED_FLASHING>0
.dr_led:        lda #$40                ;Flash the drive LED
.dr_ledac1:     eor $4000
.dr_ledac2:     sta $4000
                endif
                rts

.dr_readexec:   lda #$80                ;Job code: read sector
                ldx #1                  ;Buffer 1 job
                clc
                ;cli                     ;Already executed (at $959e on a 1581)
                jsr $ff54               ;Returns with A = job status on a 1581 ($95a3)
                lda .buf1cmd            ;Only strictly necessary for FD2000
                sei
                cmp #$02                ;Errorcode
                rts

.dr_idchange:   ;rts

;-------------------------------------------------------------------------------
; IFFL filename
;-------------------------------------------------------------------------------

.dr_filename:   dc.b "IFFLDATA",0

drivecodeend_drv_2mhz:
                rend
drivecodeend_c64_drv_2mhz: