;-------------------------------------------------------------------------------
; IFFL test with BYTEBOOZER depacking by Luigi Di Fraia 3/2022
;-------------------------------------------------------------------------------

                processor 6502

                include "basic_boot.s"

prepare:        sei

                cld
                ldx #$ff
                txs
                stx $91                 ;Signal no key pressed
                inx
                stx $02a1               ;Signal RS-232 interrupts are disabled

                lda #$00                ;Disable VIC-II interrupts
                sta $d01a
                asl $d019               ;Acknowledge pending requests
                lda #$7f
                sta $dc0d               ;Disable CIA #1 interrupts
                sta $dd0d               ;Disable CIA #2 interrupts
                lda $dc0d               ;Acknowledge pending requests
                lda $dd0d

start:          jsr initloader1
                ldx #$00                ;Relocate the IFFL loader to page 2 and 3,
moveloader:     lda fastloader,x        ;thus overwriting $02a1, $0318/$0319;
                sta $0200,x             ;getbyte will be available once finished
                lda fastloader+$0100,x
                sta $0300,x
                inx
                bne moveloader
                jsr initloader2         ;Patch getbyte_delay and call getbyte
                bcs error

                sei
                lda #$35
                sta $01                 ;Disable BASIC and Kernal ROMs

                lda #$00
                jsr loadfile_bboozer2   ;Load file $00 - music
                bcs error
                jsr initmusicplayback
                lda #$10                ;Try to load some nonexistent file -
                jsr loadfile_bboozer2   ;should be harmless as long file number
                lda #$01                ;< MAXFILES
                jsr loadfile_bboozer2   ;Load file $01 - picture
                bcs error
                jsr showpicture
done:           jmp done                ;Loop endlessly, showing the pic and
                                        ;playing the tune

error:          sta $d020               ;If any error, store errorcode to border and
                jmp done                ;loop endlessly

                include "common.s"

                include "cfg_bb2.s"
fastloader:
                rorg $0200
                include "../iffl_loader.s"
                rend
                include "../iffl_init.s"
