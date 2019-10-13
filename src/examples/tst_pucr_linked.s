;-------------------------------------------------------------------------------
; IFFL test with PUCRUNCH depacking by Luigi Di Fraia 9/2017
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
start:          jsr initloader
                bcs error
                lda #$00
                jsr loadfile_pucrunch   ;Load file $00 - music + picture linked
                bcs error
                jsr initmusicplayback
                jsr showpicture
done:           jmp done                ;Loop endlessly, showing the pic and
                                        ;playing the tune

error:          sta $d020               ;If any error, store errorcode to border and
                jmp done                ;loop endlessly

                include "common.s"

                include "cfg_pucr.s"
                include "../iffl_loader.s"
                include "../iffl_init.s"
