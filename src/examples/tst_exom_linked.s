;-------------------------------------------------------------------------------
; IFFL test with EXOMIZER depacking by Luigi Di Fraia 9/2017
;-------------------------------------------------------------------------------

                processor 6502

                include basic_boot.s

start:          jsr initloader
                bcs error
                lda #$00
                jsr loadfile_exomizer   ;Load file $00 - music + picture linked
                bcs error
                jsr initmusicplayback
                jsr showpicture
done:           jmp done                ;Loop endlessly, showing the pic and
                                        ;playing the tune

error:          sta $d020               ;If any error, store errorcode to border and
                jmp done                ;loop endlessly

                include common.s
                include cfg_exom.s
                include ..\iffl_loader.s
                include ..\iffl_init.s
