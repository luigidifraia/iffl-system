;-------------------------------------------------------------------------------
; IFFL test with BYTEBOOZER depacking by Luigi Di Fraia 9/2017
;-------------------------------------------------------------------------------

                processor 6502

                include basic_boot.s

start:          jsr initloader
                bcs error
                lda #$00
                jsr loadfile_bboozer2   ;Load file $00 - sprites and music
                bcs error
                jsr showpicture         ;Switch bank
                jsr vsetup
                jsr initirq             ;Attach tune playback and sprite animation
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

                include common.s

                include cfg_bb2.s
                include ..\iffl_loader.s
                include ..\iffl_init.s
