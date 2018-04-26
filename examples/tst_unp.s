;-------------------------------------------------------------------------------
; IFFL test with unpacked file by Luigi Di Fraia 9/2017
;-------------------------------------------------------------------------------

                processor 6502

                include basic_boot.s

start:          jsr initloader
                bcs error
                lda #$00
                jsr loadfile            ;Load file $00 - music
                bcs error
                jsr initmusicplayback
                lda #$10                ;Try to load some nonexistent file -
                jsr loadfile            ;should be harmless as long file number
                lda #$01                ;< MAXFILES
                jsr loadfile            ;Load file $01 - picture
                bcs error
                jsr showpicture
done:           jmp done                ;Loop endlessly, showing the pic and
                                        ;playing the tune

error:          sta $d020               ;If any error, store errorcode to border and
                jmp done                ;loop endlessly

                include common.s
                include cfg_unp.s
                include ..\iffl.s
