;-------------------------------------------------------------------------------
; IFFL test with EXOMIZER depacking by Luigi Di Fraia 9/2017
;-------------------------------------------------------------------------------

                processor 6502

                include basic_boot.s

start:          jsr initloader
                bcs error
                lda #$00
                jsr loadfile_exomizer   ;Load file $00 - music
                bcs error
                jsr initmusicplayback
                lda #$10                ;Try to load some nonexistent file -
                jsr loadfile_exomizer   ;should be harmless as long file number
                lda #$01                ;< MAXFILES
                jsr loadfile_exomizer   ;Load file $01 - picture
                bcs error
                jsr showpicture
done:           jmp done                ;Loop endlessly, showing the pic and
                                        ;playing the tune

error:          sta $d020               ;If any error, store errorcode to border and
                jmp done                ;loop endlessly

                include common.s
                include cfg_exom.s
                include ..\iffl_loader.s
                include ..\iffl_init.s