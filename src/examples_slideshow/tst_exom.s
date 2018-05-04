;-------------------------------------------------------------------------------
; IFFL test with EXOMIZER depacking by Luigi Di Fraia 9/2017
;-------------------------------------------------------------------------------

num_pics = 2

                processor 6502

                include basic_boot.s

start:          jsr initloader
                bcs error
                lda #$00
                jsr loadfile_exomizer   ;Load file $00 - music
                bcs error
                jsr initmusicplayback
                ldx #$01
		stx currentpic
nextpicture:    jsr waitvb
                lda $d011
                and #$ef                ;Blank screen
                sta $d011
		txa
                jsr loadfile_exomizer   ;Load next file - picture
                bcs error
                jsr showpicture
		jsr wait4space
		inc currentpic
		ldx currentpic
		cpx #num_pics+1
		bne nextpicture
done:           jmp done                ;Loop endlessly, showing the pic and
                                        ;playing the tune

error:          sta $d020               ;If any error, store errorcode to border and
                jmp done                ;loop endlessly

currentpic:	dc.b 0

                include common.s

                include cfg_exom.s
                include ..\iffl_loader.s
                include ..\iffl_init.s