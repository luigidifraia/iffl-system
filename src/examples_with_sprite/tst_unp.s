;-------------------------------------------------------------------------------
; IFFL test with BYTEBOOZER depacking by Luigi Di Fraia 9/2017
;-------------------------------------------------------------------------------

                processor 6502

                include basic_boot.s

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
                jsr loadfile            ;Load file $00 - sprites
                bcs error
                lda #$01
                jsr loadfile            ;Load file $01 - music
                bcs error
                jsr setvideomode        ;Change video mode and bank so we can display
                jsr spritesetup         ;the sprite and appreciate data while it loads
                jsr initirq             ;Attach tune playback and sprite animation
                lda #$10                ;Try to load some nonexistent file -
                jsr loadfile            ;should be harmless as long file number
                lda #$02                ;< MAXFILES
                jsr loadfile            ;Load file $02 - bitmap
                bcs error
                lda #$03
                jsr loadfile            ;Load file $03 - video RAM
                bcs error
                lda #$04
                jsr loadfile            ;Load file $04 - color RAM
                bcs error
done:           jmp done                ;Loop endlessly, showing the pic and
                                        ;playing the tune

error:          sta $d020               ;If any error, store errorcode to border and
                jmp done                ;loop endlessly

                include common.s

                include cfg_unp.s
                include ..\iffl_loader.s
                include ..\iffl_init.s
