;-------------------------------------------------------------------------------
; Common routines for IFFL test programs by Luigi Di Fraia 9/2017
;-------------------------------------------------------------------------------

                processor 6502

initirq:
                sei
                lda #<raster
                sta $0314
                lda #>raster
                sta $0315
                lda #50                 ;Set low bits of raster
                sta $d012               ;position
                lda $d011
                and #$7f                ;Set high bit of raster
                sta $d011               ;position (0)
                lda #$7f                ;Set timer interrupt off
                sta $dc0d
                lda #$01                ;Set raster interrupt on
                sta $d01a
                lda $dc0d               ;Acknowledge timer interrupt
                lda #$00
                jsr $1000               ;Init tune
                cli
                rts

raster:         jsr $1003               ;Play tune
                jsr spriteirq
                dec $d019
                jmp $ea31

screen = $6000 ; Video matrix in use
bitmap = $4000 ; Bitmap address

showpicture:    lda #((screen^$FFFF)>>14)
                sta $dd00               ;Set videobank
                lda #$3b
                sta $d011               ;Set Y-scrolling / bitmap mode
                lda #$18
                sta $d016               ;Set multicolor mode
                lda #(((screen>>6) & $f0) | ((bitmap>>10) & $0e))
                sta $d018               ;Set screen pointer
                lda #$00
                sta $d020               ;Set correct background colors
                lda #$00
                sta $d021
                ldx #$00
                rts

; Sprite IRQ segment

sprdat = $6800 ; Ghost sprite images

spriteirq:
                ; Flash sprite
_sflidx = *+1
_sflash         ldx #$00
                cpx #sflshsz
                bcc _sfl

                ldx #$00
                stx _sflidx

_sfl            lda sflshtb,x
                sta $d027                ; Flash sprite

                inc _sflidx

                ; Position sprite
_pxoff = *+1
                lda sintab
                inc _pxoff
                clc
                adc #$6c
                sta $d000

_pyoff = *+1
                lda sintab+$80
                lsr
                inc _pyoff
                lsr
                clc
                adc #$6a
                sta $d001

                ; Animate sprite
_space = *+1
                lda #$03                ; Every 4 ticks
                beq _sanim

                dec _space
                jmp _done

_sioff = *+1
_sanim          lda #$00
                inc _sioff
                and #$07
                clc
                adc #<(sprdat/$40)
                sta screen+$03f8

                lda #$03                ; Reset for 4 ticks
                sta _space

_done           rts

; Init VIC II registers and sprite image pointers

vsetup:         lda #$00
                sta $d000
                sta $d001
                sta $d027
                lda #$01
                sta $d015

                lda #<(sprdat/$40)
                sta screen+$03f8
                rts

; Color flashing sequence for sprite

sflshtb:        .byte $01,$01,$01,$0f,$0f,$0f,$0c,$0c,$0c,$0b,$0b,$0b,$0c,$0c,$0c,$0f
                .byte $0f,$0f

sflshsz = *-sflshtb

; Page-aligned sin table

                org $0a00

sintab:         .byte $7F,$7F,$7F,$7F,$7E,$7D,$7D,$7C,$7A,$79,$78,$76,$75,$73,$71,$6F
                .byte $6D,$6A,$68,$65,$63,$60,$5E,$5B,$58,$55,$52,$4F,$4C,$49,$46,$43
                .byte $3F,$3C,$39,$36,$33,$30,$2D,$2A,$27,$24,$21,$1F,$1C,$1A,$17,$15
                .byte $12,$10,$0E,$0C,$0A,$09,$07,$06,$05,$03,$02,$02,$01,$00,$00,$00
                .byte $00,$00,$00,$00,$01,$02,$02,$03,$05,$06,$07,$09,$0A,$0C,$0E,$10
                .byte $12,$15,$17,$1A,$1C,$1F,$21,$24,$27,$2A,$2D,$30,$33,$36,$39,$3C
                .byte $40,$43,$46,$49,$4C,$4F,$52,$55,$58,$5B,$5E,$60,$63,$65,$68,$6A
                .byte $6D,$6F,$71,$73,$75,$76,$78,$79,$7A,$7C,$7D,$7D,$7E,$7F,$7F,$7F
                .byte $7F,$7F,$7F,$7F,$7E,$7D,$7D,$7C,$7A,$79,$78,$76,$75,$73,$71,$6F
                .byte $6D,$6A,$68,$65,$63,$60,$5E,$5B,$58,$55,$52,$4F,$4C,$49,$46,$43
                .byte $3F,$3C,$39,$36,$33,$30,$2D,$2A,$27,$24,$21,$1F,$1C,$1A,$17,$15
                .byte $12,$10,$0E,$0C,$0A,$09,$07,$06,$05,$03,$02,$02,$01,$00,$00,$00
                .byte $00,$00,$00,$00,$01,$02,$02,$03,$05,$06,$07,$09,$0A,$0C,$0E,$10
                .byte $12,$15,$17,$1A,$1C,$1F,$21,$24,$27,$2A,$2D,$30,$33,$36,$39,$3C
                .byte $40,$43,$46,$49,$4C,$4F,$52,$55,$58,$5B,$5E,$60,$63,$65,$68,$6A
                .byte $6D,$6F,$71,$73,$75,$76,$78,$79,$7A,$7C,$7D,$7D,$7E,$7F,$7F,$7F
