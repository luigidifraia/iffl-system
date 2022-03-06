;-------------------------------------------------------------------------------
; Common routines for IFFL test programs by Luigi Di Fraia 9/2017
;-------------------------------------------------------------------------------

                processor 6502

initmusicplayback:
                sei
                lda #<raster
                sta $fffe
                lda #>raster
                sta $ffff
                lda #50                 ;Set low bits of raster
                sta $d012               ;position
                lda $d011
                and #$7f                ;Set high bit of raster
                sta $d011               ;position (0)
                lda #$01                ;Set raster interrupt on
                sta $d01a
                lda #$00
                jsr $1000               ;Init tune
                cli
                rts

raster:         pha
                tya
                pha
                txa
                pha
                jsr $1003               ;Play tune
                dec $d019
                pla
                tax
                pla
                tay
                pla
                rti

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
copycolors:     lda $6400,x             ;Copy the colorscreen
                sta $d800,x
                lda $6500,x
                sta $d900,x
                lda $6600,x
                sta $da00,x
                lda $66e8,x             ;Make sure we won't write past $dbe8
                sta $dae8,x
                inx
                bne copycolors
                rts
