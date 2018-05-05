;-------------------------------------------------------------------------------
; IFFL system with 2-bit transfer by Cadaver 9/2004
; Unitialized Y register fixed in 3/2015
;
; 1-bit transfer support added by Luigi Di Fraia 9/2017
; Buffer-less support added by Luigi Di Fraia 9/2017
; ByteBoozer 2.0, Pucrunch and Exomizer support added by Luigi Di Fraia 9/2017
;
; Call INITLOADER first to scan the IFFL file. Returncode:C=0 OK, C=1 error
; The IFFL filename is compiled in at the end of the drivecode, so change it
; as necessary.
;
; Call LOADFILE with filenumber in A to load. Returncode:C=0 OK, C=1 error
; Valid filenumbers are $00-$7e.
;
; Use any Kernal file I/O to detach the IFFL drivecode.
;
; If 2-bit transfer is used, sprites must be off and IRQs can get delayed by a
; couple of rasterlines.
;-------------------------------------------------------------------------------

                processor 6502

;-------------------------------------------------------------------------------
; Make sure you included your loader configuration file by this point!
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Defines derived from the compile options (need not be changed)
;-------------------------------------------------------------------------------

                if ADDITIONAL_ZEROPAGE>0
loadtempreg     = zpbase2+0             ;Temp variables for the loader
bufferstatus    = zpbase2+1             ;Bytes in fastload buffer
                endif

                if LOADFILE_UNPACKED>0
;-------------------------------------------------------------------------------
; LOADFILE
;
; Loads an unpacked file from the IFFL-file. INITLOADER must have been called
; first.
;
; Parameters: A:File number ($00-$7e)
; Returns: C=0 File loaded OK
;          C=1 Error
; Modifies: A,X,Y
;-------------------------------------------------------------------------------

loadfile:
                if SCPU_SUPPORT>0
                sta $d07a               ;SCPU to 1MHz mode
                endif
                jsr sendbyte            ;Send file number
                if TWOBIT_PROTOCOL=0
load_wait:      bit $dd00               ;Wait for the drive to set CLK low
                bvs load_wait           ;in preparation to sending bytes
                endif
                lda #$00
                sta bufferstatus        ;Reset amount of bytes in buffer
                jsr load_getbyte        ;Get startaddress low
                bcs loadfile_fail       ;File not found
                sta load_sta+1
                jsr load_getbyte        ;Startaddress high
                bcs loadfile_fail
                sta load_sta+2
load_loop:      jsr load_getbyte        ;Get data byte from file
                bcs loadfile_eof
                if LOAD_UNDER_IO>0
                jsr disableio           ;Allow loading under I/O area
                endif
load_sta:       sta $1000               ;and store it
                if LOAD_UNDER_IO>0
                jsr enableio
                endif
                inc load_sta+1
                bne load_loop
                inc load_sta+2
                jmp load_loop
loadfile_eof:   cmp #$01                ;Returncode 0 = OK, others error
loadfile_fail:
                if SCPU_SUPPORT>0
                sta $d07b               ;SCPU to fast mode
                endif
                rts
                endif

                if LOADFILE_BBOOZER2>0
;-------------------------------------------------------------------------------
; LOADFILE_BBOOZER2
;
; Loads a file packed with ByteBoozer 2.0 from the IFFL-file. INITLOADER must
; have been called first.
;
; Parameters: A:File number ($00-$7e)
; Returns: C=0 File loaded OK
;          C=1 Error
; Modifies: A,X,Y
;-------------------------------------------------------------------------------

loadfile_bboozer2:
                if SCPU_SUPPORT>0
                sta $d07a               ;SCPU to 1MHz mode
                endif
                tsx
                stx BB2StackPtr+1
                jsr sendbyte            ;Send file number
                if TWOBIT_PROTOCOL=0
loadbb2_wait:   bit $dd00               ;Wait for the drive to set CLK low
                bvs loadbb2_wait        ;in preparation to sending bytes
                endif
                lda #$00
                sta bufferstatus        ;Reset amount of bytes in buffer

; ByteBoozer Decruncher    /HCL may.2003
; Integrated with loader        aug.2003
; Tuned for Cycle/BoozeDesign   nov.2003
; Optimized decruncher and flip
; disk for Edge of Disgrace     aug.2008
; ByteBoozer2 integration
; Transfer 72 cycles per byte
; Fits in <$200 bytes           jan.2015

;Variables..        #Bytes
zp_base	= zpbase
put	= zp_base   ;2
bits	= zp_base+2 ;1

	if START_ADDRESS_TRIMMED_BB2=0
	jsr GetNewBits
BB2Chained
	jsr GetNewBits
	endif

	jsr GetNewBits
	if START_ADDRESS_TRIMMED_BB2>0
BB2Chained
	endif
	sty put
	jsr GetNewBits
	sty put+1

	lda #$80
	sta bits
DLoop
	jsr GetNextBit
	bcs Match
Literal
	; Literal run.. get length.
	jsr GetLen
	sta LLen+1

	ldy #0
LLoop
	jsr load_getbyte
	bcs BB2Error
	; Allow loading under I/O area
	if LOAD_UNDER_IO>0
	jsr disableio
	endif
	sta (put),y
	if LOAD_UNDER_IO>0
	jsr enableio
	endif
	iny
LLen	cpy #0
	bne LLoop

	clc
	tya
	adc put
	sta put
	bcc LInc
	inc put+1

LInc	iny
	beq DLoop

	; Has to continue with a match..

Match
	; Match.. get length.
	jsr GetLen
	sta MLen+1

	; Length 255 -> End of the current segment in chain
	cmp #$ff
	bne BB2NotEnd

BB2End
	; Check whether EOF or chained files
	jsr load_getbyte
	if START_ADDRESS_TRIMMED_BB2>0
	tay
	endif
	bcc BB2Chained
BB2CheckEof
	cmp #$01
BB2Error
BB2StackPtr
	ldx #$00
	txs
	if SCPU_SUPPORT>0
	; SCPU to fast mode
	sta $d07b
	endif
	rts

BB2NotEnd
	; Get num bits
	cmp #2
	lda #0
	rol
	jsr GetNextBit
	rol
	jsr GetNextBit
	rol
	tay
	lda Tab,y
	beq MByte

	; Get bits < 8
MLoop1	jsr GetNextBit
	rol
	bcs MLoop1
	bmi MShort
MByte
	; Get byte
	eor #$ff
	tay
	jsr load_getbyte
	bcs BB2Error
	jmp MLong
MShort
	ldy #$ff
MLong
	;clc
	adc put
	sta MLda+1
	tya
	adc put+1
	sta MLda+2
	if LOAD_UNDER_IO>0
	jsr disableio
	endif
	ldy #$ff
MLoop2	iny
MLda	lda $b00b,y
	sta (put),y
MLen	cpy #0
	bne MLoop2
	if LOAD_UNDER_IO>0
	jsr enableio
	endif

	;sec
	tya
	adc put
	sta put
	bcc *+4
	inc put+1

	jmp DLoop

GetNextBit
	asl bits
	bne GnbEnd
	; Fall through GetNewBits

GetNewBits
	php
	pha
	jsr load_getbyte
	bcs BB2Error
	tay
	pla
	plp
	sty bits
	rol bits
GnbEnd
	rts

GetLen
	lda #1
GlLoop	jsr GetNextBit
	bcc GlEnd
	jsr GetNextBit
	rol
	bpl GlLoop
GlEnd	rts

Tab
	; Short offsets
	.byte %11011111 ; 3
	.byte %11111011 ; 6
	.byte %00000000 ; 8
	.byte %10000000 ; 10
	; Long offsets
	.byte %11101111 ; 4
	.byte %11111101 ; 7
	.byte %10000000 ; 10
	.byte %11110000 ; 13
; -------------------------------------------------------------------
; end of decruncher
; -------------------------------------------------------------------
                endif

                if LOADFILE_PUCRUNCH>0
;-------------------------------------------------------------------------------
; LOADFILE_PUCRUNCH
;
; Loads a file packed with PUCRUNCH from the IFFL-file. INITLOADER must have 
; been called first.
;
; Parameters: A:File number ($00-$7e)
; Returns: C=0 File loaded OK
;          C=1 Error
; Modifies: A,X,Y
;-------------------------------------------------------------------------------

lzpos           = zpbase
bitstr          = zpbase+2

table           = depackbuffer

loadfile_pucrunch:
                if SCPU_SUPPORT>0
                sta $d07a               ;SCPU to 1MHz mode
                endif
                tsx
                stx pucrunch_stackptr+1
                jsr sendbyte            ;Send file number
                if TWOBIT_PROTOCOL=0
loadpucr_wait:  bit $dd00               ;Wait for the drive to set CLK low
                bvs loadpucr_wait       ;in preparation to sending bytes
                endif
                lda #$00
                sta bufferstatus        ;Reset amount of bytes in buffer

;-------------------------------------------------------------------------------
; PUCRUNCH DECOMPRESSOR by Pasi Ojala
;
; SHORT+IRQLOAD         354 bytes
; no rle =~             -83 bytes -> 271
; fixed params =~       -48 bytes -> 306
;                       223 bytes
; Parameters: -
; Returns: -
; Modifies: A,X,Y
;-------------------------------------------------------------------------------

        if HEADER_TRIMMED_PUCRUNCH=0
        jsr load_getbyte ; skip file startaddress
        bcc pucrunch_hi
        jmp pucrunch_fail
pucrunch_chained
pucrunch_hi
        jsr load_getbyte
        bcc pucrunch_start
        jmp pucrunch_fail
pucrunch_start
        ldx #5
222$    jsr getbyt      ; skip 'p', 'u', endAddr HI&LO; read starting escape in A
        dex
        bne 222$
        else
        jsr getbyt      ; read starting escape in A
pucrunch_chained
        endif
        sta esc+1       ; starting escape
        jsr getbyt      ; read startAddr
        sta outpos+1
        jsr getbyt
        sta outpos+2
        jsr getbyt      ; read # of escape bits
        sta escB0+1
        sta escB1+1
        lda #8
        sec
        sbc escB1+1
        sta noesc+1     ; 8-escBits

        jsr getbyt
        sta mg+1        ; maxGamma + 1
        lda #9
        sec
        sbc mg+1        ; 8 - maxGamma == (8 + 1) - (maxGamma + 1)
        sta longrle+1
        jsr getbyt
        sta mg1+1       ; (1<<maxGamma)
        asl
        clc
        sbc #0
        sta mg21+1      ; (2<<maxGamma) - 1
        jsr getbyt
        sta elzpb+1

        ldx #$03
2$      jsr getbyt      ; Get 3 bytes, 2 unused (exec address)
        dex             ; and rleUsed. X is 0 after this loop
        bne 2$

        ;jsr getbyt     ; exec address
        ;sta lo+1       ; lo
        ;jsr getbyt
        ;sta hi+1       ; hi
        ;
        ;jsr getbyt     ; rleUsed
        ;ldx #0

        tay
        sty bitstr
0$      beq 1$          ; Y == 0 ?
        jsr getbyt
        sta table,x
        inx
        dey
        bne 0$
1$      ; setup bit store - $80 means empty
        lda #$80
        sta bitstr
        bne main        ; jump always

newesc  ldy esc+1       ; remember the old code (top bits for escaped byte)
escB0   ldx #2          ; ** PARAMETER  0..8
        jsr getchkf     ; get & save the new escape code
        sta esc+1
        tya             ; pre-set the bits
        ; Fall through and get the rest of the bits.
noesc   ldx #6          ; ** PARAMETER  8..0
        jsr getchkf
        jsr putch       ; output the escaped/normal byte
        ; Fall through and check the escape bits again
main    ldy #0          ; Reset to a defined state
        tya             ; A = 0
escB1   ldx #2          ; ** PARAMETER  0..8
        jsr getchkf     ; X = 0
esc     cmp #0
        bne noesc
        ; Fall through to packed code

        jsr getval      ; X = 0
        sta lzpos       ; xstore - save the length for a later time
        lsr             ; cmp #1        ; LEN == 2 ? (A is never 0)
        bne lz77        ; LEN != 2      -> LZ77
        ;tya            ; A = 0
        jsr get1bit     ; X = 0
        lsr             ; bit -> C, A = 0
        bcc lz77_2      ; A=0 -> LZPOS+1
        ;***FALL THRU***

        ; e..e01
        jsr get1bit     ; X = 0
        lsr             ; bit -> C, A = 0
        bcc newesc      ; e..e010
        ;***FALL THRU***

        ; e..e011
srle    iny             ; Y is 1 bigger than MSB loops
        jsr getval      ; Y is 1, get len, X = 0
        sta lzpos       ; xstore - Save length LSB
mg1     cmp #64         ; ** PARAMETER 63-64 -> C clear, 64-64 -> C set..
        bcc chrcode     ; short RLE, get bytecode

longrle ldx #2          ; ** PARAMETER  111111xxxxxx
        jsr getbits     ; get 3/2/1 more bits to get a full byte, X = 0
        sta lzpos       ; xstore - Save length LSB

        jsr getval      ; length MSB, X = 0
        tay             ; Y is 1 bigger than MSB loops

chrcode jsr getval      ; Byte Code, X = 0
        tax             ; this is executed most of the time anyway
        lda table-1,x   ; Saves one jump if done here (loses one txa)

        cpx #32         ; 31-32 -> C clear, 32-32 -> C set..
        bcc 1$          ; 1..31, we got the right byte from the table

        ; Ranks 32..64 (11111°xxxxx), get byte..
        txa             ; get back the value (5 valid bits)
        ldx #3
        jsr getbits     ; get 3 more bits to get a full byte, X = 0

1$      ldx lzpos       ; xstore - get length LSB
        inx             ; adjust for cpx#$ff;bne -> bne
dorle   jsr putch
        dex
        bne dorle       ; xstore 0..255 -> 1..256
        dey
        bne dorle       ; Y was 1 bigger than wanted originally
mainbeq beq main        ; reverse condition -> jump always


lz77    jsr getval      ; X = 0
mg21    cmp #127        ; ** PARAMETER  Clears carry (is maximum value)
        bne noeof

pucrunch_end:
        jsr load_getbyte ; check whether EOF or chained files
        bcs pucrunch_eof
        if HEADER_TRIMMED_PUCRUNCH>0
        ldx #5
        endif
        jmp pucrunch_chained
pucrunch_eof:
        cmp #$01
pucrunch_fail:          ; a premature EOF is treated as an
pucrunch_stackptr:      ; error; return directly to caller
        ldx #$00
        txs
        if SCPU_SUPPORT>0
        sta $d07b       ; SCPU to fast mode
        endif
        rts

noeof   sbc #0          ; C is clear -> subtract 1  (1..126 -> 0..125)
elzpb   ldx #0          ; ** PARAMETER (more bits to get)
        jsr getchkf     ; clears Carry, X = 0

lz77_2  sta lzpos+1     ; offset MSB
        jsr getbyt2     ; clears Carry, X = 0
        ; Note: Already eor:ed in the compressor..
        ;eor #255       ; offset LSB 2's complement -1 (i.e. -X = ~X+1)
        adc outpos+1    ; -offset -1 + curpos (C is clear)
        ldx lzpos       ; xstore = LZLEN (read before it's overwritten)
        sta lzpos

        lda outpos+2
        sbc lzpos+1     ; takes C into account
        sta lzpos+1     ; copy X+1 number of chars from LZPOS to outpos+1
        ;ldy #0         ; Y was 0 originally, we don't change it

        inx             ; adjust for cpx#$ff;bne -> bne

lzslow  if LOAD_UNDER_IO>0
        jsr disableio
        endif
        lda (lzpos),y   ; using abs,y is 3 bytes longer, only 1 cycle/byte faster
        jsr outpos
        iny             ; Y does not wrap because X=0..255 and Y initially 0
        dex
        bne lzslow      ; X loops, (256,1..255)
        jmp main

putch   if LOAD_UNDER_IO>0
        jsr disableio
        endif
outpos  sta $aaaa       ; ** parameter
        inc outpos+1    ; ZP
        bne putchok
        inc outpos+2    ; ZP
putchok if LOAD_UNDER_IO>0
        jmp enableio
        else
        rts
        endif

getnew  pha             ; 1 Byte/3 cycles
        jsr load_getbyte
        bcs pucrunch_fail
0$      sec
        rol             ; Shift out the next bit and
                        ;  shift in C=1 (last bit marker)
        sta bitstr      ; bitstr initial value = $80 == empty
        pla             ; 1 Byte/4 cycles
        rts
        ; 25+12 = 37

getbyt  jsr getnew
        lda bitstr
        ror
        rts

; getval : Gets a 'static huffman coded' value
n; ** Scratches X, returns the value in A **
getval  inx             ; X <- 1
        txa             ; set the top bit (value is 1..255)
gv0     asl bitstr
        bne 1$
        jsr getnew
1$      bcc getchk      ; got 0-bit
        inx
mg      cpx #7          ; ** PARAMETER unary code maximum length + 1
        bne gv0
        beq getchk      ; inverse condition -> jump always
        ; getval: 18 bytes
        ; 15 + 17*n + 6+15*n+12 + 36*n/8 = 33 + 32*n + 36*n/8 cycles

; getbits: Gets X bits from the stream
; ** Scratches X, returns the value in A **
getbyt2 ldx #7
get1bit inx             ;2
getbits asl bitstr
        bne 1$
        jsr getnew
1$      rol             ;2
getchk  dex             ;2              more bits to get ?
getchkf bne getbits     ;2/3
        clc             ;2              return carry cleared
        rts             ;6+6
; -------------------------------------------------------------------
; end of decruncher
; -------------------------------------------------------------------
                endif

                if LOADFILE_EXOMIZER>0
;-------------------------------------------------------------------------------
; LOADFILE_EXOMIZER
;
; Loads a file packed with EXOMIZERx from the IFFL-file. INITLOADER must have
; been called first.
;
; Parameters: A:File number ($00-$7e)
; Returns: C=0 File loaded OK
;          C=1 Error
; Modifies: A,X,Y
;-------------------------------------------------------------------------------

tabl_bi         = depackbuffer
tabl_lo         = depackbuffer+52
tabl_hi         = depackbuffer+104

zp_len_lo       = zpbase+0
zp_src_lo       = zpbase+1
zp_src_hi       = zpbase+2
zp_bits_lo      = zpbase+3
zp_bits_hi      = zpbase+4
zp_bitbuf       = zpbase+5
zp_dest_lo      = zpbase+6
zp_dest_hi      = zpbase+7

loadfile_exomizer:
                if SCPU_SUPPORT>0
                sta $d07a               ;SCPU to 1MHz mode
                endif
                tsx
                stx exomizer_stackptr+1
                jsr sendbyte            ;Send file number
                if TWOBIT_PROTOCOL=0
loadexo_wait:   bit $dd00               ;Wait for the drive to set CLK low
                bvs loadexo_wait        ;in preparation to sending bytes
                endif
                lda #$00
                sta bufferstatus        ;Reset amount of bytes in buffer

; -------------------------------------------------------------------
; This source code is altered and is not the original version found on
; the Exomizer homepage.
; It contains modifications made by Krill/Plush to depack a packed file
; crunched forward and to work with his loader.
;
; Further modification (error handling, loading under IO area) &
; bugfixing of the forward decruncher by Lasse Öörni
; -------------------------------------------------------------------
;
; Copyright (c) 2002 - 2005 Magnus Lind.
;
; This software is provided 'as-is', without any express or implied warranty.
; In no event will the authors be held liable for any damages arising from
; the use of this software.
;
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
;
;   1. The origin of this software must not be misrepresented; you must not
;   claim that you wrote the original software. If you use this software in a
;   product, an acknowledgment in the product documentation would be
;   appreciated but is not required.
;
;   2. Altered source versions must be plainly marked as such, and must not
;   be misrepresented as being the original software.
;
;   3. This notice may not be removed or altered from any distribution.
;
;   4. The names of this software and/or it's copyright holders may not be
;   used to endorse or promote products derived from this software without
;   specific prior written permission.
;
; -------------------------------------------------------------------
; no code below this comment has to be modified in order to generate
; a working decruncher of this source file.
; However, you may want to relocate the tables last in the file to a
; more suitable address.
; -------------------------------------------------------------------

; -------------------------------------------------------------------
; jsr this label to decrunch, it will in turn init the tables and
; call the decruncher
; no constraints on register content, however the
; decimal flag has to be #0 (it almost always is, otherwise do a cld)
exomizer:

  if FORWARD_DECRUNCHING>0

; -------------------------------------------------------------------
; init zeropage, x and y regs.
;
  ldx #3
init_zp:
  jsr load_getbyte
  bcs exomizer_error ;File not found
exomizer_chained:
  sta zp_bitbuf-1,x
  dex
  bne init_zp
  ldy #0

; -------------------------------------------------------------------
; calculate tables (50 bytes)
; x and y must be #0 when entering
;
nextone:
  inx
  tya
  and #$0f
  beq shortcut    ; start with new sequence

  txa          ; this clears reg a
  lsr          ; and sets the carry flag
  ldx tabl_bi-1,y
rolle:
  rol
  rol zp_bits_hi
  dex
  bpl rolle    ; c = 0 after this (rol zp_bits_hi)

  adc tabl_lo-1,y
  tax

  lda zp_bits_hi
  adc tabl_hi-1,y
shortcut:
  sta tabl_hi,y
  txa
  sta tabl_lo,y

  ldx #4
  jsr get_bits    ; clears x-reg.
  sta tabl_bi,y
  iny
  cpy #52
  bne nextone
  beq begin

; -------------------------------------------------------------------
; get bits (29 bytes)
;
; args:
;   x = number of bits to get
; returns:
;   a = #bits_lo
;   x = #0
;   c = 0
;   z = 1
;   zp_bits_hi = #bits_hi
; notes:
;   y is untouched
; -------------------------------------------------------------------
get_bits:
  lda #$00
  sta zp_bits_hi
  cpx #$01
  bcc bits_done
bits_next:
  lsr zp_bitbuf
  bne bits_ok
  pha
  jsr load_getbyte
  bcs exomizer_error
  sec
  ror
  sta zp_bitbuf
  pla
bits_ok:
  rol
  rol zp_bits_hi
  dex
  bne bits_next
bits_done:
  rts

exomizer_ok:
  jsr load_getbyte ;Check whether EOF or chained files
  bcs exomizer_eof
  ldx #3
  bcc exomizer_chained
exomizer_eof:
  cmp #$01
exomizer_error:
exomizer_stackptr:
  ldx #$ff
  txs
  if SCPU_SUPPORT>0
  sta $d07b ;SCPU to fast mode
  endif
  rts

; -------------------------------------------------------------------
; literal sequence handling
;
  if LITERAL_SEQUENCES_NOT_USED=0
literal_start:
  ldx #$10    ; these 16 bits
  jsr get_bits; tell the length of the sequence
  ldx zp_bits_hi
  endif
literal_start1: ; if literal byte, a = 1, zp_bits_hi = 0
  sta zp_len_lo

; -------------------------------------------------------------------
; main copy loop
; x = length hi
; y = length lo
;
copy_start:
  ldy #$00
copy_next:
  bcs copy_noliteral
  jsr load_getbyte
  bcs exomizer_error
copy_noliteral:
  if LOAD_UNDER_IO>0
  jsr disableio
  endif
  bcc copy_store
  lda (zp_src_lo),y
copy_store:
  sta (zp_dest_lo),y
  if LOAD_UNDER_IO>0
  jsr enableio
  endif
  iny
  bne copy_skiphi
  dex
  inc zp_dest_hi
  inc zp_src_hi
copy_skiphi:
  tya
  eor zp_len_lo
  bne copy_next
  txa
  bne copy_next
  tya
  clc
  adc zp_dest_lo
  sta zp_dest_lo
  bcc copy_skiphi2
  inc zp_dest_hi
copy_skiphi2:

; -------------------------------------------------------------------
; decruncher entry point, needs calculated tables (21(13) bytes)
; x and y must be #0 when entering
;
begin:
  inx
  jsr get_bits
  tay
  bne literal_start1; if bit set, get a literal byte
getgamma:
  inx
  jsr bits_next
  lsr
  iny
  bcc getgamma
  cpy #$11
  beq exomizer_ok   ; gamma = 17   : end of file
  if LITERAL_SEQUENCES_NOT_USED=0
  bcs literal_start ; gamma = 18   : literal sequence
  endif
                    ; gamma = 1..16: sequence

; -------------------------------------------------------------------
; calulate length of sequence (zp_len) (11 bytes)
;
  ldx tabl_bi-1,y
  jsr get_bits
  adc tabl_lo-1,y  ; we have now calculated zp_len_lo
  sta zp_len_lo
; -------------------------------------------------------------------
; now do the hibyte of the sequence length calculation (6 bytes)
  lda zp_bits_hi
  adc tabl_hi-1,y  ; c = 0 after this.
  pha
; -------------------------------------------------------------------
; here we decide what offset table to use (20 bytes)
; x is 0 here
;
  bne nots123
  ldy zp_len_lo
  cpy #$04
  bcc size123
nots123:
  ldy #$03
size123:
  ldx tabl_bit-1,y
  jsr get_bits
  adc tabl_off-1,y  ; c = 0 after this.
  tay      ; 1 <= y <= 52 here

; -------------------------------------------------------------------
; calulate absolute offset (zp_src)
;
  ldx tabl_bi,y
  jsr get_bits
  adc tabl_lo,y
  bcc skipcarry
  inc zp_bits_hi
skipcarry:
  sec
  eor #$ff
  adc zp_dest_lo
  sta zp_src_lo
  lda zp_dest_hi
  sbc zp_bits_hi
  sbc tabl_hi,y
  sta zp_src_hi

; -------------------------------------------------------------------
; prepare for copy loop (8(6) bytes)
;
  pla
  tax
  sec
  jmp copy_start

  else

; -------------------------------------------------------------------
; init zeropage, x and y regs. (12 bytes)
;
  ldx #3
init_zp:
  jsr load_getbyte
  bcs exomizer_error
exomizer_chained:
  sta zp_bitbuf-1,x
  dex
  bne init_zp
  ldy #0

; -------------------------------------------------------------------
; calculate tables (50 bytes)
; x and y must be #0 when entering
;
nextone:
  inx
  tya
  and #$0f
  beq shortcut              ; starta på ny sekvens
  txa                       ; this clears reg a
  lsr                       ; and sets the carry flag
  ldx tabl_bi-1,y
rolle:          
  rol
  rol zp_bits_hi
  dex
  bpl rolle                 ; c = 0 after this (rol zp_bits_hi)
  adc tabl_lo-1,y
  tax
  lda zp_bits_hi
  adc tabl_hi-1,y
shortcut:       
  sta tabl_hi,y
  txa
  sta tabl_lo,y
  ldx #4
  jsr get_bits              ; clears x-reg.
  sta tabl_bi,y
  iny
  cpy #52
  bne nextone
  ldy #0
  beq begin

; -------------------------------------------------------------------
; get bits (29 bytes)
;
; args:
;   x = number of bits to get
; returns:
;   a = #bits_lo
;   x = #0
;   c = 0
;   z = 1
;   zp_bits_hi = #bits_hi
; notes:
;   y is untouched
; -------------------------------------------------------------------
get_bits:       
  lda #$00
  sta zp_bits_hi
  cpx #$01
  bcc bits_done
bits_next:      
  lsr zp_bitbuf
  bne bits_ok
  pha
literal_get_byte:
  php
  jsr load_getbyte
  bcs exomizer_error
  plp
  bcc literal_byte_gotten
  ror
  sta zp_bitbuf
  pla
bits_ok:
  rol
  rol zp_bits_hi
  dex
  bne bits_next
bits_done:
  rts

exomizer_ok:
  jsr load_getbyte ;Check whether EOF or chained files
  bcs exomizer_eof
  ldx #3
  bcc exomizer_chained
exomizer_eof:
  cmp #$01
exomizer_error:
exomizer_stackptr:
  ldx #$ff
  txs
  if SCPU_SUPPORT>0
  sta $d07b ;SCPU to fast mode
  endif
  rts

; -------------------------------------------------------------------
; main copy loop (18(16) bytes)
;
copy_next_hi:
  dex
  dec zp_dest_hi
  dec zp_src_hi
copy_next:
  dey
  if LITERAL_SEQUENCES_NOT_USED = 0
  bcc literal_get_byte
  endif
literal_byte_gotten:
  if LOAD_UNDER_IO>0
  jsr disableio
  endif
  bcc copy_store
  lda (zp_src_lo),y
copy_store:
  sta (zp_dest_lo),y
  if LOAD_UNDER_IO>0
  jsr enableio
  endif
copy_start:
  tya
  bne copy_next
begin:
  txa
  bne copy_next_hi
; -------------------------------------------------------------------
; decruncher entry point, needs calculated tables (21(13) bytes)
; x and y must be #0 when entering
;
  if LITERAL_SEQUENCES_NOT_USED = 0
  inx
  jsr get_bits
  tay
  bne literal_start1
  else
  dey
  endif
begin2:
  inx
  jsr bits_next
  lsr
  iny
  bcc begin2
  if LITERAL_SEQUENCES_NOT_USED > 0
  beq literal_start
  endif
  cpy #$11
  if LITERAL_SEQUENCES_NOT_USED = 0
  bcc sequence_start
  beq exomizer_ok
; -------------------------------------------------------------------
; literal sequence handling (13(2) bytes)
;
  ldx #$10
  jsr get_bits
literal_start1: 
  sta zp_len_lo
  ldx zp_bits_hi
  ldy #0
  bcc literal_start
sequence_start:
  else
  bcs exomizer_ok
  endif
; -------------------------------------------------------------------
; calulate length of sequence (zp_len) (11 bytes)
;
  ldx tabl_bi-1,y
  jsr get_bits
  adc tabl_lo-1,y         ; we have now calculated zp_len_lo
  sta zp_len_lo
; -------------------------------------------------------------------
; now do the hibyte of the sequence length calculation (6 bytes)
  lda zp_bits_hi
  adc tabl_hi-1,y         ; c = 0 after this.
  pha
; -------------------------------------------------------------------
; here we decide what offset table to use (20 bytes)
; x is 0 here
;
  bne nots123
  ldy zp_len_lo
  cpy #$04
  bcc size123
nots123:        
  ldy #$03
size123:        
  ldx tabl_bit-1,y
  jsr get_bits
  adc tabl_off-1,y          ; c = 0 after this.
  tay                       ; 1 <= y <= 52 here
; -------------------------------------------------------------------
; Here we do the dest_lo -= len_lo subtraction to prepare zp_dest
; but we do it backwards:                a - b == (b - a - 1) ^ ~0 (C-syntax)
; (16(16) bytes)
  lda zp_len_lo
literal_start:  
  sbc zp_dest_lo            ; literal enters here with y = 0, c = 1
  bcc noborrow
  dec zp_dest_hi
noborrow:
  eor #$ff
  sta zp_dest_lo
  cpy #$01                  ; y < 1 then literal
  if LITERAL_SEQUENCES_NOT_USED = 0
  bcc pre_copy
  else
  bcc literal_get_byte
  endif
; -------------------------------------------------------------------
; calulate absolute offset (zp_src) (27 bytes)
;
  ldx tabl_bi,y
  jsr get_bits;
  adc tabl_lo,y
  bcc skipcarry
  inc zp_bits_hi
  clc
skipcarry:
  adc zp_dest_lo
  sta zp_src_lo
  lda zp_bits_hi
  adc tabl_hi,y
  adc zp_dest_hi
  sta zp_src_hi
; -------------------------------------------------------------------
; prepare for copy loop (8(6) bytes)
;
  pla
  tax
  sec
  if LITERAL_SEQUENCES_NOT_USED = 0
pre_copy:
  ldy zp_len_lo
  jmp copy_start
  else
  ldy zp_len_lo
  bcs copy_start
  endif

  endif

; -------------------------------------------------------------------
; two small static tables (6(6) bytes)
;
tabl_bit:
	.byte 2,4,4
tabl_off:
	.byte 48,32,16
; -------------------------------------------------------------------
; end of decruncher
; -------------------------------------------------------------------
                endif

;-------------------------------------------------------------------------------
; LOAD_GETBYTE
;
; Gets a byte from an opened file.
;
; Parameters: -
; Returns: C=0 OK, A contains byte
;          C=1 File stream ended. A contains the error code:
;              $00 - OK, end of file
;              $01 - Sector read error (only with fastloading)
;              $02 - File not found
; Modifies: A[,X if LOADFILE_EXOMIZER+LOADFILE_PUCRUNCH=0,
;              Y if TWOBIT_PROTOCOL=0 &&
;              LOADFILE_EXOMIZER+LOADFILE_BBOOZER2=0]
;-------------------------------------------------------------------------------

load_getbyte:
                if LOADFILE_EXOMIZER+LOADFILE_BBOOZER2>0
                sty load_getbyteresty+1
                endif
                if LOADFILE_EXOMIZER+LOADFILE_PUCRUNCH>0
                stx load_getbyterestx+1
                endif

                if RECEIVE_BUFFER=0
                ldx bufferstatus        ;Bytes still in disk buffer?
                beq load_fillbuffer
load_getbyteinner:
                jsr getbyte
                dex
                stx bufferstatus 
                if BORDER_FLASHING>0
                inc $d020               ;Oldskool effect
                dec $d020
                endif
                clc
load_getbytedone:
                if LOADFILE_EXOMIZER+LOADFILE_BBOOZER2>0
load_getbyteresty:
                ldy #$00
                endif
                if LOADFILE_EXOMIZER+LOADFILE_PUCRUNCH>0
load_getbyterestx:
                ldx #$00
                endif
                rts

load_fillbuffer:
                jsr getbyte             ;Get number of bytes to transfer
                beq load_end            ;$00 means load end (either OK or error)
                sta bufferstatus
                tax
                bne load_getbyteinner

                else

                ldx bufferstatus        ;Bytes still in buffer?
                beq load_fillbuffer
load_getbyteinner:
                lda loadbuffer-1,x
                dex
                stx bufferstatus 
                if BORDER_FLASHING>0
                inc $d020               ;Oldskool effect
                dec $d020
                endif
                clc
load_getbytedone:
                if LOADFILE_EXOMIZER+LOADFILE_BBOOZER2>0
load_getbyteresty:
                ldy #$00
                endif
                if LOADFILE_EXOMIZER+LOADFILE_PUCRUNCH>0
load_getbyterestx:
                ldx #$00
                endif
                rts

load_fillbuffer:
                jsr getbyte             ;Get number of bytes to transfer
                beq load_end            ;$00 means load end (either OK or error)
                sta bufferstatus
                ldx #$00
load_fillbufferloop:
                jsr getbyte             ;Fill the buffer in a loop
                sta loadbuffer,x
                inx
                cpx bufferstatus
                bcc load_fillbufferloop
                bcs load_getbyteinner
                endif

load_end:       jsr getbyte             ;Get reasoncode ($00 = OK, higher = error)
                sec
                bcs load_getbytedone

                if LOAD_UNDER_IO>0
;-------------------------------------------------------------------------------
; DISABLEIO
;
; Stores $01 status, disables interrupts & IO area.
;
; Parameters: -
; Returns: -
; Modifies: -
;-------------------------------------------------------------------------------

disableio:      pha
                lda $01
                sta enableio_01+1
                lda #$34
                sei
                sta $01
                pla
                rts

;-------------------------------------------------------------------------------
; ENABLEIO
;
; Restores $01 status and enables interrupts
;
; Parameters: -
; Returns: -
; Modifies: -
;-------------------------------------------------------------------------------

enableio:       pha
enableio_01:    lda #$36
                sta $01
                cli
                pla
                rts
                endif

;-------------------------------------------------------------------------------
; GETBYTE
;
; Gets one byte from the diskdrive; with 2-bit protocol sprites must be off.
;
; Parameters: -
; Returns: A:Byte,Z=0 if A=0
; Modifies: A[,Y if TWOBIT_PROTOCOL=0]
;-------------------------------------------------------------------------------

getbyte:
                if TWOBIT_PROTOCOL>0
                lda $dd00               ;CLK low
                ora #$10
                sta $dd00
getbyte_wait:   bit $dd00               ;Wait for 1541 to signal data ready by
                bmi getbyte_wait        ;setting DATA low
                sei
getbyte_waitbadline:
                lda $d011
                clc                     ;Wait until a badline won't distract
                sbc $d012               ;the timing
                and #$07
                beq getbyte_waitbadline
                lda $dd00
                and #$03
                sta $dd00               ;Set CLK high
getbyte_delay:  bpl getbyte_delay2
getbyte_delay2: nop
                nop
                nop
                nop
                sta getbyte_eor+1
                lda $dd00
                lsr
                lsr
                eor $dd00
                lsr
                lsr
                eor $dd00
                lsr
                lsr
getbyte_eor:    eor #$00
                eor $dd00
                cli

                else

getbyte_wait:   bit $dd00               ;Wait until 1541 is ready to send
                bvc getbyte_wait        ;(CLK=high)
                lda #$0f
                and $dd00
                sta $dd00
                nop
                ldy #$08                ;Bit counter
getbyte_bitloop:
                nop
                nop
                lda #$10
                eor $dd00               ;Take databit from serialport and
                sta $dd00               ;store reversed clockbit
                asl
                rol loadtempreg
                lda loadtempreg
                dey
                bne getbyte_bitloop     ;All bits done?

                lda loadtempreg         ;Force Z
                endif

                rts

;-------------------------------------------------------------------------------
; SENDBYTE
;
; Sends one byte to the diskdrive with asynchronous protocol (no timing
; requirements on C64 or 1541 side)
;
; Parameters: A:Byte
; Returns: -
; Modifies: A,Y
;-------------------------------------------------------------------------------

sendbyte:       sta loadtempreg
                ldy #$08                ;Bit counter
sendbyte_bitloop:
                if TWOBIT_PROTOCOL=0
                bit $dd00               ;Wait for CLK & DATA high
                bvc sendbyte_bitloop
                bpl sendbyte_bitloop
                endif

                lsr loadtempreg         ;Rotate byte to be sent
                lda $dd00
                and #$ff-$30
                ora #$10
                bcc sendbyte_zerobit
                eor #$30
sendbyte_zerobit:
                sta $dd00
                lda #$c0                ;Wait for CLK & DATA low
sendbyte_ack:   bit $dd00
                bne sendbyte_ack
                lda $dd00
                and #$ff-$30            ;Set DATA and CLK high
                sta $dd00

                if TWOBIT_PROTOCOL>0
sendbyte_ack2:  bit $dd00               ;Wait for CLK & DATA high
                bvc sendbyte_ack2
                bpl sendbyte_ack2
                endif

                dey
                bne sendbyte_bitloop
sendbyte_endloop:
                rts

;-------------------------------------------------------------------------------
; Loader variables, if not in zeropage
;-------------------------------------------------------------------------------

                if ADDITIONAL_ZEROPAGE=0
loadtempreg:    dc.b 0                  ;Temp variable for the loader
bufferstatus:   dc.b 0                  ;Bytes in fastload buffer
                endif

;-------------------------------------------------------------------------------
; Load buffer
;-------------------------------------------------------------------------------

                if RECEIVE_BUFFER>0
loadbuffer:     ds.b 254,0
                endif
