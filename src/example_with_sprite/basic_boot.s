;-------------------------------------------------------------------------------
; BASIC boot program by Luigi Di Fraia 9/2017
;-------------------------------------------------------------------------------

                processor 6502
                org $0801

                dc.w bnext              ;Address of next BASIC instruction
                dc.w 2017               ;Line number
                dc.b $9e                ;SYS-token
                dc.b $32,$30,$36,$31    ;2061 in ASCII
                dc.b $00                ;Line terminator
bnext:          dc.w 0                  ;Line number = 0, end of program
