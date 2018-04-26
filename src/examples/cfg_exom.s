;-------------------------------------------------------------------------------
; Exomizer configuration
;-------------------------------------------------------------------------------

TWOBIT_PROTOCOL = 1                     ;Nonzero to use 2-bit protocol which may delay
                                        ;interrupts and does not allow sprites, but is
                                        ;faster. Zero to use 1-bit protocol which is
                                        ;the opposite.
BORDER_FLASHING = 1                     ;Set to nonzero to enable border flashing
                                        ;when fastloading :)
LED_FLASHING    = 1                     ;Set to nonzero to enable drive LED flashing
                                        ;when reading sectors.
ADDITIONAL_ZEROPAGE = 1                 ;Set to nonzero to use additional zeropage
                                        ;variables to shorten loader code.
RECEIVE_BUFFER  = 1                     ;Nonzero to enable the receive buffer and speed
                                        ;up transfer.
SCPU_SUPPORT    = 0                     ;Nonzero to enable/disable SuperCPU's slow mode.
LOAD_UNDER_IO   = 0                     ;Set to nonzero to enable possibility to load
                                        ;under I/O areas, and to load packed data
                                        ;under BASIC and Kernal ROMs.
LOADFILE_UNPACKED = 0                   ;Set to nonzero to include unpacked loading.
LOADFILE_BBOOZER2 = 0                   ;Set to nonzero to include BYTEBOOZER loading.
LOADFILE_EXOMIZER = 1                   ;Set to nonzero to include EXOMIZER loading.
LOADFILE_PUCRUNCH = 0                   ;Set to nonzero to include PUCRUNCH loading.
START_ADDRESS_TRIMMED_BB2 = 0           ;(BYTEBOOZER only): set to nonzero for shorter
                                        ;depacker if you use the -b switch in the
                                        ;extended version of BYTEBOOZER.
HEADER_TRIMMED_PUCRUNCH = 0             ;(PUCRUNCH only): set to nonzero for shorter
                                        ;depacker if you (manually) removed the first
                                        ;six bytes from the compressed files to load.
LITERAL_SEQUENCES_NOT_USED = 0          ;(EXOMIZER only): set to nonzero for shorter
                                        ;depacker, if you use -c switch to disable
                                        ;literal sequences in Exomizer 2, or if you
                                        ;use Exomizer 1.
FORWARD_DECRUNCHING = 0                 ;(EXOMIZER only): set to nonzero if you use -f
                                        ;switch in Exomizer 2, zero for Exomizer 1.
depackbuffer    = $0500                 ;156 bytes for EXOMIZER tables, 31 for
                                        ;PUCRUNCH.
zpbase          = $74                   ;Zeropage base address. Loader needs 2
                                        ;addresses with unpacked, 3 with BYTEBOOZER,
                                        ;3 with PUCRUNCH, and 8 with EXOMIZER loading.
zpbase2         = $02                   ;Additional 2 zeropage addresses for shortening
                                        ;the loader code (optional).
