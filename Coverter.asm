;*******************************************************
;* CMPEN 472, HW10 sample 3 program,  converting analog 
;*            signal to digital data, MC9S12C128 Program
;*
;* Siddharth Dutta
;* CMPEN 472  
;* Date: 4/21/2020
;*
;* Term ATD = ADC = Analog-to-Digital Converter
;* 
;*   Sample program for ADC Testing
;*   non-interrupt, busy wait method (different from Homework 10)
;*
;* For SIMULATOR:
;*   Serial communication at fast rate (1,500,000 baud).
;*   Typewriter program, but when 'enter' key hit, ADC number printed.
;*   Single AD conversion, 8bit, right justified.
;* For CSM-12C128 Board:
;*   Serial communication at 9600 baud only (different from Homework 10)
;*   This program prints what user types on a terminal 
;*   window - ie. a typewriter program.  However, when a user hits an 
;*   'enter' key, a hexadecimal number will be printed on the terminal. 
;*   The hexadecimal number represents the analog voltage level of the
;*   ADC input channel 7 (pin number 10 of the CSM-12C128 board Connector J1).
;*   The program does single AD conversion in 10bit but only the upper
;*   8bit is used for the conversion to the hexadecimal number.
;*   Freescale CodeWarrior,  for the MC9S12C128 Program
;*   Target: Axiom's CSM-12C128 Board, ASCII Monitor Mode
;*******************************************************
; export symbols
            XDEF      Entry       ; export 'Entry' symbol
            ABSENTRY  Entry       ; for assembly entry point
  
; symbols/addresses
PTP         EQU         $0258        ; I/O port P, these two lines for simulator
DDRP        EQU         $025A        ; I/O port P data direction control

ATDCTL2     EQU  $0082            ; Analog-to-Digital Converter (ADC) registers
ATDCTL3     EQU  $0083
ATDCTL4     EQU  $0084
ATDCTL5     EQU  $0085
ATDSTAT0    EQU  $0086
ATDDR0H     EQU  $0090
ATDDR0L     EQU  $0091
ATDDR7H     EQU  $009e
ATDDR7L     EQU  $009f

SCIBDH      EQU  $00c8            ; Serial port (SCI) Baud Rate Register H
SCIBDL      EQU  $00C9            ; Serial port (SCI) Baud Register L
SCICR2      EQU  $00CB            ; Serial port (SCI) Control Register 2
SCISR1      EQU  $00cc            ; Serial port (SCI) Status Register 1
SCIDRL      EQU  $00cf            ; Serial port (SCI) Data Register

TIOS        EQU  $0040        ; Timer Input Capture (IC) or Output Compare (OC) select
TIE         EQU  $004C        ; Timer interrupt enable register
TCNTH       EQU  $0044        ; Timer free runing main counter
TSCR1       EQU  $0046        ; Timer system control 1
TSCR2       EQU  $004D        ; Timer system control 2
TFLG1       EQU  $004E        ; Timer interrupt flag 1
TC2H        EQU  $0054        ; Timer channel 2 register

CR          equ  $0d              ; carriage return, ASCII 'Return' key
LF          equ  $0a              ; line feed, ASCII 'next line' character

;*******************************************************
; variable/data section
            ORG     $3000            ; RAMStart defined as $3000
ATDdone     DS.B    1               ; ADC finish indicator, 1 = ATD finished
ctr125u     DS.W    1                ; 16bit interrupt counter for 125 uSec. of time
CTR         DS.B    1                ; To print an 8 bit number in decimal, 
BUF         DS.B    3                ; needs work space (4 byte memory locations).
msg1        DC.B    'Hello, this is 1024 data capture program.', $00
msg2        DC.B    'Hit Enter key and change to 115.2K baud', $00
;msg3        DC.B    'Hit enter key again after changing the Terminal baud rate to 115.2K baud', $00
msg4        DC.B    'Baud rate changed to 115.2K baud. Press the switch SW1, for 1024 point data receive.', $00
;msg5        DC.B    'Stop the simulator Terminal text capture.', $00
;msg6        DC.B    'Then see and edit the text file for 1024 point data received.', $00
msg7        DC.B    'ADC done. Press SW1 to repeat.', $00

;*******************************************************
;*******************************************************
; interrupt vector section

;            ORG     $3FEA            ; Timer channel 2 interrupt vector setup, HC12 board
            ORG     $FFEA            ; Timer channel 2 interrupt vector setup, simulator
            DC.W    oc2isr

;*******************************************************
; code section
            ORG  $3100
Entry
            LDS   #Entry           ; initialize the stack pointer
            
            BSET    DDRP,%00000001   ; For SIMULATION, Push Button Switch 1 at PORTP bit 0
                                     ; set as output, but used as input when run
            ldaa    #%00000000
            staa    PTP                         

            ldd     #$009C    ; For SIMULATION, Set SCI Baud Register = $0001 => 1500000 baud at 24MHz
            std     SCIBDH    ; SCI port baud rate change
            ldaa    #$0C      ; Enable SCI port Tx and Rx units
            staa    SCICR2    ; disable SCI interrupts
            
; ATD initialization
            LDAA  #%11000000       ; Turn ON ADC, clear flags, Disable ATD interrupt
            STAA  ATDCTL2
            LDAA  #%00001000       ; Single conversion per sequence, no FIFO
            STAA  ATDCTL3
;            LDAA  #%01000111       ; 10bit, ADCLK=24MHz/16=1.5MHz, sampling time=8*(1/ADCLK)
;            STAA  ATDCTL4
            LDAA  #%10000111       ; 8bit, ADCLK=24MHz/16=1.5MHz, sampling time=2*(1/ADCLK)
            STAA  ATDCTL4          ; for SIMULATION

; Guide user, instruction
            ldx   #msg1            ; print the first message, 'Hello'
            jsr   printmsg
            jsr   nextline
            ldx   #msg2            ; print the first message, 'Hello'
            jsr   printmsg
            jsr   nextline
            ;ldx   #msg3            ; print the first message, 'Hello'
            ;jsr   printmsg
            ;jsr   nextline


loop1       jsr   getchar          ; type writer - what is typed on key board
            jsr   putchar          ; is displayed on the terminal window
            cmpa  #CR
            bne   loop1            ; if Enter/Return key is pressed, move the
            ldaa  #LF              ; cursor to next line
            ldd   #$0001            ; For SIMULATION, Set SCI Baud Register = $0001 => 1500000 baud at 24MHz
            std   SCIBDH            ; SCI port baud rate change
            jsr   putchar
            ldx   #msg4            ; print the first message, 'Hello'
            jsr   printmsg
            jsr   nextline
            ;ldx   #msg5            ; print the first message, 'Hello'
            ;jsr   printmsg
            ;jsr   nextline
            ;ldx   #msg6            ; print the first message, 'Hello'
            ;jsr   printmsg
            ;jsr   nextline
            
 
            
            
loopTx      LDAA    PTP             ; For SIMULATOR, read push button SW1 at PORTP0

            ANDA    #%00000001      ; check the bit 0 only
            BEQ     loopTx          ; For SIMULATOR, SW1 not pushed
            jsr     StartTimer2oc  

loop1024    ldx     ctr125u              ; SW1 pushed
            cpx     #1024           ; 1024 bytes will be sent, the receiver at Windows PC 
            bne     loop1024
            sei
            ldx     #0
            stx     ctr125u
            ldx     #msg7            ; print the first message, 'Hello'
            jsr     printmsg
            jsr     nextline
            

;* For SIMULATION only
loopTxON    LDAA    PTP             ; For SIMULATOR, read push button SW1 at PORTP0
            ANDA    #%00000001      ; check the bit 0 only
            BNE     loopTxON        ; SW1 pushed again?
            BRA     loopTx            
                        

;*******************************************************
; subroutine section

;***********Timer OC2 interrupt service routine***************
oc2isr      jsr   go2ADC
            ldd   #3000              ; 125usec with (24MHz/1 clock)
            addd  TC2H               ;    for next interrupt
            std   TC2H               ;    + Fast clear timer CH2 interrupt flag
            bset  TFLG1,%00000100    ; clear timer CH2 interrupt flag, not needed if fast clear enabled
            ldx   ctr125u            ; 125uSec => 8.000KHz rate
            inx
            stx   ctr125u            ; every time the RTI occur, increase interrupt count
            
oc2done     RTI
;***********end of Timer OC2 interrupt service routine********

;***********single AD conversiton*********************
; This is a sample, non-interrupt, busy wait method
;
go2ADC
            PSHA                   ; Start ATD conversion
            LDAA  #%10000111       ; right justified, unsigned, single conversion,
            STAA  ATDCTL5          ; single channel, CHANNEL 7, start the conversion

                                   
adcwait     ldaa  ATDSTAT0         ; Wait until ATD conversion finish
            anda  #%10000000       ; check SCF bit, wait for ATD conversion to finish
            beq   adcwait
            
            ldaa  ATDDR0L          ; for SIMULATOR, pick up the lower 8bit result
;            ldaa  ATDDR0H          ; pick up the upper 8bit result
            jsr   pnum10          ; print the ATD result
            ;jsr   nextline

            PULA
            RTS
;***********end of AD conversiton************** 
  

;***************StartTimer2oc************************
;* Program: Start the timer interrupt, timer channel 2 output compare
;* Input:   Constants - channel 2 output compare, 125usec at 24MHz
;* Output:  None, only the timer interrupt
;* Registers modified: D used and CCR modified
;* Algorithm:
;             initialize TIOS, TIE, TSCR1, TSCR2, TC2H, and TFLG1
;**********************************************
StartTimer2oc
            PSHD
            LDAA   #%00000100
            STAA   TIOS              ; set CH2 Output Compare
            STAA   TIE               ; set CH2 interrupt Enable
            LDAA   #%10010000        ; enable timer and set Fast Flag Clear
            STAA   TSCR1
            LDAA   #%00000000        ; TOI Off, TCRE Off, TCLK = BCLK/1
            STAA   TSCR2             ;   not needed if started from reset

            LDD     #3000            ; 125usec with (24MHz/1 clock)
            ADDD    TCNTH            ;    for first interrupt
            STD     TC2H             ;    + Fast clear timer CH2 interrupt flag

            PULD
            BSET   TFLG1,%00000100   ; initial Timer CH2 interrupt flag Clear, not needed if fast clear set
            CLI                      ; enable interrupt
            RTS
;***************end of StartTimer2oc*****************         

;***********pnum10***************************
;* Program: print a byte in decimal to SCI port
;* Input:   Register A contains an 8 bit number to print in decimal number
;* Output:  decimal number printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Keep divide number by 10 and keep the remainders
;     Then send it out to SCI port
;  Need memory location for counter CTR and buffer BUF(3 byte max)
;**********************************************
pnum10          pshd                   ;Save registers
                pshx
                pshy
                clr     CTR            ; clear character count of an 8 bit number
                clr     BUF            ; convert 8bit to 16bit, clear upper byte of 
                staa    BUF+1          ; a 16bit number
                ldd     BUF
                ldy     #BUF
pnum10p1        ldx     #10
                idiv
                beq     pnum10p2
                stab    1,y+
                inc     CTR
                tfr     x,d
                bra     pnum10p1
pnum10p2        stab    1,y+
                inc     CTR                        
;-------------------------------------------------------------------------------
pnum10p3        ldaa    #$30                
                adda    1,-y
                jsr     putchar
                dec     CTR
                bne     pnum10p3
                jsr     nextline
                puly
                pulx
                puld
                rts
;***********end of pnum10********************
 
;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* C
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL            equ     $00
printmsg        psha                   ;Save registers
                pshx
printmsgloop    ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
                cmpa    #NULL
                beq     printmsgdone   ;end of strint yet?
                jsr     putchar        ;if not, print character and do next
                bra     printmsgloop
printmsgdone    pulx 
                pula
                rts
;***********end of printmsg********************

;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar     brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
            staa  SCIDRL                       ; send a character
            rts
;***************end of putchar*****************

;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, other wise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************

getchar     brclr SCISR1,#%00100000,getchar7
            ldaa  SCIDRL
            rts
getchar7    clra
            rts
;****************end of getchar**************** 

;****************nextline**********************
nextline    ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            rts
;****************end of nextline***************

*
* Add any subroutines here
*



            END               ; this is end of assembly source file
                              ; lines below are ignored - not assembled/compiled