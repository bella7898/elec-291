; LCD_test_4bit.asm: Initializes and uses an LCD in 4-bit mode
; using the most common procedure found on the internet and datasheets.
$NOLIST
$MODN76E003
$LIST

org 0000H
    ljmp myprogram
    
org 0050H
MYSTRING1: db 'PLAYER: Bella W PLAYER 2: GUEST ',0 ;db define byte 66H - 99H?
MYSTRING2: db 'ID: 12345678    ID: 12345678    ',0
MYSTRING3: db 'LOADING...',0
MYSTRING4: db 'PLAY AGAIN :)',0
MYSTRING5: db 'SCORE:',0

;  N76E003 pinout:
;                               -------
;       PWM2/IC6/T0/AIN4/P0.5 -|1    20|- P0.40/AIN5/STADC/PWM3/IC3
;               TXD/AIN3/P0.6 -|2    19|- P0.3/PWM5/IC5/AIN6
;               RXD/AIN2/P0.7 -|3    18|- P0.2/ICPCK/OCDCK/RXD_1/[SCL]
;                    RST/P2.0 -|4    17|- P0.1/PWM4/IC4/MISO
;        INT0/OSCIN/AIN1/P3.0 -|5    16|- P0.0/PWM3/IC3/MOSI/T1
;              INT1/AIN0/P1.7 -|6    15|- P1.0/PWM2/IC2/SPCLK
;                         GND -|7    14|- P1.1/PWM1/IC1/AIN7/CLO
;[SDA]/TXD_1/ICPDA/OCDDA/P1.6 -|8    13|- P1.2/PWM0/IC0
;                         VDD -|9    12|- P1.3/SCL/[STADC]
;            PWM5/IC7/SS/P1.5 -|10   11|- P1.4/SDA/FB/PWM1
;                               -------
;

; These 'equ' must match the hardware wiring
LCD_RS equ P1.3
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P1.4
LCD_D4 equ P0.0
LCD_D5 equ P0.1
LCD_D6 equ P0.2
LCD_D7 equ P0.3

LED_RED equ P1.7 ;new red led (pin 6) active low
LED_GR1 equ P3.0 ;new green led (pin 5) active low
BUT_TON equ P1.2 ;new button (pin 13) active low
Score equ 0x30

; When using a 16.6MHz oscillator in the N76E003
; one cycle takes 1.0/16.6MHz = 60.24 ns

;---------------------------------;
; Wait 40 microseconds            ;
;---------------------------------;
Wait40uSec:
    push AR0
    mov R0, #133
L0:
    nop
    djnz R0, L0 ; 1+4 cycles->5*60.24ns*133=40us
    pop AR0
    ret

;---------------------------------;
; Wait 'R2' milliseconds          ;
;---------------------------------;
WaitmilliSec:
    push AR0
    push AR1
L3: mov R1, #40
L2: mov R0, #104
L1: djnz R0, L1 ; 4 cycles->4*60.24ns*104=25.0us
    djnz R1, L2 ; 25us*40=1.0ms
    djnz R2, L3 ; number of millisecons to wait passed in R2
    pop AR1
    pop AR0
    ret
    
WaitHalfSec: 
	mov R2,#250
	lcall WaitmilliSec
	mov R2,#250
	lcall WaitmilliSec
	ret

;---------------------------------;
; Toggles the LCD's 'E' pin       ;
;---------------------------------;
LCD_pulse:
    setb LCD_E
    lcall Wait40uSec
    clr LCD_E
    ret

;---------------------------------;
; Writes data to LCD              ;
;---------------------------------;
WriteData:
    setb LCD_RS
    ljmp LCD_byte

;---------------------------------;
; Writes command to LCD           ;
;---------------------------------;
WriteCommand:
    clr LCD_RS
    ljmp LCD_byte

;---------------------------------;
; Writes acc to LCD in 4-bit mode ;
;---------------------------------;
LCD_byte:
    ; Write high 4 bits first
    mov c, ACC.7
    mov LCD_D7, c
    mov c, ACC.6
    mov LCD_D6, c
    mov c, ACC.5
    mov LCD_D5, c
    mov c, ACC.4
    mov LCD_D4, c
    lcall LCD_pulse

    ; Write low 4 bits next
    mov c, ACC.3
    mov LCD_D7, c
    mov c, ACC.2
    mov LCD_D6, c
    mov c, ACC.1
    mov LCD_D5, c
    mov c, ACC.0
    mov LCD_D4, c
    lcall LCD_pulse
    ret

;---------------------------------;
; Configure LCD in 4-bit mode     ;
;---------------------------------;
LCD_4BIT:
    clr LCD_E   ; Resting state of LCD's enable is zero
    ; clr LCD_RW  ; Not used, pin tied to GND

    ; After power on, wait for the LCD start up time before initializing
    mov R2, #40
    lcall WaitmilliSec

    ; First make sure the LCD is in 8-bit mode and then change to 4-bit mode
    mov a, #0x33
    lcall WriteCommand
    mov a, #0x33
    lcall WriteCommand
    mov a, #0x32 ; change to 4-bit mode
    lcall WriteCommand

    ; Configure the LCD
    mov a, #0x28
    lcall WriteCommand
    mov a, #0x0c
    lcall WriteCommand
    mov a, #0x01 ;  Clear screen command (takes some time)
    lcall WriteCommand

    ;Wait for clear screen command to finish. Usually takes 1.52ms.
    mov R2, #2
    lcall WaitmilliSec
    ret

;---------------------------------;
; Main loop.  Initialize stack,   ;
; ports, LCD, and displays        ;
; letters on the LCD              ;
;---------------------------------;
myprogram:
    mov SP, #7FH
    ; Configure the pins as bi-directional so we can use them as input/output
    mov P0M1, #0x00
    mov P0M2, #0x00
    mov P1M1, #0x00
    mov P1M2, #0x00 
    mov P3M1, #0x00
    mov P3M2, #0x00
    
    setb LED_RED ;all led / buttons are initially OFF
    setb LED_GR1
    setb BUT_TON
    ;setb BUT_TON1
;---------------------------------;
; Print Name + Student Number to  ;
; LCD Screen                      ;
;---------------------------------;
    lcall LCD_4BIT
    
    mov a, #0x80 ;mov a to first line of LCD
    lcall WriteCommand
    
    mov DPTR, #MYSTRING1 ;data ptr gets beginning address of MYSTRING1
PRINT1:
	mov A, #0 ;0 address
	movc A, @A+DPTR ;Move Code byte relative to DPTR to Acc
	cjne A, #0, SEND1 ;Compare immediate to Acc and Jump if Not Equal
	sjmp DONE1

SEND1:
	lcall WriteData
	inc DPTR
	sjmp PRINT1
    
DONE1: 
    mov a, #0xC0 ; Move cursor to line 2
    lcall WriteCommand
    
   	mov DPTR, #MYSTRING2
PRINT2: 
	mov A, #0
	movc A, @A + DPTR
	cjne A, #0, SEND2
	
	mov R6, #1
BIGSCROLL:
	lcall WaitHalfSec
	lcall WaitHalfSec
	lcall WaitHalfSec
	lcall WaitHalfSec
	
	mov R7, #16
SCROLLLEFT:
	mov A, #0x18
	lcall WriteCommand
	lcall WaitmilliSec
	djnz R7, SCROLLLEFT

	mov R7, #16
SCROLLRIGHT:
	mov A, #0x1C
	lcall WriteCommand
	lcall WaitmilliSec
	djnz R7, SCROLLRIGHT

	djnz R6, BIGSCROLL 
	
	sjmp WAITFORSTART

SEND2: 
	lcall WriteData
	inc DPTR
	sjmp PRINT2
	
;---------------------------------;
; LOADING SCREEN ANIMATIONS       ;
;---------------------------------;

WAITFORSTART:                     
	clr LED_GR1 ;turn ON red led
	setb LED_RED
    jnb BUT_TON, CLEARSCREEN
    ;jnb BUT_TON1, SCROLL
	lcall WaitmilliSec
	sjmp WAITFORSTART
	
CLEARSCREEN: 
	mov A, #01H
	lcall WriteCommand
	lcall WaitmilliSec
    
    mov DPTR, #MYSTRING3 ;data ptr gets beginning address of MYSTRING1
PRINT3:
	mov A, #0 ;0 address
	movc A, @A+DPTR ;Move Code byte relative to DPTR to Acc
	cjne A, #0, SEND3 ;Compare immediate to Acc and Jump if Not Equal
	sjmp LOADINGANIMATION

SEND3:
	lcall WriteData
	inc DPTR
	sjmp PRINT3
	
LOADINGANIMATION:
	mov R7, #16
	
	mov A, #0x58 ; Memory location for first byte of custom character 0
	lcall WriteCommand
	
	; Pixel pattern
	mov A, #11111B
	lcall WriteData
	mov A, #11111B
	lcall WriteData
	mov A, #11111B
	lcall WriteData
	mov A, #11111B
	lcall WriteData
	mov A, #11111B
	lcall WriteData
	mov A, #11111B
	lcall WriteData
	mov A, #11111B
	lcall WriteData
	mov A, #11111B
	lcall WriteData
	
LOOP: 
	jnb BUT_TON, CLEARSCREEN1
	mov a, R7
	add a, #0xC0
	lcall WriteCommand
	mov a, #3
	lcall WriteData
	lcall WaitmilliSec
	djnz R7, LOOP
	mov a, #0xC0
	lcall WriteCommand
	mov a, #3
	lcall WriteData

;---------------------------------;
; START CHROME DINO GAME          ;
;---------------------------------;
	
WAITFORSTART1:                     
	clr LED_GR1 ;turn ON red led
	setb LED_RED
    jnb BUT_TON, CLEARSCREEN1
	lcall WaitmilliSec
	sjmp WAITFORSTART1

CLEARSCREEN1: 
	mov A, #01H
	lcall WriteCommand
	lcall WaitmilliSec

DINOGAMESETUP:
	mov A, #0x40 ; Memory location for first byte of custom character 0
	lcall WriteCommand
	
	; Pixel pattern
	mov A, #00100B
	lcall WriteData
	mov A, #01010B
	lcall WriteData
	mov A, #01001B
	lcall WriteData
	mov A, #11010B
	lcall WriteData
	mov A, #10001B
	lcall WriteData
	mov A, #01110B
	lcall WriteData
	mov A, #01010B
	lcall WriteData
	mov A, #01010B
	lcall WriteData
	
	mov a, #0xC1 ; Move cursor to line 1 column 9
	lcall WriteCommand
	mov a, #0 ; Display custom character 0
	lcall WriteData
	
	mov A, #0x48 ; Memory location for first byte of custom character 0
	lcall WriteCommand
	
	; Pixel pattern
	mov A, #00000B
	lcall WriteData
	mov A, #00000B
	lcall WriteData
	mov A, #01100B
	lcall WriteData
	mov A, #01101B
	lcall WriteData
	mov A, #01101B
	lcall WriteData
	mov A, #01110B
	lcall WriteData
	mov A, #01100B
	lcall WriteData
	mov A, #11111B
	lcall WriteData
	
	mov a, #0xCA ; Move cursor to line 1 column 9
	lcall WriteCommand
	mov a, #1 ; Display custom character 0
	lcall WriteData
	
	mov A, #0x50 ; Memory location for first byte of custom character 0
	lcall WriteCommand
	
	; Pixel pattern
	mov A, #00000B
	lcall WriteData
	mov A, #00110B
	lcall WriteData
	mov A, #00110B
	lcall WriteData
	mov A, #10110B
	lcall WriteData
	mov A, #10110B
	lcall WriteData
	mov A, #01110B
	lcall WriteData
	mov A, #01110B
	lcall WriteData
	mov A, #11111B
	lcall WriteData
	
	mov a, #0xCE ; Move cursor to line 1 column 9
	lcall WriteCommand
	mov a, #2 ; Display custom character 0
	lcall WriteData
	
	mov a, #0x8B
    lcall WriteCommand
    mov a, #'0'
    lcall WriteData
    mov a, #'0'
    lcall WriteData
    mov a, #':'
    lcall WriteData
    mov a, #'0'
    lcall WriteData
    mov a, #'0'
    lcall WriteData
    mov R7, #0
    mov R5, #10
    mov R4, #14

;-----------------------------------;
; MAIN GAME LOOP (ENDS ON COLLISION);
;-----------------------------------;
    
COUNTUP: 
	mov Score, #0
	
	mov a, #0x8F ;row 2, last column 
	lcall WriteCommand
	mov a, R7
	add a, #'0'
	lcall WriteData
	
	clr LED_GR1 ;turn ON red led
	setb LED_RED
    jnb BUT_TON, LIGHT 
    
	lcall WaitHalfSec
	
	mov a, #0xCF 
	lcall WriteCommand
	
	mov a, R5
	add a, #0xC0 ; Move cursor to line 1 column 9
	lcall WriteCommand
	mov a, #' ' ; Display custom character 0
	lcall WriteData
	
	mov a, R4
	add a, #0xC0 ; Move cursor to line 1 column 9
	lcall WriteCommand
	mov a, #' ' ; Display custom character 0
	lcall WriteData
	
	dec R5
	dec R4
	
	mov a, R5
	add a, #0xC0
	lcall WriteCommand
	mov a, #1
	lcall WriteData
	
	mov a, R4
	add a, #0xC0
	lcall WriteCommand
	mov a, #2
	lcall WriteData
	
	inc R7
	mov A, R5
	cjne A, #1, COUNTUP
	
;---------------------------------;
; END GAME SCREEN W/ SCORE        ;
;---------------------------------;

GAMEEND:
	setb LED_GR1
	clr LED_RED
	mov A, #01H
	lcall WriteCommand
	lcall WaitmilliSec
    
    mov DPTR, #MYSTRING4 ;data ptr gets beginning address of MYSTRING1
	
PRINT4:
	mov A, #0 ;0 address
	movc A, @A+DPTR ;Move Code byte relative to DPTR to Acc
	cjne A, #0, SEND4 ;Compare immediate to Acc and Jump if Not Equal
	sjmp DISPLAYSCORE

SEND4:
	lcall WriteData
	inc DPTR
	sjmp PRINT4
	
DISPLAYSCORE:
	mov a, #0xC0
	lcall WriteCommand
	mov DPTR, #MYSTRING5
	
PRINT5: 
	mov A, #0 ;0 address
	movc A, @A+DPTR ;Move Code byte relative to DPTR to Acc
	cjne A, #0, SEND5 ;Compare immediate to Acc and Jump if Not Equal
	sjmp FINALDONE
    
SEND5:
	lcall WriteData
	inc DPTR
	sjmp PRINT5
	
;PAUSE:
; 	clr LED_RED
; 	setb LED_GR1
; 	jnb BUT_TON1, LONG
;LONG: 
;	ljmp COUNTUP
	
;---------------------------------;
; JUMP ANIMATIONS                 ;
;---------------------------------;
	
LIGHT: 
	clr LED_RED
	setb LED_GR1
	
	mov a, #0x81; Move cursor to line 1 column 9
	lcall WriteCommand
	mov a, #0 ; Display custom character 0
	lcall WriteData
	
	mov a, #0xC1 ; Move cursor to line 1 column 9
	lcall WriteCommand
	mov a, #' ' ; Display custom character 0
	lcall WriteData
	
	lcall WaitmilliSec
	
	mov a, #0x81 ; Move cursor to line 1 column 9
	lcall WriteCommand
	mov a, #' ' ; Display custom character 0
	lcall WriteData
	
	mov a, #0xC1 ; Move cursor to line 1 column 9
	lcall WriteCommand
	mov a, #0 ; Display custom character 0
	lcall WriteData

WAIT_RELEASE: 
	jb BUT_TON, LONGJUMP
	sjmp WAIT_RELEASE

LONGJUMP: 
	ljmp COUNTUP
	
POLLBUTTON:
	jnb BUT_TON, INCREMENTSCORE
	lcall WaitmilliSec
	sjmp POLLBUTTON

INCREMENTSCORE:
	mov a, R7
	add a, #1
	da a
	mov R7, a

RELEASELOOP: 
	jnb BUT_TON, RELEASELOOP
	lcall WaitmilliSec
	
FINALDONE:
	mov a, #0xC6
    lcall WriteCommand
    mov a, R7
    swap a
    anl a, #0x0F
    add a, #0x30
    lcall WriteData
    
    mov a, R7
    anl a, #0x0F
    add a, #0x30
    lcall WriteData
    
    sjmp POLLBUTTON

FINALFINALDONE: 
	sjmp FINALFINALDONE

END


