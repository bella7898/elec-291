; Project1.asm:  *description*
;
$NOLIST
$MODMAX10
$LIST

CLK           EQU 33333333 ; Microcontroller system crystal frequency in Hz
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/(12*TIMER2_RATE))))

; Reset vector
org 0x0000
    ljmp main

; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

dseg at 0x30
; Each FSM has its own timer
FSM1_timer:  ds 1
FSM2_timer:  ds 1
; Each FSM has its own state counter
FSM1_state:  ds 1 ; Current state of FSM1
FSM2_state:  ds 1

temp_soak:   ds 1 ; Target temp / soak temp
time_soak:   ds 1 ; Soak duration
temp_reflow: ds 1
time_reflow: ds 1
sec:         ds 1 ; Seconds counter from timer interrupt
temp:        ds 1 ; Sensor interrupt
pwm:         ds 1 ; PWM output
last_state:  ds 1 ; prev state of fsm 1

; Three counters to display.
Count1:      ds 1 ; Incremented/decremented when KEY1 is pressed.
Count2:      ds 1 ; Incremented/decremented when KEY2 is pressed.
Count3:      ds 1 ; Incremented every second. Reset to zero when KEY3 is pressed.

bseg
; For each pushbutton we have a flag.  The corresponding FSM will set this
; flags to one when a valid press of the pushbutton is detected.
Key1_flag:   dbit 1
Key2_flag:   dbit 1
Key3_flag:   dbit 1

cseg
; These 'equ' must match the wiring between the DE10Lite board and the LCD!
; P0 is in connector JPIO.  Check "CV-8052 Soft Processor in the DE10Lite Board: Getting
; Started Guide" for the details.
ELCD_RS       EQU P1.7
; ELCD_RW equ Px.x ; Not used.  Connected to ground 
ELCD_E        EQU P1.1
ELCD_D4       EQU P0.7
ELCD_D5       EQU P0.5
ELCD_D6       EQU P0.3
ELCD_D7       EQU P0.1

OVEN_CTRL     EQU P4.2 ; Control pin connecting to the oven
PB6           EQU P1.5 ; Start button 

$NOLIST
$include(LCD_4bit_DE10Lite_no_RW.inc) ; A library of LCD related functions and utility macros
$LIST

Wait50ms:
;33.33MHz, 1 clk per cycle: 0.03us
	mov R0, #30
Wait50ms_L3:
	mov R1, #74
Wait50ms_L2:
	mov R2, #250
Wait50ms_L1:
	djnz R2, Wait50ms_L1 ;3*250*0.03us=22.5us
    djnz R1, Wait50ms_L2 ;74*22.5us=1.665ms
    djnz R0, Wait50ms_L3 ;1.665ms*30=50ms
    ret
    
;---------------------------------;
; UI/UX - LCD Messages            ;
;---------------------------------;
;                1234567890123456
state_msg:   db 'State', 0
temp_msg1:   db 'TO', 0
temp_msg2:   db 'TJ', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2.  Runs evere ms ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	; Increment the timers for each FSM. That is all we do here!
	inc FSM1_timer 
	inc FSM2_timer 
	reti

; Look-up table for the 7-seg displays. (Segments are turn on with zero) 
T_7seg:
    DB 40H, 79H, 24H, 30H, 19H, 12H, 02H, 78H, 00H, 10H

; Displays a BCD number pased in R0 in HEX1-HEX0
Display_BCD_7_Seg_HEX10:
	mov dptr, #T_7seg

	mov a, R0
	swap a
	anl a, #0FH
	movc a, @a+dptr
	mov HEX1, a
	
	mov a, R0
	anl a, #0FH
	movc a, @a+dptr
	mov HEX0, a
	
	ret

; Displays a BCD number pased in R0 in HEX3-HEX2
Display_BCD_7_Seg_HEX32:
	mov dptr, #T_7seg

	mov a, R0
	swap a
	anl a, #0FH
	movc a, @a+dptr
	mov HEX3, a
	
	mov a, R0
	anl a, #0FH
	movc a, @a+dptr
	mov HEX2, a
	
	ret

; Displays a BCD number pased in R0 in HEX5-HEX4
Display_BCD_7_Seg_HEX54:
	mov dptr, #T_7seg

	mov a, R0
	swap a
	anl a, #0FH
	movc a, @a+dptr
	mov HEX5, a
	
	mov a, R0
	anl a, #0FH
	movc a, @a+dptr
	mov HEX4, a
	
	ret

; The 8-bit hex number passed in the accumulator is converted to
; BCD and stored in [R1, R0]
Hex_to_bcd_8bit:
	mov b, #100
	div ab
	mov R1, a   ; After dividing, a has the 100s
	mov a, b    ; Remainder is in register b
	mov b, #10
	div ab ; The tens are stored in a, the units are stored in b 
	swap a
	anl a, #0xf0
	orl a, b
	mov R0, a
	ret
	
Display_State_LCD: 
	Set_Cursor(1, 6)
	mov a, FSM1_state
	add a, #0x30
	lcall ?WriteData

	mov a, FSM1_state
    lcall Hex_to_bcd_8bit
	lcall Display_BCD_7_Seg_HEX10
	ret

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization of hardware
    mov SP, #0x7F
    lcall Timer2_Init
    ; Turn off all the LEDs
    mov LEDRA, #0 ; LEDRA is bit addressable
    mov LEDRB, #0 ; LEDRB is NOT bit addresable
    setb EA   ; Enable Global interrupts
    
    ; Configure the pins connected to the LCD as outputs
	mov P0MOD, #10101010b ; P0.1, P0.3, P0.5, P0.7 are outputs.  ('1' makes the pin output)
    mov P1MOD, #10000010b ; P1.7 and P1.1 are outputs
    
    lcall ELCD_4BIT ; Configure LCD in four bit mode
	Set_Cursor(1, 1)
    Send_Constant_String(#state_msg)
    
    ; Initialize variables
    mov FSM1_state, #0
    mov FSM2_state, #0
    mov Count1, #0
    mov Count2, #0
    mov Count3, #0
   
    mov HEX0, #0xFF    
    mov HEX1, #0xFF
    
    mov HEX2, #0xFF
    
    mov HEX3, #0xFF
    
    mov HEX4, #0xFF
    
    mov HEX5, #0xFF
	
	; After initialization the program stays in this 'forever' loop
loop:
	lcall FSM1
	mov a, FSM1_state
	cjne a, last_state, Update_lcd
	sjmp Skip_lcd
Update_lcd: 
	mov last_state, FSM1_state
	lcall Display_State_LCD
Skip_lcd:
	lcall Wait50ms
	lcall Wait50ms
	lcall Wait50ms
	lcall Wait50ms
	sjmp loop

;-------------------------------------------------------------------------------
; non-blocking state machine for KEY1 starts here
FSM1:
	mov a, FSM1_state
FSM1_state0:
	cjne a, #0, FSM1_state1
	mov pwm, #0
	jb PB6, FSM1_state0_done
	mov FSM1_state, #1	
FSM1_state0_done:
	ret

FSM1_state1:
	cjne a, #1, FSM1_state2
	mov pwm, #100
	mov sec, #0
	mov a, temp_soak
	clr c
	subb a, temp
	jnc FSM1_state1_done
	mov FSM1_state, #2
FSM1_state1_done:
	ljmp FSM2

FSM1_state2:
	cjne a, #2, FSM1_state3
	mov pwm, #20
	mov a, time_soak
	clr c
	subb a, sec
	jnc FSM1_state2_done
	mov FSM1_state, #3
FSM1_state2_done:
	ljmp FSM2
	
FSM1_state3:
	cjne a, #3, FSM1_state4
	mov pwm, #100
	mov sec, #0
	mov a, temp_soak
	clr c
	subb a, temp
	jnc FSM1_state3_done
	mov FSM1_state, #4
FSM1_state3_done:
	ljmp FSM2

FSM1_state4:
	cjne a, #4, FSM1_state5
	mov pwm, #20
	mov a, time_soak
	clr c
	subb a, sec
	jnc FSM1_state4_done
	mov FSM1_state, #5
FSM1_state4_done:
	ljmp FSM2
	
FSM1_state5:
    cjne a, #5, FSM1_state0_jump
    mov pwm, #0
    mov a, temp
    clr c
    subb a, #60          ; calculate temp - 60
    jc FSM1_state5_to_0  ; if temp < 60, back to State 0
    sjmp FSM1_state5_done
FSM1_state0_jump: 
	ljmp FSM1_state0
FSM1_state5_to_0:
    mov FSM1_state, #0
FSM1_state5_done:
    ljmp FSM2

; sec: timer from interrupt
; temp: data from the sensor
; pwm: percentage of power


FSM2:
    mov a, FSM1_state
  
    
FSM2_state0_entry:
    cjne a, #0, FSM2_state1_entry
    ljmp FSM1

FSM2_state1_entry:
    cjne a, #1, FSM2_state2_entry
    mov temp_soak, #150
    mov time_soak, #0
    ljmp FSM1

FSM2_state2_entry:
    cjne a, #2, FSM2_state3_entry
    mov temp_soak, #0
    mov time_soak, #60
    ljmp FSM1

FSM2_state3_entry:
    cjne a, #3, FSM2_state4_entry
    mov temp_soak, #220
    mov time_soak, #0
    ljmp FSM1
    
FSM2_state4_entry:
    cjne a, #4, FSM2_state5_entry
    mov temp_soak, #0
    mov time_soak, #45
    ljmp FSM1

FSM2_state5_entry:
    cjne a, #5, FSM2_state0_entry
    mov temp_soak, #60
    mov time_soak, #0
    ljmp FSM1
;-------------------------------------------------------------------------------

; If KEY1 was detected, increment or decrement Count1.  Notice that we are displying only
; the least two signicant digits of a counter that can have values from 0 to 255.
	jbc Key1_flag, Increment_Count1
	sjmp Skip_Count1
Increment_Count1:
	jb SWA.0, Decrement_Count1
	inc Count1
	sjmp Display_Count1
Decrement_Count1:
	dec Count1
Display_Count1:	
    mov a, Count1
    lcall Hex_to_bcd_8bit
	lcall Display_BCD_7_Seg_HEX10
Skip_Count1:

; If KEY2 was detected, increment or decrement Count2.  Notice that we are displying only
; the least two signicant digits of a counter that can have values from 0 to 255.
	jbc Key2_flag, Increment_Count2
	sjmp Skip_Count2
Increment_Count2:
	jb SWA.0, Decrement_Count2
	inc Count2
	sjmp Display_Count2
Decrement_Count2:
	dec Count2
Display_Count2:	
    mov a, Count2
    lcall Hex_to_bcd_8bit
	lcall Display_BCD_7_Seg_HEX32
Skip_Count2:


END
