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
milliseconds: ds 2
; Each FSM has its own state counter
FSM1_state:  ds 1 ; Current state of FSM1
FSM2_state:  ds 1

temp_soak:   ds 1 ; Target temp / soak temp
time_soak:   ds 1 ; Soak duration
temp_reflow: ds 1
time_reflow: ds 1
sec:         ds 1 ; Seconds counter from timer interrupt
sec1:        ds 1
temp:        ds 1 ; Sensor interrupt
pwm:         ds 1 ; PWM output
last_state:  ds 1 ; prev state of fsm 1

; PWM generator
ms10:		 ds 1 ; 1ms -> 10ms
pwm_step:	 ds 1 ; 0..99 (10ms steps over 1 second interval)
T_target:	 ds 1 ; temperature parameter


; keypad storage
bcd:		 ds 5 ;

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

ROW1 EQU P1.2
ROW2 EQU P1.4
ROW3 EQU P1.6
ROw4 EQU P2.0
COL1 EQU P2.2
COL2 EQU P2.4
COL3 EQU P2.6
COL4 EQU P3.0

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

Wait25ms:
;33.33MHz, 1 clk per cycle: 0.03us
	mov R0, #15
Wait25ms_L3:
	mov R1, #74
Wait25ms_L2:
	mov R2, #250
Wait25ms_L1:
	djnz R2, Wait25ms_L1 ;3*250*0.03us=22.5us
    djnz R1, Wait25ms_L2 ;74*22.5us=1.665ms
    djnz R0, Wait25ms_L3 ;1.665ms*15=25ms
    ret
    
;---------------------------------;
; UI/UX - LCD Messages            ;
;---------------------------------;
;                1234567890123456
state_msg:   db 'State', 0
temp_msg1:   db 'TO', 0
temp_msg2:   db 'TJ', 0
time_msg1:   db 'Clk', 0

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

	; 1 ms to 10 ms
	inc ms10
	mov a, ms10
	cjne a, #10, ISR_done
	mov ms10, #0

	; 10 ms step counter (0 to 99)
	inc pwm_step
	mov a, pwm_step
	cjne a, #100, do_pwm
	mov pwm_step, #0
	inc sec				; 10ms*100 = 1s

do_pwm: ; if pwm_step < pwm: ON
	mov a, pwm_step
	clr c 
	subb a, pwm ; a = pwm_step - pwm
	jc pwm_on	

pwm_off:
	clr OVEN_CTRL
	sjmp ISR_DONE

pwm_on:
	setb OVEN_CTRL

ISR_done:
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

; Convert 3-digit BCD to hex
BCD3_to_Hex:
	; hundreds
    mov a, bcd+1
    anl a, #0x0F
    mov b, #100
    mul ab
    mov R2, a

    ; tens
    mov a, bcd+0
    swap a
    anl a, #0x0F
    mov b, #10
    mul ab
    add a, R2

    ; ones
    mov b, bcd+0
    anl b, #0x0F
    add a, b   ; final value in A

    ret

; KEYPAD STUFF

MYRLC MAC
	mov a, %0
	rlc a
	mov %0, a
ENDMAC

MYRRC MAC
	mov a, %0
	rrc a
	mov %0, a
ENDMAC

Shift_Digits_Left:
	mov R0, #4
Shift_Digits_Left_L0:
	clr c
	MYRLC(bcd+0)
	MYRLC(bcd+1)
	MYRLC(bcd+2)
	MYRLC(bcd+3)
	MYRLC(bcd+4)
	djnz R0, Shift_Digits_Left_L0
	mov a, R7
	orl a, bcd+0
	mov bcd+0, a
	ret

Shift_Digits_Right:
	mov R0, #4
Shift_Digits_Right_L0:
	clr c
	MYRRC(bcd+4)
	MYRRC(bcd+3)
	MYRRC(bcd+2)
	MYRRC(bcd+1)
	MYRRC(bcd+0)
	djnz R0, Shift_Digits_Right_L0
	ret

CHECK_COLUMN MAC
	jb %0, CHECK_COL_%M
	mov R7, %1
	jnb %0, $
	setb c
	ret
CHECK_COL_%M:
ENDMAC
	
Configure_Keypad_Pins:
	orl P1MOD, #0b_01010100
	orl P2MOD, #0b_00000001
	anl P2MOD, #0b_10101011
	anl P3MOD, #0b_11111110
	ret

Keypad:
	; First check the backspace/correction pushbutton.  We use KEY1 for this function.
	$MESSAGE TIP: KEY1 is the erase key
	jb KEY.1, keypad_L0
	lcall Wait25ms ; debounce
	jb KEY.1, keypad_L0
	jnb KEY.1, $ ; The key was pressed, wait for release
	lcall Shift_Digits_Right
	clr c
	ret
	
keypad_L0:
	; Make all the rows zero.  If any column is zero then a key is pressed.
	clr ROW1
	clr ROW2
	clr ROW3
	clr ROW4
	mov c, COL1
	anl c, COL2
	anl c, COL3
	anl c, COL4
	jnc Keypad_Debounce
	clr c
	ret
		
Keypad_Debounce:
	; A key maybe pressed.  Wait and check again to discard bounces.
	lcall Wait25ms ; debounce
	mov c, COL1
	anl c, COL2
	anl c, COL3
	anl c, COL4
	jnc Keypad_Key_Code
	clr c
	ret
	
Keypad_Key_Code:	
	; A key is pressed.  Find out which one by checking each possible column and row combination.

	setb ROW1
	setb ROW2
	setb ROW3
	setb ROW4
	
	$MESSAGE TIP: SW0 is used to control the layout of the keypad. SW0=0: unmodified keypad. SW0=1: keypad rotated 90 deg CCW
	
	; This check section is for a keypad with the labels rotated 90 deg ccw
keypad_default:
		; Check row 1	
	clr ROW1
	CHECK_COLUMN(COL1, #01H)
	CHECK_COLUMN(COL2, #02H)
	CHECK_COLUMN(COL3, #03H)
	CHECK_COLUMN(COL4, #0AH)
	setb ROW1

	; Check row 2	
	clr ROW2
	CHECK_COLUMN(COL1, #04H)
	CHECK_COLUMN(COL2, #05H)
	CHECK_COLUMN(COL3, #06H)
	CHECK_COLUMN(COL4, #0BH)
	setb ROW2

	; Check row 3	
	clr ROW3
	CHECK_COLUMN(COL1, #07H)
	CHECK_COLUMN(COL2, #08H)
	CHECK_COLUMN(COL3, #09H)
	CHECK_COLUMN(COL4, #0CH)
	setb ROW3

	; Check row 4	
	clr ROW4
	CHECK_COLUMN(COL1, #0EH)
	CHECK_COLUMN(COL2, #00H)
	CHECK_COLUMN(COL3, #0FH)
	CHECK_COLUMN(COL4, #0DH)
	setb ROW4

	clr c
	ret

; display current BCD on 7-seg
Display_BCD_Entry:
	mov dptr, #T_7seg
	
	; Display bcd+0 on HEX1-HEX0
	mov a, bcd+0
	swap a
	anl a, #0FH
	movc a, @a+dptr
	mov HEX1, a
	
	mov a, bcd+0
	anl a, #0FH
	movc a, @a+dptr
	mov HEX0, a
	
	; Display bcd+1 on HEX3-HEX2
	mov a, bcd+1
	swap a
	anl a, #0FH
	movc a, @a+dptr
	mov HEX3, a
	
	mov a, bcd+1
	anl a, #0FH
	movc a, @a+dptr
	mov HEX2, a
	
	; Display bcd+2 on HEX5-HEX4
	mov a, bcd+2
	swap a
	anl a, #0FH
	movc a, @a+dptr
	mov HEX5, a
	
	mov a, bcd+2
	anl a, #0FH
	movc a, @a+dptr
	mov HEX4, a
	ret

Display_Parameters_LCD:
	clr EA
	
	; Line 1: Show TSoak and TReflow temperatures
	Set_Cursor(1, 1)
	mov a, #'T'
	lcall ?WriteData
	mov a, #'S'
	lcall ?WriteData
	mov a, #':'
	lcall ?WriteData
	
	mov a, temp_soak
	lcall Hex_to_bcd_8bit
	; hundreds
	mov a, R1
	add a, #0x30
	lcall ?WriteData
	; tens
	mov a, R0
	swap a
	anl a, #0x0F
	add a, #0x30
	lcall ?WriteData
	; ones
	mov a, R0
	anl a, #0x0F
	add a, #0x30
	lcall ?WriteData
	
	mov a, #' '
	lcall ?WriteData
	
	mov a, #'T'
	lcall ?WriteData
	mov a, #'R'
	lcall ?WriteData
	mov a, #':'
	lcall ?WriteData
	
	mov a, temp_reflow
	lcall Hex_to_bcd_8bit
	; hundreds
	mov a, R1
	add a, #0x30
	lcall ?WriteData
	; tens
	mov a, R0
	swap a
	anl a, #0x0F
	add a, #0x30
	lcall ?WriteData
	; ones
	mov a, R0
	anl a, #0x0F
	add a, #0x30
	lcall ?WriteData
	
	; Line 2: Show tSoak and tReflow times
	Set_Cursor(2, 1)
	mov a, #'t'
	lcall ?WriteData
	mov a, #'S'
	lcall ?WriteData
	mov a, #':'
	lcall ?WriteData
	
	mov a, time_soak
	lcall Hex_to_bcd_8bit
	; hundreds
	mov a, R1
	add a, #0x30
	lcall ?WriteData
	; tens
	mov a, R0
	swap a
	anl a, #0x0F
	add a, #0x30
	lcall ?WriteData
	; ones
	mov a, R0
	anl a, #0x0F
	add a, #0x30
	lcall ?WriteData
	
	mov a, #' '
	lcall ?WriteData
	
	mov a, #'t'
	lcall ?WriteData
	mov a, #'R'
	lcall ?WriteData
	mov a, #':'
	lcall ?WriteData
	
	mov a, time_reflow
	lcall Hex_to_bcd_8bit
	; hundreds
	mov a, R1
	add a, #0x30
	lcall ?WriteData
	; tens
	mov a, R0
	swap a
	anl a, #0x0F
	add a, #0x30
	lcall ?WriteData
	; ones
	mov a, R0
	anl a, #0x0F
	add a, #0x30
	lcall ?WriteData
	
	setb EA
	ret

Display_State_LCD: 
	clr EA
	
    WriteCommand(#0x01)
	
	Set_Cursor(1, 1)
    Send_Constant_String(#state_msg)

	Set_Cursor(1, 6)
	mov a, FSM1_state
	add a, #0x30
	lcall ?WriteData
	
	Set_Cursor(2, 4)
	mov a, sec
	lcall Hex_to_bcd_8bit
	
	; hundreds digit
	mov a, R1
	add a, #0x30
	lcall ?WriteData
	
	; tens digit
	mov a, R0
	swap a
	anl a, #0x0F
	add a, #0x30
	lcall ?WriteData
	
	; ones digit
	mov a, R0
	anl a, #0x0F
	add a, #0x30
	lcall ?WriteData
	
	Set_Cursor(2, 7)
	mov a, sec1
	lcall Hex_to_bcd_8bit
	
	; tens digit
	mov a, R0
	swap a
	anl a, #0x0F
	add a, #0x30
	lcall ?WriteData
	
	; ones digit
	mov a, R0
	anl a, #0x0F
	add a, #0x30
	lcall ?WriteData
	
	setb EA
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
	clr EA
    ; Turn off all the LEDs
    mov LEDRA, #0 ; LEDRA is bit addressable
    mov LEDRB, #0 ; LEDRB is NOT bit addresable


	; PWM 
	orl P4MOD, #00000100b   ; set P4.2 as output
	clr OVEN_CTRL           ; oven initially off
	mov pwm, #0             ; start w/ 0% duty
	mov sec, #0             ; start seconds at 0
	mov ms10, #0
	mov pwm_step, #0
	mov last_state, #0FFh	; force first LCD updatew
    
    ; Configure the pins connected to the LCD as outputs
	mov P0MOD, #10101010b ; P0.1, P0.3, P0.5, P0.7 are outputs.  ('1' makes the pin output)
    mov P1MOD, #10000010b ; P1.7 and P1.1 are outputs

	lcall Configure_Keypad_Pins
    
    lcall ELCD_4BIT ; Configure LCD in four bit mode
	Set_Cursor(1, 1)
    Send_Constant_String(#state_msg)
    Set_Cursor(2, 1)
    Send_Constant_String(#time_msg1)
    Set_Cursor(1, 10) 
    Send_Constant_String(#temp_msg1)
    Set_Cursor(2, 10)
    Send_Constant_String(#temp_msg2)

	setb EA
    
    ; Initialize variables
    mov sec, #0
    mov sec1, #0

	; bcd array intialization for keypad
	mov bcd+0, #0
    mov bcd+1, #0
    mov bcd+2, #0
    mov bcd+3, #0
    mov bcd+4, #0

	; default parameters
	mov temp_soak, #150
	mov time_soak, #60
	mov temp_reflow, #220
	mov time_reflow, #45
    
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
    
	clr OVEN_CTRL
	
	; After initialization the program stays in this 'forever' loop
loop:
	lcall FSM1
	lcall FSM2

	mov a, FSM1_state
	cjne a, #0, Display_State_Normal 
	
	lcall Display_Parameters_LCD ; state 0 shows parameters 
	sjmp loop_continue

Display_State_Normal:
	lcall Display_State_LCD

loop_continue:
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

	; check keypad
	lcall Keypad
	jnc Check_Start_Button ; if no key pressed, check start button

	; if key pressed, check if it's #
	mov a, R7
	cjne a, #0FH, Normal_Key ; if not #, it's normal digit

	; SW0 = temp_sock, SW1 = time_soak, SW2 = temp_reflow, SW3= time_reflow
	jb SWA.0, Save_Temp_Soak
	jb SWA.1, Save_Time_Soak
	jb SWA.2, Save_Temp_Reflow
	jb SWA.3, Save_Time_Reflow
	sjmp FSM1_state0_done

Save_Temp_Soak:
	lcall BCD3_to_Hex
	mov temp_soak, a
	sjmp Clear_BCD
	
Save_Time_Soak:
	lcall BCD3_to_Hex
	mov time_soak, a
	sjmp Clear_BCD
	
Save_Temp_Reflow:
	lcall BCD3_to_Hex
	mov temp_reflow, a
	sjmp Clear_BCD
	
Save_Time_Reflow:
	lcall BCD3_to_Hex
	mov time_reflow, a
	sjmp Clear_BCD
	
Clear_BCD:
	; Clear the BCD display after saving
	mov bcd+0, #0
	mov bcd+1, #0
	mov bcd+2, #0
	mov bcd+3, #0
	mov bcd+4, #0
	sjmp FSM1_state0_done
	
Normal_Key:
	; Normal digit key, shift it in
	lcall Shift_Digits_Left
	lcall Display_BCD_Entry
	sjmp FSM1_state0_done
	
Check_Start_Button:
	; Display current BCD entry
	lcall Display_BCD_Entry
	jb PB6, FSM1_state0_done
	mov sec1, #0
	mov sec, #0
	mov FSM1_state, #1
	
FSM1_state0_done:
	ret

FSM1_state1:
	cjne a, #1, FSM1_state2
	mov pwm, #100
	mov a, temp_soak
	clr c
	subb a, temp
	jnc FSM1_state1_done
	mov sec1, #0
	mov FSM1_state, #2
FSM1_state1_done:
	ret

FSM1_state2:
	cjne a, #2, FSM1_state3
	mov pwm, #20
	mov a, time_soak
	clr c
	subb a, sec
	jnc FSM1_state2_done
	mov sec1, #0
	mov FSM1_state, #3
FSM1_state2_done:
	ret
	
FSM1_state3:
	cjne a, #3, FSM1_state4
	mov pwm, #100
	mov sec, #0
	mov a, temp_reflow
	clr c
	subb a, temp
	jnc FSM1_state3_done
	mov sec1, #0
	mov FSM1_state, #4
FSM1_state3_done:
	ret

FSM1_state4:
	cjne a, #4, FSM1_state5
	mov pwm, #20
	mov a, time_reflow
	clr c
	subb a, sec
	jnc FSM1_state4_done
	mov sec1, #0
	mov FSM1_state, #5
FSM1_state4_done:
	ret
	
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
	mov sec1, #0
    mov FSM1_state, #0
FSM1_state5_done:
    ret

; sec: timer from interrupt
; temp: data from the sensor
; pwm: percentage of power

FSM2:
    mov a, FSM1_state

FSM2_state0_entry:
    cjne a, #0, FSM2_state1_entry
    ret

FSM2_state1_entry:
    cjne a, #1, FSM2_state2_entry
	mov FSM1_state, #1
    ret

FSM2_state2_entry:
    cjne a, #2, FSM2_state3_entry
    ret

FSM2_state3_entry:
    cjne a, #3, FSM2_state4_entry
    ret
    
FSM2_state4_entry:
    cjne a, #4, FSM2_state5_entry
    ret

FSM2_state5_entry:
    cjne a, #5, FSM2_state0_entry
    ret

END
