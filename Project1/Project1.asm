; Project1.asm:  *description*
;
$NOLIST
$MODMAX10
$LIST

CLK           EQU 33333333 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/(12*TIMER0_RATE)))) ; The prescaler in the CV-8052 is always 12 unlike the N76E003 where is selectable.
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/(12*TIMER2_RATE))))

;FREQ   EQU 33333333
;BAUD   EQU 115200
;T2LOAD EQU 65536-(FREQ/(32*BAUD))

FREQ   EQU 33333333
BAUD   EQU 57600
T2LOAD EQU 256-((FREQ*2)/(32*12*BAUD))

; Reset vector
org 0x0000
    ljmp main

org 0x000B
	ljmp Timer0_ISR

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

distance:     ds 2 ; Distance in cm (16-bit)
too_close:    ds 1 ; Flag: 1 if object too close
proximity_beep_timer: ds 1 ; Timer for proximity beep interval

temp_soak:   ds 1 ; Target temp / soak temp
time_soak:   ds 1 ; Soak duration
temp_reflow: ds 1
time_reflow: ds 1
sec:         ds 1 ; Seconds counter from timer interrupt
sec1:        ds 1
ovenTemp:    ds 1 ; Sensor interrupt
pwm:         ds 1 ; PWM output
last_state:  ds 1 ; prev state of fsm 1

;Temperature variables
x:   ds 4
y:   ds 4
amb_tmp: ds 4
therm_tmp: ds 4
;bcd: ds 5

VAL_LM4040: ds 2
VAL_LM335:  ds 2
VAL_THERM:  ds 2


; PWM generator
ms10:		 ds 1 ; 1ms -> 10ms
pwm_step:	 ds 1 ; 0..99 (10ms steps over 1 second interval)
T_target:	 ds 1 ; temperature parameter


; keypad storage
bcd:	ds 5 ;

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
sec_flag: 	 dbit 1
half_sec_flag: dbit 1
buzzer_active: dbit 1
proximity_warning: dbit 1

;temp variable
mf:		dbit 1

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
BUZZER        EQU P2.1

US_TRIGGER    EQU P3.2 ; Trigger pin for ultrasonic sensor
US_ECHO       EQU P3.3 ; Echo pin for ultrasonic sensor
FAN		      EQU P

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

myLUT:
    DB 0xC0, 0xF9, 0xA4, 0xB0, 0x99        ; 0 TO 4
    DB 0x92, 0x82, 0xF8, 0x80, 0x90        ; 4 TO 9
    DB 0x88, 0x83, 0xC6, 0xA1, 0x86, 0x8E  ; A to F

$include(math32.asm)

;InitSerialPort:
	; Configure serial port and baud rate
;	clr TR2 ; Disable timer 2
;	mov T2CON, #30H ; RCLK=1, TCLK=1 
;	mov RCAP2H, #high(T2LOAD)  
;	mov RCAP2L, #low(T2LOAD)
;	setb TR2 ; Enable timer 2
;	mov SCON, #52H
;	ret

InitSerialPort:
	clr TR1 ; Disable timer 1
	mov TMOD, #020H ; Set timer 1 as 8-bit auto reload
	mov TH1, #T2LOAD
	mov TL1, #0
	mov a, PCON ; Set SMOD to 1
	orl a, #80H
	mov PCON, a
	setb TR1 ; Enable timer 1
	mov SCON, #52H
	ret

putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

SendString:
    CLR A
    MOVC A, @A+DPTR
    JZ SSDone
    LCALL putchar
    INC DPTR
    SJMP SendString
SSDone:
    ret

Display_STOP_HEX:
    ; Display "STOP" on HEX displays
    ; S = 0x92, t = 0x87, O = 0xC0, P = 0x8C
    mov HEX5, #0x92  ; S
    mov HEX4, #0x87  ; t
    mov HEX3, #0xC0  ; O
    mov HEX2, #0x8C  ; P
    mov HEX1, #0xFF  ; blank
    mov HEX0, #0xFF  ; blank
    ret

;Temperature reading functions
Display_Temp_Serial:
	mov a, bcd+3
	swap a
	anl a, #0FH
	orl a, #'0'
	lcall putchar
	
	mov a, bcd+3
	anl a, #0FH
	orl a, #'0'
	lcall putchar

	mov a, bcd+2
	swap a
	anl a, #0FH
	orl a, #'0'
	lcall putchar
	
	mov a, bcd+2
	anl a, #0FH
	orl a, #'0'
	lcall putchar
	
	mov a, #'.'
	lcall putchar
	
	mov a, bcd+1
	swap a
	anl a, #0FH
	orl a, #'0'
	lcall putchar
	
	mov a, bcd+1
	anl a, #0FH
	orl a, #'0'
	lcall putchar
	
	mov a, bcd+0
	swap a
	anl a, #0FH
	orl a, #'0'
	lcall putchar
	
	mov a, bcd+0
	anl a, #0FH
	orl a, #'0'
	lcall putchar

	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar
	
	ret

	
S_BCD mac
	push ar0
	mov r0, %0
	lcall ?S_BCD
	pop ar0
endmac

?S_BCD:
	push acc
	; Write most significant digit
	mov a, r0
	swap a
	anl a, #0fh
	orl a, #30h
	lcall putchar
	; write least significant digit
	mov a, r0
	anl a, #0fh
	orl a, #30h
	lcall putchar
	pop acc
	ret
	
Read_ADC:
	mov a, ADC_H
	mov R1, a
	mov a, ADC_L
	mov R0, a
	ret
	
Display_temp_7seg:
	
	mov dptr, #myLUT
	mov HEX5, #0xFF
	
	mov a, bcd+3
	anl a, #0FH
	movc a, @a+dptr
	mov HEX4, a
	
	mov a, bcd+2
	swap a
	anl a, #0FH
	movc a, @a+dptr
	mov HEX3, a
	
	mov a, bcd+2
	anl a, #0FH
	movc a, @a+dptr
	anl a, #0x7f ; Turn on decimal point
	mov HEX2, a

	mov a, bcd+1
	swap a
	anl a, #0FH
	movc a, @a+dptr
	mov HEX1, a
	
	mov a, bcd+1
	anl a, #0FH
	movc a, @a+dptr
	mov HEX0, a
	
	ret
	
TEMP_BCD2HEX:
	; hundreds
    mov a, bcd+3
    anl a, #0x0F
    mov b, #100
    mul ab
    mov R2, a

    ; tens
    mov a, bcd+2
    swap a
    anl a, #0x0F
    mov b, #10
    mul ab
    add a, R2

    ; ones
    mov b, bcd+2
    anl b, #0x0F
    add a, b   ; final value in A
    mov ovenTemp, a

    ret
	
checkTemp:	
	mov ADC_C, #0x03
	lcall wait50ms
	
	mov R7, #16
	mov VAL_LM4040+0, #0
	mov VAL_LM4040+1, #0
	
	loop_ref:
		lcall Read_ADC
		mov a, R0
		add a, VAL_LM4040+0
		mov VAL_LM4040+0, a
		mov a, R1
		addc a, VAL_LM4040+1
		mov VAL_LM4040+1, a
		djnz R7, loop_ref
		
	mov ADC_C, #0x05
	lcall wait50ms
	
	mov R7, #16
	mov VAL_THERM+0, #0
	mov VAL_THERM+1, #0
	
	loop_therm:
	lcall Read_ADC
	; Save result for later use
	mov a, R0
	add a, VAL_THERM+0
	mov VAL_THERM+0, a
	mov a, R1
	addc a, VAL_THERM+1
	mov VAL_THERM+1, a
	djnz R7, loop_therm
	
	; CHN2 - LM335 Temperature Sensor
	mov ADC_C, #0x04
	lcall wait50ms
	
	mov R7, #16
	mov VAL_LM335+0, #0
	mov VAL_LM335+1, #0
	
	loop_amb:
	lcall Read_ADC
	; Save result for later use
	mov a, R0
	add a, VAL_LM335+0
	mov VAL_LM335+0, a
	mov a, R1
	addc a, VAL_LM335+1
	mov VAL_LM335+1, a
	djnz R7, loop_amb
	
	; Perform voltage and temperature conversions to send to LCD and Serial
	mov x+0, VAL_LM335+0
	mov x+1, VAL_LM335+1
	mov x+2, #0
	mov x+3, #0
	Load_y(41100) ; The MEASURED voltage reference: 4.0959V, with 4 decimal places
	lcall mul32
	; Retrive the ADC LM4040 value
	mov y+0, VAL_LM4040+0
	mov y+1, VAL_LM4040+1
	; Pad other bits with zero
	mov y+2, #0
	mov y+3, #0
	lcall div32

	; Convert to temperature (LM335 Voltage)
	Load_y(27300)
	lcall sub32
	Load_y(100)
	lcall mul32
	
	mov amb_tmp+3, x+3
	mov amb_tmp+2, x+2
	mov amb_tmp+1, x+1
	mov amb_tmp+0, x+0
	
	lcall hex2bcd
	lcall Display_temp_7seg
	
	mov x+0, VAL_THERM+0
	mov x+1, VAL_THERM+1
	 ;Pad other bits with zero
	mov x+2, #0
	mov x+3, #0
	Load_y(41100) ; The MEASURED voltage reference: 4.0959V, with 4 decimal places
	lcall mul32
	; Retrive the ADC LM4040 value
	mov y+0, VAL_LM4040+0
	mov y+1, VAL_LM4040+1
	; Pad other bits with zero
	mov y+2, #0
	mov y+3, #0
	lcall div32
	Load_y(74)
	lcall mul32
	
	mov therm_tmp+3, x+3
	mov therm_tmp+2, x+2
	mov therm_tmp+1, x+1
	mov therm_tmp+0, x+0
	
	mov x+0, amb_tmp+0
	mov x+1, amb_tmp+1
	mov x+2, amb_tmp+2
	mov x+3, amb_tmp+3

	mov y+0, therm_tmp+0
	mov y+1, therm_tmp+1
	mov y+2, therm_tmp+2
	mov y+3, therm_tmp+3
	
		
	lcall add32
	lcall hex2bcd
	lcall Display_temp_7seg
	lcall TEMP_BCD2HEX
	lcall Display_Temp_Serial
	
	ret
	
checkFirst50:
	mov a, sec
	cjne a, #60, check50skip
	sjmp check50Temp
	
check50skip:
	ret
	
check50Temp:
	lcall checkTemp
	mov a, #50
	clr c
	subb a, ovenTemp
	jnc stop
	ret
	
stop:
	lcall Beep_Error
	lcall Display_STOP_HEX
	mov FSM1_state, #0
	ret

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

Beep_Once:
	setb buzzer_active
	clr half_sec_flag
	setb TR0
wait_beep: 
	jnb half_sec_flag, wait_beep
	clr TR0
	clr buzzer_active
	ret

Beep_Loop:
	setb buzzer_active
	lcall Beep_Once
	clr half_sec_flag
Beep_wait:
	jnb half_sec_flag, Beep_wait
	djnz R3, Beep_Loop
Beep_Done:
	clr buzzer_active
	ret

Beep_State_Transition:
	mov R3, #1
	lcall Beep_Loop
	ret

Beep_Success:
	mov R3, #5
	lcall Beep_Loop
	ret

Beep_Error:
	mov R3, #10
	lcall Beep_Loop
	ret

;---------------------------------;
; Ultrasonic Sensor Functions     ;
;---------------------------------;

; Wait 10us (approximately)
Wait_10us:
    push R0
    mov R0, #11  ; Adjust for 33MHz clock
Wait_10us_loop:
    djnz R0, Wait_10us_loop
    pop R0
    ret

; Send 10us trigger pulse
US_Send_Trigger:
    clr US_TRIGGER
    lcall Wait_10us
    setb US_TRIGGER
    lcall Wait_10us
    clr US_TRIGGER
    ret

; Measure distance using ultrasonic sensor
; Returns distance in cm in [distance+1, distance+0]
US_Measure_Distance:
    push acc
    push b
    push psw
    
	; check if timer 0 running or buzzer buzzer_active
	jb buzzer_active, US_Skip_Measurement
	jb TR0, US_Skip_Measurement 

	sjmp US_Safe_To_Measure

US_Skip_Measurement: ; timer 0 or buzzer active so skip 
	mov distance+0, #0xFF
	mov distance+1, #0xFF
	sjmp US_Done

US_Safe_To_Measure:
	lcall US_Send_Trigger
	; wait for echo to go high
	mov R0, #0

US_Wait_Echo_High:
    jb US_ECHO, US_Echo_High   ; if high, jump ahead
    djnz R0, US_Wait_Echo_High ; otherwise, keep waiting
    sjmp US_Timeout            ; if wait too long, timeout
    
US_Echo_High:
    ; Configure Timer 0 for distance measurement (16-bit mode)
    push TMOD	; save current timer mode
    mov a, TMOD
    anl a, #0xF0  ; clear Timer 0 bits
    orl a, #0x01  ; set Timer 0 to mode 1 (16-bit timer)
    mov TMOD, a
    
    ; Initialize and start Timer 0
    clr TF0
    mov TH0, #0
    mov TL0, #0
    setb TR0
    
    ; Wait for echo to go low (with timeout)
    mov R0, #0
US_Wait_Echo_Low:
    jnb US_ECHO, US_Echo_Low
    mov a, TH0
    cjne a, #0xFF, US_No_Timeout
US_No_Timeout:
    jnc US_Timeout_Restore
    djnz R0, US_Wait_Echo_Low
    sjmp US_Timeout_Restore
    
US_Echo_Low:
    ; Stop timer
    clr TR0
    
    ; Calculate distance
    ; Distance (cm) = (Time * Speed of Sound) / 2
    ; With 33MHz clock and /12 prescaler: each tick = 0.36us
    ; Distance ≈ Timer_Value / 162
    ; We'll use approximation: distance = TH0 (high byte gives us rough cm)
    
    mov a, TL0
    mov R0, a
    mov a, TH0
    mov R1, a
    
    ; Simple approximation: distance ≈ TH0
    mov a, R1
    mov distance+1, #0
    mov distance+0, a
    
    ; check if too close (less than 8 cm)
    mov a, distance+0
    clr c
    subb a, #8		; can test w different distances
    jnc US_Not_Too_Close
    
    ; Too close!
    setb proximity_warning
    sjmp US_Restore_Timer
    
US_Not_Too_Close:
    clr proximity_warning
    sjmp US_Restore_Timer

US_Timeout_Restore:
    clr TR0		; stop timer
US_Timeout:
    ; no object detected or too far
    mov distance+0, #0xFF
    mov distance+1, #0xFF
    clr proximity_warning
    
US_Restore_Timer:
    ; Restore Timer 0 to buzzer mode
    pop TMOD		; restore original timer mode
    mov TH0, #high(TIMER0_RELOAD)
    mov TL0, #low(TIMER0_RELOAD)
    
US_Done:
    pop psw
    pop b
    pop acc
    ret

; Check proximity every 2s (to not interfere w buzzer)
; and beep periodically if needed
Check_Proximity:
    ; Only check if oven is running (not in state 0)
    mov a, FSM1_state
    cjne a, #0, Check_Prox_Continue
    clr proximity_warning
    ret
    
Check_Prox_Continue:
	mov a, sec
	anl a, #01H			; check odd/even
	jnz Proximity_Done	; skip if odd

    lcall US_Measure_Distance ; measure distance
    
    ; If proximity warning is set, beep periodically
    jnb proximity_warning, No_Proximity_Warning
    
    ; Quick double beep for warning (using R3)
    push acc
    mov R3, #2
    lcall Beep_Loop
    pop acc
    sjmp Proximity_Done
    
No_Proximity_Warning:
    sjmp Proximity_Done
    
Proximity_Done:
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
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	push acc
    push psw
    push b
	;clr TF0  ; According to the data sheet this is done for us already.
	mov TH0, #high(TIMER0_RELOAD) ; Timer 0 doesn't have autoreload in the CV-8052
	mov TL0, #low(TIMER0_RELOAD)
	cpl BUZZER
	pop b
    pop psw
    pop acc
	reti

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
    push acc
    push psw
    push b
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
    
    cjne a, #50, check_full_sec
    cpl half_sec_flag
check_full_sec: 
	cjne a, #100, do_pwm
    mov pwm_step, #0
    inc sec                ; 10ms*100 = 1s
    inc sec1
    cpl half_sec_flag

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
    pop b
    pop psw
    pop acc
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
	
	Set_Cursor(1, 1)
    Send_Constant_String(#state_msg)
    Set_Cursor(2, 1)
    Send_Constant_String(#time_msg1)
    Set_Cursor(1, 10) 
    Send_Constant_String(#temp_msg1)
    Set_Cursor(2, 10)
    Send_Constant_String(#temp_msg2)

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
    lcall Timer0_Init
    lcall Timer2_Init
	clr EA
    ; Turn off all the LEDs
    mov LEDRA, #0 ; LEDRA is bit addressable
    mov LEDRB, #0 ; LEDRB is NOT bit addresable
    
    LCALL InitSerialPort
    mov ADC_C, #0x80 ; Reset ADC
	lcall Wait50ms
	
	clr TR0

	; PWM 
	orl P4MOD, #00000100b   ; set P4.2 as output
	clr OVEN_CTRL           ; oven initially off
	clr BUZZER

	; ultrasonic sensor 
 	orl P3MOD, #00000100b   ; set trigger as output
    anl P3MOD, #11110111b   ; set echo as input
    clr US_TRIGGER
    mov proximity_beep_timer, #0
    clr proximity_warning

	mov pwm, #0             ; start w/ 0% duty
	mov sec, #0             ; start seconds at 0
	mov ms10, #0
	mov pwm_step, #0
	mov last_state, #0FFh	; force first LCD updatew
    
    ; Configure the pins connected to the LCD as outputs
	mov P0MOD, #10101010b ; P0.1, P0.3, P0.5, P0.7 are outputs.  ('1' makes the pin output)
    mov P1MOD, #10000010b ; P1.7 and P1.1 are outputs
    mov P2MOD, #00000010b

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
	mov temp_soak, #70
	mov time_soak, #15
	mov temp_reflow, #90
	mov time_reflow, #20
    
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
	mov ovenTemp, #0
	mov LEDRA, sec
	
	; After initialization the program stays in this 'forever' loop
loop:
	lcall FSM1

	lcall Check_Proximity

	mov a, FSM1_state
	cjne a, #0, Display_State_Normal 

	jnb proximity_warning, loop_no_prox
    clr TR0  ; Make sure buzzer is off in state 0

loop_no_prox:
    mov last_state, FSM1_state
    lcall Display_Parameters_LCD ; state 0 shows parameters 
    sjmp loop_continue

Display_State_Normal:
	mov a, FSM1_state
	cjne  a, last_state, State_Changed
	sjmp Skip_Clear

State_Changed: 
	lcall Beep_Once
	mov a, last_state
	cjne a, #0, Skip_Clear 
	mov a, FSM1_state
	cjne a, #1, Skip_Clear
	
	clr EA
	WriteCommand(#0x01)
	lcall Wait50ms
	setb EA

Skip_Clear: 
	mov last_state, FSM1_state
	lcall Display_State_LCD
	sjmp loop_continue

loop_continue:
	lcall Wait50ms
	;lcall Wait50ms
	;lcall Wait50ms
	;lcall Wait50ms
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
	mov LEDRA, sec
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
	mov a, FSM1_state
	cjne a, #1, FSM1_state2

	; check stop button
	jnb PB6, lj_FSM1_stop_to_state0

	mov LEDRA, sec
	mov pwm, #100
	lcall checkFirst50
	
	jnb half_sec_flag, FSM1_state1_continue
	lcall checkTemp
	
FSM1_state1_continue:
	mov a, temp_soak
	clr c
	subb a, ovenTemp
	jnc FSM1_state1_done
	mov sec1, #0
	mov FSM1_state, #2

FSM1_state1_done:
	ret
	
lj_FSM1_stop_to_state0: 
	ljmp FSM1_stop_to_state0

FSM1_state2:
	cjne a, #2, FSM1_state3

	; check stop button
	jnb PB6, lj_FSM1_stop_to_state0
	
	mov pwm, #20
	jnb half_sec_flag, FSM1_state2_continue
	lcall checkTemp
	
FSM1_state2_continue:
	mov a, time_soak
	clr c
	subb a, sec1
	jnc FSM1_state2_done
	mov sec1, #0
	mov FSM1_state, #3
FSM1_state2_done:
	ret
	
FSM1_state3:
	cjne a, #3, FSM1_state4

	; check stop button
	jnb PB6, FSM1_stop_to_state0

	mov pwm, #100
	jnb half_sec_flag, FSM1_state3_continue
	lcall checkTemp
	
FSM1_state3_continue:
	mov a, temp_reflow
	clr c
	subb a, ovenTemp
	jnc FSM1_state3_done
	mov sec1, #0
	mov FSM1_state, #4
FSM1_state3_done:
	ret

FSM1_state4:
	cjne a, #4, FSM1_state5

	; check stop button
	jnb PB6, FSM1_stop_to_state0
	
	mov pwm, #20
	jnb half_sec_flag, FSM1_state4_continue
	lcall checkTemp
	
FSM1_state4_continue:
	mov a, time_reflow
	clr c
	subb a, sec1
	jnc FSM1_state4_done
	mov sec1, #0
	mov FSM1_state, #5
	lcall Beep_Once

FSM1_state4_done:
	ret
	
FSM1_state5:
    cjne a, #5, FSM1_state0_jump
    mov pwm, #0
    jnb half_sec_flag, FSM1_state5_continue
	lcall checkTemp
	
FSM1_state5_continue:
    mov a, ovenTemp
    clr c
    subb a, #60          ; calculate temp - 60
    jc FSM1_state5_to_0  ; if temp < 60, back to State 0

	mov bcd+0, #0
	mov bcd+1, #0
	mov bcd+2, #0
	mov bcd+3, #0
	mov bcd+4, #0

	clr EA
	WriteCommand(#0x01)
	lcall Wait50ms
	setb EA

    sjmp FSM1_state5_done
	
FSM1_state0_jump: 
	ljmp FSM1_state0
FSM1_state5_to_0:
	mov sec1, #0
    mov FSM1_state, #0
    lcall Beep_Success
FSM1_state5_done:
    ret
	
FSM1_stop_to_state0: 
	mov pwm, #0

	lcall Display_STOP_HEX

	lcall Wait50ms
	lcall Wait50ms
	lcall Wait50ms

	mov sec1, #0
	mov sec, #0
	mov FSM1_state, #0

	; Clear BCD
    mov bcd+0, #0
    mov bcd+1, #0
    mov bcd+2, #0
    mov bcd+3, #0
    mov bcd+4, #0
    
    ; Clear HEX displays\
    mov HEX0, #0xFF
    mov HEX1, #0xFF
    mov HEX2, #0xFF
    mov HEX3, #0xFF
    mov HEX4, #0xFF
    mov HEX5, #0xFF

	ret
