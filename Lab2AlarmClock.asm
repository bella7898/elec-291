; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P1.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'CLEAR' push button connected to P1.5 is pressed.
$NOLIST
$MODN76E003
$LIST

;  N76E003 pinout:
;                               -------
;       PWM2/IC6/T0/AIN4/P0.5 -|1    20|- P0.4/AIN5/STADC/PWM3/IC3
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

CLK           EQU 16600000 ; Microcontroller system frequency in Hz
TIMER0_RATE   EQU 4096    ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

CLEAR_BUTTON  equ P1.5 ; add 1 to selected column
UPDOWN        equ P1.6
SOUND_OUT     equ P1.7
SAVE_BUTTON equ P1.2; pin 15, button 1 --- save alarm 
HR_INC_BUTTON       equ P0.4 ; pin 5, button 2  --- select row 
MIN_INC_BUTTON      equ P1.1 ; pin 14, button 3 ---- toggle column
TIMER_BUTTON      equ P3.0 ; pin 8, button 4 ---- add 5 min 

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
Hours_counter: ds 1 
Minutes_counter: ds 1
Alarm_hour: ds 1
Alarm_minutes: ds 1
Edit_state_mode: ds 1 ; 0 = no edit, 1 = edit timer, 2 = edit alarm
Shift_position: ds 1  ; 0 = edit hour, 1 = edit min, 2 = edit sec, 3 = edit am/pm (diff for row 2)
Alarm_h_store: ds 1
Alarm_m_store: ds 1
SW_minutes: ds 1 
SW_hrs: ds 1


; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
half_seconds_count: dbit 1 
one_second_flag: dbit 1 ; Set to one every two half seconds 
am_pm_flag: dbit 1 ; 0 = AM, 1 = PM
am_pm_alarm: dbit 1 ; 0 = AM, 1 = PM
Alarm_am_store: dbit 1
SW_running: dbit 1

cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P1.3
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P1.4
LCD_D4 equ P0.0
LCD_D5 equ P0.1
LCD_D6 equ P0.2
LCD_D7 equ P0.3

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'Clock: xx ', 0
Initial_Message1: db 'Alarm: xx ', 0
Alarm_Saved: db 'Alarm Saved!', 0
Alarm_Displayed: db 'Alarm Displayed!', 0
Stopwatch: db 'Timer: xx ', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	orl CKCON, #0b00001000 ; Input for timer 0 is sysclk/1
	mov a, TMOD
	anl a, #0xf0 ; 11110000 Clear the bits for timer 0
	orl a, #0x01 ; 00000001 Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    clr TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz wave at pin SOUND_OUT   ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	; Timer 0 doesn't have 16-bit auto-reload, so
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	setb TR0
	
	cpl SOUND_OUT ; Connect speaker the pin assigned to 'SOUND_OUT'!
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
	orl T2MOD, #0x80 ; Enable timer 2 autoreload
	mov RCMP2H, #high(TIMER2_RELOAD)
	mov RCMP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
	orl EIE, #0x80 ; Enable timer 2 interrupt ET2=1
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in the ISR.  It is bit addressable.
	;cpl P0.4 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb one_second_flag ; Let the main program know half second had passed
	clr TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, BCD_counter
	jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01
	sjmp Timer2_ISR_da
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a
	
	cjne a, #0x60, Timer2_ISR_done
	mov BCD_counter, #0
	
	mov a, Edit_state_mode
	cjne a, #3, increment_clock_mode
	
increment_stopwatch_mode: 
	mov a, SW_minutes
	add a, #0x01
	da a
	mov SW_minutes, a
	
	cjne a, #0x60, Timer2_ISR_done
	mov SW_minutes, #0
	
	mov a, SW_hrs
	add a, #0x01
	da a
	mov SW_hrs, a
	sjmp Timer2_ISR_done

increment_clock_mode: 
	mov a, Minutes_counter
	add a, #0x01
	da a
	mov Minutes_counter, a
	
	cjne a, #0x60, Timer2_ISR_done
	mov Minutes_counter, #0
	
	mov a, Hours_counter
	add a, #0x01
	da a
	mov Hours_counter, a
	
	cjne a, #0x12, Check_hr_13
	cpl am_pm_flag
	sjmp Timer2_ISR_done
	
Check_hr_13:
	cjne a, #0x13, Timer2_ISR_done
	mov Hours_counter, #0x01
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    mov P0M1, #0x00
    mov P0M2, #0x00
    mov P1M1, #0x00
    mov P1M2, #0x00
    mov P3M2, #0x00
    mov P3M2, #0x00
    
    mov Hours_counter, #0x11
    mov Minutes_counter, #0x59
    clr am_pm_flag
    clr one_second_flag
    mov half_seconds_count, #0
    mov Alarm_hour, #0x00
	mov Alarm_minutes, #0x00
	clr am_pm_alarm
	mov Edit_state_mode, #0
	mov Shift_position, #0
	
	mov Alarm_h_store, #0x00
	mov Alarm_m_store, #0x00
	clr Alarm_am_store
	
	mov SW_hrs, #0x00
	mov SW_minutes, #0x00
	mov SW_running, #0x00
   
    lcall Timer0_Init
    lcall Timer2_Init
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    setb half_seconds_flag
	mov BCD_counter, #0x00
	Set_Cursor(2, 1)
    Send_Constant_String(#Initial_Message1)
    
    Set_Cursor(1, 8)
	Display_BCD(Hours_counter)
 	WriteData(#':')
	Display_BCD(Minutes_counter)
	WriteData(#':')

	Display_BCD(BCD_counter)
	Set_Cursor(1, 16)
	WriteData(#'A')
	
	Set_Cursor(2, 8)
	Display_BCD(Alarm_hour)
	WriteData(#':')
	
	Display_BCD(Alarm_minutes)
	Set_Cursor(2, 13)
	WriteData(#'A')
	
	; After initialization the program stays in this 'forever' loop
loop:
	jb MIN_INC_BUTTON, save_script
	Wait_Milli_Seconds(#50)
	jb MIN_INC_BUTTON, save_script
	
	mov a, Edit_state_mode
	
	cjne a, #0, skip_message1
	
	WriteCommand(#0x01)
	Wait_Milli_Seconds(#2)
	Set_Cursor(1, 1)
    Send_Constant_String(#Alarm_Displayed)

	mov a, Alarm_h_store
	mov Alarm_hour, a
	mov a, Alarm_m_store
	mov Alarm_minutes, a
	mov a, Alarm_am_store
	mov am_pm_alarm, a
	
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
	WriteCommand(#0x01)

skip_message1:
wait_release1:
	jnb MIN_INC_BUTTON, $
	Wait_Milli_Seconds(#50)
	ljmp loop_b

save_script: 
	jb CLEAR_BUTTON, add_5
	Wait_Milli_Seconds(#50)
	jb CLEAR_BUTTON, add_5
	
	mov a, Edit_state_mode
	
	cjne a, #0, skip_message
	
	WriteCommand(#0x01)
	Wait_Milli_Seconds(#2)
	Set_Cursor(1, 1)
    Send_Constant_String(#Alarm_Saved)
	
	mov a, Alarm_hour
	mov Alarm_h_store, a
	mov a, Alarm_minutes
	mov Alarm_m_store, a
	mov a, am_pm_alarm
	mov Alarm_am_store, a
	
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
	WriteCommand(#0x01)
skip_message:
wait_release:
	jnb CLEAR_BUTTON, $
	Wait_Milli_Seconds(#50)
	ljmp loop_b

add_5:
	jb TIMER_BUTTON, check_row
	Wait_Milli_Seconds(#100)
	jb TIMER_BUTTON, check_row
	
	mov a, Alarm_minutes
	add a, #0x05
	da a
	
	cjne a, #0x60, no_rollover
	mov a, #0x00
	mov Alarm_minutes, a
	
	mov a, Alarm_hour
	add a, #0x01
	da a 
	cjne a, #0x12, no_rollover_hour
	mov a, #0x00
	mov Alarm_hour, a
	sjmp end_snooze
no_rollover: 
	mov Alarm_minutes, a
	sjmp end_snooze
no_rollover_hour:
	mov Alarm_hour, a
end_snooze:
	jnb TIMER_BUTTON, $
	Wait_Milli_Seconds(#50)
	ljmp loop_b	
	
check_row:
	jb HR_INC_BUTTON, maybe_check_column  ; if the 'CLEAR' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb HR_INC_BUTTON, maybe_check_column  ; if the 'CLEAR' button is not pressed skip
	
	; A valid press of the 'CLEAR' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	inc Edit_state_mode
	mov a, Edit_state_mode
	cjne a, #4, edit_okay
	mov Edit_state_mode, #0
	
edit_okay:
	jnb HR_INC_BUTTON, $
	Wait_Milli_Seconds(#50)
	ljmp loop_b           ; Display the new value
maybe_check_column:
	mov a, Edit_state_mode
	jz  loop_a_long
	cjne a, #3, check_column
	
	jb CLEAR_BUTTON, sw_reset
	Wait_Milli_Seconds(#50)
	jb CLEAR_BUTTON, sw_reset
	
	cpl SW_running
	
	jnb CLEAR_BUTTON, $
	Wait_Milli_Seconds(#50)
	ljmp loop_b
	
sw_reset: 
	jb MIN_INC_BUTTON, check_column
	Wait_Milli_Seconds(#50)
	jb MIN_INC_BUTTON, check_column
	
	mov SW_hrs, #0x00
	mov SW_minutes, #0x00
	mov BCD_counter, #0x00
	
	jnb MIN_INC_BUTTON, $
	Wait_Milli_Seconds(#50)
	ljmp loop_b
	
loop_a_long: 
	ljmp loop_a
check_column: 
	jb MIN_INC_BUTTON, check_addition ; if the 'CLEAR' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb MIN_INC_BUTTON, check_addition ; if the 'CLEAR' button is not pressed skip
	jnb MIN_INC_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'CLEAR' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	inc Shift_position
	mov a, Shift_position
	cjne a, #4, edit1_okay
	mov Shift_position, #0
edit1_okay: 
	ljmp loop_b           ; Display the new value
check_addition: 
	jb CLEAR_BUTTON, loop_a
	Wait_Milli_Seconds(#50)
	jb CLEAR_BUTTON, loop_a
	jnb CLEAR_BUTTON, $
	
	mov a, Edit_state_mode
	cjne a, #1, edit_row_2
	
	mov a, Shift_position
	jz increment_hour
	cjne a, #1, increment_sec
	mov a, Minutes_counter
	add a, #0x01
	da a
	cjne a, #0x60, min_ok
	mov a, #0x00
min_ok: 
	mov Minutes_counter, a 
	sjmp done_inc
increment_hour: 
	mov a, Hours_counter
	add a, #0x01
	da a
	cjne a, #0x13, inc_hour
	mov a, #0x00
inc_hour:
	mov Hours_counter, a
	sjmp done_inc
increment_sec:
	mov a, Shift_position 
	cjne a, #3, toggle_ampm
	mov a, BCD_counter
	add a, #0x01
	da a
	cjne a, #0x60, sec_ok
	mov a, #0x00
sec_ok:
	mov BCD_counter, a
	sjmp done_inc
toggle_ampm:
	cpl am_pm_flag
	sjmp done_inc
edit_row_2:
	mov a, Shift_position
	jz increment_hour_alarm
	cjne a, #1, toggle_ampm_alarm
	mov a, Alarm_minutes
	add a, #0x01
	da a
	cjne a, #0x60, min_alarm_ok
	mov a, #0x00
min_alarm_ok: 
	mov Alarm_minutes, a 
	sjmp done_inc
increment_hour_alarm: 
	mov a, Alarm_hour
	add a, #0x01
	da a
	cjne a, #0x13, hour_alarm_ok
	mov a, #0x00
hour_alarm_ok: 
	mov Alarm_hour, a
	sjmp done_inc
toggle_ampm_alarm: 
	cjne a, #2, done_inc
	cpl am_pm_alarm
done_inc: 
	ljmp loop_b
loop_a:
	jnb one_second_flag, loop_long
	sjmp loop_b
loop_long: 
	ljmp loop
loop_b:
	mov a, Edit_state_mode
	cjne a, #3, normal_display
	
	WriteCommand(#0x01)
	Wait_Milli_Seconds(#2)
	
	Set_Cursor(1,1)
	Send_Constant_String(#Stopwatch)
	
	Set_Cursor(1,8)
	Display_BCD(SW_hrs)
	WriteData(#':')
	Display_BCD(SW_minutes)
	WriteData(#':')
	
	ljmp Display_Done
	
normal_display:

	WriteCommand(#0x01)
	Wait_Milli_Seconds(#2)
	
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    setb half_seconds_flag
	Set_Cursor(2, 1)
    Send_Constant_String(#Initial_Message1)
   
	Set_Cursor(1, 8)
	Display_BCD(Hours_counter)
	WriteData(#':')
	Display_BCD(Minutes_counter)
	WriteData(#':')
	Display_BCD(BCD_counter)
	Set_Cursor(1, 16)
	jnb am_pm_flag, Show_AM
	WriteData(#'P')
Display_Alarm:
	Set_Cursor(2, 8)
	Display_BCD(Alarm_hour)
	WriteData(#':')
	Display_BCD(Alarm_minutes)
	Set_Cursor(2, 13)
	jnb am_pm_alarm, Show_AM_ALARM
	WriteData(#'P')
	sjmp Show_Variable
Show_AM: 
	WriteData(#'A')
	sjmp Display_Alarm
Show_AM_ALARM: 
	WriteData(#'A')
Show_Variable: 
	Set_Cursor(2, 15)
	mov A, Edit_state_mode
	add A, #0x30
	lcall ?WriteData
	mov A, Shift_position
	add A, #0x30
	lcall ?WriteData
	
Display_Done:
    clr one_second_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 14)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counter) ; This macro is also in 'LCD_4bit.inc'
    
    mov a, Hours_counter
    cjne a, Alarm_hour, No_Alarm
    mov a, Minutes_counter
    cjne a, Alarm_minutes, No_Alarm
    
    mov c, am_pm_flag
    jb am_pm_alarm, check_both_pm
    
    jc No_Alarm
    sjmp Alarm_Match
    
check_both_pm: 
	jnc No_Alarm

Alarm_Match:    
    cpl TR0
    sjmp END_DONE
    
No_Alarm: 
	clr TR0 

END_DONE: 
    ljmp loop
END
