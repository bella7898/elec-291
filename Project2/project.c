#include <avr/io.h>
#include <avr/interrupt.h>
#include <stdio.h>
#include <stdlib.h>
#include "usart.h"
#include "project.h"
#include "lcd.h"
#include <util/delay.h>

#define THRESHOLD 200
#define CENTER    512

// Button pin macros
#define BTN_PD3  (!(PIND & (1 << PD3)))
#define BTN_PD5  (!(PIND & (1 << PD5)))
#define BTN_PD6  (!(PIND & (1 << PD6)))
#define BTN_PB1  (!(PINB & (1 << PB1)))

volatile uint8_t carrier_enabled = 0;

ISR(TIMER2_COMPA_vect)
{
    if (carrier_enabled && !(PIND & (1 << PD1)))
        PORTB ^= (1 << PB2);
    else
        PORTB &= ~(1 << PB2);
}

void IR_Off(void)
{
    carrier_enabled = 0;
    PORTB &= ~(1 << PB2);
}

void IR_On(void)
{
    carrier_enabled = 1;
}

void IR_Init(void)
{
    DDRB  |=  (1 << PB2);
    PORTB &= ~(1 << PB2);

    TCCR2A = (1 << WGM21);
    TCCR2B = (1 << CS20);
    OCR2A  = 209;
    TIMSK2 = (1 << OCIE2A);

    IR_Off();
}

void Configure_Pins(void)
{
    DDRD  |=  0b10010000;
    DDRB  &= ~0b00000011;
    PORTB |=  0b00000011;
    DDRC  &= ~(1 << PC2);
    PORTC |=  (1 << PC2);
    PORTD |=  (1 << PD3) | (1 << PD5) | (1 << PD6);
}

void ADC_Init(void)
{
    ADMUX  = (1 << REFS0);
    ADCSRA = (1 << ADEN) | (1 << ADPS2) | (1 << ADPS1) | (1 << ADPS0);
}

uint16_t ADC_Read(uint8_t channel)
{
    ADMUX = (1 << REFS0) | (channel & 0x07);
    ADCSRA |= (1 << ADSC);
    while (ADCSRA & (1 << ADSC));
    return ADC;
}

const char* Get_Joystick_State(uint16_t x, uint16_t y, uint8_t sw_pressed)
{
    if (sw_pressed)              return "S";
    if (y < CENTER - THRESHOLD)  return "F";
    if (y > CENTER + THRESHOLD)  return "B";
    if (x < CENTER - THRESHOLD)  return "L";
    if (x > CENTER + THRESHOLD)  return "R";
    return "";
}

void waitms(int ms) { _delay_ms(1); while (--ms) _delay_ms(1); }

void Check_Buttons(void)
{
    static uint8_t last_pd3 = 0, last_pd5 = 0, last_pd6 = 0, last_pb1 = 0;

    uint8_t pd3 = BTN_PD3;
    uint8_t pd5 = BTN_PD5;
    uint8_t pd6 = BTN_PD6;
    uint8_t pb1 = BTN_PB1;

    if (pd3  && !last_pd3)  { putchar('T'); IR_On();  }
    if (!pd3 &&  last_pd3)    IR_Off();
    if (pd5  && !last_pd5)  { putchar('G'); IR_On();  }
    if (!pd5 &&  last_pd5)    IR_Off();
    if (pd6  && !last_pd6)  { putchar('S'); IR_On();  }
    if (!pd6 &&  last_pd6)    IR_Off();
    if (pb1  && !last_pb1)  { putchar('P'); IR_On();  }
    if (!pb1 &&  last_pb1)    IR_Off();

    last_pd3 = pd3; last_pd5 = pd5;
    last_pd6 = pd6; last_pb1 = pb1;
}

int main(void)
{
    char buff[17];
    const char *joy_state = "";
    const char *last_joy_state = "";

    Configure_Pins();
    ADC_Init();
    usart_init();
    IR_Init();

    LCD_4BIT();
    waitms(500);
    sei();

    LCDprint("Ready", 1, 1);

    while (1)
    {
        Check_Buttons();

        uint16_t x  = ADC_Read(0);
        uint16_t y  = ADC_Read(1);
        uint8_t  sw = !(PINC & (1 << PC2));

        joy_state = Get_Joystick_State(x, y, sw);
        if (joy_state != last_joy_state)
        {
        	if (joy_state[0] != '\0'){
	            printf("J:%s\r\n", joy_state);
	            sprintf(buff, "Joy: %s", joy_state);
	            LCDprint(buff, 2, 1);
	            last_joy_state = joy_state;
            }
        }

        waitms(200);
    }
}