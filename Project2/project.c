#include <avr/io.h>
#include <avr/interrupt.h>
#include <stdio.h>
#include <stdlib.h>
#include "usart.h"
#include "project.h"
#include "lcd.h"
#include <util/delay.h>

// Joystick
#define THRESHOLD 200
#define CENTER    512

#define PIN_PERIOD (PINB & 0b00000010)

void Configure_Pins(void)
{
    DDRB |=  0b00000001; // PB0 output (LCD D7)
    DDRD |=  0b11111000; // PD3-PD7 outputs (LCD)
    DDRB &= ~0b00000010; // PB1 input (signal)
    PORTB |= 0b00000010; // Enable pull-up on PB1
    DDRC &= ~(1 << PC2); // PC2 (SW) input
    PORTC |= (1 << PC2); // Pull-up on SW
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
    if (sw_pressed)                return "PRESSED         ";
    if (y < CENTER - THRESHOLD)    return "FORWARD         ";
    if (y > CENTER + THRESHOLD)    return "BACKWARD        ";
    if (x < CENTER - THRESHOLD)    return "LEFT            ";
    if (x > CENTER + THRESHOLD)    return "RIGHT           ";
    return                                "CENTER          ";
}

// -------------------------------------------------------------------
// Timing helpers using Timer1 free-running counter (no prescaler)
// -------------------------------------------------------------------
void wait_1ms(void)
{
    unsigned int saved = TCNT1;
    while ((TCNT1 - saved) < (F_CPU / 1000L));
}

void waitms(int ms)
{
    while (ms--) wait_1ms();
}

// -------------------------------------------------------------------
// GetPeriod: measures 'n' periods of the signal on PB1.
// -------------------------------------------------------------------
long int GetPeriod(int n)
{
    int i, overflow;
    unsigned int saved_a, saved_b;

    overflow = 0;
    TIFR1 = 1;
    while (PIN_PERIOD != 0)
    {
        if (TIFR1 & 1) { TIFR1 = 1; overflow++; if (overflow > 50) return 0; }
    }
    overflow = 0;
    TIFR1 = 1;
    while (PIN_PERIOD == 0)
    {
        if (TIFR1 & 1) { TIFR1 = 1; overflow++; if (overflow > 50) return 0; }
    }

    overflow = 0;
    TIFR1 = 1;
    saved_a = TCNT1;
    for (i = 0; i < n; i++)
    {
        while (PIN_PERIOD != 0)
        {
            if (TIFR1 & 1) { TIFR1 = 1; overflow++; if (overflow > 1024) return 0; }
        }
        while (PIN_PERIOD == 0)
        {
            if (TIFR1 & 1) { TIFR1 = 1; overflow++; if (overflow > 1024) return 0; }
        }
    }
    saved_b = TCNT1;

    if (saved_b < saved_a) overflow--;
    return overflow * 0x10000L + (saved_b - saved_a);
}

int main(void)
{
    long int count;
    float T, f, C_nF;
    char buff[17];
    const char *joy_state = "";
    const char *last_joy_state = "";

    Configure_Pins();
    ADC_Init();
    usart_init();

    TCCR1B |= _BV(CS10);

    LCD_4BIT();
    waitms(500);

    printf("\x1b[2J\x1b[1;1H");
    printf("ATMega328P Capacitance Meter + Joystick\r\n");
    printf("File: %s\r\n", __FILE__);
    printf("Compiled: %s, %s\r\n\r\n", __DATE__, __TIME__);

    LCDprint("Freq:", 1, 1);
    LCDprint("Cap:", 2, 1);

    while (1)
    {
        // --- Joystick ---
        uint16_t x  = ADC_Read(0);          // VRx ? PC0 (ADC0)
        uint16_t y  = ADC_Read(1);          // VRy ? PC1 (ADC1)
        uint8_t  sw = !(PINC & (1 << PC2)); // SW  ? PC2 (active LOW)

        joy_state = Get_Joystick_State(x, y, sw);
        if (joy_state != last_joy_state)
        {
            printf("Joystick: %s | X=%4u Y=%4u SW=%u\r\n", joy_state, x, y, sw);
            last_joy_state = joy_state;
        }

        // --- Capacitance measurement ---
        count = GetPeriod(10);

        if (count > 0)
        {
            T    = count / (F_CPU * 10.0);
            f    = 1.0 / T;

            printf("F:%.2f Hz  C:%.3f nF  Joy:%s\r\n", f, C_nF, joy_state);

            sprintf(buff, "F:%.1f Hz", f);
            LCDprint(buff, 1, 1);
            sprintf(buff, "C:%.3f nF", C_nF);
            LCDprint(buff, 2, 1);
        }
        else
        {
            printf("NO SIGNAL  Joy:%s\r\n", joy_state);
            LCDprint("Freq: NO SIGNAL ", 1, 1);
            LCDprint("Cap:  -------   ", 2, 1);
        }

        waitms(200);
    }
}