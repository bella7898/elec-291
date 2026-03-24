#ifndef I2C_H
#define I2C_H

#include <avr/io.h>
#include <util/delay.h>

#define SCL_DDR   DDRB
#define SCL_PORT  PORTB
#define SCL_PIN   PB0

#define SDA_DDR   DDRD
#define SDA_PORT  PORTD
#define SDA_PIN   PD7
#define SDA_IN    PIND

void I2C_Init(void);
void I2C_Start(void);
void I2C_Stop(void);
uint8_t I2C_Write(uint8_t data);

#endif