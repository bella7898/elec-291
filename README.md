# ELEC 291 - Electrical Engineering Design Studio I
---
## Lab 1 - The N76E003 Microcontroller System  
- requirements: print name and student number
- additional features:
  - I built the chrome dino game, with scrolling background and custom characters for obstacle
  - scrolling feature simulates a 'multiplayer' idea in the first loading screen
  - jumping feature is possible with a button
  - score increments and is displayed on the game end screen, able to be incremented as well. 
## Lab 2 - Alarm Clock
- requirements: functional clock and alarm, with buzzer
- additional features:
  - stopwatch mode counts up forever
  - you are able to save and display an old alarm
  - additional button is able to increment the alarm by 5 min
## Lab 3 - SPI Data Logging using Python
- requirements: send temperature data every 1 sec to the terminal, display a temperature strip chart using Python
- additional features:
  - color changing background based on the temperature crossing set temperature limits
  - crosshair lines detect the closest point on the real line when the mouse hovers on the chart, displays a (x, y) coordinate nearby
  - does a fast fourier transform and displays the smoothed purple line
  - calculates the line of best fit and displays that as a dashed red line
  - able to write the data to a .csv file
## Project 1 - Reflow Oven Controller
- requirements: 
  - program a fsm capable of following a profile for soldering surface mount pcbs and interface with a ssr box that controls a toaster oven to control the temperature
  - read the temperature of the oven with a k-type thermocouple wire and plot the temperatures in a stripchart with a Python program
  - have settable parameters for soak time/temp and reflow time/temp
  - implement a stop and start button
  - terminate the program if the oven does not reach 50℃ within the first 60s
  - maintain thermocouple accuracy (±3℃)
  - display using the built-in 7 segment display on the DE10-Lite and LCD display
  - have sound feedback using a magnetic buzzer
- additional features:
  - user can set parameters in state 0 of the FSM using a 4x4 Keypad
  - speaker and microphone that can record a short audio message and play it back at a specific time
  - servo motors open the door of the oven when the state transitions from 4 - 5 to accelerate cooling
  - OpenCV computer vision implementation using a pretrained YOLO hand-gestures model
