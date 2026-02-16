import time
import serial
import cv2
import numpy as np
from ultralytics import YOLO
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import sys, time, math
import threading
import csv

model = YOLO('best.pt')

cap = cv2.VideoCapture(0)

frame_count = 0

t_units = ["C", "F", "K"]
t_ranges = [15, 250, 59, 482, 288, 524]

csvfile = open("data.csv", "w", newline="")
writer = csv.writer(csvfile)
writer.writerow(["time", "value"])

def on_close_figure(event):
    csvfile.close()
    cap.release()
    cv2.destroyAllWindows()
    sys.exit(0)

def on_key(event):
    if event.key == 'd':
        ser.write(("d").encode())
    if event.key == 'r':
        ser.write(("r").encode())

# configure the serial port
ser = serial.Serial(
    port='COM5',
    baudrate=57600,
    parity=serial.PARITY_NONE,
    stopbits=serial.STOPBITS_TWO,
    bytesize=serial.EIGHTBITS
)
ser.isOpen()
start_flag = False
option_units = 0
line_c = 'red'

print("Welcome to the Reflow Temperature Logger!")
print("Enter 0 for Celsius, 1 for Farenheit, and 2 for Kelvin:")
option_units = int(input(""))

if option_units > 2:
    option_units = 0

print("Enter a threshold value to monitor:")
threshold = float(input(""))

print("Enter 'start' to begin data logging:")

while start_flag == False: 
    cmd = input("") 
    if cmd == "start":  
        start_flag = True
        
print("\n")
print(model.names)

"""
Graphing Portion of Code
"""
xsize=100
def data_gen():
    t = data_gen.t
    while True:
       t+=1
       strin = ser.readline()
       if strin == "trans":
            stage = stage + 1
       else:
           temp_val = float(strin.decode('utf-8').strip())
           if option_units == 1:
               temp_val = (float(9/5)*temp_val)+32
           elif option_units == 2:
               temp_val = temp_val+float(273.15)

       if temp_val >= (t_ranges[option_units*2]+section*3) and temp_val <= (t_ranges[option_units*2+1]):
           line_c = 'red'
       elif temp_val >= (t_ranges[option_units*2]+section*2) and temp_val <=(t_ranges[option_units*2]+section*3):
           line_c = 'green'
       elif temp_val >= (t_ranges[option_units*2]+section) and temp_val <= (t_ranges[option_units*2]+section*2):
           line_c = 'blue'
       else:
           line_c = 'purple'

       if temp_val > threshold:
           warning.set_visible(True)
       else:
           warning.set_visible(False)

       writer.writerow([t, temp_val])
       csvfile.flush()
       
       yield t, temp_val, line_c

def webcam_loop():
    global frame_count
    while True:
        ret, frame = cap.read()
        frame_count += 1

        if frame_count % 3 != 0:
            continue
        
        results = model(frame, conf=0.5)
        annotated = results[0].plot()
        cv2.imshow("Hand", annotated)
        cv2.waitKey(1)

        for box in results[0].boxes:
            cls_id = int(box.cls[0])
            label = model.names[cls_id]
            if label == "O":       
                ser.write(("d").encode())
                break
        if cv2.waitKey(1) & 0xFF == ord('q'):
            break
    cap.release()
    cv2.destroyAllWindows()

def run(data):
  
    # update the data
    t,y,lc = data
    if t>-1:
        xdata.append(t)
        ydata.append(y)
        if t>xsize: # Scroll to the left.
            ax.set_xlim(t-xsize, t)
        line.set_data(xdata, ydata)
        line.set_color(lc)

    return line,

#def on_close_figure(event):
    #sys.exit(0)

data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('key_press_event', on_key)
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line, = ax.plot([], [], lw=2)
ax.set_ylim(t_ranges[option_units*2],t_ranges[option_units*2+1])
ax.set_xlim(0, xsize)
section = (t_ranges[option_units*2+1]-t_ranges[option_units*2])/4

ax.axhspan(t_ranges[option_units*2], t_ranges[option_units*2]+section, color='purple', alpha=0.2)
ax.axhspan(t_ranges[option_units*2]+section, t_ranges[option_units*2]+section*2, color='blue', alpha=0.2)
ax.axhspan(t_ranges[option_units*2]+section*2, t_ranges[option_units*2]+section*3, color='green', alpha=0.2)
ax.axhspan(t_ranges[option_units*2]+section*3, t_ranges[option_units*2+1], color='red', alpha=0.2)

ax.set_xlabel("Time (s)")
ax.set_ylabel("Temperature ("+t_units[option_units]+")")
ax.set_title("Thermocouple Data")
warning = ax.text(0.8, 1.03, "Overheat", transform=ax.transAxes, fontsize=12, color='red')
ax.grid()
xdata, ydata = [], []

# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=100, repeat=False)
t = threading.Thread(target=webcam_loop, daemon=True)
t.start()
plt.show()



