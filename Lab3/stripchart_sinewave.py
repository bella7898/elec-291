import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import sys, time, math
import serial
import csv
from scipy.fft import fft, ifft, fftfreq

csvfile = open("data.csv", "w", newline="")
writer = csv.writer(csvfile)
writer.writerow(["time", "value"])

#configure the serial port
ser = serial.Serial(
    port='COM5', 
    baudrate=115200, 
    parity = serial.PARITY_NONE, 
    stopbits=serial.STOPBITS_TWO, 
    bytesize=serial.EIGHTBITS,
    timeout=1
)
ser.isOpen()

xsize=30
ysize=27

temp_safe = 26.4
temp_warning = 26.7
temp_danger = 27

fft_cutoff = 0.1
   
def data_gen():
    t = data_gen.t
    while True:
       t+=1
       line=ser.readline()
       val = float(line.decode('utf-8').strip())
       yield t, val

def fft_smooth(data, cutoff_fraction=0.1):
    if len(data)<10:
        return data

    fft_data= fft(data)

    n = len(data)

    cutoff_index = int(n*cutoff_fraction)
    fft_filtered = fft_data.copy()
    fft_filtered[cutoff_index:n-cutoff_index] = 0

    smoothed = np.real(ifft(fft_filtered))
    return smoothed

def run(data):
    global ysize
    # update the data
    t,y = data
    if t>-1:
        xdata.append(t)
        ydata.append(y)
        if t>xsize: # Scroll to the left.
            ax.set_xlim(0, t)

        current_y_min, current_y_max = ax.get_ylim()
        if y> current_y_max:
            ax.set_ylim(current_y_min, current_y_max + 2)
            ysize = ysize + 2
        elif y<current_y_min:
            ax.set_ylim(current_y_min - 2, current_y_max)

        update_color_zones()
        
        line.set_data(xdata, ydata)
        writer.writerow([t,y])

    if len(ydata) > 10:
        smoothed_y = fft_smooth(ydata, fft_cutoff)
        fft_line.set_data(xdata, smoothed_y)

    if len(xdata) > 5:
        x = np.array(xdata)
        y = np.array(ydata)

        m, b = np.polyfit(x, y, 1)
        y_trend = m*x + b
        trendline.set_data(x, y_trend)
        trendline.set_label(f'Trend: y = {m: .3f}x + {b:.2f}')
        fft_line.set_label(f'FFT Noise Filter ({int(fft_cutoff*100)} % freq)')
        ax.legend()

    return line,

def update_color_zones():
    for zone in color_zones:
        zone.remove()
    color_zones.clear()

    y_min, y_max = ax.get_ylim()

    zone1 = ax.axhspan(y_min, temp_safe, facecolor='lightgreen', alpha=0.3, zorder=0)
    color_zones.append(zone1)
    zone2 = ax.axhspan(temp_safe, temp_warning, facecolor='lightyellow', alpha=0.3, zorder=0)
    color_zones.append(zone2)
    zone3 = ax.axhspan(temp_warning, temp_danger, facecolor='peachpuff', alpha=0.3, zorder=0)
    color_zones.append(zone3)
    zone4 = ax.axhspan(temp_danger, y_max, facecolor='lightcoral', alpha=0.3, zorder=0)
    color_zones.append(zone4)

def on_mouse_move(event):
    if event.inaxes != ax or len(xdata) == 0:
        vline.set_visible(False)
        hline.set_visible(False)
        coord_text.set_visible(False)
        fig.canvas.draw_idle()
        return

    x_mouse = event.xdata
    y_mouse = event.ydata

    x_array = np.array(xdata)
    y_array = np.array(ydata)

    distances = np.sqrt((x_array - x_mouse)**2 + (y_array - y_mouse)**2)
    closest_idx = np.argmin(distances)

    closest_x = xdata[closest_idx]
    closest_y = ydata[closest_idx]

    vline.set_xdata([closest_x, closest_x])
    vline.set_visible(True)

    hline.set_ydata([closest_y, closest_y])
    hline.set_visible(True)

    coord_text.set_position((closest_x, closest_y))
    coord_text.set_text(f'({closest_x:.1f}, {closest_y:.2f})')
    coord_text.set_visible(True)
    fig.canvas.draw_idle()

def on_close_figure(event):
    csvfile.close()
    sys.exit(0)

data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
fig.canvas.mpl_connect('motion_notify_event', on_mouse_move)
ax = fig.add_subplot(111)

color_zones = []

line, = ax.plot([], [], lw=2)
trendline, = ax.plot([], [], 'r--', lw=2, label='Trend')
fft_line, = ax.plot([], [], 'purple', lw=2, label='FFT Noise Filter', zorder=6)
vline = ax.axvline(x = 0, color='gray', linestyle='--', lw=1, visible=False)
hline = ax.axhline(y = 0, color='gray', linestyle='--', lw=1, visible=False)
coord_text= ax.text(0, 0, '', fontsize=9,
                    bbox=dict(boxstyle='round, pad=0.5', facecolor='pink', alpha=0.7),
                    visible=False)
ax.axhline(y= temp_safe, color='green', linestyle=':', lw= 1, alpha = 0.7, label=f'Safe limit: {temp_safe} °C')
ax.axhline(y= temp_warning, color='orange', linestyle=':', lw= 1, alpha = 0.7, label=f'Warning: {temp_warning} °C')
ax.axhline(y= temp_danger, color='red', linestyle=':', lw= 1, alpha = 0.7, label=f'Danger: {temp_danger} °C')

ax.legend()
ax.set_title('Temperature vs Time Plot')
ax.set_ylim(26, ysize)
ax.set_ylabel('Temperature (°C)')
ax.set_xlim(0, xsize)
ax.set_xlabel('Time (s)')
ax.grid()
xdata, ydata = [], []

update_color_zones()

# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig,
                              run,
                              data_gen,
                              blit=False,
                              interval=100,
                              repeat=False,
                              cache_frame_data=False
                              )
plt.show()
