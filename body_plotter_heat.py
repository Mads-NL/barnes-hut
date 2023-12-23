import os
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np
import re

# Directory containing the data files
data_directory = 'data/'

# Function to read data from a file
def read_data(file_path):
    data = np.loadtxt(file_path, usecols=(0, 1))  # Reading only x and y columns
    return data

# Function to sort files by their numerical value in the filename
def numerical_sort(file):
    numbers = re.findall(r'\d+', file)
    return int(numbers[0]) if numbers else 0

# Function to extract and animate the data
def animate_bodies(data_directory):
    # List all files and sort them numerically
    files = sorted([f for f in os.listdir(data_directory) if f.startswith('body_data_') and f.endswith('.dat')], key=numerical_sort)
    all_data = []

    # Read data from each file
    for file in files:
        file_path = os.path.join(data_directory, file)
        data = read_data(file_path)
        all_data.append(data)

    # Create animation
    fig, ax = plt.subplots()
    ax.set_xlim(-1, 1)
    ax.set_ylim(-1, 1)
    ax.set_aspect('equal', adjustable='box')

    # Heatmap setup
    heatmap, xedges, yedges = np.histogram2d([], [], bins=100, range=[[-1, 1], [-1, 1]])
    extent = [xedges[0], xedges[-1], yedges[0], yedges[-1]]
    im = ax.imshow(heatmap.T, extent=extent, origin='lower', aspect='auto', cmap='hot')

    # Update function for animation
    def update(frame):
        heatmap, _, _ = np.histogram2d(all_data[frame][:,0], all_data[frame][:,1], bins=100, range=[[-1, 1], [-1, 1]])
        im.set_data(heatmap.T)
        im.autoscale()  # Auto-scale the colormap to the histogram
        return im,

    # Create animation
    ani = animation.FuncAnimation(fig, update, frames=len(all_data), interval=200)

    return ani

# Create and display the animation
ani = animate_bodies(data_directory)

# Save the animation as a GIF
output_file = 'body_animation_heatmap.gif'
ani.save(output_file, fps=30)
