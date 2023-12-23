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
    ax.set_aspect('equal', adjustable='box')  # Ensuring equal aspect ratio

    # Scatter plot which will be updated
    scatter = ax.scatter([], [], s=30, edgecolor='black', alpha=0.7)  # Adjust size, edge color, and transparency

    # Styling the plot
    ax.set_facecolor('lightgrey')  # Setting a background color
    ax.grid(True, which='both', linestyle='--', linewidth=0.5)  # Adding a grid
    ax.set_xlabel('X Position')
    ax.set_ylabel('Y Position')
    ax.set_title('Body Movement Animation')

    # Update function for animation
    def update(frame):
        scatter.set_offsets(all_data[frame])
        return scatter,

    # Create animation
    ani = animation.FuncAnimation(fig, update, frames=len(all_data), interval=200, blit=True)

    return ani

# Create and display the animation
ani = animate_bodies(data_directory)

# Save the animation as a GIF
output_file = 'body_animation.gif'
ani.save(output_file, fps=30)
