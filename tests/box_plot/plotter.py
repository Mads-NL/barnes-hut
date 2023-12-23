import numpy as np
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.collections import PolyCollection

def plot_cuboid(center, size):
    """
    Creates a plot of a cuboid based on the center and size.
    """
    ox, oy, oz = center
    l, w, h = size

    x = np.linspace(ox-l/2, ox+l/2, 2)
    y = np.linspace(oy-w/2, oy+w/2, 2)
    z = np.linspace(oz-h/2, oz+h/2, 2)
    x, y = np.meshgrid(x, y)

    ax.plot_surface(x, y, np.full_like(x, oz-h/2), color='b', alpha=0.1)
    ax.plot_surface(x, y, np.full_like(x, oz+h/2), color='b', alpha=0.1)

    y, z = np.meshgrid(y[:,0], z)
    ax.plot_surface(np.full_like(y, ox-l/2), y, z, color='b', alpha=0.1)
    ax.plot_surface(np.full_like(y, ox+l/2), y, z, color='b', alpha=0.1)

    x, z = np.meshgrid(x[0], z)
    ax.plot_surface(x, np.full_like(x, oy-w/2), z, color='b', alpha=0.1)
    ax.plot_surface(x, np.full_like(x, oy+w/2), z, color='b', alpha=0.1)

# Load bodies' positions
bodies_file = 'body_data.dat'  # Replace with the actual path
bodies_data = np.loadtxt(bodies_file)
x_coords, y_coords, z_coords = bodies_data[:,0], bodies_data[:,1], bodies_data[:,2]

# Load boxes' extents
boxes_file = 'box_data.dat'  # Replace with the actual path
boxes_data = np.loadtxt(boxes_file)

# Create a 3D plot
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

# Plot bodies
ax.scatter(x_coords, y_coords, z_coords, color='k')

# Plot boxes
for box in boxes_data:
    center = ((box[0] + box[1])/2, (box[2] + box[3])/2, (box[4] + box[5])/2)
    size = (box[1] - box[0], box[3] - box[2], box[5] - box[4])
    plot_cuboid(center, size)

# Set labels
ax.set_xlabel('X Coordinate')
ax.set_ylabel('Y Coordinate')
ax.set_zlabel('Z Coordinate')

# Show the plot
plt.show()
