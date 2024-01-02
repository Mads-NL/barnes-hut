# Barnes-Hut Simulation Repository

## Overview
This repository contains an implementation of the Barnes-Hut algorithm in Fortran. The Barnes-Hut algorithm is a tree-based method to approximate the forces in an N-body simulation, ideal for simulating systems with a large number of particles, like stars in a galaxy. The next step is to implement SPH for a realistic simulation of a galaxy.

## Files Description
- **barnes_lf.f95**: Implements the `calculate_forces` subroutine for calculating forces acting on each body using the tree structure.
- **body.f95**: Defines the `Body` type module with properties such as position, velocity, mass, forces, and accelerations.
- **initializer.f95**: Contains the `init` subroutine for initializing simulation parameters, including the setup of bodies.
- **main.f95**: The main program that integrates all modules and subroutines to run the simulation.
- **parameters.f95**: A module that defines simulation constants and parameters like the gravitational constant and theta.
- **saver.f95**: Includes the `save_body_positions` subroutine to save the state of bodies at each simulation step.
- **tree.f95**: Defines the `Node` and `Node_p` types for the tree structure, central to the Barnes-Hut algorithm.

## Usage

To run the simulation, follow these steps:

1. **Compilation**: Do `make` in the folder.
2. **Execution**: Run `./exec/grav.exe`
