# program name
PROG = grav.exe

# source folder name
VPATH=src
# object folder
OBJDIR=objs
# module folder
MODDIR=mods
# executable folder
EXECDIR=exec

#compiler and flags
FC = gfortran
FFLAGS = -fopenmp -c -O3 -mcmodel=medium -g -I$(MODDIR)
FLINK = -fopenmp -O3 -mcmodel=medium -g
LINKER = $(FC) -o

#object files
OBJS = body.o parameters.o tree.o initializer.o barnes_lf.o saver.o main.o

model: $(PROG)

# creates the model
$(PROG): $(OBJS)
	@echo "--------------------------------------"
	@echo "Creating the executable for the model"
	@echo "--------------------------------------"
	$(LINKER) $(PROG) $(OBJS) $(FLINK)
	mv *.o $(OBJDIR)
	mv *.mod $(MODDIR)
	mv *.exe $(EXECDIR)

%.o: %.f95
	@echo "--------------------------------------"
	@echo "Compiling the file"
	@echo "--------------------------------------"
	$(FC) $(FFLAGS) $<

# cleans up everything
clean:
	@echo "--------------------------------------"
	@echo "Cleaning everything up in model"
	@echo "--------------------------------------"
	rm -f *~ *.nc plot*.png *.exe *.o *.mod
	rm -f $(OBJDIR)/*.o $(OBJDIR)/*~
	rm -f $(MODDIR)/*.mod $(MODDIR)/*~
	rm -f $(EXECDIR)/*~ $(EXECDIR)/*.exe $(EXECDIR)/*.nc
	rm -f $(VPATH)/*~

body.o : body.f95
tree.o : tree.f95 body.o
parameters.o : parameters.f95 tree.o body.o
initializer.o : initializer.f95 parameters.o tree.o body.o
barnes_lf.o : barnes_lf.f95 parameters.o tree.o body.o
saver.o : saver.f95 parameters.o tree.o body.o
main.o : main.f95 parameters.o tree.o body.o
