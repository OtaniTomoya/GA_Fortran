# Makefile for the genetic algorithm Fortran project with OpenMP

# Compiler
FC = gfortran

# Compiler flags
FFLAGS = -O2 -Wall -Wextra -fimplicit-none -fopenmp

# Module and object files
MODULES = parameters.mod data_handling.mod tree_structure.mod tree_generation.mod leaf_label_update.mod prediction.mod evaluation.mod genetic_operators.mod

OBJS = parameters.o data_handling.o tree_structure.o tree_generation.o leaf_label_update.o prediction.o evaluation.o genetic_operators.o genetic_algorithm_main.o

# Executable name
EXEC = genetic_algorithm

# Default target
all: $(EXEC)

# Link the executable
$(EXEC): $(OBJS)
	$(FC) $(FFLAGS) -o $(EXEC) $(OBJS)

# Compile modules and source files
parameters.o: parameters.f90
	$(FC) $(FFLAGS) -c parameters.f90

data_handling.o: data_handling.f90 parameters.mod
	$(FC) $(FFLAGS) -c data_handling.f90

tree_structure.o: tree_structure.f90 parameters.mod
	$(FC) $(FFLAGS) -c tree_structure.f90

tree_generation.o: tree_generation.f90 tree_structure.mod parameters.mod
	$(FC) $(FFLAGS) -c tree_generation.f90

leaf_label_update.o: leaf_label_update.f90 tree_structure.mod parameters.mod
	$(FC) $(FFLAGS) -c leaf_label_update.f90

prediction.o: prediction.f90 tree_structure.mod parameters.mod
	$(FC) $(FFLAGS) -c prediction.f90

evaluation.o: evaluation.f90 prediction.mod tree_structure.mod parameters.mod
	$(FC) $(FFLAGS) -c evaluation.f90

genetic_operators.o: genetic_operators.f90 tree_structure.mod parameters.mod tree_generation.mod leaf_label_update.mod
	$(FC) $(FFLAGS) -c genetic_operators.f90

genetic_algorithm_main.o: genetic_algorithm_main.f90 genetic_operators.mod tree_structure.mod parameters.mod evaluation.mod
	$(FC) $(FFLAGS) -c genetic_algorithm_main.f90

# Clean up
clean:
	rm -f $(OBJS) $(MODULES) $(EXEC)

# Phony targets
.PHONY: all clean
