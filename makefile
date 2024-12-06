# Makefile for the genetic algorithm Fortran project with OpenMP

# Compiler
FC = gfortran

# Compiler flags
FFLAGS = -Wall -Wextra -fimplicit-none -fopenmp -J$(MODDIR)

# Directories
MODDIR = mod
OBJDIR = obj

# Module and object files
MODULES = $(MODDIR)/parameters.mod $(MODDIR)/data_handling.mod $(MODDIR)/tree_structure.mod $(MODDIR)/tree_generation.mod $(MODDIR)/leaf_label_update.mod $(MODDIR)/prediction.mod $(MODDIR)/evaluation.mod $(MODDIR)/genetic_operators.mod

OBJS = $(OBJDIR)/parameters.o $(OBJDIR)/data_handling.o $(OBJDIR)/tree_structure.o $(OBJDIR)/tree_generation.o $(OBJDIR)/leaf_label_update.o $(OBJDIR)/prediction.o $(OBJDIR)/evaluation.o $(OBJDIR)/genetic_operators.o $(OBJDIR)/genetic_algorithm_main.o

# Executable name
EXEC = genetic_algorithm

# Default target
all: $(EXEC)

# Ensure directories exist
$(MODDIR) $(OBJDIR):
	mkdir -p $@

# Link the executable
$(EXEC): $(OBJS)
	$(FC) $(FFLAGS) -o $(EXEC) $(OBJS)

# Compile modules and source files
$(OBJDIR)/parameters.o: parameters.f90 | $(MODDIR) $(OBJDIR)
	$(FC) $(FFLAGS) -c parameters.f90 -o $@

$(OBJDIR)/data_handling.o: data_handling.f90 $(MODDIR)/parameters.mod | $(MODDIR) $(OBJDIR)
	$(FC) $(FFLAGS) -c data_handling.f90 -o $@

$(OBJDIR)/tree_structure.o: tree_structure.f90 $(MODDIR)/parameters.mod | $(MODDIR) $(OBJDIR)
	$(FC) $(FFLAGS) -c tree_structure.f90 -o $@

$(OBJDIR)/tree_generation.o: tree_generation.f90 $(MODDIR)/tree_structure.mod $(MODDIR)/parameters.mod | $(MODDIR) $(OBJDIR)
	$(FC) $(FFLAGS) -c tree_generation.f90 -o $@

$(OBJDIR)/leaf_label_update.o: leaf_label_update.f90 $(MODDIR)/tree_structure.mod $(MODDIR)/parameters.mod | $(MODDIR) $(OBJDIR)
	$(FC) $(FFLAGS) -c leaf_label_update.f90 -o $@

$(OBJDIR)/prediction.o: prediction.f90 $(MODDIR)/tree_structure.mod $(MODDIR)/parameters.mod | $(MODDIR) $(OBJDIR)
	$(FC) $(FFLAGS) -c prediction.f90 -o $@

$(OBJDIR)/evaluation.o: evaluation.f90 $(MODDIR)/prediction.mod $(MODDIR)/tree_structure.mod $(MODDIR)/parameters.mod | $(MODDIR) $(OBJDIR)
	$(FC) $(FFLAGS) -c evaluation.f90 -o $@

$(OBJDIR)/genetic_operators.o: genetic_operators.f90 $(MODDIR)/tree_structure.mod $(MODDIR)/parameters.mod $(MODDIR)/tree_generation.mod $(MODDIR)/leaf_label_update.mod | $(MODDIR) $(OBJDIR)
	$(FC) $(FFLAGS) -c genetic_operators.f90 -o $@

$(OBJDIR)/genetic_algorithm_main.o: genetic_algorithm_main.f90 $(MODDIR)/genetic_operators.mod $(MODDIR)/tree_structure.mod $(MODDIR)/parameters.mod $(MODDIR)/evaluation.mod | $(MODDIR) $(OBJDIR)
	$(FC) $(FFLAGS) -c genetic_algorithm_main.f90 -o $@

# Clean up
clean:
	rm -rf $(OBJDIR) $(MODDIR) $(EXEC)

# Phony targets
.PHONY: all clean
