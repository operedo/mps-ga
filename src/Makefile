MPI_HOME = /opt/mpi/bullxmpi/1.1.11.1

CC=icc
CCFLAGS=

#F90= pgf90
#F90= ifort
F90 = $(MPI_HOME)/bin/mpif90


#F90= xlf90_r
#F90FLAGS= -auto
#F90FLAGS= -O2 -inline-level=0
#F90FLAGS= -O2
#F90FLAGS= -O2 -xHost -assume byterecl
#F90FLAGS= -O2 -auto

#F90FLAGS= -heap-arrays 1000 
#F90FLAGS= -auto -parallel -fpp -O3 -xSSE4.2 -vec-report2 
F90FLAGS= -auto -xSSE4.2 -vec-report2 
#F90FLAGS= 

#F90FLAGS= -O1 -auto
#F90FLAGS= -O2 -auto -axSSE4.2
#F90FLAGS= -auto -axSSE4.2
#F90FLAGS= -xHost -O3 
#F90FLAGS= -O1 
#F90FLAGS= -O1
#F90FLAGS= -O3 -xSSE4.2 
#F90FLAGS= -O2 -inline-level=0
#F90FLAGS= -pg -O2 -inline-level=0
OPENMPFLAGS= -openmp
#OPENMPFLAGS= 
SERIALFLAGS= 
#OPENMPFLAGS= -qsuffix=f=f90 -qsmp=omp


ACCOPT = -ta=nvidia -Minfo=accel
ACCLDOPT = -ta=nvidia



AR= ar
ARFLAGS= cr
RANLIB= ranlib

MPSGALIB= libmpsga.a

#MPSGA_OBJECTS= chainOperations.o chainOperationsV2.o mutationWithoutRandom.o mutationConditionalWithoutRandom.o crossoverWithoutRandom.o fitnessFunctionChainOperations.o fitnessFunctionChainOperationsCells.o fitnessFunctionChainOperationsCellsUnroll.o fitnessFunctionChainOperationsCellsMPI.o loadHistogram.o loadHistogramCells.o printHistogram.o generateInitialPopulation.o
MPSGA_OBJECTS= chainOperations.o chainOperationsV2.o mutationWithoutRandom.o mutationConditionalWithoutRandom.o crossoverWithoutRandom.o fitnessFunctionChainOperations.o fitnessFunctionChainOperationsCells_baseline.o fitnessFunctionChainOperationsCells.o fitnessFunctionChainOperationsCellsUnroll.o loadHistogram.o loadHistogramCells.o loadHistogramCells_baseline.o printHistogram.o generateInitialPopulation.o
IO_OBJECTS=
AUX_OBJECTS= initRandomSeed.o qsort.o rng.o dotmulSIMD.o dotsquareSIMD.o 
#AUX_OBJECTS= initRandomSeed.o qsort.o rng.o
MOD_OBJECTS= chainoperations.mod  chainoperationsv2.mod  rng.mod

acc: lib mainAcc

mpi: lib mainMPIOpenmp

paral: lib mainOpenmp

serial: lib mainSerial

lib: $(AUX_OBJECTS) $(IO_OBJECTS) $(MPSGA_OBJECTS)  
	$(AR) $(ARFLAGS) \
		$(MPSGALIB) \
		$(MPSGA_OBJECTS) \
		$(AUX_OBJECTS) \
		$(MOD_OBJECTS)
	$(RANLIB) $(MPSGALIB)	
	cp $(MPSGALIB) ../lib

mainMPIOpenmp: mainMPIOpenmp.o $(MPSGA_OBJECTS) $(AUX_OBJECTS) $(IO_OBJECTS)
	$(F90) $(F90FLAGS) $(OPENMPFLAGS) -o mainMPIOpenmp.exe \
		mainMPIOpenmp.o \
		$(MPSGALIB)


mainOpenmp: mainOpenmp.o $(MPSGA_OBJECTS) $(AUX_OBJECTS) $(IO_OBJECTS)
	$(F90) $(F90FLAGS) $(OPENMPFLAGS) -o mainOpenmp.exe \
		mainOpenmp.o \
		$(MPSGALIB)

mainAcc: mainAcc.o $(MPSGA_OBJECTS) $(AUX_OBJECTS) $(IO_OBJECTS)
	$(F90) $(ACCLDOPT) $(OPENMPFLAGS) -o mainAcc.exe \
		mainAcc.o \
		$(MPSGALIB)


mainSerial: mainSerial.o $(MPSGA_OBJECTS) $(AUX_OBJECTS) $(IO_OBJECTS)
	$(F90) $(F90FLAGS) $(SERIALFLAGS) -o mainSerial.exe \
		mainSerial.o \
		$(MPSGALIB)

clean :
	        rm -f *.o *.mod *.a ../lib/*.a *.exe

.SUFFIXES: .o .f90 .c

.f90.o : ; $(F90) -c $(F90FLAGS) $(OPENMPFLAGS) $*.f90
#.f90.o : ; $(F90) -c $(ACCOPT) $(OPENMPFLAGS) $*.f90

.c.o : ; $(CC) -c $(CCFLAGS) $(OPENMPFLAGS) $*.c

