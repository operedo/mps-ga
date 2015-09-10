MPI_HOME = /opt/mpi/bullxmpi/1.1.11.1
EXTRAE_HOME = /gpfs/apps/NVIDIA/CEPBATOOLS/extrae/2.2.1/64
PAPI_HOME = /gpfs/apps/NVIDIA/PAPI/4.1.3/64
XML2_LDFLAGS = -L/usr/lib64
XML2_LIBS = -lxml2
UNWIND_HOME = /gpfs/apps/NVIDIA/LIBUNWIND/1.0.1

#F90 = $(MPI_HOME)/bin/mpif90
F90 = ifort
F90FLAGS = -auto -openmp -g -I$(EXTRAE_HOME)/include -I.
#CLIBS = -L$(EXTRAE_HOME)/lib -lompitracef -lxml2 $(PAPI_HOME)/lib/libpapi.a -L/usr/lib64 -lbfd -L/usr/lib64 -liberty $(UNWIND_HOME)/lib/libunwind.a /lib64/librt.so.1
#CLIBS = -L$(EXTRAE_HOME)/lib -lompitracef -lxml2 -L$(PAPI_HOME)/lib -lpapi -L/usr/lib64 -lbfd -L/usr/lib64 -liberty -L$(UNWIND_HOME)/lib -lunwind /lib64/librt.so.1
CLIBS = $(EXTRAE_HOME)/lib/libomptrace.a $(PAPI_HOME)/lib/libpapi.a $(XML2_LDFLAGS) $(XML2_LIBS)  -L/usr/lib64 -lbfd -L/usr/lib64 -liberty $(UNWIND_HOME)/lib/libunwind.a /lib64/librt.so.1 -liomp


trace: chainOperationsV2.o fitnessFunctionChainOperationsCells.o loadHistogramCells.o testfitnessChainOperationsCells_trace.o 
	$(F90) -c $(F90FLAGS) chainOperationsV2.f90
	$(F90) -c $(F90FLAGS) loadHistogramCells.f90
	$(F90) -c $(F90FLAGS) fitnessFunctionChainOperationsCells.f90
	$(F90) -c $(F90FLAGS) testfitnessChainOperationsCells_trace.f90
	$(F90) $(F90FLAGS) -o testfitnessChainOperationsCells_trace.exe testfitnessChainOperationsCells_trace.o loadHistogramCells.o fitnessFunctionChainOperationsCells.o chainOperationsV2.o $(CLIBS)  
#	$(F90) $(F90FLAGS) -o testfitnessChainOperationsCells_trace.exe testfitnessChainOperationsCells_trace.o loadHistogramCells.o fitnessFunctionChainOperationsCells.o chainOperationsV2.o $(CLIBS) 
#	$(F90) $(F90FLAGS) -o testfitnessChainOperationsCells_trace testfitnessChainOperationsCells_trace.o loadHistogramCells.o fitnessFunctionChainOperationsCells.o chainOperationsV2.o

clean:
	rm -fr TRACE.sym TRACE.mpits set-0 testfitnessChainOperationsCells_trace.exe *.o *.mod 

.SUFFIXES: .o .f90

.f90.o : ; $(F90) -c $(F90FLAGS) $*.f90 -I.
#.f90.o : ; $(F90) -c $(F90FLAGS) $*.f90

