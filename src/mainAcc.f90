
program mainAcc

  use chainOperations

  use rng

  !use omp_lib

  implicit none
  
  ! debug=1 activates the writes,prints; debug=0 no printing
  integer(4),parameter       :: debug=0
  
  type individual
     integer(4), pointer    :: matrix(:,:)
     integer(4), pointer    :: histogram(:)
     integer(4)             :: fitness
  end type individual

  type individualOnlyMatrix
     integer(4), pointer    :: matrix(:,:)
  end type individualOnlyMatrix
   
  type(individual), pointer  :: population(:)
  type(individual)           :: trainingImage
  type(individualOnlyMatrix), pointer  :: parents(:)
  !type(individual)           :: child
  integer(4),pointer         :: fitnessarray(:)
  integer(4),pointer         :: indexesarray(:)
  ! Rows and columns of the image and template
  integer(4)                 :: rows, cols, ti_rows, ti_cols, tem_rows, tem_cols
  ! Number of population and number of generations
  integer(4)                 :: npopul, ngener, nparents, cut_points,indextmp,ncategories
  ! Probabilities for selection mutation and restart
  real(4)                    :: pro_selection, pro_mutation, pro_restart, pro_cut, num_conditionants
  real(4)                    :: npopulinv,initial_fitnessinv 
  integer(4)                 :: igener,ipopul, ipopulaux, irow, icol, tmp
  real(4)                    :: realrand,realrand2,randModval, randModval2
  integer(4)                 :: bitrand, pro_restart_int
  integer(4)                 :: indexrand
  integer(4), pointer        :: image(:)
  integer(4)                 :: indexParentA, indexParentB
  real(4),pointer            :: randarray(:)
  integer(4),pointer         :: conditionants(:,:)
  integer(4),pointer         :: tem_cells_rows(:), tem_cells_cols(:)
  real(4)                    :: average, timeIni, timeEnd
  integer(4)                 :: maximum, minimum, value, tem_cells, ii, id, num_threads
  character *50              :: inputFile,outputFile,dataFile, histoFile,beginFile
  character*100              :: buffer
  type(rng_t), pointer       :: rngval(:)
  integer(4)                 :: clock_start, clock_end, clock_rate, clock_main, clock_fit1
  integer(4)                 :: clock_fit2, clock_qsort, clock_print,clock_loadhistIni,clock_loadhistEnd
  integer(4)                 :: clock_restart, clock_store, clock_breed,clock_write
  real(4)                    :: elapsed_time
  real(4)                    :: initial_fitness, perc_fitness
  integer(4)                 :: entro_restart,begin_on
  integer(4),pointer         :: begin_matrix(:,:)


  !$OMP PARALLEL
  num_threads=1!OMP_get_num_threads() 
  !print *, 'omp_num_threads=',num_threads
  !print *, 'Hello World from thread', OMP_get_thread_num()
  !$OMP END PARALLEL

  call getarg(1,buffer)
  read(buffer,*) dataFile
  !print *,dataFile

  call getarg(2,buffer)
  read(buffer,*) outputFile
  !print *,outputFile

  call getarg(3,buffer)
  read(buffer,*) histoFile
  !print *,histoFile
  
  if(debug==1)then
  call system_clock(COUNT_RATE=clock_rate)
  call system_clock(COUNT=clock_start)
  call cpu_time(timeIni)
  print *, "timeIni=", timeIni
  end if

  ! Read the parameters used in the genetic algorithm
  open (unit = 13, file = "../data/params/"//dataFile)
  read (13,*) ti_rows, ti_cols, rows, cols, tem_rows, tem_cols, &
              ncategories, npopul, ngener, pro_selection, pro_mutation, &
              pro_restart, pro_cut, num_conditionants, begin_on
  read (13,*) inputFile
  !read (13,*) outputFile
  read (13,*) tem_cells
  if(tem_cells>0) then
     allocate(tem_cells_rows(tem_cells))
     allocate(tem_cells_cols(tem_cells))
     do ii=1,tem_cells
        read (13,*) tem_cells_rows(ii), tem_cells_cols(ii)
     end do
  end if
  if(begin_on>0) then
     allocate(begin_matrix(rows,cols))
     read(13,*) beginFile
     close(13)
     open (unit = 10, file = "../data/begin/"//beginFile)
     read (10,*) begin_matrix  
     write (*,'(110I2)') begin_matrix
     close(10)
  else
     close(13)
  end if

  ! Initialice the random seed
  call initRandomSeed()
  allocate(rngval(num_threads))
  do ii=1,num_threads
     call rng_seed(rngval(ii),932117 + ii)
  end do

  !print *, inputFile
  !print *, outputFile

  open(unit=66, file="../data/output/"//outputFile)

  nparents=int(floor(real(npopul)*pro_selection))
  cut_points=floor(rows*cols*pro_cut)
  ! Allocate the population and their respective matrices and arrays
  allocate(population(npopul))
  allocate(parents(nparents))
  allocate(fitnessarray(npopul))
  allocate(indexesarray(npopul))
  do ipopul = 1,npopul
       allocate(population(ipopul)%matrix(rows,cols))
       population(ipopul)%fitness = 10000
       if (ipopul<= nparents) then
          allocate(parents(ipopul)%matrix(rows,cols))
       end if
  end  do    
  allocate(trainingImage%matrix(ti_rows,ti_cols))
  
  allocate(randarray(cut_points*num_threads))
  if(num_conditionants>0) then
     allocate(conditionants(rows,cols))
  end if
  ! Read the training image
  open (unit = 13, file = "../data/input/"//inputFile)
  read (13,*) trainingImage%matrix 
  close(13)

  ! Calculate histogram for training image
  if(debug==1)call system_clock(COUNT=clock_loadhistIni)
  if (tem_cells<=0) then
     call loadHistogram(trainingImage%matrix,ti_rows,ti_cols,tem_rows,tem_cols,clock_rate)
  else
     call loadHistogramCells(trainingImage%matrix,ti_rows,ti_cols,tem_rows,tem_cols, tem_cells, tem_cells_rows, tem_cells_cols,clock_rate)
  end if 
  if(debug==1)then
  call system_clock(COUNT=clock_loadhistEnd)
  print *, "CLOCK time (load histogram)=",real((real(clock_loadhistEnd)-real(clock_loadhistIni))/real(clock_rate))
  end if

  !call printChainArray(tem_cells)

  ! Set the conditionants
  if (num_conditionants>0) then
     do icol=1,cols
        do irow=1,rows
           conditionants(irow,icol)=0
        end do
     end do
     call random_number(realrand2)
     irow=floor(realrand2*real(ti_rows))+1
     do icol=1,num_conditionants
        if (floor(real(icol)/real(cols)).NE.floor(real(icol-1)/real(cols))) then
           call random_number(realrand2)
           irow=floor(realrand2*real(ti_rows))+1
        end if
        call random_number(realrand)
        tmp=int(floor(realrand*real(ti_cols)))+1 
        conditionants(irow+(rows-ti_rows)/2,tmp+(cols-ti_cols)/2)=1
     end do
  end if
  if (num_conditionants>0) then
     write (66,*) 'Conditionants:'
     if (rows==110) then
        write (66,'(110I2)') conditionants
     end if
     if (rows==120) then
        write (66,'(120I2)') conditionants
     end if
     if (rows==100) then
        write (66,'(100I2)') conditionants
     end if
     if (rows==50) then
        write (66,'(50I2)') conditionants
     end if
     if (rows==10) then
        write (66,'(10I2)') conditionants 
     end if
  end if
        
  perc_fitness=100.0   

  ! Begin the genetic algorithm

  entro_restart=0
  npopulinv=1.0/real(npopul)

  do igener = 1,ngener
 
     if(debug==1)then 
     call system_clock(COUNT=clock_main)
     print *, "CLOCK time (enter main loop)=",real((real(clock_main)-real(clock_start))/real(clock_rate))
     end if

     if (igener == 1) then        
        do ipopul = 1,npopul
           ! Create the realizations
           if(begin_on>0 .and. ipopul==1) then
              do icol = 1,cols
                 do irow = 1,rows
                    population(ipopul)%matrix(irow,icol) = begin_matrix(irow,icol)   
                 end do
              end do
           elseif(begin_on==0 .and. ipopul>=1) then
              !if(mod(ipopul,100)==0)then
              !   call generateInitialPopulation(2,population(ipopul)%matrix,trainingImage%matrix,rows,cols,ti_rows,ti_cols,ncategories,num_conditionants,conditionants,rngval(1))
              !else
                 call generateInitialPopulation(1,population(ipopul)%matrix,trainingImage%matrix,rows,cols,ti_rows,ti_cols,ncategories,num_conditionants,conditionants,rngval(1))
              !end if
           endif
           !if (debug==1) then
           !   write (*,*) ""
           !   write (*,'(A,I3)') " Realization: ",ipopul
           !   write (*,'(100I1)')population(ipopul)%matrix
           !   write (*,*) ""
           !end if
           
           if(tem_cells<=0) then
              call fitnessFunctionChainOperations(population(ipopul)%matrix,rows,cols,tem_rows,tem_cols,value)
           else
              call fitnessFunctionChainOperationsCells(population(ipopul)%matrix,rows,cols,tem_rows,tem_cols,value, tem_cells, tem_cells_rows,tem_cells_cols,1,1)
           end if

           population(ipopul)%fitness=value
           fitnessarray(ipopul)=value
           indexesarray(ipopul)=ipopul
           !if (debug==1) then
           !   write (*,*) "Fitness: ",population(ipopul)%fitness
           !   write (*,*) ""
           !end if
        end do      


         
        if (debug==1) then
        call system_clock(COUNT=clock_fit1)
        print *, "CLOCK time (fitness calc first)=",real((real(clock_fit1)-real(clock_main))/real(clock_rate))
        end if

     end  if

     if (igener > 1) then
        !!$OMP PARALLEL PRIVATE(id,value) SHARED(rows,cols,tem_rows,tem_cols,tem_cells,tem_cells_rows,tem_cells_cols,population,fitnessarray,indexesarray)
        !id= OMP_get_thread_num() 
        !!$OMP DO SCHEDULE(dynamic,10)
        do ipopul = 1,npopul
           if(tem_cells<=0) then
              call fitnessFunctionChainOperations(population(ipopul)%matrix,rows,cols,tem_rows,tem_cols,value)
           else
              call fitnessFunctionChainOperationsCells(population(ipopul)%matrix,rows,cols,tem_rows,tem_cols,value, tem_cells, tem_cells_rows, tem_cells_cols,1,1)
           end if
           population(ipopul)%fitness=value
           fitnessarray(ipopul)=value
           indexesarray(ipopul)=ipopul
        end do                  
        !!$OMP END DO
        !!$OMP END PARALLEL
        
        if (debug==1) then
        call system_clock(COUNT=clock_fit2)
        print *, "CLOCK time (fitness calc)=",real((real(clock_fit2)-real(clock_main))/real(clock_rate))
        end if

     end if
     call qsort(fitnessarray,indexesarray,npopul)
    
     if (debug==1) then
     call system_clock(COUNT=clock_qsort)
     if(igener==1) then
        print *, "CLOCK time (qsort)=",real((real(clock_qsort)-real(clock_fit1))/real(clock_rate))
     else
        print *, "CLOCK time (qsort)=",real((real(clock_qsort)-real(clock_fit2))/real(clock_rate))
     end if
     end if 


     average=0.0
     !$OMP PARALLEL PRIVATE(ipopul) SHARED(population)  
     !$OMP DO REDUCTION(+:average)
     do ipopul=1,npopul
        average=average+real(population(ipopul)%fitness)
     end do
     !$OMP END DO
     !$OMP END PARALLEL
     average=average*npopulinv

     if(igener==1) then
        initial_fitness=real( population(indexesarray(1))%fitness ) 
        initial_fitnessinv=1/initial_fitness
     end if

     perc_fitness=(real(population(indexesarray(1))%fitness)*initial_fitnessinv)*100.0 

     write (*,*) 'gen:', igener, population(indexesarray(npopul))%fitness, average, population(indexesarray(1))%fitness, perc_fitness 
     
     !ATENCION: se comento rutina printHistogram para ver eficiencia
     !call printHistogram(population(indexesarray(1))%matrix,rows,cols,tem_rows,tem_cols, tem_cells, tem_cells_rows,tem_cells_cols, histoFile)



     if(debug==1)call system_clock(COUNT=clock_print)
     !print *, "CLOCK time (printHisto)=",real((real(clock_print)-real(clock_qsort))/real(clock_rate))


     !print *,'restart=',(real(population(indexesarray(npopul))%fitness) -  real(population(indexesarray(1))%fitness))/real(population(indexesarray(1))%fitness) 

     ! Restart the population
     !if(mod(igener,1000)==0) then
     if ((real(population(indexesarray(npopul))%fitness) -  real(population(indexesarray(1))%fitness))/real(population(indexesarray(1))%fitness) <= 0.001 ) then
        pro_restart_int=floor(pro_restart*npopul) 
        !!$OMP PARALLEL PRIVATE(id,realrand,realrand2,ipopulaux,bitrand) SHARED(rngval,pro_restart_int,population,indexesarray,rows,cols,num_conditionants)
        id= 0!OMP_get_thread_num() 
        id=id+1
        !!$OMP DO     
        do ipopul=1,pro_restart_int
           realrand2=rng_uniform(rngval(id))
           ipopulaux=floor(realrand2*real(npopul))+1  
           if( ipopulaux == indexesarray(1) ) then
              ipopulaux=ipopulaux+1
              if (ipopulaux>npopul) then
                 ipopulaux=ipopulaux-2
              end if
           end if
           do icol = 1,cols
              do irow = 1,rows
                 if (num_conditionants>0) then
                    if(conditionants(irow,icol)==0) then
                       realrand=rng_uniform(rngval(id))
                       bitrand= int(realrand*real(ncategories))
                       population(ipopulaux)%matrix(irow,icol) = bitrand
                    end if
                 else
                    realrand=rng_uniform(rngval(id))
                    bitrand= int(realrand*real(ncategories))
                    population(ipopulaux)%matrix(irow,icol) = bitrand    
                 end if
              end do
           end do 
        end do
        !!$OMP END DO
        !!$OMP END PARALLEL
     
        if(debug==1)then
        call system_clock(COUNT=clock_restart)
        print *, "CLOCK time (restart)=",real((real(clock_restart)-real(clock_print))/real(clock_rate))
        end if

        entro_restart=1

     end if

     ! Storage of parents
     !$OMP PARALLEL PRIVATE(id,indextmp,icol,irow) SHARED(nparents,rows,cols,parents,population)
     id= 0!OMP_get_thread_num() 
     id=id+1
     !$OMP DO
     do ipopul=1,nparents
        indextmp=indexesarray(ipopul)
        do icol = 1,cols
           do irow = 1,rows
              parents(ipopul)%matrix(irow,icol)=population(indextmp)%matrix(irow,icol)
           end do
        end do
     end do
     !$OMP END DO
     !$OMP END PARALLEL

     if(debug==1)then
     call system_clock(COUNT=clock_store)
     if(entro_restart==1) then
        print *, "CLOCK time (storage)=",real((real(clock_store)-real(clock_restart))/real(clock_rate))
        entro_restart=0
     else
        print *, "CLOCK time (storage)=",real((real(clock_store)-real(clock_print))/real(clock_rate))
     end if
     end if

     ! Copy best parent in the next generation
     do icol = 1,cols
        do irow = 1,rows
           population(1)%matrix(irow,icol)=parents(1)%matrix(irow,icol)
        end do
     end do
     
     ! Breeding (crossover)

     !!$OMP PARALLEL PRIVATE(id,realrand,indexParentA,indexParentB,   &
     !!$OMP            randModval,randModval2)              & 
     !!$OMP SHARED(num_threads,npopul,nparents,cut_points,            &
     !!$OMP            rows,cols,parents,population,num_conditionants,&
     !!$OMP            conditionants,pro_mutation)                    
     id= 0!OMP_get_thread_num() 
     id=id+1
     !!$OMP DO 
     do ipopul=2,npopul
        realrand=rng_uniform(rngval(id))
        indexParentA= floor(realrand*nparents)+1
        
        realrand=rng_uniform(rngval(id))
        indexParentB= floor(realrand*nparents)+1
        
        do irow=1,cut_points
           randarray(irow + (id-1)*cut_points)=rng_uniform(rngval(id))
        end do

        randModval=rng_uniform(rngval(id)) 
        randModval2=rng_uniform(rngval(id)) 
        
        call crossoverWithoutRandom(parents(indexParentA)%matrix,parents(indexParentB)%matrix,population(ipopul)%matrix,rows,cols,cut_points,randModval,randModval2,randarray((id*cut_points-cut_points+1):(id*cut_points)))

        randModval=rng_uniform(rngval(id)) 
        randModval2=rng_uniform(rngval(id)) 
        
        realrand=rng_uniform(rngval(id))
        bitrand= int(realrand*real(ncategories))
        
        if (num_conditionants>0) then
           call mutationConditionalWithoutRandom(population(ipopul)%matrix,rows,cols,pro_mutation,rows*cols,conditionants,randModval,randModval2,bitrand)
        else
           call mutationWithoutRandom(population(ipopul)%matrix,rows,cols,pro_mutation,rows*cols,randModval,randModval2,bitrand)
        end if
        
     end do
     !!$OMP END DO
     !!!$OMP BARRIER
     !!$OMP END PARALLEL
     
     if(debug==1)then
     call system_clock(COUNT=clock_breed)
     print *, "CLOCK time (breeding)=",real((real(clock_breed)-real(clock_store))/real(clock_rate))
     end if

     if(mod(igener,50)==0) then
        write (66,*) 'Generation=',igener
        if (rows==102) then
           write (66,'(102I2)') population(indexesarray(1))%matrix
        end if
        if (rows==104) then
           write (66,'(104I2)') population(indexesarray(1))%matrix
        end if
        if (rows==116) then
           write (66,'(116I2)') population(indexesarray(1))%matrix
        end if
        if (rows==109) then
           write (66,'(109I2)') population(indexesarray(1))%matrix
        end if
        if (rows==110) then
           write (66,'(110I2)') population(indexesarray(1))%matrix
        end if
        if (rows==120) then
           write (66,'(120I2)') population(indexesarray(1))%matrix
        end if
        if (rows==100) then
           write (66,'(100I2)') population(indexesarray(1))%matrix
        end if
        if (rows==50) then
           write (66,'(50I2)') population(indexesarray(1))%matrix
        end if
        if (rows==10) then
           write (66,'(10I2)') population(indexesarray(1))%matrix
        end if

        if(debug==1)then
        call system_clock(COUNT=clock_write)
        print *, "CLOCK time (write)=",real((real(clock_write)-real(clock_breed))/real(clock_rate))
        end if

     end if

  end do
  
  if(debug==1)then
  call system_clock(COUNT=clock_end)
  print *, "CLOCK time=",real((clock_end-clock_start)/clock_rate)
  call cpu_time(timeEnd)
  print *, "CPU time=", timeEnd-timeIni
  end if

  close(66)

  deallocate(randarray)
  deallocate(trainingImage%matrix)
  deallocate(indexesarray)
  deallocate(fitnessarray)
  deallocate(parents)
  deallocate(population)
  if(tem_cells>0) then
     deallocate(tem_cells_rows)
     deallocate(tem_cells_cols)
  end if
  if (num_conditionants>0) then
     deallocate(conditionants)
  end if
  deallocate(rngval)
  if(begin_on>0) then
     deallocate(begin_matrix)
  end if
end program mainAcc
