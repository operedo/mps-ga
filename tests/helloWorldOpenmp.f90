program HelloWorld 
integer NoofThreads, ThreadID, OMP_GET_NUM_THREADS 
integer OMP_GET_THREAD_NUM

!$OMP  PARALLEL PRIVATE(NoofThreads, ThreadID)

ThreadID = OMP_GET_THREAD_NUM() 
print *, 'Hello World from thread = ', ThreadID

if (ThreadID .EQ. 0) then 
        NoofThreads = OMP_GET_NUM_THREADS() 
        print *, 'Number of threads = ', NoofThreads 
 end if

!$OMP  END PARALLEL

stop 
end
