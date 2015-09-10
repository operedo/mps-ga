
subroutine generateInitialPopulation(mode,individualMatrix,trainingImageMatrix,cols,rows,ti_rows,ti_cols,ncategories,num_conditionants,conditionants,rngval)

   use rng

   implicit none

   integer(4),intent(in) :: mode
   integer(4),intent(in) :: rows,cols,ti_rows,ti_cols,ncategories,num_conditionants
   integer(4),intent(in) :: trainingImageMatrix(rows,cols),conditionants(rows,cols)
   integer(4),intent(inout) :: individualMatrix(rows,cols)
   integer(4) :: icol,irow,bitrand
   real(4) :: realrand  
   integer(4) :: nsquares,rowlength,collength,isquare,randrow,randcol
   type(rng_t)       :: rngval

   !mode=1 : pixel based generation with/without conditioning data
   if(mode==1)then

   do icol = 1,cols
      do irow = 1,rows
      call random_number(realrand)                 
         !bitrand= int(realrand*2.0)
         bitrand= int(realrand*real(ncategories))
         if (num_conditionants>0) then
            if (conditionants(irow,icol)==0) then
               individualMatrix(irow,icol) = bitrand    
            else
               !population(ipopul)%matrix(irow,icol) = trainingImage%matrix(irow,icol) 
               !if( (irow>(rows-ti_rows)/2 .or. irow<= rows-(rows-ti_rows)/2) .and. (icol>(cols-ti_cols)/2 .or. icol<= cols-(cols-ti_cols)/2) ) then
               individualMatrix(irow,icol) = trainingImageMatrix(irow-(rows-ti_rows)*0.5,icol-(cols-ti_cols)*0.5) 
               !else
               !   population(ipopul)%matrix(irow,icol) = bitrand    
               !endif
            end if
         else
            individualMatrix(irow,icol) = bitrand    
         end if
      end do
   end do

   !mode=2 : squares based generation with/without conditioning data
   elseif(mode==2)then
      
!      realrand=rng_uniform(rngval)!random_number(realrand)                 
!      nsquares=int(realrand*real(cols*rows*0.25))
!      do isquare=1,nsquares
!         realrand=rng_uniform(rngval)!random_number(realrand)                 
!         randrow=int(realrand*real(rows))+1
!         realrand=rng_uniform(rngval)!random_number(realrand)                 
!         randcol=int(realrand*real(cols))+1
!         realrand=rng_uniform(rngval)!random_number(realrand)                 
!         rowlength=int(min(int(realrand*real(rows))+randrow,rows))
!         realrand=rng_uniform(rngval)!random_number(realrand)                 
!         collength=int(min(int(realrand*real(cols))+randcol,cols))
!         realrand=rng_uniform(rngval)!random_number(realrand)                 
!         bitrand= int(realrand*real(ncategories))
!         do irow=randrow,rowlength
!            do icol=randcol,collength
!               individualMatrix(irow,icol)=bitrand
!            end do
!         end do
!      end do

      call random_number(realrand)                 
      nsquares=int(realrand*real(cols*rows*0.1))
      do isquare=1,nsquares
         call random_number(realrand)                 
         randrow=int(realrand*real(rows))+1
         call random_number(realrand)                 
         randcol=int(realrand*real(cols))+1
         call random_number(realrand)                 
         rowlength=int(min(int(realrand*real(rows*0.25))+randrow,rows))
         call random_number(realrand)                 
         collength=int(min(int(realrand*real(cols*0.25))+randcol,cols))
         call random_number(realrand)                 
         bitrand= int(realrand*real(ncategories))
         do irow=randrow,rowlength
            do icol=randcol,collength
               individualMatrix(irow,icol)=bitrand
            end do
         end do
      end do


   !mode=3 : 1-category generation with/without conditioning data
   elseif(mode==3)then

   call random_number(realrand)                 
   bitrand= int(realrand*real(ncategories))
   do icol = 1,cols
      do irow = 1,rows
         !bitrand= int(realrand*2.0)
         if (num_conditionants>0) then
            if (conditionants(irow,icol)==0) then
               individualMatrix(irow,icol) = bitrand    
            else
               !population(ipopul)%matrix(irow,icol) = trainingImage%matrix(irow,icol) 
               !if( (irow>(rows-ti_rows)/2 .or. irow<= rows-(rows-ti_rows)/2) .and. (icol>(cols-ti_cols)/2 .or. icol<= cols-(cols-ti_cols)/2) ) then
               individualMatrix(irow,icol) = trainingImageMatrix(irow-(rows-ti_rows)*0.5,icol-(cols-ti_cols)*0.5) 
               !else
               !   population(ipopul)%matrix(irow,icol) = bitrand    
               !endif
            end if
         else
            individualMatrix(irow,icol) = bitrand    
         end if
      end do
   end do

   !mode=4 : column-stripes generation with/without conditioning data
   elseif(mode==4)then

   do icol = 1,cols
      call random_number(realrand)                 
      bitrand= int(realrand*real(ncategories))
      do irow = 1,rows
         !bitrand= int(realrand*2.0)
         if (num_conditionants>0) then
            if (conditionants(irow,icol)==0) then
               individualMatrix(irow,icol) = bitrand    
            else
               !population(ipopul)%matrix(irow,icol) = trainingImage%matrix(irow,icol) 
               !if( (irow>(rows-ti_rows)/2 .or. irow<= rows-(rows-ti_rows)/2) .and. (icol>(cols-ti_cols)/2 .or. icol<= cols-(cols-ti_cols)/2) ) then
               individualMatrix(irow,icol) = trainingImageMatrix(irow-(rows-ti_rows)*0.5,icol-(cols-ti_cols)*0.5) 
               !else
               !   population(ipopul)%matrix(irow,icol) = bitrand    
               !endif
            end if
         else
            individualMatrix(irow,icol) = bitrand    
         end if
      end do
   end do

   !mode=5 : row-stripes generation with/without conditioning data
   elseif(mode==5)then

   do irow = 1,rows
      call random_number(realrand)                 
      bitrand= int(realrand*real(ncategories))
      do icol = 1,cols
         !bitrand= int(realrand*2.0)
         if (num_conditionants>0) then
            if (conditionants(irow,icol)==0) then
               individualMatrix(irow,icol) = bitrand    
            else
               !population(ipopul)%matrix(irow,icol) = trainingImage%matrix(irow,icol) 
               !if( (irow>(rows-ti_rows)/2 .or. irow<= rows-(rows-ti_rows)/2) .and. (icol>(cols-ti_cols)/2 .or. icol<= cols-(cols-ti_cols)/2) ) then
               individualMatrix(irow,icol) = trainingImageMatrix(irow-(rows-ti_rows)*0.5,icol-(cols-ti_cols)*0.5) 
               !else
               !   population(ipopul)%matrix(irow,icol) = bitrand    
               !endif
            end if
         else
            individualMatrix(irow,icol) = bitrand    
         end if
      end do
   end do


   end if

end subroutine generateInitialPopulation
