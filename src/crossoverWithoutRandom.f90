subroutine crossoverWithoutRandom(indivA_matrix,indivB_matrix, indivAcrossed_matrix,rows, cols,size_cut_points, randModval, randModval2, randCutPoints)
   implicit none
   
   integer(4), intent(in)           :: rows, cols
   integer(4), intent(in)           :: indivA_matrix(rows,cols)
   integer(4), intent(in)           :: indivB_matrix(rows,cols)
   integer(4), intent(inout)        :: indivAcrossed_matrix(rows,cols)
   integer(4), intent(in)           :: size_cut_points

   real(4), intent(in)              :: randModval, randModval2
   real(4), intent(in)              :: randCutPoints(size_cut_points) 


   integer(4)                       :: size_indiv
   integer(4),pointer               :: indivA(:)
   integer(4),pointer               :: indivB(:)
   integer(4),pointer               :: indivAcrossed(:)
!   integer(4), intent(inout)        :: indivBcrossed(size_indiv)
!   real(8)              :: rand_cut_points(size_cut_points)
   integer(4),pointer               :: points(:)
   integer(4),pointer               :: points_index(:)
   integer(4),pointer               :: cutA(:) , cutB(:)
   integer(4)                       :: ii, jj, kk, modval, modval2
   real(4)                          :: rand


   size_indiv=rows*cols
   allocate(indivA(size_indiv))
   allocate(indivB(size_indiv))
   allocate(indivAcrossed(size_indiv))
   
   !call random_number(rand)
   modval2=int(randModval2*2.0)

   if(modval2==0) then
      !!$OMP PARALLEL 
      !!$OMP DO PRIVATE(jj,ii)
      !! SHARED(cols,rows,indivA,indivB,indivA_matrix,indivB_matrix)
      do jj=1,cols
         do ii=1,rows
            indivA(ii+(jj-1)*rows)=indivA_matrix(ii,jj)
            indivB(ii+(jj-1)*rows)=indivB_matrix(ii,jj)
         end do
      end do
      !!$OMP END DO
      !!$OMP END PARALLEL
   else
      !!$OMP PARALLEL 
      !!$OMP DO PRIVATE(jj,ii)
      !! SHARED(cols,rows,indivA,indivB,indivA_matrix,indivB_matrix)
      do jj=1,cols
         do ii=1,rows
            indivA(jj+(ii-1)*cols)=indivA_matrix(ii,jj)
            indivB(jj+(ii-1)*cols)=indivB_matrix(ii,jj)
         end do
      end do
      !!$OMP END DO
      !!$OMP END PARALLEL
   end if

   !call random_number(rand)
   modval=int(randModval*2.0)
   

   allocate(points(size_cut_points+2))
   allocate(points_index(size_cut_points+2))

   do ii = 2,size_cut_points+1
      !call random_number(rand)
      points(ii) = int(floor(real(size_indiv)*randCutPoints(ii-1))+1)      
   end do
   points(1)=1
   points(size_cut_points+2)=size_indiv
   
   call qsort(points,points_index,size_cut_points+2)

   !!$OMP PARALLEL 
   !!$OMP DO PRIVATE(ii)
   !! SHARED(size_indiv,indivAcrossed,indivBcrossed,indivA,indivB)
   !do ii=1,size_indiv
   !   indivAcrossed(ii)=indivA(ii)
!      indivBcrossed(ii)=indivB(ii)
   !end do
   !!$OMP END DO
   !!$OMP END PARALLEL

   indivAcrossed(1:size_indiv)=indivA(1:size_indiv)


   do ii=1, size(points)-1
      if (points(ii)<points(ii+1)) then
         if (ii==1) then
            allocate( cutA(points(ii+1)-points(ii)+1) )
            allocate( cutB(points(ii+1)-points(ii)+1) )
            do jj=1, size(cutA)
               cutA(jj)=indivA(points(ii)+jj-1)
               cutB(jj)=indivB(points(ii)+jj-1)
            end do
            
            if (mod(ii,2)==modval) then
               do jj=1,size(cutA)
                  indivAcrossed(points(ii)+jj-1)=cutB(jj)
!                  indivBcrossed(points(ii)+jj-1)=cutA(jj)
               end do
            end if
            deallocate(cutA)   
            deallocate(cutB)   
         else
            if(points(ii)+1<points(ii+1)) then
               allocate( cutA(points(ii+1)-points(ii)) )
               allocate( cutB(points(ii+1)-points(ii)) )
               do jj=1, size(cutA)
                  cutA(jj)=indivA(points(ii)+jj)
                  cutB(jj)=indivB(points(ii)+jj)
               end do
            
               if (mod(ii,2)==modval) then
                  do jj=1,size(cutA)
                     indivAcrossed(points(ii)+jj)=cutB(jj)
!                     indivBcrossed(points(ii)+jj)=cutA(jj)
                  end do
               end if
               deallocate(cutA)   
               deallocate(cutB) 
            end if
         end if
      end if
   end do

   if(modval2==0) then
      !!$OMP PARALLEL 
      !!$OMP DO PRIVATE(jj,ii)
      !! SHARED(cols,rows,indivAcrossed_matrix,indivAcrossed)
      do jj=1,cols
         do ii=1,rows
            indivAcrossed_matrix(ii,jj)=indivAcrossed(ii+(jj-1)*rows) 
         end do
      end do
      !!$OMP END DO
      !!$OMP END PARALLEL
   else
      !!$OMP PARALLEL 
      !!$OMP DO PRIVATE(jj,ii)
      !! SHARED(cols,rows,indivAcrossed_matrix,indivAcrossed)
      do jj=1,cols
         do ii=1,rows
            indivAcrossed_matrix(ii,jj)=indivAcrossed(jj+(ii-1)*cols) 
         end do
      end do
      !!$OMP END DO
      !!$OMP END PARALLEL
   end if



   deallocate(points_index)
   deallocate(points)
   deallocate(indivA)
   deallocate(indivB)
   deallocate(indivAcrossed)

end subroutine crossoverWithoutRandom  
