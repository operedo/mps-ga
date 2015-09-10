
!subroutine mutationWithoutRandom(indiv_array_matrix,rows,cols,perc_mutation,num_points,rand1,rand2,newcat,igener)
subroutine mutationWithoutRandom(indiv_array_matrix,rows,cols,perc_mutation,num_points,rand1,rand2,newcat)
   implicit none

!!   integer(4)                       :: size_indiv
   integer(4), intent(in)           :: num_points
   integer(4), intent(in)           :: rows,cols,newcat
   integer(4), intent(inout)        :: indiv_array_matrix(rows,cols)
!!   integer(4)                       :: indiv_array(rows*cols)
   real(4),    intent(in)           :: perc_mutation
   integer(4)                       :: ii,jj,irow,icol, randAllele, squaresize, dotsize, radius
   real(4), intent(in)              :: rand1,rand2
!   real(8), intent(in)              :: rand1,rand2
   real(4) :: rand3, limsup,liminf

   squaresize=int(real(rows)*0.1*0.5)
   dotsize=int(real(rows)*0.01)
   radius=int(real(rows)*0.02)
   call random_number(rand3)

   if (rand1 .LT. perc_mutation) then
   !if (rand1 .LT. 1.1) then
      randAllele= floor(real(rows*cols)*rand2) + 1

      icol = min( cols , max( 1 , int(real(randAllele)/real(rows)) ) )
      irow = min( rows , max( 1, mod(randAllele,rows)+1 ) )

!      if(rand3<=0.66)then
         ! mutation of a pixel
!         do jj=-dotsize,dotsize,1
!            do ii=-dotsize,dotsize,1
!               !indiv_array_matrix(irow,icol) = newcat
!               indiv_array_matrix(max(min(irow+ii,rows),1),max(min(icol+jj,cols),1)) = newcat
!            end do
!         end do
         do jj=icol-radius,icol+radius,1
            limsup=irow+int(sqrt(real(radius*radius-(jj-icol)*(jj-icol))))+1
            liminf=irow-int(sqrt(real(radius*radius-(jj-icol)*(jj-icol))))-1
            !print *,'limsup=',limsup,' liminf=',liminf
            do ii=liminf,limsup,1
               !indiv_array_matrix(irow,icol) = newcat
               indiv_array_matrix(max(min(ii,rows),1),max(min(jj,cols),1)) = newcat
            end do
         end do

!      elseif(rand3>0.66 .and. rand3<0.84)then
!         ! mutation of a 50x50 square
!         do jj=-squaresize,squaresize,1
!            do ii=-squaresize,squaresize,1
!               indiv_array_matrix(max(min(irow+ii,rows),1),max(min(icol+jj,cols),1)) = newcat
!            end do
!         end do
!      elseif(rand3>=0.84)then
!         ! mutation of a 50x50 square rotated 90-degrees
!         do ii=0,squaresize
!            do jj=-ii,ii
!               indiv_array_matrix(max(min(irow+ii-squaresize,rows),1),max(min(icol+jj-squaresize,cols),1)) = newcat
!            end do
!         end do
!         do ii=-squaresize,0
!            do jj=ii,-ii
!               indiv_array_matrix(max(min(irow+ii+squaresize,rows),1),max(min(icol+jj-squaresize,cols),1)) = newcat
!            end do
!         end do

!      end if
      
   end if

end subroutine mutationWithoutRandom 

subroutine mutationWithoutRandomObjects(indiv_array_matrix,rows,cols,perc_mutation,num_points,rand1,rand2,newcat)
   implicit none

!!   integer(4)                       :: size_indiv
   integer(4), intent(in)           :: num_points
   integer(4), intent(in)           :: rows,cols,newcat
   integer(4), intent(inout)        :: indiv_array_matrix(rows,cols)
!!   integer(4)                       :: indiv_array(rows*cols)
   real(4),    intent(in)           :: perc_mutation
   integer(4)                       :: ii,jj,irow,icol, randAllele,gener
   real(4), intent(in)              :: rand1,rand2
!   real(8), intent(in)              :: rand1,rand2

   gener=1
      !do ii=1,num_points
         !call random_number(rand1)
         if (rand1 .LT. perc_mutation) then
            !call random_number(rand2)     
            randAllele= floor(real(rows*cols)*rand2) + 1

            icol = min( cols , max( 1 , int(real(randAllele)/real(rows)) ) )
            irow = min( rows , max( 1, mod(randAllele,rows)+1 ) )

            !print *,'mutation=: irow=',irow,', icol=',icol

            !if(rand2<0.1)then
            if(gener>2000)then
               ! mutation of a pixel
               indiv_array_matrix(irow,icol) = newcat
            !elseif(rand2>=0.1 .and. rand2<0.55)then
            elseif(gener>=1000 .and. gener<2000)then
               ! mutation of a 10x10 square
               do ii=-5,5,1
                  do jj=-5,5,1
                     indiv_array_matrix(max(min(irow+ii,rows),1),max(min(icol+jj,cols),1)) = newcat
                  end do
               end do
            !elseif(rand2>0.55)then
            elseif(gener>0 .and. gener<=1000)then
               ! mutation of a 20x20 square rotated 90-degrees
               do ii=0,9
                  do jj=-ii,ii
                     indiv_array_matrix(max(min(irow+ii-5,rows),1),max(min(icol+jj,cols),1)) = newcat
                  end do
               end do
               do ii=-10,0
                  do jj=ii,-ii
                     indiv_array_matrix(max(min(irow+ii+5,rows),1),max(min(icol+jj,cols),1)) = newcat
                  end do
               end do

            end if
            
            !if ( indiv_array_matrix(irow,icol) == 0 ) then
            !   indiv_array_matrix(irow,icol) = 1
            !else
            !   indiv_array_matrix(irow,icol) = 0
            !end if
         end if
      !end do

end subroutine mutationWithoutRandomObjects 





!subroutine mutationAnnealingWithoutRandom(indiv_array_matrix,rows,cols,perc_mutation,num_points,rand1,rand2,newcat,oldfitness,newfitness)
!   implicit none
!
!!!   integer(4)                       :: size_indiv
!   integer(4), intent(in)           :: num_points
!   integer(4), intent(in)           :: rows,cols,newcat
!   integer(4), intent(inout)        :: indiv_array_matrix(rows,cols)
!!!   integer(4)                       :: indiv_array(rows*cols)
!   real(4),    intent(in)           :: perc_mutation
!   integer(4)                       :: ii,jj,irow,icol, randAllele,domutation
!   real(4), intent(in)              :: rand1,rand2,temp
!!   real(8), intent(in)              :: rand1,rand2
!
!   temp=pro_mutation*5000.0
!        if(oldfitness>=newfitness)then
!                domutation=1
!        else
!                if(exp(-real(newfitness-oldfitness)/(temp))>rand1) then
!                        domutation=2
!                else
!                        domutation=0
!                endif
!        endif
!
!         if (rand1 .LT. perc_mutation) then
!            !call random_number(rand2)     
!            randAllele= floor(real(rows*cols)*rand2) + 1
!
!            icol = min( cols , max( 1 , int(real(randAllele)/real(rows)) ) )
!            irow = min( rows , max( 1, mod(randAllele,rows)+1 ) )
!
!            !print *,'mutation=: irow=',irow,', icol=',icol
!
!            if(rand2<0.4)then
!               ! mutation of a pixel
!               indiv_array_matrix(irow,icol) = newcat
!            else
!               ! mutation of a 10x10 square
!               !do ii=-5,5,1
!               !   do jj=-5,5,1
!               !      indiv_array_matrix(max(min(irow+ii,rows),1),max(min(icol+jj,cols),1)) = newcat
!               !   end do
!               !end do
!               ! mutation of a 10x10 square rotated 90-degrees
!               do ii=0,4
!                  do jj=-ii,ii
!                     indiv_array_matrix(max(min(irow+ii-5,rows),1),max(min(icol+jj,cols),1)) = newcat
!                  end do
!               end do
!               do ii=-5,0
!                  do jj=ii,-ii
!                     indiv_array_matrix(max(min(irow+ii+5,rows),1),max(min(icol+jj,cols),1)) = newcat
!                  end do
!               end do
!
!            end if
!            
!            !if ( indiv_array_matrix(irow,icol) == 0 ) then
!            !   indiv_array_matrix(irow,icol) = 1
!            !else
!            !   indiv_array_matrix(irow,icol) = 0
!            !end if
!         end if
!      !end do
!
!end subroutine mutationWithoutRandom 
!
