
subroutine mutationConditionalWithoutRandom(indiv_array_matrix,rows,cols,perc_mutation,num_points,conditionants, rand1, rand2,newcat)
   implicit none

   integer(4), intent(in)           :: num_points
   integer(4), intent(in)           :: rows,cols,newcat
   integer(4), intent(inout)        :: indiv_array_matrix(rows,cols)
   real(4),    intent(in)           :: perc_mutation
   integer(4), intent(in), optional :: conditionants(rows,cols)
   integer(4)                       :: ii,jj,irow,icol,randAllele,squaresize,radius
   real(4), intent(in)              :: rand1,rand2
   integer(4)                       :: isConditionant,prevValue
   real(4) :: rand3, limsup,liminf

   squaresize=int(real(rows)*0.1*0.5)
   radius=int(real(rows)*0.02)
   call random_number(rand3)

   if (rand1 .LT. perc_mutation) then
      isConditionant=1
      randAllele= floor(real(rows*cols)*rand2) + 1

      icol = min( cols , max( 1 , int(real(randAllele)/real(rows)) ) )
      irow = min( rows , max( 1, mod(randAllele,rows)+1 ) )

      if (conditionants(irow,icol)==0) then
         isConditionant=0
      else
         prevValue=indiv_array_matrix(irow,icol)
      end if

         do jj=icol-radius,icol+radius,1
            limsup=irow+int(sqrt(real(radius*radius-(jj-icol)*(jj-icol))))+1
            liminf=irow-int(sqrt(real(radius*radius-(jj-icol)*(jj-icol))))-1
            !print *,'limsup=',limsup,' liminf=',liminf
            do ii=liminf,limsup,1
               !indiv_array_matrix(irow,icol) = newcat
               indiv_array_matrix(max(min(ii,rows),1),max(min(jj,cols),1)) = newcat
            end do
         end do

      
      !if(isConditionant==0) then
      !   indiv_array_matrix(irow,icol) = newcat
      !end if

!      if(rand3<=0.33)then
!      !if(perc_mutation<=0.25)then
!      !if(igener>2000)then
!         ! mutation of a pixel
!         indiv_array_matrix(irow,icol) = newcat
!      elseif(rand3>0.33 .and. rand3<0.66)then
!      !elseif(perc_mutation>0.25 .and. perc_mutation<0.5)then
!      !elseif(igener>=1000 .and. igener<2000)then
!         ! mutation of a 50x50 square
!         do ii=-squaresize,squaresize,1
!            do jj=-squaresize,squaresize,1
!               indiv_array_matrix(max(min(irow+ii,rows),1),max(min(icol+jj,cols),1)) = newcat
!            end do
!         end do
!      elseif(rand3>=0.66)then
!      !elseif(perc_mutation>=0.5)then
!      !elseif(igener>0 .and. igener<=1000)then
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

      if(isConditionant==1) then
         indiv_array_matrix(irow,icol) = prevValue
      end if

   end if
end subroutine mutationConditionalWithoutRandom  


