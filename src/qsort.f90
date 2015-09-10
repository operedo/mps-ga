RECURSIVE SUBROUTINE qsort(list, order,length)
! Quick sort routine from:
! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
! Modified by Alan Miller to include an associated integer array which gives
! the positions of the elements in the original order.

   IMPLICIT NONE
   INTEGER, INTENT(IN)                    :: length
   INTEGER, INTENT(INOUT)  :: list(length)
   INTEGER, INTENT(INOUT)  :: order(length)
   ! Local variable
   INTEGER :: i
   !print *,SIZE(list)
   DO i = 1, SIZE(list)
     order(i) = i
   END DO
   !print *,'llega aca?'
   CALL qsort_1(1, SIZE(list))

CONTAINS
   RECURSIVE SUBROUTINE qsort_1(left_end, right_end)
     INTEGER, INTENT(IN) :: left_end, right_end
     !     Local variables
     INTEGER             :: i, j, itemp
     INTEGER                :: reference, temp
     INTEGER, PARAMETER  :: max_simple_sort_size = 6
     IF (right_end < left_end + max_simple_sort_size) THEN
       ! Use interchange sort for small lists
       CALL interchange_sort(left_end, right_end)
     ELSE
       ! Use partition ("quick") sort
       reference = list((left_end + right_end)/2)
       i = left_end - 1; j = right_end + 1
       DO
         ! Scan list from left end until element >= reference is found
         DO
           i = i + 1
           IF (list(i) >= reference) EXIT
         END DO
         ! Scan list from right end until element <= reference is found
         DO
           j = j - 1
           IF (list(j) <= reference) EXIT
         END DO
         IF (i < j) THEN
           ! Swap two out-of-order elements
           temp = list(i); list(i) = list(j); list(j) = temp
           itemp = order(i); order(i) = order(j); order(j) = itemp
         ELSE IF (i == j) THEN
           i = i + 1
           EXIT
         ELSE
           EXIT
         END IF
       END DO
       IF (left_end < j) CALL qsort_1(left_end, j)
       IF (i < right_end) CALL qsort_1(i, right_end)
     END IF
   END SUBROUTINE qsort_1

   SUBROUTINE interchange_sort(left_end, right_end)
     INTEGER, INTENT(IN) :: left_end, right_end
     !     Local variables
     INTEGER             :: i, j, itemp
     INTEGER                :: temp
     DO i = left_end, right_end - 1
       DO j = i+1, right_end
         IF (list(i) > list(j)) THEN
           temp = list(i); list(i) = list(j); list(j) = temp
           itemp = order(i); order(i) = order(j); order(j) = itemp
         END IF
       END DO
     END DO
   END SUBROUTINE interchange_sort
END SUBROUTINE qsort


RECURSIVE SUBROUTINE qsortreal(list, order,length)
! Quick sort routine from:
! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
! Modified by Alan Miller to include an associated integer array which gives
! the positions of the elements in the original order.

   IMPLICIT NONE
   INTEGER, INTENT(IN)                    :: length
   real(8), INTENT(INOUT)  :: list(length)
   INTEGER, INTENT(INOUT)  :: order(length)
   ! Local variable
   INTEGER :: i
   !print *,SIZE(list)
   DO i = 1, SIZE(list)
     order(i) = i
   END DO
   !print *,'llega aca?'
   CALL qsortreal_1(1, SIZE(list))

CONTAINS
   RECURSIVE SUBROUTINE qsortreal_1(left_end, right_end)
     INTEGER, INTENT(IN) :: left_end, right_end
     !     Local variables
     INTEGER             :: i, j, itemp
     real(8)                :: reference, temp
     INTEGER, PARAMETER  :: max_simple_sort_size = 6
     IF (right_end < left_end + max_simple_sort_size) THEN
       ! Use interchange sort for small lists
       CALL interchange_sort_real(left_end, right_end)
     ELSE
       ! Use partition ("quick") sort
       reference = list((left_end + right_end)/2)
       i = left_end - 1; j = right_end + 1
       DO
         ! Scan list from left end until element >= reference is found
         DO
           i = i + 1
           IF (list(i) >= reference) EXIT
         END DO
         ! Scan list from right end until element <= reference is found
         DO
           j = j - 1
           IF (list(j) <= reference) EXIT
         END DO
         IF (i < j) THEN
           ! Swap two out-of-order elements
           temp = list(i); list(i) = list(j); list(j) = temp
           itemp = order(i); order(i) = order(j); order(j) = itemp
         ELSE IF (i == j) THEN
           i = i + 1
           EXIT
         ELSE
           EXIT
         END IF
       END DO
       IF (left_end < j) CALL qsortreal_1(left_end, j)
       IF (i < right_end) CALL qsortreal_1(i, right_end)
     END IF
   END SUBROUTINE qsortreal_1

   SUBROUTINE interchange_sort_real(left_end, right_end)
     INTEGER, INTENT(IN) :: left_end, right_end
     !     Local variables
     INTEGER             :: i, j, itemp
     real(8)                :: temp
     DO i = left_end, right_end - 1
       DO j = i+1, right_end
         IF (list(i) > list(j)) THEN
           temp = list(i); list(i) = list(j); list(j) = temp
           itemp = order(i); order(i) = order(j); order(j) = itemp
         END IF
       END DO
     END DO
   END SUBROUTINE interchange_sort_real
END SUBROUTINE qsortreal
