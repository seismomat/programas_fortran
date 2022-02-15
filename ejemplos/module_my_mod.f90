module my_mod
    implicit none
  
    private  ! All entities are now module-private by default
    public public_var, print_matrix  ! Explicitly export public entities
    public sum_matrix
    
    real, parameter :: public_var = 2
    integer :: private_var
  
  contains
      
    ! Print matrix A to screen
    subroutine print_matrix(A)
      real, intent(in) :: A(:,:)  ! An assumed-shape dummy argument
  
      integer :: i
  
      do i = 1, size(A,1)
        print *, A(i,:)
      end do
  
    end subroutine print_matrix

    function sum_matrix(A) result(B)
        implicit none 
        real:: A(:,:) 
        real,allocatable :: B(:,:)


        allocate(B(size(A,1),size(A,2)))
        B=A+A
    end function sum_matrix
  
  end module my_mod