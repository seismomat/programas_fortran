program array_slice
    implicit none
  
    integer :: i
    integer :: array1(10)  ! 1D integer array of 10 elements
    integer :: array2(10, 10)  ! 2D integer array of 100 elements
    REAL :: x(5)
    real :: y(25)
    real :: z(5,5)
  
    array1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]  ! Array constructor
    array1 = [(i, i = 1, 10)]  ! Implied do loop constructor
    array1(:) = 0  ! Set all elements to zero
    array1(1:5) = 1  ! Set first five elements to one
    array1(6:) = 1  ! Set all elements after five to one
    print *, array1(1:10:2)  ! Print out elements at odd indices
    print *, array2(:,1)  ! Print out the first column in a 2D array
    print *, array1(10:1:-1)  ! Print an array in reverse
  
    x = [1, 2, 3, 4, 5]
    print *, NORM2(x)
    print *, x+x

    y= [(real(i), i = 1, 25)]
    z= reshape(y,(/5,5/))

    print *, shape(y)
    print *, shape(reshape(y,(/5,5/)))
    print *, size(reshape(y,(/5,5/)),1)
    print *, z(1,:)
    print *, z(:,1)
  
  end program array_slice