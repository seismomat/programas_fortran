program use_mod
    use my_mod
    implicit none
  
    real :: mat(10, 10)
    real :: mat2(10, 10)
  
    mat(:,:) = public_var
  
    call print_matrix(mat)

    mat2=sum_matrix(mat)
    print*, mat2

  
  end program use_mod