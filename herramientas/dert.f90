program dert
!----------------------------------------------------!
!                 DECLARACIÓN DE VARIABLES
!----------------------------------------------------!
    implicit none
    double precision:: a=5000.0,t0=0.05,dt=0.005
    double precision:: tmax=0.1
    integer::i,n
    double precision,allocatable::t(:),p(:)
    double precision,allocatable::dpp(:),dpn(:)
    character(len=*),parameter:: name='derivadas.dat'

!----------------------------------------------------!
!                 Cuerpo del programa
!----------------------------------------------------!

    n=nint(tmax/dt) ! pasos en el tiempo
    allocate(t(n),p(n),dpn(n),dpp(n))

    t=[((i-1)*dt,i=1,n)]
    ! funcion original 
    p=exp(-a*(t-t0)**2)
    ! derivada analitica
    dpp=0.0
    dpp=-2.0*a*(1.0-2.0*a*(t-t0)**2)*p

    dpn=0.0
    do i=2,n-1
        ! derivada numerica
        dpn(i)=(p(i+1)+p(i-1)-2.0*p(i))/dt**2
    end do 

    open(unit=2,status='replace',action='write',file=name)
    do i=1,n
        write(2,*)t(i),dpp(i),dpn(i)
    end do 
    close(2)

    
    
end program dert