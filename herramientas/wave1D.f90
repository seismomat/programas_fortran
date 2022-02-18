program wave1D
    use, intrinsic :: omp_lib
    integer,parameter:: nd=100,nst=500
    double precision:: p(nd),p1(nd),c(nd),s(nst)
    double precision,parameter:: pi=4.d0*datan(1.d0)
    double precision::dx,c0
    integer::nx,jr
    double precision::dt,tmax
    integer::nstep
    double precision::Kc,a,x0,d
    double precision::x(nd)
    integer::j,n
    double precision::pp(nd)
    double precision::pa,xr,sa,t,CFL
    ! modelo geologico
    dx=20.0;c0=2000.0;nx=100
    ! se inicia la velocidad
    c=c0
    print*, 'Kmax',0.5/dx

    ! localizacion del hidrofono
    jr=36

    ! Courant-Friedrichs Lewy 
    dt=0.001; tmax=0.3; nstep=nint(tmax/dt)
    CFL=(dt*c0)/dx
    print*,'dt',dt
    print*,'CFL',CFl
    
    !condiciones iniciales
    Kc=0.006
    a=(Kc/0.68)**2
    x0=(dx*nx)/2
    d=c0*dt
    x=[( (j-1)*dx ,j=1,nx)]

    ! condiciones iniciales

    p=exp(-a*(x-x0)**2) ! en tiempo cero
    ! en tiempo dt
    p1=0.5*(exp(-a*(x-x0-d)**2) +exp(-a*(x-x0+d)**2))

    call omp_set_num_threads(4)
    !!!!! evolucion temporal 
    do n = 1, nstep
        !$omp parallel private(j) shared(CFL,dx,dt,c,p,p1)
        print*, 'tiempo',n, 'Thread: ', omp_get_thread_num()
        !$omp do
        do j = 2, nx-1
            CFL=dt*c(j)/dx
            p1(j)=-p1(j)+2.0*p(j)+CFL*CFL*(p(j+1)+p(j-1)-2.0*p(j))
        end do

        !$omp end do 
        !$omp barrier
        !$omp end parallel
        pp=p1
        ! present -->past
        p1=p
        ! future --> present
        p=pp

        !! leyendo sismograma
        s(n)=p(jr)
        
    end do


    !!!!!! presiones y solucion analitica
    d=c0*tmax

    !!! validacion con la solucion analitica
    open(unit=10,status='replace',action='write',file='wave1D.dat')
    do j = 1, nx
        ! solucion analitica
        pa=0.5*(exp(-a*(x(j)-x0-d)**2) +exp(-a*(x(j)-x0+d)**2))
        write(10,*)x(j),pa,p(j)
    end do
    close(10)

    !!! sismograma
    xr=(jr-1.0)*dx
    open(unit=11,status='replace',action='write',file='seis.dat')
    do n = 1, nstep
        t=(n-1)*dt
        d=c0*t
        sa=0.5*(exp(-a*(xr-x0-d)**2) +exp(-a*(xr-x0+d)**2))
        write(11,*)t,sa,s(n)
    end do
    close(11)

    





end program wave1D