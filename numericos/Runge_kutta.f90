function f(x,v) result(eval)
    implicit none 
    double precision::x,v
    double precision,parameter:: g=9.81,l=0.5,beta=0.6
    double precision:: eval

    eval= (-g/l)*sin(x)-beta*v

end function 

program Runge_kutta
    ! runge kutta de tres puntos
    implicit none
    double precision:: x,v
    double precision,parameter:: h=0.01,pi=4.d0*datan(1.d0)
    integer::t,n=1000
    double precision:: k1,k2,k3,l1,l2,l3
    double precision::f

    x=pi/5.0;v=0.0
    k1=0.0;k2=0.0;k3=0.0;l1=0.0;l2=0.0;l3=0.0

    open(unit=2,file='runge.dat',status='replace',action='write')
    do t=1,n
        k1=h*v
        l1=h*f(x,v)
        k2=h*(v+l1/2.0)
        l2=h*f(x+k1/2.0,v+l1/2.0)
        k3=h*(v-l1+2.0*l2)
        l3=h*f(x-k1+2.0*k2,v-l1+2.0*l2)

        v=v+(l1+4.0*l2+l3)/6.0
        x=x+(k1+4.0*k2+k3)/6.0
        write(2,*)h*t,x,v
    end do 
    close(2)

end program Runge_kutta