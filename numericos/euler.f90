function f(x,v) result(eval)
    implicit none 
    double precision,intent(in):: x,v
    double precision:: g=9.81,l=0.5
    double precision:: eval

    eval= (-g/l)*sin(x)

end function

program Euler
implicit none 

double precision, parameter:: pi=4.d0*datan(1.d0)
double precision::x,v
double precision:: h=0.01
integer:: n=1000,t
double precision:: f

x=pi/5.0

v=0.0

open(unit=10,file='euler.dat',status='replace',action='write')
do t=1,n
    v=v+f(x,v)*h
    x=x+v*h 
    write(10,*) t*h,x,v
end do 
close(10)

end program 