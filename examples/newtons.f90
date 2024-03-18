!program to calculate the zeros of a function using newtons method
!current configured to calculate the golden ratio
program newtons
    use series
    implicit none
    real(real128) :: guess
    type(ball) :: result
    type(number) :: f, an, err

    type(number) :: x
    type(expandedint) :: n
    x = arg(1)
    n = eintsum(0)



    ! function goes here
    f = x**2-x-1
    ! initial guess goes here
    guess = 1

    ! a_n = -f(a_(n-1))/f'(a_(n-1))
    ! e_n = f''(a_(n-1))/(2f'(a_+(n-1)))*e_(n-1)
    an = 0-populate(f,[lastcalc(0)])/populate(diff(f,1),[lastcalc(0)])
    err = populate(diff(diff(f,1),1),[lastcalc(0)])/populate(2*diff(f,1),[lastcalc(0)])*lasterr(0)


    result = eval(sum(eint(0),infinity,an,err,minn=0,startval=guess,starterr=1._real128),0.000000001_real128)

    print*,result
    print'(A)','centered at:'
    print'(F0.38)',result%val
    print'(A)','interval:'
    print'(F0.38)',result%val-result%epsilon
    print'(F0.38)',result%val+result%epsilon
end program