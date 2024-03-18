! example code to calculate pi

program pi
    use series
    implicit none
    ! a ball is simply a floating point value with an error bound
    type(ball) :: result
    ! a number is a tree that represents a number constructed out of the basic elements
    type(number) :: atanx, an, err, pi4

    type(number) :: x
    ! an expanded int is an int that can also be infinity or a summation index
    type(expandedint) :: n

    ! defining variables
    ! x is the first argument, in this library one constructs trees and those trees can be functions
    ! when those functions are filled, any spot containing x will be populated with the first argument
    x = arg(1)
    ! n is the summation index of the sum with 0 depth upwards
    n = eintsum(0)

    ! an = (-1)**n * x**(2n+1) / (2n+1)
    ! an here is the part on the inside of the sum (this is the maclaurin series for arctan(x))
    ! must use expandedints or ints in exponents, expandedints cannot be multiplied hence (x**n)**2*x instead of x**(2n+1)
    ! numb() converts ints or expandedints into numbers
    an = numb(-1)**n * (x**n)**2*x / (2*numb(n)+1)
    ! err = x**(2n+2) / (2n+2)
    ! err here is the error bound for the sum, in this case the alternating series error bound
    ! same logic as before with the exponents
    err = (x**(n+1))**2 / (2*numb(n)+2)

    ! the sum from 0 to infinity of an, with an error bound of err
    ! this will only work from -1 to 1 due to the radius of convergence of the maclaurin series
    atanx = sum(eint(0),infinity,an,err)

    ! π/4=2*atan(1/3)+atan(1/7) according to wikipedia
    ! tried calculating atan(1) directly but convergence slow cause on the very edge
    ! populate function is effectively a function call, replacing x with the value (note that nothing is actually calculated yet)
    pi4 = 2*populate(atanx,[numb(1)/3])+populate(atanx,[numb(1)/7])

    ! when a number is printed, it will print the tree representing it
    print*,pi4

    ! eval actually converts the tree (4*pi4 here) into a ball
    ! the second argument to eval is the maximum error bound, eval will continue to evaluate until the error bound is low enough
    ! if the min error is set any lower then the floating point gods deem the task impossible
    result = eval(4*pi4,0.00000000000000000000000000000041_real128)

    ! when a ball is printed, it will print the center and the error bound
    print*,result

    ! you can also directly find the center and the interval that the ball falls within
    print'(A)','centered at:'
    print'(F0.38)',result%val
    print'(A)','interval:'
    print'(F0.38)',result%val-result%epsilon
    print'(F0.38)',result%val+result%epsilon
end program