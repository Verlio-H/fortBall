module series
    use iso_fortran_env
    implicit none

    type Number
        class(*), allocatable :: subnode
    end type

    type Ratio
        real(real128) :: value
    end type

    type Inf
        logical :: sign
    end type 

    type Var
        integer :: argNumb
    end type

    type SeriesVarN
        integer :: depth
    end type

    type SeriesVarMaxN
        integer :: depth
    end type

    type Sum
        type(Number) :: lowerBound
        type(Number) :: Expr
        type(Number) :: NecessaryN !error bound rearranged so that the error bound is inserted and n is produced
    end type

    type Div
        type(Number) :: a
        type(Number) :: b
    end type 

    type Mult
        type(Number) :: a
        type(Number) :: b
    end type

    type Add
        type(Number) :: a
        type(Number) :: b
    end type

    type Pow
        type(Number) :: a
        integer :: b
    end type

    type Ball
        real(real128) :: val
        real(real128) :: epsilon
    end type

    interface operator(+)
        module procedure addition, addition_int_numb, addition_numb_int
    end interface

    interface operator(-)
        module procedure subtract, subtract_int_numb, subtract_numb_int
    end interface

    interface operator(/)
        module procedure divide, divide_int_numb, divide_numb_int
    end interface

    interface operator(*)
        module procedure multiply, multiply_int_numb, multiply_numb_int
    end interface

    interface operator(**)
        module procedure exponentiate
    end interface

    interface write(formatted)
        module procedure write
    end interface

    interface number
        module procedure intval
    end interface
contains
    pure elemental type(number) function arg(argNumb)
        integer, intent(in) :: argNumb
        arg%subnode = Var(argNumb)
    end function 

    pure elemental type(number) function intval(input)
        integer, intent(in) :: input
        intval%subnode = Ratio(input)
    end function

    pure elemental type(number) function exponentiate(a,b)
        type(number), intent(in) :: a
        integer, intent(in) :: b
        exponentiate%subnode = Pow(a,b)
    end function

    pure elemental type(number) function addition(a,b)
        type(number), intent(in) :: a
        type(number), intent(in) :: b
        addition%subnode = Add(a,b)
    end function

    pure elemental type(number) function addition_numb_int(a,b)
        type(number), intent(in) :: a
        integer, intent(in) :: b
        addition_numb_int%subnode = Add(a,intval(b))
    end function

    pure elemental type(number) function addition_int_numb(a,b)
        integer, intent(in) :: a
        type(number), intent(in) :: b
        addition_int_numb%subnode = Add(intval(a),b)
    end function

    pure elemental type(number) function subtract(a,b)
        type(number), intent(in) :: a
        type(number), intent(in) :: b
        subtract%subnode = Add(a,b*intval(-1))
    end function

    pure elemental type(number) function subtract_numb_int(a,b)
        type(number), intent(in) :: a
        integer, intent(in) :: b
        subtract_numb_int%subnode = Add(a,intval(-b))
    end function

    pure elemental type(number) function subtract_int_numb(a,b)
        integer, intent(in) :: a
        type(number), intent(in) :: b
        subtract_int_numb%subnode = Add(intval(a),b*intval(-1))
    end function

    pure elemental type(number) function divide(a,b)
        type(number), intent(in) :: a
        type(number), intent(in) :: b
        divide%subnode = Div(a,b)
    end function

    pure elemental type(number) function divide_numb_int(a,b)
        type(number), intent(in) :: a
        integer, intent(in) :: b
        divide_numb_int%subnode = Div(a,intval(b))
    end function

    pure elemental type(number) function divide_int_numb(a,b)
        integer, intent(in) :: a
        type(number), intent(in) :: b
        divide_int_numb%subnode = Div(intval(a),b)
    end function

    pure elemental type(number) function multiply(a,b)
        type(number), intent(in) :: a
        type(number), intent(in) :: b
        multiply%subnode = Mult(a,b)
    end function

    pure elemental type(number) function multiply_numb_int(a,b)
        type(number), intent(in) :: a
        integer, intent(in) :: b
        multiply_numb_int%subnode = Mult(a,intval(b))
    end function

    pure elemental type(number) function multiply_int_numb(a,b)
        integer, intent(in) :: a
        type(number), intent(in) :: b
        multiply_int_numb%subnode = Mult(intval(a),b)
    end function

    recursive type(number) function populate(input,args) result(result)
        type(number), intent(in) :: input
        type(number), intent(in) :: args(0:)

        select type (val=>input%subnode)
        type is (Var)
            if (val%argNumb<=size(args)) then
                result = args(val%argNumb)
            end if
        type is (Ratio)
            result%subnode = val
        type is (Add)
            result%subnode = Add(populate(val%a,args),populate(val%b,args))
        type is (Pow)
            result%subnode = Pow(populate(val%a,args),val%b)
        type is (Mult)
            result%subnode = Mult(populate(val%a,args),populate(val%b,args))
        type is (Div)
            result%subnode = Div(populate(val%a,args),populate(val%b,args))
        end select
    end function 

    recursive type(Ball) pure elemental function eval(input,mineps) result(result)
        type(number), intent(in) :: input
        real(real128), value :: mineps
        type(Ball) :: resultx, resulty
        real(real128) :: xmax, xmin, ymax, ymin, zmax, zmin, znom, ULP
        integer :: i
        select type (val=>input%subnode)
        type is (Var)
            error stop "variables cannot be within any evaluated numbers"
        type is (Ratio)
            result%val = val%value
            !result%epsilon = 2**real(exponent(result%val)-112,real128)/2
            result%epsilon = 0
        type is (Add)
            result%epsilon = 100000000
            mineps = mineps * 2
            i = 0
            do while (result%epsilon>mineps)
                if (i/=0) then
                    if (resultx%epsilon>mineps.and.resulty%epsilon>mineps) return
                end if
                mineps = mineps*0.5
                resultx = eval(val%a,mineps/2)
                resulty = eval(val%b,mineps/2)
                result%val = resultx%val+resulty%val
            ! ULP = 2^(exp-(pbits-1))
                ULP = 2**real(exponent(result%val)-112,real128)
            ! error = max(emax,emin) + 1/2 ULP
                result%epsilon = resultx%epsilon+resulty%epsilon+ULP/2
                i = i + 1
            end do
        type is (Pow)
            result%epsilon = 100000000
            i = 0
            do while (result%epsilon>mineps)
                if (i/=0) then
                    if (resultx%epsilon>mineps) return
                end if
                mineps = mineps*0.5
                resultx = eval(val%a,mineps)
                xmin = sign(abs(resultx%val)-resultx%epsilon,resultx%val)
                xmax = sign(abs(resultx%val)+resultx%epsilon,resultx%val)

                zmax = xmax**val%b
                zmin = xmin**val%b

                znom = resultx%val**val%b

                zmax = abs(zmax-znom)
                zmin = abs(zmin-znom)
                result%val = znom
            ! ULP = 2^(exp-(pbits-1))
                ULP = 2**real(exponent(result%val)-112,real128)
            ! error = max(emax,emin) + 1/2 ULP
                result%epsilon = max(zmax,zmin)+ULP/2
                i = i + 1
            end do
        type is (Mult)
            result%epsilon = 100000000
            mineps = mineps * 2
            i = 0
            do while (result%epsilon>mineps)
                if (i/=0) then
                    if (resultx%epsilon>mineps.and.resulty%epsilon>mineps) return
                end if
                mineps = mineps*0.5
                resultx = eval(val%a,mineps)
                xmin = sign(abs(resultx%val)-resultx%epsilon,resultx%val)
                xmax = sign(abs(resultx%val)+resultx%epsilon,resultx%val)

                resulty = eval(val%b,mineps)
                ymin = sign(abs(resulty%val)-resulty%epsilon,resulty%val)
                ymax = sign(abs(resulty%val)+resulty%epsilon,resulty%val)
            ! zmax = xmax*ymax
                zmax = xmax*ymax
            ! zmin = xmin*ymin
                zmin = xmin*ymin
            ! znominal = x*y
                znom = resultx%val*resulty%val
            ! emax = abs(zmax-znominal)
                zmax = abs(zmax-znom)
            ! emin = abs(zmin-znominal)
                zmin = abs(zmin-znom)
                result%val = znom
            ! ULP = 2^(exp-(pbits-1))
                ULP = 2**real(exponent(result%val)-112,real128)
            ! error = max(emax,emin) + 1/2 ULP
                result%epsilon = max(zmax,zmin)+ULP/2
                i = i + 1
            end do
        type is (Div)
            result%epsilon = 100000000
            mineps = mineps * 2
            i = 0
            do while (result%epsilon>mineps)
                if (i/=0) then
                    if (resultx%epsilon>mineps.and.resulty%epsilon>mineps) return
                end if
                mineps = mineps*0.5
                resultx = eval(val%a,mineps)
                xmin = sign(abs(resultx%val)-resultx%epsilon,resultx%val)
                xmax = sign(abs(resultx%val)+resultx%epsilon,resultx%val)

                resulty = eval(val%b,mineps)
                ymin = sign(abs(resulty%val)-resulty%epsilon,resulty%val)
                ymax = sign(abs(resulty%val)+resulty%epsilon,resulty%val)
            ! zmax = xmax/ymin
                zmax = xmax/ymin
            ! zmin = xmin/ymax
                zmin = xmin/ymax
            ! znominal = x/y
                znom = resultx%val/resulty%val
            ! emax = abs(zmax-znominal)
                zmax = abs(zmax-znom)
            ! emin = abs(zmin-znominal)
                zmin = abs(zmin-znom)
                result%val = znom
            ! ULP = 2^(exp-(pbits-1))
                ULP = 2**real(exponent(result%val)-112,real128)
            ! error = max(emax,emin) + 1/2 ULP
                result%epsilon = max(zmax,zmin)+ULP/2
                i = i + 1
            end do
        end select
    end function

    subroutine write(dtv,unit,iotype,v_list,iostat,iomsg)
        class(Ball), intent(in) :: dtv
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        iostat = 0
        if (size(v_list)/=0) then
            iostat = 1
            iomsg = iotype ! to silence the beast of gfortran
            iomsg = 'variable list not supported for this io type'
            return
        end if
        select type (dtv)
        type is (Ball)
            write(unit,'(ES0.0E0,A,ES0.0E0)') dtv%val,' +- ',dtv%epsilon
        end select
    end subroutine
end module