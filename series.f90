module series
    use iso_fortran_env
    implicit none


    integer, parameter :: TYPE_INT = 0
    integer, parameter :: TYPE_ADD = 1
    integer, parameter :: TYPE_MLT = 2
    integer, parameter :: TYPE_DIV = 3
    integer, parameter :: TYPE_POW = 4
    integer, parameter :: TYPE_FACT = 5
    integer, parameter :: TYPE_SUM = 6
    integer, parameter :: TYPE_VAR = 7

    integer, parameter :: INT_VAL = 0
    integer, parameter :: INT_N = 1
    integer, parameter :: INT_INF = 2

    integer, parameter :: MAX_SUM_DEPTH = 1024
    integer :: sumstack(MAX_SUM_DEPTH)
    integer :: sumptr = 0

    type expandedint
        integer :: type
        integer :: val
        integer :: offset = 0
    end type

    type(expandedint), parameter :: infinity = expandedint(INT_INF,0)

    type Number
        integer :: type
        type(Number), allocatable :: a
        type(Number), allocatable :: b
        type(expandedint) :: numb1 = expandedint(INT_VAL,0)
        type(expandedint) :: numb2 = expandedint(INT_VAL,0)
        integer :: minn = 0
    end type

    type Ball
        real(real128) :: val
        real(real128) :: epsilon
    end type

    interface operator(+)
        module procedure addition, addition_int_numb, addition_numb_int, eintadd
    end interface

    interface operator(-)
        module procedure subtract, subtract_int_numb, subtract_numb_int, eintsub
    end interface

    interface operator(/)
        module procedure divide, divide_int_numb, divide_numb_int
    end interface

    interface operator(*)
        module procedure multiply, multiply_int_numb, multiply_numb_int
    end interface

    interface operator(**)
        module procedure exponentiate, expanded_exponentiate
    end interface

    interface write(formatted)
        module procedure write, write_number
    end interface

    interface numb
        module procedure intval, eintval
    end interface
contains

    pure elemental type(expandedint) function eintadd(a,b) result(c)
        type(expandedint), intent(in) :: a
        integer, intent(in) :: b
        select case (a%type)
        case (INT_VAL)
            c%type = INT_VAL
            c%val = a%val+b
        case (INT_N)
            c%type = INT_N
            c%val = a%val
            c%offset = a%offset+b
        case (INT_INF)
            c = a
        end select
    end function

    pure elemental type(expandedint) function eintsub(a,b) result(c)
        type(expandedint), intent(in) :: a
        integer, intent(in) :: b
        c = eintadd(a,-b)
    end function

    pure elemental type(number) function arg(argNumb,derivative)
        integer, intent(in) :: argNumb
        integer, intent(in), optional :: derivative
        arg%type = TYPE_VAR
        arg%numb1 = expandedint(INT_VAL,argNumb)
        arg%numb2 = expandedint(INT_VAL,0)
        if (present(derivative)) arg%numb2 = expandedint(INT_VAL,derivative)
    end function 

    pure elemental integer function collapseInt(input) result(j)
        type(expandedint), intent(in) :: input
        select case (input%type)
        case (INT_VAL)
            j = input%val
        case (INT_N)
            if (input%val>=sumptr) error stop "sum depth not great enough"
            j = sumstack(sumptr-input%val)+input%offset
        case (INT_INF)
            j = huge(j)
        end select
    end function

    pure elemental type(number) function sum(low,high,expr,error,minn)
        type(expandedint), intent(in) :: low
        type(expandedint), intent(in) :: high
        type(number), intent(in) :: expr
        type(number), intent(in) :: error
        integer, intent(in), optional :: minn
        sum%type = TYPE_SUM
        sum%numb1 = low
        sum%numb2 = high
        sum%a = expr
        sum%b = error
        if (present(minn)) sum%minn = minn
    end function

    pure elemental type(expandedint) function eint(input)
        integer, intent(in) :: input
        eint = expandedint(INT_VAL,input)
    end function

    pure elemental type(expandedint) function eintsum(input)
        integer, intent(in) :: input
        eintsum = expandedint(INT_N,input)
    end function

    pure elemental type(number) function intval(input)
        integer, intent(in) :: input
        intval%type = TYPE_INT
        intval%numb1 = expandedint(INT_VAL,input)
    end function

    pure elemental type(number) function eintval(input)
        type(expandedint), intent(in) :: input
        eintval%type = TYPE_INT
        eintval%numb1 = input
    end function

    pure elemental type(number) function exponentiate(a,b)
        type(number), intent(in) :: a
        integer, intent(in) :: b
        exponentiate = number(TYPE_POW)
        exponentiate%type = TYPE_POW
        exponentiate%a = a
        exponentiate%numb1 = expandedint(INT_VAL,b)
    end function

    pure elemental type(number) function expanded_exponentiate(a,b)
        type(number), intent(in) :: a
        type(expandedint), intent(in) :: b
        expanded_exponentiate = number(TYPE_POW)
        expanded_exponentiate%type = TYPE_POW
        expanded_exponentiate%a = a
        expanded_exponentiate%numb1 = b
    end function

    pure elemental type(number) function addition(a,b)
        type(number), intent(in) :: a
        type(number), intent(in) :: b
        addition%type = TYPE_ADD
        addition%a = a
        addition%b = b
    end function

    pure elemental type(number) function addition_numb_int(a,b)
        type(number), intent(in) :: a
        integer, intent(in) :: b
        addition_numb_int = addition(a,intval(b))
    end function

    pure elemental type(number) function addition_int_numb(a,b)
        integer, intent(in) :: a
        type(number), intent(in) :: b
        addition_int_numb = addition(intval(a),b)
    end function

    pure elemental type(number) function subtract(a,b)
        type(number), intent(in) :: a
        type(number), intent(in) :: b
        subtract%type = TYPE_ADD
        subtract%a = a
        subtract%b = intval(-1)*b
    end function

    pure elemental type(number) function subtract_numb_int(a,b)
        type(number), intent(in) :: a
        integer, intent(in) :: b
        subtract_numb_int = subtract(a,intval(b))
    end function

    pure elemental type(number) function subtract_int_numb(a,b)
        integer, intent(in) :: a
        type(number), intent(in) :: b
        subtract_int_numb = subtract(intval(a),b)
    end function

    pure elemental type(number) function divide(a,b)
        type(number), intent(in) :: a
        type(number), intent(in) :: b
        divide%type = TYPE_DIV
        divide%a = a
        divide%b = b
    end function

    pure elemental type(number) function divide_numb_int(a,b)
        type(number), intent(in) :: a
        integer, intent(in) :: b
        divide_numb_int = a/intval(b)
    end function

    pure elemental type(number) function divide_int_numb(a,b)
        integer, intent(in) :: a
        type(number), intent(in) :: b
        divide_int_numb = intval(a)/b
    end function

    pure elemental type(number) function multiply(a,b)
        type(number), intent(in) :: a
        type(number), intent(in) :: b
        multiply%type = TYPE_MLT
        multiply%a = a
        multiply%b = b
        multiply%numb1 = expandedint(INT_VAL,0)
        multiply%numb2 = expandedint(INT_VAL,0)
    end function

    pure elemental type(number) function multiply_numb_int(a,b)
        type(number), intent(in) :: a
        integer, intent(in) :: b
        multiply_numb_int = multiply(a,intval(b))
    end function

    pure elemental type(number) function multiply_int_numb(a,b)
        integer, intent(in) :: a
        type(number), intent(in) :: b
        multiply_int_numb = multiply(intval(a),b)
    end function

    recursive function populate(input,args) result(result)
        type(number), intent(in) :: input
        type(number), intent(in) :: args(:)
        type(number) :: result
        integer :: i
        select case (input%type)
        case (TYPE_VAR)
            if (input%numb1%val<=size(args)) then
                result = args(input%numb1%val)
                do i=1,input%numb2%val
                    result = diff(result,-1)
                end do
            end if
        case (TYPE_INT)
            result = input
        case (TYPE_ADD)
            result = populate(input%a,args)+populate(input%b,args)
        case (TYPE_POW)
            result = populate(input%a,args)**input%numb1
        case (TYPE_MLT)
            result = populate(input%a,args)*populate(input%b,args)
        case (TYPE_DIV)
            result = populate(input%a,args)/populate(input%b,args)
        case (TYPE_SUM)
            result = sum(input%numb1,input%numb2,populate(input%a,args),populate(input%b,args),input%minn)
        case default
            error stop
        end select
    end function 

    recursive type(number) function diff(input, argument) result(result)
        type(number), intent(in) :: input
        integer, intent(in) :: argument
        type(number) :: temp1, temp2
        logical :: ltemp1, ltemp2
        select case (input%type)
        case (TYPE_VAR)
            if (input%numb1%val==argument) then
                if (input%numb2%val==0) then
                    result = intval(1)
                else
                    result = intval(0)
                end if
            else
                result = arg(input%numb1%val,input%numb2%val+1)
            end if
        case (TYPE_INT)
            result = intval(0)
        case (TYPE_ADD)
            temp1 = diff(input%a,argument)
            ltemp1 = .false.
            if (temp1%type==TYPE_INT.and.temp1%numb1%type==INT_VAL.and.temp1%numb1%val==0) ltemp1 = .true.
            temp2 = diff(input%b,argument)
            ltemp2 = .false.
            if (temp2%type==TYPE_INT.and.temp2%numb1%type==INT_VAL.and.temp2%numb1%val==0) ltemp2 = .true.
            if (ltemp1.and.ltemp2) then
                result = numb(0)
            else if (ltemp1) then
                result = temp2
            else if (ltemp2) then
                result = temp1
            else
                result = temp1+temp2
            end if
        case (TYPE_POW)
            temp1 = diff(input%a,argument)
            if (temp1%type==TYPE_INT.and.temp1%numb1%type==INT_VAL.and.temp1%numb1%val==0) then
                result = numb(0)
            else
                if (temp1%type==TYPE_INT.and.temp1%numb1%type==INT_VAL.and.temp1%numb1%val==1) then
                    result = numb(input%numb1)*input%a**(input%numb1-1)
                else
                    result = numb(input%numb1)*input%a**(input%numb1-1)*diff(input%a,argument)
                end if
            end if
        case (TYPE_MLT)
            temp1 = diff(input%a,argument)
            ltemp1 = .false.
            if (temp1%type==TYPE_INT.and.temp1%numb1%type==INT_VAL.and.temp1%numb1%val==0) ltemp1 = .true.
            temp2 = diff(input%b,argument)
            ltemp2 = .false.
            if (temp2%type==TYPE_INT.and.temp2%numb1%type==INT_VAL.and.temp2%numb1%val==0) ltemp2 = .true.
            if (ltemp1.and.ltemp2) then
                result = numb(0)
            else if (ltemp1) then
                result = input%a*temp2
            else if (ltemp2) then
                result = temp1*input%b
            else
                result = temp1*input%b+input%a*temp2
            end if
        case (TYPE_DIV)
            temp1 = diff(input%a,argument)
            ltemp1 = .false.
            if (temp1%type==TYPE_INT.and.temp1%numb1%type==INT_VAL.and.temp1%numb1%val==0) ltemp1 = .true.
            temp2 = diff(input%b,argument)
            ltemp2 = .false.
            if (temp2%type==TYPE_INT.and.temp2%numb1%type==INT_VAL.and.temp2%numb1%val==0) ltemp2 = .true.
            if (ltemp1.and.ltemp2) then
                result = numb(0)
            else if (ltemp1) then
                result = 0-input%a*temp2/input%b**2
            else if (ltemp2) then
                result = temp1/input%b
            else
                result = (input%b*temp1-input%a*temp2)/input%b**2
            end if
        case (TYPE_SUM)
            result = sum(input%numb1,input%numb2,diff(input%a,argument),diff(input%b,argument),input%minn+1)
        case default
            error stop
        end select
    end function

    recursive type(Ball) function eval(input,mineps) result(result)
        type(number), intent(in) :: input
        real(real128), value :: mineps
        type(Ball) :: resultx, resulty
        real(real128) :: xmax, xmin, ymax, ymin, zmax, zmin, znom, ULP
        integer :: i, j, k
        select case (input%type)
        case (TYPE_VAR)
            error stop "Variables cannot be within any evaluated numbers"
        case (TYPE_INT)
            select case (input%numb1%type)
            case (INT_VAL)
                result%val = input%numb1%val
            case (INT_N)
                if (input%numb1%val>=sumptr) error stop "sum depth not great enough"
                result%val = sumstack(sumptr-input%numb1%val)+input%numb1%offset
            case (INT_INF)
                result%val = 1./0. ! generate infinity, really should never be used but is here for completeness sake
            end select
            !result%epsilon = 2**real(exponent(result%val)-112,real128)/2
            result%epsilon = 0
        case (TYPE_ADD)
            result%epsilon = 100000000
            mineps = mineps * 2
            i = 0
            do while (result%epsilon>mineps)
                if (i/=0) then
                    if (resultx%epsilon>mineps.and.resulty%epsilon>mineps) return
                end if
                mineps = mineps*0.5
                resultx = eval(input%a,mineps/2)
                resulty = eval(input%b,mineps/2)
                result%val = resultx%val+resulty%val
            ! ULP = 2^(exp-(pbits-1))
                ULP = 2**real(exponent(result%val)-112,real128)
            ! error = max(emax,emin) + 1/2 ULP
                result%epsilon = resultx%epsilon+resulty%epsilon+ULP/2
                i = i + 1
            end do
        case (TYPE_POW)
            result%epsilon = 100000000
            i = 0
            do while (result%epsilon>mineps)
                if (i/=0) then
                    if (resultx%epsilon>mineps) return
                end if
                mineps = mineps*0.5

                j = collapseInt(input%numb1)
                resultx = eval(input%a,mineps)
                xmin = sign(abs(resultx%val)-resultx%epsilon,resultx%val)
                xmax = sign(abs(resultx%val)+resultx%epsilon,resultx%val)

                zmax = xmax**j
                zmin = xmin**j

                znom = resultx%val**j

                zmax = abs(zmax-znom)
                zmin = abs(zmin-znom)
                result%val = znom
            ! ULP = 2^(exp-(pbits-1))
                ULP = 2**real(exponent(result%val)-112,real128)
            ! error = max(emax,emin) + 1/2 ULP
                result%epsilon = max(zmax,zmin)+ULP/2
                i = i + 1
            end do
        case (TYPE_MLT)
            result%epsilon = 100000000
            mineps = mineps * 2
            i = 0
            do while (result%epsilon>mineps)
                if (i/=0) then
                    if (resultx%epsilon>mineps.and.resulty%epsilon>mineps) return
                end if
                mineps = mineps*0.5
                resultx = eval(input%a,mineps)
                xmin = sign(abs(resultx%val)-resultx%epsilon,resultx%val)
                xmax = sign(abs(resultx%val)+resultx%epsilon,resultx%val)

                resulty = eval(input%b,mineps)
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
        case (TYPE_DIV)
            result%epsilon = 100000000
            mineps = mineps * 2
            i = 0
            do while (result%epsilon>mineps)
                if (i/=0) then
                    if (resultx%epsilon>mineps.and.resulty%epsilon>mineps) return
                end if
                mineps = mineps*0.5
                resultx = eval(input%a,mineps)
                xmin = sign(abs(resultx%val)-resultx%epsilon,resultx%val)
                xmax = sign(abs(resultx%val)+resultx%epsilon,resultx%val)

                resulty = eval(input%b,mineps)
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
        case (TYPE_SUM)
            i=collapseInt(input%numb1)
            k=i+input%minn
            j=collapseInt(input%numb2)
            sumptr = sumptr + 1
            ! add all 3 error bounds, so perhaps use n/10
            result%val = 0
            result%epsilon = 0
            do
                sumstack(sumptr) = i
                resultx = eval(input%a,mineps/10)
                result%val = result%val+resultx%val
                result%epsilon = result%epsilon+resultx%epsilon+2**real(exponent(result%val)-112,real128)/2

                resulty = eval(input%b,mineps/10)
                resulty%val = abs(resulty%val)

                if (i>k.and.resulty%val+resulty%epsilon+result%epsilon<=mineps) exit
                i = i + 1
                if (i>j) exit
            end do
            result%epsilon=resulty%val+resulty%epsilon+result%epsilon
        case default
            error stop
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
            iomsg = 'Variable list not supported for this io type'
            return
        end if
        select type (dtv)
        type is (Ball)
            write(unit,'(ES0.36E0,A,ES0.36E0)') dtv%val,' +- ',dtv%epsilon
        end select
    end subroutine

    subroutine write_number(dtv,unit,iotype,v_list,iostat,iomsg)
        class(number), intent(in) :: dtv
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        iostat = 0
        if (size(v_list)/=0) then
            iostat = 1
            iomsg = iotype ! to silence the beast of gfortran
            iomsg = 'Variable list not supported for this io type'
            return
        end if
        select type (dtv)
        type is (number)
            call printNumb(unit, dtv, 0)
            write(unit,'(A)') 'end'
        end select
    end subroutine

    recursive subroutine printNumb(unit,val, indent)
        integer, intent(in) :: unit
        type(number), intent(in) :: val
        integer, intent(in) :: indent
        select case (val%type)
        case (TYPE_VAR)
            write(unit,'(A,I0,A,I0,A)') repeat('  ',indent)//'var:',val%numb1%val,', derivative:',val%numb2%val,achar(10)
        case (TYPE_INT)
            select case (val%numb1%type)
            case (INT_VAL)
                write(unit,'(A,I0,A)') repeat('  ',indent)//'int:',val%numb1%val,achar(10)
            case (INT_N)
                write(unit,'(A,I0,A,I0,A)') repeat('  ',indent)//'n:',val%numb1%val,'+',val%numb1%offset,achar(10)
            case (INT_INF)
                write(unit,'(A)') repeat(' ',indent)//'infinity'//achar(10)
            end select
        case (TYPE_ADD)
            write(unit,'(A)') repeat('  ',indent)//'+:'//achar(10)
            call printNumb(unit,val%a,indent+1)
            call printNumb(unit,val%b,indent+1)
        case (TYPE_POW)
            write(unit,'(A)') repeat('  ',indent)//'**'
            select case (val%numb1%type)
            case (INT_VAL)
                write(unit,'(A,I0,A)') 'int:',val%numb1%val,achar(10)
            case (INT_N)
                write(unit,'(A,I0,A,I0,A)') 'n:',val%numb1%val,'+',val%numb1%offset,achar(10)
            case (INT_INF)
                write(unit,'(A)') 'infinity'//achar(10)
            end select

            call printNumb(unit,val%a,indent+1)
        case (TYPE_MLT)
            write(unit,'(A)') repeat('  ',indent)//'*:'//achar(10)
            call printNumb(unit,val%a,indent+1)
            call printNumb(unit,val%b,indent+1)
        case (TYPE_DIV)
            write(unit,'(A)') repeat('  ',indent)//'/:'//achar(10)
            call printNumb(unit,val%a,indent+1)
            call printNumb(unit,val%b,indent+1)
        case (TYPE_SUM)
            write(unit,'(A)') repeat(' ',indent)//'sum:'//achar(10)
            write(unit,'(A)') repeat(' ',indent+1)//'from:'//achar(10)
            select case (val%numb1%type)
            case (INT_VAL)
                write(unit,'(A,I0,A)') repeat('  ',indent+2)//'int:',val%numb1%val,achar(10)
            case (INT_N)
                write(unit,'(A,I0,A,I0,A)') repeat('  ',indent+2)//'n:',val%numb1%val,'+',val%numb1%offset,achar(10)
            case (INT_INF)
                write(unit,'(A)') repeat(' ',indent+2)//'infinity'//achar(10)
            end select
            write(unit,'(A)') repeat(' ',indent+1)//'to:'//achar(10)
            select case (val%numb2%type)
            case (INT_VAL)
                write(unit,'(A,I0,A)') repeat('  ',indent+2)//'int:',val%numb2%val,achar(10)
            case (INT_N)
                write(unit,'(A,I0,A,I0,A)') repeat('  ',indent+2)//'n:',val%numb2%val,'+',val%numb2%offset,achar(10)
            case (INT_INF)
                write(unit,'(A)') repeat(' ',indent+2)//'infinity'//achar(10)
            end select
            write(unit,'(A)') repeat(' ',indent+1)//'expression:'//achar(10)
            call printNumb(unit,val%a,indent+2)
            write(unit,'(A)') repeat(' ',indent+1)//'error:'//achar(10)
            call printNumb(unit,val%b,indent+2)
        case default
            write(unit,'(A)') 'unknown type'//achar(10)
        end select
    end subroutine
end module
