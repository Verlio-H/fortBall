module series
    use iso_fortran_env
    implicit none


    integer, parameter :: TYPE_INT = 0
    integer, parameter :: TYPE_BALL = 1
    integer, parameter :: TYPE_ADD = 2
    integer, parameter :: TYPE_MLT = 3
    integer, parameter :: TYPE_DIV = 4
    integer, parameter :: TYPE_POW = 5
    integer, parameter :: TYPE_FACT = 6
    integer, parameter :: TYPE_SUM = 7
    integer, parameter :: TYPE_SSUM = 8
    integer, parameter :: TYPE_VAR = 9
    integer, parameter :: TYPE_LASTCALC = 10
    integer, parameter :: TYPE_LASTERR = 11


    integer, parameter :: INT_VAL = 0
    integer, parameter :: INT_N = 1
    integer, parameter :: INT_MAXN = 2
    integer, parameter :: INT_INF = 3


    type expandedint
        integer :: type
        integer :: val
        integer :: offset = 0
        integer :: multiplication = 1
    end type

    type(expandedint), parameter :: infinity = expandedint(INT_INF,0)

    type Number
        integer :: type
        type(Number), allocatable :: a
        type(Number), allocatable :: b
        type(expandedint) :: numb1 = expandedint(INT_VAL,0)
        type(expandedint) :: numb2 = expandedint(INT_VAL,0)
        integer :: minn = 0
        real(real128) :: initval = 0
        real(real128) :: initerr = 0
        integer, allocatable :: diffs(:)
    end type

    type Ball
        real(real128) :: val
        real(real128) :: epsilon
    end type

    integer, parameter :: MAX_SUM_DEPTH = 1024
    integer :: sumstack(MAX_SUM_DEPTH)
    integer :: ssummaxstack(MAX_SUM_DEPTH)
    type(Ball) :: lastcalcstack(MAX_SUM_DEPTH)
    type(Ball) :: lasterrorstack(MAX_SUM_DEPTH)
    integer :: sumptr = 0

    interface operator(+)
        module procedure addition, addition_int_numb, addition_numb_int
        module procedure eintadd, eintaddrev
    end interface

    interface operator(-)
        module procedure subtract, subtract_int_numb, subtract_numb_int
        module procedure eintsub, eintsubrev
    end interface

    interface operator(/)
        module procedure divide, divide_int_numb, divide_numb_int
    end interface

    interface operator(*)
        module procedure multiply, multiply_int_numb, multiply_numb_int
        module procedure eintmlt, eintmltrev
    end interface

    interface operator(**)
        module procedure exponentiate, expanded_exponentiate
    end interface

    interface write(formatted)
        module procedure write, write_number
    end interface

    interface numb
        module procedure intval, eintval, numbball
    end interface

    interface factorial
        module procedure factorial_int, factorial_eint
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
            c%multiplication = a%multiplication
        case (INT_INF)
            c = a
        end select
    end function

    pure elemental type(number) function lastcalc(a)
        integer, intent(in) :: a
        lastcalc%type = TYPE_LASTCALC
        lastcalc%minn = a
    end function

    pure elemental type(number) function lasterr(a)
        integer, intent(in) :: a
        lasterr%type = TYPE_LASTERR
        lasterr%minn = a
    end function

    pure elemental type(expandedint) function eintaddrev(b,a) result(c)
        type(expandedint), intent(in) :: a
        integer, intent(in) :: b
        c = eintadd(a,b)
    end function

    pure elemental type(expandedint) function eintsub(a,b) result(c)
        type(expandedint), intent(in) :: a
        integer, intent(in) :: b
        c = eintadd(a,-b)
    end function

    pure elemental type(expandedint) function eintsubrev(b,a) result(c)
        type(expandedint), intent(in) :: a
        integer, intent(in) :: b
        c = b+(-1)*a
    end function

    pure elemental type(expandedint) function eintmlt(a,b) result(c)
        type(expandedint), intent(in) :: a
        integer, intent(in) :: b
        select case (a%type)
        case (INT_VAL)
            c%type = INT_VAL
            c%val = a%val*b
        case (INT_N)
            c%type = INT_N
            c%val = a%val
            c%offset = a%offset*b
            c%multiplication = a%multiplication*b
        case (INT_INF)
            c = a
        end select
    end function

    pure elemental type(expandedint) function eintmltrev(b,a) result(c)
        type(expandedint), intent(in) :: a
        integer, intent(in) :: b
        c = eintmlt(a,b)
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
            j = sumstack(sumptr-input%val)*input%multiplication+input%offset
        case (INT_INF)
            j = huge(j)
        end select
    end function

    pure elemental type(number) function sum(low,high,expr,error,minn,startval,starterr)
        type(expandedint), intent(in) :: low
        type(expandedint), intent(in) :: high
        type(number), intent(in) :: expr
        type(number), intent(in) :: error
        integer, intent(in), optional :: minn
        real(real128), intent(in), optional :: startval, starterr
        sum%type = TYPE_SUM
        sum%numb1 = low
        sum%numb2 = high
        sum%a = expr
        sum%b = error
        if (present(minn)) sum%minn = minn
        if (present(startval)) sum%initval = startval
        if (present(starterr)) sum%initerr = starterr
    end function

    pure elemental type(number) function ssum(low,high,expr,error,startval,starterr)
        type(expandedint), intent(in) :: low
        type(expandedint), intent(in) :: high
        type(number), intent(in) :: expr
        type(number), intent(in) :: error
        real(real128), intent(in), optional :: startval, starterr
        ssum%type = TYPE_SSUM
        ssum%numb1 = low
        ssum%numb2 = high
        ssum%a = expr
        ssum%b = error
        if (present(startval)) ssum%initval = startval
        if (present(starterr)) ssum%initerr = starterr
    end function 

    pure elemental type(expandedint) function eint(input)
        integer, intent(in) :: input
        eint = expandedint(INT_VAL,input)
    end function

    pure elemental type(expandedint) function eintsum(input)
        integer, intent(in) :: input
        eintsum = expandedint(INT_N,input)
    end function

    pure elemental type(expandedint) function eintsummax(input)
        integer, intent(in) :: input
        eintsummax = expandedint(INT_MAXN,input)
    end function

    pure elemental type(number) function numbball(val,err)
        real(real128), intent(in) :: val, err
        numbball%type = TYPE_BALL
        numbball%initval = val
        numbball%initerr = err
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

    pure elemental type(number) function factorial_int(a)
        integer, intent(in) :: a
        factorial_int = number(TYPE_FACT)
        factorial_int%numb1 = eint(a)
    end function

    pure elemental type(number) function factorial_eint(a)
        type(expandedint), intent(in) :: a
        factorial_eint = number(TYPE_FACT)
        factorial_eint%numb1 = a
    end function

    pure elemental type(number) function exponentiate(a,b)
        type(number), intent(in) :: a
        integer, intent(in) :: b
        exponentiate = number(TYPE_POW)
        exponentiate%a = a
        exponentiate%numb1 = expandedint(INT_VAL,b)
    end function

    pure elemental type(number) function expanded_exponentiate(a,b)
        type(number), intent(in) :: a
        type(expandedint), intent(in) :: b
        expanded_exponentiate = number(TYPE_POW)
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

    pure recursive function populate(input,args) result(result)
        type(number), intent(in) :: input
        type(number), intent(in) :: args(:)
        type(number) :: result
        integer :: i
        select case (input%type)
        case (TYPE_VAR)
            if (input%numb1%val<=size(args)) then
                result = args(input%numb1%val)
                do i=1,input%numb2%val
                    result = diff(result,input%diffs(i))
                end do
            end if
        case (TYPE_INT,TYPE_FACT,TYPE_LASTCALC,TYPE_LASTERR,TYPE_BALL)
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
            result = sum(input%numb1,input%numb2,populate(input%a,args),populate(input%b,args),&
             input%minn,input%initval,input%initerr)
        case (TYPE_SSUM)
            result = ssum(input%numb1,input%numb2,populate(input%a,args),populate(input%b,args),&
             input%initval,input%initerr)
        case default
            error stop
        end select
    end function 

    pure elemental recursive type(number) function diff(input, argument) result(result)
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
                result = input
                result%numb2%val = result%numb2%val + 1
                if (allocated(result%diffs)) then
                    result%diffs = [result%diffs, argument]
                else
                    result%diffs = [argument]
                end if
            end if
        case (TYPE_INT,TYPE_FACT,TYPE_BALL)
            result = intval(0)
        case (TYPE_LASTCALC,TYPE_LASTERR)
            error stop "backreferences cannot be derived currently"
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
        result = sum(input%numb1,input%numb2,diff(input%a,argument),diff(input%b,argument),input%minn+1,input%initval,input%initerr)
        case (TYPE_SSUM)
            result = ssum(input%numb1,input%numb2,diff(input%a,argument),diff(input%b,argument),input%initval,input%initerr)
        case default
            error stop
        end select
    end function

    impure elemental recursive type(Ball) function eval(input,maxeps) result(result)
        type(number), intent(in) :: input
        real(real128), value :: maxeps
        type(Ball) :: resultx, resulty
        real(real128) :: xmax, xmin, ymax, ymin, zmax, zmin, znom, ULP, maxepstmp
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
                result%val = sumstack(sumptr-input%numb1%val)*input%numb1%multiplication+input%numb1%offset
            case (INT_MAXN)
                if (input%numb1%val>=sumptr) error stop "sum depth not great enough"
                result%val = ssummaxstack(sumptr-input%numb1%val)*input%numb1%multiplication+input%numb1%offset
            case (INT_INF)
                result%val = 1./0. ! generate infinity, really should never be used but is here for completeness sake
            end select
            !result%epsilon = 2**real(exponent(result%val)-112,real128)/2
            result%epsilon = 0
        case (TYPE_BALL)
            result%val = input%initval
            result%epsilon = input%initerr
        case (TYPE_LASTCALC)
            result = lastcalcstack(sumptr-input%minn)
        case (TYPE_LASTERR)
            result = lasterrorstack(sumptr-input%minn)
        case (TYPE_FACT)
            select case (input%numb1%type)
            case (INT_VAL)
                result%val = input%numb1%val
                result%val = gamma(result%val+1)
            case (INT_N)
                if (input%numb1%val>=sumptr) error stop "sum depth not great enough"
                result%val = sumstack(sumptr-input%numb1%val)*input%numb1%multiplication+input%numb1%offset
                result%val = gamma(result%val+1)
            case (INT_MAXN)
                if (input%numb1%val>=sumptr) error stop "sum depth not great enough"
                result%val = ssummaxstack(sumptr-input%numb1%val)*input%numb1%multiplication+input%numb1%offset
                result%val = gamma(result%val+1)
            case (INT_INF)
                result%val = 1./0. ! generate infinity, really should never be used but is here for completeness sake
            end select
            result%epsilon = 2**real(exponent(result%val)-112,real128)/2
        case (TYPE_ADD)
            result%epsilon = 100000000
            maxepstmp = maxeps * 2
            i = 0
            do while (result%epsilon>maxeps)
                if (i/=0) then
                    if (resultx%epsilon>maxeps.and.resulty%epsilon>maxeps) return
                end if
                maxepstmp = maxepstmp*0.5
                resultx = eval(input%a,maxepstmp/2)
                resulty = eval(input%b,maxepstmp/2)
                result%val = resultx%val+resulty%val
            ! ULP = 2^(exp-(pbits-1))
                ULP = 2**real(exponent(result%val)-112,real128)
            ! error = max(emax,emin) + 1/2 ULP
                result%epsilon = resultx%epsilon+resulty%epsilon+ULP/2
                i = i + 1
            end do
        case (TYPE_POW)
            result%epsilon = 100000000
            maxepstmp = maxeps * 2
            i = 0
            do while (result%epsilon>maxeps)
                if (i/=0) then
                    if (resultx%epsilon>maxeps) return
                    if (2**real(exponent(result%val)-112,real128)>maxeps) return
                end if
                maxepstmp = maxeps*0.5

                j = collapseInt(input%numb1)
                resultx = eval(input%a,maxepstmp)
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
            maxepstmp = maxeps * 2
            i = 0
            do while (result%epsilon>maxeps)
                if (i/=0) then
                    if (resultx%epsilon>maxepstmp.and.resulty%epsilon>maxepstmp) return
                    if (2**real(exponent(result%val)-112,real128)>maxeps) return
                end if
                maxepstmp = maxepstmp*0.5
                resultx = eval(input%a,maxepstmp)
                xmax = sign(abs(resultx%val)+resultx%epsilon,resultx%val)

                resulty = eval(input%b,maxepstmp)
                ymax = sign(abs(resulty%val)+resulty%epsilon,resulty%val)
            ! zmax = xmax*ymax
                zmax = xmax*ymax
            ! znominal = x*y
                znom = resultx%val*resulty%val
            ! emax = abs(zmax-znominal)
                zmax = abs(zmax-znom)
                result%val = znom
            ! ULP = 2^(exp-(pbits-1))
                ULP = 2**real(exponent(result%val)-112,real128)
            ! error = max(emax,emin) + 1/2 ULP
                result%epsilon = zmax+ULP/2
                i = i + 1
            end do
        case (TYPE_DIV)
            result%epsilon = 100000000
            maxepstmp = maxeps * 2
            i = 0
            do while (result%epsilon>maxeps)
                if (i/=0) then
                    if (resultx%epsilon>maxeps.and.resulty%epsilon>maxeps) return
                    if (2**real(exponent(result%val)-112,real128)>maxeps) return
                end if
                maxepstmp = maxepstmp*0.5
                resultx = eval(input%a,maxepstmp)
                xmin = sign(abs(resultx%val)-resultx%epsilon,resultx%val)
                xmax = sign(abs(resultx%val)+resultx%epsilon,resultx%val)

                resulty = eval(input%b,maxepstmp)
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
            result%val = input%initval
            result%epsilon = 0
            resulty%val = input%initerr
            resulty%epsilon = 0
            do
                sumstack(sumptr) = i
                lastcalcstack(sumptr) = result
                lasterrorstack(sumptr) = resulty
                resultx = eval(input%a,maxeps/(i+10))
                result%val = result%val+resultx%val
                result%epsilon = result%epsilon+resultx%epsilon+2**real(exponent(result%val)-112,real128)/2

                resulty = eval(input%b,maxeps/(i+10))
                resulty%val = abs(resulty%val)
                if (i>k.and.resulty%val+resulty%epsilon+result%epsilon<=maxeps) exit
                i = i + 1
                if (i>j) exit
            end do
            result%epsilon=resulty%val+resulty%epsilon+result%epsilon
            sumptr = sumptr - 1
        case (TYPE_SSUM)
            j=50
            sumptr = sumptr + 1
            resulty%val = maxeps+0.01
            resulty%epsilon = maxeps
            result%epsilon = maxeps
            do while (resulty%val+resulty%epsilon+result%epsilon > maxeps)
                ! this helps convergence significantly
                j = j + atan(resulty%val/maxeps)*j + 50
                result%val = input%initval
                result%epsilon = 0
                resulty%val = input%initerr
                resulty%epsilon = 0
                if (j>collapseInt(input%numb2)) j = collapseInt(input%numb2)
                do i=collapseInt(input%numb1),j,1
                    sumstack(sumptr) = i
                    ssummaxstack(sumptr) = j
                    lastcalcstack(sumptr) = result
                    lasterrorstack(sumptr) = resulty
                    resultx = eval(input%a,maxeps/(j+5))
                    result%val = result%val+resultx%val
                    result%epsilon = result%epsilon+resultx%epsilon+2**real(exponent(result%val)-112,real128)/2

                    resulty = eval(input%b,maxeps/(j+5))
                    resulty%val = abs(resulty%val)
                end do
                result%epsilon=resulty%val+resulty%epsilon+result%epsilon
                print*,j
                if (j==collapseInt(input%numb2)) exit
            end do
            sumptr = sumptr - 1
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
                write(unit,'(A,I0,A,I0,A,I0,A)') repeat('  ',indent)//'n:',val%numb1%val,'*',val%numb1%multiplication,&
                 '+',val%numb1%offset,achar(10)
            case (INT_MAXN)
                write(unit,'(A,I0,A,I0,A,I0,A)') repeat('  ',indent)//'maxn:',val%numb1%val,'*',val%numb1%multiplication,&
                 '+',val%numb1%offset,achar(10)
            case (INT_INF)
                write(unit,'(A)') repeat('  ',indent)//'infinity'//achar(10)
            end select
        case (TYPE_FACT)
            write(unit,'(A)') repeat('  ',indent)//'!:'//achar(10)
            select case (val%numb1%type)
            case (INT_VAL)
                write(unit,'(A,I0,A)') repeat('  ',indent+1)//'int:',val%numb1%val,achar(10)
            case (INT_N)
                write(unit,'(A,I0,A,I0,A,I0,A)') repeat('  ',indent+1)//'n:',val%numb1%val,'*',val%numb1%multiplication,&
                 '+',val%numb1%offset,achar(10)
            case (INT_MAXN)
                write(unit,'(A,I0,A,I0,A,I0,A)') repeat('  ',indent+1)//'maxn:',val%numb1%val,'*',val%numb1%multiplication,&
                 '+',val%numb1%offset,achar(10)
            case (INT_INF)
                write(unit,'(A)') repeat('  ',indent+1)//'infinity'//achar(10)
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
                write(unit,'(A,I0,A,I0,A,I0,A)') 'n:',val%numb1%val,'*',val%numb1%multiplication,'+',val%numb1%offset,achar(10)
            case (INT_MAXN)
                write(unit,'(A,I0,A,I0,A,I0,A)') 'maxn:',val%numb1%val,'*',val%numb1%multiplication,'+',val%numb1%offset,achar(10)
            case (INT_INF)
                write(unit,'(A)') 'infinity'//achar(10)
            end select

            call printNumb(unit,val%a,indent+1)
        case (TYPE_MLT)
            write(unit,'(A)') repeat('  ',indent)//'*:'//achar(10)
            call printNumb(unit,val%a,indent+1)
            call printNumb(unit,val%b,indent+1)
        case (TYPE_LASTCALC)
            write(unit,'(A,I0,A)') repeat('  ',indent)//'lastcalc:',val%numb1,achar(10)
        case (TYPE_LASTERR)
            write(unit,'(A,I0,A)') repeat('  ',indent)//'lasterr:',val%numb1,achar(10)
        case (TYPE_DIV)
            write(unit,'(A)') repeat('  ',indent)//'/:'//achar(10)
            call printNumb(unit,val%a,indent+1)
            call printNumb(unit,val%b,indent+1)
        case (TYPE_SUM,TYPE_SSUM)
            write(unit,'(A)') repeat('  ',indent)//'sum:'//achar(10)
            write(unit,'(A)') repeat('  ',indent+1)//'from:'//achar(10)
            select case (val%numb1%type)
            case (INT_VAL)
                write(unit,'(A,I0,A)') repeat('  ',indent+2)//'int:',val%numb1%val,achar(10)
            case (INT_N)
                write(unit,'(A,I0,A,I0,A,I0,A)') repeat('  ',indent+2)//'n:',val%numb1%val,'*',val%numb1%multiplication,&
                 '+',val%numb1%offset,achar(10)
            case (INT_INF)
                write(unit,'(A)') repeat('  ',indent+2)//'infinity'//achar(10)
            end select
            write(unit,'(A)') repeat('  ',indent+1)//'to:'//achar(10)
            select case (val%numb2%type)
            case (INT_VAL)
                write(unit,'(A,I0,A)') repeat('  ',indent+2)//'int:',val%numb2%val,achar(10)
            case (INT_N)
                write(unit,'(A,I0,A,I0,A,I0,A)') repeat('  ',indent+2)//'n:',val%numb2%val,'*',val%numb2%multiplication,&
                 '+',val%numb2%offset,achar(10)
            case (INT_MAXN)
                write(unit,'(A,I0,A,I0,A,I0,A)') repeat('  ',indent+2)//'maxn:',val%numb2%val,'*',val%numb2%multiplication,&
                 '+',val%numb2%offset,achar(10)
            case (INT_INF)
                write(unit,'(A)') repeat('  ',indent+2)//'infinity'//achar(10)
            end select
            write(unit,'(A)') repeat('  ',indent+1)//'expression:'//achar(10)
            call printNumb(unit,val%a,indent+2)
            write(unit,'(A)') repeat('  ',indent+1)//'error:'//achar(10)
            call printNumb(unit,val%b,indent+2)
        case default
            write(unit,'(A)') 'unknown type'//achar(10)
        end select
    end subroutine
end module
