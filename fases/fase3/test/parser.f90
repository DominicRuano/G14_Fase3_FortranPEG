
!auto-generated
module parser
    implicit none
    character(len=:), allocatable, private :: input
    integer, private :: savePoint, lexemeStart, cursor, copyCursor
    logical, private :: isTerminal = .true.

    interface toStr
        module procedure intToStr
        module procedure strToStr
    end interface

    
    type :: operation
        character(len=:), allocatable :: operator
        integer :: operand
    end type

   

    contains

    

    function parse(str) result(res)
        character(len=:), allocatable :: str
        integer :: res

        input = str
        cursor = 1

        res = peg_Expression()
    end function parse

    ! Inicio de las reglas
    
    function peg_Expression() result (res)
        integer :: res
        integer :: expr_0_0
type(operation) :: expr_0_1
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                
                expr_0_0 = peg_Term()

                lexemeStart = cursor

                expr_0_1 = ""
                isTerminal = .false.
                do while (.not. cursor > len(input))
                    copyCursor = cursor
                    expr_0_1 = expr_0_1 // peg_ExpressionTail()
                    if ( savePoint == cursor) then ! no se consumio nada en 0 o muchos
                        ! Si llego aqui ocurrio un falso positivo en los errores
                        ! por eso solo continua y los activa al salir del loop
                        exit
                    else if (copyCursor == cursor) then ! no se consumio nada luego de muchos
                        ! Si llego aqui ocurrio un error y los errores estan desactivados
                        ! Los reactiva y marca el error.
                        isTerminal = .true.
                        call pegError()
                        exit
                    end if
                end do
                isTerminal = .true.
                if (.not. acceptEOF()) cycle
                
                res = peg_Expression_f0(expr_0_0, expr_0_1)


                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_Expression

    ! Inicio de la gramatica


    function peg_ExpressionTail() result (res)
        type(operation) :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
integer :: expr_0_3
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                expr_0_0 = peg__()

                expr_0_1 = peg_0_fa0()
expr_0_2 = peg__()
expr_0_3 = peg_Term()
			res = expr_0_0//expr_0_1//expr_0_0//expr_0_1//expr_0_2//expr_0_3
                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_ExpressionTail

    ! Inicio de la gramatica


    function peg_Term() result (res)
        integer :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                expr_0_0 = peg_Factor()

                expr_0_1 = peg_0_fa1()
			res = 
                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_Term

    ! Inicio de la gramatica


    function peg_Factor() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
integer :: expr_0_2
character(len=:), allocatable :: expr_0_3
character(len=:), allocatable :: expr_0_4
character(len=:), allocatable :: expr_1_0
        integer :: i

        savePoint = cursor
        
        do i = 0, 2
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. acceptString('(')) cycle
                expr_0_0 = consumeInput()
expr_0_1 = peg__()
expr_0_2 = peg_Expression()
expr_0_3 = peg__()

                lexemeStart = cursor
                if(.not. acceptString(')')) cycle
                expr_0_4 = consumeInput()
                
                
            res = toStr(expr_0_2);
        

                exit
            

            case(1)
                cursor = savePoint
                
                expr_1_0 = peg_Integer()
                
                
            res = toStr(expr_1_0);
        

                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_Factor

    ! Inicio de la gramatica


    function peg_Integer() result (res)
        integer :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                
                expr_0_0 = peg__()

                lexemeStart = cursor
                if (.not. acceptSet(["[object Object]"])) cycle

                do while (.not. cursor > len(input))
                    if (.not. acceptSet(["[object Object]"])) exit
                end do

                expr_0_1 = consumeInput()
                
                
                res = peg_Integer_f0(expr_0_1)


                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_Integer

    ! Inicio de la gramatica


    function peg__() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
                lexemeStart = cursor

                do while (.not. cursor > len(input))
                    if (.not. acceptSet([" ","\t","\n","\r"])) exit
                end do

                expr_0_0 = consumeInput()
                
                
            res = toStr(expr_0_0);
        

                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg__

    ! Inicio de la gramatica


    ! Inicio de las acciones
    
    function peg_Expression_f0(head, tail) result(res)
        integer :: head
type(operation) :: tail
        integer :: res
        
        integer :: i

        if (size(tail) < 0) then
            res = head
            return
        end if

        do i = 1, size(tail)
            if (tail(i)%operator == '+') then
                head = head + tail(i)%operand
            else
                head = head - tail(i)%operand
            end if
        end do

        res = head
    
    end function peg_Expression_f0
    

    function peg_0_fa0() result(res)
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
character(len=:), allocatable :: expr_0_3
character(len=:), allocatable :: expr_0_4
character(len=:), allocatable :: expr_0_5
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_1_2
character(len=:), allocatable :: expr_1_3
character(len=:), allocatable :: expr_1_4
character(len=:), allocatable :: expr_1_5
character(len=:), allocatable :: expr_2_0
character(len=:), allocatable :: expr_2_1
character(len=:), allocatable :: expr_2_2
character(len=:), allocatable :: expr_2_3
character(len=:), allocatable :: expr_2_4
character(len=:), allocatable :: expr_2_5
        character(len=:), allocatable :: res
        

                        integer :: i
savePoint = cursor

        do i = 0, 2
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. acceptString('+')) cycle
                expr_0_0 = consumeInput()
                
                
            res = toStr(expr_0_0);
        

                exit
            

            case(1)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. acceptString('-')) cycle
                expr_1_0 = consumeInput()
                
                
            res = toStr(expr_1_0);
        

                exit
            
            case default
                call pegError()
            end select
        end do
 
    end function peg_0_fa0
    

    function peg_0_fa2() result(res)
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
character(len=:), allocatable :: expr_0_3
character(len=:), allocatable :: expr_0_4
character(len=:), allocatable :: expr_0_5
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_1_2
character(len=:), allocatable :: expr_1_3
character(len=:), allocatable :: expr_1_4
character(len=:), allocatable :: expr_1_5
character(len=:), allocatable :: expr_2_0
character(len=:), allocatable :: expr_2_1
character(len=:), allocatable :: expr_2_2
character(len=:), allocatable :: expr_2_3
character(len=:), allocatable :: expr_2_4
character(len=:), allocatable :: expr_2_5
        character(len=:), allocatable :: res
        

                        integer :: i
savePoint = cursor

        do i = 0, 2
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. acceptString('*')) cycle
                expr_0_0 = consumeInput()
                
                
            res = toStr(expr_0_0);
        

                exit
            

            case(1)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. acceptString('/')) cycle
                expr_1_0 = consumeInput()
                
                
            res = toStr(expr_1_0);
        

                exit
            
            case default
                call pegError()
            end select
        end do
 
    end function peg_0_fa2
    

    function peg_0_fa1() result(res)
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
character(len=:), allocatable :: expr_0_3
character(len=:), allocatable :: expr_0_4
character(len=:), allocatable :: expr_0_5
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_1_2
character(len=:), allocatable :: expr_1_3
character(len=:), allocatable :: expr_1_4
character(len=:), allocatable :: expr_1_5
character(len=:), allocatable :: expr_2_0
character(len=:), allocatable :: expr_2_1
character(len=:), allocatable :: expr_2_2
character(len=:), allocatable :: expr_2_3
character(len=:), allocatable :: expr_2_4
character(len=:), allocatable :: expr_2_5
        character(len=:), allocatable :: res
        

                        integer :: i
savePoint = cursor

        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                
                expr_0_0 = peg_0_fa2()
expr_0_1 = peg__()
expr_0_2 = peg_Factor()
			res = expr_0_0//expr_0_1
                exit
            
            case default
                call pegError()
            end select
        end do
 
    end function peg_0_fa1
    

    function peg_Integer_f0(num) result(res)
        character(len=:), allocatable :: num
        integer :: res
        

        read(num, *) res
    
    end function peg_Integer_f0
    

    ! Funciones auxiliares
    function acceptString(str) result(accept)
        character(len=*) :: str
        logical :: accept
        integer :: offset

        offset = len(str) - 1
        if (str /= input(cursor:cursor + offset)) then
            accept = .false.
            return
        end if
        cursor = cursor + len(str)
        accept = .true.
    end function acceptString

    function acceptRange(bottom, top) result(accept)
        character(len=1) :: bottom, top
        logical :: accept

        if(.not. (input(cursor:cursor) >= bottom .and. input(cursor:cursor) <= top)) then
            accept = .false.
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptRange

    function acceptSet(set) result(accept)
        character(len=1), dimension(:) :: set
        logical :: accept

        if(.not. (findloc(set, input(cursor:cursor), 1) > 0)) then
            accept = .false.
            return
        end if
        if((findloc([char(13)], input(cursor:cursor), 1) > 0)) then
            if((findloc([char(10)], input(cursor+1:cursor+1), 1) > 0)) then
                cursor = cursor + 1
            end if
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptSet

    function acceptPeriod() result(accept)
        logical :: accept

        if (cursor > len(input)) then
            accept = .false.
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptPeriod

    function acceptEOF() result(accept)
        logical :: accept

        if(.not. cursor > len(input)) then
            accept = .false.
            return
        end if
        accept = .true.
    end function acceptEOF

    function consumeInput() result(substr)
        character(len=:), allocatable :: substr

        substr = input(lexemeStart:cursor - 1)
    end function consumeInput

    subroutine pegError()
        if ( isTerminal ) then
            print *, "Error at ", cursor, ": '", input(cursor:cursor), "'"
            call exit(1)
        else 
            print *, "Falso positivo at ", cursor, ": '", input(cursor:cursor), "' (borrar antes de la calificaion)"
        end if

    end subroutine pegError

    function intToStr(int) result(cast)
        integer :: int
        character(len=31) :: tmp
        character(len=:), allocatable :: cast

        write(tmp, '(I0)') int
        cast = trim(adjustl(tmp))
    end function intToStr

    function strToStr(str) result(cast)
        character(len=:), allocatable :: str
        character(len=:), allocatable :: cast

        cast = str
    end function strToStr

    function acceptStringCaseInsensitive(str) result(accept)
        character(len=*) :: str
        logical :: accept
        integer :: offset

        offset = len(str) - 1
        if (tolower(str) /= tolower(input(cursor:cursor + offset))) then
            accept = .false.
            return
        end if
        cursor = cursor + len(str)
        accept = .true.
    end function acceptStringCaseInsensitive

    function tolower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i

        lower_str = str 
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
    end function tolower

    function matchExactRepetition(int, str) result(accept) ! expression |count|
        integer, intent(in) :: int
        character(len=*) :: str
        logical :: accept
        integer :: i

        accept = .true.

        do i = 1, int
            if (.not. acceptString(str)) then
                accept = .false.
                return
            end if
        end do
    end function matchExactRepetition

    function matchVariableRepetition(minReps, maxReps, str) result(accept) ! expression |min..max|
        integer, intent(in) :: minReps, maxReps
        character(len=*), intent(in) :: str
        logical :: accept
        integer :: i, count

        count  = 0
        accept = .false.

        do i = 1, maxReps
            if (.not. acceptString(str)) then
                exit
            end if
            count = count + 1
        end do

        if (count >= minReps) then
            accept = .true.
        end if

    end function matchVariableRepetition

    function matchExactRepetitionWithSeparator(count, str, sep) result(accept) ! expression |count, delimiter|
        integer, intent(in) :: count
        character(len=*), intent(in) :: str
        character(len=*), intent(in) :: sep
        logical :: accept
        integer :: i

        accept = .true.

        do i = 1, count
            if (.not. acceptString(str)) then
                accept = .false.
                return
            end if
            if (i < count) then
                if (.not. acceptString(sep)) then
                    accept = .false.
                    return
                end if
            end if
        end do
    end function matchExactRepetitionWithSeparator

    function matchVariableRepetitionWithSeparator(minReps, maxReps, str, sep) result(accept) ! expression |min..max, delimiter|
        integer, intent(in) :: minReps, maxReps
        character(len=*), intent(in) :: str, sep
        logical :: accept
        integer :: i, count

        count  = 0
        accept = .false.

        do i = 1, maxReps
            if (.not. acceptString(str)) then
                exit
            end if
            count = count + 1
            if (i < maxReps) then
                if (.not. acceptString(sep)) then
                    exit
                end if
            end if
        end do

        if (count >= minReps) then
            accept = .true.
        end if

    end function matchVariableRepetitionWithSeparator

end module parser
