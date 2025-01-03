
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

    

    contains

    

    function parse(str) result(res)
        character(len=:), allocatable :: str
        character(len=:), allocatable :: res

        input = str
        cursor = 1

        res = peg_s()
    end function parse

    ! Inicio de las reglas
    
    function peg_s() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                
                expr_0_0 = peg_0_fd3()

                expr_0_1 = peg_0_fd4()

                expr_0_2 = peg_0_fd5()
			res = expr_0_0//expr_0_1//expr_0_2
                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_s

    ! Inicio de la gramatica


    ! Inicio de las acciones
    
    function peg_0_fd3() result(res)
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
                if(.not. acceptString('a')) cycle
                expr_0_0 = consumeInput()
                
                
            res = toStr(expr_0_0);
        

                exit
            

            case(1)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. acceptString('b')) cycle
                expr_1_0 = consumeInput()
                
                
            res = toStr(expr_1_0);
        

                exit
            
            case default
                call pegError()
            end select
        end do
 
    end function peg_0_fd3
    

    function peg_0_fd4() result(res)
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
                if(.not. acceptString('1')) cycle
                expr_0_0 = consumeInput()
                
                
            res = toStr(expr_0_0);
        

                exit
            

            case(1)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. acceptString('2')) cycle
                expr_1_0 = consumeInput()
                
                
            res = toStr(expr_1_0);
        

                exit
            
            case default
                call pegError()
            end select
        end do
 
    end function peg_0_fd4
    

    function peg_0_fd5() result(res)
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
                if (.not. (acceptRange('0', '9'))) cycle

                do while (.not. cursor > len(input))
                    if (.not. (acceptRange('0', '9'))) exit
                end do

                expr_0_0 = consumeInput()
                
                
            res = toStr(expr_0_0);
        

                exit
            

            case(1)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if (.not. (acceptRange('a', 'z'))) cycle

                do while (.not. cursor > len(input))
                    if (.not. (acceptRange('a', 'z'))) exit
                end do

                expr_1_0 = consumeInput()
                
                
            res = toStr(expr_1_0);
        

                exit
            
            case default
                call pegError()
            end select
        end do
 
    end function peg_0_fd5
    

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

    function matchExactRepetition(int, str) result(accept)
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

    function matchVariableRepetition(minReps, maxReps, str) result(accept)
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

    function matchExactRepetitionWithSeparator(count, str, sep) result(accept)
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

    function matchVariableRepetitionWithSeparator(minReps, maxReps, str, sep) result(accept)
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
