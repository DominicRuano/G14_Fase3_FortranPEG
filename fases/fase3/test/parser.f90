
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

        res = peg_pruebas()
    end function parse

    ! Inicio de las reglas
    
    function peg_pruebas() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_2_0
character(len=:), allocatable :: expr_2_1
character(len=:), allocatable :: expr_3_0
character(len=:), allocatable :: expr_3_1
character(len=:), allocatable :: expr_4_0
character(len=:), allocatable :: expr_4_1
character(len=:), allocatable :: expr_4_2
character(len=:), allocatable :: expr_5_0
character(len=:), allocatable :: expr_6_0
character(len=:), allocatable :: expr_7_0
character(len=:), allocatable :: expr_8_0
        integer :: i

        savePoint = cursor
        
        do i = 0, 9
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. acceptString('a')) cycle
                expr_0_0 = consumeInput()

            copyCursor = cursor
            if ( acceptString('-')) then
                cursor = copyCursor
                cycle
            end if
            cursor = copyCursor
expr_0_1 = peg_num()
                if (.not. acceptEOF()) cycle
                
            res = toStr(expr_0_0)//toStr(expr_0_1);
        

                exit
            

            case(1)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. acceptString('b')) cycle
                expr_1_0 = consumeInput()

            copyCursor = cursor
            if (.not. acceptString('-')) cycle
            !cursor = copyCursor
expr_1_1 = peg_num()
                if (.not. acceptEOF()) cycle
                
            res = toStr(expr_1_0)//toStr(expr_1_1);
        

                exit
            

            case(2)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. acceptString('c')) cycle
                expr_2_0 = consumeInput()

                lexemeStart = cursor
                if(.not. acceptPeriod()) cycle
                expr_2_1 = consumeInput()
                if (.not. acceptEOF()) cycle
                
            res = toStr(expr_2_0)//toStr(expr_2_1);
        

                exit
            

            case(3)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. acceptString('d')) cycle
                expr_3_0 = consumeInput()

                lexemeStart = cursor

                expr_3_1 = peg_num()
                if (lexemeStart == cursor) cycle

                isTerminal = .false.
                do while (.not. cursor > len(input))
                    copyCursor = cursor
                    expr_3_1 = expr_3_1 // peg_num()
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
                
            res = toStr(expr_3_0)//toStr(expr_3_1);
        

                exit
            

            case(4)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. acceptString('e')) cycle
                expr_4_0 = consumeInput()

                lexemeStart = cursor
                if(.not. (acceptSet([char(32)]))) cycle
                expr_4_1 = consumeInput()
expr_4_2 = peg_num()
                if (.not. acceptEOF()) cycle
                
            res = toStr(expr_4_0)//toStr(expr_4_2);
        

                exit
            

            case(5)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if (.not. matchExactRepetition(3, "f")) then
                    !call pegError()
                    cycle
                end if
                expr_5_0 = consumeInput()
            
                if (.not. acceptEOF()) cycle
                
            res = toStr(expr_5_0);
        

                exit
            

            case(6)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if (.not. matchExactRepetitionWithSeparator(3, "g", ', ')) then
                    !call pegError()
                    cycle
                end if
                expr_6_0 = consumeInput()
            
                if (.not. acceptEOF()) cycle
                
            res = toStr(expr_6_0);
        

                exit
            

            case(7)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if (.not. matchVariableRepetition(1, 3, "h")) then
                    !call pegError()
                    cycle
                end if
                expr_7_0 = consumeInput()
            
                if (.not. acceptEOF()) cycle
                
            res = toStr(expr_7_0);
        

                exit
            

            case(8)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if (.not. matchVariableRepetitionWithSeparator(1, 3, "i", ', ')) then
                    !call pegError()
                    cycle
                end if
                expr_8_0 = consumeInput()
            
                if (.not. acceptEOF()) cycle
                
            res = toStr(expr_8_0);
        

                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_pruebas

    ! Inicio de la gramatica


    function peg_num() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. (acceptRange('0', '9'))) cycle
                expr_0_0 = consumeInput()
                
                
            res = toStr(expr_0_0);
        

                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_num

    ! Inicio de la gramatica


    ! Inicio de las acciones
    

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
