
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
character(len=:), allocatable :: expr_0_3
character(len=:), allocatable :: expr_0_4
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                
                expr_0_0 = peg_protocol()

                lexemeStart = cursor
                if(.not. acceptString('://')) cycle
                expr_0_1 = consumeInput()
expr_0_2 = peg_domain()

                lexemeStart = cursor
                if (.not. acceptString('/')) exit
                expr_0_3 = consumeInput()

                lexemeStart = cursor

                expr_0_4 = ""
                isTerminal = .false.
                do while (.not. cursor > len(input))
                    copyCursor = cursor
                    expr_0_4 = expr_0_4 // peg_path()
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
                
                res = toStr(expr_0_0)//toStr(expr_0_1)//toStr(expr_0_2)//toStr(expr_0_3)//toStr(expr_0_4)


                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_s

    ! Inicio de la gramatica


    function peg_protocol() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if(.not. acceptString('http')) cycle
                expr_0_0 = consumeInput()

                lexemeStart = cursor
                if (.not. acceptString('s')) exit
                expr_0_1 = consumeInput()
                
                
                res = toStr(expr_0_0)//toStr(expr_0_1)


                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_protocol

    ! Inicio de la gramatica


    function peg_domain() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if (.not. (acceptSet(['.','-']) .or. acceptRange('a', 'z') .or. acceptRange('A', 'Z') .or. acceptRange('0', '9'))) cycle

                do while (.not. cursor > len(input))
                    if (.not. (acceptSet(['.','-']) .or. acceptRange('a', 'z') .or. acceptRange('A', 'Z') .or. acceptRange('0', '9'))) exit
                end do

                expr_0_0 = consumeInput()
                
                
                res = toStr(expr_0_0)


                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_domain

    ! Inicio de la gramatica


    function peg_path() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
                lexemeStart = cursor
                if (.not. (acceptSet(['.','_','/','-']) .or. acceptRange('a', 'z') .or. acceptRange('A', 'Z') .or. acceptRange('0', '9'))) cycle

                do while (.not. cursor > len(input))
                    if (.not. (acceptSet(['.','_','/','-']) .or. acceptRange('a', 'z') .or. acceptRange('A', 'Z') .or. acceptRange('0', '9'))) exit
                end do

                expr_0_0 = consumeInput()
                
                
                res = toStr(expr_0_0)


                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_path

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
end module parser
