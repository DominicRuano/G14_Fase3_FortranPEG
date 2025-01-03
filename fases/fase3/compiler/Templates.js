import { startingRuleEnabled } from "./Translator.js";

/**
 *
 * @param {{
*  beforeContains: string
*  afterContains: string
*  startingRuleId: string;
*  startingRuleType: string;
*  rules: string[];
*  actions: string[];
* }} data
* @returns {string}
*/
export const main = (data) => `
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

    ${data.beforeContains}

    contains

    ${data.afterContains}

    function parse(str) result(res)
        character(len=:), allocatable :: str
        ${data.startingRuleType} :: res

        input = str
        cursor = 1

        res = ${data.startingRuleId}()
    end function parse

    ! Inicio de las reglas
    ${data.rules.join('\n')}

    ! Inicio de las acciones
    ${data.actions.join('\n')}

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
`;

/**
*
* @param {{
*  id: string;
*  returnType: string;
*  exprDeclarations: string[];
*  expr: string;
* }} data
* @returns
*/
export const rule = (data) => `
    function peg_${data.id}() result (res)
        ${data.returnType} :: res
        ${data.exprDeclarations.join('\n')}
        integer :: i

        savePoint = cursor
        ${data.expr}
    end function peg_${data.id}

    ! Inicio de la gramatica
`;

/**
*
* @param {{
*  exprs: string[]
* }} data
* @returns
*/
export const election = (data) => `
        do i = 0, ${data.exprs.length}
            select case(i)
            ${data.exprs.map(
                (expr, i) => `
            case(${i})
                cursor = savePoint
                ${expr}
                exit
            `
            ).join('\n')}
            case default
                call pegError()
            end select
        end do
`;

/**
*
* @param {{
*  exprs: string[]
*  startingRule: boolean
*  resultExpr: string
* }} data
* @returns
*/
export const union = (data) => `
                ${data.exprs.join('\n')}
                ${data.startingRule && startingRuleEnabled ? 'if (.not. acceptEOF()) cycle' : ''}
                ${data.resultExpr}
`;

/**
*
* @param {{
*  expr: string;
*  destination: string
*  quantifier?: string;
* }} data
* @returns
*/
export const strExpr = (data) => {
    if (!data.quantifier) {
        return `
                lexemeStart = cursor
                if(.not. ${data.expr}) cycle
                ${data.destination} = consumeInput()`;
    }
    switch (data.quantifier) {
        case '+':
            return `
                lexemeStart = cursor
                if (.not. ${data.expr}) cycle

                do while (.not. cursor > len(input))
                    if (.not. ${data.expr}) exit
                end do

                ${data.destination} = consumeInput()`;
        case '*':
            return `
                lexemeStart = cursor

                do while (.not. cursor > len(input))
                    if (.not. ${data.expr}) exit
                end do

                ${data.destination} = consumeInput()`;
        case '?':
            return `
                lexemeStart = cursor
                if (.not. ${data.expr}) exit
                ${data.destination} = consumeInput()`;
        default:
            throw new Error(
                `'${data.quantifier}' quantifier needs implementation`
            );
    }
};


/**
*
* @param {{
    *  expr: string;
    *  destination: string
    *  quantifier?: string;
    * }} data
    * @returns
    */
export const idExpr = (data) => {
    if (!data.quantifier) {
        return `
                lexemeStart = cursor
                if(.not. ${data.expr}) exit
                ${data.destination} = consumeInput()`;
    }
    switch (data.quantifier) {
        case '+':
            return `
                lexemeStart = cursor

                ${data.destination} = ${data.expr}
                if (lexemeStart == cursor) cycle

                isTerminal = .false.
                do while (.not. cursor > len(input))
                    copyCursor = cursor
                    ${data.destination} = ${data.destination} // ${data.expr}
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
                isTerminal = .true.`;
        case '*':
            return `
                lexemeStart = cursor

                ${data.destination} = ""
                isTerminal = .false.
                do while (.not. cursor > len(input))
                    copyCursor = cursor
                    ${data.destination} = ${data.destination} // ${data.expr}
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
                isTerminal = .true.`;
        case '?':
            return `
                lexemeStart = cursor

                isTerminal = .false.
                copyCursor = cursor
                ${data.destination} = ${data.expr}
                if ( savePoint == cursor) then ! no se consumio nada en 0 o muchos
                    ! Si llego aqui ocurrio un falso positivo en los errores
                    ! por eso solo continua y lo activa al salir del if
                else if (copyCursor == cursor) then ! no se consumio nada luego de muchos
                    ! Si llego aqui ocurrio un error y los errores estan desactivados
                    ! Los reactiva y marca el error.
                    isTerminal = .true.
                    call pegError()
                    exit
                end if
                isTerminal = .true.`;
        case '~':
            return `
                integer :: i

                ${data.expr}`;
        default:
            throw new Error(
                `'${data.quantifier}' quantifier needs implementation`
            );
    }
};

/**
*
* @param {{
*  exprs: string[];
* }} data
* @returns
*/
export const strResultExpr = (data) => {
    if (data.exprs && data.exprs.length > 0) {
        return `
            res = ${data.exprs.map((expr) => `toStr(${expr})`).join('//')};
        `;
    }
    return `
            res = ""`;
};

/**
*
* @param {{
*  fnId: string;
*  exprs: string[];
* }} data
* @returns
*/
export const fnResultExpr = (data) => `
                res = ${data.fnId}(${data.exprs.join(', ')})
`;

/**
*
* @param {{
*  ruleId: string;
*  choice: number
*  signature: string[];
*  returnType: string;
*  paramDeclarations: string[];
*  code: string;
* }} data
* @returns
*/
export const action = (data) => {
    const signature = data.signature.join(', ');
    return `
    function peg_${data.ruleId}_f${data.choice}(${signature}) result(res)
        ${data.paramDeclarations.join('\n')}
        ${data.returnType} :: res
        ${data.code}
    end function peg_${data.ruleId}_f${data.choice}
    `;
};