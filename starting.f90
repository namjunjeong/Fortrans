subroutine starting
    use value
    implicit none
    integer :: start,omt=1,i
    character(len=30) :: s
    1    continue
    call system('cls')
    write(*,11)"=============================================================================="
    write(*,11)"|     @@@   @@@@   @@@@@@   @@@@@@@@@@ @@@@@@      @@    @@@     @@   @@@@@@ |"
    write(*,11)"|    @@    @@@@@@  @@    @@ @@@@@@@@@@ @@    @@   @@@@   @@ @    @@  @@@@    |"
    write(*,11)"|   @@    @@    @@ @@    @@     @@     @@    @@  @@  @@  @@ @@   @@ @@       |"
    write(*,11)"|@@@@@@@@ @@    @@ @@@@@@@      @@     @@@@@@@   @@  @@  @@  @   @@  @@@     |"
    write(*,11)"|@@@@@@@@ @@    @@ @@@@         @@     @@@@     @@@@@@@@ @@  @@  @@   @@@@@  |"
    write(*,11)"|   @@    @@    @@ @@ @@        @@     @@ @@    @@@@@@@@ @@   @  @@      @@@ |"
    write(*,11)"|   @@    @@    @@ @@  @@       @@     @@  @@   @@    @@ @@   @@ @@        @@|"
    write(*,11)"|   @@     @@@@@@  @@   @@      @@     @@   @@  @@    @@ @@    @ @@     @@@@ |"
    write(*,11)"|  @@       @@@@   @@    @@     @@     @@    @@ @@    @@ @@     @@@  @@@@@@  |"
    write(*,11)"=============================================================================="
    write(*,11)"|how to play:1 game start:2 adjust screen resolution:3                       |"
    write(*,11)"=============================================================================="
    write(*,'("please input number : ")',advance='no')
    read(*,*) start
    select case(start)
        case(1)
            write(*,11)"=============================================================================="
            write(*,11)"|this game is fortress made by fortran!                                       |"
            write(*,11)"|we support only multiplay. so you need two player!                           |"
            write(*,11)"|O : player1, X : player2, ~ : water, @ : land                                |"
            write(*,11)"|if you bumped into water, you will die immediately                           |"
            write(*,11)"|please enjoy the game!                                                       |"
            write(*,11)"|made by namjun jeong,kyunghee univ,nucleer engineering                       |"
            write(*,11)"=============================================================================="
            write(*,"(//)")
            write(*,11)"|press any key for return to main page                                        |"
            read(*,*) s
            goto 1
        case(2)
            goto 2
        case(3)
            write(*,*) "please adjust the resolution of cmd so that all characters fit on screen"
            call sleep(3)
            call system('cls')
            do while(omt.GE.1)
                do i=1,29
                    write(*,*)"=============================================================================="
                enddo
                call sleep(4)
                write(*,*) "one more time?(1 for yes, 0 for no)"
                read(*,*) omt
            enddo
            goto 1
        case(tmode)
            call testmode
    endselect
    2    continue
    write(*,11)"=============================================================================="
    write(*,11)"|please select type of land                                                  |"
    write(*,11)"|superplain:0, plain:1, valley:2, supervalley:3                              |"
    write(*,11)"=============================================================================="
    
    read(*,*) whatland
    select case(whatland)!if you input 4, call testmode
        case(superplain)
            landh_1=0
            landh_2=0
        case(plain)
            landh_1=0
            landh_2=1
        case(valley)
            landh_1=1
            landh_2=1
        case(supervalley)
            landh_1=1
            landh_2=2
    endselect
    call game
    11    format(a78)
    end