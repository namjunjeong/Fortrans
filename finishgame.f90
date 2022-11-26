subroutine finishgame(whowin)
    use value
    use iso_c_binding
    implicit none
    interface
        subroutine usleep(useconds) bind(C)
            use iso_c_binding 
            implicit none 
            integer(c_int32_t), value :: useconds 
        end
    end interface
    integer :: whowin,i
    if(whowin.eq.S_player1) then
        do i=1,10
            call system("cls")
            write(*,*) "********************"
            write(*,*) "*congraturation!!!!*"
            write(*,*) "********************"
            write(*,*) "===================="
            write(*,*) "=player 1 win!!!!!!="
            write(*,*) "===================="
            call usleep(100000_c_int32_t)
            call system("cls")
            write(*,*) "===================="
            write(*,*) "=congraturation!!!!="
            write(*,*) "===================="
            write(*,*) "********************"
            write(*,*) "*player 1 win!!!!!!*"
            write(*,*) "********************"
            call usleep(100000_c_int32_t)
        enddo
    elseif(whowin.eq.S_player2) then
        do i=1,10
            call system("cls")
            write(*,*) "********************"
            write(*,*) "*congraturation!!!!*"
            write(*,*) "********************"
            write(*,*) "===================="
            write(*,*) "=player 2 win!!!!!!="
            write(*,*) "===================="
            call usleep(100000_c_int32_t)
            call system("cls")
            write(*,*) "===================="
            write(*,*) "=congraturation!!!!="
            write(*,*) "===================="
            write(*,*) "********************"
            write(*,*) "*player 2 win!!!!!!*"
            write(*,*) "********************"
            call usleep(100000_c_int32_t)	
        enddo
    endif
    stop
    end