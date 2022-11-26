subroutine game
    use value
    implicit none
    call makefield
    call setplayer
    call showfield
    do while(.true.)
        call move
        call system('cls')
        call showfield
        call fire
        if(hp1.LE.0) then
            call finishgame(S_player2)
        elseif(hp2.LE.0) then
            call finishgame(S_player1)
        endif
        if(turn.eq.1) then
            turn=2
        elseif(turn.eq.2) then
            turn=1
        endif
        write(*,'(a78)') "================================turn changed!!================================"
        write(*,'(a38,i1,a39)') "================================player",turn,"'s turn================================="
    enddo
    end