subroutine findloc(vall,px,py)
    use value
    implicit none
    integer :: vall,px,py,i,j
    if(vall.eq.1) then
        do 10 i=1,nx
        do 10 j=1,ny
            if(numf(i,j).eq.S_player1) then
                px=i
                py=j
            endif
        10    enddo
    elseif(vall.eq.2) then
        do 11 i=1,nx
        do 11 j=1,ny
            if(numf(i,j).eq.S_player2) then
                px=i
                py=j
            endif
        11    enddo
    endif
    end