subroutine setplayer
    use value
    implicit none
    integer :: ri,top,i,j
    integer,allocatable :: temp(:)
    allocate(temp(ny))
    call timeshow
    call srand(seed)
    ri=rand()
    ri=mod(int(10*rand()),5)+1
    do i=1,ny
        temp(i)=numf(ri,i)
    enddo
    do i=ny,1,-1
        if(temp(i).eq.S_land) then
            top=i
            exit
        endif
    enddo
    numf(ri,top+1)=S_player1
    ri=mod(int(10*rand()),5)+1
    ri=nx-ri
    do i=1,ny
        temp(i)=numf(ri,i)
    enddo
    do i=ny,1,-1
        if(temp(i).eq.S_land) then
            top=i
            exit
        endif
    enddo
    numf(ri,top+1)=S_player2
    if(allocated(temp)) deallocate(temp)
    end