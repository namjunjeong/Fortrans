subroutine setplayer
    use value
    implicit none
    integer :: ri,top
    integer,allocatable :: temp(:)
    allocate(temp(ny))
    call timeshow
    call srand(seed)
    ri=rand()
    ri=mod(int(10*rand()),5)+1
    do i=1,ny
        temp(i)=numf(ri,i)
    enddo
    write(*,'(20i1)') temp
    write(*,'(20i1)') numf(ri,:)
    top=minloc(temp-S_topland)
    numf(ri,top+1)=S_player1
    write(*,*) ri,top
    ri=mod(int(10*rand()),5)+1
    ri=nx-ri
    temp=numf(ri,:)
    top=minloc(abs(temp-S_topland),1)
    numf(ri,top+1)=S_player2
    if(allocated(temp)) deallocate(temp)
    end