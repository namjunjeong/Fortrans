subroutine move
    use value
    implicit none
    integer :: mcount=5,moveval,col(ny),i,j,px,py,top
    do while(mcount.ne.0)!1 오른쪽 -1 왼쪽 0 move파트 종료
    1    continue
        call findloc(turn,px,py)
        write(*,'(a78)') "=============================================================================="
        write(*,"(a13,i2,1x,a62)") "|you can move",mcount,"time more                                                    |"
        write(*,'(a78)') "|1 for go right one pixel,-1 for left,0 for stop moving!                     |"
        write(*,'(a78)') "=============================================================================="
        write(*,'("please input value : ")',advance='no') 
        read(*,*) moveval
        do while(moveval.ne.-1.and.moveval.ne.0.and.moveval.ne.1)
            write(*,'( "please input value between -1,0.1! : ")')
            read(*,*) moveval
        enddo
        call system('cls')
        select case(moveval)
            case(-1)
            
                if(px.LE.1) then!왼쪽이 맵 끝일때
                    call showfield
                    write(*,'(a13)') "out of bound!"
                    goto 1
                endif
                
                col=numf(px-1,:)
                
                if(col(py).eq.S_sky) then!왼쪽이 비어있을때
                    numf(px,py)=S_sky
                    numf(px-1,py)=turn+2
                    mcount=mcount-1
                endif
                
                if(col(py).eq.S_land) then!왼쪽이 막혀있을때
                    if(col(py+1).eq.S_land.and.col(py+2).eq.S_land) then
                        write(*,'(a17)') "to high to go up!"
                        goto 1
                    endif
                    if(col(py+1).eq.S_sky) then!왼쪽막,왼쪽위 뚫
                        numf(px,py)=S_sky
                        numf(px-1,py+1)=turn+2
                        mcount=mcount-1
                    else if(col(py+2).eq.S_sky) then!왼쪽,왼쪽위막, 왼쪽위위 뚫
                        numf(px,py)=S_sky
                        numf(px-1,py+2)=turn+2
                        mcount=mcount-1
                    endif
                    if(col(py+1).eq.S_player1.or.col(py+1).eq.S_player2) then
                        if(col(py+2).eq.S_sky) then
                            numf(px,py)=s_sky
                            numf(px-1,py+2)=turn+2
                        elseif(col(py+2).eq.S_land) then
                            write(*,'(a18)') "you can't go there"
                            goto 1
                        endif
                    endif
                    if(col(py+2).eq.S_player1.or.col(py+1).eq.S_player2) then
                        write(*,'(a18)') "you can't go there"
                        goto 1
                    endif
                endif
                
                if(col(py).eq.S_player1.or.col(py).eq.S_player2) then!왼쪽에 플레이어가 있을때
                    if(col(py+1).ne.S_land) then
                        numf(px,py)=S_sky
                        numf(px-1,py+1)=turn+2
                        mcount=mcount-1
                    else
                        write(*,'(a18)') "you can't go there"
                        goto 1
                    endif
                endif
                
                
                
            case(1)
                if(px.GE.nx) then
                    call showfield
                    write(*,'(a13)') "out of bound!"
                    goto 1
                endif
                col=numf(px+1,:)
                
                if(col(py).eq.S_sky) then!오른쪽이 비어있을때
                    numf(px,py)=S_sky
                    numf(px+1,py)=turn+2
                    mcount=mcount-1
                endif
                
                if(col(py).eq.S_land) then!오른쪽이 막혀있을때
                    if(col(py+1).eq.S_land.and.col(py+2).eq.S_land) then
                        write(*,'(a17)') "to high to go up!"
                        goto 1
                    endif
                    if(col(py+1).eq.S_sky) then!오른쪽막,오른쪽위 뚫
                        numf(px,py)=S_sky
                        numf(px+1,py+1)=turn+2
                        mcount=mcount-1
                    else if(col(py+2).eq.S_sky) then!오른쪽,오른쪽위막, 오른쪽위위 뚫
                        numf(px,py)=S_sky
                        numf(px+1,py+2)=turn+2
                        mcount=mcount-1
                    endif
                    if(col(py+1).eq.S_player1.or.col(py+1).eq.S_player2) then
                        if(col(py+2).eq.S_sky) then
                            numf(px,py)=s_sky
                            numf(px+1,py+2)=turn+2
                        elseif(col(py+2).eq.S_land) then
                            write(*,'(a18)') "you can't go there"
                            goto 1
                        endif
                    endif
                    if(col(py+2).eq.S_player1.or.col(py+1).eq.S_player2) then
                        write(*,'(a18)') "you can't go there"
                        goto 1
                    endif
                endif
                
                if(col(py).eq.S_player1.or.col(py).eq.S_player2) then!오른쪽에 플레이어가 있을때
                    if(col(py+1).ne.S_land) then
                        numf(px,py)=S_sky
                        numf(px+1,py+1)=turn+2
                        mcount=mcount-1
                    else
                        write(*,'(a18)') "you can't go there"
                        goto 1
                    endif
                endif
            case(0)
                mcount=0
        endselect
    !체력 감소 구문 넣어야함
    call accpos
    call system('cls')
    call showfield
    enddo
    mcount=5
    end