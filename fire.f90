subroutine fire
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
    
    integer :: px,py,t,i,j,tx,ty,step,acc!tx,ty 포탄의 loc
    double precision :: calx=1.0d0,caly1,calyp,calym,theta,v0!px,py:플레이어 loc. calx,caly는 포물선 궤도 계산
    write(*,'( "please input fire angle!(10~90,-10~-90) : ")',advance='no')
    read(*,*) theta
    do while(theta.gt.90.or.theta.lt.10.and.theta.gt.-10.or.theta.lt.-90)
    write(*,'("please input theta between 10~90,-10~-90!! : ")',advance='no') 
    read(*,*) theta
    enddo
    write(*,'( "please input fire power! : ")',advance='no')
    read(*,*) v0
    do while(v0.LE.0)
        write(*,'( "please input value larger than 0! : ")',advance='no')
        read(*,*) v0
    enddo
    theta=theta*pi/180
    call findloc(turn,px,py)
    tx=px
    do
        calx=real((tx-px))*2/7
        caly1=(tan(theta)*calx)-((grav*calx**2)/(2*v0**2*cos(theta)**2))
        calyp=(tan(theta)*(real((tx+1-px))*2/7))-((grav*(real((tx+1-px))*2/7)**2)/(2*v0**2*cos(theta)**2))
        calym=(tan(theta)*(real((tx-1-px))*2/7))-((grav*(real((tx-1-px))*2/7)**2)/(2*v0**2*cos(theta)**2))
        if(theta.lt.0) then
            tx=tx-1
            if(nint(caly1).LT.nint(calym)) then
                step=1
                acc=1
            else if(nint(caly1).EQ.nint(calym)) then
                step=1
                acc=0
            else
                step=-1
                acc=-1
            endif
            do ty=py+nint(caly1)+acc,py+nint(calym),step
            if(tx.GE.nx+1.or.tx.LE.0) then
                call system('cls')
                call showfield
                write(*,'(a18)') "nothing happened!!"
                goto 1
            endif
            if(ty.LE.ny) then
                if(numf(tx,ty).EQ.S_water) then
                    call system('cls')
                    call showfield
                    write(*,'(a18)') "nothing happened!"
                    goto 1
                endif
                if(numf(tx,ty).eq.s_land.or.numf(tx,ty).eq.S_player2.or.numf(tx,ty).eq.S_player1) then
                    call system('cls')
                    call bang(tx,ty)
                    goto 1
                endif
                numf(tx,ty)=S_proj
            endif
            call system('cls')
            call showfield
            call usleep(10000_c_int32_t)
            enddo
        else
            tx=tx+1
            if(nint(caly1).LT.nint(calyp)) then
                step=1
                acc=1
            else if(nint(caly1).EQ.nint(calyp)) then
                step=1
                acc=0
            else
                step=-1
                acc=-1
            endif
            do ty=py+nint(caly1)+acc,py+nint(calyp),step
            if(tx.GE.nx+1.or.tx.LE.0) then
                call system('cls')
                call showfield
                write(*,'(a18)') "nothing happened!!"
                goto 1
            endif
            if(ty.LE.ny) then
                if(numf(tx,ty).EQ.S_water) then
                    call system('cls')
                    call showfield
                    write(*,'(a18)') "nothing happened!"
                    goto 1
                endif
                if(numf(tx,ty).eq.s_land.or.numf(tx,ty).eq.S_player2.or.numf(tx,ty).eq.S_player1) then
                    call system('cls')
                    call bang(tx,ty)
                    goto 1
                endif
                numf(tx,ty)=S_proj
            endif
            call system('cls')
            call showfield
            call usleep(10000_c_int32_t)
            enddo
        endif
    
    enddo
    1    continue
    where(numf.eq.S_proj) numf=S_sky!temp
    end