!폭발 데미지와 이펙트등 폭발에 관련된 부분은 담당
!3떨어진곳 8뎀 2떨어진곳 15뎀 1떨어진곳 26뎀 적중 40뎀 
subroutine bang(tx,ty)
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
    integer :: tx,ty,temp(nx+6,ny+6),fieldtemp(nx,ny),backup(nx,ny),px,py,i,j
    backup=numf!백업
    ! 코드 축약할려면 할 수는 있는데 어차피 나 혼자 코딩하는거라
    ! 걍 대충 하나하나 다 함
    temp=0
    tx=tx+3
    ty=ty+3
    temp(tx,ty)=4
    
    temp(tx-1,ty)=3
    temp(tx,ty+1)=3
    temp(tx+1,ty)=3
    temp(tx,ty-1)=3
    
    temp(tx-2,ty)=2
    temp(tx-1,ty+1)=2
    temp(tx+1,ty+1)=2
    temp(tx+2,ty)=2
    temp(tx+1,ty-1)=2
    temp(tx-1,ty-1)=2
    
    temp(tx-3,ty)=1
    temp(tx-2,ty+1)=1
    temp(tx+2,ty+1)=1
    temp(tx+3,ty)=1
    temp(tx+2,ty-1)=1
    temp(tx-2,ty-1)=1
    do 11 i=1,nx
    do 11 j=1,ny
        fieldtemp(i,j)=temp(i+3,j+3)
    11    enddo
    do 12 i=1,nx
    do 12 j=1,ny
        if(fieldtemp(i,j).NE.0.and.numf(i,j).EQ.S_player1) then
            select case(fieldtemp(i,j))
                case(1)
                    hp1=hp1-8
                case(2)
                    hp1=hp1-15
                case(3)
                    hp1=hp1-26
                case(4)
                    hp1=hp1-40
            end select
        endif
        if(fieldtemp(i,j).NE.0.and.numf(i,j).EQ.S_player2) then
            select case(fieldtemp(i,j))
                case(1)
                    hp2=hp2-8
                case(2)
                    hp2=hp2-15
                case(3)
                    hp2=hp2-26
                case(4)
                    hp2=hp2-40
            end select
        endif
    12    enddo
    where(fieldtemp.ne.0)
        backup=S_sky
    endwhere
    call findloc(1,px,py)
    backup(px,py)=S_player1
    call findloc(2,px,py)
    backup(px,py)=S_player2
    numf=backup
    numf(:,:2)=S_water
    call accpos
    call showfield
    end