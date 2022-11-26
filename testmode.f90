subroutine testmode
    use value
    implicit none
    integer :: stype,axisx1,axisy1,axisx2,axisy2,forp,i,j
    numf=S_sky
    do
        write(*,*) "fill=0,point=1,stop making=-1"
        read(*,*) forp
        if(forp.eq.0) then
            write(*,*) "land=0,sky=1,water=2,player1=3,player2=4,proj=5"
            read(*,*) stype
            write(*,*) "starting point"
            read(*,*) axisx1,axisy1
            write(*,*) "last point"
            read(*,*) axisx2,axisy2
            do 11 i=axisx1,axisx2
            do 11 j=axisy1,axisy2
                numf(i,j)=stype
            11    enddo
        elseif(forp.eq.1) then
            write(*,*) "land=0,sky=1,water=2,player1=3,player2=4,proj=5"
            read(*,*) stype
            if(stype.eq.-1) exit
            write(*,*) "axisx,axisy"
            read(*,*) axisx1,axisy1
            numf(axisx1,axisy1)=stype
        elseif(forp.eq.-1) then
            exit
        endif
    enddo
    call showfield
    do while(.true.)
        call move
        call system('cls')
        call showfield
        call fire
        if(hp1.LE.0.or.hp2.LE.0) then
            call finishgame
        endif
        if(turn.eq.1) then
            turn=2
        elseif(turn.eq.2) then
            turn=1
        endif
    
        write(*,*) "turn change"
    enddo
    end