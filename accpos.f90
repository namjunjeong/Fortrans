subroutine accpos
    use value
    implicit none
    integer :: px,py
    call findloc(1,px,py)
    if(numf(px,py-1).ne.S_land.and.numf(px,py-1).ne.S_water.and.&
    &numf(px,py-1).ne.S_player1.and.numf(px,py-1).ne.S_player2) then
    
        do while(numf(px,py-1).ne.S_land.and.numf(px,py-1).ne.S_water&
        &.and.numf(px,py-1).ne.S_player1.and.numf(px,py-1).ne.S_player2)
            numf(px,py-1)=S_player1
            numf(px,py)=S_sky
            py=py-1
            damage=damage+1
            call system('cls')
            call showfield
            call sleep(1)
            if(numf(px,py-1).eq.S_water) then
                call system('cls')
                call showfield
                write(*,*) "water bump!"
                call sleep(1)
                call finishgame(S_player2)
            endif
        enddo
        damage=int(2**(damage-2)-0.2)
        hp1=hp1-damage
        damage=0
    endif
    call findloc(2,px,py)
    if(numf(px,py-1).ne.S_land.and.numf(px,py-1).ne.S_water.and.&
    &numf(px,py-1).ne.S_player1.and.numf(px,py-1).ne.S_player2) then
    
        do while(numf(px,py-1).ne.S_land.and.numf(px,py-1).ne.S_water&
        &.and.numf(px,py-1).ne.S_player1.and.numf(px,py-1).ne.S_player2)
            numf(px,py-1)=S_player2
            numf(px,py)=S_sky
            py=py-1
            damage=damage+1
            call system('cls')
            call showfield
            call sleep(1)
            if(numf(px,py-1).eq.S_water) then
                call system('cls')
                call showfield
                write(*,*) "water bump!"
                call sleep(1)
                call finishgame(S_player1)
            endif
        enddo
        damage=int(2**(damage-2)-0.2)
        hp2=hp2-damage
        damage=0
    endif
    
    end