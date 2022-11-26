program main
	use value
	call system('cls')
	call mem_al
	call starting
	call mem_deal
	end
	
	
	!subroutine
	!subroutine
	!subroutine
	!subroutine
	!subroutine
	!subroutine
	
	
	!player의 x,y값을 찾아주는 subroutine
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
	
	! 공중에 떠 있는 플레이어의 위치를 보정해주는 서브루틴
	! 높은곳에서 떨어지면 int(2^(높이-2)-0.2)의 데미지를 입음
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
	
	!memory allocate
	subroutine mem_al
	use value
	implicit none
	allocate(numf(nx,ny),field(nx,ny))
	end
	
	!memory deallocate
	subroutine mem_deal
	use value
	implicit none
	if(allocated(numf)) deallocate(numf)
	if(allocated(field)) deallocate(field)
	end