! 0:land, 1:sky 2:water 

subroutine showfield
	use value
	implicit none
	integer :: i,j,temphp1,temphp2
	where(numf.eq.S_land)
		field='@'
	elsewhere(numf.eq.S_sky)
		field=' '
	elsewhere(numf.eq.S_water)
		field='~'
	elsewhere(numf.eq.S_player1)
		field='O'
	elsewhere(numf.eq.S_player2)
		field='X'
	elsewhere(numf.eq.S_proj)
		field='.'
	endwhere
	write(*,'(a78)') "=============================================================================="
	do j=ny,1,-1
		write(*,"('|',76a1,'|')") field(:,j)
	enddo
	write(*,'(a78)') "=============================================================================="
	temphp1=floor(hp1/5.0)
	temphp2=floor(hp2/5.0)
	if(temphp1.le.19) then
		do i=1,temphp1
			barhp1(i:i)='O'
		enddo
		do i=temphp1+1,20
			barhp1(i:i)='X'
		enddo
	elseif(temphp1.gt.19) then
		do i=1,20
			barhp1(i:i)='O'
		enddo
	endif
	if(temphp2.le.19) then
		do i=1,temphp2
			barhp2(i:i)='O'
		enddo
		do i=temphp2+1,20
			barhp2(i:i)='X'
		enddo
	elseif(temphp2.gt.19) then
		do i=1,20
			barhp2(i:i)='O'
		enddo
	endif
	write(*,'(a1,a14,a20,8x,a14,a20,a1)') "|","hp of player1:",barhp1,"hp of player2:",barhp2,"|"
	write(*,'(a78)') "=============================================================================="
	end