subroutine makefield
use value
implicit none
!rand 함수를 이용해 1~10를 시작으로 함
!rand 함수를 이용해 +1,0,-1 y 값을 증감시켜 나머지 땅 생성
integer :: last_point,ri
call timeshow
call srand(seed)
last_point=rand()
last_point=int(10*rand())+1
write(*,*) last_point
i=1
do i=1,nx
	numf(i,:last_point)=0
	ri=mod(int(10*rand()),3)
	select case(ri)
		case(0)
			last_point=last_point-1
		case(1)
			last_point=last_point
		case(2)
			last_point=last_point+1
	endselect
	do while(last_point.lt.1)
		last_point=last_point+1
		ri=mod(int(10*rand()),3)
		select case(ri)
			case(0)
				last_point=last_point-1
			case(1)
				last_point=last_point
			case(2)
				last_point=last_point+1
		endselect
	enddo
	do while(last_point.gt.10)
		last_point=last_point-1
		ri=mod(int(10*rand()),3)
		select case(ri)
			case(0)
				last_point=last_point-1
			case(1)
				last_point=last_point
			case(2)
				last_point=last_point+1
		endselect
	enddo
enddo
end