subroutine makefield
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
	!rand 함수를 이용해 1~10를 시작으로 함
	!rand 함수를 이용해 +1,0,-1 y 값을 증감시켜 나머지 땅 생성
	integer :: last_point,ri,i,j
	numf=S_sky
	call timeshow
	call srand(seed)
	last_point=rand()
	last_point=int(10*rand())+3
	do i=1,nx
		numf(i,:last_point)=S_land
		ri=mod(int(10*rand()),5)
		select case(ri)
			case(0)
				last_point=last_point-landh_2
			case(1)
				last_point=last_point-landh_1
			case(2)
				last_point=last_point
			case(3)
				last_point=last_point+landh_1
			case(4)
				last_point=last_point+landh_2
		endselect
		do while(last_point.lt.3)
			last_point=last_point+1
			ri=mod(int(10*rand()),3)+2
			select case(ri)
				case(0)
					last_point=last_point+landh_2
				case(1)
					last_point=last_point+landh_1
				case(2)
					last_point=last_point
				case(3)
					last_point=last_point+landh_1
				case(4)
					last_point=last_point+landh_2
			endselect
		enddo
		do while(last_point.gt.13)
			last_point=last_point-1
			ri=mod(int(10*rand()),3)
			select case(ri)
				case(0)
					last_point=last_point-landh_2
				case(1)
					last_point=last_point-landh_1
				case(2)
					last_point=last_point
				case(3)
					last_point=last_point-landh_1
				case(4)
					last_point=last_point-landh_2
			endselect
		enddo
		numf(i,:2)=S_water
		call system('cls')
		call showfield
		write(*,'(a78)') "--------------------------------making field..--------------------------------"
		call usleep(5000_c_int32_t)
	enddo
	call system('cls')
	end