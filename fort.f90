module value
implicit none
integer :: i,j
integer :: nx,ny
integer,allocatable :: numf(:,:)
character,allocatable :: field(:,:)
end

program main
use value
nx=70
ny=15
call mem_al
do 11 i=1,nx
do 11 j=1,ny
	numf(i,j)=0
11	enddo
call showfield
call mem_deal
end






