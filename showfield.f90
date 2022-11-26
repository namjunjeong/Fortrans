subroutine showfield
use value
implicit none
do j=ny,1,-1
	write(*,"(70i1)") numf(:,j)
enddo
end