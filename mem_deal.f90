subroutine mem_deal
use value
implicit none
if(allocated(numf)) deallocate(numf)
if(allocated(field)) deallocate(field)
end