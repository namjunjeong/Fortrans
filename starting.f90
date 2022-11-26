subroutine starting
use value
implicit none
write(*,*) "please write type of land"
write(*,*) "superplain:0, plain:1, valley:2, supervalley:3"
read(*,*) whatland
call fieldtype
end