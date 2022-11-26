subroutine fieldtype
use value
implicit none
if(whatland.eq.superplain) then
    landh_1=0
    landh_2=0
elseif(whatland.eq.plain) then
    landh_1=0
    landh_2=1
elseif(whatland.eq.valley) then
    landh_1=1
    landh_2=2
elseif(whatland.eq.supervalley) then
    landh_1=2
    landh_2=2
endif
end