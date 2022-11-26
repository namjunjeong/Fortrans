module value
    implicit none
    integer :: time(8),seed,landh_1,landh_2,whatland,hp1=100,hp2=100,turn=1,damage
    integer,parameter :: S_land=0,S_sky=1,S_water=2,S_player1=3,S_player2=4,S_proj=6
    integer,parameter :: superplain=0,plain=1,valley=2,supervalley=3,tmode=4
    integer,parameter :: nx=76,ny=16
    integer,allocatable :: numf(:,:)
    double precision,parameter :: pi=4.0*atan(1.0d0),grav=9.8d0
    character,allocatable :: field(:,:)
    character(len=20) :: barhp1,barhp2
    
    contains
        subroutine timeshow
            call date_and_time(values=time)
            seed=time(8)
        end
    end