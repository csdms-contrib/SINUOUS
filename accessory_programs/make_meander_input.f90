program make_meander_input
use ifport
implicit none
!   MAKE_MEANDER_INPUT - Creates an input file of channel X,Y coordinates
!Copyright (C) 2021 Alan D. Howard
!Developer can be contacted by ahoward@psi.edu
!This program is free software; you can redistribute it and/or modify it under the terms of the
 !GNU General Public License as published by the Free Software Foundation; either version 3
 !of the License, or (at your option) any later version. 
!This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
! without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
! See the GNU General Public License for more details. 
!You should have received a copy of the GNU General Public License along with this program;
! if not, write to the Free Software Foundation, Inc.,
real*4 x,y, elapse, resist, elev, width,vertical
integer i,isize
width=50.0
elapse=0.0
resist=1.0
elev=0.0
open(5,file='make_meander.prm',action='read')
read(5,*) isize
read(5,*) width
read(5,*) vertical
open(10,file='meander.dat',action='write')
x=width*isize
y=vertical*random(0)
write(10,200) x,y
200 format(2(G12.5,' '))
do i=1,isize
x=x-width
y=vertical*random(0)
write(10,200) x,y
enddo
close(10)
stop
end



