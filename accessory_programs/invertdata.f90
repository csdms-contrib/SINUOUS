program invert_input_data
implicit none
!   INVERT_INPUT_DATA - invertw order of X,Y coordinates
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
real*8 x(10000),y(10000)
real*8 gradient,elevation,resistance,time
integer i, itot
character*40 params,infile,outfile
open(10,file='invertdata.prm',action='read')
!read(10,*) gradient
500 continue
read(10,100,end=600) infile
100 format(a)
read(10,100) outfile
infile=trim(infile)
outfile=trim(outfile)

open(20,file=infile,action='read')
open(30,file=outfile,action='write')
123 continue
X=0.0
Y=0.0
read(20,*)time,itot
write(*,222) itot
222 format('itot=',i10)
do i=1, itot
   read(20,*) x(i), y(i)
   !itot=itot+1
enddo
!write(*,333) x(i)
333 format(g12.5)
!write(*.222) itot
if (.not.eof(20)) goto 123
200 continue
    close(20)
	elevation=0.0
    gradient=0.0
    resistance=1.0
do i=itot,1,-1
    !if (i.gt.1) elevation=elevation+dsqrt((x(i)-x(i-1))**2+(y(i)-y(i-1))**2)*gradient
   write(30,300) x(i), y(i)!, elevation
 300 format(g13.6,' ',g13.6)!,' 0.0 1.0 ',g13.6 )

 enddo
 close(30)
 goto 500
600 continue
    close (10)
 stop
 end
