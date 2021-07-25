   program combine_digit
   implicit none
!   COMBINE_DIGIT - Combine 3 digitized centelines into single file
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
 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. }
   real*8 x1(4000), x2(4000),x3(4000),y1(4000),y2(4000),y3(4000)
   real*8 xn(4000),yn(4000),xaverage,yaverage,naverage
   integer nsamp1,nsamp2,nsamp3,nsampn
   character*30 input1,input2,input3,directory,output
   character*60 file1,file2,file3,outfile
   integer n,n1,n2,ncombine
   real*8 maxdistance
   real*8 distance,d1,d2
   open(10,file='combine_digit.prm',action='read')
   read(10,100) directory
   read(10,100) input1
   read(10,100) input2
   read(10,100) input3
   read(10,100) output
   read(10,*) maxdistance
100 format(a)
   file1=trim(directory)//trim(input1)
   file2=trim(directory)//trim(input2)
   file3=trim(directory)//trim(input3)
   outfile=trim(directory)//trim(output)
   open(20,file=file1,action='read')
   open(22,file=file2,action='read')
   open(24,file=file3,action='read')
   open(30,file=outfile,action='write')
   open(40,file='combine_results.prn',action='write')
   call readdata(20,x1,y1,nsamp1)
   call readdata(22,x2,y2,nsamp2)
   call readdata(24,x3,y3,nsamp3)
   write(*,500) nsamp1,nsamp2,nsamp3
500 format('n1=',i5,' n2=',i5,' n3=',i5)   
   do n=1,nsamp1
       xaverage=x1(n)
       yaverage=y1(n)
       naverage=1.0
       !xcomp=x1(n)
       !ycomp=y1(n)
       do n1=1,nsamp2
          d1=distance(x1(n),y1(n),x2(n1),y2(n1))
          !write(40,600) d1
600  format('d1='g12.5)
          if (d1<maxdistance) then
              xaverage=xaverage+x2(n1)
              yaverage=yaverage+y2(n1)
              naverage=naverage+1.0
          endif
       enddo
       do n2=1,nsamp3
          d2=distance(x1(n),y1(n),x3(n2),y3(n2))
          !write(40,600) d2
          if (d2<maxdistance) then
              xaverage=xaverage+x3(n2)
              yaverage=yaverage+y3(n2)
              naverage=naverage+1.0
          endif
       enddo
   write(40,300) n,naverage
300 format(' n=',i6,' nsamp=',g12.5)
   xaverage=xaverage/naverage
   yaverage=yaverage/naverage
   write(30,400) xaverage,yaverage
400 format(g14.7,',',g14.7)   
   enddo
   close(30)
   close(40)
   stop
   end
       
   
   
   
   
   real*8 function distance (x1,y1,x2,y2)
   implicit none
   real*8 x1,y1,x2,y2
      distance=sqrt((x1-x2)**2+(y1-y2)**2)
     ! write(*,100) distance
100 format(g12.5)
   return
   end   
   
   subroutine readdata(iunit,x,y,nnn)
   implicit none
   real*8 x(4000),y(4000)
   integer iunit
   integer nnn
   character*20 garbage
   !read(iunit,100) garbage
100  format (a)
   nnn=0
   do
      nnn=nnn+1
      read(iunit,*,end=200) x(nnn),y(nnn)
   enddo
200 continue
   close(iunit)
   nnn=nnn-1
   return
   end   
