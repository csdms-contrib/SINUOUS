    program test_fit
 !   TEST_FIT - Compares fit of simulated stream to a natural stream
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
    ! ********************************************************************************
    !   This program calculates the area between two curves which may intersect multiple times
    !   It is primarily used to judge the fit between simulated and actual stream meanders
    !   It is set up to read a single file of x,y values which are the actual meander centerline to fit
    !   A second file is then read, which is a series of simulated meander centerlines as x,y pairs.
    !   Each simulated case is preceded by an integer which is the simulation iteration and a number which gives
    !      the number of x,y pairs to be read for this case.  Several cases can occur in the second file
    ! ********************************************************************************
    implicit none
    real(8) :: xa1,xa2,xb1,xb2,ya1,ya2,yb1,yb2,xi,yi,Min_distance,sum1,sum2
    logical :: does_intersect
    real(8) :: x1(20000),y1(20000),x2(20000),y2(20000),x3(20000),y3(20000),distance(20000)
    real(8) :: xl,yl,xu,yu,suma(20000),sumall,iter,minerror,miniter
    integer :: n1,n2,n3,i,j,k,idist,ilast,jlast,ii(20000),jj(20000)
    character(40) :: inname1,inname2,infile1,infile2
    x1=0.0
    y1=0.0
    minerror=1.0e+35
    ! ********************************************************************************
    !   The parameter file consists of the name of the two centerline input files
    !   It also reads a parameter min_distance that is the minimum distance between 
    !      successive centerline crossovers that will be evaluated
    ! ********************************************************************************
     open(40,file='test_fit.prm',action='read')
    read(40,2998) inname1
    read(40,2998) inname2
2998 format(a)
    read(40,*) min_distance
    !min_distance=8.0
    infile1=trim(inname1)
    infile2=trim(inname2)
    open(10,file=infile1,action='read')
    n1=0
    do !read x1(),y1() 
       n1=n1+1
       read(10,*,end=700) x1(n1),y1(n1)
    enddo !do x(1), y1()
700 n1=n1-1
    write(*,702) n1
702 format(' n1=',i5)
    open(20,file=infile2,action='read')
    ! *******************************************************************************
    !  diff_area.dat gives detailed information about the size of each area between cross-overs
    !  diff_result.dat just gives the total area between simulated and actual centerline as a function
    !    if iteration number
    !  This latter file is useful to identify the simulation time giving the best fit to the
    !    natural stream as well as, for various simulation runs with different parameters, the
    !    best combination of parameters that minimizes areas between the two curves
    ! *******************************************************************************
    open(30,file='diff_area.dat',action='write')
    open(40,file='diff_result.dat',action='write')
    do !cycle through iterations
    x2=0.0
    y2=0.0
    x3=0.0
    y3=0.0
    ii=0
    jj=0
    distance=0.0
    read(20,*,end=999) iter,n2    
    do i=1,n2
        read(20,*) x2(i),y2(i)
    enddo ! x2(),y2()
750 continue
    write(30,998) iter,n2
998 format(/,' iter=',i8,' nsamp=',i8)    
    n3=0
    ilast=0
    jlast=0
    xa1=x1(1)
    ya1=y1(1)
    ! *******************************************************************************
    !  Cycle through points in the first, test curve
    ! *******************************************************************************
    
    do i=2,n1  !cycle through n1 points
       xa2=x1(i)
       ya2=y1(i)
       xb1=x2(1)
       yb1=y2(1)
    ! *******************************************************************************\
    !  Cycle through the second, simulated curve
    ! *******************************************************************************
       do j=2,n2 !cycle through n2 points
          xb2=x2(j)
          yb2=y2(j)
    ! *******************************************************************************
    ! check intersect determines if two curves intersect.  The input parameters are
    !  for two curves, a and b, and two sets of points defining the endpoints of two
    !  curve segments. Does_intersect is true if the two line segments touch or cross
    !  The values xi and yi are the location of the intersection         
    ! *******************************************************************************
          call check_intersect(xa1,ya1,xa2,ya2,xb1,yb1,xb2,yb2,xi,yi,does_intersect)
          if (does_intersect) then
    ! *******************************************************************************
    ! Each intersection is cataloged. N3 is the pointer to the current crossing.
    !  ii and jj are the locations of the intersection along the first and second curve,
    !  respectively.  x3 and y3 are the locations of the intersection. distance is the 
    !  linear distance between the present and previous intersection location.  Two versions
    !  distance have been used, the euclidian distance and the distance measured in i and j
    !  values
    ! *******************************************************************************
             n3=n3+1
             ii(n3)=i
             jj(n3)=j
             x3(n3)=xi
             y3(n3)=yi
             if (n3>2) then
                distance(n3)=sqrt((x3(n3)-x3(n3-1))**2+(y3(n3)-y3(n3-1))**2)
             else
                distance(n3)=0.0
             endif
             if ((ilast.gt.0).and.(jlast.gt.0)) then
                distance(n3)= sqrt(float((i-ilast)**2+(jlast-j)**2))
             else
                distance(n3)=0.0
             endif
             ilast=i
             jlast=j
             !write(30,770) i,j,n3,xi,yi,distance(n3)
           !  write(*,780) i,j,n3,xi,yi,distance(n3)
770  format(i5,' ',i5,' ',i5,' ',i5,g12.5,' ',g12.5,' ',g12.5)
780  format('i=',i5,' j=',i5,' k=',i5' xi=',g15.8,' yi=',g15.8,' d=',g12.5)
          endif !does intersect
          xb1=xb2
          yb1=yb2
       enddo !cycle through n2 points
       xa1=xa2
       ya1=ya2
    enddo
    suma=0.0
    sumall=0.0
    ! *******************************************************************************
    !   Here we cycle through each pair of intersections, and calculate the area
    !     between the two curves if the separation exceeds min_distance.  If the line connecting
    !     the two intersections slopes (absolutely) less than 45 degrees the area is
    !     calculated relative to the x axis, otherwise relative to the y axis.
    ! *******************************************************************************
    do k=2,n3 !through intersections
        if (distance(k)>min_distance) then
           if (abs(x3(k)-x3(k-1))>abs(y3(k)-y3(k-1))) then
              xl=x3(k-1)
              yl=y3(k-1)
              sum1=0.0
                do i=ii(k-1)+1,ii(k)-1
                   xu=x1(i)
                   yu=y1(i)
                   sum1=sum1+(xu-xl)*(yl+yu)/2.0
                   xl=xu
                   yl=yu
                enddo
                xu=x3(k)
                yu=y3(k)
                sum1=sum1+(xu-xl)*(yl+yu)/2.0 
                sum2=0.0
                xl=x3(k-1)
                yl=y3(k-1)             
                do j=jj(k-1)+1,jj(k)-1
                   xu=x2(j)
                   yu=y2(j)
                   sum2=sum2+(xu-xl)*(yl+yu)/2.0
                   xl=xu
                   yl=yu
                enddo
                xu=x3(k)
                yu=y3(k)
                sum2=sum2+(xu-xl)*(yl+yu)/2.0
           else
              xl=x3(k-1)
              yl=y3(k-1)
              sum1=0.0
                do i=ii(k-1)+1,ii(k)-1
                   xu=x1(i)
                   yu=y1(i)
                   sum1=sum1+(yu-yl)*(xl+xu)/2.0
                   xl=xu
                   yl=yu
                enddo
                xu=x3(k)
                yu=y3(k)
                sum1=sum1+(yu-yl)*(xl+xu)/2.0
                sum2=0.0
                xl=x3(k-1)
                yl=y3(k-1)             
                do j=jj(k-1)+1,jj(k)-1
                   xu=x2(j)
                   yu=y2(j)
                   sum2=sum2+(yu-yl)*(xl+xu)/2.0
                   xl=xu
                   yl=yu
                enddo
                xu=x3(k)
                yu=y3(k)
                sum2=sum2+(yu-yl)*(xl+xu)/2.0                                            
           endif
    ! *******************************************************************************
    !  Here we record each enclosed area and the total area between curves
    ! *******************************************************************************
        suma(k)=abs(sum1-sum2)
        sumall=sumall+suma(k) 
       
       ! write(*,2300) k,sum1,sum2,suma(k),sumall        
        write(30,997) k,suma(k)
2300 format('k=',i5,' sum1=',g12.5,' sum2=',g12.5,' area=',g12.5,' sumarea=',g12.5)        
 997 format('k=',i5,' area=',g12.5)
        endif        
    enddo
    
    write(30,996) sumall
    write(40,994) iter,sumall
994 format(g12.6,',',g14.7)
996 format(' total enclosed area=',g12.5)
    if (sumall.lt.minerror) then
            minerror=sumall
            miniter=iter
    endif
    enddo
999 continue
    write(*,899) minerror,miniter
899 format('minerror=',g13.6,' miniter=',g13.6)
     close(30) 
     close(40)                     
    stop
    end     


    subroutine check_intersect(xa1,ya1,xa2,ya2,xb1,yb1,xb2,yb2,xi,yi,does_intersect)
    implicit none
    real(8) :: xa1,xa2,xb1,xb2,ya1,ya2,yb1,yb2,xi,yi
    real(8) :: aslope,bslope,ainter,binter
    logical :: does_intersect,avertical,bvertical
    ! *******************************************************************************
    !  This tests whether two line segments intersect or have common endpoints, returning
    !    the x ay y locations of the intersection and a logical value which is true if
    !    the curves intersect, otherwise false.
    !  There are several cases depending upon the orientations of the two line segments
    !  xa1,ya1 and xa2, ya2 are the coordinates of the first line segment
    !  xb1,yb1 and xb2, yb2 are the coordinates of the second line segment
    ! *******************************************************************************
    
    does_intersect=.false.
    avertical=.false.
    bvertical=.false.
    xi=0.0
    yi=0.0
    if ((xa1==xa2).and.(ya1==ya2)) return
    if ((xb1==xb2).and.(yb1==yb2)) return    
    if((xa1==xb1).and.(ya1==yb1)) then
        does_intersect=.true.
        xi=xa1
        yi=ya1
    endif
    if((xa2==xb2).and.(ya2==yb2)) then
        does_intersect=.true.
        xi=xa2
        yi=ya2
    endif
    if((xa1==xb2).and.(ya1==yb2)) then
        does_intersect=.true.
        xi=xa1
        yi=ya1
    endif
    if((xa2==xb1).and.(ya2==yb1)) then
        does_intersect=.true.
        xi=xa2
        yi=ya2
    endif
    
    if (does_intersect) return
    if (xa1==xa2) then
       avertical=.true.
       xi=xa1
    else
       aslope=(ya1-ya2)/(xa1-xa2)
       ainter=ya1-aslope*xa1
    endif
    if (xb1==xb2) then
       bvertical=.true.
       xi=xb1
    else
       bslope=(yb1-yb2)/(xb1-xb2)
       binter=yb1-bslope*xb1
    endif
    if (avertical.and.bvertical) return
    if (avertical.or.bvertical) then
    else
        if (aslope==bslope) return
    endif
    if (avertical) then
        yi=bslope*xi+binter
    else
       if (bvertical) then
          yi=aslope*xi+ainter
       else
           xi=(binter-ainter)/(aslope-bslope)
           yi=aslope*xi+ainter
       endif
    endif
    if (((xi>=xa1).and.(xi<=xa2)).or.((xi>=xa2).and.(xi<=xa1))) then
        if(((yi>=ya1).and.(yi<=ya2)).or.((yi>=ya2).and.(yi<=ya1))) then
            if (((xi>=xb1).and.(xi<=xb2)).or.((xi>=xb2).and.(xi<=xb1))) then
                if(((yi>=yb1).and.(yi<=yb2)).or.((yi>=yb2).and.(yi<=yb1))) then        
                    does_intersect=.true.
                    return
                else
                    return
                endif
            else
                return
            endif
        else
           return
        endif
    else
       return
    endif
    return
    end
