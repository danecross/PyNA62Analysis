       subroutine FiltIni
c
c read coefficents for Digital Filter
c
       implicit none
       integer nref_max
       parameter(nref_max=20)
       real a(nref_max,4,-25:25,3)
       real b(nref_max,4,-25:25,3)
       real a2(nref_max,4,-40:40,2)
       real b2(nref_max,4,-40:40,2)
       real a3(nref_max,4,-40:20,2)
       real b3(nref_max,4,-40:20,2)
       real aux(0:5)
       integer Icell_shape(0:127,0:127),array(0:15)
       integer tempX,tempY1,tempY2,RefSet,Phase
       common/filCDE/Icell_shape,a,b,a2,b2,a3,b3
       integer LKFIS,Nref,Nword,Nhead,temp1,temp2,temp3
       integer Itype,Iref,ig,is,it,ix,iy,iy1,iy2
       CHARACTER*8 H1
       CHARACTER*10 H2, H3*80
       CHARACTER*255 FILENAME
       CHARACTER*255 FILENAME2
       COMMON /LKRDIGIFILTER/ FILENAME, FILENAME2

c
c read bank giving the reference shape for each of the calo cell
c
#ifdef OLDCONS
CCC       OPEN(unit=5,file='LKr/cellshape_data.txt')
       OPEN(unit=5,file=FILENAME2)
       do ix=0,127
          do iy=0,127
              READ(5,*) Icell_shape(ix,iy)
          enddo
       enddo
       CLOSE(5)
c
c read all reference shapes, with either 3 or 2 samples
c
       OPEN(unit=7,file=FILENAME)
       LKFIS = 0
       do while(LKFIS.eq.0)
           READ(7,*,IOSTAT=LKFIS) Itype,Nref,Nword,Nhead
           if (LKFIS.eq.0) then
             do Iref=1,Nref
               do ig=1,4
                  if (itype.eq.1) then
                     do it=-25,25
                        do is=1,3
                            READ(7,*,IOSTAT=LKFIS) a(Iref,ig,It,is)
                            READ(7,*,IOSTAT=LKFIS) b(Iref,ig,It,is)
                        enddo
                      enddo
                  elseif (itype.eq.2) then
                     do it=-40,40
                        do is=1,2
                            READ(7,*,IOSTAT=LKFIS) a2(Iref,ig,It,is)
                            READ(7,*,IOSTAT=LKFIS) b2(Iref,ig,It,is)
                        enddo
                      enddo
                  elseif (itype.eq.3) then
                     do it=-40,20
                        do is=1,2
                            READ(7,*,IOSTAT=LKFIS) a3(Iref,ig,It,is)
                            READ(7,*,IOSTAT=LKFIS) b3(Iref,ig,It,is)
                        enddo
                      enddo
                  endif
               enddo
             enddo
         endif
       enddo                    
       CLOSE(7)
#else
       OPEN(unit=5,file=FILENAME2)
       READ(5,1001) H1,Itype,Nref,Nword,H2
       READ(5,'(a)') H3
       READ(5,'(a)') H3
       READ(5,'(a)') H3
 1001  FORMAT(A,3I6,4X,A)
       do ix=0,127
          do iy1=0,127, 16
             iy2 = iy1 + 15
              READ(5,500) tempX, tempY1, tempY2,array
 500          format(3I5, 1X, 16I4)
              DO iy=0,15
                 Icell_shape(tempX,tempY1+iy) = array(iy)
              enddo
           enddo
        enddo
       CLOSE(5)

       OPEN(unit=7,file=FILENAME)
       READ(7,1002) H1,Itype,Nref,Nword,H2
       READ(7,'(a)') H3
       READ(7,'(a)') H3
       READ(7,'(a)') H3
 1002  FORMAT(A,3I6,4X,A)
       do Iref=1,Nref
          do ig=1,1
             do it=-25,25
                READ(7,550) RefSet,Phase,aux
 550            format(2I5, 2X, 6F10.3)
                do is=0,2
                   a(Iref,ig,it,is+1) = aux(is)
                   b(Iref,ig,it,is+1) = aux(is+3)
                enddo
             enddo
          enddo
       enddo
       CLOSE(7)
#endif       
      return
      end
