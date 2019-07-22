       subroutine readinit() 
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
       integer Icell_shape(0:127,0:127)
       common/filCDE/Icell_shape,a,b,a2,b2,a3,b3
       integer LKFIS,nword,nhead
       integer Itype,Iref,is,it,ix,iy
       integer igain,ig,iter
       real pulse(4),Energy,Time
      INTEGER NCMAX
      PARAMETER(NCMAX=14000)
      INTEGER ICLMAX
      PARAMETER(ICLMAX=15)
      INTEGER IXLKR,IYLKR,CALDEAD
      INTEGER*8 IBADC,IBENE
      INTEGER CALNCEL
      INTEGER NSAMPL
      REAL EPULSE,TPULSE
      INTEGER ICLUS6,NCLUS6,ICLUS13
      INTEGER IFLAG
      REAL EFIRST_SAMPLE
      INTEGER LPLKRE1,LPLKRA1
      COMMON /cadCDE/ IXLKR(NCMAX),IYLKR(NCMAX),
     &      IBENE(NCMAX),
     &      IBADC(NCMAX),CALDEAD(NCMAX),ICLUS6(NCMAX),
     &      NCLUS6(NCMAX,ICLMAX),ICLUS13(NCMAX),
     &       EPULSE(NCMAX),TPULSE(NCMAX),IFLAG(NCMAX),CALNCEL,NSAMPL,
     &      EFIRST_SAMPLE(NCMAX),LPLKRE1,LPLKRA1
      INTEGER NCELX,NCELY
      PARAMETER(NCELX=128,NCELY=128)
      INTEGER INDHIGH
      REAL XCELL,YCELL
      REAL XCELL_LKR(0:NCELX-1),YCELL_LKR(0:NCELY-1)
      REAL EDGE_X,EDGE_Y,EDGE_D
      INTEGER NHMAX
      PARAMETER(NHMAX=250)
      REAL EHIGH,THIGH
      INTEGER IXHIGH,IYHIGH,ITHIGH,IUSED
      INTEGER NHIGH
      INTEGER IPOINTLKR
      COMMON /cunCDE/EHIGH(NHMAX),THIGH(NHMAX),
     &                INDHIGH(0:NCELX-1,0:NCELY-1),
     &                IXHIGH(NHMAX),IYHIGH(NHMAX),ITHIGH(NHMAX),
     &                IUSED(NHMAX),NHIGH,
     &                IPOINTLKR(0:NCELX-1,0:NCELY-1),XCELL,YCELL,
     &                EDGE_X,EDGE_Y,EDGE_D,XCELL_LKR,YCELL_LKR  
            integer ndead_max 
       parameter(ndead_max=500) 
       integer ndead 
       real xdead,ydead 
       common/deaCDE/ndead,xdead(ndead_max),ydead(ndead_max)
      INTEGER NCLMAX
      PARAMETER(NCLMAX=25)
      REAL ECLUS,XCLUS,YCLUS,TSEED,ESEED
      REAL RMSX5,RMSY5
      INTEGER IXSEED,IYSEED,ITSEED
      LOGICAL DEAD33,DEAD55
      INTEGER NCLUS
      INTEGER NCCLUS
      INTEGER NBAD_CELL_MAX
      PARAMETER(NBAD_CELL_MAX=10)
      INTEGER NBAD_CELL
      INTEGER IBAD_CELL
      REAL EARRAY,TARRAY
      INTEGER IFLARRAY
      REAL ERAW
      INTEGER IFLAGREC
      REAL ENOISE,SPACECORR,ERCAL
      REAL ECLUSF,XCLUSF,YCLUSF
      REAL ENOISE7
      COMMON/cluCDE/NCLUS,ECLUS(NCLMAX),ERAW(NCLMAX),
     &      XCLUS(NCLMAX),YCLUS(NCLMAX),
     &      TSEED(NCLMAX),
     &      IXSEED(NCLMAX),IYSEED(NCLMAX),ITSEED(NCLMAX),
     &      RMSX5(NCLMAX),RMSY5(NCLMAX),NCCLUS(NCLMAX),
     &      DEAD33(NCLMAX),NBAD_CELL(NCLMAX),
     &      IBAD_CELL(NBAD_CELL_MAX,NCLMAX),DEAD55(NCLMAX),
     &      EARRAY(NCLMAX,11,11),TARRAY(NCLMAX,11,11),
     &      IFLARRAY(NCLMAX,11,11),IFLAGREC,ENOISE(NCLMAX),
     &      SPACECORR(NCLMAX),ERCAL(NCLMAX),
     &     ECLUSF(NCLMAX),XCLUSF(NCLMAX),YCLUSF(NCLMAX),ENOISE7(NCLMAX),
     &      ESEED(NCLMAX)
       INTEGER ii,j

       real*8 qbene(8),qbadc(8)

       PRINT*,'GOOD CELLS ',CALNCEL,XCELL,YCELL,NDEAD,NHIGH,NCLUS
       do ii = 1,NCLUS
         PRINT*,ECLUS(NCLUS),ERAW(NCLUS),XCLUS(NCLUS),YCLUS(NCLUS)
       enddo

   
CC       do ii = 1,NDEAD
CC         PRINT*,XDEAD(ii),YDEAD(ii)
CC       enddo
CC       do i = 0,127
CC         PRINT*,i,XCELL_LKR(i),YCELL_LKR(i)
CC       enddo
       
CCCC       do ii = 1,CALNCEL
CCCC         CALL Q(IBENE(ii),qbene)
CCCC         CALL Q(IBADC(ii),qbadc)
CCCC         PRINT*,IXLKR(ii),IYLKR(ii),CALDEAD(ii),
CCCC     +          qbene(6),qbadc(6)
CCCC       enddo
CCCC
       do ii = 1,NHIGH
         PRINT*,'SEED ',EHIGH(ii),THIGH(ii),IXHIGH(ii),IYHIGH(ii),
     &                  IPOINTLKR(IXHIGH(ii),IYHIGH(ii))
       enddo

CC       do Iref=1,20
CC         do ig=1,4
CC           do it=-25,25
CC             do is=1,3
CC               PRINT*,a(Iref,ig,It,is)
CC               PRINT*,b(Iref,ig,It,is)
CC             enddo
CC           enddo
CC         enddo
CC       enddo
CCC       do ix=0,127
CCC          do iy=0,127
CCC              PRINT*,Icell_shape(ix,iy)
CCC          enddo
CCC       enddo
       return
       end
