        SUBROUTINE TESTITER(IER)
C
C test if clustering should be iterated
C  ier=0 no
C  ier=1 yes, if 2 clusters within Rclus
C             if 2nd cluster has E<1.5 GeV, it is dropped
c             there is one dead cell in a 3x3 around cluster max
C
C  ier <0 error
C
      IMPLICIT NONE
      INTEGER IER
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
      INTEGER IIX,IIY,IX,IY,ipoint
        INTEGER ICLUS,JCLUS
        REAL DIST,DMIN,EMIN,RMERGE
        INTEGER NNEW
        REAL ENEW(NCLMAX),XNEW(NCLMAX),YNEW(NCLMAX)
        INTEGER IXNEW(NCLMAX),IYNEW(NCLMAX),ITNEW(NCLMAX)
      REAL TNEW(NCLMAX)
        real RITER
        data RITER/30.0/
        RMERGE=10.0
C
        IER=0
        NNEW=0
C
C loop over clusters
C
        DO ICLUS=1,NCLUS
c
c is there a dead cell in cluster ?
c
           IF (DEAD33(ICLUS)) IER=IBSET(IER,2)
           IF (DEAD55(ICLUS)) IER=IBSET(IER,1)

           DMIN=999.
           EMIN=999.
C
C is there a nearby cluster ?
C
           DO JCLUS=1,NCLUS
             IF (JCLUS.NE.ICLUS) THEN
                DIST=SQRT((XCLUS(ICLUS)-XCLUS(JCLUS))**2+
     &                    (YCLUS(ICLUS)-YCLUS(JCLUS))**2)
                IF (DIST.LE.DMIN) THEN
                  DMIN=DIST
                  EMIN=ECLUS(JCLUS)
                ENDIF
             ENDIF
           ENDDO
           IF (DMIN.LE.RITER) IER=IBSET(IER,0)

           IF (ECLUS(ICLUS).LE.0.001) IER=IBSET(IER,0)
C
C merge cluster if appropriate
c  (remove clusters with zero energy)
C
           IF ((DMIN.GE.RMERGE.OR.ECLUS(ICLUS).GE.1.5.OR.
     &         ECLUS(ICLUS).GT.EMIN).AND.
     &         ECLUS(ICLUS).GT.0.050) THEN
              NNEW=NNEW+1
              ENEW(NNEW)=ECLUS(ICLUS)
              XNEW(NNEW)=XCLUS(ICLUS)
              YNEW(NNEW)=YCLUS(ICLUS)
              IXNEW(NNEW)=IXSEED(ICLUS)
              IYNEW(NNEW)=IYSEED(ICLUS)
              ITNEW(NNEW)=ITSEED(ICLUS)
              TNEW(NNEW)=TSEED(ICLUS)
           ENDIF
        ENDDO
C
C if something has been done, update some arrays
C
        IF (IER.GT.0) THEN
          IF (NNEW.ne.NCLUS) then  ! cluster number has changed
             call wzero(iclus13,calncel)
             call wzero(iclus6,calncel)
             do ICLUS=1,NNEW
                IX=IXNEW(ICLUS)
                IY=IYNEW(ICLUS)
                do IIX=MAX(IX-20,0),MIN(IX+20,NCELX-1)
                 do IIY=MAX(IY-20,0),MIN(IY+20,NCELY-1)
                  Ipoint=IpointLkr(iix,iiy)
                  If (Ipoint.gt.0) then
                   Iclus6(Ipoint)=Iclus6(Ipoint)+1
                   if (Iclus6(Ipoint).le.iclmax) then
                     Nclus6(Ipoint,Iclus6(Ipoint))=ICLUS
                   endif
                   if (iabs(iix-ix).le.6.and.iabs(iiy-iy).le.6) then
                     Iclus13(Ipoint)=Iclus13(Ipoint)+1
                   endif
                  endif
                 enddo
                enddo
              enddo
          endif
           NCLUS=NNEW
           CALL wzero(ECLUS,NCLMAX)
           CALL wzero(XCLUS,NCLMAX)
           CALL wzero(YCLUS,NCLMAX)
           DO ICLUS=1,NNEW
              ECLUS(ICLUS)=ENEW(ICLUS)
              XCLUS(ICLUS)=XNEW(ICLUS)
              YCLUS(ICLUS)=YNEW(ICLUS)
              IXSEED(ICLUS)=IXNEW(ICLUS)
              IYSEED(ICLUS)=IYNEW(ICLUS)
              ITSEED(ICLUS)=ITNEW(ICLUS)
              TSEED(ICLUS)=TNEW(ICLUS)
           ENDDO
        ENDIF
        RETURN
        END
C
C --------------------------------------------------------------------
C
