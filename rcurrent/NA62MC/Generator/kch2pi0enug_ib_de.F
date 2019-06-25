 1    SUBROUTINE KCH2PI0ENUG_IB_DE(JKAON, pi0decay, EgCut, ThetaCut)
C.    ****************************************************************************
C.    *                                                                          *
C.    * Generates a  K+- -> pi0 e nu_e gamma decay                               *
C.    * Use prof. KUBIS generator                                                *
C.    * Kubis, B., Müller, E., Gasser, J. et al. Eur. Phys. J. C (2007) 50: 557. *
C.    * https://doi.org/10.1140/epjc/s10052-007-0215-9                           *
C.    *										 *
C.    * IB and DE components (and their Interference) are simulated,	         *
C.    *	according to this paper.						 *
C.    * The lower limits on the photon energy and on the angle between the       *
C.    * photon and the electron are set in the macro file with the data cards:   *
C.    *      /decay/RadiativePhotonMinEnergy [MeV]                               *
C.    *      /decay/LeptonPhotonMinAngle [radians]			         *
C.    ****************************************************************************

#include "common_blocks.f"
#include "masses.f"

      Real PPCM(4,4)
      INTEGER JPION, JELEC, JGAM, JNU, J
      INTEGER IPION, IELEC, IGAM, INU
      integer pi0decay, pzmode
      REAL*8 EgCut, ThetaCut

C     Particles 4-momenta
      REAL*8 PPI(4), PEL(4), PNU(4), PGAM(4)
      

c      write(*,*) 'enter in KCH2PI0ENUG_IB_DE' 

      call kce3g_ib_de(PPCM,EgCut,ThetaCut) 


C     Copy output 4-momenta
C     Output: PPCM(4 ,      4)
C                  Px       e
C                  Py       pi
C                  Pz       nu
C                  E        gamma
      DO J=1,4
         PEL(J)=PPCM(J,3)
         PPI(J)=PPCM(J,2)
         PNU(J)=PPCM(J,4)
         PGAM(J)=PPCM(J,1)
      ENDDO


c ... Fill MC particle list
      IPION = IDPIZ
      INU   = IDNU
      IGAM  = IDGAM
      IELEC = IDELEP
      JPION = MCADD4GEN(IPION, PPI,  0)
      JELEC = MCADD4GEN(IELEC, PEL,  0)
      JNU   = MCADD4GEN(INU,   PNU,  0)
      JGAM  = MCADD4GEN(IGAM,  PGAM, 0)

C     Boost particles to LAB-system
      CALL DBOOST(p4ini(1,jkaon), MKCH, ppi, ppi)
      CALL DBOOST(p4ini(1,jkaon), MKCH, pel, pel)
      CALL DBOOST(p4ini(1,jkaon), MKCH, pnu, pnu)
      CALL DBOOST(p4ini(1,jkaon), MKCH, pgam, pgam)



c ... Fill MC particle list
      JPION = MCADD4(IPION, PPI)
      JELEC = MCADD4(IELEC, PEL)
      JGAM  = MCADD4(IGAM,  PGAM)

C --- FORCED PI0 DECAY

      pzmode = mod (pi0decay, 100)
      call pi0decay_manager(jpion, ppi, pzmode)

      RETURN
      END



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE kce3g_ib_de(PCM_internal,EgCut,ThetaCut)

      IMPLICIT NONE
#include "masses.f"
      
      Real PCM_internal(4,4)
      REAL*8 EgCut,ThetaCut

C======================
      INTEGER LX,LY,LZ
C      integer cnt1,cnt2,cnt3
      PARAMETER (lx=1,ly=2,lz=3)
C=============matrix variables===================
      integer n1,nt1,nt2,iseed
      real*8 xmpi,xmel,xmk
C     data xmpi,xmel,xmk/134.9766,0.51099891,493.677/
      PARAMETER (xmpi=1000.0*MP0,xmel=1000.0*MEL,xmk=1000.0*MKCH)
      real*8 et1,xm1(4)
      real*8 p1(4,4)
      real*8 wt1,realV(4),realA(4),SM,cv(30),egl,cmax
      real*8 fp0,xlap,xlapss
      integer iegflag,ictflag,Nevent,i
      common/fplus/fp0,xlap,xlapss
      real*8 totmtrx,totmtrx_rand
C=================================================
      REAL XPIMAS,XPZMAS,XMUMAS,XELMAS    
      REAL XKCHMAS,XKCMASS
      REAL*8 ME,MKC
      REAL*8 SQMK,SQME
      REAL*8 SQMKC
      REAL*8 EG0
      PARAMETER (XPIMAS=MPI, XPZMAS=MP0)
      PARAMETER (XMUMAS=MMU, XELMAS=MEL)
      PARAMETER (xkcmass = MKCH)
      PARAMETER ( mkc = xkcmass , sqmkc = mkc**2)
      PARAMETER (xkchmas = xkcmass)
C      PARAMETER (MK=XKMASS,SQMK=MK**2)
      PARAMETER (EG0=MP0/2.)
      PARAMETER (ME=XELMAS,SQME=ME**2)
C======================
      INTEGER NCODEP , NCOPIZ(6)
      COMMON / DECAYP_common /NCODEP , NCOPIZ
C
c===========   FILLED IN PHOTOS
      INTEGER NMXHEP
      PARAMETER (NMXHEP=2000)
      INTEGER IDHEP,ISTHEP,JDAHEP,JMOHEP,NEVHEP,NHEP
      REAL*8 PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)


C
C     SOME CONSTANTS
C
      REAL     RLMASS, RLMAS2, PIMASS, PIMAS2, RKMASS, RKMAS2
      PARAMETER( RLMASS=MEL, RLMAS2=RLMASS**2 )
      PARAMETER( PIMASS=MP0,  PIMAS2=PIMASS**2 )
      PARAMETER( RKMASS=MKCH,  RKMAS2=RKMASS**2 )

C  INPUT
      INTEGER JKAON
C                               ! INDEX OF PARENT KAON
C  VAR
      INTEGER JPION, JELEC, JNU, JGAM, j
      INTEGER JG1, JG2, JG3,NRADGAMMA
      INTEGER IPION,IELEC,INU,IGAM
      REAL*8 PPI(4),PEL(4),PNU(4),PGAM(4)
      REAL EPIP, A, QQ, WTTST, WTAGA, tpi, tlep
      REAL*8 PRAD(4), PRAD2(4), PRAD3(4)
      INTEGER n_decay, nhep_before, nhep_after,nhep_rad,ip,ip_mother

C  FUNCTION
      INTEGER MCADD4
      REAL RANF
C  COMMON
        INTEGER NP, kgenev
        REAL    TECM, AMASS
      COMMON /GENIN / NP,TECM,AMASS(18),KGENEV
        REAL PCM, WT
      COMMON /GENOUT/ PCM(5,18),WT
C============
C======================
      INTEGER   MXPART,   NONE,  TERM , IFIRST_PART
      PARAMETER(MXPART=500,NONE=0,TERM=-1)
      INTEGER MCQUIT,MPART,EVTVAL,PARVAL,PID,PARENT,CHILD,PARTIME
      INTEGER IPARTVER,IMED
      INTEGER MXPTRA
      INTEGER NACCI,NACCI_CODE(MXPART)
      REAL EVTWT,EINI,PINI,DIRINI,EPART,PPART,DIR,XYZ,CURVERT,IPREV_VERT
      REAL TACCI,EVT_TIME
      REAL*8 P4INI
      REAL ESIMP(MXPART)
      COMMON/MCLIST/MCQUIT,EVTWT,MPART,MXPTRA,EVTVAL,PARVAL(MXPART),
     +               PID(MXPART),PARENT(MXPART),CHILD(MXPART),
     +               EINI(MXPART),PINI(MXPART),DIRINI(3,MXPART),
     +               EPART(MXPART),PPART(MXPART),DIR(3,MXPART),
     +               XYZ(3,MXPART),PARTIME(MXPART),TACCI,EVT_TIME,
     +               CURVERT,IPREV_VERT,IFIRST_PART,IPARTVER(MXPART),
     +               IMED(MXPART),
     +               NACCI,NACCI_CODE
      COMMON / MCP4IN / P4INI(4,MXPART)
      COMMON / MCHHHH / ESIMP
      REAL MCPARTSPIN
      COMMON / MCSPIN / MCPARTSPIN(3,MXPART)

C DATA
      real*8 RN_hitandmiss(2),totmax,dummy
      integer nevt
      data  totmax / 999999. / 
      
      call initialize(xmk,xmpi,egl,cmax,realV,realA,
     .                xlap,xlapss,fp0,EgCut,ThetaCut)
      xm1(1)=0.
      xm1(2)=xmpi/xmk
      xm1(3)=xmel/xmk
      xm1(4)=0.

 1        CONTINUE

      CALL RANMAR(RN_hitandmiss(2),2) 
      iseed=INT(RN_hitandmiss(1)*210819791) 


      nt1=0
      nt2=0

      CALL RMARIN1(ISEED,NT1,NT2)
      
      n1=4
      et1=1. ! xmk/xmk 

      call rambos(n1,et1,xm1,P1,WT1) 
c      write(*,*), "phase space: ",WT1

 
C
      CALL MATSQ(xm1,P1,egl,cmax,realV,realA,SM,cv,iegflag,ictflag)
      if(iegflag*ictflag.eq.0) goto 1

c      write(*,*), "matrix: ",SM

      Totmtrx=SM*WT1
      TOTmtrx_rand=totmax*RN_hitandmiss(2) 

      if(TOTmtrx_rand.gt.Totmtrx) GOTO 1 

      do i=1,4
         ppi(i)=P1(i,2)*MKCH
         pel(i)=P1(i,3)*MKCH
         pnu(i)=P1(i,4)*MKCH
         pgam(i)=P1(i,1)*MKCH
      enddo

      do i=1,4         
         PCM_internal(i,1) = P1(i,1)*MKCH
         PCM_internal(i,2) = P1(i,2)*MKCH
         PCM_internal(i,3) = P1(i,3)*MKCH
         PCM_internal(i,4) = P1(i,4)*MKCH
      enddo

      Nevent=Nevent+1

      RETURN
      END



      subroutine rambo(N,ET,XM,P,WT)
      implicit none
      integer K,NM,I,N,IBEGIN,ITMAX,ITER
      real*8 TWOPI,PO2LOG,Z(100),XMT,RN(10),C,S,F,Q(4,100)
      real*8 R(4),RMAS,B(3),G,A,X,BQ,P(4,N),WT,IWARN(5),ACC
      real*8 XMAX,XM2(100),P2(100),ACCU,F0,G0,X2,E(100),V(100)
      real*8 WT2,WT3,WTM,XM(100),ET
c      real*8 WT2,WT3,WTM,XM(4),ET
      DATA ACC/1.D-14/,ITMAX/6/,IBEGIN/0/,IWARN/5*0/
   
c      write(*,*) 'enter rambo subroutine' 
c      write (*,*) 'rambo: N =', N
c      write (*,*) 'rambo: ET =', ET
c      write (*,*) 'rambo: XM =', XM
c      write (*,*) 'rambo: P =', P
c      write (*,*) 'rambo: WT =', WT

      IBEGIN=1
      TWOPI=8.*DATAN(1.D0)
      PO2LOG=DLOG(TWOPI/4.)
      Z(2)=PO2LOG
      DO 101 K=3,100
 101      Z(K)=Z(K-1)+PO2LOG-2.*DLOG(DFLOAT(K-2))
      DO 102 K=3,100
 102      Z(K)=(Z(K)-DLOG(DFLOAT(K-1)))
C
C CHECK ON THE NUMBER OF PARTICLES
 103       IF(N.GT.1.AND.N.LT.101) GOTO 104
      PRINT 1001,N
      STOP
C CHECK WHETHER TOTAL ENERGY IS SUFFICIENT; COUNT NONZERO MASSES
 104   XMT=0.
      NM=0
      DO 105 I=1,N
      IF(XM(I).NE.0.D0) NM=NM+1
 105   XMT=XMT+DABS(XM(I))
      IF(XMT.LE.ET) GOTO 201
      PRINT 1002,XMT,ET
      STOP
C GENERATE N MASSLESS MOMENTA IN INFINITE PHASE SPACE
 201   DO 202 I=1,N

      CALL RANMAR(RN,8)
     
      C=2.*RN(1)-1.
      S=DSQRT(1.-C*C)
      F=TWOPI*RN(2)
      Q(4,I)=-DLOG(RN(3)*RN(4))
      Q(3,I)=Q(4,I)*C
      Q(2,I)=Q(4,I)*S*DCOS(F)
 202   Q(1,I)=Q(4,I)*S*DSIN(F)
C CALCULATE THE PARAMETERS OF THE CONFORMAL TRANSFORMATION
      DO 203 I=1,4
 203      R(I)=0.
      DO 204 I=1,N
      DO 204 K=1,4
 204      R(K)=R(K)+Q(K,I)
      RMAS=DSQRT(R(4)**2-R(3)**2-R(2)**2-R(1)**2)
      DO 205 K=1,3
 205      B(K)=-R(K)/RMAS
      G=R(4)/RMAS
      A=1./(1.+G)
      X=ET/RMAS
C TRANSFORM THE Q'S CONFORMALLY INTO THE P'S
      DO 207 I=1,N
      BQ=B(1)*Q(1,I)+B(2)*Q(2,I)+B(3)*Q(3,I)
      DO 206 K=1,3
 206      P(K,I)=X*(Q(K,I)+B(K)*(Q(4,I)+A*BQ))
 207       P(4,I)=X*(G*Q(4,I)+BQ)
C
C CALCULATE WEIGHT AND POSSIBLE WARNINGS
      WT=PO2LOG
      IF(N.NE.2) WT=(2.*N-4.)*DLOG(ET)+Z(N)
      IF(WT.GE.-180.D0) GOTO 208
      IF(IWARN(1).LE.5) PRINT 1004,WT
      IWARN(1)=IWARN(1)+1
 208   IF(WT.LE. 174.D0) GOTO 209
      IF(IWARN(2).LE.5) PRINT 1005,WT
      IWARN(2)=IWARN(2)+1
C RETURN FOR WEIGHTED MASSLESS MOMENTA
 209   IF(NM.NE.0) GOTO 210
      WT=DEXP(WT)
      RETURN
C
C MASSIVE PARTICLES: RESCALE THE MOMENTA BY A FACTOR X
 210   XMAX=DSQRT(1.-(XMT/ET)**2)
      DO 301 I=1,N
      XM2(I)=XM(I)**2
 301   P2(I)=P(4,I)**2
      ITER=0
      X=XMAX
      ACCU=ET*ACC
 302   F0=-ET
      G0=0.
      X2=X*X
      DO 303 I=1,N
      E(I)=DSQRT(XM2(I)+X2*P2(I))
      F0=F0+E(I)
 303   G0=G0+P2(I)/E(I)
      IF(DABS(F0).LE.ACCU) GOTO 305
      ITER=ITER+1
      IF(ITER.LE.ITMAX) GOTO 304
      PRINT 1006,ITMAX
      GOTO 305
 304   X=X-F0/(X*G0)
      GOTO 302
 305   DO 307 I=1,N
      V(I)=X*P(4,I)
      DO 306 K=1,3
 306      P(K,I)=X*P(K,I)
 307       P(4,I)=E(I)

C CALCULATE THE MASS-EFFECT WEIGHT FACTOR
      WT2=1.
      WT3=0.
      DO 308 I=1,N
      WT2=WT2*V(I)/E(I)
 308   WT3=WT3+V(I)**2/E(I)
      WTM=(2.*N-3.)*DLOG(X)+DLOG(WT2/WT3*ET)
C
C RETURN FOR  WEIGHTED MASSIVE MOMENTA
      WT=WT+WTM
      IF(WT.GE.-180.D0) GOTO 309
      IF(IWARN(3).LE.5) PRINT 1004,WT
      IWARN(3)=IWARN(3)+1
 309   IF(WT.LE. 174.D0) GOTO 310
      IF(IWARN(4).LE.5) PRINT 1005,WT
      IWARN(4)=IWARN(4)+1
 310   WT=DEXP(WT)
      RETURN

 1001  FORMAT(' RAMBO FAILS: # OF PARTICLES =',I5,' IS NOT ALLOWED')
 1002   FORMAT(' RAMBO FAILS: TOTAL MASS =',D15.6,' IS NOT',
     . ' SMALLER THAN TOTAL ENERGY =',D15.6)
 1004    FORMAT(' RAMBO WARNS: WEIGHT = EXP(',F20.9,') MAY UNDERFLOW')
 1005     FORMAT(' RAMBO WARNS: WEIGHT = EXP(',F20.9,') MAY  OVERFLOW')
 1006      FORMAT(' RAMBO WARNS:',I3,' ITERATIONS DID NOT GIVE THE',
     . ' DESIRED ACCURACY =',D15.6)
      END

      subroutine initialize(xmk,xmpi,egl,cmax,realV,realA,
     .                      xlap,xlapss,fp0,EgCut,ThetaCut)
c----------------------------------------------------------------
      implicit none
      real*8 xmk,xmpi,egl,cmax,xlap,xlapss,fp0
c      common/fplus/fp0,xlap,xlapss
      real*8 realV(4),realA(4)
      real*8 EgCut,ThetaCut

c  egl = lower limit on photon energy (always in units of MK)
c  cmax = upper limit on cosine of opening angle theta between
c         photon and charged lepton

       cmax=cos(ThetaCut) ! ThetaCut already in [rad]
       egl=1000.0*EgCut/xmk ! from [GeV] to [MeV]

      write(72,*) 'cutoff parameters:'
      write(72,*) 'egl = ',egl,' cmax = ',cmax
      write(72,*)

c structure dependent terms 
c
c as given in Table 2 of GKMS, O(p^4)

      realV(1)=-1.242
      realV(2)=-0.189
      realV(3)=-0.019
      realV(4)=0.
      realA(1)=-1.185
      realA(2)=0.296
      realA(3)=-1.185
      realA(4)=0.


c form factors
c      fpt =fp0*(1.+xlap/rpp*vt +xlapss/rpp**2*vt**2)

c      xlap=0.0294
c      xlapss=0.
c   test: linear parametrization replaced by quadratic (code was written for the latter but xlapss was set =0 and xlap to the linear value)
      xlap=0.0252
      xlapss=0.0008
      fp0=0.998

c      write(72,*)'form factor parameters:'
c      write(72,*)'fp(0)   =',fp0
c      write(72,*)'lambda  =',xlap
c      write(72,*)'lambda" =', xlapss
c      write(72,*)
c      write(72,*)'structure dependent terms:'
c      write(72,*)'Re(V1) =',realV(1),'   Re(V2) =',realV(2)
c      write(72,*)'Re(V3) =',realV(3),  ' Re(V4) =',realV(4)
c      write(72,*)'Re(A1) =',realA(1),'   Re(A2) =',realA(2)
c      write(72,*)'Re(A3) =',realA(3),'   Re(A4) =',realA(4)
c      write(72,*)

c number of points in rambo

c      IC=100

c      write(72,*) 'Ic= ',IC
c      write(6,*) 'Ic= ',IC

      return
      end

      function scal(p1,p2)
c     does scalar product
      implicit none
      real*8 p1(4),p2(4),scal
      integer i
      
      scal=p1(4)*p2(4)
      
      do i=1,3
         scal=scal-p1(i)*p2(i)
      enddo
      
      return
      end
      
      SUBROUTINE MATSQ(XM,vP,egl,cmax,realV,realA,SM,cv,iegflag,ictflag)
      implicit none
#include "masses.f"      
c      real*8 pi,XM(100),vP(4,100),realV(4),realA(4),SM,cv(30),egl,cmax
      real*8 pi,XM(4),vP(4,4),realV(4),realA(4),SM,cv(30),egl,cmax
      real*8 rimV(4),rimA(4),p(4),ps(4),q(4),pe(4),pnu(4),w(4)
      real*8 eg,el,xl,x,va,vb,vc,vd,ve,vf,vg,vh,vi,vk,vl,vm,vn
      real*8 vs,vt,vu,vw,vw2,fpt,fpw2,dfp,rp,rpp
      real*8 ca(10),cb(4),cb5(4),cc(4),cc5(4)
      real*8 cd(4),cd5(4),ce(4),ce5(4)
      integer iegflag,ictflag,i
      real*8 xfpi,xfk,xmpi,xmpip,xmk,scal
      real*8 fp0,xlap,xlapss
      parameter (xfpi=92.4/MKCH, xfk=1.22*92.4/MKCH,
     .           xmpi=MP0,xmpip=MPI,xmk=MKCH)
      common/fplus/fp0,xlap,xlapss

!--------------------------------------
! 1=gamma 2=pion 3=electron 4=neutrino
!--------------------------------------
c     write(*,*) 'enter MATSQ' 

      xlap=0.0252
      xlapss=0.0008
      fp0=0.998
      pi=3.1415926535
      rp=XM(2)**2
      rpp=rp*(xmpip/xmpi)**2

!kaon momentum
      p(4)=1.
      p(1)=0.
      p(2)=0.
      p(3)=0.

!other momenta

      do i=1,4
         q(i) = vp(i,1)        !photon momentum
         ps(i)= vp(i,2)        !pion
         pe(i)= vp(i,3)        !electron
         pnu(i)=vp(i,4)        !neutrino
         w(i)=pe(i)+pnu(i)     !W-boson
      enddo

      EG=vP(4,1)
      EL=vP(4,3)
      XL=EL-SQRT(EL*EL-xm(3)*xM(3))*CMAX
      XL=XL*EG
      x=scal(pe,q)

      iegflag=1
      ictflag=1
      IF(EG.LE.EGL) iegflag=0 
      IF(X.LE.XL) ictflag=0

!scalar products as they occur in the expressions

      va=scal(p,ps)
      vb=scal(p,q)
      vc=scal(p,pe)
      vd=scal(p,pnu)
      ve=scal(ps,q)
      vf=scal(ps,pe)
      vg=scal(ps,pnu)
      vh=scal(q,pe)
      vi=scal(q,pnu)
      vk=scal(pe,pnu)
      vl=scal(p,w)
      vm=scal(ps,w)
      vn=scal(q,w)

! Mandelstam variables
      vs=rp+2.*scal(ps,q)
      vt=1.+rp-2.*scal(p,ps)
      vu=1.-2.*scal(p,q)
      vw2=scal(w,w)

! form factors
      fpt =fp0*(1.+xlap/rpp*vt +xlapss/rpp**2*vt**2)
      fpw2=fp0*(1.+xlap/rpp*vw2+xlapss/rpp**2*vw2**2)
      dfp =(fpt-fpw2)/scal(q,w)

      ca(1) = (4.0*( - rp*vb**2*vi - rp*vb**2*vk - rp*
     .  vb*vc*vi - 2.0*rp*vb*vc*vk + rp*vb*vd*vh + rp*vh*
     .  vk - 2.0*va*vb*vg*vh + 2.0*vb**2*ve*vg + 2.0*vb**2*
     .  vf*vg + 2.0*vb*vc*ve*vg + 4.0*vb*vc*vf*vg - 2.0*vf*
     .  vg*vh))/(vb**2*vh)

      ca(2) = (4.0*(rp*vb**2*vh*vk - rp*vb**2*vi*vk - 
     .  2.0*rp*vb**2*vk**2 + rp*vb*vc*vi*vn + 2.0*rp*vb*vc*
     .  vk*vn - rp*vb*vd*vh*vn + 2.0*rp*vb*vh*vk*vl - 2.0*rp
     .  *vh*vk*vn + 2.0*va*vb*vg*vh*vn + 2.0*vb**2*ve*vg*
     .  vk + 4.0*vb**2*vf*vg*vk - 2.0*vb**2*vg*vh*vm - 2.0*
     .  vb*vc*ve*vg*vn - 4.0*vb*vc*vf*vg*vn - 4.0*vb*vf*
     .  vg*vh*vl + 4.0*vf*vg*vh*vn))/(vb**2*vh)

      ca(3) = (4.0*(2.0*rp*vb**2*vk**2 - 2.0*rp*vb*vk*vl*
     .  vn + rp*vk*vn**2 - 4.0*vb**2*vf*vg*vk + 4.0*vb*vf*
     .  vg*vl*vn - 2.0*vf*vg*vn**2))/vb**2

      cb(1) = (4.0*( - rp*vb*vh*vi - va*vf*vh*vi - va
     .  *vg*vh**2 - vb*ve*vf*vk + 2.0*vb*ve*vg*vh + vb*
     .  vf**2*vi + vb*vf*vg*vh + vc*ve*vg*vh + vd*ve*
     .  vf*vh))/(vb*vh)
      
      cb(2) = (4.0*( - va*vh*vk*vn + vb*ve*vh*vk - 
     .  vb*ve*vk**2 + vb*vf*vi*vk + vb*vg*vh*vk + vb*
     .  vg*vh*vn - vb*vh*vi*vm + vc*vg*vh*vn + vd*vf*
     .  vh*vn + ve*vh*vk*vl - vf*vh*vi*vl - vg*vh**2*
     .  vl))/(vb*vh)

      cb(3) = 0.

      cb(4) = (2.0*(2.0*rp*va*vh*vk*vn - rp*vb*ve*vh*vk 
     .  + rp*vb*ve*vi*vk + 2.0*rp*vb*ve*vk**2 - rp*vb*vf*
     .  vi*vn - 2.0*rp*vb*vf*vk*vn - rp*vb*vg*vh*vn - 2.0*rp
     .  *ve*vh*vk*vl - 4.0*va*vf*vg*vh*vn - 2.0*vb*ve**2*
     .  vg*vk - 4.0*vb*ve*vf*vg*vk + 2.0*vb*ve*vf*vg*vn +
     .  2.0*vb*ve*vg*vh*vm + 4.0*vb*vf**2*vg*vn + 4.0*ve*
     .  vf*vg*vh*vl))/(vb*vh)

      cb5(1) = (4.0*( - rp*vb*vh*vi - rp*vb*vh*vk - rp*
     .  vc*vh*vi + rp*vd*vh**2 + va*vf*vh*vi - va*vg*
     .  vh**2 + vb*ve*vf*vk + 2.0*vb*ve*vg*vh - vb*vf**
     .  2.0*vi + vb*vf*vg*vh + vc*ve*vg*vh - vd*ve*vf*
     .  vh))/(vb*vh)

      cb5(2) = (4.0*(va*vh**2*vk - va*vh*vi*vk - vb*
     .  ve*vh*vk - vb*ve*vk**2 - vb*vf*vh*vk + vb*vf*
     .  vi*vk - vb*vg*vh*vn + vb*vh*vi*vm + vb*vh*vk*
     .  vm - vc*ve*vh*vk + vc*vh*vi*vm + vd*ve*vh*vk 
     .  - vd*vh**2*vm))/(vb*vh)

      cb5(3) = 0.

      cb5(4) = (2.0*( - rp*ve*vh*vk + rp*ve*vi*vk - rp*
     .  vf*vi*vn - rp*vg*vh*vn - 2.0*ve**2*vg*vk + 2.0*ve*
     .  vf*vg*vn + 2.0*ve*vg*vh*vm))/vh

      cc(1) = (4.0*(va*vf*vi*vn + va*vg*vh*vn + vb*
     .  ve*vf*vk + vb*ve*vg*vk - vb*vf*vi*vm - vb*vg*
     .  vh*vm - vc*ve*vg*vn - vd*ve*vf*vn))/vb

      cc(2) = (4.0*(va*vk*vn**2 + 2.0*vb*ve*vk**2 - 2.0*
     .  vb*vf*vi*vk + vb*vf*vk*vn - 2.0*vb*vg*vh*vk + 
     .  vb*vg*vk*vn - vb*vk*vm*vn - vc*vg*vn**2 - vd*
     .  vf*vn**2 - ve*vk*vl*vn + vf*vi*vl*vn + vg*vh*
     .  vl*vn))/vb

      cc(3) = 0.

      cc(4) = (4.0*( - rp*va*vk*vn**2 - 2.0*rp*vb*ve*vk
     .  **2 + rp*vb*vk*vm*vn + rp*ve*vk*vl*vn + 2.0*va*vf
     .  *vg*vn**2 + 4.0*vb*ve*vf*vg*vk - 2.0*vb*vf*vg*vm*
     .  vn - 2.0*ve*vf*vg*vl*vn))/vb

      cc5(1) = (4.0*(rp*vb*vh*vk - rp*vb*vi*vk + rp*vc*
     .  vi*vn - rp*vd*vh*vn - va*vf*vi*vn + va*vg*vh*
     .  vn - vb*ve*vf*vk + vb*ve*vg*vk + vb*vf*vi*vm 
     .  - vb*vg*vh*vm - vc*ve*vg*vn + vd*ve*vf*vn))/
     .  vb

      cc5(2) = (4.0*vn*( - va*vh*vk + va*vi*vk + vc*
     .  ve*vk - vc*vi*vm - vd*ve*vk + vd*vh*vm))/vb

      cc5(3) = 0.0

      cc5(4) = 0.0

      cd(1) = (4.0*(va*vh - vb*vf - ve*vh))/(vb*vh)


      cd(2) = (4.0*( - vb*vi - vb*vk + vb*vn + vh*
     .  vl - vh*vn))/(vb*vh)

      cd(3) = 0.0

      cd(4) = (2.0*( - rp*vn + 2.0*ve*vg))/vh

      ce(1) = (4.0*( - va*vn + vb*vm + ve*vn))/vb

      ce(2) = (4.0*(2.0*vb*vk - vl*vn + vn**2))/vb

      ce(3) = 0.0

      ce(4) = 0.0

      cd5(1) = (4.0*(vb*vf - vf*vh + vg*vh))/(vb*vh)

      cd5(2) = (4.0*( - vb*vh - vb*vk - vf*vh + vg*
     .  vh))/(vb*vh)

      cd5(3) = 0.0

      cd5(4) = (2.0*( - rp*vh - rp*vi - 2.0*rp*vk + 2.0*ve*
     .  vg + 4.0*vf*vg))/vh

      ce5(1) = (4.0*( - vb*vf + vb*vg + vf*vn - vg*
     .  vn))/vb

      ce5(2) = (4.0*vn*(vf - vg))/vb

      ce5(3) = 0.0

      ce5(4) = 0.0


c IB^2 term in Eq. (B.1)
      cv(1)=ca(1)*fpt**2+ca(2)*fpt*dfp+ca(3)*dfp**2
c terms ~ Re(V_i) in (B.1)
      cv(2)=(cb(1) *fpt+cc(1) *dfp)
      cv(3)=(cb(2) *fpt+cc(2) *dfp)
      cv(4)=(cb(3) *fpt+cc(3) *dfp)
      cv(5)=(cb(4) *fpt+cc(4) *dfp)
c terms ~ Re(A_i) in (B.1)
      cv(6)=(cb5(1)*fpt+cc5(1)*dfp)
      cv(7)=(cb5(2)*fpt+cc5(2)*dfp)
      cv(8)=(cb5(3)*fpt+cc5(3)*dfp)
      cv(9)=(cb5(4)*fpt+cc5(4)*dfp)

c Coefficients for the decomposition Eq. (4.8) in GKMS (lambda, lambda")
c b0 = cv(10), b1 = cv(11), b2 = cv(12), b3 = cv(13), b4 = cv(14), b5 = cv(15)
c b0(SD) = cv(16), b1(SD) = cv(17), b3(SD) = cv(18)

      cv(10)=ca(1)
      cv(11)=2.*(vt*ca(1)+ca(2))/rpp
      cv(12)=(vt**2*ca(1)+2.*vt*ca(2)+4.*ca(3))/rpp**2
      cv(13)=(2.*vt**2*ca(1)+2.*(vt+vw2)*ca(2))/rpp**2
      cv(14)=(2.*vt**3*ca(1)+2.*vt*(2.*vt+vw2)*ca(2)+8.*(vt+vw2)*ca(3))
     .       /rpp**3
      cv(15)=(vt**4*ca(1)+2.*vt**2*(vt+vw2)*ca(2)+4.*(vt+vw2)**2*ca(3))
     .       /rpp**4

      cv(16)=0.
      cv(17)=0.
      cv(18)=0.
      do i=1,4
         cv(16)=cv(16)+(cb(i)*realV(i)+cb5(i)*realA(i))/fp0
         cv(17)=cv(17)+(vt*cb(i) +2.*cc(i) )/rpp*realV(i)/fp0
     .                +(vt*cb5(i)+2.*cc5(i))/rpp*realA(i)/fp0
         cv(18)=cv(18)
     .         +(vt**2*cb(i) +2.*(vt+vw2)*cc(i) )/rpp**2*realV(i)/fp0
     .         +(vt**2*cb5(i)+2.*(vt+vw2)*cc5(i))/rpp**2*realA(i)/fp0
      enddo

c check identity with sm; cv(19): IB+SD; cv(20): IB only
      cv(19)=(cv(10)+cv(16)
     .      +(cv(11)+cv(17))*xlap
     .      + cv(12)        *xlap**2
     .      +(cv(13)+cv(18))*xlapss
     .      + cv(14)        *xlap*xlapss
     .      + cv(15)        *xlapss**2)*fp0**2
      cv(20)=(cv(10)
     .      + cv(11)        *xlap
     .      + cv(12)        *xlap**2
     .      + cv(13)        *xlapss
     .      + cv(14)        *xlap*xlapss
     .      + cv(15)        *xlapss**2)*fp0**2


c cv(1) alone is IB^2; rest are IB-SD interference terms

      sm=cv(1) !elemento di matrice da usare in MC
     .+realV(1)*cv(2)+realV(2)*cv(3)+realV(3)*cv(4)+realV(4)*cv(5)
     .+realA(1)*cv(6)+realA(2)*cv(7)
     .+realA(3)/(1.-vw2)*cv(8)+realA(4)*cv(9)

      return
      end


      SUBROUTINE RANMAR1(RVEC,LENV)

      IMPLICIT NONE
      integer LENV,IVEC
      real*8 RVEC(LENV)
      INTEGER ijklin,ntotin,ntot2n,IJKL,ntot,ntot2,kalled
      INTEGER IJKLUT,NTOTUT,NTOT2T
      DATA NTOT,NTOT2,IJKL/-1,0,0/
c      real*8 IJ,KL,S,T,U(97),TWOM24,C,CD,CM,UNI,ZUNI
      real*8 S,T,U(97),TWOM24,C,CD,CM,UNI,ZUNI
c      integer I,J,K,L,M,II,JJ,I24,I97,J97,LOOP2,NOW,IDUM
      integer IJ,KL,I,J,K,L,M,II,JJ,I24,I97,J97,LOOP2,NOW,IDUM
      COMMON/RASET1/U,C,I97,J97
      SAVE CD, CM, TWOM24, NTOT, NTOT2, IJKL
      integer MODCNS
      PARAMETER (MODCNS=1000000000)

c      write(*,*) 'enter in RANMAR1' 

      IF (NTOT .GE. 0)  GO TO 50   
      IJKL = 54217137                                                         
      NTOT = 0                                                                  
      NTOT2 = 0                                                                 
      KALLED = 0                                                                
      GO TO 1

      ENTRY RMARIN1(IJKLIN,NTOTIN,NTOT2N)

      IJKL = IJKLIN
      NTOT = MAX(NTOTIN,0)
      NTOT2= MAX(NTOT2N,0)
      KALLED=1
 1     CONTINUE

      IJ = IJKL/30082                                                           
      KL = IJKL - 30082*IJ                                                      
      I = MOD(IJ/177, 177) + 2 
      J = MOD(IJ, 177)     + 2 
      K = MOD(KL/169, 178) + 1 
      L = MOD(KL, 169)  

c      WRITE(72,'(A,I10,2X,2I10)') ' RANMAR INITIALIZED:',IJKL,NTOT,NTOT2
c      WRITE(*,'(A,I10,2X,2I10)') ' RANMAR INITIALIZED:',IJKL,NTOT,NTOT2  
      DO 2 II= 1, 97                                                            
      S = 0.                                                                    
      T = .5                                                                    
      DO 3 JJ= 1, 24                                                            
         M = MOD(MOD(I*J,179)*K, 179)                                           
         I = J                                                                  
         J = K                                                                  
         K = M                                                                  
         L = MOD(53*L+1, 169)      
         IF (MOD(L*M,64) .GE. 32)  S = S+T
 3           T = 0.5*T
 2            U(II) = S
      TWOM24 = 1.0
      DO 4 I24= 1, 24
 4        TWOM24 = 0.5*TWOM24  
      C  =   362436.*TWOM24
      CD =  7654321.*TWOM24 
      CM = 16777213.*TWOM24
      I97 = 97
      J97 = 33

      DO 45 LOOP2= 1, NTOT2+1
      NOW = MODCNS
      IF (LOOP2 .EQ. NTOT2+1)  NOW=NTOT                                         
      IF (NOW .GT. 0)  THEN                                                     
        WRITE(72,'(A,I15)') ' RMARIN SKIPPING OVER ',NOW                         
       DO 40 IDUM = 1, NTOT                                                     
       UNI = U(I97)-U(J97)                                                      
       IF (UNI .LT. 0.)  UNI=UNI+1.                                             
       U(I97) = UNI                                                             
       I97 = I97-1                                                              
       IF (I97 .EQ. 0)  I97=97                                                  
       J97 = J97-1                                                              
       IF (J97 .EQ. 0)  J97=97                                                  
       C = C - CD 
       IF (C .LT. 0.)  C=C+CM                                                   
 40      CONTINUE                                                                 
      ENDIF
 45    CONTINUE  
                                                                
      IF (KALLED .EQ. 1)  RETURN 

 50    CONTINUE  
      DO 100 IVEC= 1, LENV                                                      
      UNI = U(I97)-U(J97) 
      IF (UNI .LT. 0.)  UNI=UNI+1.                                              
      U(I97) = UNI                                                              
      I97 = I97-1                                                               
      IF (I97 .EQ. 0)  I97=97                                                   
      J97 = J97-1                                                               
      IF (J97 .EQ. 0)  J97=97
      C = C - CD  
      IF (C .LT. 0.)  C=C+CM                                                    
      UNI = UNI-C                                                               
      IF (UNI .LT. 0.) UNI=UNI+1.
      RVEC(IVEC) = UNI 
         IF (UNI .EQ. 0.)  THEN                                                 
         ZUNI = TWOM24*U(2)                                                     
C             AN EXACT ZERO HERE IS VERY UNLIKELY, BUT LET'S BE SAFE.           
         IF (ZUNI .EQ. 0.) ZUNI= TWOM24*TWOM24                                  
         RVEC(IVEC) = ZUNI                                                      
         ENDIF                                                                  
 100      CONTINUE                                                                  
      NTOT = NTOT + LENV                                                        
         IF (NTOT .GE. MODCNS)  THEN                                            
         NTOT2 = NTOT2 + 1                                                      
         NTOT = NTOT - MODCNS                                                   
         ENDIF                                                                  
      RETURN                                                                    
C           ENTRY TO OUTPUT CURRENT STATUS                                      
      ENTRY RMARUT1(IJKLUT,NTOTUT,NTOT2T)                                        
      IJKLUT = IJKL                                                             
      NTOTUT = NTOT                                                             
      NTOT2T = NTOT2    
      RETURN                                                                    
      END


