      SUBROUTINE KCH2PIPI0PI0 (JKAON,RADFLG,PI0DECAY,twoPhotMaxAngle,
     $                         NTrials)

C-----------------------------------------------------------C
C Generates the K+- --> pi+- pi0 pi0 decay according to the C
C empirical Dalitz plot parametrisation, PLB686 (2010) 101  C
C                                                           C
C History:                                                  C
C JUNE-99  C.CHESHKOV                                       C
C NOV 2009 B.Bloch modified according to empirical          C 
C          parametrisation + PHOTOS                         C
C-----------------------------------------------------------C

#include "common_blocks.f"
#include "masses.f"

      INTEGER RADFLG
      REAL*8  twoPhotMaxAngle
      INTEGER NTrials
      logical radcor
      logical GOODCOMBINATION
      REAL*8  PHOTILEN, PHOTJLEN, photonsAngle, scalarProd
      REAL*8  p4kaon(4)
      REAL*8     EMIN0, EMIN, EMAX0, EMAX
      PARAMETER( EMIN0=MPI,
     &           EMAX0=(SQMKCH+SQMPI-(MP0+MP0)**2)/(2.*MKCH))
      PARAMETER( EMIN =MP0,
     &           EMAX =(SQMKCH+SQMP0-(MPI+MP0)**2)/(2.*MKCH))
C                               ASSUME MINIMUM INVARIANT MASS OF 2 PARTICLES
C                               (SUM OF MASSES) TO GET EMAX OF 3RD PARTICLE.
      REAL*8     Y0, Y1, YMAX
      PARAMETER( Y0 = 2./3.*(SQMKCH+SQMPI-SQMP0)/SQMPI,
     &           Y1 = 2.*MKCH/SQMPI,
     &           YMAX = Y0 - Y1*EMIN0 )
C                              USE THE INVARIANT VARIABLE Y
C                              Y = Y0 - Y1*E

C  parameters according to empirical parametrization paper
      real*8 g,h,k0,a,b,p,q,wi,ut,s0,u0
      parameter    (G = 0.6715,  H = -0.0270, K0 = 0.0081,
     &  A = -0.1299, B = -0.0378, P = 0.0661, Q = 0.4474, wi = 0.00015)
      common/decayaux/ut,s0,u0

      INTEGER IOK,J,JPI,JPI01,JPI02,ifoi,jgam,ipch,ip01,ip02,nradgam,i
      INTEGER pi0decay,pzmode1, pzmode2
      REAL*8 U,V,CUSP,WTME,WCOMP,WDMX,aux
      REAL*8 PI(4),PI01(4),PI02(4),dp(4,99)
      DATA wdmx /0.8346/,ifoi/0/

      REAL*8 RANF
C======================

      RADCOR=.FALSE.
      IF (RADFLG.GT.0) RADCOR=.TRUE.

C --- GENERATE uniform phase space
      KGENEV   = 1
      NP       = 3
      AMASS(1) = MPI
      AMASS(2) = MP0
      AMASS(3) = MP0
      TECM     = MKCH
      s0 = (sqmkch+2.*sqmp0+sqmpi)/3.
      u0 =  y0      !        2.*(sqmkch-sqmp0+sqmpi)/(3.*sqmpi)
      Ut = (4.*sqmpi - s0)/sqmpi
      if (ifoi.eq.0) then
         ifoi = ifoi +1
c         print *,'==============empirical param for pi pi0 pi0 =='
c         print *,'| s0,u0,ut  ',s0,u0,ut,'  E(CM) ',TECM 
c         print *,'=============='
      endif
      NTrials = 0

c ... save the kaon momentum as it's overwritten and biasing cannot work
c ... otherwise
      do j = 1, 4
         p4kaon(j) = p4ini(j,jkaon)
      enddo

 1    CONTINUE

      CALL GENBOD_fix(iok)
      if (iok.eq.0) goto 1

C --- KINEMATIC VARIABLES
      U = y0 - y1*PCM(4,1)    ! u0 - 2.*MKCH*PCM(4,1)/SQMPI
C      U = (sqmkch + sqmpi -2.*MKCH*PCM(4,1) -s0)/ sqmpi
      V = y1*(PCM(4,2)-PCM(4,3)) ! 2*MKCH*(PCM(4,2)-PCM(4,3))/SQMPI
C --- MATRIX ELEMENT with U,U2 and V2 terms
C --- Cusp term, below and above threshold
      if(U.le.Ut) then
        cusp = A*(Ut-U)**Q
      else
        cusp = B*(U-Ut)**Q
      endif
      wtme = (1.+0.5*G*U+0.5*H*U*U+0.5*K0*V*V+cusp)**2
C --- correction for extra narrow peak  
      if(abs(U-Ut).le.0.5*wi) wtme = wtme*(1.+P*wi)
C --- rejection for generation according to phase space* ME
      if (wt*wtme.gt.wdmx) then
      write(0,*) 'warning,wt*wtme,max = ',wt*wtme,wdmx,' to be updated!'
C     wdmx = wtme
      endif
      aux = wt
      WCOMP = WDMX*RANF()
      IF (WCOMP.GT.WT*WTME)  GOTO 1
C --- store 4-momenta
      DO J = 1, 4
        PI(j)   = PCM(j,1)
        PI01(j) = PCM(j,2)
        PI02(j) = PCM(j,3)
      ENDDO

C      print *,' u ',y0 - y1*PCM(4,1)
C      print *,' moo2 from u ',u*SQMPI+s0
C      print *,' moo2 K-pi ',sqmkch + sqmpi -2.*MKCH*PCM(4,1)
C      print *,' moo2 pi0+pi0',2.*(sqmp0+ PCM(4,2)*PCM(4,3)-
C     & PCM(3,2)*PCM(3,3)-PCM(2,2)*PCM(2,3)-PCM(1,2)*PCM(1,3))

c --- APPLY RADIATIVE CORRECTIONS if requested (PHOTOS)
      NRADGAM = 0
      nhep = 4
      IF (.NOT.RADCOR) GOTO 3   ! there is nothing more to do
C      if (ifoi.le.5) print*,' calling photos at event ',ifoi

c ... FILL HEPEVT COMMON BLOCK

c ... kaon in rest frame
      isthep(1)   = 2   ! particle is decayed
      idhep(1)    = 321 ! pdg identifier for K+
      jmohep(1,1) = 0   ! no mother for kaon
      jmohep(2,1) = 0
      jdahep(1,1) = 2   !  first daughter index
      jdahep(2,1) = 4   !  last  daughter index, daugthers are in 2-3-4
      phep(1,1)   = 0.
      phep(2,1)   = 0.
      phep(3,1)   = 0.
      phep(4,1)   = MKCH ! energy
      phep(5,1)   = MKCH ! mass
      do i = 1,4
         vhep(i,1) = 0.
      enddo
c ... charged pion
      isthep(2)   = 1
      idhep(2)    = 211  ! pdg identifier for pi+
      jmohep(1,2) = 1    ! mother particle: kaon
      jmohep(2,2) = 0
      jdahep(1,2) = 0    !  first daughter index
      jdahep(2,2) = 0    !  last  daughter index
      phep(1,2)   = PI(1)
      phep(2,2)   = PI(2)
      phep(3,2)   = PI(3)
      phep(4,2)   = PI(4)
      phep(5,2)   = MPI
      do i = 1,4
         vhep(i,2) = 0.
      enddo
c ... first pi0
      isthep(3)   = 1    ! pi0 is stable at this point, decays later
      idhep(3)    = 111  ! pdg identifier for charged pion
      jmohep(1,3) = 1    ! mother particle: kaon
      jmohep(2,3) = 0
      jdahep(1,3) = 0    !  first daughter index
      jdahep(2,3) = 0    !  last  daughter index
      phep(1,3)   = PI01(1)
      phep(2,3)   = PI01(2)
      phep(3,3)   = PI01(3)
      phep(4,3)   = PI01(4)
      phep(5,3)   = MP0
      do i = 1,4
         vhep(i,3) = 0.
      enddo
c ... second pi0
      isthep(4)   = 1  ! pi0 is stable at this point, decays later
      idhep(4)    = 111 ! pdg identifier for charged pion
      jmohep(1,4) = 1    ! mother particle: kaon
      jmohep(2,4) = 0
      jdahep(1,4) = 0    !  first daughter index
      jdahep(2,4) = 0    !  last  daughter index
      phep(1,4)   = PI02(1)
      phep(2,4)   = PI02(2)
      phep(3,4)   = PI02(3)
      phep(4,4)   = PI02(4)
      phep(5,4)   = MP0
      do i = 1,4
         vhep(i,4) = 0.
      enddo

c ... CALL PHOTOS, READ NEW MOMENTA FROM HEPEVT
C      call phodmp
      call photos(1)
      nradgam = nhep-4
      if (nradgam.eq.0) go to 3  ! nothing has changed
      ifoi = ifoi + 1
C reload 4-momenta of daughters after radiation, identify particles
      ip01 = 0
      ip02 = 0
      ipch = 0
      do j = 2,nhep
        if (abs(idhep(j)).eq.211) ipch = j
        if (idhep(j).eq.111) then
           if (ip01.eq.0) then
               ip01 = j
           elseif (ip02.eq.0) then
               ip02 = j
           endif
         endif
      enddo
      do i = 1,4      
         PI(i)   = phep(i,ipch) 
         PI01(i) = phep(i,ip01) 
         PI02(i) = phep(i,ip02) 
      enddo

 3    continue

C     Save original particles in K rest frame into GeneParts
      JPI   = MCADD4GEN(IDPIP, PI,   0)
      JPI01 = MCADD4GEN(IDPIZ, PI01, 0)
      JPI02 = MCADD4GEN(IDPIZ, PI02, 0)

C     Boost into laboratory frame
      CALL DBOOST(p4kaon,MKCH,PI,PI)
      CALL DBOOST(p4kaon,MKCH,PI01,PI01)
      CALL DBOOST(p4kaon,MKCH,PI02,PI02)

C     Handle also the radiative photons
      if (nradgam.gt.0) then
        do  j = 2, nhep
          if (idhep(j).eq.22) then ! PDG ID for photon
            do i = 1,4
              dp(i,j) = phep(i,j)
            enddo
            jgam = MCADD4GEN(IDGAM, dp(1,j), 2)
            call DBOOST(p4kaon,MKCH,dp(1,j),dp(1,j))
          endif
        enddo
      endif
c      if (nradgam.ge.1 .and. ifoi.le.10) then
c        print *,' == rad event ',ifoi,nradgam,' photons'
c      endif

C     Register particles for tracking
      JPI   = MCADD4(IDPIP, PI)
      JPI01 = MCADD4(IDPIZ, PI01)
      JPI02 = MCADD4(IDPIZ, PI02)
      if (nradgam.gt.0) then
        do j = 2, nhep
          if (idhep(j).eq.22) jgam = MCADD4(IDGAM, dp(1,j))
        enddo
      endif

C --- FORCED PI0 DECAYS

      pzmode1 = mod (pi0decay, 100)
      pzmode2 = mod (pi0decay/100, 100)
      
      call pi0decay_manager(jpi01, pi01, pzmode1)
      call pi0decay_manager(jpi02, pi02, pzmode2)
      
c ... Biasing based on the photon-photon angle
      if (twoPhotMaxAngle.GT.0) then
        NTrials = NTrials+1
        GOODCOMBINATION = .FALSE.
        DO I = NPART-3, NPART
          DO J = I+1, NPART
          PHOTILEN = SQRT(p4ini(1,i)**2+p4ini(2,i)**2+p4ini(3,i)**2)
          PHOTJLEN = SQRT(p4ini(1,j)**2+p4ini(2,j)**2+p4ini(3,j)**2)
          scalarProd = p4ini(1,i)*p4ini(1,j)
     $               + p4ini(2,i)*p4ini(2,j)
     $               + p4ini(3,i)*p4ini(3,j)
          photonsAngle = ACOS(scalarProd/(PHOTILEN*PHOTJLEN))
          IF (photonsAngle.LT.twoPhotMaxAngle) GOODCOMBINATION = .TRUE.
          ENDDO
        ENDDO
        IF (.NOT. GOODCOMBINATION) then
          NPART = 0
          NPARTGEN = 0
          GOTO 1
        endif
      endif
      RETURN
      END
