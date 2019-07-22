      SUBROUTINE KCH2PIPIPI(JKAON,RADFLG)
C====================================================
C GENERATE THE DECAY K+- -> PI+-PI+PI-
C NA48 VERSION:          APRIL 99     C.CHESHKOV
C COULOMB CORRECTIONS:   DECEMBER 04  E.GOUDZOVSKI
C RADIATIVE CORRECTIONS: SEPTEMBER 06 E.GOUDZOVSKI
C NA62 VERSION:          APRIL 2011   E.GOUDZOVSKI
C
C RADFLG=1 --> Coulomb corr only
C RADFLG=2 --> Coulomb corr + radiative corr (PHOTOS)
C====================================================

#include "common_blocks.f"
#include "masses.f"

      INTEGER RADFLG

      REAL*8    TOPWT, UMIN, UMAX, ALPHA
      PARAMETER (ALPHA=0.007297352568) ! fine-structure constant
      PARAMETER (TOPWT=0.49)           ! weight limit for genbod
      PARAMETER (UMIN=3.0-SQMKCH/3.0/SQMPI)
      PARAMETER (UMAX=2.0/3.0*SQMKCH/SQMPI-2.0*MKCH/MPI)

      REAL*8    G, H, K, WTMAX, WTMAXC
      PARAMETER (G = -0.21134, H = 0.01848, K = -0.00463, ! measured by NA48/2
     &           WTMAX  = 1. + G*UMIN + H*UMIN*UMIN,
     &           WTMAXC = 10.0)

      INTEGER I,J,NRADGAMMA,iok
      INTEGER JPI1, JPI2, JPI3
      INTEGER JG1, JG2, JG3
      REAL*8  PPI1(4), PPI2(4), PPI3(4), PRAD1(4), PRAD2(4), PRAD3(4)
      REAL*8  U, V, WTME, S0, WCOMP, X(3), B(3), A(3), R(3), RANF

C-------------------------------------------------------------
C --- GENERATE EVENT UNIFORMLY IN PHASE SPACE (CERNLIB GENBOD)
      KGENEV   = 1
      NP       = 3
      AMASS(1) = MPI
      AMASS(2) = MPI
      AMASS(3) = MPI
      TECM     = MKCH

 1    CONTINUE
      npartgen = 0
      CALL GENBOD_FIX(iok)
      if (iok.eq.0) goto 1

      WCOMP = RANF()
      IF (WCOMP.GT.(WT/TOPWT)) GOTO 1

C --- KINEMATIC VARIABLES
      U = 2*MKCH*(MKCH/3.0-PCM(4,1))/SQMPI
      V = 2*MKCH*(PCM(4,2)-PCM(4,3))/SQMPI

C --- MATRIX ELEMENT
      WTME  = 1.0 + G*U + H*U*U + K*V*V
      WCOMP = RANF() * WTMAX
      IF (WCOMP.GT.WTME) GOTO 1

      DO J = 1, 4
        PPI1(j) = PCM(j,1)
        PPI2(j) = PCM(j,2)
        PPI3(j) = PCM(j,3)
      ENDDO

      NRADGAMMA = 0
      IF (RADFLG.EQ.0) GOTO 3 ! no radiative corrections
      IF (RADFLG.EQ.1) GOTO 2 ! Coulomb factor only

c --- APPLY PHOTOS RADIATIVE CORRECTIONS

c ... FILL HEPEVT COMMON BLOCK
      nhep = 4

c ... kaon
      isthep(1)   = 2
      idhep(1)    = 321 ! pdg identifier for kaon
      jmohep(1,1) = 0   ! no mother for kaon
      jmohep(2,1) = 0
      jdahep(1,1) = 2   !  first daughter index
      jdahep(2,1) = 4   !  last  daughter index
      phep(1,1)   = 0.
      phep(2,1)   = 0.
      phep(3,1)   = 0.
      phep(4,1)   = MKCH ! energy
      phep(5,1)   = MKCH ! mass
      do i = 1,4
         vhep(i,1) = 0.
      enddo

c ... odd pion
      isthep(2)   = 1
      idhep(2)    = -211 ! pdg identifier for odd pion
      jmohep(1,2) = 1    ! mother particle: kaon
      jmohep(2,2) = 0
      jdahep(1,2) = 0    !  first daughter index
      jdahep(2,2) = 0    !  last  daughter index
      phep(1,2)   = PPI1(1)
      phep(2,2)   = PPI1(2)
      phep(3,2)   = PPI1(3)
      phep(4,2)   = PPI1(4)
      phep(5,2)   = MPI
      do i = 1,4
         vhep(i,2) = 0.
      enddo

c ... even pion #1
      isthep(3)   = 1
      idhep(3)    = 211  ! pdg identifier for even pion
      jmohep(1,3) = 1    ! mother particle: kaon
      jmohep(2,3) = 0
      jdahep(1,3) = 0    !  first daughter index
      jdahep(2,3) = 0    !  last  daughter index
      phep(1,3)   = PPI2(1)
      phep(2,3)   = PPI2(2)
      phep(3,3)   = PPI2(3)
      phep(4,3)   = PPI2(4)
      phep(5,3)   = MPI
      do i = 1,4
         vhep(i,3) = 0.
      enddo

c ... even pion #2
      isthep(4)   = 1
      idhep(4)    = 211  ! pdg identifier for even pion
      jmohep(1,4) = 1    ! mother particle: kaon
      jmohep(2,4) = 0
      jdahep(1,4) = 0    !  first daughter index
      jdahep(2,4) = 0    !  last  daughter index
      phep(1,4)   = PPI3(1)
      phep(2,4)   = PPI3(2)
      phep(3,4)   = PPI3(3)
      phep(4,4)   = PPI3(4)
      phep(5,4)   = MPI
      do i = 1,4
         vhep(i,4) = 0.
      enddo

c ... CALL PHOTOS, READ NEW MOMENTA FROM HEPEVT
      call photos(1)
      nradgamma = nhep-4

      do i = 1,4
         PPI1(i) = dble(phep(i,2))
         PPI2(i) = dble(phep(i,3))
         PPI3(i) = dble(phep(i,4))
      enddo
      if (nradgamma.gt.0) PRAD1(i) = dble(phep(i,5)) ! 1st gamma
      if (nradgamma.gt.1) PRAD2(i) = dble(phep(i,6)) ! 2nd gamma
      if (nradgamma.gt.2) PRAD3(i) = dble(phep(i,7)) ! 3rd gamma

C --- COULOMB CORRECTIONS [Ref: G. Isidori, arXiv:0709.2439]
 2    CONTINUE

c ... Squared invariant masses of pion pairs
      IF (NRADGAMMA.EQ.0) THEN
        S0   = SQMKCH/3.0+SQMPI
        X(1) = S0 + U*SQMPI
        X(2) = S0 - 0.5*(U+V)*SQMPI
        X(3) = S0 - 0.5*(U-V)*SQMPI
      ELSE ! any gammas radiated? --> recompute pipi masses
        X(1) = (PPI2(4)+PPI3(4))**2 - (PPI2(1)+PPI3(1))**2 -
     >         (PPI2(2)+PPI3(2))**2 - (PPI2(3)+PPI3(3))**2
        X(2) = (PPI1(4)+PPI3(4))**2 - (PPI1(1)+PPI3(1))**2 -
     >         (PPI1(2)+PPI3(2))**2 - (PPI1(3)+PPI3(3))**2
        X(3) = (PPI1(4)+PPI2(4))**2 - (PPI1(1)+PPI2(1))**2 -
     >         (PPI1(2)+PPI2(2))**2 - (PPI1(3)+PPI2(3))**2
      ENDIF

c ... Relative velocities and the Coulomb factor
      DO J = 1, 3
        IF (X(J)/SQMPI<=4.0) THEN
          B(J) = 0.0
          R(J) = 0.0
        ELSE
          B(J) = SQRT(1.0-4.0*SQMPI/X(J))/(1.0-2.0*SQMPI/X(J))
          A(J) = 6.2831853072*ALPHA/B(J)
          IF (J.NE.1) A(J) = -A(J) ! swap sign for different-sign pairs
          R(J) = A(J)/(EXP(A(J))-1.0)
        ENDIF
      ENDDO

      WTME = R(1)*R(2)*R(3)
      WCOMP = RANF() * WTMAXC
      IF (WCOMP.GT.WTME) GOTO 1

 3    CONTINUE

C --- Save the GeneParts
      JPI1 = MCADD4GEN(IDPIM, PPI1, 0)
      JPI2 = MCADD4GEN(IDPIP, PPI2, 0)
      JPI3 = MCADD4GEN(IDPIP, PPI3, 0)
      if (nradgamma.gt.0) JG1 = MCADD4GEN(IDGAM, PRAD1, 2)
      if (nradgamma.gt.1) JG2 = MCADD4GEN(IDGAM, PRAD2, 2)
      if (nradgamma.gt.2) JG3 = MCADD4GEN(IDGAM, PRAD3, 2)

C --- BOOST PARTICLES INTO LAB FRAME
      CALL DBOOST(p4ini(1,jkaon), MKCH, ppi1, ppi1)
      CALL DBOOST(p4ini(1,jkaon), MKCH, ppi2, ppi2)
      CALL DBOOST(p4ini(1,jkaon), MKCH, ppi3, ppi3)
      IF (nradgamma.gt.0) CALL DBOOST(p4ini(1,jkaon),MKCH,PRAD1,PRAD1)
      IF (nradgamma.gt.1) CALL DBOOST(p4ini(1,jkaon),MKCH,PRAD2,PRAD2)
      IF (nradgamma.gt.2) CALL DBOOST(p4ini(1,jkaon),MKCH,PRAD3,PRAD3)

C --- PASS DAUGHTER PARTICLES TO GEANT4 FOR TRACKING
      JPI1 = MCADD4(IDPIM, PPI1)
      JPI2 = MCADD4(IDPIP, PPI2)
      JPI3 = MCADD4(IDPIP, PPI3)
      IF (nradgamma.gt.0) JG1 = MCADD4(IDGAM, PRAD1)
      IF (nradgamma.gt.1) JG2 = MCADD4(IDGAM, PRAD2)
      IF (nradgamma.gt.2) JG3 = MCADD4(IDGAM, PRAD3)

      RETURN
      END
