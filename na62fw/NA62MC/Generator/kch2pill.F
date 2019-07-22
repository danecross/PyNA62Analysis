      SUBROUTINE KCH2PILL(JKAON,MODE,RADFLAG)
*==========================================================*
* Generator of decays K+- -> PI+- L+ L- (L=e,mu) at O(p^6) *
* Reference: G.D'Ambrosio et al., hep-ph/9808289           *
*                                                          *
* E.Goudzovski                                  08/01/2004 *
* Radiative corrections (PHOTOS) added ...      06/02/2004 *
* Bug fixes (photos call, y distribution) ...   24/03/2005 *
* Adapted for NA62 ...                          07/03/2011 *
*==========================================================*

#include "common_blocks.f"
#include "masses.f"

C ... INPUT
      INTEGER MODE    ! 1=(pi e e), 2=(pi mu mu)
      INTEGER RADFLAG ! rad.corr: 0=no, 1=photos (piee only), 2=photos(piee only)+coulomb
C ... ChPT parameters
      REAL*8 GFERMI, ALPHA, BETA, FSC
      PARAMETER (GFERMI=1.16637e-5,ALPHA=-20.6e-8,BETA=-2.8e-8)
      PARAMETER (FSC=0.007297352568)   ! fine-structure constant   
      REAL*8 AA,BB
      PARAMETER (AA=-0.584,BB=-0.700) ! Form-factor parameters: average of NA48/2 and E865
C...PARAMETERS
      REAL*8    REL, REL2, RMU, RMU2, WTMAX, RANF
      REAL*8    TOPWT, TOPWT_EE, TOPWT_MM, WTMAXC
      PARAMETER (TOPWT_EE=0.37) ! max genbod weight: piee
      PARAMETER (TOPWT_MM=0.47) ! max genbod weight: pimumu
      PARAMETER (WTMAXC=10.0)   ! for the Coulomb correction --> there is a pole
      PARAMETER (REL=MEL/MKCH, REL2=REL**2)
      PARAMETER (RMU=MMU/MKCH, RMU2=RMU**2)
      PARAMETER (WTMAX=2.4) ! tuned for both modes for the fixed AA and BB
C...VARIABLES
      INTEGER iok, I, IL1, IL2, JPI, JL1, JL2, JG, JG2, JG3, NRADGAMMA
      REAL*8 RL2, XX, ZZ, MPIE2, MEE2, A2, SQ
      REAL*8 ZR, WPOL, GRE, GIM, XRE, XIM, Z0, FACTOR
      REAL*8 WLOOPRE, WLOOPIM, WABS2, WCOMP, WTME
      REAL*8 PPI(4),PLEP(4),PL1(4),PL2(4),PRAD1(4),PRAD2(4),PRAD3(4)
      REAL*8 m_i(3), m_j(3), X(3), A(3), B(3), R(3)

C --- Radiative correction flag validity check
      if ((mode.eq.1.and.(radflag.lt.0.or.radflag.gt.2)).or.
     >    (mode.eq.2.and.radflag.ne.0.and.radflag.ne.2)) then
         write(*,*) "[KCH2PILL] WARNING: invalid radcor flag, set to 0"
         radflag = 0
      endif

      IF (MODE.EQ.1) RL2 = REL2
      IF (MODE.EQ.2) RL2 = RMU2

C --- GENERATE EVENT UNIFORMLY IN PHASE SPACE (CERNLIB GENBOD)
      KGENEV   = 1
      NP       = 3
      TECM     = MKCH
      AMASS(1) = MPI
      IF (MODE.EQ.1) THEN
         AMASS(2) = MEL
         AMASS(3) = MEL
         TOPWT    = TOPWT_EE
         IL1      = IDELEP
         IL2      = IDELEM
      ENDIF
      IF (MODE.EQ.2) THEN
         AMASS(2) = MMU
         AMASS(3) = MMU
         TOPWT    = TOPWT_MM
         IL1      = IDMUP
         IL2      = IDMUM
      ENDIF

 1    CONTINUE
      CALL GENBOD_FIX(iok)
      if (iok.eq.0) goto 1

      WCOMP = RANF()
      IF (WCOMP.GT.(WT/TOPWT)) GOTO 1

C --- KINEMATIC VARIABLES
      MPIE2 = (PCM(4,1)+PCM(4,2))**2 - (PCM(1,1)+PCM(1,2))**2 -
     >        (PCM(2,1)+PCM(2,2))**2 - (PCM(3,1)+PCM(3,2))**2
      MEE2  = (PCM(4,2)+PCM(4,3))**2 - (PCM(1,2)+PCM(1,3))**2 -
     >        (PCM(2,2)+PCM(2,3))**2 - (PCM(3,2)+PCM(3,3))**2
      XX    = MPIE2/SQMKCH
      ZZ    = MEE2/SQMKCH

c --- CALCULATE THE SQUARED FORM-FACTOR
      ZR      = ZZ/RPI2
      SQ      = SQRT(ABS(1.0-4.0/ZR))
      WPOL    = 1e6*GFERMI*SQMKCH*(AA + BB*ZZ)
      IF (ZR.LE.4.0) THEN
         GRE  = SQ*ASIN(0.5*SQRT(ZR))
         GIM  = 0.0
      ELSE
         GRE  = -0.5*SQ*DLOG((1.-SQ)/(1.+SQ))
         GIM  = -0.5*SQ*3.14159265359
      ENDIF
      XRE     = 4./9.-4.*RPI2/3./ZZ-(1.-4./ZR)*GRE/3.
      XIM     = -(1.-4./ZR)*GIM/3.
      Z0      = 1./3.+RPI2
      FACTOR  = (1.+ZZ/2.5)*(ALPHA+BETA*(ZZ-Z0)/RPI2)/RPI2
      WLOOPRE = 1e6*FACTOR*XRE
      WLOOPIM = 1e6*FACTOR*XIM
      WABS2   = (WLOOPRE+WPOL)**2+WLOOPIM**2

C --- SQUARED AMPLITUDE
      A2 = (2*XX+ZZ-2*RPI2-2*RL2)*(2+2*RL2-2*XX-ZZ)+ZZ*(ZZ-2-2*RPI2)
      A2 = A2*WABS2

      WCOMP = RANF() * WTMAX
      IF (WCOMP.GT.A2) GOTO 1

C --- FILL THE STANDARD ARRAYS
      DO I = 1,4
         PPI(I)  = PCM(I,1)
         PL1(I)  = PCM(I,2)
         PL2(I)  = PCM(I,3)
         PLEP(I) = PL1(I)+PL2(I)
      ENDDO

c --- APPLY RADIATIVE CORRECTIONS (PHOTOS) TO GAMMA->EE

      NRADGAMMA = 0
      if (mode.ne.1)   goto 99  ! Decay into muons, skip this stage
      if (radflag.eq.0) goto 99 ! Photos correcton is not requested, skip this stage

c ... FILL HEPEVT COMMON BLOCK

      nhep = 5

c ... kaon
      isthep(1)   = 2
      idhep(1)    = 321 ! pdg identifier for kaon
      jmohep(1,1) = 0   ! no mother for kaon
      jmohep(2,1) = 0
      jdahep(1,1) = 2   !  first daughter index
      jdahep(2,1) = 3   !  last  daughter index
      phep(1,1)   = 0.
      phep(2,1)   = 0.
      phep(3,1)   = 0.
      phep(4,1)   = MKCH ! energy
      phep(5,1)   = MKCH ! mass
      do i = 1,4
         vhep(i,1) = 0.
      enddo

c ... pion
      isthep(2)   = 1
      idhep(2)    = 211 ! pdg identifier for pion
      jmohep(1,2) = 1   ! mother particle: kaon
      jmohep(2,2) = 0
      jdahep(1,2) = 0   !  first daughter index
      jdahep(2,2) = 0   !  last  daughter index
      phep(1,2)   = PPI(1)
      phep(2,2)   = PPI(2)
      phep(3,2)   = PPI(3)
      phep(4,2)   = PPI(4)
      phep(5,2)   = MPI
      do i = 1,4
         vhep(i,2) = 0.
      enddo

c ... gamma
      isthep(3)   = 2
      idhep(3)    = 22  ! pdg identifier for gamma
      jmohep(1,3) = 1   ! mother particle: kaon
      jmohep(2,3) = 0
      jdahep(1,3) = 4   !  first daughter index
      jdahep(2,3) = 5   !  last  daughter index
      phep(1,3)   = PLEP(1)
      phep(2,3)   = PLEP(2)
      phep(3,3)   = PLEP(3)
      phep(4,3)   = PLEP(4)
      phep(5,3)   = SQRT(MEE2)
      do i = 1,4
         vhep(i,3) = 0.
      enddo

c ... positron
      isthep(4)   = 1
      idhep(4)    = -11
      jmohep(1,4) = 3
      jmohep(2,4) = 0
      jdahep(1,4) = 0
      jdahep(2,4) = 0
      phep(1,4)   = PL1(1)
      phep(2,4)   = PL1(2)
      phep(3,4)   = PL1(3)
      phep(4,4)   = PL1(4)
      phep(5,4)   = MEL
      do i = 1,4
         vhep(i,4) = 0.
      enddo

c ... electron
      isthep(5)   = 1
      idhep(5)    = 11
      jmohep(1,5) = 3
      jmohep(2,5) = 0
      jdahep(1,5) = 0
      jdahep(2,5) = 0
      phep(1,5)   = PL2(1)
      phep(2,5)   = PL2(2)
      phep(3,5)   = PL2(3)
      phep(4,5)   = PL2(4)
      phep(5,5)   = MEL
      do i = 1,4
         vhep(i,5) = 0.
      enddo

c ... CALL PHOTOS, READ NEW MOMENTA FROM HEPEVT
      call photos(1)
      nradgamma = nhep-5
      if (nradgamma.gt.0) then  ! radiative gammas generated
         do i = 1,4
            PPI(i)  = phep(i,2) ! pion
            PL1(i)  = phep(i,4) ! positron
            PL2(i)  = phep(i,5) ! electron
            if (nradgamma.gt.0) PRAD1(i) = phep(i,6) ! 1st gamma
            if (nradgamma.gt.1) PRAD2(i) = phep(i,7) ! 2nd gamma
            if (nradgamma.gt.2) PRAD3(i) = phep(i,8) ! 3rd gamma
         enddo
      endif

 99   CONTINUE

c --- The Coulomb correction [G. Isidori, EPJ C53 (2008) 567; arXiv:0709.2439]
c
c     Indices: 1 = l+ ; 2 = l-; 3 = pi+.
c     X(i) are squared invariant masses:
c     X(1) for pair 12 (l+l-)
c     X(2) for pair 13 (l+pi+) --> the same-sign pair
c     X(3) for pair 23 (l-pi+)

      if (radflag.ne.2) goto 999 ! Coulomb correcton not requested, skip this stage

      IF (MODE.EQ.1) THEN ! electrons
         m_i(1) = MEL
         m_i(2) = MEL
         m_i(3) = MEL
         m_j(1) = MEL
      ELSE                ! muons
         m_i(1) = MMU
         m_i(2) = MMU
         m_i(3) = MMU
         m_j(1) = MMU
      ENDIF
      m_j(2) = MPI
      m_j(3) = MPI

c ... Squared masses of the particle pairs
      X(1) = (PL1(4)+PL2(4))**2 - (PL1(1)+PL2(1))**2 -
     >       (PL1(2)+PL2(2))**2 - (PL1(3)+PL2(3))**2
      X(2) = (PL1(4)+PPI(4))**2 - (PL1(1)+PPI(1))**2 -
     >       (PL1(2)+PPI(2))**2 - (PL1(3)+PPI(3))**2
      X(3) = (PL2(4)+PPI(4))**2 - (PL2(1)+PPI(1))**2 -
     >       (PL2(2)+PPI(2))**2 - (PL2(3)+PPI(3))**2

c .. Relative velocities B(I) and the Coulomb factor
      DO I = 1, 3
        IF (X(I)<=(m_i(I)+m_j(I))**2) THEN
          B(I) = 0.0
          R(I) = 0.0
        ELSE
          B(I) = SQRT(1.0 + ((m_i(I)**2-m_j(I)**2)/X(I))**2 -
     >    2.0*(m_i(I)**2+m_j(I)**2)/X(I))/
     >    (1.0-(m_i(I)**2+m_j(I)**2)/X(I))
          A(I) = 6.2831853072*FSC/B(I)

          IF (I.NE.2) A(I) = -A(I) ! swap sign for the different sign pairs
          R(I) = A(I)/(EXP(A(I))-1.0)
        ENDIF
      ENDDO

      WTME = R(1)*R(2)*R(3)
      WCOMP = RANF() * WTMAXC
      IF (WCOMP.GT.WTME) GOTO 1

 999  CONTINUE

C --- Save original particles in K rest frame into GeneParts
      JPI = MCADD4GEN(IDPIP, PPI, 0)
      JL1 = MCADD4GEN(IL1,   PL1, 0) ! positive lepton
      JL2 = MCADD4GEN(IL2,   PL2, 0) ! negative lepton
      if (nradgamma.gt.0) JG  = MCADD4GEN(IDGAM, PRAD1, 2)
      if (nradgamma.gt.1) JG2 = MCADD4GEN(IDGAM, PRAD2, 2)
      if (nradgamma.gt.2) JG3 = MCADD4GEN(IDGAM, PRAD3, 2)

C --- BOOST FROM KAON FRAME INTO LAB FRAME
      CALL DBOOST( p4ini(1,jkaon), MKCH, ppi, ppi )
      CALL DBOOST( p4ini(1,jkaon), MKCH, pl1, pl1 )
      CALL DBOOST( p4ini(1,jkaon), MKCH, pl2, pl2 )
      IF (nradgamma.gt.0) CALL DBOOST(p4ini(1,jkaon),MKCH,PRAD1,PRAD1)
      IF (nradgamma.gt.1) CALL DBOOST(p4ini(1,jkaon),MKCH,PRAD2,PRAD2)
      IF (nradgamma.gt.2) CALL DBOOST(p4ini(1,jkaon),MKCH,PRAD3,PRAD3)

C --- PASS DAUGHTER PARTICLES TO GEANT4 FOR TRACKING
      JPI = MCADD4(IDPIP, PPI)
      JL1 = MCADD4(IL1,   PL1) ! positive lepton
      JL2 = MCADD4(IL2,   PL2) ! negative lepton
      if (nradgamma.gt.0) JG  = MCADD4(IDGAM, PRAD1)
      if (nradgamma.gt.1) JG2 = MCADD4(IDGAM, PRAD2)
      if (nradgamma.gt.2) JG3 = MCADD4(IDGAM, PRAD3)

      RETURN
      END
