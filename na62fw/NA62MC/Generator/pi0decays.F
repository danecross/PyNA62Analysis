c    Package for simulation of decays of pi0 produced in primary kaon decays.
c    Includes a steering routine (pi0decay_manager),
c    routines for individual decay modes (mainly adapted from the NA48/2 CMC code)
c    and a number of auxiliary routines.
c
c    If the specified pi0 decay mode 0, then no action is taken,
c    and the pi0 is handled by the standard Geant4 tracking.
c
c    Evgueni.Goudzovski@cern.ch, 11 March 2011
c
c    pi0 dalitz decay update:
c    michal.koval@cern.ch, 29 January 2015
c-------------------------------------------------------------------------

      SUBROUTINE pi0decay_manager(jpi0, ppi0, pzmode)

#include "common_blocks.f"
#include "masses.f"

      integer pzmode, jpi0
      real*8  ppi0(4)

c ... mode=0: pi0 decay is managed by Geant4
      if (pzmode.eq.0) return

c ... If the requested decay mode is not listed below, pi0 decay will decay into invisible

c ... pi0 --> gamma gamma
      if (pzmode.eq.1) call pi0gg (ppi0)

c ... pi0 --> gamma e+ e- (Dalitz decay), generated in C++ generator
      if (pzmode.gt.10.and.pzmode.lt.20) then
         call generate_pi0dal (ppi0, pzmode)      ! call c++ generator
      endif

c ... pi0 --> e+e-
      if (pzmode.gt.20.and.pzmode.lt.30) then
         call generate_pi0ee (ppi0, pzmode)      ! call c++ generator
      endif

c ... pi0 --> e+e-e+e- (double Dalitz decay)
c ... if (pzmode.eq.31) call pi0ddal (ppi0)      ! old (faulty) fortran generator, no radiative corrections
      if (pzmode.gt.30.and.pzmode.lt.40) then
         call generate_pi0ddal (ppi0, pzmode)      ! call the C++ generator
      endif

c ... lepton flavour violating modes
      if (pzmode.eq.41) call pi0_mue (ppi0, 1) ! mu+e-
      if (pzmode.eq.42) call pi0_mue (ppi0, 2) ! mu-e+

c ... decays to photons
      if (pzmode.eq.51) call pi0ggg  (ppi0)
      if (pzmode.eq.52) call pi0gggg (ppi0)
      if (pzmode.eq.53) call pi0gnunu(ppi0)

c ... pi0 --> gamma X
      if (pzmode.eq.90) call pi0gX (ppi0)

c ... pi0 is not to be tracked by Geant, so any other pi0 decay mode is invisible
      KEEP(jpi0) = 0

      return
      end

c-------------------------------------------------
c     Simulation of the pi0 --> gamma gamma decay

      SUBROUTINE PI0GG(PPI0)

#include "common_blocks.f"
#include "masses.f"

      INTEGER I, JG
      REAL*8  PPI0(4), EGAMMA, VEC(3), PG1(4), PG2(4)

C     GENERATE PHOTONS IN THE PI0 FRAME
      CALL GENSPH(VEC)
      EGAMMA = 0.5*MP0
      DO I = 1, 3
        PG1(I) =  EGAMMA * VEC(I)
        PG2(I) = -PG1(I)
      ENDDO
      PG1(4) = EGAMMA
      PG2(4) = EGAMMA

C     Save original particles in pi0 rest frame into GeneParts
      JG = MCADD4GEN(IDGAM, PG1, 1) ! flag=1 means pi0 decay product
      JG = MCADD4GEN(IDGAM, PG2, 1)

C     Boost into laboratory frame
      CALL DBOOST(PPI0, MP0, PG1, PG1)
      CALL DBOOST(PPI0, MP0, PG2, PG2)

C     Register particles for tracking
      JG = MCADD4(IDGAM, PG1)
      JG = MCADD4(IDGAM, PG2)

      RETURN
      END

c-------------------------------------------------
c     Simulation of the pi0 --> gamma X decay

      SUBROUTINE PI0GX(PPI0)

#include "common_blocks.f"
#include "masses.f"

      INTEGER I, JG
      REAL*8  PPI0(4), EGAMMA, VEC(3), PG1(4), PG2(4)

C     GENERATE DAUGHTERS IN THE PI0 FRAME
      CALL GENSPH(VEC)
      EGAMMA = 0.5*(MP0*MP0-EXOMASS*EXOMASS)/MP0
      IF (EGAMMA .le. 0) THEN
         print *, 'PI0GX-Error-', EGAMMA,MP0,EXOMASS
         RETURN
      ENDIF

      DO I = 1, 3
        PG1(I) =  EGAMMA * VEC(I)
        PG2(I) = -PG1(I)
      ENDDO
      PG1(4) = EGAMMA
      PG2(4) = 0.5*(MP0*MP0+EXOMASS*EXOMASS)/MP0

C     Save original particles in pi0 rest frame into GeneParts
      JG = MCADD4GEN(IDGAM, PG1, 1) ! flag=1 means pi0 decay product
      JG = MCADD4GEN(IDEXO, PG2, 1)

C     Boost into laboratory frame
      CALL DBOOST(PPI0, MP0, PG1, PG1)
      CALL DBOOST(PPI0, MP0, PG2, PG2)

C     Register particles for tracking
      JG = MCADD4(IDGAM, PG1)
      JG = MCADD4(IDEXO, PG2)

      RETURN
      END

C========================================================================
c     The following double dalitz decay generator seems not to work well.
c     The decay width should exhibit symmetry under the exchange
c     of samely charged leptons, which is not satisfied in this case.
c     Therefore it is recommended to use the new C++ generator, which
c     also allows for the NLO radiative corrections.
C========================================================================

      SUBROUTINE PI0DDAL(PPI0)

#include "common_blocks.f"
#include "masses.f"

      REAL*8  PPI0(4), PPL1(4), PMI1(4), PPL2(4), PMI2(4)
      INTEGER dbldal, rc, jelec1, jelec2

      rc = dbldal(ppi0, ppl1, pmi1, ppl2, pmi2)
      if( rc .ne.-1 ) then
         print *, 'DBLDAL-W-', rc
      else
         JELEC1 = MCADD4 (IDELEP, PPL1)
         JELEC1 = MCADD4 (IDELEM, PMI1)
         JELEC2 = MCADD4 (IDELEP, PPL2)
         JELEC2 = MCADD4 (IDELEM, PMI2)
      endif

      RETURN
      END

c-------------------------------------------------
C
C         Double Dalitz Decay program
C
C         This function generates weight-1 Double-Dalitz
C         decay final states (e+e-e+e-) for the decay of
C         a pseudoscalar particle whose four-momentum is
C         PTOT.  The resulting four-momenta of the four
C         final-state particles are returned in the remaining
C         arguments.  The matrix element is calculated
C         using the complete lowest-order QED amplitudes.
C
          INTEGER FUNCTION DBLDAL(PTOT,PPL1,PMI1,PPL2,PMI2)

#include "common_blocks.f"
#include "masses.f"

          INTEGER*4 K,NACC,NMISS,MAXTRY,NGEN
          REAL*8 PTOT(4),M1,M2,P11CM(4),P22CM(4)
          REAL*8 PPL1(4),PMI1(4),PPL2(4),PMI2(4)
          REAL*8 PSTSQ,PSTAR,MTOTSQ,ALPHA2
          REAL*8 DOT4,DIAGSQ,M11SQ,M22SQ,M11,M22,MTOT,PI
          REAL*8 PSTQ11,PSTQ22,PST11,PST22,DIAG1,DIAG2
          REAL*8 PPL1CM(4),PPL2CM(4),PMI1CM(4),PMI2CM(4)
          REAL*8 P12CM(4),P21CM(4),M12SQ,M21SQ
          REAL*8 PHASE1,PHASE2,WEIGHT,MXWTPI,MXWTK,RELWT
          REAL*8 C11C21,C11C12,C22C12,C21C22
          REAL*8 PL1PL2,PL1MI2,MI1PL2,MI1MI2
          REAL*8 PP1P11,PP1P12,PP1P21,PP1P22
          REAL*8 PP2P11,PP2P12,PP2P21,PP2P22
          REAL*8 PM1P11,PM1P12,PM1P21,PM1P22
          REAL*8 PM2P11,PM2P12,PM2P21,PM2P22
          REAL*8 INT1,INT2,INT3,INT4,INT5,INT6
          REAL*8 INTERF,F1,F2,F3,F4,F5,F6,MMSQ,FF
          REAL*8 ICONST,ITOT,MPPSQ,MMMSQ
          REAL*8 RANF,P11P12,P21P22,P11P21,P12P22
          REAL*8 PST2SQ,PSTQ12,PSTQ21,GENWT1,GENWT2,GGG
          REAL*8 POWER,TEMP,HIGH,LOW,GCONST,WCONST,DELTAT
          REAL*8 VEC(3)
          INTEGER JEL

          PARAMETER (ALPHA2 = 1./137.06**2)
          PARAMETER (PI = 3.1415926536)
          PARAMETER (FF = 1.)
          PARAMETER (MXWTPI = 0.00021)
          PARAMETER (MXWTK = 0.00035)
          PARAMETER (MAXTRY = 10000)

          DATA POWER /-0.93/
C
C         Initialize status code to zero.
C
          DBLDAL = 0
          MTOTSQ = DOT4(PTOT,PTOT)
          IF (MTOTSQ.LT.16.*SQMEL) GOTO 999
          MTOT = SQRT(MTOTSQ)
          LOW = (2.*MEL)**(POWER + 1.)
          HIGH = (MTOT - 2.*MEL)**(POWER + 1.)
          GCONST = (POWER + 1.)/(HIGH - LOW)
          NMISS = 0

10        M11 = (LOW + RANF()*(HIGH - LOW))**(1./(POWER + 1.))
          M22 = (LOW + RANF()*(HIGH - LOW))**(1./(POWER + 1.))
          NGEN = NGEN + 1
          IF ((M11 + M22).GT.MTOT) GOTO 10
          GENWT1 = GCONST*GCONST*(M11*M22)**POWER
          M11SQ = M11*M11
          M22SQ = M22*M22

          PSTSQ = 0.25*(MTOTSQ - 2.*(M11SQ + M22SQ)
     +                  + (M11SQ - M22SQ)*(M11SQ - M22SQ)/MTOTSQ)
          IF (PSTSQ.LE.0.) GOTO 10
          PSTAR = SQRT(PSTSQ)
          CALL GENSPH(VEC)
          DO 12 K=1,3
             P11CM(K) = PSTAR*VEC(K)
             P22CM(K) = -P11CM(K)
 12       continue
          P11CM(4) = SQRT(M11SQ + PSTSQ)
          P22CM(4) = SQRT(M22SQ + PSTSQ)
          PSTQ11 = 0.25*M11SQ - SQMEL
          PSTQ22 = 0.25*M22SQ - SQMEL
          IF ((PSTQ11.LE.0.).OR.(PSTQ22.LE.0.)) GOTO 10
          PST11 = SQRT(PSTQ11)
          PST22 = SQRT(PSTQ22)
          PPL1CM(4) = 0.5*M11
          PMI1CM(4) = 0.5*M11
          PPL2CM(4) = 0.5*M22
          PMI2CM(4) = 0.5*M22
          CALL GENSPH(VEC)
          DO 20 K=1,3
             PPL1CM(K) = PST11*VEC(K)
             PMI1CM(K) = -PPL1CM(K)
 20       continue
          CALL GENSPH(VEC)
          DO 30 K=1,3
             PPL2CM(K) = PST22*VEC(K)
             PMI2CM(K) = -PPL2CM(K)
 30       continue

          CALL DBOOST(P11CM,M11,PPL1CM,PPL1CM)
          CALL DBOOST(P11CM,M11,PMI1CM,PMI1CM)
          CALL DBOOST(P22CM,M22,PPL2CM,PPL2CM)
          CALL DBOOST(P22CM,M22,PMI2CM,PMI2CM)
          DO 40 K=1,4
             P12CM(K) = PPL1CM(K) + PMI2CM(K)
             P21CM(K) = PPL2CM(K) + PMI1CM(K)
 40       continue
          M12SQ = DOT4(P12CM,P12CM)
          M21SQ = DOT4(P21CM,P21CM)
          GENWT2 = GCONST*GCONST*(M12SQ*M21SQ)**(0.5*POWER)
          PST2SQ = 0.25*(MTOTSQ - 2.*(M12SQ + M21SQ)
     +                   + (M12SQ - M21SQ)*(M12SQ - M21SQ)/MTOTSQ)
          PSTQ12 = 0.25*M12SQ - SQMEL
          PSTQ21 = 0.25*M21SQ - SQMEL
          IF ((PST2SQ.LE.0.).OR.(PSTQ12.LE.0.).OR.(PSTQ21.LE.0))
     +       GOTO 10
          PL1MI2 = DOT4(PPL1CM,PMI2CM)
          PL1PL2 = DOT4(PPL1CM,PPL2CM)
          MI1PL2 = DOT4(PMI1CM,PPL2CM)
          MI1MI2 = DOT4(PMI1CM,PMI2CM)
          MPPSQ = 2.*SQMEL + 2.*PL1PL2
          MMMSQ = 2.*SQMEL + 2.*MI1MI2
C
C         The expression for the matrix element appears in
C
C         Miyazaki and Takasugi, Phys. Rev. D8, 2051 (1973).
C
C         [N.B. There are MANY ERRORS in this paper:
C
C            1. In the expression for M1**2 on page 2061,
C               the (p_+ p_+')**2 in the denominator should be
C               (p_+ + p_+')**2.
C
C            2. In the first expression for the
C               interference term, the third term should be
C               (m**2 - p_-p_-')(p_+(mu)p_+'(alpha) + mu<->alpha)]
C
C            3. In the second expression for M1**2 (using the
C               new variables), the denominator in the final
C               term should be three-vector P**2, not
C               four-momentum P**2.
C
C            4. The expression for M2**2 om page 2062 has two
C               sign errors: the (1 + MU) should be (1 - MU),
C               and the (1 - MU) should be (1 + MU).
C
C            And furthermore, an integration of the interference
C            term over phase space yields a contribution to rho
C            of -0.037 x 10**(-5) for both pi0 and K0 decays to
C            e+ e- e+ e-, not -0.18 and -0.35 as the paper states.]
C
C
C         Two diagrams contribute to the decay.  In the first,
C         electron 1 and positron 1 come from the same virtual
C         photon, as do electron 2 and positron 2.  In the other
C         diagram, positron 1 and electron 2 come from one
C         virtual photon; positron 2 and electron 1 come from
C         the other.   I begin with the squared amplitudes
C         from these diagrams (the "diagonal" terms):
C
          DIAG1 = FF*DIAGSQ(PPL1CM,PMI1CM,PPL2CM,PMI2CM)
          DIAG2 = FF*DIAGSQ(PPL1CM,PMI2CM,PPL2CM,PMI1CM)
C
C         Now for the fun part: the interference term...
C
          F1 = 0.5*M11SQ
          F2 = 0.5*M22SQ
          F3 = 0.5*MMMSQ - 2.*SQMEL
          F4 = -0.5*MPPSQ + 2.*SQMEL
          F5 = -0.5*M21SQ
          F6 = 0.5*M12SQ

          P11P12 = 0.5*M12SQ + 0.5*M11SQ + 0.5*MMMSQ - 2.*SQMEL
          P21P22 = 0.5*M22SQ + 0.5*M21SQ + 0.5*MMMSQ - 2.*SQMEL
          P11P21 = 0.5*M11SQ + 0.5*M21SQ + 0.5*MPPSQ - 2.*SQMEL
          P11P21 = 0.5*M11SQ + 0.5*M21SQ + 0.5*MPPSQ
          P12P22 = 0.5*M22SQ + 0.5*M12SQ + 0.5*MPPSQ - 2.*SQMEL
C
C         Each of the six terms in the interference term
C         is a second-rank tensor with indices alpha, mu
C         (in the paper).  The contraction of each of the
C         tensors with the common coefficienct tensor has
C         five terms.  The first is proportional to
C         delta_(alpha,mu); its total contribution is DELTAT.
C         The other four are proportional to:
C
C         p11_alpha p21_mu      coeff = C11C21
C         p11_alpha p12_mu      coeff = C11C12
C         p22_alpha p12_mu      coeff = C22C12
C         p21_alpha p22_mu      coeff = C21C22
C
          DELTAT = -0.25*SQMEL*(MPPSQ - MMMSQ)
     +                  *((M11SQ - M22SQ)*(M12SQ - M21SQ)
     +                    + (MPPSQ - MMMSQ)*(MTOTSQ + 4.*SQMEL))

          C11C21 = -P12P22
          C11C12 =  P21P22
          C22C12 = -P11P21
          C21C22 =  P11P12

          PP1P11 = 0.5*M11SQ
          PP1P12 = 0.5*M12SQ
          PP1P21 = 0.5*MPPSQ + 0.5*M11SQ - 2.*SQMEL
          PP1P22 = 0.5*MPPSQ + 0.5*M12SQ - 2.*SQMEL
          PM1P11 = 0.5*M11SQ
          PM1P12 = 0.5*M11SQ + 0.5*MMMSQ - 2.*SQMEL
          PM1P21 = 0.5*M21SQ
          PM1P22 = 0.5*M21SQ + 0.5*MMMSQ - 2.*SQMEL
          PP2P11 = 0.5*M21SQ + 0.5*MPPSQ - 2.*SQMEL
          PP2P12 = 0.5*MPPSQ + 0.5*M22SQ - 2.*SQMEL
          PP2P21 = 0.5*M21SQ
          PP2P22 = 0.5*M22SQ
          PM2P11 = 0.5*M12SQ + 0.5*MMMSQ - 2.*SQMEL
          PM2P12 = 0.5*M12SQ
          PM2P21 = 0.5*M22SQ + 0.5*MMMSQ - 2.*SQMEL
          PM2P22 = 0.5*M22SQ

          INT1 = C11C21*(PM2P11*PP2P21 - PP2P11*PM2P21)
     +           + C11C12*(PM2P11*PP2P12 - PP2P11*PM2P12)
     +           + C22C12*(PM2P22*PP2P12 - PP2P22*PM2P12)
     +           + C21C22*(PM2P22*PP2P21 - PP2P22*PM2P21)

          INT2 = C11C21*(PM1P11*PP1P21 - PP1P11*PM1P21)
     +           + C11C12*(PM1P11*PP1P12 - PP1P11*PM1P12)
     +           + C22C12*(PM1P22*PP1P12 - PP1P22*PM1P12)
     +           + C21C22*(PM1P22*PP1P21 - PP1P22*PM1P21)

          INT3 = C11C21*(PP2P11*PP1P21 + PP1P11*PP2P21)
     +           + C11C12*(PP2P11*PP1P12 + PP1P11*PP2P12)
     +           + C22C12*(PP2P22*PP1P12 + PP1P22*PP2P12)
     +           + C21C22*(PP2P22*PP1P21 + PP1P22*PP2P21)

          INT4 = C11C21*(PM2P11*PM1P21 + PM1P11*PM2P21)
     +           + C11C12*(PM2P11*PM1P12 + PM1P11*PM2P12)
     +           + C22C12*(PM2P22*PM1P12 + PM1P22*PM2P12)
     +           + C21C22*(PM2P22*PM1P21 + PM1P22*PM2P21)

          INT5 = C11C21*(PM2P11*PP1P21 - PP1P11*PM2P21)
     +           + C11C12*(PM2P11*PP1P12 - PP1P11*PM2P12)
     +           + C22C12*(PM2P22*PP1P12 - PP1P22*PM2P12)
     +           + C21C22*(PM2P22*PP1P21 - PP1P22*PM2P21)

          INT6 = C11C21*(PP2P11*PM1P21 - PM1P11*PP2P21)
     +           + C11C12*(PP2P11*PM1P12 - PM1P11*PP2P12)
     +           + C22C12*(PP2P22*PM1P12 - PM1P22*PP2P12)
     +           + C21C22*(PP2P22*PM1P21 - PM1P22*PP2P21)

          ITOT = F1*INT1 + F2*INT2 + F3*INT3 + DELTAT
     +                   + F4*INT4 + F5*INT5 + F6*INT6
          ICONST = -64.*FF/(MTOTSQ*M11SQ*M22SQ*M12SQ*M21SQ)
          INTERF = ICONST*ITOT

          MMSQ = DIAG1 + DIAG2 + INTERF
          IF (MMSQ.LT.0.) THEN
             WRITE (0,1290) MMSQ,DIAG1,DIAG2,INTERF
1290         FORMAT(' ERROR: MMSQ = ',G12.6,' D1, D2, I = ',3G12.6)
             NMISS = NMISS + 1
             GOTO 10
          ENDIF

          PHASE1 = PSTAR*PST11*PST22/(256.*PI**5*MTOT)
          PHASE2 = SQRT(PST2SQ*PSTQ12*PSTQ21)/(256.*PI**5*MTOT)
          WCONST = 8.*PI*PI*ALPHA2
     +             /(0.5*MTOT*(GENWT1/PHASE1 + GENWT2/PHASE2))
          GGG = FF*MTOT/(16.*PI)
          WEIGHT = WCONST*MMSQ/GGG

          IF (MTOT.LT.0.2) THEN
             RELWT = WEIGHT/MXWTPI
          ELSE
             RELWT = WEIGHT/MXWTK
          ENDIF
          IF (RELWT.LT.0.) THEN
             DBLDAL = 4
             GOTO 999
          ELSE IF (RELWT.LT.RANF()) THEN
             NMISS = NMISS + 1
             IF (NMISS.GT.MAXTRY) THEN
                DBLDAL = 1
                GOTO 999
             ENDIF
             GOTO 10
          ELSE IF (RELWT.GT.1.) THEN
             DBLDAL = 2
          ELSE
             DBLDAL = -1
          ENDIF
          NACC = NACC + 1

C     Save original particles in pi0 rest frame into GeneParts
          JEL = MCADD4GEN(IDELEP, PPL1CM, 1)
          JEL = MCADD4GEN(IDELEP, PPL2CM, 1)
          JEL = MCADD4GEN(IDELEM, PMI1CM, 1)
          JEL = MCADD4GEN(IDELEM, PMI2CM, 1)

          CALL DBOOST(PTOT,MTOT,PPL1CM,PPL1)
          CALL DBOOST(PTOT,MTOT,PPL2CM,PPL2)
          CALL DBOOST(PTOT,MTOT,PMI1CM,PMI1)
          CALL DBOOST(PTOT,MTOT,PMI2CM,PMI2)

c          IF (RANF().LT.0.5) THEN
c             do K=1,4
c                TEMP = PMI1(K)
c                PMI1(K) = PMI2(K)
c                PMI2(K) = TEMP
c             enddo
c          ENDIF

999       RETURN
          END

c...........................................................
          DOUBLE PRECISION FUNCTION DOT4(P1,P2)
          IMPLICIT NONE
          REAL*8 P1(4),P2(4)
          DOT4 = P1(4)*P2(4) - P1(1)*P2(1) - P1(2)*P2(2) - P1(3)*P2(3)
          RETURN
          END

c...........................................................

          DOUBLE PRECISION FUNCTION DIAGSQ(PPL1,PMI1,PPL2,PMI2)
C
C         This function calculates the diagonal terms of the
C         Double Dalitz matrix element: called by DBLDAL.

#include "common_blocks.f"
#include "masses.f"

          INTEGER*4 I
          REAL*8 PPL1(4),PMI1(4),PPL2(4),PMI2(4)
          REAL*8 P11(4),P22(4),M12SQ,M21SQ
          REAL*8 TERM1,TERM2,TERM3,ASYMM,MPPSQ,MMMSQ
          REAL*8 PSTSQ,DOT4,MTOTSQ,M11SQ,M22SQ

          DO 10 I=1,4
             P11(I) = PPL1(I) + PMI1(I)
             P22(I) = PPL2(I) + PMI2(I)
 10       continue
          M11SQ = DOT4(P11,P11)
          M22SQ = DOT4(P22,P22)
          MTOTSQ = M11SQ + M22SQ + 2.*DOT4(P11,P22)
          PSTSQ = 0.25*(MTOTSQ - 2.*(M11SQ + M22SQ)
     +                  + (M11SQ - M22SQ)*(M11SQ - M22SQ)/MTOTSQ)

          MPPSQ = 2.*(DOT4(PPL1,PPL2) + SQMEL)
          MMMSQ = 2.*(DOT4(PMI1,PMI2) + SQMEL)
          M12SQ = 2.*(DOT4(PPL1,PMI2) + SQMEL)
          M21SQ = 2.*(DOT4(PPL2,PMI1) + SQMEL)

          TERM1 = 0.5*PSTSQ*MTOTSQ*M11SQ*M22SQ

          TERM2 = 0.5*M11SQ*M11SQ*M22SQ*M22SQ
     +            + SQMEL*((PSTSQ*MTOTSQ - 2.*M11SQ*M22SQ)
     +                     *(M11SQ + M22SQ)
     +                     + 2.*M11SQ*M22SQ*(MTOTSQ + 4.*SQMEL))
     +            - 0.25*M11SQ*M22SQ
     +                  *(2.*(MPPSQ*MMMSQ + M12SQ*M21SQ)
     +                    + (MPPSQ + MMMSQ)*(M12SQ + M21SQ))
C
C         Term 3 depends on the angle between the two e+e-
C         planes (the "virtual photon decay planes") for this pairing.
C         It is proportional to Sin(Delta phi)**2:
C
          TERM3 = 4.*(ASYMM(PPL1,PMI1,PPL2,PMI2))**2

          DIAGSQ = 64.*(TERM1 + TERM2 + TERM3)
     +                /(M11SQ*M11SQ*M22SQ*M22SQ*MTOTSQ)

          RETURN

          END

c...........................................................

          DOUBLE PRECISION FUNCTION ASYMM(PA,PB,PC,PD)
C
C         This function calculates the totally antisymmetric
C         invariant resulting from the contraction of the four
C         four-vector arguments with the epsilon tensor.
C
          IMPLICIT NONE

          REAL*8 PA(4),PB(4),PC(4),PD(4),SIGNX
          INTEGER*4 INDICES(0:3,24),IPERM

          DATA INDICES /4,1,2,3,  4,1,3,2,  4,3,1,2,  4,3,2,1,
     +                  4,2,3,1,  4,2,1,3,  1,2,4,3,  1,4,2,3,
     +                  1,4,3,2,  1,2,3,4,  1,3,2,4,  1,3,4,2,
     +                  2,3,4,1,  2,4,3,1,  2,4,1,3,  2,1,4,3,
     +                  2,1,3,4,  2,3,1,4,  3,2,1,4,  3,4,1,2,
     +                  3,4,2,1,  3,1,2,4,  3,1,4,2,  3,2,4,1/

          ASYMM = 0.
          SIGNX = 1.
          DO IPERM=1,24
             ASYMM = ASYMM + SIGNX*PA(INDICES(0,IPERM))
     +                            *PB(INDICES(1,IPERM))
     +                            *PC(INDICES(2,IPERM))
     +                            *PD(INDICES(3,IPERM))
             SIGNX = -SIGNX
          ENDDO

          RETURN
          END

c-------------------------------------------------

        SUBROUTINE PI0_MUE(PPI0, MODE)

C       forced lepton flavour violating decay: pi0 --> mu e

#include "common_blocks.f"
#include "masses.f"

      INTEGER mode, JL, i, jgamma
      REAL*8  PPI0(4), EE, EMU, MOM, VEC(3), PE(4), PMU(4)

C     GENERATE LEPTONS IN THE PI-SYSTEM
      EE  = 0.5 * (SQMP0 - MMU*MMU + MEL*MEL) / MP0
      EMU = 0.5 * (SQMP0 - MEL*MEL + MMU*MMU) / MP0
      MOM = sqrt(EE*EE   - MEL*MEL)

      CALL GENSPH(VEC)
      DO I = 1, 3
        PE(I)  = MOM * VEC(I)
        PMU(I) = -PE(I)
      ENDDO
      PE(4)  = EE
      PMU(4) = EMU

C     Save original particles in pi0 rest frame into GeneParts
      IF (MODE.EQ.1) THEN
        JL = MCADD4GEN(IDMUP,  PMU, 1) ! mu+
        JL = MCADD4GEN(IDELEM, PE,  1) ! e-
      ELSE
        JL = MCADD4GEN(IDMUM,  PMU, 1) ! mu-
        JL = MCADD4GEN(IDELEP, PE,  1) ! e+
      ENDIF

C     Boost into laboratory frame
      CALL DBOOST(PPI0,MP0,PE, PE)
      CALL DBOOST(PPI0,MP0,PMU,PMU)

C     Register particles for tracking
      IF (MODE.EQ.1) THEN
        JL = MCADD4(IDMUP,  PMU) ! mu+
        JL = MCADD4(IDELEM, PE)  ! e-
      ELSE
        JL = MCADD4(IDMUM,  PMU) ! mu-
        JL = MCADD4(IDELEP, PE)  ! e+
      ENDIF

      RETURN
      END

c-------------------------------------------------

      SUBROUTINE PI0GGG(PPI0)

c ... C-violating pi0-->gamma gamma gamma decay (uniform differential decay rate)

#include "common_blocks.f"
#include "masses.f"

      INTEGER JG1, JG2, JG3, I, IOK
      REAL*8 PPI0(4), PG1(4), PG2(4), PG3(4), TOPWT, WRAND, RANF
      PARAMETER (TOPWT=0.40)

C --- GENERATE EVENT UNIFORMLY IN PHASE SPACE (CERNLIB GENBOD)
      KGENEV   = 1
      NP       = 3
      TECM     = MP0
      AMASS(1) = 0
      AMASS(2) = 0
      AMASS(3) = 0

 1    CONTINUE
      CALL GENBOD_FIX(iok)
      if (iok.eq.0) goto 1

      WRAND = RANF()
      IF (WRAND.GT.(WT/TOPWT)) GOTO 1

C --- FILL STANDARD ARRAYS
      DO I = 1,4
         PG1(I)  = PCM(I,1)
         PG2(I)  = PCM(I,2)
         PG3(I)  = PCM(I,3)
      ENDDO

C     Save original particles in pi0 rest frame into GeneParts
      JG1 = MCADD4GEN(IDGAM, PG1, 1)
      JG2 = MCADD4GEN(IDGAM, PG2, 1)
      JG3 = MCADD4GEN(IDGAM, PG3, 1)

C     Boost into laboratory frame
      CALL DBOOST(PPI0, MP0, PG1, PG1)
      CALL DBOOST(PPI0, MP0, PG2, PG2)
      CALL DBOOST(PPI0, MP0, PG3, PG3)

C     Register particles for tracking
      JG1 = MCADD4(IDGAM, PG1)
      JG2 = MCADD4(IDGAM, PG2)
      JG3 = MCADD4(IDGAM, PG3)

      RETURN
      END

c-------------------------------------------------

      SUBROUTINE PI0GGGG(PPI0)

c ... pi0-->4 gamma decay (uniform differential decay rate)

#include "common_blocks.f"
#include "masses.f"

      INTEGER JG1, JG2, JG3, JG4, I, IOK
      REAL*8 PPI0(4), PG1(4), PG2(4), PG3(4), PG4(4), TOPWT, WRAND, RANF
      PARAMETER (TOPWT=0.10)

C --- GENERATE EVENT UNIFORMLY IN PHASE SPACE (CERNLIB GENBOD)
      KGENEV   = 1
      NP       = 4
      TECM     = MP0
      AMASS(1) = 0
      AMASS(2) = 0
      AMASS(3) = 0
      AMASS(4) = 0

 1    CONTINUE
      CALL GENBOD_FIX(iok)
      if (iok.eq.0) goto 1

      WRAND = RANF()
      IF (WRAND.GT.(WT/TOPWT)) GOTO 1

C --- FILL STANDARD ARRAYS
      DO I = 1,4
         PG1(I)  = PCM(I,1)
         PG2(I)  = PCM(I,2)
         PG3(I)  = PCM(I,3)
         PG4(I)  = PCM(I,4)
      ENDDO

C     Save original particles in pi0 rest frame into GeneParts
      JG1 = MCADD4GEN(IDGAM, PG1, 1)
      JG2 = MCADD4GEN(IDGAM, PG2, 1)
      JG3 = MCADD4GEN(IDGAM, PG3, 1)
      JG4 = MCADD4GEN(IDGAM, PG4, 1)

C     Boost into laboratory frame
      CALL DBOOST(PPI0, MP0, PG1, PG1)
      CALL DBOOST(PPI0, MP0, PG2, PG2)
      CALL DBOOST(PPI0, MP0, PG3, PG3)
      CALL DBOOST(PPI0, MP0, PG4, PG4)

C     Register particles for tracking
      JG1 = MCADD4(IDGAM, PG1)
      JG2 = MCADD4(IDGAM, PG2)
      JG3 = MCADD4(IDGAM, PG3)
      JG4 = MCADD4(IDGAM, PG4)

      RETURN
      END

C====================================================================

      SUBROUTINE PI0GNUNU(PPI0)
C     Forced pi0 --> gamma nu nu decay, SM spectrum, Arnellos et al., NPB196 (1982) 365
C     EG, 14 Nov 2016
#include "common_blocks.f"
#include "masses.f"
      INTEGER i, j
      REAL*8  PPI0(4), VEC(3), PG(4), x, WRAND, RANF

C     Generate the SM photon spectrum
 1    CONTINUE
      x     = RANF()
      WRAND = 0.106*RANF()
      IF (WRAND.GT.(x**3-x**4)) GOTO 1
      PG(4) = 0.5*x*MP0 ! photon energy
      CALL GENSPH(VEC)
      DO i = 1, 3
        PG(i) = PG(4) * VEC(i)
      ENDDO

C     Save original particles in pi0 rest frame into GeneParts
      J = MCADD4GEN(IDGAM, PG, 1)
C     Boost into laboratory frame
      CALL DBOOST(PPI0,MP0,PG,PG)
C     Register for tracking
      J = MCADD4(IDGAM, PG)

      RETURN
      END
