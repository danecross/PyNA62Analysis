      SUBROUTINE KCH2PIPI0G (JKAON, TYPE, pi0decay, twoPhotMaxAngle,
     $                       NTrials)
C======================================================
C GENERATE THE DECAY K+- -> PI+- PI0 GAMMA ACCORDING TO
C    J.D.GOOD, PHYS. REV. 113,  352 (1959)
C    N.CHRIST, PHYS. REV. 159, 1292 (1967)
C    M.MCGUIGAN & A.SANDA, PHYS. REV. D36, 1413 (1987)
C
C TYPE: 1=IB, 2=DE, 3=INT term
C They correspond to NA62MC decay modes 80, 81 and 82.
C
C E. GOUDZOVSKI, MARCH 2002
C======================================================

#include "common_blocks.f"
#include "masses.f"

      REAL*8 WTMAX 
      PARAMETER(WTMAX=0.4)

      INTEGER iok, JPIC, JPI0, JGAMMA, i, j
      INTEGER NTrials
      integer type, pi0decay, pzmode
      REAL*8 PPIC(4), PPI0(4), PGAMMA(4)
      REAL*8 WCOMP, LEN12, LEN22, SCALAR, COSTHE, COSTHE2, SINTHE2
      REAL*8 W2, PROBIB, WEI, RANF
      REAL*8 twoPhotMaxAngle, p4kaon(4)
      REAL*8 scalarProd, PHOTILEN, PHOTJLEN, photonsAngle
      logical GOODCOMBINATION

C --- GENERATE EVENT UNIFORMLY IN PHASE SPACE USING CERNLIB GENBOD
      KGENEV  = 1
      NP      = 3
      AMASS(1)= MPI
      AMASS(2)= MP0
      AMASS(3)= 0.0
      TECM    = MKCH

      NTrials = 0
c ... save the kaon momentum as it's overwritten and biasing cannot work
c ... otherwise
      do j = 1, 4
         p4kaon(j) = p4ini(j,jkaon)
      enddo

 1    CONTINUE
      CALL GENBOD_FIX(iok)
      if (iok.eq.0) goto 1

c ... Infrared cut-off for the IB term
      IF (type.eq.1.and.pcm(4,3).lt.0.010) GOTO 1

      WCOMP = RANF()
      IF (WCOMP.GT.(WT/WTMAX)) GOTO 1
      do j = 1, 4
         ppic(j)   = pcm(j,1)
         ppi0(j)   = pcm(j,2)
         pgamma(j) = pcm(j,3)
      enddo

C --- COMPUTE THE DIFFERENTIAL DECAY RATE
      LEN12   = ppic(1)**2   + ppic(2)**2   + ppic(3)**2
      LEN22   = pgamma(1)**2 + pgamma(2)**2 + pgamma(3)**2
      SCALAR  = ppic(1)*pgamma(1)+ppic(2)*pgamma(2)+ppic(3)*pgamma(3)
      COSTHE  = SCALAR/sqrt(LEN12*LEN22)
      COSTHE2 = SCALAR**2/(LEN12*LEN22)
      SINTHE2 = 1.0 - COSTHE2
      PROBIB  = LEN12 * SINTHE2 / ((0.5*MKCH - ppi0(4)
     >                            -(MPI**2-MP0**2)/(2*MKCH))**2)

      if (type.eq.1) then ! IB term with an infrared cutoff
         WEI = PROBIB / 5000.
      endif
      if (type.eq.2) then ! DE term
         W2  = (pgamma(4)**2)*(ppic(4)-sqrt(LEN12)*COSTHE)/MKCH/SQMPI
         WEI = PROBIB * W2**2 / 0.5
      endif
      if (type.eq.3) then ! INT term
         W2  = (pgamma(4)**2)*(ppic(4)-sqrt(LEN12)*COSTHE)/MKCH/SQMPI
         WEI = PROBIB * W2 / 5.5
      endif

      WCOMP = RANF()
      IF (WEI.LT.WCOMP) GOTO 1

C --- FILL MC PARTICLE LIST
      JPIC   = MCADD4GEN(IDPIP, PPIC,   0)
      JPI0   = MCADD4GEN(IDPIZ, PPI0,   0)
      JGAMMA = MCADD4GEN(IDGAM, PGAMMA, 0)

C --- BOOST PARTICLES INTO LAB FRAME
      CALL DBOOST (p4kaon, MKCH, ppic, ppic)
      CALL DBOOST (p4kaon, MKCH, ppi0, ppi0)
      CALL DBOOST (p4kaon, MKCH, pgamma, pgamma)

C --- FILL MC PARTICLE LIST
      JPIC   = MCADD4(IDPIP, PPIC)
      JPI0   = MCADD4(IDPIZ, PPI0)
      JGAMMA = MCADD4(IDGAM, PGAMMA)

C --- FORCED PI0 DECAY
      pzmode = mod (pi0decay, 100)

      call pi0decay_manager(jpi0, ppi0, pzmode)

c ... Biasing based on the photon-photon angle
      if (twoPhotMaxAngle.GT.0) then
        NTrials = NTrials+1
        GOODCOMBINATION = .FALSE.
        DO I = NPART-2, NPART
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
        IF (.NOT.GOODCOMBINATION) then
            NPARTGEN = 0
            NPART = 0
            GOTO 1
        endif
      endif
      RETURN
      END
