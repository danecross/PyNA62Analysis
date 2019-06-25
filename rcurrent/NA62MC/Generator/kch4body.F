      SUBROUTINE KCH4BODY(JKAON, MODE)
C=================================================================
C A generic 3-body K+ decay with a flat phase space distribution.
C Useful for forbidden decays:
C their dynamics depends on the new physics model.
C
C Evgueni.Goudzovski@cern.ch, August 2012
C=================================================================

#include "common_blocks.f"
#include "masses.f"

      INTEGER MODE, ID1, ID2, ID3, ID4, J, iok
      REAL*8  M1, M2, M3, M4, W, TOPWT, RANF
      REAL*8  PP1(4), PP2(4), PP3(4), PP4(4)

      parameter (topwt=0.15) ! the upper limit is tuned for modes 140 and 141 only

C --- Translate the mode into particle IDs
      if (mode.lt.1.or.mode.gt.2) mode = 1
      if (mode.eq.1) then ! e- nu mu+ mu+
         id1 = idelem
         id2 = idnu
         id3 = idmup
         id4 = idmup
      endif
      if (mode.eq.2) then ! mu- nu e+ e+
         id1 = idmum
         id2 = idnu
         id3 = idelep
         id4 = idelep
      endif

C --- Assign particle masses
      if (id1.eq.idnu)                      m1 = 0.0
      if (id1.eq.idelem .or. id1.eq.idelep) m1 = MEL
      if (id1.eq.idmup  .or. id1.eq.idmum)  m1 = MMU
      if (id1.eq.idpip  .or. id1.eq.idpim)  m1 = MPI

      if (id2.eq.idnu)                      m2 = 0.0
      if (id2.eq.idelem .or. id2.eq.idelep) m2 = MEL
      if (id2.eq.idmup  .or. id2.eq.idmum)  m2 = MMU
      if (id2.eq.idpip  .or. id2.eq.idpim)  m2 = MPI

      if (id3.eq.idnu)                      m3 = 0.0
      if (id3.eq.idelem .or. id3.eq.idelep) m3 = MEL
      if (id3.eq.idmup  .or. id3.eq.idmum)  m3 = MMU
      if (id3.eq.idpip  .or. id3.eq.idpim)  m3 = MPI

      if (id4.eq.idnu)                      m4 = 0.0
      if (id4.eq.idelem .or. id4.eq.idelep) m4 = MEL
      if (id4.eq.idmup  .or. id4.eq.idmum)  m4 = MMU
      if (id4.eq.idpip  .or. id4.eq.idpim)  m4 = MPI

C --- GENERATE EVENT UNIFORMLY IN PHASE SPACE (CERNLIB GENBOD)
      KGENEV   = 1
      NP       = 4
      AMASS(1) = M1
      AMASS(2) = M2
      AMASS(3) = M3
      AMASS(4) = M4
      TECM     = MKCH

 1    CONTINUE
      CALL GENBOD_FIX(iok)
      if (iok.eq.0) goto 1

      W = topwt*RANF()
      IF (W.GT.WT) GOTO 1

      DO J = 1, 4
        PP1(j) = PCM(j,1)
        PP2(j) = PCM(j,2)
        PP3(j) = PCM(j,3)
        PP4(j) = PCM(j,4)
      ENDDO

C --- FILL MC PARTICLE LIST
      J = MCADD4GEN (ID1, PP1, 0)
      J = MCADD4GEN (ID2, PP2, 0)
      J = MCADD4GEN (ID3, PP3, 0)
      J = MCADD4GEN (ID4, PP4, 0)

C --- BOOST PARTICLES INTO LAB FRAME
      CALL DBOOST(p4ini(1,jkaon), MKCH, pp1, pp1)
      CALL DBOOST(p4ini(1,jkaon), MKCH, pp2, pp2)
      CALL DBOOST(p4ini(1,jkaon), MKCH, pp3, pp3)
      CALL DBOOST(p4ini(1,jkaon), MKCH, pp4, pp4)

C --- FILL MC PARTICLE LIST
      J = MCADD4 (ID1, PP1)
      J = MCADD4 (ID2, PP2)
      J = MCADD4 (ID3, PP3)
      J = MCADD4 (ID4, PP4)

      RETURN
      END
