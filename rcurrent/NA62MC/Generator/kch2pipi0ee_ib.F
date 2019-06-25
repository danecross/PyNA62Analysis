      SUBROUTINE KCH2PIPI0EE_IB (JKAON, PI0DECAY)
C=================================================
C GENERATE THE DECAY K+- -> PI+- PI0 e+ e-
C According to L.Cappiello, G.D'Ambrosio, O. Cata' 
C Eur. Phys. J. C72 (2012) 1872 [arXiv:1112.5184]
C DOI 10.1140/epjc/s10052-012-1872-x
C
C Last revised:   M.Raggi,       26/07/2012
C NA62MC version: E.Goudzovski,  27/03/2013
C==================================================

#include "common_blocks.f"
#include "masses.f"

      INTEGER J, J1, J2, J3, J4, IOK, PI0DECAY, PZMODE
      REAL*8 PPIC(4), PPI0(4), PEP(4), PEM(4)
      REAL*8 K1,Q2,WEI,WCOMP,RANF
      REAL*8 DOT4_ppee, vdot_ppee

      REAL*8 FIB(3),SPI,T(3,3)
      REAL*8 P1(4),P2(4),QG(4),QP(4),PK(4)

C --- GENERATE EVENT UNIFORMLY IN PHASE SPACE USING CERNLIB GENBOD
      KGENEV   = 1
      NP       = 4
      AMASS(1) = MPI
      AMASS(2) = MP0
      AMASS(3) = MEL
      AMASS(4) = MEL
      TECM     = MKCH
      PK(1)    = 0.0
      PK(2)    = 0.0
      PK(3)    = 0.0
      PK(4)    = MKCH

 1    CONTINUE
      CALL GENBOD_FIX(iok)
      if (iok.eq.0) go to 1

      do j = 1, 4
         ppic(j) = pcm(j,1)
         ppi0(j) = pcm(j,2)
         pep(j)  = pcm(j,3)
         pem(j)  = pcm(j,4)

c ...    D'Ambrosio paper variables
         QG(j) = pcm(j,3)-pcm(j,4) !Q in paper
         QP(j) = pcm(j,3)+pcm(j,4) !q in paper
         P1(j) = pcm(j,1)
         P2(j) = pcm(j,2)
      enddo

C     IB MATRIX ELEMENT FROM FORMULA 18 G. D'Ambrosio with F1 F2 coming from 21
      Q2  = 2*MEL**2 + 2*(pep(4)*pem(4)-vdot_ppee(pep,pem))
      K1  = 2/Q2**2            !!removed EQ**2
      SPI = MPI**2+Mp0**2 + 2*(ppic(4)*ppi0(4)-vdot_ppee(ppic,ppi0))

c     Do not use complex numbers just multiply the i in the final formula
c     Removed the terms in EQ in the F1 and F2 and in K1 removed Mp+p0
      FIB(1) = 4.0*DOT4_ppee(QP,P2) /
     >         ((2*DOT4_ppee(QP,P1)+Q2)*(2*DOT4_ppee(QP,PK)-Q2))
      FIB(2) = -2.0 / (2.0*DOT4_ppee(QP,PK)-Q2)

      T(1,1) = DOT4_ppee(QP,P1)**2 -DOT4_ppee(QG,P1)**2 - Q2*MPI**2
      T(2,2) = DOT4_ppee(QP,P2)**2 -DOT4_ppee(QG,P2)**2 - Q2*Mp0**2
      T(1,2) = DOT4_ppee(QP,P1)*DOT4_ppee(QP,P2)-
     >         DOT4_ppee(QG,P1)*DOT4_ppee(QG,P2)-
     >         Q2*DOT4_ppee(P1,P2)
         
      wei = K1 * (abs(FIB(1)**2)*T(1,1)+ abs(FIB(2)**2)*T(2,2) +
     >            2*FIB(1)*FIB(2)*T(1,2))

      WEI = WEI*WT/0.5E8
      WCOMP = RANF()
      IF (WCOMP.GT.WEI) GOTO 1

C --- Save the GeneParts
      J1 = MCADD4GEN(IDPIP,  PPIC, 0)
      J2 = MCADD4GEN(IDPIZ,  PPI0, 0)
      J3 = MCADD4GEN(IDELEP, PEP,  0)
      J4 = MCADD4GEN(IDELEM, PEM,  0)

C --- BOOST PARTICLES INTO LAB FRAME
      CALL DBOOST(p4ini(1,jkaon), MKCH, ppic, ppic)
      CALL DBOOST(p4ini(1,jkaon), MKCH, ppi0, ppi0)
      CALL DBOOST(p4ini(1,jkaon), MKCH, pep,  pep)
      CALL DBOOST(p4ini(1,jkaon), MKCH, pem,  pem)

C --- PASS DAUGHTER PARTICLES TO GEANT4 FOR TRACKING
      J1 = MCADD4(IDPIP,  PPIC)
      J2 = MCADD4(IDPIZ,  PPI0)
      J3 = MCADD4(IDELEP, PEP)
      J4 = MCADD4(IDELEM, PEM)

C --- FORCED PI0 DECAYS
      pzmode = mod (pi0decay, 100)
      call pi0decay_manager(j2, ppi0, pzmode)

      RETURN
      END

c.......................................

      FUNCTION DOT4_ppee(X,Y)
      real*8 DOT4_ppee, vdot_ppee
      real*8 X(4),Y(4)
      dot4_ppee = X(4)*Y(4) - vdot_ppee(X,Y)
      END

      FUNCTION vdot_ppee(X,Y)
      real*8 vdot_ppee
      real*8 X(3),Y(3)
      vdot_ppee = X(1)*Y(1) + X(2)*Y(2) + X(3)*Y(3)
      END
