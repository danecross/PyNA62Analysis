      SUBROUTINE KCH2PIGLL(JKAON,MODE)
C==================================================
C Generate decays K+- -> pi+- gamma l+ l- at O(p^6)
C Reference: F.Gabbiani, hep-ph/9812419
C
C Original matrix element code by F.Gabbiani
C NA48 version by E.Goudzovski /February 2003/
C NA62 version: EG, March 2011
C==================================================

#include "common_blocks.f"
#include "masses.f"

      INTEGER MODE              ! 1=(pi g e e), 2=(pi g mu mu)
      REAL*8 DD,LAM

      COMMON/C/PI,ALFA,E
      COMMON/C1/D
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
      COMMON/C3/AL1,AL3,CH,CS1
      REAL*8 PI,ALFA,E,D
      REAL*8 MKA,MP,MR,CC,A1,A2,LIN
      REAL*8 AL1,AL3,CH,CS1

      REAL*8 BE1,BE3,GA3,ZE1
      REAL*8 CKMS1,CKMC1,CKMC3,MLEP,GF,G8
      REAL*8 Q,Z,Y,QQ,ZZ,YY
      REAL*8 WT1,WTCOMP,WTMAX,RANF

      REAL*8 PPI(4),PG(4),PEE(4),PE1(4),PE2(4),PG_B(4),PEE_B(4)
      REAL*8 P1(4,4), P2(4,4), P3(4,4), P4(4,4)
      REAL*8 PHI, SINPHI, COSPHI, COSTHE,SINTHE,COSPSI,SINPSI
      REAL*8 M_GG, M_EE, E_E, P_E, P_EE, E_EE, P_G, E_G
      REAL*8 P2_GEE, P_GEE, E_GEE, G_GEE, B_GEE
      REAL*8 PPI_FINAL(4), PG_FINAL(4), PE1_FINAL(4), PE2_FINAL(4)
      REAL*8 VEC(3)
      INTEGER II, IE1, IE2, JPI, JG, JE1, JE2

c --- The constant $\hat{c}$ can be varied [-10;10]
      CH = 1.8

c --- Maximum weight is tuned as function of $\hat{c}$
      IF (MODE.EQ.1) THEN       ! electrons
         IF (CH.LE.0.0) WTMAX = 2.0-0.5*CH
         IF (CH.GT.0.0) WTMAX = 3.0+CH
      ELSE                      ! muons
         IF (CH.LE.0.0)               WTMAX = 0.02-0.004*CH
         IF (CH.GT.0.0.AND.CH.LE.6.0) WTMAX = 0.02
         IF (CH.GT.6.0)               WTMAX = 0.03
      ENDIF

c --- Masses
      MKA  = MKCH
      MP   = MPI
      MR   = 0.770
      MLEP = MEL
      IF (MODE.EQ.2) MLEP = MMU
      E    = MLEP*MLEP/(MKA*MKA)
      D    = MP*MP/(MKA*MKA)

c --- Chiral constants
      AL1 =  93.16E-8
      AL3 =  -6.72E-8
      CS1 =  -1.83E-8
      BE1 = -27.06E-8
      BE3 =  -2.22E-8
      GA3 =   2.95E-8
      ZE1 =  -0.40E-8
      PI  =   3.14159265358979324

c --- Coupling and CKM constants
      ALFA  = 1.0D00/137.0359895D00
      GF    = 1.16639E-5
      G8    = 5.1D00
      CKMS1 = 0.221D00
      CKMC1 = 0.9753D00
      CKMC3 = 1.0D00
      LIN   = BE1-BE3/2.0D00+DSQRT(3.0D00)*GA3
      A1    = 8.0D00*(2.0D00*ZE1-CS1)/(3.0D00*D*D)
      A2    = 8.0D00*(4.0D00*ZE1+CS1)/(3.0D00*D*D)
      CC    = GF*CKMS1*CKMC1*CKMC3*MKA*MKA*G8/DSQRT(2.0D00)

c --- Generation
 10   CONTINUE
      Z  = ((1.0D00-DSQRT(D))**2-4.0D00*E)*RANF()+4.0D00*E
      Q  = (DLOG(Z)-DLOG(4.0D00*E))*RANF()+DLOG(4.0D00*E)
      Y  = DSQRT(LAM(1.0D00,Z,D))*(Z-DEXP(Q))*RANF()/Z+
     >     (-DSQRT(LAM(1.0D00,Z,D))*(Z-DEXP(Q))+
     >     DEXP(Q)*(Z+1.0D00-D))/(2.0D00*Z)

      WT1    = DD(Y,Z,Q) * 1E24
      WTCOMP = RANF()
      IF (WTCOMP.GT.(WT1/WTMAX)) GOTO 10

**********************************************************
*    CALCULATE MOMENTA, KNOWING THE KINEMATIC VARIABLES  *
*    THEN ROTATE KAON REST FRAME IN RANDOM DIRECTION     *
*                                           EG 20/02/03  *
**********************************************************

      QQ   = DEXP(Q)
      ZZ   = Z
      YY   = Y
      M_EE = MKCH*DSQRT(QQ)
      M_GG = MKCH*DSQRT(ZZ)
      E_E  = 0.5*M_EE

c --- (EE)/G --> in (GEE) frame
      P_EE = 0.5*(MKCH*DSQRT(ZZ) - M_EE*M_EE/MKCH/DSQRT(ZZ))
      E_EE   = DSQRT(P_EE**2+M_EE**2)
      P_G    = P_EE
      E_G    = P_G

c --- (GEE) --> in K frame
      P2_GEE = 0.25*MKCH*MKCH - 0.5*M_GG*M_GG - 0.5*MP*MP +
     >         0.25*(M_GG*M_GG-MP*MP)**2/MKCH/MKCH
      P_GEE  = DSQRT(P2_GEE)
      E_GEE  = DSQRT(P2_GEE+M_GG**2)
      G_GEE  = E_GEE/M_GG
      B_GEE  = P_GEE/E_GEE

c --- (EE) flight angle in (GEE) frame
      COSPHI = 0.5/B_GEE/P_EE*(YY*MKCH/G_GEE+E_G-E_EE)
      SINPHI = SQRT(1.-COSPHI**2)

c --- Momenta
      PPI(1) = -P_GEE
      PPI(2) = 0.
      PPI(3) = 0.
      PPI(4) = SQRT(P2_GEE+MP**2)
      PEE(1) = P_EE*COSPHI
      PEE(2) = P_EE*SINPHI
      PEE(3) = 0.
      PEE(4) = E_EE
      PG(1)  = -PEE(1)
      PG(2)  = -PEE(2)
      PG(3)  = 0.
      PG(4)  = E_G

c --- Boost G and (EE) from K frame into (GEE) frame
      PEE_B(1) = G_GEE*B_GEE*PEE(4) + G_GEE*PEE(1)
      PEE_B(2) = PEE(2)
      PEE_B(3) = PEE(3)
      PEE_B(4) = G_GEE*PEE(4) + G_GEE*B_GEE*PEE(1)
      PG_B(1)  = G_GEE*B_GEE*PG(4) + G_GEE*PG(1)
      PG_B(2)  = PG(2)
      PG_B(3)  = PG(3)
      PG_B(4)  = G_GEE*PG(4) + G_GEE*B_GEE*PG(1)

c --- Simulate the decay G* --> E+E-
      CALL GENSPH(VEC)
      PE1(4) = E_E
      PE2(4) = E_E
      P_E    = SQRT(E_E**2-MLEP**2)
      DO II = 1,3
         PE1(II) =  P_E * VEC(II)
         PE2(II) = -PE1(II)
      ENDDO
      CALL DBOOST(PEE_B,M_EE,PE1,PE1)
      CALL DBOOST(PEE_B,M_EE,PE2,PE2)

c --- Momenta are simulated ==> perform standartisation
      DO II = 1, 4
         P1(1, II) = PPI(II)
         P1(2, II) = PG_B(II)
         P1(3, II) = PE1(II)
         P1(4, II) = PE2(II)
      ENDDO

c --- For rotation of momenta into random direction, let us use
c --- Euler angles: rotations around z, unrotated x, unrotated z

c --- a) Counterclockwise rotation around Z axis (PHI)
      PHI    = RANF()*2.0*PI
      SINPHI = DSIN(PHI)
      COSPHI = DCOS(PHI)
      DO II = 1, 4
         P2(II,1) =  P1(II,1)*COSPHI + P1(II,2)*SINPHI
         P2(II,2) = -P1(II,1)*SINPHI + P1(II,2)*COSPHI
         P2(II,3) =  P1(II,3)
         P2(II,4) =  P1(II,4)
      ENDDO

c --- b) Generate uniformly the new direction of Z axis,
c ---    define the corresponding Euler angles THETA, PSI
      CALL GENSPH(VEC)
      COSTHE = VEC(3)/SQRT(VEC(1)**2+VEC(2)**2+VEC(3)**2)
      SINTHE = SQRT((VEC(1)**2+VEC(2)**2)/
     >              (VEC(1)**2+VEC(2)**2+VEC(3)**2))
      COSPSI = VEC(2)/SQRT(VEC(1)**2+VEC(2)**2)
      SINPSI = VEC(1)/SQRT(VEC(1)**2+VEC(2)**2)

c --- c) Clockwise rotation around X axis (THETA)
      DO II = 1, 4
         P3(II,1) = P2(II,1)
         P3(II,2) = P2(II,2)*COSTHE - P2(II,3)*SINTHE
         P3(II,3) = P2(II,2)*SINTHE + P2(II,3)*COSTHE
         P3(II,4) = P2(II,4)
      ENDDO

c --- d) Counterclockwise rotation around Z axis (PSI)
      DO II = 1, 4
         P4(II,1) = P3(II,1)*COSPSI - P3(II,2)*SINPSI
         P4(II,2) = P3(II,1)*SINPSI + P3(II,2)*COSPSI
         P4(II,3) = P3(II,3)
         P4(II,4) = P3(II,4)
      ENDDO

**********************************************************
*    BOOST PARTICLES INTO LAB FRAME, FILL MC LIST
**********************************************************

      DO II  = 1, 4
         PPI_FINAL(II) = P4(1,II)
         PG_FINAL (II) = P4(2,II)
         PE1_FINAL(II) = P4(3,II)
         PE2_FINAL(II) = P4(4,II)
      ENDDO

C     --- Particle codes
      IF (MODE.EQ.1) THEN
         IE1 = IDELEM
         IE2 = IDELEP
      ELSE
         IE1 = IDMUP
         IE2 = IDMUM
      ENDIF

C --- Fill MC particle list
      JPI = MCADD4GEN (IDPIP, PPI_FINAL, 0)
      JG  = MCADD4GEN (IDGAM, PG_FINAL,  0)
      JE1 = MCADD4GEN (IE1,   PE1_FINAL, 0)
      JE2 = MCADD4GEN (IE2,   PE2_FINAL, 0)

C --- Boost into lab frame
      CALL DBOOST (p4ini(1,jkaon), MKCH, PPI_FINAL, PPI_FINAL)
      CALL DBOOST (p4ini(1,jkaon), MKCH, PG_FINAL,  PG_FINAL)
      CALL DBOOST (p4ini(1,jkaon), MKCH, PE1_FINAL, PE1_FINAL)
      CALL DBOOST (p4ini(1,jkaon), MKCH, PE2_FINAL, PE2_FINAL)

C --- Fill MC particle list
      JPI = MCADD4 (IDPIP, PPI_FINAL)
      JG  = MCADD4 (IDGAM, PG_FINAL)
      JE1 = MCADD4 (IE1,   PE1_FINAL)
      JE2 = MCADD4 (IE2,   PE2_FINAL)

      RETURN
      END

C    ==============================================
C    =  Here starts matrix element computation... =
C    ==============================================

      REAL*8 FUNCTION RF(Z)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C/PI,ALFA,E
      RF=0.0D00
      IF(Z.EQ.0.0D00)THEN
         RF=0.0D00
      ELSEIF(Z.LE.4.0D00)THEN
         RF=1.0D00-4.0D00*(DASIN(DSQRT(Z)/2.0D00))**2/Z
      ELSEIF(Z.GT.4.0D00)THEN
         RF=1.0D00+(DLOG((1.0D00-DSQRT(1.0D00-4.0D00/Z))/
     1        (1.0D00+DSQRT(1.0D00-4.0D00/Z))))**2/Z-PI*PI/Z
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION RR(Z)
      IMPLICIT REAL*8(A-H,L-Z)
      RR=0.0D00
      IF(Z.EQ.0.0D00)THEN
         RR=0.0D00
      ELSEIF(Z.LE.4.0D00)THEN
         RR=-1.0D00/6.0D00+2.0D00/Z-2.0D00*DSQRT(4.0D00/Z-1.0D00)
     1        *DASIN(DSQRT(Z)/2.0D00)/Z
      ELSEIF(Z.GT.4.0D00)THEN
         RR=-1.0D00/6.0D00+2.0D00/Z+DSQRT(1.0D00-4.0D00/Z)*
     1        DLOG((1.0D00-DSQRT(1.0D00-4.0D00/Z))/
     2        (1.0D00+DSQRT(1.0D00-4.0D00/Z)))/Z
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION XIF(Z)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C/PI,ALFA,E
      XIF=0.0D00
      IF(Z.LE.4.0D00)THEN
         XIF=0.0D00
      ELSEIF(Z.GT.4.0D00)THEN
         XIF=2.0D00*DLOG((1.0D00-DSQRT(1.0D00-4.0D00/Z))/
     1   (1.0D00+DSQRT(1.0D00-4.0D00/Z)))*PI/Z
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION XIR(Z)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C/PI,ALFA,E
      XIR=0.0D00
      IF(Z.LE.4.0D00)THEN
         XIR=0.0D00
      ELSEIF(Z.GT.4.0D00)THEN
         XIR=DSQRT(1.0D00-4.0D00/Z)*PI/Z
      ENDIF
      RETURN
      END

C     Part with linear terms,
C     multiplied by (Z-Q)**2 to stabilize it
      REAL*8 FUNCTION RA(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
      COMMON/C3/AL1,AL3,CH,CS1
      RFF=(Z-Q)+Z*(RF(Z/D)-1.0D00)-Q*(RF(Q/D)-1.0D00)+
     1     Q*(RR(Z/D)+1.0D00/6.0D00)*(Z/D)-
     2     Q*(RR(Q/D)+1.0D00/6.0D00)*(Q/D)
      RA=CC*((Z+D-1.0D00)*
     1     ((Z-Q)+Z*(RF(Z)-1.0D00)-Q*(RF(Q)-1.0D00)+
     2     Q*(RR(Z)+1.0D00/6.0D00)*Z-
     3     Q*(RR(Q)+1.0D00/6.0D00)*Q)-CH*(Z-Q)*(Z-Q))+
     4     (2.0D00*(2.0D00*AL1-AL3)+
     5     (1.0D00+1.0D00/(3.0D00*D)-Z/D)*LIN-
     6     A1*(1.0D00+6.0D00*(D-Z)+9.0D00*(D-Z)**2)/18.0D00)*RFF-
     7     A2*(-RLL(Y,Z,Q)+2.0D00*RN(Y,Z,Q)+
     8     ((1.0D00-3.0D00*D)/9.0D00+D*(1.0D00+3.0D00*D)*
     9     (1.0D00+1.0D00/(3.0D00*D)-Z/D)/12.0D00)*RFF)+
     1     (Z-Q)**2*RDIVA(Z,Q)
      RETURN
      END

C     Part with linear terms,
C     multiplied by (Z-Q)**2 to stabilize it
      REAL*8 FUNCTION XIA(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
      COMMON/C3/AL1,AL3,CH,CS1
      XIFF=Z*XIF(Z/D)-Q*XIF(Q/D)+
     1     Q*XIR(Z/D)*(Z/D)-Q*XIR(Q/D)*(Q/D)
      XIA=(2.0D00*(2.0D00*AL1-AL3)+
     1     (1.0D00+1.0D00/(3.0D00*D)-Z/D)*LIN-
     2     A1*(1.0D00+6.0D00*(D-Z)+9.0D00*(D-Z)**2)/18.0D00)*XIFF-
     3     A2*(-XILL(Y,Z,Q)+2.0D00*XIN(Y,Z,Q)+
     4     ((1.0D00-3.0D00*D)/9.0D00+D*(1.0D00+3.0D00*D)*
     5     (1.0D00+1.0D00/(3.0D00*D)-Z/D)/12.0D00)*XIFF)+
     6     (Z-Q)**2*XIDIVA(Z,Q)
      RETURN
      END

      REAL*8 FUNCTION LAM(X,Y,Z)
      REAL*8 X,Y,Z
C     Absolute value to avoid trouble
      LAM=DABS(X*X+Y*Y+Z*Z-2.0D00*(X*Y+Y*Z+Z*X))
      RETURN
      END

      REAL*8 FUNCTION DD(Y,Z,Q)
C     DD is dimensional, dimension of mass
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C/PI,ALFA,E
      COMMON/C1/D
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
C     Remember: all the functions in A are multiplied by (Z-Q)^2
C     Sum of squares of real and imaginary parts
      DD=ALFA*ALFA*ALFA*MKA*PS(Y,Z,DEXP(Q))*
     1     ((1.0D00-DSQRT(D))**2-4.0D00*E)* !Jacobian for z
     2     (DLOG(Z)-DLOG(4.0D00*E))* !Jacobian for q
     3     DSQRT(LAM(1.0D00,Z,D))*(Z-DEXP(Q))/Z* !Jacobian for y
     4     DSQRT(1.0D00-4.0D00*E/DEXP(Q))* !finite mass corrections
     5     (1.0D00+2.0D00*E/EXP(Q))/ !finite mass corrections
     6     (2.0D00**11*PI**6)   !denominator coefficient

      RETURN
      END

      REAL*8 FUNCTION RXYL(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      RXYL=(-6.0D00*D+Q)*(Z-Q)/24.0D00+(Z-Q)*(Z-Q)*
     1     (-13.0D00/144.0D00-D/(6.0D00*Z)+(8.0D00*D*D+2.0D00*D*Z-Z*Z)*
     2     RART(Z/D)/(12.0D00*(4.0D00*D-Z)*Z*DSQRT(Z)))+(D*D/2.0D00)*
     3     (RF1(Q/D)-RF1(Z/D))-(Q*Q/2.0D00)*(-D/(3.0D00*Z)+
     4     (8.0D00*D*D+2.0D00*D*Z-Z*Z)*RART(Z/D)/
     5     (6.0D00*(4.0D00*D-Z)*Z*DSQRT(Z))+D/(3.0D00*Q)-
     6     (8.0D00*D*D+2.0D00*D*Q-Q*Q)*RART(Q/D)/
     7     (6.0D00*(4.0D00*D-Q)*Q*DSQRT(Q)))+
     8     D*Q*(RART(Z/D)/DSQRT(Z)-RART(Q/D)/DSQRT(Q))
      RETURN
      END

      REAL*8 FUNCTION XIXYL(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      XIXYL=
     1     (Z-Q)*(Z-Q)*((8.0D00*D*D+2.0D00*D*Z-Z*Z)*
     2     XIART(Z/D)/(12.0D00*(4.0D00*D-Z)*Z*DSQRT(Z)))+
     3     (D*D/2.0D00)*(XIF1(Q/D)-XIF1(Z/D))-
     4     (Q*Q/2.0D00)*((8.0D00*D*D+2.0D00*D*Z-Z*Z)*XIART(Z/D)/
     5     (6.0D00*(4.0D00*D-Z)*Z*DSQRT(Z))-
     6     (8.0D00*D*D+2.0D00*D*Q-Q*Q)*XIART(Q/D)/
     7     (6.0D00*(4.0D00*D-Q)*Q*DSQRT(Q)))+
     8     D*Q*(XIART(Z/D)/DSQRT(Z)-XIART(Q/D)/DSQRT(Q))
      RETURN
      END

      REAL*8 FUNCTION RL(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      RL=-3.0D00/2.0D00-(D*RF1(Z/D)-D*RF1(Q/D)+
     1     RART(Q/D)*DSQRT(Q)-RART(Z/D)*DSQRT(Z))/(Z-Q)
      RETURN
      END

      REAL*8 FUNCTION XIL(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      XIL=-(D*XIF1(Z/D)-D*XIF1(Q/D)+
     1     XIART(Q/D)*DSQRT(Q)-XIART(Z/D)*DSQRT(Z))/(Z-Q)
      RETURN
      END

      REAL*8 FUNCTION RXL(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      RXL=(Z-Q)*(-4.0D00*Z/9.0D00-(4.0D00*D-Z)*RART(Z/D)/(3.0D00*
     1     DSQRT(Z))+4.0D00*Q/9.0D00+(4.0D00*D-Q)*RART(Q/D)/(3.0D00*
     2     DSQRT(Q)))
      RETURN
      END

      REAL*8 FUNCTION XIXL(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      XIXL=-(Z-Q)*((4.0D00*D-Z)*XIART(Z/D)/(3.0D00*
     1     DSQRT(Z))-(4.0D00*D-Q)*XIART(Q/D)/(3.0D00*
     2     DSQRT(Q)))
      RETURN
      END

      REAL*8 FUNCTION RX2L(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      RX2L=RXL(Z,Q)/2.0D00
      RETURN
      END

      REAL*8 FUNCTION XIX2L(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      XIX2L=XIXL(Z,Q)/2.0D00
      RETURN
      END

      REAL*8 FUNCTION RYL(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      RYL=(Z-Q)*(3.D00*D+Q)/6.0D00+(Z-Q)*(Z-Q)*
     1     (-4.0D00/9.0D00+D/(3.0D00*Z)+
     2     (-4.0D00*D*D+5.0D00*D*Z-Z*Z)*RART(Z/D)/(3.0D00*(4.0D00*D-Z)*
     3     Z*DSQRT(Z)))-(D*D/2.0D00)*(2.0D00*RART(Z/D)*DSQRT(Z)/D-Z/D-
     4     2.0D00*RART(Q/D)*DSQRT(Q)/D+Q/D)-
     5     (Q*Q/3.0D00)*(D/Z+(-4.0D00*D*D+5.0D00*D*Z-Z*Z)*
     6     RART(Z/D)/((4.0D00*D-Z)*Z*DSQRT(Z))-D/Q-(-4.0D00*D*D+
     7     5.0D00*D*Q-Q*Q)*RART(Q/D)/((4.0D00*D-Q)*Q*DSQRT(Q)))+
     8     D*Q*(RF1(Z/D)-RF1(Q/D))-
     9     2.0D00*D*Q*(RART(Z/D)/DSQRT(Z)-
     1     RART(Q/D)/DSQRT(Q))
      RETURN
      END

      REAL*8 FUNCTION XIYL(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      XIYL=(Z-Q)*(Z-Q)*
     1     (-4.0D00*D*D+5.0D00*D*Z-Z*Z)*XIART(Z/D)/(3.0D00*(4.0D00*D-Z)*
     2     Z*DSQRT(Z))-(D*D/2.0D00)*(2.0D00*XIART(Z/D)*DSQRT(Z)/D-
     3     2.0D00*XIART(Q/D)*DSQRT(Q)/D)-
     4     (Q*Q/3.0D00)*((-4.0D00*D*D+5.0D00*D*Z-Z*Z)*
     5     XIART(Z/D)/((4.0D00*D-Z)*Z*DSQRT(Z))-(-4.0D00*D*D+
     6     5.0D00*D*Q-Q*Q)*XIART(Q/D)/((4.0D00*D-Q)*Q*DSQRT(Q)))+
     7     D*Q*(XIF1(Z/D)-XIF1(Q/D))-
     8     2.0D00*D*Q*(XIART(Z/D)/DSQRT(Z)-
     9     XIART(Q/D)/DSQRT(Q))
      RETURN
      END

      REAL*8 FUNCTION RY2L(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      IF(Q.EQ.Z)THEN
         RY2L=0.0D00
      ELSE
         RY2L=(Z-Q)*(Q/24.0D00+D/4.0D00)+(D*D-3.0D00*D*Q-Q*Q/4)/3.0D00+
     1       (Z-Q)*(Z-Q)*(-2.0D00/9.0D00+D/(3.0D00*Z)-
     2       (8.0D00*D*D-6.0D00*D*Z+Z*Z)*RART(Z/D)/(6.0D00*(4.0D00*D-Z)*
     3        Z*DSQRT(Z)))
     4        -D*D/3.0D00+D*(Z+Q)/12.0D00-(D*D*D/(3.0D00*(Z-Q)))*
     5        (RART(Z/D)*Z*DSQRT(Z)/(D*D)-RART(Q/D)*Q*DSQRT(Q)/
     6        (D*D))-Q*Q*D/(3.0D00*Z)+(Q*Q*Q/(3.0D00*(Z-Q)))*
     7        ((-8.0D00*D*D+6.0D00*D*Z-Z*Z)*RART(Z/D)/
     8        (2.0D00*(4.0D00*D-Z)*Z*DSQRT(Z))-
     9      (-8.0D00*D*D+6.0D00*D*Q-Q*Q)*RART(Q/D)/(2.0D00*(4.0D00*D-Q)*
     1        Q*DSQRT(Q)))-D*Q+(D*D*Q/(Z-Q))*
     2        (2.0D00*RART(Z/D)*DSQRT(Z)/D-
     3        2.0D00*RART(Q/D)*DSQRT(Q)/D-RF1(Z/D)+RF1(Q/D))-
     4        (D*Q*Q/(Z-Q))*(RF1(Z/D)-3.0D00*RART(Z/D)/DSQRT(Z)-
     5        RF1(Q/D)+3.0D00*RART(Q/D)/DSQRT(Q))
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION XIY2L(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      IF(Q.EQ.Z)THEN
         XIY2L=0.0D00
      ELSE
         XIY2L=
     1        (Z-Q)*(Z-Q)*(
     2     -(8.0D00*D*D-6.0D00*D*Z+Z*Z)*XIART(Z/D)/(6.0D00*(4.0D00*D-Z)*
     3        Z*DSQRT(Z)))-(D*D*D/(3.0D00*(Z-Q)))*
     4        (XIART(Z/D)*Z*DSQRT(Z)/(D*D)-XIART(Q/D)*Q*DSQRT(Q)/
     5        (D*D))+(Q*Q*Q/(3.0D00*(Z-Q)))*
     6        ((-8.0D00*D*D+6.0D00*D*Z-Z*Z)*XIART(Z/D)/
     7        (2.0D00*(4.0D00*D-Z)*Z*DSQRT(Z))-
     8     (-8.0D00*D*D+6.0D00*D*Q-Q*Q)*XIART(Q/D)/(2.0D00*(4.0D00*D-Q)*
     9        Q*DSQRT(Q)))+(D*D*Q/(Z-Q))*
     1        (2.0D00*XIART(Z/D)*DSQRT(Z)/D-
     2        2.0D00*XIART(Q/D)*DSQRT(Q)/D-XIF1(Z/D)+XIF1(Q/D))-
     3        (D*Q*Q/(Z-Q))*(XIF1(Z/D)-3.0D00*XIART(Z/D)/DSQRT(Z)-
     4        XIF1(Q/D)+3.0D00*XIART(Q/D)/DSQRT(Q))
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION PK1(Y,Z)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      PK1=(Y+(Z+1.0D00-D)/2.0D00)/2.0D00
      RETURN
      END

      REAL*8 FUNCTION PK2(Y,Z)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      PK2=(-Y+(Z+1.0D00-D)/2.0D00)/2.0D00
      RETURN
      END

      REAL*8 FUNCTION RLL(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      RLL=2.0D00*(1.0D00-2.0D00*(PK1(Y,Z)+PK2(Y,Z)))*RXYL(Z,Q)+
     1     PK1(Y,Z)*RYL(Z,Q)+
     2     PK2(Y,Z)*(2.0D00*RY2L(Z,Q)-RYL(Z,Q)+RXL(Z,Q))
      RETURN
      END

      REAL*8 FUNCTION XILL(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      XILL=2.0D00*(1.0D00-2.0D00*(PK1(Y,Z)+PK2(Y,Z)))*XIXYL(Z,Q)+
     1     PK1(Y,Z)*XIYL(Z,Q)+
     2     PK2(Y,Z)*(2.0D00*XIY2L(Z,Q)-XIYL(Z,Q)+XIXL(Z,Q))
      RETURN
      END

      REAL*8 FUNCTION RXY(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      RXY=-(Z-Q)/2.0D00-D*(RF1(Z/D)-RF1(Q/D))+
     1     Q*(RART(Z/D)/DSQRT(Z)-RART(Q/D)/DSQRT(Q))
      RETURN
      END

      REAL*8 FUNCTION XIXY(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      XIXY=-D*(XIF1(Z/D)-XIF1(Q/D))+
     1     Q*(XIART(Z/D)/DSQRT(Z)-XIART(Q/D)/DSQRT(Q))
      RETURN
      END

      REAL*8 FUNCTION RXY2(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      IF(Q.EQ.Z)THEN
         RXY2=0.0D00
      ELSE
         RXY2=-(Z-Q)/6.0D00+(Q/3.0D00+D)+D-
     1        (2.0D00*D/(Z-Q))*(RART(Z/D)*DSQRT(Z)-
     2        RART(Q/D)*DSQRT(Q))+2.0D00*Q*D/(3.0D00*Z)-
     3        (2.0D00*Q*Q/(3.0D00*(Z-Q)))*
     4        ((-4.0D00*D*D+5.0D00*D*Z-Z*Z)*RART(Z/D)/((4.0D00*D-Z)*Z*
     5        DSQRT(Z))-(-4.0D00*D*D+5.0D00*D*Q-Q*Q)*RART(Q/D)/
     6        ((4.0D00*D-Q)*Q*DSQRT(Q)))+(2.0D00*Q*D/(Z-Q))*
     7        (RF1(Z/D)-RF1(Q/D))-(4.0D00*Q*D/(Z-Q))*
     8        RART(Z/D)/DSQRT(Z)+(4.0D00*DSQRT(Q)*D/(Z-Q))*RART(Q/D)
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION XIXY2(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      IF(Q.EQ.Z)THEN
         XIXY2=0.0D00
      ELSE
         XIXY2=
     1        -(2.0D00*D/(Z-Q))*(XIART(Z/D)*DSQRT(Z)-
     2        XIART(Q/D)*DSQRT(Q))-
     3        (2.0D00*Q*Q/(3.0D00*(Z-Q)))*
     4        ((-4.0D00*D*D+5.0D00*D*Z-Z*Z)*XIART(Z/D)/((4.0D00*D-Z)*Z*
     5        DSQRT(Z))-(-4.0D00*D*D+5.0D00*D*Q-Q*Q)*XIART(Q/D)/
     6        ((4.0D00*D-Q)*Q*DSQRT(Q)))+(2.0D00*Q*D/(Z-Q))*
     7        (XIF1(Z/D)-XIF1(Q/D))-(4.0D00*Q*D/(Z-Q))*
     8        XIART(Z/D)/DSQRT(Z)+(4.0D00*DSQRT(Q)*D/(Z-Q))*XIART(Q/D)
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION RXY3(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      IF(Q.EQ.Z)THEN
         RXY3=0.0D00
      ELSE
         RXY3=-(Z-Q)/12.0D00+(6.0D00*D+Q)/8.0D00+
     1        (4.0D00*D*D-12.0D00*D*Q-Q*Q)/(4.0D00*(Z-Q))-
     2        D*D/(Z-Q)+D*(Z+Q)/(4.0D00*(Z-Q))-
     3        (D*D*D/((Z-Q)*(Z-Q)))*(RART(Z/D)*Z*DSQRT(Z)/
     4        (D*D)-RART(Q/D)*Q*DSQRT(Q)/(D*D))-
     5        Q*Q*D/(Z*(Z-Q))+(Q*Q*Q/((Z-Q)*(Z-Q)))*
     6       ((-8.0D00*D*D+6.0D00*D*Z-Z*Z)*RART(Z/D)/(2.0D00*Z*DSQRT(Z)*
     7        (4.0D00*D-Z))-(-8.0D00*D*D+6.0D00*D*Q-Q*Q)*RART(Q/D)/
     8        (2.0D00*Q*DSQRT(Q)*(4.0D00*D-Q)))-
     9        (3.0D00*D*Q)/(Z-Q)+(3.0D00*D*D*Q/((Z-Q)*(Z-Q)))*
     1        (2.0D00*DSQRT(Z)*RART(Z/D)/D-2.0D00*DSQRT(Q)*
     2        RART(Q/D)/D-RF1(Z/D)+RF1(Q/D))-
     3        (3.0D00*D*Q*Q/((Z-Q)*(Z-Q)))*(RF1(Z/D)-RF1(Q/D))+
     4        (9.0D00*D*Q*Q/((Z-Q)*(Z-Q)))*(RART(Z/D)/DSQRT(Z)-
     5        RART(Q/D)/DSQRT(Q))
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION XIXY3(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      IF(Q.EQ.Z)THEN
         XIXY3=0.0D00
      ELSE
         XIXY3=-(D*D*D/((Z-Q)*(Z-Q)))*(XIART(Z/D)*Z*DSQRT(Z)/
     1        (D*D)-XIART(Q/D)*Q*DSQRT(Q)/(D*D))+
     2        ((Q*Q*Q)/((Z-Q)*(Z-Q)))*
     3      ((-8.0D00*D*D+6.0D00*D*Z-Z*Z)*XIART(Z/D)/(2.0D00*Z*DSQRT(Z)*
     4        (4.0D00*D-Z))-(-8.0D00*D*D+6.0D00*D*Q-Q*Q)*XIART(Q/D)/
     5        (2.0D00*Q*DSQRT(Q)*(4.0D00*D-Q)))+
     6        (3.0D00*D*D*Q/((Z-Q)*(Z-Q)))*
     7        (2.0D00*DSQRT(Z)*XIART(Z/D)/D-2.0D00*DSQRT(Q)*
     8        XIART(Q/D)/D-XIF1(Z/D)+XIF1(Q/D))-
     9        (3.0D00*D*Q*Q/((Z-Q)*(Z-Q)))*(XIF1(Z/D)-XIF1(Q/D))+
     1        (9.0D00*D*Q*Q/((Z-Q)*(Z-Q)))*(XIART(Z/D)/DSQRT(Z)-
     2        XIART(Q/D)/DSQRT(Q))
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION RX2Y(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      RX2Y=-(Z-Q)/6.0D00+
     1     2.0D00*D*(-RART(Z/D)/DSQRT(Z)+
     2     RART(Q/D)/DSQRT(Q))+(Q/3.0D00)*
     3    (-2.0D00*D/Z+2.0D00*D/Q-(8.0D00*D*D+2.0D00*D*Q-Q*Q)*RART(Q/D)/
     4     ((4.0D00*D-Q)*Q*DSQRT(Q))+(8.0D00*D*D+
     5     2.0D00*D*Z-Z*Z)*RART(Z/D)/((4.0D00*D-Z)*Z*DSQRT(Z)))
      RETURN
      END

      REAL*8 FUNCTION XIX2Y(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      XIX2Y=
     1     2.0D00*D*(-XIART(Z/D)/DSQRT(Z)+
     2     XIART(Q/D)/DSQRT(Q))-(Q/3.0D00)*
     3     ((8.0D00*D*D+2.0D00*D*Q-Q*Q)*XIART(Q/D)/
     4     ((4.0D00*D-Q)*Q*DSQRT(Q))-(8.0D00*D*D+
     5     2.0D00*D*Z-Z*Z)*XIART(Z/D)/((4.0D00*D-Z)*Z*DSQRT(Z)))
      RETURN
      END

      REAL*8 FUNCTION RX2Y2(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      IF(Q.EQ.Z)THEN
         RX2Y2=0.0D00
      ELSE
         RX2Y2=-(Z-Q)/24.0D00-(6.0D00*D-Q)/12.0D00-
     1        (D*D/(Z-Q))*(RF1(Z/D)-RF1(Q/D))-
     2        D*Q/(3.0D00*Z)-(Q*Q/(Z-Q))*
     3      ((8.0D00*D*D+2.0D00*D*Z-Z*Z)*RART(Z/D)/(6.0D00*(4.0D00*D-Z)*
     4        Z*DSQRT(Z))-(8.0D00*D*D+2.0D00*D*Q-Q*Q)*RART(Q/D)/
     5        (6.0D00*(4.0D00*D-Q)*Q*DSQRT(Q)))+
     6        (2.0D00*D*Q/(Z-Q))*(RART(Z/D)/DSQRT(Z)-
     7        RART(Q/D)/DSQRT(Q))
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION XIX2Y2(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      IF(Q.EQ.Z)THEN
         XIX2Y2=0.0D00
      ELSE
         XIX2Y2=-(D*D/(Z-Q))*(XIF1(Z/D)-XIF1(Q/D))-
     1        (Q*Q/(Z-Q))*
     2     ((8.0D00*D*D+2.0D00*D*Z-Z*Z)*XIART(Z/D)/(6.0D00*(4.0D00*D-Z)*
     3        Z*DSQRT(Z))-(8.0D00*D*D+2.0D00*D*Q-Q*Q)*XIART(Q/D)/
     4        (6.0D00*(4.0D00*D-Q)*Q*DSQRT(Q)))+
     5        (2.0D00*D*Q/(Z-Q))*(XIART(Z/D)/DSQRT(Z)-
     6        XIART(Q/D)/DSQRT(Q))
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION RX3Y(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      RX3Y=RX2Y(Z,Q)/2.0D00
      RETURN
      END

      REAL*8 FUNCTION XIX3Y(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      XIX3Y=XIX2Y(Z,Q)/2.0D00
      RETURN
      END

      REAL*8 FUNCTION RN(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      A11=2.0D00*PK1(Y,Z)*PK1(Y,Z)-PK1(Y,Z)*(Z+Q)
      A12=2.0D00*PK1(Y,Z)*PK2(Y,Z)-
     1     PK1(Y,Z)*(Z-Q)/2.0D00-PK2(Y,Z)*(Z+Q)/2.0D00
      A22=2.0D00*PK2(Y,Z)*PK2(Y,Z)-PK2(Y,Z)*(Z-Q)
      RN=A11*(-RX3Y(Z,Q)+RX2Y(Z,Q))+
     1     A12*(2.0D00*RX2Y2(Z,Q)+RXY(Z,Q)-RX2Y(Z,Q)-RXY2(Z,Q))+
     2     A22*(RXY2(Z,Q)-RXY3(Z,Q))
      RETURN
      END

      REAL*8 FUNCTION XIN(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      A11=2.0D00*PK1(Y,Z)*PK1(Y,Z)-PK1(Y,Z)*(Z+Q)
      A12=2.0D00*PK1(Y,Z)*PK2(Y,Z)-
     1     PK1(Y,Z)*(Z-Q)/2.0D00-PK2(Y,Z)*(Z+Q)/2.0D00
      A22=2.0D00*PK2(Y,Z)*PK2(Y,Z)-PK2(Y,Z)*(Z-Q)
      XIN=A11*(-XIX3Y(Z,Q)+XIX2Y(Z,Q))+
     1     A12*(2.0D00*XIX2Y2(Z,Q)+XIXY(Z,Q)-XIX2Y(Z,Q)-XIXY2(Z,Q))+
     2     A22*(XIXY2(Z,Q)-XIXY3(Z,Q))
      RETURN
      END

      REAL*8 FUNCTION RDL(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      RDL=13.0D00*(Z+Q)/144.0D00-
     1     13.0D00*D/12.0D00-(D*D/(2.0D00*(Z-Q)))*
     2     (RF1(Z/D)-RF1(Q/D))+(Q*Q/(2.0D00*(Z-Q)))*
     3     (-(-8.0D00*D*D-2.0D00*Q*D+Q*Q)*RART(Q/D)/(6.0D00*DSQRT(Q)*Q*
     4     (4.0D00*D-Q))+(-8.0D00*D*D-2.0D00*Z*D+Z*Z)*RART(Z/D)/
     5     (6.0D00*DSQRT(Z)*Z*(4.0D00*D-Z)))+
     6     (Q*D/(Z-Q))*(RART(Z/D)/DSQRT(Z)-RART(Q/D)/DSQRT(Q))-
     7     (2.0D00*Q*D+Q*Z-10.0D00*D*Z+Z*Z)*RART(Z/D)/
     8     (12.0D00*Z*DSQRT(Z))
      RETURN
      END

      REAL*8 FUNCTION XIDL(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      XIDL=-(D*D/(2.0D00*(Z-Q)))*(XIF1(Z/D)-XIF1(Q/D))+
     1     (Q*Q/(2.0D00*(Z-Q)))*
     2     (-(-8.0D00*D*D-2.0D00*Q*D+Q*Q)*XIART(Q/D)/(6.0D00*DSQRT(Q)*Q*
     3     (4.0D00*D-Q))+(-8.0D00*D*D-2.0D00*Z*D+Z*Z)*XIART(Z/D)/
     4     (6.0D00*DSQRT(Z)*Z*(4.0D00*D-Z)))+
     5     (Q*D/(Z-Q))*(XIART(Z/D)/DSQRT(Z)-XIART(Q/D)/DSQRT(Q))-
     6     (2.0D00*Q*D+Q*Z-10.0D00*D*Z+Z*Z)*XIART(Z/D)/
     7     (12.0D00*Z*DSQRT(Z))
      RETURN
      END

      REAL*8 FUNCTION RD1L(Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      RD1L=5.0D00*Q/18.0D00-4.0D00*D/3.0D00+
     1     (4.0D00*D-Q)*RART(Q/D)/(3.0D00*DSQRT(Q))
      RETURN
      END

      REAL*8 FUNCTION XID1L(Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      XID1L=(4.0D00*D-Q)*XIART(Q/D)/(3.0D00*DSQRT(Q))
      RETURN
      END

      REAL*8 FUNCTION RB(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
      RB=A2*(-2.0D00*RDL(Z,Q)+RD1L(Q))+RDIVB(Y,Z,Q)
      RETURN
      END

      REAL*8 FUNCTION XIB(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
      XIB=A2*(-2.0D00*XIDL(Z,Q)+XID1L(Q))+XIDIVB(Y,Z,Q)
      RETURN
      END

      REAL*8 FUNCTION RPD1L(Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      RPD1L=-8.0D00/9.0D00+8.0D00*D/(3.0D00*Q)-
     1     2.0D00*(4.0D00*D-Q)*RART(Q/D)/(3.0D00*Q*DSQRT(Q))
      RETURN
      END

      REAL*8 FUNCTION XIPD1L(Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      XIPD1L=-2.0D00*(4.0D00*D-Q)*XIART(Q/D)/
     1     (3.0D00*Q*DSQRT(Q))
      RETURN
      END

      REAL*8 FUNCTION RD(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
      RD=A2*(RDL(Z,Q)-RD1L(Q)/2.0D00+
     1     (2.0D00*PK2(Y,Z)-(Z-Q)/2.0D00)*
     2     (2.0D00*RXYL(Z,Q)/((Z-Q)*(Z-Q))-
     3     RYL(Z,Q)/((Z-Q)*(Z-Q)))+
     4     (2.0D00*Y-Q)*(RXL(Z,Q)/((Z-Q)*(Z-Q))-
     5     RL(Z,Q)/2.0D00)+(2.0D00*PK1(Y,Z)-
     6     (Z+Q)/2.0D00)*RPD1L(Q)/4.0D00)+RDIVD(Y,Z,Q)
      RETURN
      END

      REAL*8 FUNCTION XID(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
      XID=-A2*(XIDL(Z,Q)-XID1L(Q)/2.0D00+
     1     (2.0D00*PK2(Y,Z)-(Z-Q)/2.0D00)*
     2     (2.0D00*XIXYL(Z,Q)/((Z-Q)*(Z-Q))-
     3     XIYL(Z,Q)/((Z-Q)*(Z-Q)))+
     4     (2.0D00*Y-Q)*(XIXL(Z,Q)/((Z-Q)*(Z-Q))-
     5     XIL(Z,Q)/2.0D00)+(2.0D00*PK1(Y,Z)-
     6     (Z+Q)/2.0D00)*XIPD1L(Q)/4.0D00)
      RETURN
      END

      REAL*8 FUNCTION RC(Z)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
      ME=0.54745D00
      ET=ME*ME/(MKA*MKA)
      RC=2.0D00*CC*(2.0D00+D-3.0D00*ET)/(3.0D00*(Z-ET))
      RETURN
      END

      REAL*8 FUNCTION RART(Z)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      RART=0.0D00
      IF(Z.EQ.0.0D00)THEN
         RART=0.0D00
      ELSEIF(Z.LE.4.0D00)THEN
         RART=DSQRT(D)*DSQRT(4.0D00/Z-1.0D00)*DSQRT(Z)*
     1        DASIN(DSQRT(Z)/2.0D00)
      ELSEIF(Z.GT.4.0D00)THEN
         RART=-DSQRT(D)*DSQRT(1.0D00-4.0D00/Z)*DSQRT(Z)*
     1        (DLOG((1.0D00-DSQRT(1.0D00-4.0D00/Z))/
     2        (1.0D00+DSQRT(1.0D00-4.0D00/Z))))/2.0D00
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION XIART(Z)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C/PI,ALFA,E
      COMMON/C1/D
      XIART=0.0D00
      IF(Z.LE.4.0D00)THEN
         XIART=0.0D00
      ELSEIF(Z.GT.4.0D00)THEN
         XIART=-DSQRT(D)*DSQRT(1.0D00-4.0D00/Z)*DSQRT(Z)*PI/2.0D00
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION RF1(Z)
      IMPLICIT REAL*8(A-H,L-Z)
      RF1=Z*(RF(Z)-1.0D00)/2.0D00
      RETURN
      END

      REAL*8 FUNCTION XIF1(Z)
      IMPLICIT REAL*8(A-H,L-Z)
      XIF1=Z*XIF(Z)/2.0D00
      RETURN
      END

      REAL*8 FUNCTION RDIVA(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
      LL=DLOG(MP*MP/(MR*MR))
      RDIVA=-A1*((D-Q/12.0D00)*LL+
     1     RD1L(Q)/2.0D00)+
     2     A2*((1.0D00+LL)/12.0D00+
     3     (D-Q/12.0D00)*LL/2.0D00+RD1L(Q)/4.0D00)
      RETURN
      END

      REAL*8 FUNCTION XIDIVA(Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
      XIDIVA=-A1*XID1L(Q)/2.0D00+A2*XID1L(Q)/4.0D00
      RETURN
      END

      REAL*8 FUNCTION RDIVB(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C1/D
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
      LL=DLOG(MP*MP/(MR*MR))
      RDIVB=A2*((Z-Q)*LL/12.0D00-(Q*LL/6.0D00-RD1L(Q))/4.0D00)
      RETURN
      END

      REAL*8 FUNCTION XIDIVB(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
      XIDIVB=A2*XID1L(Q)/4.0D00
      RETURN
      END

      REAL*8 FUNCTION RDIVD(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      COMMON/C2/MKA,MP,MR,CC,A1,A2,LIN
      LL=DLOG(MP*MP/(MR*MR))
      RDIVD=-A2*(Z-Q)*LL/24.0D00
      RETURN
      END

      REAL*8 FUNCTION PS(Y,Z,Q)
      IMPLICIT REAL*8(A-H,L-Z)
      ACO=(Z-Q)*(Z-Q)/3.0D00
      BCO=16.0D00*PK1(Y,Z)*PK1(Y,Z)*PK2(Y,Z)*PK2(Y,Z)/
     1     (3.0D00*(Z-Q)*(Z-Q))+8.0D00*PK2(Y,Z)*PK2(Y,Z)*Q/
     2     (3.0D00*(Z-Q)*(Z-Q))+2.0D00/3.0D00-8.0D00*
     3     PK1(Y,Z)*PK2(Y,Z)/(3.0D00*(Z-Q))
      DCO=8.0D00*PK1(Y,Z)*PK2(Y,Z)*Q/(3.0D00*(Z-Q))+
     1     8.0D00*PK2(Y,Z)*PK2(Y,Z)*Q*Q/(3.0D00*(Z-Q)*(Z-Q))-
     2     2.0D00*Q/3.0D00
      ABCO=-4.0D00*PK2(Y,Z)*PK2(Y,Z)*Q/
     1     (3.0D00*(Z-Q))-(Z-Q)/3.0D00
      ADCO=-4.0D00*PK2(Y,Z)*Q/3.0D00
      BDCO=16.0D00*PK1(Y,Z)*PK2(Y,Z)*PK2(Y,Z)*Q/
     1     (3.0D00*(Z-Q)*(Z-Q))
      RAC=RA(Y,Z,Q)/((Z-Q)*(Z-Q))
      XIAC=XIA(Y,Z,Q)/((Z-Q)*(Z-Q))
      PS=ACO*(RAC*RAC+XIAC*XIAC+RC(Z)**2)+
     1     BCO*(RB(Y,Z,Q)**2+XIB(Y,Z,Q)**2)+
     2     DCO*(RD(Y,Z,Q)**2+XID(Y,Z,Q)**2)+
     3     2.0D00*ABCO*(RAC*RB(Y,Z,Q)+XIAC*XIB(Y,Z,Q))+
     4     2.0D00*ADCO*(RAC*RD(Y,Z,Q)+XIAC*XID(Y,Z,Q))+
     5     2.0D00*BDCO*(RB(Y,Z,Q)*RD(Y,Z,Q)+XIB(Y,Z,Q)*XID(Y,Z,Q))
      RETURN
      END
