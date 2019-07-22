      SUBROUTINE KCH2LNULL(JKAON, MODE)
C-------------------------------------------------------
C Main routine for charged K -> 4 leptons generation
C (and also charged pion -> 4 leptons)
C   MODE=1  K  -> e nu e e
C   MODE=2  K  -> mu nu e e
C   MODE=3  K  -> e nu mu mu
C   MODE=4  K  -> mu nu mu mu
C   MODE=5  pi -> e nu e e
C   MODE=6  pi -> mu nu e e
C M. Sozzi and E. Goudzovski 9.2002
C-------------------------------------------------------

#include "common_blocks.f"
#include "masses.f"

      INTEGER MODE

      REAL*8 XMK,XML,XMPI,XMLP,FK,FPI,XL9,XL10,FUNNY
      REAL*8 CROSS0,CROSS5,CROSS1,CROSS2,CROSS3,CROSS4
      INTEGER IFORM,ICUT,MCUT,IMAEL
      REAL*8 ZMIN

      COMMON/MASSES/XMK,XML,XMPI,XMLP,FK,FPI,XL9,XL10,FUNNY
      COMMON/GAMMAS/CROSS0,CROSS5,CROSS1,CROSS2,CROSS3,CROSS4
      COMMON/CHECKS/IFORM,ICUT,MCUT,IMAEL
      COMMON/CUTS/ZMIN

      REAL*8 XM(100),P(4,100)
      REAL*8 PPART1(4),PPART2(4),PPART3(4),PPART4(4)
      REAL*8 X,GAMMA,TOPGAMMA,WT1,WTMAX,Z,Z1,R,ET
      INTEGER EQL,ID(4),I,J,IDUMMY,IL

C--- FUNCTIONS
      REAL*8 PROD,CROSS_EQ,CROSS_NE,RANF

C--- Steering
C-   For K-->lnuee modes: MCUT=1 means generation for Z>1E-3,
C-   MCUT=0 means generation in full phase space
      IF (MODE.EQ.1) THEN ! K -> e nu e e
         EQL   = 1        ! means equal leptons
         MCUT  = 1        ! can be 0 or 1
         WTMAX = 0.33
         IF (MCUT.EQ.0) TOPGAMMA = 2.6E-3 ! For ZMIN=0.0
         IF (MCUT.EQ.1) TOPGAMMA = 2.1E-5 ! For ZMIN=1.0E-3
         XML   = MEL
         XMLP  = MEL
         ID(2) = IDELEP
         ID(3) = IDELEP
         ID(4) = IDELEM

      ELSEIF (MODE.EQ.2) THEN ! K -> mu nu e e
         EQL   = 0            ! means unequal leptons
         MCUT  = 1            ! can be 0 or 1
         WTMAX = 0.26
         IF (MCUT.EQ.0) TOPGAMMA = 100  ! for ZMIN=0.0
         IF (MCUT.EQ.1) TOPGAMMA = 0.16 ! for ZMIN=1.0E-3
         XML   = MMU
         XMLP  = MEL
         ID(2) = IDMUP
         ID(3) = IDELEP
         ID(4) = IDELEM

      ELSEIF (MODE.EQ.3) THEN ! K -> e nu mu mu
         EQL      = 0         ! means unequal leptons
         MCUT     = 0         ! always zero
         WTMAX    = 0.20
         TOPGAMMA = 4.4E-7
         XML      = MEL
         XMLP     = MMU
         ID(2)    = IDELEP
         ID(3)    = IDMUP
         ID(4)    = IDMUM

      ELSEIF (MODE.EQ.4) THEN ! K -> mu nu mu mu
         EQL      = 1         ! means equal leptons
         MCUT     = 0         ! always zero
         WTMAX    = 0.07
         TOPGAMMA = 3.1E-6
         XML      = MMU
         XMLP     = MMU
         ID(2)    = IDMUP
         ID(3)    = IDMUP
         ID(4)    = IDMUM

      ELSEIF (MODE.EQ.5) THEN ! pi -> e nu e e
         EQL   = 1            ! means equal leptons
         MCUT  = 1            ! can be 0 or 1
         WTMAX = 0.33
         IF (MCUT.EQ.0) TOPGAMMA = 2.0E-3 ! for ZMIN=0.0
         IF (MCUT.EQ.1) TOPGAMMA = 3.0E-5 ! for ZMIN=1.0E-3
         XML   = MEL
         XMLP  = MEL
         ID(2) = IDELEP
         ID(3) = IDELEP
         ID(4) = IDELEM

      ELSEIF (MODE.EQ.6) THEN ! pi -> mu nu e e
         EQL   = 0            ! means unequal leptons
         MCUT  = 1            ! can be 0 or 1
         WTMAX = 0.005
         IF (MCUT.EQ.0) TOPGAMMA = 1.8  ! for ZMIN=0.0
         IF (MCUT.EQ.1) TOPGAMMA = 0.02 ! for ZMIN=1.0E-3
         XML   = MMU
         XMLP  = MEL
         ID(2) = IDMUP
         ID(3) = IDELEP
         ID(4) = IDELEM

      ELSE
         WRITE (0,*) 'CALLING ERROR IN CHK24L'
         STOP
      ENDIF

C--- Index of the lepton with the charge opposite to parent charge,
C--- or opposite to the lepton in ID(2), which is the same thing
      IL = 4

C--- Initialization
      ZMIN  = 1.0E-3 ! If changed, then TOPGAMMA for MCUT=1 has to change
      IMAEL = 1      ! 1 If matrix element simulated, 0 if phase space only
      IFORM = 1      ! 1 If full form factors, 0 if tree level only
      ICUT  = MCUT   ! 0 If full phase space, 1 if a cut added, see MCUT

C--- Some constants
C THE FUNNY NUMBER (IN THE K CASE) IS:
C 3613.138 = XMK**5 * |V_US|**2 * G_F**2 / GAMMA_TOT
C GAMMA_TOT IS THE TOTAL K^+ WIDTH
C V_US THE RELEVANT KM MATRIX ELEMENT
C G_F THE FERMI CONSTANT

      FPI  = 0.0932
      XMPI = MPI
      IF (MODE.LE.4) THEN
         XMK   = MKCH
         FK    = 0.1136 / XMK
         XL9   = 6.9E-3
         FUNNY = 3613.138
      ELSE
         XMK   = MPI
         FK    = FPI / XMK
         XL9   = 6.0E-3
         FUNNY = 269.787
      ENDIF
      XL10 = -5.5E-3
      R    = (XMLP/XMK)**2
      ET   = 1.0

C--- Lepton masses array for decay generation
C--- Masses are scaled to kaon mass, which is equal to unity
      XM(1) = 0.0
      XM(2) = XML/XMK
      XM(3) = XMLP/XMK
      XM(4) = XMLP/XMK

C--- Generation
 10   CONTINUE
      CALL RAMBOS(4, ET, XM, P, WT1)
      X = RANF()
      IF (X.GT.WT1/WTMAX) GOTO 10
      IF (IMAEL.EQ.0) GOTO 20 ! pure phase space simulation
      Z  = 2.0 * (PROD(P,3,4)  + R)
      Z1 = 2.0 * (PROD(P,2,IL) + R)
      IF (MCUT.NE.0) THEN
         IF (Z.LT.ZMIN) GOTO 10
         IF (EQL.eq.1 .and. Z1.LT.ZMIN) GOTO 10
         ENDIF
      IF (EQL.GT.0) GAMMA = CROSS_EQ(P)
      IF (EQL.EQ.0) GAMMA = CROSS_NE(P)
      IF (GAMMA.GT.TOPGAMMA) THEN
         WRITE(0,*) '@@warning: gamma=',gamma,' topgamma=',topgamma
         ENDIF
      X = RANF()
      IF (X.GT.GAMMA/TOPGAMMA) GOTO 10
 20   CONTINUE

C--- In case of pion decay, correct energy from kaon to pion
      IF (MODE.GE.5) THEN
         P4INI(4,JKAON) = SQRT(
     >   P4INI(1,JKAON)**2+P4INI(2,JKAON)**2+P4INI(3,JKAON)**2
     >   +SQMPI)
        ENDIF

C--- Boost to lab system
      ID(1) = IDNU ! add neutrino
      DO J=1,4
        PPART1(J) = P(J,1) * XMK
        PPART2(J) = P(J,2) * XMK
        PPART3(J) = P(J,3) * XMK
        PPART4(J) = P(J,4) * XMK
      ENDDO
      IDUMMY = MCADD4GEN(ID(1), PPART1, 0)
      IDUMMY = MCADD4GEN(ID(2), PPART2, 0)
      IDUMMY = MCADD4GEN(ID(3), PPART3, 0)
      IDUMMY = MCADD4GEN(ID(4), PPART4, 0)
C      CALL DBOOST(P4INI(1,JKAON), MKCH, PPART1, PPART1)
      CALL DBOOST(P4INI(1,JKAON), MKCH, PPART2, PPART2)
      CALL DBOOST(P4INI(1,JKAON), MKCH, PPART3, PPART3)
      CALL DBOOST(P4INI(1,JKAON), MKCH, PPART4, PPART4)
C      IDUMMY = MCADD4(ID(1), PPART1)
      IDUMMY = MCADD4(ID(2), PPART2)
      IDUMMY = MCADD4(ID(3), PPART3)
      IDUMMY = MCADD4(ID(4), PPART4)

      RETURN
      END

c=======================================
      FUNCTION PROD(P,I,J)
      IMPLICIT NONE
      INTEGER I,J
      REAL*8 P(4,100), PROD
      PROD = P(4,I)*P(4,J)-P(3,I)*P(3,J)-P(2,I)*P(2,J)-P(1,I)*P(1,J)
      RETURN
      END
c=========================================
      FUNCTION FVKM1(Z)
      IMPLICIT NONE
      COMPLEX*8 FVKM1,HF
      REAL*8 Z,Q2
      REAL*8 XMK,XML,XMPI,XMLP,FK,FPI,XL9,XL10,FUNNY
      COMMON/MASSES/XMK,XML,XMPI,XMLP,FK,FPI,XL9,XL10,FUNNY
      Q2 = Z*XMK**2
      FVKM1 =  CMPLX(2.0)*HF(Q2,XMK) + HF(Q2,XMPI)
      RETURN
      END
c============================================
      FUNCTION EPSILO(P)
      IMPLICIT NONE
      REAL*8 P(4,100), EPSILO
C WE MADE USE OF TURNING ONE OF THE MOMENTA INTO A KAON, THE PL ONE
C THE   ONE IS BECAUSE WE NEED TO LOWER ALL THE MOMENTA INDICES
C AND I HAVE USED E^(0123) = -1 HERE
      EPSILO =   1.0*
     *(P(1,1)*P(2,3)*P(3,4)+P(2,1)*P(3,3)*P(1,4)+P(3,1)*P(1,3)*P(2,4)
     *-P(1,1)*P(3,3)*P(2,4)-P(3,1)*P(2,3)*P(1,4)-P(2,1)*P(1,3)*P(3,4))
      RETURN
      END
c============================================
      FUNCTION HF(Q2,XM)
      IMPLICIT NONE
      COMPLEX*8 Y,HF
      REAL*8 PI,Q2,XM,X
      REAL*8 XMK,XML,XMPI,XMLP,FK,FPI,XL9,XL10,FUNNY
      COMMON/MASSES/XMK,XML,XMPI,XMLP,FK,FPI,XL9,XL10,FUNNY
      PI = 4.0 *  DATAN(DBLE(1.0))
      X = Q2/XM**2
      Y = SQRT((1.0,0.0)-(4.0,-1E-12)/CMPLX(X))
      HF =  CMPLX(2.0/3.0*XL9/FPI**2 *Q2 +
     & (XM/(FPI*4.0*PI))**2 *
     & (-2.0/3.0+5.0/36.0*X-X/12.0* LOG((XM/0.770  )**2))   )
     & +
     &  CMPLX( (XM/(FPI*4.0*PI))**2 * (1.0/3.0-X/12.0) ) *
     &       Y*LOG((Y+(1.0,0.0))/(Y-(1.0,0.0)))
      RETURN
      END
c================================================
      FUNCTION CROSS_EQ(P)
      IMPLICIT NONE
      REAL*8 CROSS_EQ
      REAL*8 XMK,XML,XMPI,XMLP,FK,FPI,XL9,XL10,FUNNY
      REAL*8 CROSS0,CROSS5,CROSS1,CROSS2,CROSS3,CROSS4
      INTEGER IFORM,ICUT,MCUT,IMAEL
      REAL*8 ZMIN
      REAL*8 A1I,C1,C2,C4,CV,DENOM1,DENOM2,DENOM3,DENOM4
      REAL*8 E11M,E11T,E12,E12M,E14,E14M,E1E1,E1E1C,E1E2,E1E2C,E1E4
      REAL*8 E1V,E1V1,E1V1C,E1E4C,E1VM,E22,E22M,E24,E24M,E2E1,E2E1C
      REAL*8 E2E2C,E2E4,E2E4C,E2V,E2E2,E2V1,E2V1C,E2VM,E44,E44M,E4E1
      REAL*8 E4E1C,E4E2,E4E2C,E4E4,E4E4C,E4V,E4V1,EV1C,E4VM,EPSI
      REAL*8 ET,EVV,E4V1C,EVVM,F1,F2,F4,FV,PI,R,T,T1,TINT1,TINT2
      REAL*8 TINT3,TINT4,V1E1,V1E1C,V1E2,V1E2C,V1E4,V1E4C,V1I,V1V1
      REAL*8 V1V1C,W2,W21,X1,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19
      REAL*8 x2,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29
      REAL*8 x3,x30,x31,x32,x33,x34,x35,x36,x4,x5,x6,x7,x8,x9,xnorm
      REAL*8 z,z1,prod,epsilo

      INTEGER IBEGIN

      COMMON/MASSES/XMK,XML,XMPI,XMLP,FK,FPI,XL9,XL10,FUNNY
      COMMON/GAMMAS/CROSS0,CROSS5,CROSS1,CROSS2,CROSS3,CROSS4
      COMMON/CHECKS/IFORM,ICUT,MCUT,IMAEL
      COMMON/CUTS/ZMIN

      REAL*8 P(4,100)
      COMPLEX*8 E1,E2,E4,V1,E1C,E2C,E4C,V1C,E11,E21,E41,V11
      COMPLEX*8 E1C1,E2C1,E4C1,V1C1
      COMPLEX*8 A1,A2,A4,FVKM1
      DATA IBEGIN/0/

      IF(IBEGIN.EQ.0) THEN
        R =(XML/XMK)**2
        ET=1.0
        PI = 4.0 * ATAN(1.0)
        IBEGIN=1
C 137.035989**2 = 1./(ALPHA_EM)**2
        XNORM = FUNNY/(64.0* PI**6 *137.035989**2)
        V1I = -XMK/(8.0*PI**2*FPI)
        A1I= -4.0/FPI*(XL9+XL10)*XMK
      ENDIF
C
C   CALCULATING ALL RELEVANT SCALAR PRODUCTS
      X1  = PROD(P,2,1)
      X2  = PROD(P,2,3)
      X3  = PROD(P,2,4)
      X4  = X1 + R
      X5  = X2+X3
      X6  = PROD(P,1,3)
      X7  = PROD(P,1,4)
      X8  = X1
      X9  = X6 + X7
      X10 = PROD(P,3,4)
      X11 = X2 + X6
      X12 = R + X10
      X13 = X3 + X7
      X14 = X12
      X15 = X11 + X13
      X16 = ET*P(4,2)
      X17 = ET*P(4,1)
      X18 = ET*P(4,3)
      X19 = ET*P(4,4)
      X20 = X16+X17
      X21 = X18+X19
      X22 = X2 + X1
      X23 = R + X3
      X24 = X6
      X25 = X1 + X7
      X26 = R + X6
      X27 = X2 + X10
      X28 = X10 + X7
      X29 = R + X3
      X30 = ET*(P(4,1)+P(4,3))
      X31 = ET*(P(4,2)+P(4,4))
      X32 = X27 + X25
      X33 = X12 + X9
      X34 = X5 + X14
      X35 = X11 + X8
      X36 = X23 + X25
      Z = 2.0*R + 2.0*X10
      Z1= 2.0*R + 2.0*X3
      W2 = R + 2.0 * X1
      W21= R + 2.0 * X6
      DENOM1 = 1.0 / (2.0*X5+Z)
      DENOM2 = 1.0 / (2.0*X21-Z)
      DENOM3 = 1.0 / (2.0*X27+Z1)
      DENOM4 = 1.0 / (2.0*X31-Z1)
      EPSI = -EPSILO(P)
C THE - SIGN IS BECAUSE OF THE I**2 THAT WE HAVE DROPPED
      IF (ICUT.EQ.1) THEN
        IF(Z.LT.ZMIN .OR. Z1.LT.ZMIN) THEN
          CROSS0 = 0.0
          CROSS5 = 0.0
          CROSS1 = 0.0
          CROSS2 = 0.0
          CROSS3 = 0.0
          CROSS4 = 0.0
          CROSS_EQ = 0.0
          RETURN
        ENDIF
      ENDIF
C
C CALCULATION OF THE FORMFACTORS (COMPLEX ONES)
      IF(IFORM.EQ.0) THEN
        A1=(0.0,0.0)
        A2=(0.0,0.0)
        A4=(0.0,0.0)
        V1=(0.0,0.0)
      ELSE
        A1 = CMPLX(A1I)
        A2 = CMPLX(-2.0*FK/Z)* FVKM1(Z)
        A4 = CMPLX(2.0*FK*DENOM2/Z) * FVKM1(Z)
        V1 = CMPLX(V1I)
      ENDIF
      E1 = A1*CMPLX(X15) + A2*CMPLX(Z)
      E2 = -A1
      E4 = A1 - CMPLX(Z)*A4
      IF(IFORM.EQ.0) THEN
        A1 = (0.0,0.0)
        A2 = (0.0,0.0)
        A4 = (0.0,0.0)
        V11 = (0.0,0.0)
      ELSE
        A1 = CMPLX(A1I)
        A2 = CMPLX(-2.0*FK/Z1) * FVKM1(Z1)
        A4 = CMPLX(2.0*FK*DENOM4/Z1) * FVKM1(Z1)
        V11= CMPLX(V1I)
      ENDIF
      E11= A1*CMPLX(X32) + A2*CMPLX(Z1)
      E21= -A1
      E41= A1 - CMPLX(Z1)*A4
      E1C = CONJG(E1)
      E2C = CONJG(E2)
      E4C = CONJG(E4)
      V1C = CONJG(V1)
      E1C1= CONJG(E11)
      E2C1= CONJG(E21)
      E4C1= CONJG(E41)
      V1C1= CONJG(V11)
C
C CALCULATION OF THE VARIOUS PARTS OF THE TRACES
C
C THE DIRECT PIECE
C     CALCULATION OF THE RELEVANT COMBINATIONS OF FORM FACTORS
      E1E1C = DBLE(E1*E1C)
      E2E2C = DBLE(E2*E2C)
      E4E4C = DBLE(E4*E4C)
      V1V1C = DBLE(V1*V1C)
      E11T = DBLE(E1 + E1C)
      E22 =  DBLE(E2 + E2C)
      E44 =  DBLE(E4+E4C)
      EVV =  DBLE(V1+V1C)
      E12 =  DBLE(E1*E2C + E1C*E2)
      E14 =  DBLE(E1*E4C + E1C*E4)
      E1V =  DBLE(E1*V1C + E1C*V1)
      E24 =  DBLE(E2*E4C + E2C*E4)
      E2V =  DBLE(E2*V1C + E2C*V1)
      E4V =  DBLE(E4*V1C + E4C*V1)
      E11M = EPSI * AIMAG(E1 - E1C)
      E22M = EPSI * AIMAG(E2 - E2C)
      E44M = EPSI * AIMAG(E4 - E4C)
      EVVM = EPSI * AIMAG(V1 - V1C)
      E12M = EPSI * AIMAG(E1*E2C - E1C*E2)
      E14M = EPSI * AIMAG(E1*E4C - E1C*E4)
      E1VM = EPSI * AIMAG(E1*V1C - E1C*V1)
      E24M = EPSI * AIMAG(E2*E4C - E2C*E4)
      E2VM = EPSI * AIMAG(E2*V1C - E2C*V1)
      E4VM = EPSI * AIMAG(E4*V1C - E4C*V1)

      T = 0.0
      T = T + E1E1C * ( 64*X1*R + 64*X2*X7 + 64*X3*X6 )

      T = T + E2E2C * ( 32*X1*X10 - 64*X1*X18*X19 + 32*X1*R - 64*X10*
     +    X16*X17 + 128*X16*X17*X18*X19 - 64*X16*X17*R )

      T = T + E4E4C * ( 32*X1*X10*W2 - 64*X1*X18*X19*W2 + 32*X1*R*W2
     +     - 64*X4*X8*X10 + 128*X4*X8*X18*X19 - 64*X4*X8*R )

      T = T + V1V1C * ( 64*X1*X10*X21**2 - 64*X1*X10*Z + 64*X1*X12*X14
     +     - 64*X1*X12*X19*X21 - 64*X1*X14*X18*X21 + 64*X1*X18*X19*Z -
     +    64*X2*X7*X21**2 + 64*X2*X7*Z - 64*X2*X9*X14 + 64*X2*X9*X19*
     +    X21 + 64*X2*X14*X17*X21 - 64*X2*X17*X19*Z - 64*X3*X6*X21**2
     +     + 64*X3*X6*Z - 64*X3*X9*X12 + 64*X3*X9*X18*X21 + 64*X3*X12*
     +    X17*X21 - 64*X3*X17*X18*Z - 64*X5*X6*X14 + 64*X5*X6*X19*X21
     +     - 64*X5*X7*X12 + 64*X5*X7*X18*X21 + 64*X5*X9*X10 - 128*X5*X9
     +    *X18*X19 - 64*X5*X9*R - 64*X5*X10*X17*X21 + 64*X5*X12*X17*X19
     +     + 64*X5*X14*X17*X18 + 64*X5*X17*X21*R + 64*X6*X14*X16*X21 -
     +    64*X6*X16*X19*Z + 64*X7*X12*X16*X21 - 64*X7*X16*X18*Z - 64*X9
     +    *X10*X16*X21 + 64*X9*X12*X16*X19 + 64*X9*X14*X16*X18 + 64*X9*
     +    X16*X21*R + 64*X10*X16*X17*Z - 128*X12*X14*X16*X17 - 64*X16*
     +    X17*R*Z )

      T = T + FK**2*DENOM1**2 * ( 256*X1*X2*X3*R + 128*X1*X2*X14*R +
     +    128*X1*X3*X12*R - 128*X1*X10*R**2 - 128*X1*R**3 - 64*X1*R**2*
     +    Z + 256*X2*X3*X9*R - 128*X2*X5*X7*R - 64*X2*X7*R*Z + 128*X2*
     +    X9*X14*R - 128*X3*X5*X6*R - 64*X3*X6*R*Z + 128*X3*X9*X12*R +
     +    128*X5*X9*R**2 - 128*X9*X10*R**2 - 128*X9*R**3 )

      T = T + FK**2*DENOM1*DENOM2 * (  - 256*X1*X2*X19*R - 256*X1*X3*
     +    X18*R + 256*X1*X10*X16*R + 128*X1*X10*X21*R - 128*X1*X12*X19*
     +    R - 128*X1*X14*X18*R + 256*X1*X16*R**2 + 128*X1*X21*R**2 -
     +    128*X2*X9*X19*R - 128*X3*X9*X18*R + 128*X5*X6*X19*R + 128*X5*
     +    X7*X18*R - 128*X5*X10*X17*R - 128*X5*X17*R**2 + 128*X9*X10*
     +    X16*R + 128*X9*X16*R**2 )

      T = T + FK**2*DENOM2**2 * (  - 128*X1*X10*R + 256*X1*X18*X19*R -
     +    128*X1*R**2 )

      T = T + FK*DENOM1*E11T * (  - 64*X1*X10*R - 64*X1*R**2 + 64*X2*X7
     +    *R + 64*X3*X6*R - 64*X9*X10*R - 128*X9*R**2 )

      T = T + FK*DENOM1*E22 * ( 64*X2*X17*X19*R + 64*X3*X17*X18*R - 32*
     +    X6*X19*X21*R - 32*X7*X18*X21*R - 32*X9*X10*R + 64*X9*X18*X19*
     +    R - 32*X9*R**2 - 64*X10*X16*X17*R + 32*X12*X17*X19*R + 32*X14
     +    *X17*X18*R - 64*X16*X17*R**2 )

      T = T + FK*DENOM1*E44 * ( 64*X2*X8*X19*R + 64*X3*X8*X18*R - 32*X6
     +    *X15*X19*R - 32*X7*X15*X18*R - 64*X8*X10*X16*R - 32*X8*X10*
     +    X21*R + 32*X8*X12*X19*R + 32*X8*X14*X18*R - 64*X8*X16*R**2 -
     +    32*X8*X21*R**2 - 32*X9*X10*X20*R + 32*X9*X11*X19*R + 32*X9*
     +    X13*X18*R - 32*X9*X20*R**2 + 32*X10*X15*X17*R + 32*X15*X17*
     +    R**2 )

      T = T + FK*DENOM1*EVV * ( 32*X6*X14*X21*R - 32*X6*X19*R*Z + 32*X7
     +    *X12*X21*R - 32*X7*X18*R*Z + 32*X9*X12*X19*R + 32*X9*X14*X18*
     +    R + 64*X9*X21*R**2 - 64*X12*X14*X17*R - 64*X17*R**2*Z )

      T = T + FK*DENOM1*E22M * (  - 32*X18*R + 32*X19*R )

      T = T + FK*DENOM1*E44M * (  - 32*X18*R + 32*X19*R )

      T = T + FK*DENOM1*EVVM * ( 64*X2*R - 64*X3*R + 32*X12*R - 32*X14*
     +    R )

      T = T + FK*DENOM2*E11T * (  - 64*X6*X19*R - 64*X7*X18*R + 64*X10*
     +    X17*R + 64*X17*R**2 )

      T = T + FK*DENOM2*E22 * ( 64*X10*X17*R - 128*X17*X18*X19*R + 64*
     +    X17*R**2 )

      T = T + FK*DENOM2*E44 * ( 64*X8*X10*R - 128*X8*X18*X19*R + 64*X8*
     +    R**2 )

      T = T + FK*DENOM2*EVVM * (  - 64*X18*R + 64*X19*R )

      T = T + E12 * ( 32*X1*X10 - 64*X1*X18*X19 + 32*X1*R + 32*X2*X17*
     +    X19 + 32*X3*X17*X18 + 32*X6*X16*X19 + 32*X7*X16*X18 - 64*X10*
     +    X16*X17 - 64*X16*X17*R )

      T = T + E14 * ( 32*X1*X10*X20 - 32*X1*X11*X19 - 32*X1*X13*X18 +
     +    32*X1*X20*R + 32*X2*X8*X19 + 32*X3*X8*X18 + 32*X4*X6*X19 + 32
     +    *X4*X7*X18 - 32*X4*X10*X17 - 32*X4*X17*R - 32*X8*X10*X16 - 32
     +    *X8*X16*R )

      T = T + E1V * (  - 32*X2*X9*X19 + 32*X2*X14*X17 - 32*X3*X9*X18 +
     +    32*X3*X12*X17 + 32*X5*X6*X19 + 32*X5*X7*X18 + 64*X5*X17*R -
     +    32*X6*X14*X16 - 32*X7*X12*X16 - 64*X9*X16*R )

      T = T + E24 * ( 32*X1*X10*X20 - 64*X1*X18*X19*X20 + 32*X1*X20*R
     +     - 32*X4*X10*X17 + 64*X4*X17*X18*X19 - 32*X4*X17*R - 32*X8*
     +    X10*X16 + 64*X8*X16*X18*X19 - 32*X8*X16*R )

      T = T + E2V * (  - 32*X2*X9*X19 + 32*X2*X17*X19*X21 - 32*X3*X9*
     +    X18 + 32*X3*X17*X18*X21 + 32*X5*X6*X19 + 32*X5*X7*X18 - 64*X5
     +    *X17*X18*X19 - 32*X6*X16*X19*X21 - 32*X7*X16*X18*X21 + 64*X9*
     +    X16*X18*X19 )

      T = T + E4V * (  - 32*X2*X9*X19*X20 + 32*X2*X15*X17*X19 - 32*X3*
     +    X9*X18*X20 + 32*X3*X15*X17*X18 + 32*X5*X6*X19*X20 + 32*X5*X7*
     +    X18*X20 - 32*X5*X11*X17*X19 - 32*X5*X13*X17*X18 - 32*X6*X15*
     +    X16*X19 - 32*X7*X15*X16*X18 + 32*X9*X11*X16*X19 + 32*X9*X13*
     +    X16*X18 )

      T = T + E12M * ( 32*X18 - 32*X19 )

      T = T + E1VM * (  - 32*X2 + 32*X3 + 32*X6 - 32*X7 )

      T = T + E2VM * (  - 32*X16*X18 + 32*X16*X19 + 32*X17*X18 - 32*X17
     +    *X19 )

      T = T + E4VM * (  - 32*X4*X18 + 32*X4*X19 + 32*X8*X18 - 32*X8*X19
     +     )
C
C THE EXCHANGED SQUARED
C     CALCULATION OF THE RELEVANT COMBINATIONS OF FORM FACTORS
      E1E1C = DBLE(E11*E1C1)
      E2E2C = DBLE(E21*E2C1)
      E4E4C = DBLE(E41*E4C1)
      V1V1C = DBLE(V11*V1C1)
      E11T = DBLE(E11 + E1C1)
      E22 =  DBLE(E21 + E2C1)
      E44 =  DBLE(E41 + E4C1)
      EVV =  DBLE(V11 + V1C1)
      E12 =  DBLE(E11*E2C1 + E1C1*E21)
      E14 =  DBLE(E11*E4C1 + E1C1*E41)
      E1V =  DBLE(E11*V1C1 + E1C1*V11)
      E24 =  DBLE(E21*E4C1 + E2C1*E41)
      E2V =  DBLE(E21*V1C1 + E2C1*V11)
      E4V =  DBLE(E41*V1C1 + E4C1*V11)
      E11M = EPSI * AIMAG(E11 - E1C1)
      E22M = EPSI * AIMAG(E21 - E2C1)
      E44M = EPSI * AIMAG(E41 - E4C1)
      EVVM = EPSI * AIMAG(V11 - V1C1)
      E12M = EPSI * AIMAG(E11*E2C1 - E1C1*E21)
      E14M = EPSI * AIMAG(E11*E4C1 - E1C1*E41)
      E1VM = EPSI * AIMAG(E11*V1C1 - E1C1*V11)
      E24M = EPSI * AIMAG(E21*E4C1 - E2C1*E41)
      E2VM = EPSI * AIMAG(E21*V1C1 - E2C1*V11)
      E4VM = EPSI * AIMAG(E41*V1C1 - E4C1*V11)
      T1 = 0.0
      T1 = T1 + E1E1C  * ( 64*X1*X10 + 64*X2*X7 + 64*X6*R )

      T1 = T1 + E2E2C  * ( 32*X3*X6 - 64*X3*X17*X18 - 64*X6*X16*X19
     +     + 32*X6*R + 128*X16*X17*X18*X19 - 64*X17*X18*R )

      T1 = T1 + E4E4C  * ( 32*X3*X6*W21 - 64*X3*X24*X26 - 64*X6*X16*
     +    X19*W21 + 32*X6*R*W21 + 128*X16*X19*X24*X26 - 64*X24*X26*R )

      T1 = T1 + V1V1C  * (  - 64*X1*X10*X31**2 + 64*X1*X10*Z1 - 64*X1
     +    *X18*X19*Z1 + 64*X1*X18*X29*X31 + 64*X1*X19*X27*X31 - 64*X1*
     +    X27*X29 - 64*X2*X7*X31**2 + 64*X2*X7*Z1 - 64*X2*X17*X19*Z1 +
     +    64*X2*X17*X29*X31 + 64*X2*X19*X25*X31 - 64*X2*X25*X29 + 64*X3
     +    *X6*X31**2 - 64*X3*X6*Z1 + 64*X3*X17*X18*Z1 - 64*X3*X17*X27*
     +    X31 - 64*X3*X18*X25*X31 + 64*X3*X25*X27 + 64*X6*X16*X19*Z1 -
     +    64*X6*X16*X29*X31 - 64*X6*X19*X23*X31 + 64*X6*X23*X29 - 64*X7
     +    *X16*X18*Z1 + 64*X7*X16*X27*X31 + 64*X7*X18*X23*X31 - 64*X7*
     +    X23*X27 - 64*X10*X16*X17*Z1 + 64*X10*X16*X25*X31 + 64*X10*X17
     +    *X23*X31 - 64*X10*X23*X25 + 64*X16*X17*X27*X29 + 64*X16*X18*
     +    X25*X29 - 128*X16*X19*X25*X27 - 128*X17*X18*X23*X29 - 64*X17*
     +    X18*R*Z1 + 64*X17*X19*X23*X27 + 64*X17*X27*X31*R + 64*X18*X19
     +    *X23*X25 + 64*X18*X25*X31*R - 64*X25*X27*R )

      T1 = T1 + FK**2*DENOM3**2 * (  - 128*X1*X10*X27*R - 64*X1*X10*R*
     +    Z1 + 256*X2*X6*X10*R + 128*X2*X6*X29*R - 128*X2*X7*X27*R - 64
     +    *X2*X7*R*Z1 + 256*X2*X10*X25*R + 128*X2*X25*X29*R - 128*X3*X6
     +    *R**2 - 128*X3*X25*R**2 + 128*X6*X10*X23*R - 128*X6*R**3 - 64
     +    *X6*R**2*Z1 + 128*X10*X23*X25*R + 128*X25*X27*R**2 - 128*X25*
     +    R**3 )

      T1 = T1 + FK**2*DENOM3*DENOM4 * ( 128*X1*X19*X27*R - 256*X2*X6*
     +    X19*R - 128*X2*X19*X25*R + 256*X3*X6*X18*R + 128*X3*X6*X31*R
     +     - 128*X3*X17*X27*R + 128*X3*X18*X25*R - 256*X6*X10*X16*R -
     +    128*X6*X16*X29*R + 256*X6*X18*R**2 - 128*X6*X19*X23*R + 128*
     +    X6*X31*R**2 + 128*X7*X16*X27*R - 128*X10*X16*X25*R - 128*X17*
     +    X27*R**2 + 128*X18*X25*R**2 )

      T1 = T1 + FK**2*DENOM4**2 * (  - 128*X3*X6*R + 256*X6*X16*X19*R
     +     - 128*X6*R**2 )

      T1 = T1 + FK*DENOM3*E11T * ( 64*X1*X10*R + 64*X2*X7*R - 64*X3*X6*
     +    R - 64*X3*X25*R - 64*X6*R**2 - 128*X25*R**2 )

      T1 = T1 + FK*DENOM3*E22 * (  - 32*X1*X19*X31*R + 64*X2*X17*X19*R
     +     - 64*X3*X17*X18*R - 32*X3*X25*R - 32*X7*X16*X31*R + 64*X10*
     +    X16*X17*R + 32*X16*X17*X29*R + 64*X16*X19*X25*R - 64*X17*X18*
     +    R**2 + 32*X17*X19*X23*R - 32*X25*R**2 )

      T1 = T1 + FK*DENOM3*E44 * (  - 32*X1*X19*X32*R + 64*X2*X19*X24*R
     +     + 32*X3*X17*X32*R - 64*X3*X18*X24*R - 32*X3*X24*X31*R - 32*
     +    X3*X25*X30*R - 32*X7*X16*X32*R + 64*X10*X16*X24*R + 32*X16*
     +    X24*X29*R + 32*X16*X25*X28*R + 32*X17*X32*R**2 - 64*X18*X24*
     +    R**2 + 32*X19*X22*X25*R + 32*X19*X23*X24*R - 32*X24*X31*R**2
     +     - 32*X25*X30*R**2 )

      T1 = T1 + FK*DENOM3*EVV * (  - 32*X1*X19*R*Z1 + 32*X1*X29*X31*R
     +     - 32*X7*X16*R*Z1 + 32*X7*X23*X31*R + 32*X16*X25*X29*R - 64*
     +    X17*X23*X29*R - 64*X17*R**2*Z1 + 32*X19*X23*X25*R + 64*X25*
     +    X31*R**2 )

      T1 = T1 + FK*DENOM3*E22M * ( 32*X16*R - 32*X19*R )

      T1 = T1 + FK*DENOM3*E44M * ( 32*X16*R - 32*X19*R )

      T1 = T1 + FK*DENOM3*EVVM * (  - 64*X2*R + 64*X10*R - 32*X23*R +
     +    32*X29*R )

      T1 = T1 + FK*DENOM4*E11T * (  - 64*X1*X19*R + 64*X3*X17*R - 64*X7
     +    *X16*R + 64*X17*R**2 )

      T1 = T1 + FK*DENOM4*E22 * ( 64*X3*X17*R - 128*X16*X17*X19*R + 64*
     +    X17*R**2 )

      T1 = T1 + FK*DENOM4*E44 * ( 64*X3*X24*R - 128*X16*X19*X24*R + 64*
     +    X24*R**2 )

      T1 = T1 + FK*DENOM4*EVVM * ( 64*X16*R - 64*X19*R )

      T1 = T1 + E12 * ( 32*X1*X18*X19 + 32*X2*X17*X19 + 32*X3*X6 - 64*
     +    X3*X17*X18 - 64*X6*X16*X19 + 32*X6*R + 32*X7*X16*X18 + 32*X10
     +    *X16*X17 - 64*X17*X18*R )

      T1 = T1 + E14 * ( 32*X1*X19*X26 + 32*X2*X19*X24 + 32*X3*X6*X30 -
     +    32*X3*X17*X26 - 32*X3*X18*X24 - 32*X6*X16*X28 - 32*X6*X19*X22
     +     + 32*X6*X30*R + 32*X7*X16*X26 + 32*X10*X16*X24 - 32*X17*X26*
     +    R - 32*X18*X24*R )

      T1 = T1 + E1V * (  - 32*X1*X18*X29 + 32*X1*X19*X27 + 32*X2*X17*
     +    X29 - 32*X2*X19*X25 + 32*X7*X16*X27 - 32*X7*X18*X23 - 32*X10*
     +    X16*X25 + 32*X10*X17*X23 + 64*X17*X27*R - 64*X18*X25*R )

      T1 = T1 + E24 * ( 32*X3*X6*X30 - 32*X3*X17*X26 - 32*X3*X18*X24 -
     +    64*X6*X16*X19*X30 + 32*X6*X30*R + 64*X16*X17*X19*X26 + 64*X16
     +    *X18*X19*X24 - 32*X17*X26*R - 32*X18*X24*R )

      T1 = T1 + E2V * (  - 32*X1*X18*X19*X31 + 32*X1*X19*X27 + 32*X2*
     +    X17*X19*X31 - 32*X2*X19*X25 - 32*X7*X16*X18*X31 + 32*X7*X16*
     +    X27 + 32*X10*X16*X17*X31 - 32*X10*X16*X25 - 64*X16*X17*X19*
     +    X27 + 64*X16*X18*X19*X25 )

      T1 = T1 + E4V * (  - 32*X1*X18*X19*X32 + 32*X1*X19*X27*X30 + 32*
     +    X2*X17*X19*X32 - 32*X2*X19*X25*X30 - 32*X7*X16*X18*X32 + 32*
     +    X7*X16*X27*X30 + 32*X10*X16*X17*X32 - 32*X10*X16*X25*X30 - 32
     +    *X16*X17*X27*X28 + 32*X16*X18*X25*X28 - 32*X17*X19*X22*X27 +
     +    32*X18*X19*X22*X25 )

      T1 = T1 + E12M * (  - 32*X16 + 32*X19 )

      T1 = T1 + E1VM * (  - 32*X1 + 32*X2 + 32*X7 - 32*X10 )

      T1 = T1 + E2VM * (  - 32*X16*X17 + 32*X16*X18 + 32*X17*X19 - 32*
     +    X18*X19 )

      T1 = T1 + E4VM * (  - 32*X16*X24 + 32*X16*X26 + 32*X19*X24 - 32*
     +    X19*X26 )
C
C THE INTERFERENCE TERMS
C THE INTERNAL BREMSSTRAHLUNG PIECE, HERE THERE CAN BE NO EPSILON TERMS
      TINT1 = 0.0

      TINT1 = TINT1 + FK**2*DENOM1*DENOM3 * ( 64*X1*X2*X10*R + 64*X1*X2
     +    *X29*R + 64*X1*X2*R**2 - 64*X1*X3*R**2 - 32*X1*X10*R**2 - 32*
     +    X1*X14*R**2 - 32*X1*X34*R**2 - 32*X1*R**3 - 64*X2**2*X7*R +
     +    64*X2*X3*X6*R + 64*X2*X3*X25*R + 64*X2*X6*X14*R + 64*X2*X6*
     +    R**2 - 64*X2*X7*X12*R - 64*X2*X7*X23*R - 64*X2*X7*X34*R + 32*
     +    X2*X7*R**2 + 64*X2*X9*X10*R + 64*X2*X9*X29*R + 64*X2*X9*R**2
     +     + 64*X2*X14*X25*R + 64*X2*X25*R**2 - 32*X3*X6*R**2 - 32*X3*
     +    X9*R**2 - 64*X3*X25*R**2 + 32*X5*X7*R**2 + 32*X5*X25*R**2 -
     +    64*X6*X10*R**2 - 32*X6*X29*R**2 - 32*X6*X34*R**2 - 32*X6*R**3
     +     + 32*X7*X27*R**2 + 32*X7*X34*R**2 + 32*X7*R**3 - 64*X9*X10*
     +    R**2 + 32*X9*X23*R**2 + 32*X9*X27*R**2 - 32*X9*X29*R**2 - 32*
     +    X9*R**3 - 32*X10*X25*R**2 + 32*X12*X25*R**2 - 32*X14*X25*R**2
     +     - 32*X25*R**3 )


      TINT1 = TINT1 + FK**2*DENOM1*DENOM4 * ( 64*X1*X3*X18*R - 64*X1*
     +    X10*X16*R - 32*X1*X10*X21*R + 32*X1*X12*X19*R + 32*X1*X14*X18
     +    *R - 64*X1*X16*R**2 + 32*X1*X18*R**2 + 32*X1*X19*R**2 - 64*X2
     +    *X3*X17*R + 64*X2*X7*X16*R + 32*X2*X7*X21*R - 32*X2*X9*X19*R
     +     - 32*X2*X14*X17*R - 32*X2*X17*R**2 + 32*X3*X6*X21*R + 32*X3*
     +    X9*X18*R - 32*X3*X12*X17*R + 32*X3*X17*R**2 - 32*X5*X6*X19*R
     +     - 32*X5*X7*X18*R + 32*X5*X10*X17*R - 32*X6*X14*X16*R - 32*X6
     +    *X16*R**2 + 32*X6*X19*R**2 + 32*X6*X21*R**2 + 32*X7*X12*X16*R
     +     - 32*X7*X16*R**2 - 32*X7*X18*R**2 - 32*X9*X10*X16*R - 64*X9*
     +    X16*R**2 + 32*X9*X18*R**2 + 64*X9*X19*R**2 + 32*X10*X17*R**2
     +     - 32*X12*X17*R**2 + 32*X17*R**3 )

      TINT1 = TINT1 + FK**2*DENOM2*DENOM3 * ( 32*X1*X10*X31*R - 32*X1*
     +    X18*X29*R - 32*X1*X18*R**2 - 32*X1*X19*X27*R + 32*X1*X19*R**2
     +     + 32*X1*X31*R**2 + 64*X2*X7*X18*R + 32*X2*X7*X31*R - 64*X2*
     +    X10*X17*R - 32*X2*X17*X29*R - 32*X2*X17*R**2 - 32*X2*X19*X25*
     +    R - 64*X3*X6*X18*R - 32*X3*X6*X31*R + 32*X3*X17*X27*R + 32*X3
     +    *X17*R**2 - 32*X3*X18*X25*R + 64*X6*X10*X16*R + 32*X6*X16*X29
     +    *R + 32*X6*X16*R**2 - 64*X6*X18*R**2 + 32*X6*X19*X23*R + 32*
     +    X6*X19*R**2 - 32*X7*X16*X27*R - 32*X7*X16*R**2 + 32*X7*X18*
     +    X23*R - 32*X7*X18*R**2 + 32*X10*X16*X25*R - 32*X10*X17*X23*R
     +     + 32*X10*X17*R**2 + 32*X16*X25*R**2 - 32*X17*X23*R**2 + 32*
     +    X17*R**3 - 64*X18*X25*R**2 + 64*X19*X25*R**2 )

      TINT1 = TINT1 + FK**2*DENOM2*DENOM4 * ( 32*X1*X10*R - 64*X1*X18*
     +    X19*R + 32*X1*R**2 - 32*X2*X7*R + 64*X2*X17*X19*R + 32*X3*X6*
     +    R - 64*X6*X16*X19*R + 32*X6*R**2 + 32*X7*R**2 - 64*X17*X19*
     +    R**2 )
C
C PART WITH FORMFACTORS IN FIRST PART AND IB IN 2ND
      F1 = DBLE(E1)
      F2 = DBLE(E2)
      F4 = DBLE(E4)
      FV = DBLE(V1)
      C1 = AIMAG(E1)
      C2 = AIMAG(E2)
      C4 = AIMAG(E4)
      CV = AIMAG(V1)
      TINT2 = 0.0
      TINT2 = TINT2 + C1*FK*DENOM3*EPSI * (  - 96*R )

      TINT2 = TINT2 + F1*FK*DENOM3 * ( 32*X1*X10*R - 32*X1*X27*R + 32*
     +    X1*X29*R + 64*X2*X6*R + 32*X2*X7*R + 32*X2*X25*R - 32*X3*X6*R
     +     - 32*X3*X25*R - 64*X6*X10*R + 32*X6*X23*R - 32*X6*X29*R - 32
     +    *X6*R**2 - 32*X7*X23*R + 32*X7*X27*R - 32*X10*X25*R - 64*X25*
     +    R**2 )

      TINT2 = TINT2 + C1*FK*DENOM4*EPSI * ( 32*R )

      TINT2 = TINT2 + F1*FK*DENOM4 * (  - 32*X1*X19*R + 32*X3*X17*R -
     +    64*X6*X16*R + 64*X6*X19*R - 32*X7*X16*R + 32*X17*R**2 )

      TINT2 = TINT2 + C2*FK*DENOM3*EPSI * ( 32*X17*R + 32*X19*R - 32*R
     +     )

      TINT2 = TINT2 + F2*FK*DENOM3 * ( 16*X1*X10*R - 32*X1*X18**2*R -
     +    32*X1*X18*X31*R + 32*X1*X19*X31*R + 16*X1*R**2 + 16*X2*X7*R
     +     + 32*X2*X17*X18*R - 32*X2*X17*X19*R - 16*X3*X6*R + 32*X3*X17
     +    *X18*R - 32*X6*X10*R + 32*X6*X16*X18*R + 32*X6*X18*X19*R - 16
     +    *X6*X29*R - 16*X6*R**2 - 32*X7*X18**2*R - 32*X7*X18*X31*R +
     +    16*X7*X27*R + 16*X7*R**2 - 32*X10*X16*X17*R + 32*X10*X17*X18*
     +    R - 16*X10*X25*R - 32*X16*X17*R**2 + 32*X16*X18*X25*R - 32*
     +    X16*X19*X25*R + 32*X17*X18*X23*R + 32*X17*X18*X29*R + 32*X17*
     +    X18*R**2 - 32*X17*X19*X23*R - 32*X17*X19*R**2 + 32*X18*X19*
     +    X25*R - 16*X25*R**2 )

      TINT2 = TINT2 + C2*FK*DENOM4*EPSI * ( 16*R )

      TINT2 = TINT2 + F2*FK*DENOM4 * ( 16*X1*X18*R - 16*X1*X19*R - 16*
     +    X2*X17*R - 16*X3*X17*R - 16*X6*X16*R + 16*X6*X19*R - 16*X7*
     +    X16*R + 16*X7*X18*R - 16*X10*X17*R + 64*X16*X17*X19*R - 16*
     +    X17*R**2 )

      TINT2 = TINT2 + C4*FK*DENOM3*EPSI * (  - 16*X4*R - 16*X6*R + 32*
     +    X10*R + 16*X11*R - 32*X18*R + 16*X19*R + 16*X29*R - 16*X31*R
     +     + 16*R**2 )

      TINT2 = TINT2 + F4*FK*DENOM3 * ( 16*X1*X10*X20*R - 32*X1*X11*X18*
     +    R + 16*X1*X11*X19*R - 16*X1*X13*X18*R - 32*X1*X18*X36*R + 32*
     +    X1*X19*X36*R + 16*X1*X20*R**2 + 16*X2*X7*X20*R + 32*X2*X8*X18
     +    *R - 16*X2*X8*X19*R - 16*X2*X13*X17*R - 16*X3*X6*X20*R + 16*
     +    X3*X8*X18*R + 16*X3*X11*X17*R + 32*X4*X6*X18*R - 16*X4*X6*X19
     +    *R + 16*X4*X7*X18*R - 16*X4*X10*X17*R - 16*X4*X17*R**2 + 32*
     +    X4*X18*X25*R - 32*X4*X19*X25*R - 32*X6*X10*X20*R + 16*X6*X13*
     +    X16*R + 32*X6*X13*X18*R + 16*X6*X13*X31*R - 16*X6*X19*X36*R
     +     - 16*X6*X20*X29*R - 16*X6*X20*R**2 - 16*X7*X11*X16*R - 32*X7
     +    *X11*X18*R - 16*X7*X11*X31*R - 16*X7*X18*X36*R + 16*X7*X20*
     +    X27*R + 16*X7*X20*R**2 - 16*X8*X10*X16*R - 16*X8*X10*X31*R -
     +    16*X8*X16*R**2 + 32*X8*X18*X23*R + 16*X8*X18*X29*R + 16*X8*
     +    X18*R**2 - 32*X8*X19*X23*R + 16*X8*X19*X27*R - 16*X8*X19*R**2
     +     - 16*X8*X31*R**2 + 32*X10*X11*X17*R + 16*X10*X17*X36*R - 16*
     +    X10*X20*X25*R + 16*X11*X17*X29*R + 16*X11*X17*R**2 + 16*X11*
     +    X19*X25*R - 16*X13*X17*X27*R - 16*X13*X17*R**2 + 16*X13*X18*
     +    X25*R )

      TINT2 = TINT2 + F4*FK*DENOM3 * ( 16*X17*X36*R**2 - 16*X20*X25*
     +    R**2 )

      TINT2 = TINT2 + C4*FK*DENOM4*EPSI * (  - 32*X19*R + 16*R )

      TINT2 = TINT2 + F4*FK*DENOM4 * ( 16*X1*X11*R + 16*X1*X13*R - 32*
     +    X1*X19*X20*R - 16*X2*X8*R - 16*X3*X8*R - 16*X4*X6*R - 16*X4*
     +    X7*R + 32*X4*X17*X19*R - 16*X6*X13*R + 32*X6*X19*X20*R + 16*
     +    X7*X11*R - 16*X8*X10*R + 32*X8*X16*X19*R + 32*X8*X18*X19*R -
     +    16*X8*R**2 - 32*X11*X17*X19*R )

      TINT2 = TINT2 + CV*FK*DENOM3*EPSI * (  - 64*X1*R - 32*X2*R + 96*
     +    X6*R + 32*X7*R - 64*X23*R + 64*X25*R + 32*X27*R - 32*R**2 )

      TINT2 = TINT2 + FV*FK*DENOM3 * (  - 32*X2*X9*X18*R - 32*X2*X9*X19
     +    *R - 32*X2*X9*X31*R + 32*X2*X12*X17*R + 32*X2*X14*X17*R + 32*
     +    X2*X17*X34*R + 32*X3*X9*X18*R + 32*X3*X9*X31*R - 32*X3*X12*
     +    X17*R - 32*X3*X17*X34*R + 32*X5*X6*X18*R + 32*X5*X6*X31*R -
     +    32*X5*X7*X31*R + 32*X5*X10*X17*R - 32*X5*X17*X27*R + 32*X5*
     +    X17*X29*R - 32*X6*X12*X16*R - 32*X6*X12*X19*R + 32*X6*X14*X18
     +    *R - 32*X6*X16*X34*R + 32*X7*X12*X31*R + 32*X7*X16*X34*R - 32
     +    *X7*X18*X34*R - 32*X9*X10*X16*R + 32*X9*X10*X18*R + 32*X9*X16
     +    *X27*R - 32*X9*X16*X29*R + 32*X9*X18*X29*R + 32*X9*X18*R**2
     +     + 32*X9*X31*R**2 - 32*X10*X12*X17*R - 32*X12*X17*X29*R - 32*
     +    X12*X17*R**2 - 32*X12*X19*X25*R + 32*X14*X18*X25*R - 32*X17*
     +    X34*R**2 )

      TINT2 = TINT2 + CV*FK*DENOM4*EPSI * ( 32*X16*R - 32*X17*R - 32*
     +    X19*R )

      TINT2 = TINT2 + FV*FK*DENOM4 * (  - 32*X3*X9*R + 32*X3*X17*X21*R
     +     - 32*X5*X6*R - 32*X5*X17*X19*R - 32*X6*X14*R + 32*X6*X16*X21
     +    *R + 32*X6*X19*X21*R + 32*X9*X16*X18*R + 64*X9*X16*X19*R - 32
     +    *X9*X18*X19*R - 32*X9*R**2 - 32*X12*X16*X17*R + 32*X12*X17*
     +    X19*R - 32*X14*X16*X17*R + 32*X17*X21*R**2 )

C
C PART WITH FORMFACTORS IN 2ND PART AND IB IN FIRST
      F1 = DBLE(E11)
      F2 = DBLE(E21)
      F4 = DBLE(E41)
      FV = DBLE(V11)
      C1 =-AIMAG(E11)
      C2 =-AIMAG(E21)
      C4 =-AIMAG(E41)
      CV =-AIMAG(V11)
      TINT3 = 0.0
      TINT3 = TINT3 + C1  *FK*DENOM1*EPSI * (  - 96*R )

      TINT3 = TINT3 + F1  *FK*DENOM1 * ( 64*X1*X2*R - 64*X1*X3*R - 32*
     +    X1*X10*R + 32*X1*X12*R - 32*X1*X14*R - 32*X1*R**2 + 32*X2*X7*
     +    R + 32*X2*X9*R + 32*X3*X6*R - 32*X3*X9*R - 32*X5*X6*R + 32*X5
     +    *X7*R + 32*X6*X14*R - 32*X7*X12*R - 32*X9*X10*R - 64*X9*R**2
     +     )

      TINT3 = TINT3 +   C1*FK*DENOM2*EPSI * ( 32*R )

      TINT3 = TINT3 +   F1*FK*DENOM2 * (  - 64*X1*X18*R + 64*X1*X19*R
     +     - 32*X6*X19*R - 32*X7*X18*R + 32*X10*X17*R + 32*X17*R**2 )

      TINT3 = TINT3 +   C2*FK*DENOM1*EPSI * ( 32*X17*R + 32*X19*R - 32*
     +    R )

      TINT3 = TINT3 +   F2*FK*DENOM1 * (  - 32*X1*X3*R - 16*X1*X10*R -
     +    16*X1*X14*R + 32*X1*X16*X18*R + 32*X1*X16*X19*R - 16*X1*R**2
     +     + 16*X2*X7*R + 32*X2*X16*X17*R - 32*X2*X17*X19*R + 16*X3*X6*
     +    R - 16*X3*X9*R + 32*X3*X16*X17*R - 32*X3*X17*X18*R + 16*X5*X7
     +    *R - 32*X6*X16**2*R - 32*X6*X16*X21*R + 32*X6*X19*X21*R + 16*
     +    X6*R**2 - 32*X7*X16**2*R - 32*X7*X16*X21*R + 16*X7*R**2 + 32*
     +    X9*X16*X18*R + 32*X9*X16*X19*R - 32*X9*X18*X19*R - 16*X9*R**2
     +     + 32*X10*X16*X17*R + 32*X12*X16*X17*R - 32*X12*X17*X19*R +
     +    32*X14*X16*X17*R + 32*X16*X17*R**2 - 32*X17*X18*R**2 - 32*X17
     +    *X19*R**2 )

      TINT3 = TINT3 +   C2*FK*DENOM2*EPSI * ( 16*R )

      TINT3 = TINT3 +   F2*FK*DENOM2 * (  - 16*X1*X18*R + 16*X1*X19*R
     +     - 16*X2*X17*R - 16*X3*X17*R + 16*X6*X16*R - 16*X6*X19*R + 16
     +    *X7*X16*R - 16*X7*X18*R - 16*X10*X17*R + 64*X17*X18*X19*R -
     +    16*X17*R**2 )

      TINT3 = TINT3 +   C4*FK*DENOM1*EPSI * ( 16*X2*R + 32*X3*R + 16*
     +    X14*R - 32*X16*R + 16*X17*R - 16*X26*R - 16*X30*R + 16*R**2 )

      TINT3 = TINT3 +   F4*FK*DENOM1 * (  - 32*X1*X3*X30*R - 16*X1*X10*
     +    X30*R - 16*X1*X14*X30*R + 32*X1*X16*X26*R + 32*X1*X16*X28*R
     +     + 16*X1*X18*X28*R - 16*X1*X19*X26*R - 16*X1*X19*X33*R + 16*
     +    X1*X21*X28*R - 16*X1*X30*R**2 + 16*X2*X7*X30*R + 32*X2*X16*
     +    X24*R - 16*X2*X17*X28*R - 16*X2*X19*X24*R + 16*X3*X6*X30*R -
     +    16*X3*X9*X30*R + 32*X3*X17*X22*R - 16*X3*X17*X26*R + 16*X3*
     +    X17*X33*R - 16*X3*X18*X24*R - 16*X3*X21*X24*R + 16*X5*X7*X30*
     +    R - 16*X5*X17*X28*R + 16*X5*X19*X24*R - 32*X6*X16*X22*R - 16*
     +    X6*X16*X28*R - 32*X6*X16*X33*R + 16*X6*X19*X22*R + 32*X6*X19*
     +    X33*R + 16*X6*X30*R**2 - 32*X7*X16*X22*R + 16*X7*X16*X26*R -
     +    16*X7*X16*X33*R - 16*X7*X18*X22*R - 16*X7*X21*X22*R + 16*X7*
     +    X30*R**2 + 32*X9*X16*X26*R + 16*X9*X16*X28*R + 16*X9*X19*X22*
     +    R - 32*X9*X19*X26*R - 16*X9*X30*R**2 + 16*X10*X16*X24*R + 16*
     +    X10*X17*X22*R + 32*X12*X16*X24*R - 32*X12*X19*X24*R + 16*X14*
     +    X16*X24*R + 16*X14*X17*X22*R + 16*X16*X24*R**2 + 16*X17*X22*
     +    R**2 - 16*X17*X26*R**2 - 16*X17*X28*R**2 + 16*X17*X33*R**2 -
     +    16*X18*X24*R**2 )

      TINT3 = TINT3 +   F4*FK*DENOM1 * (  - 16*X19*X24*R**2 - 16*X21*
     +    X24*R**2 )

      TINT3 = TINT3 +   C4*FK*DENOM2*EPSI * (  - 32*X19*R + 16*R )

      TINT3 = TINT3 +   F4*FK*DENOM2 * ( 32*X1*X19*X30*R - 16*X1*X26*R
     +     - 16*X1*X28*R - 16*X2*X24*R - 16*X3*X24*R - 32*X6*X19*X30*R
     +     + 16*X6*X22*R + 16*X6*X28*R + 16*X7*X22*R - 16*X7*X26*R - 16
     +    *X10*X24*R + 32*X16*X19*X24*R - 32*X17*X19*X22*R + 32*X17*X19
     +    *X26*R + 32*X18*X19*X24*R - 16*X24*R**2 )

      TINT3 = TINT3 +   CV*FK*DENOM1*EPSI * ( 96*X1*R - 32*X2*R + 32*X5
     +    *R - 64*X6*R + 32*X7*R + 64*X9*R - 64*X12*R - 32*R**2 )

      TINT3 = TINT3 +   FV*FK*DENOM1 * ( 32*X1*X16*X27*R + 32*X1*X16*
     +    X29*R - 32*X1*X18*X23*R - 32*X1*X18*X34*R - 32*X1*X19*X23*R
     +     + 32*X1*X21*X27*R - 32*X2*X16*X25*R + 32*X2*X17*X23*R + 32*
     +    X2*X17*X29*R + 32*X2*X17*X34*R - 32*X2*X19*X25*R - 32*X2*X21*
     +    X25*R + 32*X3*X16*X25*R - 32*X3*X17*X23*R + 32*X3*X17*X27*R
     +     - 32*X3*X18*X25*R - 32*X5*X17*X27*R + 32*X5*X18*X25*R - 32*
     +    X7*X16*X34*R + 32*X7*X18*X34*R + 32*X7*X21*X23*R - 32*X7*X21*
     +    X27*R + 32*X9*X16*X29*R - 32*X9*X19*X23*R + 32*X10*X16*X25*R
     +     - 32*X10*X17*X23*R - 32*X10*X17*X34*R + 32*X10*X21*X25*R +
     +    32*X14*X16*X25*R - 32*X14*X17*X23*R + 32*X14*X17*X27*R - 32*
     +    X14*X18*X25*R + 32*X16*X25*R**2 - 32*X17*X23*R**2 - 32*X17*
     +    X34*R**2 + 32*X21*X25*R**2 )

      TINT3 = TINT3 +   CV*FK*DENOM2*EPSI * (  - 32*X17*R + 32*X18*R -
     +    32*X19*R )

      TINT3 = TINT3 +   FV*FK*DENOM2 * ( 32*X1*X18*X31*R + 32*X1*X19*
     +    X31*R - 32*X1*X27*R - 32*X1*X29*R + 32*X10*X17*X31*R - 32*X10
     +    *X25*R + 32*X16*X18*X25*R - 32*X16*X19*X25*R - 32*X17*X18*X23
     +    *R - 32*X17*X18*X29*R + 32*X17*X19*X23*R - 32*X17*X19*X27*R
     +     + 32*X17*X31*R**2 + 64*X18*X19*X25*R - 32*X25*R**2 )
C
C THE PART WHERE BOTH HAVE FORMFACTORS
      E1E1 = DBLE (E1*E1C1)
      E1E1C= AIMAG(E1*E1C1)
      E1E2 = DBLE (E1*E2C1)
      E1E2C= AIMAG(E1*E2C1)
      E1E4 = DBLE (E1*E4C1)
      E1E4C= AIMAG(E1*E4C1)
      E1V1 = DBLE (E1*V1C1)
      E1V1C= AIMAG(E1*V1C1)
      E2E1 = DBLE (E2*E1C1)
      E2E1C= AIMAG(E2*E1C1)
      E2E2 = DBLE (E2*E2C1)
      E2E2C= AIMAG(E2*E2C1)
      E2E4 = DBLE (E2*E4C1)
      E2E4C= AIMAG(E2*E4C1)
      E2V1 = DBLE (E2*V1C1)
      E2V1C= AIMAG(E2*V1C1)
      E4E1 = DBLE (E4*E1C1)
      E4E1C= AIMAG(E4*E1C1)
      E4E2 = DBLE (E4*E2C1)
      E4E2C= AIMAG(E4*E2C1)
      E4E4 = DBLE (E4*E4C1)
      E4E4C= AIMAG(E4*E4C1)
      E4V1 = DBLE (E4*V1C1)
      E4V1C= AIMAG(E4*V1C1)
      V1E1 = DBLE (V1*E1C1)
      V1E1C= AIMAG(V1*E1C1)
      V1E2 = DBLE (V1*E2C1)
      V1E2C= AIMAG(V1*E2C1)
      V1E4 = DBLE (V1*E4C1)
      V1E4C= AIMAG(V1*E4C1)
      V1V1 = DBLE (V1*V1C1)
      V1V1C= AIMAG(V1*V1C1)
      TINT4 = 0
      TINT4 = TINT4 + E1E1   * ( 32*X1*R + 64*X2*X7 + 32*X6*R - 32*X7*
     +    R )

      TINT4 = TINT4 + E1E2C *EPSI * ( 16 - 32*X16 - 32*X18 )

      TINT4 = TINT4 + E1E2   * (  - 16*X1*X10 + 32*X1*X18*X19 + 16*X2*
     +    X7 + 16*X3*X6 - 32*X3*X17*X18 - 32*X6*X16*X19 + 16*X6*R + 32*
     +    X10*X16*X17 + 32*X16*X17*R - 32*X17*X18*R - 32*X17*X19*R )

      TINT4 = TINT4 + E1E4C *EPSI * ( 16*X2 - 16*X7 + 16*X17 - 16*X22
     +     - 16*X26 )

      TINT4 = TINT4 + E1E4   * (  - 16*X1*X10*X30 + 16*X1*X18*X28 + 16
     +    *X1*X19*X26 + 16*X2*X7*X30 - 16*X2*X17*X28 + 16*X2*X19*X24 +
     +    16*X3*X6*X30 - 16*X3*X17*X26 - 16*X3*X18*X24 - 16*X6*X16*X28
     +     - 16*X6*X19*X22 + 16*X6*X30*R + 16*X7*X16*X26 - 16*X7*X18*
     +    X22 + 16*X10*X16*X24 + 16*X10*X17*X22 + 32*X16*X24*R - 16*X17
     +    *X26*R - 16*X18*X24*R - 32*X19*X24*R )

      TINT4 = TINT4 + E1V1C *EPSI * ( 32*X2 + 32*X7 - 64*R )

      TINT4 = TINT4 + E1V1   * ( 32*X2*X17*X29 - 32*X2*X19*X25 + 32*X7
     +    *X16*X27 - 32*X7*X18*X23 + 32*X17*X27*R - 32*X18*X25*R )

      TINT4 = TINT4 + E2E1C *EPSI * ( 16 - 32*X16 - 32*X18 )

      TINT4 = TINT4 + E2E1   * ( 16*X1*X10 - 32*X1*X18*X19 + 16*X1*R
     +     + 16*X2*X7 - 16*X3*X6 + 32*X3*X17*X18 + 32*X6*X16*X19 - 32*
     +    X10*X16*X17 - 32*X16*X17*R + 32*X17*X18*R - 32*X17*X19*R )

      TINT4 = TINT4 + E2E2C *EPSI * ( 8 - 16*X16 - 16*X18 )

      TINT4 = TINT4 + E2E2   * (  - 8*X1*X10 + 16*X1*X18*X19 - 8*X1*R
     +     + 8*X2*X7 - 8*X3*X6 + 16*X3*X17*X18 + 16*X6*X16*X19 - 8*X6*R
     +     - 8*X7*R + 16*X10*X16*X17 - 64*X16*X17*X18*X19 + 16*X16*X17*
     +    R + 16*X17*X18*R )

      TINT4 = TINT4 + E2E4C *EPSI * (  - 8*X1 + 8*X10 - 8*X18 + 8*X22
     +     + 8*X24 )

      TINT4 = TINT4 + E2E4   * (  - 8*X1*X10*X30 + 8*X1*X18*X28 + 8*X1
     +    *X19*X26 - 8*X1*X30*R + 8*X2*X7*X30 - 8*X2*X17*X28 + 8*X2*X19
     +    *X24 - 8*X3*X6*X30 + 8*X3*X17*X26 + 8*X3*X18*X24 + 32*X6*X16*
     +    X19*X30 - 8*X6*X16*X28 - 8*X6*X19*X22 - 8*X6*X30*R + 8*X7*X16
     +    *X26 - 8*X7*X18*X22 - 8*X7*X30*R + 8*X10*X16*X24 + 8*X10*X17*
     +    X22 - 32*X16*X17*X19*X26 - 32*X16*X18*X19*X24 + 8*X16*X24*R
     +     + 8*X17*X22*R + 8*X17*X26*R + 8*X17*X28*R + 8*X18*X24*R - 8*
     +    X19*X24*R )

      TINT4 = TINT4 + E2V1C *EPSI * (  - 16*X1 - 16*X10 + 32*X16*X17
     +     + 32*X18*X19 - 16*R )

      TINT4 = TINT4 + E2V1   * ( 32*X1*X18*X19*X31 - 16*X1*X18*X29 -
     +    16*X1*X19*X27 - 32*X10*X16*X17*X31 + 16*X10*X16*X25 + 16*X10*
     +    X17*X23 + 32*X16*X17*X18*X29 + 32*X16*X17*X19*X27 - 32*X16*
     +    X17*X31*R - 32*X16*X18*X19*X25 + 16*X16*X25*R - 32*X17*X18*
     +    X19*X23 - 32*X17*X19*X31*R + 16*X17*X23*R + 16*X17*X27*R + 16
     +    *X17*X29*R - 16*X18*X25*R + 16*X19*X25*R )

      TINT4 = TINT4 + E4E1C *EPSI * (  - 16*X4 + 16*X8 )

      TINT4 = TINT4 + E4E1   * ( 16*X1*X10*X20 - 16*X1*X11*X19 - 16*X1
     +    *X13*X18 + 16*X1*X20*R + 16*X2*X7*X20 + 16*X2*X8*X19 - 16*X2*
     +    X13*X17 - 16*X3*X6*X20 + 16*X3*X8*X18 + 16*X3*X11*X17 + 16*X4
     +    *X6*X19 + 16*X4*X7*X18 - 16*X4*X10*X17 - 16*X4*X17*R + 16*X6*
     +    X13*X16 - 16*X7*X11*X16 - 16*X8*X10*X16 - 16*X8*X16*R + 32*X8
     +    *X18*R - 32*X8*X19*R )

      TINT4 = TINT4 + E4E2C *EPSI * ( 8*X3 - 8*X4 - 8*X6 - 8*X13 + 8*
     +    X17 )

      TINT4 = TINT4 + E4E2   * (  - 8*X1*X10*X20 - 8*X1*X11*X19 - 8*X1
     +    *X13*X18 + 32*X1*X18*X19*X20 - 8*X1*X20*R + 8*X2*X7*X20 + 8*
     +    X2*X8*X19 - 8*X2*X13*X17 - 8*X3*X6*X20 + 8*X3*X8*X18 + 8*X3*
     +    X11*X17 + 8*X4*X6*X19 + 8*X4*X7*X18 + 8*X4*X10*X17 - 32*X4*
     +    X17*X18*X19 + 8*X4*X17*R + 8*X6*X13*X16 - 8*X6*X20*R - 8*X7*
     +    X11*X16 - 8*X7*X20*R + 8*X8*X10*X16 - 32*X8*X16*X18*X19 + 8*
     +    X8*X16*R + 8*X8*X18*R - 8*X8*X19*R + 8*X11*X17*R + 8*X13*X17*
     +    R )

      TINT4 = TINT4 + E4E4C *EPSI * ( 16*X6*X19 - 8*X6 - 16*X19*X24 +
     +    16*X19*R + 8*X24 - 8*R )

      TINT4 = TINT4 + E4E4   * (  - 8*X1*X10*X35 - 16*X1*X11*X19*X30
     +     + 8*X1*X11*X28 - 8*X1*X13*X26 + 16*X1*X18*X19*X35 + 16*X1*
     +    X19*X20*X26 - 8*X1*X35*R + 8*X2*X7*X35 + 16*X2*X8*X19*X30 - 8
     +    *X2*X8*X28 - 8*X2*X13*X24 - 16*X2*X17*X19*X35 + 16*X2*X19*X20
     +    *X24 - 8*X3*X6*X35 + 8*X3*X8*X26 + 8*X3*X11*X24 + 16*X4*X6*
     +    X19*X30 - 8*X4*X6*X28 + 8*X4*X7*X26 + 8*X4*X10*X24 - 16*X4*
     +    X17*X19*X26 - 16*X4*X18*X19*X24 + 8*X4*X24*R + 8*X6*X13*X22
     +     + 16*X6*X16*X19*X35 - 16*X6*X19*X20*X22 - 8*X6*X35*R - 8*X7*
     +    X11*X22 - 8*X7*X35*R + 8*X8*X10*X22 - 16*X8*X16*X19*X26 - 16*
     +    X8*X18*X19*X22 - 16*X8*X19*X30*R + 8*X8*X22*R + 8*X8*X26*R +
     +    8*X8*X28*R - 16*X11*X16*X19*X24 + 16*X11*X17*X19*X22 + 8*X11*
     +    X24*R + 8*X13*X24*R + 16*X17*X19*X35*R - 16*X19*X20*X24*R )

      TINT4 = TINT4 + E4V1C *EPSI * (  - 16*X1*X20 - 16*X2*X19 - 16*X3
     +    *X18 + 16*X4*X17 - 16*X6*X19 - 16*X7*X18 + 16*X8*X16 + 16*X10
     +    *X16 + 16*X10*X17 - 16*X10*X20 + 16*X11*X19 + 16*X13*X18 + 16
     +    *X16*R + 16*X17*R - 16*X18*R + 16*X19*R - 16*X20*R )

      TINT4 = TINT4 + E4V1   * ( 16*X1*X10*X20*X31 - 16*X1*X10*X36 +
     +    32*X1*X18*X19*X36 - 16*X1*X18*X20*X29 - 16*X1*X19*X20*X27 +
     +    16*X1*X20*X31*R - 16*X1*X36*R + 16*X2*X7*X20*X31 - 16*X2*X7*
     +    X36 - 16*X2*X13*X17*X31 + 16*X2*X13*X25 + 16*X2*X17*X19*X36
     +     - 16*X2*X19*X20*X25 - 16*X3*X6*X20*X31 + 16*X3*X6*X36 + 16*
     +    X3*X11*X17*X31 - 16*X3*X11*X25 - 16*X3*X17*X18*X36 + 16*X3*
     +    X18*X20*X25 - 16*X4*X10*X17*X31 + 16*X4*X10*X25 + 16*X4*X17*
     +    X18*X29 + 16*X4*X17*X19*X27 - 16*X4*X17*X31*R - 32*X4*X18*X19
     +    *X25 + 16*X4*X25*R + 16*X6*X13*X16*X31 - 16*X6*X13*X23 - 16*
     +    X6*X16*X19*X36 + 16*X6*X19*X20*X23 - 16*X7*X11*X16*X31 + 16*
     +    X7*X11*X23 + 16*X7*X16*X18*X36 - 16*X7*X18*X20*X23 - 16*X8*
     +    X10*X16*X31 + 16*X8*X10*X23 + 16*X8*X16*X18*X29 + 16*X8*X16*
     +    X19*X27 - 16*X8*X16*X31*R - 32*X8*X18*X19*X23 - 16*X8*X18*X31
     +    *R - 16*X8*X19*X31*R + 16*X8*X23*R + 16*X8*X27*R + 16*X8*X29*
     +    R + 16*X11*X16*X19*X25 - 16*X11*X17*X19*X23 - 16*X13*X16*X18*
     +    X25 + 16*X13*X17*X18*X23 + 16*X17*X18*X36*R - 16*X17*X19*X36*
     +    R )

      TINT4 = TINT4 + E4V1   * (  - 16*X18*X20*X25*R + 16*X19*X20*X25*
     +    R )

      TINT4 = TINT4 + V1E1C *EPSI * ( 32*X2 + 32*X7 - 64*R )

      TINT4 = TINT4 + V1E1   * (  - 32*X2*X9*X19 + 32*X2*X14*X17 + 32*
     +    X5*X7*X18 + 32*X5*X17*R - 32*X7*X12*X16 - 32*X9*X16*R )

      TINT4 = TINT4 + V1E2C *EPSI * (  - 16*X3 - 16*X6 + 32*X16*X19 +
     +    32*X17*X18 - 16*R )

      TINT4 = TINT4 + V1E2   * ( 16*X3*X9*X18 + 16*X3*X12*X17 - 32*X3*
     +    X17*X18*X21 - 16*X5*X6*X19 + 32*X5*X17*X18*X19 + 16*X5*X17*R
     +     - 16*X6*X14*X16 + 32*X6*X16*X19*X21 - 32*X9*X16*X18*X19 - 16
     +    *X9*X16*R + 16*X9*X18*R + 16*X9*X19*R - 32*X12*X16*X17*X19 +
     +    16*X12*X17*R + 32*X14*X16*X17*X18 + 16*X14*X17*R - 32*X17*X18
     +    *X21*R - 32*X17*X19*X21*R )

      TINT4 = TINT4 + V1E4C *EPSI * (  - 16*X1*X19 - 16*X2*X19 + 16*X3
     +    *X17 + 16*X3*X18 - 16*X3*X30 - 16*X6*X30 - 16*X7*X16 - 16*X10
     +    *X16 + 16*X16*X28 - 16*X16*R + 16*X17*X26 + 16*X17*R + 16*X18
     +    *X24 + 16*X18*R + 16*X19*X22 + 16*X19*R - 16*X30*R )

      TINT4 = TINT4 + V1E4   * (  - 16*X1*X10*X21*X30 + 16*X1*X10*X33
     +     + 16*X1*X12*X19*X30 - 16*X1*X12*X28 - 16*X1*X18*X19*X33 + 16
     +    *X1*X18*X21*X28 + 16*X2*X7*X21*X30 - 16*X2*X7*X33 - 16*X2*X9*
     +    X19*X30 + 16*X2*X9*X28 + 16*X2*X17*X19*X33 - 16*X2*X17*X21*
     +    X28 + 16*X3*X6*X21*X30 - 16*X3*X6*X33 + 16*X3*X9*X26 + 16*X3*
     +    X12*X24 - 16*X3*X17*X21*X26 - 16*X3*X18*X21*X24 - 16*X5*X6*
     +    X19*X30 + 16*X5*X17*X19*X26 + 16*X5*X18*X19*X24 + 16*X5*X24*R
     +     - 16*X6*X14*X16*X30 + 32*X6*X16*X19*X33 + 16*X6*X21*X30*R -
     +    16*X6*X33*R - 16*X7*X12*X16*X30 + 16*X7*X12*X22 + 16*X7*X16*
     +    X18*X33 - 16*X7*X18*X21*X22 + 16*X9*X10*X16*X30 - 16*X9*X10*
     +    X22 - 16*X9*X16*X18*X28 - 32*X9*X16*X19*X26 - 16*X9*X16*X30*R
     +     + 16*X9*X18*X19*X22 + 16*X9*X19*X30*R + 16*X9*X26*R - 16*X10
     +    *X16*X17*X33 + 16*X10*X17*X21*X22 + 16*X12*X16*X17*X28 - 32*
     +    X12*X16*X19*X24 - 16*X12*X17*X19*X22 + 16*X12*X24*R + 16*X14*
     +    X16*X17*X26 + 16*X14*X16*X18*X24 + 16*X14*X24*R + 16*X16*X17*
     +    X33*R - 16*X16*X21*X24*R - 16*X17*X19*X33*R - 16*X17*X21*X26*
     +    R )

      TINT4 = TINT4 + V1E4   * (  - 16*X18*X21*X24*R - 16*X19*X21*X24*
     +    R )

      TINT4 = TINT4 + V1V1C *EPSI * ( 16*X1*X18 + 16*X1*X19 - 16*X1*
     +    X21 - 8*X2*X17 - 8*X2*X19 + 8*X3*X17 - 8*X3*X18 - 32*X5*X17
     +     + 8*X5*X18 - 8*X6*X16 + 8*X6*X19 + 8*X7*X16 + 8*X7*X18 + 32*
     +    X9*X16 - 8*X9*X19 - 8*X12*X16 + 16*X12*X31 + 8*X14*X17 + 16*
     +    X14*X31 - 16*X14 - 16*X16*X29 + 8*X16*X34 - 16*X16*R + 16*X17
     +    *X29 - 8*X17*X34 + 48*X17*R - 16*X18*X34 + 16*X19*X21 + 16*
     +    X19*X23 - 16*X19*X25 - 16*X19*X34 - 8*X21*X23 + 8*X21*X25 -
     +    32*X21*X31 - 16*X21*R + 32*X34 )

      TINT4 = TINT4 + V1V1   * ( 16*X1*X12*X19*X31 - 16*X1*X12*X29 -
     +    16*X1*X14*X18*X31 + 16*X1*X14*X27 + 16*X1*X18*X21*X29 - 16*X1
     +    *X19*X21*X27 + 16*X2*X9*X19*X31 - 16*X2*X9*X29 + 16*X2*X14*
     +    X17*X31 - 16*X2*X14*X25 - 32*X2*X17*X19*X34 + 16*X2*X17*X21*
     +    X29 + 16*X2*X19*X21*X25 + 16*X3*X9*X18*X31 - 16*X3*X9*X27 -
     +    16*X3*X12*X17*X31 + 16*X3*X12*X25 + 16*X3*X17*X21*X27 - 16*X3
     +    *X18*X21*X25 - 16*X5*X6*X19*X31 + 16*X5*X6*X29 - 16*X5*X7*X18
     +    *X31 + 16*X5*X7*X27 + 16*X5*X10*X17*X31 - 16*X5*X10*X25 + 16*
     +    X5*X17*X31*R - 16*X5*X25*R + 16*X6*X14*X16*X31 - 16*X6*X14*
     +    X23 - 16*X6*X16*X21*X29 + 16*X6*X19*X21*X23 - 16*X7*X12*X16*
     +    X31 + 16*X7*X12*X23 + 32*X7*X16*X18*X34 - 16*X7*X16*X21*X27
     +     - 16*X7*X18*X21*X23 + 32*X7*X21*X31*R - 32*X7*X34*R - 16*X9*
     +    X10*X16*X31 + 16*X9*X10*X23 - 16*X9*X16*X31*R + 16*X9*X18*X31
     +    *R - 16*X9*X19*X31*R + 16*X9*X23*R - 16*X9*X27*R + 16*X9*X29*
     +    R + 16*X10*X16*X21*X25 - 16*X10*X17*X21*X23 - 16*X12*X17*X31*
     +    R + 16*X12*X25*R - 16*X14*X17*X31*R + 16*X14*X25*R + 16*X16*
     +    X21*X25*R )

      TINT4 = TINT4 + V1V1   * ( 32*X17*X19*X34*R - 16*X17*X21*X23*R
     +     + 16*X17*X21*X27*R - 16*X17*X21*X29*R - 16*X18*X21*X25*R -
     +    16*X19*X21*X25*R )
      CROSS0 = XNORM*T/Z**2
      CROSS5 = XNORM*T1/Z1**2
      CROSS1 = XNORM*2.0*TINT1/(Z*Z1)
      CROSS2 = XNORM*2.0*TINT2/(Z*Z1)
      CROSS3 = XNORM*2.0*TINT3/(Z*Z1)
      CROSS4 = XNORM*2.0*TINT4/(Z*Z1)
      CROSS_EQ = CROSS0 + CROSS5 + CROSS1 + CROSS2 + CROSS3 + CROSS4

      RETURN
      END

C-------------------------------------------------------
      FUNCTION CROSS_NE(P)
      IMPLICIT NONE

      REAL*8 XMK,XML,XMPI,XMLP,FK,FPI,XL9,XL10,FUNNY
      REAL*8 CROSS0,CROSS5,CROSS1,CROSS2,CROSS3,CROSS4
      INTEGER IFORM,ICUT,MCUT,IMAEL
      REAL*8 ZMIN

      COMMON/MASSES/XMK,XML,XMPI,XMLP,FK,FPI,XL9,XL10,FUNNY
      COMMON/GAMMAS/CROSS0,CROSS5,CROSS1,CROSS2,CROSS3,CROSS4
      COMMON/CHECKS/IFORM,ICUT,MCUT,IMAEL
      COMMON/CUTS/ZMIN

      INTEGER IBEGIN

      REAL*8 CROSS_NE

      REAL*8 A1I,DENOM1,DENOM2,E11M,E11T,E12,E12M,E14,E14M,E1E1C,E1V
      REAL*8 E1VM,E22,E22M,E24,E24M,E2E2C,E2V,E2VM,E44,E44M,E4E4C
      REAL*8 E4V,E4VM,EPSI,ET,EVV,EVVM,PI,R,RP,T,V1I,V1V1C,W2
      REAL*8 x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
      REAL*8 x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,xnorm
      REAL*8 Z, PROD, EPSILO

      REAL*8 P(4,100)
      COMPLEX*8 E1,E2,E4,V1,E1C,E2C,E4C,V1C
      COMPLEX*8 A1,A2,A4,FVKM1
      DATA IBEGIN/0/

      IF(IBEGIN.EQ.0) THEN
        R =(XML/XMK)**2
        RP=(XMLP/XMK)**2
        ET=1.0
        PI = 4.0 * ATAN(1.0)
        IBEGIN=1
C 137.035989**2 = 1./(ALPHA_EM)**2
        XNORM = FUNNY/(32.0* PI**6 *137.035989**2)
        V1I = -XMK/(8.0*PI**2*FPI)
        A1I= -4.0/FPI*(XL9+XL10)*XMK
      ENDIF
C
C   CALCULATING ALL RELEVANT SCALAR PRODUCTS
      X1  = PROD(P,2,1)
      X2  = PROD(P,2,3)
      X3  = PROD(P,2,4)
      X4  = X1 + R
      X5  = X2+X3
      X6  = PROD(P,1,3)
      X7  = PROD(P,1,4)
      X8  = X1
      X9  = X6 + X7
      X10 = PROD(P,3,4)
      X11 = X2 + X6
      X12 = RP + X10
      X13 = X3 + X7
      X14 = X12
      X15 = X11 + X13
      X16 = ET*P(4,2)
      X17 = ET*P(4,1)
      X18 = ET*P(4,3)
      X19 = ET*P(4,4)
      X20 = X16+X17
      X21 = X18+X19
      Z = 2.0*RP + 2.0*X10
      W2 = R + 2.0 * X1
      DENOM1 = 1.0 / (2.0*X5+Z)
      DENOM2 = 1.0 / (2.0*X21-Z)
      EPSI = -EPSILO(P)
C THE - SIGN IS BECAUSE OF THE I**2 THAT WE HAVE DROPPED
C
C CALCULATION OF THE FORMFACTORS (COMPLEX ONES)
      IF(ICUT.EQ.1) THEN
        IF (Z.LT.ZMIN) THEN
          CROSS_NE = 0.0
          RETURN
        ENDIF
      ENDIF
      IF(IFORM.EQ.1) THEN
        A1 = CMPLX(A1I)
        A2 = CMPLX(-2.0*FK/Z)* FVKM1(Z)
        A4 = CMPLX(2.0*FK*DENOM2/Z) * FVKM1(Z)
        V1 = CMPLX(V1I)
      ELSE
        A1=(0.0,0.0)
        A2=(0.0,0.0)
        A4=(0.0,0.0)
        V1=(0.0,0.0)
      ENDIF
      E1 = A1*CMPLX(X15) + A2*CMPLX(Z)
      E2 = -A1
      E4 = A1 - CMPLX(Z)*A4
      E1C = CONJG(E1)
      E2C = CONJG(E2)
      E4C = CONJG(E4)
      V1C = CONJG(V1)
C
C CALCULATION OF THE TRACE
C CALCULATION OF THE RELEVANT COMBINATIONS OF FORM FACTORS
      E1E1C = DBLE(E1*E1C)
      E2E2C = DBLE(E2*E2C)
      E4E4C = DBLE(E4*E4C)
      V1V1C = DBLE(V1*V1C)
      E11T = DBLE(E1 + E1C)
      E22 =  DBLE(E2 + E2C)
      E44 =  DBLE(E4+E4C)
      EVV =  DBLE(V1+V1C)
      E12 =  DBLE(E1*E2C + E1C*E2)
      E14 =  DBLE(E1*E4C + E1C*E4)
      E1V =  DBLE(E1*V1C + E1C*V1)
      E24 =  DBLE(E2*E4C + E2C*E4)
      E2V =  DBLE(E2*V1C + E2C*V1)
      E4V =  DBLE(E4*V1C + E4C*V1)
      E11M = EPSI * AIMAG(E1 - E1C)
      E22M = EPSI * AIMAG(E2 - E2C)
      E44M = EPSI * AIMAG(E4 - E4C)
      EVVM = EPSI * AIMAG(V1 - V1C)
      E12M = EPSI * AIMAG(E1*E2C - E1C*E2)
      E14M = EPSI * AIMAG(E1*E4C - E1C*E4)
      E1VM = EPSI * AIMAG(E1*V1C - E1C*V1)
      E24M = EPSI * AIMAG(E2*E4C - E2C*E4)
      E2VM = EPSI * AIMAG(E2*V1C - E2C*V1)
      E4VM = EPSI * AIMAG(E4*V1C - E4C*V1)
      T = 0.0
      T = T + E1*E1C * ( 64*X1*RP + 64*X2*X7 + 64*X3*X6 )

      T = T + E2*E2C * ( 32*X1*X10 - 64*X1*X18*X19 + 32*X1*RP - 64*X10*
     +    X16*X17 + 128*X16*X17*X18*X19 - 64*X16*X17*RP )

      T = T + E4*E4C * ( 32*X1*X10*W2 - 64*X1*X18*X19*W2 + 32*X1*W2*RP
     +     - 64*X4*X8*X10 + 128*X4*X8*X18*X19 - 64*X4*X8*RP )

      T = T + V1*V1C * ( 64*X1*X10*X21**2 - 64*X1*X10*Z + 64*X1*X12*X14
     +     - 64*X1*X12*X19*X21 - 64*X1*X14*X18*X21 + 64*X1*X18*X19*Z -
     +    64*X2*X7*X21**2 + 64*X2*X7*Z - 64*X2*X9*X14 + 64*X2*X9*X19*
     +    X21 + 64*X2*X14*X17*X21 - 64*X2*X17*X19*Z - 64*X3*X6*X21**2
     +     + 64*X3*X6*Z - 64*X3*X9*X12 + 64*X3*X9*X18*X21 + 64*X3*X12*
     +    X17*X21 - 64*X3*X17*X18*Z - 64*X5*X6*X14 + 64*X5*X6*X19*X21
     +     - 64*X5*X7*X12 + 64*X5*X7*X18*X21 + 64*X5*X9*X10 - 128*X5*X9
     +    *X18*X19 - 64*X5*X9*RP - 64*X5*X10*X17*X21 + 64*X5*X12*X17*
     +    X19 + 64*X5*X14*X17*X18 + 64*X5*X17*X21*RP + 64*X6*X14*X16*
     +    X21 - 64*X6*X16*X19*Z + 64*X7*X12*X16*X21 - 64*X7*X16*X18*Z
     +     - 64*X9*X10*X16*X21 + 64*X9*X12*X16*X19 + 64*X9*X14*X16*X18
     +     + 64*X9*X16*X21*RP + 64*X10*X16*X17*Z - 128*X12*X14*X16*X17
     +     - 64*X16*X17*Z*RP )

      T = T + FK**2*DENOM1**2 * ( 256*X1*X2*X3*R + 128*X1*X2*X14*R +
     +    128*X1*X3*X12*R - 128*X1*X10*R**2 - 128*X1*R**2*RP - 64*X1*R*
     +    Z*RP + 256*X2*X3*X9*R - 128*X2*X5*X7*R - 64*X2*X7*R*Z + 128*
     +    X2*X9*X14*R - 128*X3*X5*X6*R - 64*X3*X6*R*Z + 128*X3*X9*X12*R
     +     + 128*X5*X9*R*RP - 128*X9*X10*R**2 - 128*X9*R**2*RP )

      T = T + FK**2*DENOM1*DENOM2 * (  - 256*X1*X2*X19*R - 256*X1*X3*
     +    X18*R + 256*X1*X10*X16*R + 128*X1*X10*X21*R - 128*X1*X12*X19*
     +    R - 128*X1*X14*X18*R + 256*X1*X16*R*RP + 128*X1*X21*R*RP -
     +    128*X2*X9*X19*R - 128*X3*X9*X18*R + 128*X5*X6*X19*R + 128*X5*
     +    X7*X18*R - 128*X5*X10*X17*R - 128*X5*X17*R*RP + 128*X9*X10*
     +    X16*R + 128*X9*X16*R*RP )

      T = T + FK**2*DENOM2**2 * (  - 128*X1*X10*R + 256*X1*X18*X19*R -
     +    128*X1*R*RP )

      T = T + FK*DENOM1*E11T * (  - 64*X1*X10*R - 64*X1*R*RP + 64*X2*X7
     +    *R + 64*X3*X6*R - 64*X9*X10*R - 128*X9*R*RP )

      T = T + FK*DENOM1*E22 * ( 64*X2*X17*X19*R + 64*X3*X17*X18*R - 32*
     +    X6*X19*X21*R - 32*X7*X18*X21*R - 32*X9*X10*R + 64*X9*X18*X19*
     +    R - 32*X9*R*RP - 64*X10*X16*X17*R + 32*X12*X17*X19*R + 32*X14
     +    *X17*X18*R - 64*X16*X17*R*RP )

      T = T + FK*DENOM1*E44 * ( 64*X2*X8*X19*R + 64*X3*X8*X18*R - 32*X6
     +    *X15*X19*R - 32*X7*X15*X18*R - 64*X8*X10*X16*R - 32*X8*X10*
     +    X21*R + 32*X8*X12*X19*R + 32*X8*X14*X18*R - 64*X8*X16*R*RP -
     +    32*X8*X21*R*RP - 32*X9*X10*X20*R + 32*X9*X11*X19*R + 32*X9*
     +    X13*X18*R - 32*X9*X20*R*RP + 32*X10*X15*X17*R + 32*X15*X17*R*
     +    RP )

      T = T + FK*DENOM1*EVV * ( 32*X6*X14*X21*R - 32*X6*X19*R*Z + 32*X7
     +    *X12*X21*R - 32*X7*X18*R*Z + 32*X9*X12*X19*R + 32*X9*X14*X18*
     +    R + 64*X9*X21*R*RP - 64*X12*X14*X17*R - 64*X17*R*Z*RP )

      T = T + FK*DENOM1*E22M * (  - 32*X18*R + 32*X19*R )

      T = T + FK*DENOM1*E44M * (  - 32*X18*R + 32*X19*R )

      T = T + FK*DENOM1*EVVM * ( 64*X2*R - 64*X3*R + 32*X12*R - 32*X14*
     +    R )

      T = T + FK*DENOM2*E11T * (  - 64*X6*X19*R - 64*X7*X18*R + 64*X10*
     +    X17*R + 64*X17*R*RP )

      T = T + FK*DENOM2*E22 * ( 64*X10*X17*R - 128*X17*X18*X19*R + 64*
     +    X17*R*RP )

      T = T + FK*DENOM2*E44 * ( 64*X8*X10*R - 128*X8*X18*X19*R + 64*X8*
     +    R*RP )

      T = T + FK*DENOM2*EVVM * (  - 64*X18*R + 64*X19*R )

      T = T + E12 * ( 32*X1*X10 - 64*X1*X18*X19 + 32*X1*RP + 32*X2*X17*
     +    X19 + 32*X3*X17*X18 + 32*X6*X16*X19 + 32*X7*X16*X18 - 64*X10*
     +    X16*X17 - 64*X16*X17*RP )

      T = T + E14 * ( 32*X1*X10*X20 - 32*X1*X11*X19 - 32*X1*X13*X18 +
     +    32*X1*X20*RP + 32*X2*X8*X19 + 32*X3*X8*X18 + 32*X4*X6*X19 +
     +    32*X4*X7*X18 - 32*X4*X10*X17 - 32*X4*X17*RP - 32*X8*X10*X16
     +     - 32*X8*X16*RP )

      T = T + E1V * (  - 32*X2*X9*X19 + 32*X2*X14*X17 - 32*X3*X9*X18 +
     +    32*X3*X12*X17 + 32*X5*X6*X19 + 32*X5*X7*X18 + 64*X5*X17*RP -
     +    32*X6*X14*X16 - 32*X7*X12*X16 - 64*X9*X16*RP )

      T = T + E24 * ( 32*X1*X10*X20 - 64*X1*X18*X19*X20 + 32*X1*X20*RP
     +     - 32*X4*X10*X17 + 64*X4*X17*X18*X19 - 32*X4*X17*RP - 32*X8*
     +    X10*X16 + 64*X8*X16*X18*X19 - 32*X8*X16*RP )

      T = T + E2V * (  - 32*X2*X9*X19 + 32*X2*X17*X19*X21 - 32*X3*X9*
     +    X18 + 32*X3*X17*X18*X21 + 32*X5*X6*X19 + 32*X5*X7*X18 - 64*X5
     +    *X17*X18*X19 - 32*X6*X16*X19*X21 - 32*X7*X16*X18*X21 + 64*X9*
     +    X16*X18*X19 )

      T = T + E4V * (  - 32*X2*X9*X19*X20 + 32*X2*X15*X17*X19 - 32*X3*
     +    X9*X18*X20 + 32*X3*X15*X17*X18 + 32*X5*X6*X19*X20 + 32*X5*X7*
     +    X18*X20 - 32*X5*X11*X17*X19 - 32*X5*X13*X17*X18 - 32*X6*X15*
     +    X16*X19 - 32*X7*X15*X16*X18 + 32*X9*X11*X16*X19 + 32*X9*X13*
     +    X16*X18 )

      T = T + E12M * ( 32*X18 - 32*X19 )

      T = T + E1VM * (  - 32*X2 + 32*X3 + 32*X6 - 32*X7 )

      T = T + E2VM * (  - 32*X16*X18 + 32*X16*X19 + 32*X17*X18 - 32*X17
     +    *X19 )

      T = T + E4VM * (  - 32*X4*X18 + 32*X4*X19 + 32*X8*X18 - 32*X8*X19
     +     )
      CROSS_NE = XNORM*T/Z**2

      RETURN
      END
