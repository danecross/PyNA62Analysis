      SUBROUTINE KCH2PIGG (JKAON) 
C==============================================
C GENERATE THE DECAY K+- -> PI+- GAMMA GAMMA
C SEE G.D'AMBROSIO, J.PORTOLES, HEP-PH/9606213
C
C E. GOUDZOVSKI             26/09/02, 08/12/02
C==============================================

#include "common_blocks.f"
#include "masses.f"

      REAL*8    TOPWT, WT4, WT6, RANF
      PARAMETER (TOPWT=0.38)

      INTEGER JPIC, JE1, JE2, j
      REAL*8 PPI(4), PG1(4), PG2(4)
      REAL*8 MGG2, Z, Y, WTME, WCOMP
      INTEGER OP6, iok

C---------------------------------
C  PARAMETER OF THE MODEL: C_HAT
C---------------------------------
      REAL*8 C_HAT
      PARAMETER (C_HAT = 1.8)

C------------------------------------------------------------
C THE MAXIMUM WEIGHT IS DEFINED AS A FUNCTION OF C_HAT
C SO THAT WE CAN SAFELY CHANGE C_HAT IN THE RANGE (-10; 10),
C AND MATRIX ELEMENT WEIGHTING PROCEDURE WILL WORK PROPERLY
C------------------------------------------------------------
      REAL*8 WTMAX, WTMAX4, WTMAX6
      PARAMETER ( WTMAX4 = (0.8e-11) + (0.35e-12)*(C_HAT+0.5)**2 )
      PARAMETER ( WTMAX6 = (1.2e-11) + (0.35e-12)*(C_HAT-0.5)**2 )

C-----------------------------------------------------------
C THE FLAG BELOW INDICATES:
C OP6==0: MATRIX EL-T AT LEADING CHPT ORDER O(P^4)
C OP6==1: MATRIX EL-T AT O(P^6), INCL. UNITARITY CORRECTIONS
C-----------------------------------------------------------
      OP6 = 1

C-------------------------------------------------------------
C --- GENERATE EVENT UNIFORMLY IN PHASE SPACE (CERNLIB GENBOD)
      KGENEV  = 1
      NP      = 3
      AMASS(1)= MPI
      AMASS(2)= 0.0
      AMASS(3)= 0.0
      TECM    = MKCH
 1    CONTINUE
      CALL GENBOD_FIX(iok)
      if (iok.eq.0) goto 1
      WCOMP = RANF()
      IF (WCOMP.GT.(WT/TOPWT)) GOTO 1
C
C --- KINEMATIC VARIABLES
      MGG2   = (PCM(4,2)+PCM(4,3))**2 - (PCM(1,2)+PCM(1,3))**2 -
     >         (PCM(2,2)+PCM(2,3))**2 - (PCM(3,2)+PCM(3,3))**2
      Z      = MGG2/SQMKCH
      Y      = (PCM(4,2)-PCM(4,3))/MKCH
C
C --- MATRIX ELEMENT
      IF (OP6.EQ.0) THEN
        WTME  = WT4(Z,C_HAT)
	WTMAX = WTMAX4
      ELSE
        WTME  = WT6(Z,Y,C_HAT)
	WTMAX = WTMAX6
      ENDIF
      IF (WTME.GT.WTMAX) WRITE(0,*) "@warning WT=",WTME,"WTMAX=",WTMAX
      WCOMP = RANF() * WTMAX
      IF (WCOMP.GT.WTME) GOTO 1

      do j = 1, 4
         ppi(j) = pcm(j,1)
         pg1(j) = pcm(j,2)
         pg2(j) = pcm(j,3)
      enddo

C --- FILL MC PARTICLE LIST
      JPIC = MCADD4GEN(IDPIP, PPI, 0)
      JE1  = MCADD4GEN(IDGAM, PG1, 0)
      JE2  = MCADD4GEN(IDGAM, PG2, 0)

C --- BOOST PARTICLES INTO LAB FRAME
      CALL DBOOST (p4ini(1,jkaon), MKCH, ppi, ppi)
      CALL DBOOST (p4ini(1,jkaon), MKCH, pg1, pg1)
      CALL DBOOST (p4ini(1,jkaon), MKCH, pg2, pg2)

C --- FILL MC PARTICLE LIST
      JPIC = MCADD4(IDPIP, PPI)
      JE1  = MCADD4(IDGAM, PG1)
      JE2  = MCADD4(IDGAM, PG2)

      RETURN
      END

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C --- auxiliary functions ---
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C ... 1. MATRIX ELEMENT AT O(P^4)

      FUNCTION WT4(Z, C_HAT)
      IMPLICIT NONE
      REAL*8 WT4, Z, C_HAT, A_AMPL_SQ, C_AMPL_SQ, A2, C2
      A2  = A_AMPL_SQ(Z, C_HAT)
      C2  = C_AMPL_SQ(Z)
      WT4 = (Z**2)*(A2+C2)
      RETURN
      END

C ... 2. MATRIX ELEMENT AT O(P^6)
      FUNCTION WT6(Z, Y, C_HAT)
      IMPLICIT NONE
#include "masses.f"
      REAL*8 WT6, Z, Y, C_HAT, A_AMPL, B_AMPL, C_AMPL_SQ
      REAL*8 A_RE,A_IM,B_RE,B_IM,B_SQ,AB_RE,AB_IM,AB_SQ,C_SQ,LAMBDA,S
      A_RE   = A_AMPL(Z,Y,C_HAT,1)
      A_IM   = A_AMPL(Z,Y,C_HAT,2)
      B_RE   = B_AMPL(Z,C_HAT,1)
      B_IM   = B_AMPL(Z,C_HAT,2)
      C_SQ   = C_AMPL_SQ(Z)
      B_SQ   = B_RE**2 + B_IM**2
      AB_RE  = A_RE + B_RE
      AB_IM  = A_IM + B_IM
      AB_SQ  = AB_RE**2 + AB_IM**2
      LAMBDA = 1.+(RPI2**2)+(Z**2)-2.*(RPI2+Z+RPI2*Z)
      S      = Y**2 - 0.25*LAMBDA
      WT6    = (Z**2)*(AB_SQ+C_SQ) + (S**2)*(B_SQ)
      RETURN
      END

c...............................................................

      FUNCTION A_AMPL(Z, Y, C_HAT, MODE)
C=============================================================
C RE/IM PARTS OF O(P^6) AMPLITUDE A(Z,Y) FOR K-->PI G G
C MODE==1: REAL PART, MODE!=1: IMAGINARY PART
C SEE G.D'AMBROSIO, J.PORTOLES, HEP-PH/9606213
C=============================================================      

      IMPLICIT NONE
#include "masses.f"

CC    PARAMETERS
      REAL*8 G8, MRHO
      PARAMETER (G8=9.2E-6, MRHO=0.7711)
      REAL*8 ALPHA1, ALPHA3, BETA1, BETA3, GAMMA3, DZETA1, XI1
      PARAMETER (ALPHA1 = 92.11E-8)
      PARAMETER (ALPHA3 = -6.97E-8)
      PARAMETER (BETA1  = -26.76E-8)
      PARAMETER (BETA3  = -2.17E-8)
      PARAMETER (GAMMA3 = 2.98E-8)
      PARAMETER (DZETA1 = -0.51E-8)
      PARAMETER (XI1    = -1.66E-8)
CC    FUNCTIONS
      REAL*8 F_RE, F_IM, R_RE, R_IM, DLOG
CC    INPUT
      REAL*8 Z, Y, C_HAT, A_AMPL
      INTEGER MODE
CC    VARIABLES
      REAL*8 AA,BB,CC,DD,EE,FF,GG,HH,II,JJ,LL,MM,NN,OO,PP,QQ,RR,SS,TT,UU
      REAL*8 PRR
      REAL*8 FFUN, FFUNZ, RFUN

      PRR = SQMPI/MRHO/MRHO
      AA = (2.*(2.*ALPHA1-ALPHA3) + (1.+1./3./RPI2-Z/RPI2) *
     >     (BETA1-BETA3/2.+SQRT(3.)*GAMMA3))/Z
      BB = 8./3./RPI2/RPI2*(2.*DZETA1-XI1)
      CC = RPI2 * (DLOG(PRR)-1.)
      DD = (1.+6.*(RPI2-Z)+9.*(RPI2-Z)**2)/18./Z
      EE = 8./3./(RPI2**2)*(4.*DZETA1+XI1)
      FF = (1.+6.*RPI2)*DLOG(PRR)/12.
      GG = RPI2/2.
      HH = (9.*RPI2-5.-(3.*(1.+3.*RPI2)*(RPI2-Z)))/36./Z
      II = Y**2/Z
      JJ = 1./12.
      LL = (1.+2.*RPI2/Z)/2.
      MM = ((1.-RPI2+Z)**2)/4./Z
      NN = 1.-RPI2+Z
      OO = 1./24. + Z/72./RPI2
      PP = (1.+2.*RPI2/Z)/12.
      QQ = Z/RPI2
      RR = Z
      SS = 1./36.- RPI2/24./Z + Z/72./RPI2
      TT = (Z/RPI2 + 1. - 2.*RPI2/Z)/12.
      UU = (RPI2/2./Z)*(1.-RPI2/Z)

      IF (MODE.EQ.1) THEN   ! REAL
        FFUN  = F_RE(Z/RPI2)
        FFUNZ = F_RE(Z)
        RFUN  = R_RE(Z/RPI2)
      ELSE                  ! IMAGINARY
        FFUN  = F_IM(Z/RPI2)
        FFUNZ = F_IM(Z)
        RFUN  = R_IM(Z/RPI2)
      ENDIF

      IF (MODE.EQ.1) THEN

         A_AMPL = 0.5 * (
     >            AA*FFUN - BB*(CC+DD*FFUN)
     >            - EE * (
     >            - FF + GG - HH*FFUN
     >            + II * (JJ+    3.*RFUN+LL*FFUN)
     >            - MM * (JJ+    1.*RFUN+LL*FFUN)
     >            + NN * (OO+PP*(QQ*RFUN+3.*FFUN))
     >            - RR * (SS+    TT*RFUN+UU*FFUN)
     >            )
     >            + G8*SQMKCH * ((Z+RPI2-1.)*FFUNZ/Z - C_HAT)
     >            )

      ELSE

         A_AMPL = 0.5 * (
     >            AA*FFUN - BB*DD*FFUN
     >            - EE * (
     >            - HH*FFUN
     >            + II * (    3.*RFUN+LL*FFUN)
     >            - MM * (    1.*RFUN+LL*FFUN)
     >            + NN * (PP*(QQ*RFUN+3.*FFUN))
     >            - RR * (    TT*RFUN+UU*FFUN)
     >            )
     >            + G8*SQMKCH * ((Z+RPI2-1.)*FFUNZ/Z)
     >            )

      ENDIF

      RETURN
      END

c----------------------------------------------------------------
      FUNCTION A_AMPL_SQ(Z, C_HAT)
      IMPLICIT NONE
#include "masses.f"
      REAL*8 G8
      PARAMETER (G8=9.2E-6)

      REAL*8 F_RE, F_IM
      REAL*8 Z, C_HAT, A_RE, A_IM, A_AMPL_SQ
      A_RE   = G8*SQMKCH/2./Z *
     >         ((Z+1.-RPI2)*F_RE(Z/RPI2)+(Z+RPI2-1.)*F_RE(Z)-C_HAT*Z)
      A_IM   = G8*SQMKCH/2./Z *
     >         ((Z+1.-RPI2)*F_IM(Z/RPI2)+(Z+RPI2-1.)*F_IM(Z))
      A_AMPL_SQ   = A_RE**2 + A_IM**2
      RETURN
      END

c ............................................................
      FUNCTION B_AMPL(Z, C_HAT, MODE)
      IMPLICIT NONE
#include "masses.f"
CC    PARAMETERS
      REAL*8 MRHO
      PARAMETER (MRHO=0.7711)
      REAL*8 ALPHA1, ALPHA3, BETA1, BETA3, GAMMA3, DZETA1, XI1
      PARAMETER (ALPHA1 = 92.11E-8)
      PARAMETER (ALPHA3 = -6.97E-8)
      PARAMETER (BETA1  = -26.76E-8)
      PARAMETER (BETA3  = -2.17E-8)
      PARAMETER (GAMMA3 = 2.98E-8)
      PARAMETER (DZETA1 = -0.51E-8)
      PARAMETER (XI1    = -1.66E-8)
CC    FUNCTIONS
      REAL*8 F_RE, F_IM, R_RE, R_IM
CC    INPUT
      REAL*8 Z, C_HAT, B_AMPL
      INTEGER MODE
CC    VARIABLES
      REAL*8 FFUN, RFUN, AA, BB, CC, DD, PRR
      PRR = SQMPI/MRHO/MRHO
      AA = (4.*DZETA1+XI1)/RPI2/RPI2/3.
      BB = (Z/RPI2/18.) - (1.+2.*DLOG(PRR))/6.
      CC = 2.*RPI2/Z
      DD = (Z/RPI2-10.)/3.

      IF (MODE.EQ.1) THEN   ! REAL
        FFUN   = F_RE(Z/RPI2)
        RFUN   = R_RE(Z/RPI2)
        B_AMPL = AA*(BB-CC*FFUN+DD*RFUN)
      ELSE                  ! IMAGINARY
        FFUN   = F_IM(Z/RPI2)
        RFUN   = R_IM(Z/RPI2)
        B_AMPL = AA*(-CC*FFUN+DD*RFUN)
      ENDIF

      RETURN
      END

c.....................................................
      FUNCTION C_AMPL_SQ(Z)
      IMPLICIT NONE
#include "masses.f"
      REAL*8 G8, GAMMAPI0, META, RETA
      PARAMETER (G8=9.2E-6, GAMMAPI0=8.02E-9)
      PARAMETER (META=.5473, RETA=META/MKCH)

      REAL*8 X, Y, Z, C1_RE, C1_IM, C2_RE, C_RE, C_IM, C_AMPL_SQ
      X         = Z - RPI2
      Y         = RPI*GAMMAPI0/MKCH
      C1_RE     = X**2/(X**2+Y**2)
      C1_IM     = -X*Y/(X**2+Y**2)
      C2_RE     = (Z-(2.+RPI2)/3.)/(Z-RETA**2)
      C_RE      = G8*SQMKCH*(C1_RE-C2_RE)
      C_IM      = G8*SQMKCH*C1_IM
      C_AMPL_SQ = C_RE**2 + C_IM**2
      RETURN
      END
c...................................................
      FUNCTION R_RE(X)
      IMPLICIT NONE
      REAL*8 X, R_RE, S
      IF (X.LE.4.) THEN
         R_RE = -1./6. + 2./X - 2./X*SQRT(4./X-1.)*ASIN(SQRT(X)/2.)
      ELSE
         S = SQRT(1.-4./X)
         R_RE = -1./6. + 2./X - S/X*DLOG((1.-S)/(1.+S))
      ENDIF
      RETURN
      END
c.........................................................
      FUNCTION R_IM(X)
      IMPLICIT NONE
      REAL*8 X, R_IM, S
      IF (X.LE.4.) THEN
         R_IM = 0.
      ELSE
         S = SQRT(1.-4./X)
         R_IM = S/X*3.141592653
      ENDIF
      RETURN
      END 
c.........................................................
      FUNCTION F_RE(X)
      IMPLICIT NONE
      REAL*8 X, F_RE, DLOG, SQRT, A, B, S
      IF (X.LE.4.) THEN
         F_RE = 1. - 4./X*((ASIN(SQRT(X)/2.))**2)
      ELSE
         S = SQRT(1.-4./X)
         A = DLOG((1.-S)/(1.+S))
         B = 3.141592653
         F_RE = 1. + (A**2-B**2)/X
      ENDIF
      RETURN
      END
c......................................................
      FUNCTION F_IM(X)
      IMPLICIT NONE
      REAL*8 X, F_IM, S, A, B
      IF (X.LE.4.) THEN
         F_IM = 0.
      ELSE
         S = SQRT(1.-4./X)
         A = DLOG((1.-S)/(1.+S))
         B = 3.141592653
         F_IM = 2.*A*B/X
      ENDIF
      RETURN
      END
