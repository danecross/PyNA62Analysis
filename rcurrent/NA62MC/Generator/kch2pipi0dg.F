      SUBROUTINE KCH2PIPI0DG (JKAON, pi0decay)
C======================================================
C GENERATE THE DECAY K+- -> PI+- PI0 (DARK-GAMMA)
C M. FABBRICHESI, E. GABRIELLI, B. MELE
C        Phys. Rev. Lett. 119 (2017) no. 3, 031801
C
C FOR FURTHER DETAILS SEE BELOW THE INCLUSION
C OF DARK1.F GIVEN BY AUTHORS
C
C E. GOUDZOVSKI & T. HUSEK OCTOBER 2017
C======================================================

#include "common_blocks.f"
#include "masses.f"

      REAL*8 WTMAX 
      PARAMETER(WTMAX=0.4)

      INTEGER iok, JPIC, JPI0, JGAMMA, j
      integer type, pi0decay, pzmode
      REAL*8 PPIC(4), PPI0(4), PGAMMA(4)
      REAL*8 WCOMP, RANF
      real*8 xi, alphaD, Lambda
      real*8 z1, z2, dBR, dBRmax, check

c --- initialize dBRmax calculation
c      real*8 maxweight
c      save maxweight
c      data maxweight /0./

c --- initialize first call check
      logical firstcall
      save firstcall
      data firstcall /.true./

c --- INITIAL INPUT VALUES

c      xi: coupling constant
c      alphaD: U(1) dark fine structure constant
c      Lambda: new physics scale of the flavor-changing magnetic operator

      xi = 0.05d0
      alphaD = 0.1d0
      Lambda = 100.d0

      dBRmax = 2.77d-6

c --- if this is the first call, initiate looptools and test it!

      if(firstcall) then
         print *,''
         print *,'LoopTools initialization...'
         print *,''
         call ltini

         print *,''
c         print *,'testing LoopTools if dBR ~ 1.69e-6 ...'
         z1 = 0.1d0
         z2 = 0.15d0
         call KppDark(z1, z2, alphaD, xi, Lambda, dBR, check)
c	 print *,'dBR/dz1dz2(0.1, 0.15) = ',dBR
         if (abs(1-dBR/1.69e-6).gt.0.01) then
	    print *,'[kch2pipi0dg] LoopTools initialized incorrectly'
            call exit(0)
         endif
         print *,''

         firstcall = .false.
      endif

C --- GENERATE EVENT UNIFORMLY IN PHASE SPACE USING CERNLIB GENBOD
      KGENEV  = 1
      NP      = 3
      AMASS(1)= MPI
      AMASS(2)= MP0
      AMASS(3)= 0.0
      TECM    = MKCH

 1    CONTINUE
      CALL GENBOD_FIX(iok)
      if (iok.eq.0) goto 1

      WCOMP = RANF()
      IF (WCOMP.GT.(WT/WTMAX)) GOTO 1
      do j = 1, 4
         ppic(j)   = pcm(j,1)
         ppi0(j)   = pcm(j,2)
         pgamma(j) = pcm(j,3)
      enddo

C --- COMPUTE THE DIFFERENTIAL DECAY RATE

c --- definition
c      z1 = k.q1 / mK^2    
c      z2 = k.q2 / mK^2

      z1 = (ppic(4)*pgamma(4) - ppic(1)*pgamma(1) - ppic(2)*pgamma(2)
     >  - ppic(3)*pgamma(3)) / MKCH**2
      z2 = (ppi0(4)*pgamma(4) - ppi0(1)*pgamma(1) - ppi0(2)*pgamma(2) 
     >  - ppi0(3)*pgamma(3)) / MKCH**2

      call KppDark(z1, z2, alphaD, xi, Lambda, dBR, check)
c --- test if outside of kinematically allowed region
      if(check.eq.1) goto 1
c --- clear memory - very important, otherwise running out of it soon!
      call clearcache
c --- call exit routine for LoopTools (not necessary and only annoying printout)
c     call ltexi

c --- find the dBR maximum
c      if(dBR.gt.maxweight) then
c         maxweight = dBR
c         print *,maxweight
c      endif

      WCOMP = RANF()
      IF (WCOMP.GT.(dBR/dBRmax)) GOTO 1

C --- FILL MC PARTICLE LIST
      JPIC   = MCADD4GEN(IDPIP, PPIC,   0)
      JPI0   = MCADD4GEN(IDPIZ, PPI0,   0)
      JGAMMA = MCADD4GEN(IDEXO, PGAMMA, 0)

C --- BOOST PARTICLES INTO LAB FRAME
      CALL DBOOST (p4ini(1,jkaon), MKCH, ppic, ppic)
      CALL DBOOST (p4ini(1,jkaon), MKCH, ppi0, ppi0)
      CALL DBOOST (p4ini(1,jkaon), MKCH, pgamma, pgamma)

C --- Pass the daughters to Geant4
      JPIC   = MCADD4(IDPIP, PPIC)
      JPI0   = MCADD4(IDPIZ, PPI0)
c      JGAMMA = MCADD4(IDEXO, PGAMMA)

C --- FORCED PI0 DECAY
      pzmode = mod (pi0decay, 100)
      call pi0decay_manager(jpi0, ppi0, pzmode)

      RETURN
      END

C=========================================================================
c INCLUDE DARK1.F
C=========================================================================
c
c Differential BR (dBR/(dz1*dz2)) for the K+ decay process 
c
c          K+(p) --> pi+(q1) pi0(q2) + Dark-Photon(k)
c
c     Author: Emidio Gabrielli  
c     date: 28/09/2017
c      
c     from: Phys.Rev.Lett. 119 (2017) no.3, 031801
c
c           Definition of symbols
c           ---------------------
c    z1=k.q1/mK^2    
c    z2=k.q2/mK^2
c    alphaD = U(1) dark fine structure constant
c    Lambda = new physics scale of the flavor-changing magnetic operator
c    xi = coupling constants
c
c    z1+z2 = Edark/mK  where Edark = energy of dark-photon in K rest mass frame
c--------------------------------------------------------------
c INPUTS: "z1,z2,alphaD,xi,Lambda"  to be declared in Real*8
c          Lambda in TeV units
c-------------------------------------------------------------
c
c------------------------------------------------------------------
c OUTPUT dBR = dBR/(dz1*dz2)
c        check=0 -> OK, check=1 -> out of kinematic region
c------------------------------------------------------------------

      SUBROUTINE KppDark(z1,z2,alphaD,xi,Lambda,dBR,check)

      implicit none
      real*8 pigk,mP,mK,mP2,mK2,M,M2,fK,GammaTOT,z1,z2,C,R,Fz,fact
      real*8 ME2,xi,Lambda,LambdaMeV,dGamma,dBR,eD,alphaD,TeV
      real*8 check
      real*8 m12,E2,E3,m12min,m12max,m23min,m23max
      real*8 z1min,z1max,z2min,z2max

      complex*16 A,B,Aex,Bex

#include "looptools.h"
c --- e.g. the LoopTools 2.14 header file
c --- gives the following necessary minimum definitions to run the code
c #define dd0 1
c #define dd00 13
c     double complex D0i
c     external D0i

#include "masses.f"

      pigk=4.d0*datan(1.d0)

c --- use na62mc masses in GeV -> MeV
c      mP=135.d0  ! mass(pi0) in units of MeV
      mP=MP0*1000
c      mK=494.d0  ! in units of MeV
      mK=MKCH*1000

      M=200.d0   ! in units of MeV
      mP2=mP**2
      mK2=mK**2
      M2=M**2
      fK=92.4d0  ! fK=f --> chiral quark model (in units of MeV)
      GammaTOT=5.35d-14 ! in units of MeV
      TeV=1.d+6  ! in units of MeV
      eD=dsqrt(4*pigk*alphaD)
      LambdaMeV=Lambda*TeV

      check=0
c --- kinematic limits for z1 and z2
      m12 = dsqrt(2*z1*mK2 + mP2)
      E2 = (m12**2 - mP2)/(2*m12)
      E3 = (mK2 - m12**2 - mP2)/(2*m12)

      m12min = mP
      m12max = mK - mP
      m23min = dsqrt((E2 + E3)**2 - (E2 + dsqrt(E3**2 - mP2))**2)
      m23max = dsqrt((E2 + E3)**2 - (E2 - dsqrt(E3**2 - mP2))**2)

      z1min = (m12min**2 - mP2)/(2*mK2)
      z1max = (m12max**2 - mP2)/(2*mK2)

      z2min = (m23min**2 - mP2)/(2*mK2)
      z2max = (m23max**2 - mP2)/(2*mK2)


      if((z1.gt.z1max).or.(z1.le.z1min).or.
     &  (z2.gt.z2max).or.(z2.le.z2min)) then 
      check=1
      endif

c---------------------------------------------------
         
       r=mP/mK
       Fz = z1*z2*(1 - 2*(z1 + z2) - 2*r**2) - r**2*(z1**2 + z2**2);

        A=D0i(dd0,0.d0,mK2,mP2,mP2,-mK2*(-1 + 2*z1 + 2*z2),mP2 + 
     &      2*mK2*z2,M2,M2,M2,M2)

        B=D0i(dd00,0.d0,mP2,mP2,mK2,mP2 + 2*mK2*z2,
     &     -mK2*(-1 + 2*z1 + 2*z2),M2,M2,M2,M2)

        Aex=D0i(dd0,0.d0,mK2,mP2,mP2,-mK2*(-1 + 2*z2 + 2*z1),mP2 + 
     &      2*mK2*z1,M2,M2,M2,M2)

        Bex=D0i(dd00,0.d0,mP2,mP2,mK2,mP2 + 2*mK2*z1,
     &     -mK2*(-1 + 2*z2 + 2*z1),M2,M2,M2,M2)


       C=Dreal((M2*(A+Aex)-(B+Bex))*Dconjg((M2*(A+Aex)-(B+Bex))))

       fact=eD/(32*pigk**2)*xi/LambdaMeV*M**3*mK**3/(fK**3*pigk**2)

       ME2=C*fact**2

       dGamma=mK/(4*pigk)**3*ME2*Fz

       if(int(check).eq.0) then
       dBR=dGamma/GammaTOT
       else 
       dBR=0.d0
       endif

       return
       end
