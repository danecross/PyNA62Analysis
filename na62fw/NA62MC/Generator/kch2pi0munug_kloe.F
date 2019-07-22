      SUBROUTINE KCH2PI0MUNUG_KLOE(JKAON, pi0decay,
     >                             TotalMomentumCut, NTrials)
C.    ********************************************************************
C.    *                                                                  *
C.    * Generates a  K+- -> pi0 mu nu_mu gamma decay                     *
C.    * Use KLOE MC Generator                                            *
C.    * C. Gatti Eur. Phys. J. C45 (2006) 417                            *
C.    ********************************************************************
c
c  A COMMENT FROM MICHELE VELTRI, 23 Sep 2010
c
c  This generator is based on O(p^2) chiral expansion, this means that
c  the scalar and vector ff are the same i.e. f0=f+ (or lambda0=lambda+).
c  f+ is factored out in the amplitude formula:
c
c      effe = (1.d+00+lamda*(mkc**2+mp0**2-2.d+00*mkc*Ep)/mpi**2)
c      Am = -Psf*(g0+g1*Eg+g2*Eg**2+g3*Eg**3)*effe**2/mkc**5/52.d+00
c
c  To correctly account for the scalar form factor one need to reweight the event,
c  the weight is the ratio of the matrix element (squared) actual/generated:
c      w=me/gen_me
c  In practice one has to use the formula of the kmu3 decay matrix element once
c  with the desired ff (I use lamp=0.0296; lam0=0.0196, the "old" linear
c  parametrization from PDG) and another time with the generated ff
c  (lamp=0.0296 => lam0=0.0296) and then take the ratio.
c
c********************************************************************************

#include "common_blocks.f"
#include "masses.f"

      Real PPCM(4,4)
      INTEGER JPION, JMUON, JGAM, JNU, J
      INTEGER IPION, IMUON, IGAM, INU
      integer pi0decay, pzmode, NTrials
      real*8 PPI(4), PMU(4), PNU(4), PGAM(4) ! particle 4-momenta
      real*8 p4kaon(4),mom, momtot,TotalMomentumCut

      NTrials = 0

c ... save the kaon momentum as it's overwritten and biasing cannot work otherwise
      do j = 1, 4
         p4kaon(j) = p4ini(j,jkaon)
      enddo

 11   continue
      NTrials = NTrials+1

      call kcmu3g(PPCM)

C     Copy output 4-momenta
C     Output: PPCM(4 ,      4)
C                  Px       mu
C                  Py       pi
C                  Pz       nu
C                  E        gamma
      DO J=1,4
         PMU(J)=PPCM(J,1)
         PPI(J)=PPCM(J,2)
         PNU(J)=PPCM(J,3)
         PGAM(J)=PPCM(J,4)
      ENDDO

c ... Fill MC particle list
      IPION = IDPIZ
      INU   = IDNU
      IGAM  = IDGAM
      IMUON = IDMUP
      JPION = MCADD4GEN(IPION, PPI, 0)
      JMUON = MCADD4GEN(IMUON, PMU, 0)
      JNU   = MCADD4GEN(INU,   PNU, 0)
      IF (PGAM(4).NE.0.) JGAM = MCADD4GEN(IGAM, PGAM, 0)

C     Boost particles to LAB-system
      CALL DBOOST(p4kaon, MKCH, ppi, ppi)
      CALL DBOOST(p4kaon, MKCH, pmu, pmu)
      CALL DBOOST(p4kaon, MKCH, pnu, pnu)
      IF (PGAM(4).NE.0.) CALL DBOOST(p4kaon, MKCH, pgam, pgam)

C     Fill MC particle list
      JPION = MCADD4(IPION, PPI)
      JMUON = MCADD4(IMUON, PMU)
      IF (PGAM(4).NE.0.) JGAM = MCADD4(IGAM, PGAM)

C --- FORCED PI0 DECAY

      pzmode = mod (pi0decay, 100)
      call pi0decay_manager(jpion, ppi, pzmode)

c ... Biasing: require mimimum total momentum of pi+-, mu+-, e+- particles in lab frame
c ... E Goudzovski, 19/10/2018
      if (TotalMomentumCut.gt.0.0) then ! already converted into [GeV] in CMC.cc
         momtot = 0.0 ! [GeV] as the standard unit used in the generators
         do j = 1, npart
            if (pid(j).eq.IDPIP.or.pid(j).eq.IDPIM.or.
     >          pid(j).eq.IDMUP.or.pid(j).eq.IDMUM.or.
     >          pid(j).eq.IDELEP.or.pid(j).eq.IDELEM) then
               mom = sqrt(p4ini(1,j)**2 + p4ini(2,j)**2 + p4ini(3,j)**2)
               momtot = momtot + mom
            endif
         enddo
         if (momtot.lt.TotalMomentumCut) then ! [GeV]
            NPARTGEN = 0
            NPART = 0
            goto 11
         endif
      endif

      RETURN
      END

c---------------------------------------------------------------------

      subroutine kcmu3g(PCM)
      
cccc      Integer Function kcmu3g(PCM)
C
C     Author: C.Gatti  Date: 19/12/03
C     
C     MC generator for K+ ->pi mu nu gamma events
C
C     Output: PCM(4 ,      4)
C                  Px      mu   
C                  Py      pi   
C                  Pz      nu   
C                  E       gamma
C
C     !!!!!! Only the form factor f+ has been included !!!!!!
C
CG 04/02/04 corrected extraction

      Implicit none
#include "masses.f"

      External RANLUX

C     Output      
      Real PCM(4,4)

C     Local
      Real*8 enel(2),angl(4),krnd(1),angoli(2),ampli(1)

      Integer status
      Real prob

      Double Precision  Elmax,Plmax,fnorm,ffact,El,Pl
      Double Precision  cte,ste,pe,cfe,sfe             
      Double Precision  ctk,stk,pk
      Double Precision  bet,b,kmax,Eg,M0q,M0,M0qmax,M0qmin
      Double Precision  cpn,spn,ppn,Epcm,Ppcm,gam,begam
      Double Precision ct,st,cf,sf,piox,pioy,pioz,Ep,Pp,cpk
      Double Precision x,y,g0,g1,g2,g3,Psf,effe,Am

      Double Precision RCM(4,4)

C     Parameters

      Double Precision pi,alfa,lamda
      REAL*8 mkc
      PARAMETER(mkc=MKCH)

      Parameter (pi = 3.1415927d+00)
      Parameter (alfa=1.d+00/137.03599976d+00)
c m.v.      Parameter (lamda=0.030d+00)  ! slope of the form factor f+    
      Parameter (lamda=0.0296d+00)  ! slope of the form factor f+    

C     beginning of the code
           
 10      Continue

C     extract El Cteta and k as b(K/M)^(b-1)/x  where x=E-p*Cteta
 
C     extract El and Cteta as 1/x integrating on k

C     lepton energy , extracted as Integr(1/(E-p cte))dcte Integr(b(K/M)^(b-1)) dk 
      
      status = 0
      kmax = (mkc**2-(mmu+mp0)**2)/(2.d+00*mkc)
      Elmax = (mkc**2+mmu**2-mp0**2)/(2.d+00*mkc)
      Plmax = dsqrt(Elmax**2-mmu**2)
      Do while(status.eq.0)
         CALL RANLUX(enel,2)   
         fnorm = dble(sqrt(enel(1)))
         ffact =((Elmax+Plmax)/mmu)**fnorm
         El = mmu*0.5d+00*(ffact+1.d+00/ffact)
         Pl = dsqrt(El**2-mmu**2)
         bet = dsqrt(1.d+00-(mmu/El)**2)
         b = - 2.d+00*alfa/pi *
     $        (1.d+00 - dlog((1.d+00+bet)/(1.d+00-bet))/(2.d+00*bet))         
         If (b.gt.0.d+00) then
            prob = sngl((kmax/mkc)**b)
            if (enel(2).le.prob) status = 1
         Endif         
      Enddo

      CALL RANLUX(angl,4)
      
C     lepton solid angle 
      cte = dble(angl(1))*2.-1.d+00
      ste = dsqrt(1.d+00-cte**2)
      pe = dble(angl(2))*2.d+00*sngl(pi)
C     photon solid angle extracted as 1/(E-p cte)
      If (Pl.gt.0.d+00) then
         ctk = (El - (El+Pl)*((El-Pl)/(El+Pl))**dble(angl(3)))/Pl
      Else
         ctk = 2.d+00*dble(angl(3))-1.d+00
      Endif
      If ((1.d+00-ctk**2).lt.0.d+00) then
C     write (*,*) 'ctk>1',ctk
         goto 10
      Endif
      stk = dsqrt(1.d+00-ctk**2)
      pk = dble(angl(4))*2.d+00*pi

C     extract Eg according to Eg^b

      CALL RANLUX(krnd,1)      
      
      Eg = Kmax * dble(krnd(1))**(1.d+00/b)

C     D. plot
C     M0q = pion neutrino inv. mass^2
      M0q = mkc**2+mmu**2-2.d+00*mkc*(El+Eg)+2.d+00*El*Eg-
     $     2.d+00*Pl*Eg*ctk
      M0 = dsqrt(M0q)
      M0qmax = (mkc-mmu)**2
      M0qmin = mp0**2
      If (M0q.lt.M0qmin.or.M0q.gt.M0qmax) goto 10
      
C     extract teta and phi in pi neutrino CMS
      CALL RANLUX(angoli,2)
      
      cpn = dble(angoli(1))*2.-1.d+00
      spn = dsqrt(1.d+00-cpn**2)
      ppn = dble(angoli(2))*2.*pi      
      
      Epcm = (M0q+mp0**2)/(2.d+00*M0)
      Ppcm = (M0q-mp0**2)/(2.d+00*M0)

      gam = (mkc-El-Eg)/M0
      begam = dsqrt(gam**2-1.d+00)
      
      ct = (-Pl-Eg *ctk)/dsqrt(Pl**2+Eg**2+2.d+00*Pl*Eg*ctk)
      if (ct.lt.-1.0) ct=-1.0
      if (ct.gt.+1.0) ct=+1.0
      st = dsqrt(1.d+00-ct**2)
      cf = dcos(pk+pi)
      sf = dsin(pk+pi)
C     pion momentum
      piox = Ppcm*spn*(cf*ct*dcos(ppn)-sf*dsin(ppn))
     $     + cf*st*gam*Ppcm*cpn + cf*st*begam*Epcm 
      
      pioy = Ppcm*spn*(sf*ct*dcos(ppn)+cf*dsin(ppn))
     $     + sf*st*gam*Ppcm*cpn + sf*st*begam*Epcm 

      pioz = -Ppcm*st*spn*dcos(ppn)+
     $     gam*ct*Ppcm*cpn+begam*ct*Epcm
      
      Ep=gam*Epcm+begam*Ppcm*cpn
      Pp=dsqrt(piox**2+pioy**2+pioz**2)

      If (Pp.gt.0.d+00) then
         cpk = (piox*stk*dcos(pk)+pioy*stk*dsin(pk)+pioz*ctk)
     $        /Pp
      Else
         cpk=0.d+00
      Endif
C     amplitude variables
C
C     x = Pl*Pg/MkEg  
C     y same for pion
C
      x = (El - Pl*ctk)/Mkc
      y = (Ep-Pp*cpk)/Mkc

C     g0
      
      g0 =  (-2.d+00*(16.d+00*El**2*mkc**2 - 8.d+00*Ep*mkc**3 + 
     $     4.d+00*mkc**4 - 6.d+00*Ep*mkc*mmu**2 + 
     $     3.d+00*mkc**2*mmu**2 + mmu**4 - 
     $     8.d+00*El*mkc*(-2.d+00*Ep*mkc + 2.d+00*mkc**2 + mmu**2) + 
     $     4.d+00*mkc**2*mp0**2 - mmu**2*mp0**2)*(mmu**2 + 
     $     mkc*x*(-2.d+00*El + mkc*x)))/
     $     (mkc**2*x)   
      
C     g1

      g1 =  (2.d+00*(16.d+00*mkc**2*mmu**2 + 8.d+00*mmu**4 + 
     $     4.d+00*mkc**4*x - 9.d+00*mkc**2*mmu**2*x - 
     $     5.d+00*mmu**4*x + 4.d+00*mkc**2*mp0**2*x + 
     $     3.d+00*mmu**2*mp0**2*x + 
     $     16.d+00*mkc**4*x**2 + 8.d+00*mkc**2*mmu**2*x**2 - 
     $     16.d+00*mkc**4*x**3 - 
     $     8.d+00*mkc**2*mmu**2*x**3 + 
     $     2.d+00*Ep*mkc*(-1.d+00 + x)*(4.d+00*mkc**2*x*
     $     (1.d+00 + 2.d+00*x) + mmu**2*(8.d+00 + 3.d+00*x))- 
     $     8.d+00*mkc**4*x**2*y - 6.d+00*mkc**2*mmu**2*x**2*y - 
     $     16.d+00*El**2*mkc**2*x*(-5.d+00 + 4.d+00*x + y) + 
     $     2.d+00*El*mkc*(mmu**2*(-16.d+00 + x + 8.d+00*x**2 + 
     $     3.d+00*x*y) + 4.d+00*x*(-mp0**2-6.d+00*Ep*mkc*(-1.d+00 + x)+ 
     $     mkc**2*(-6.d+00 + 4.d+00*x**2+y+ 2.d+00*x*y)))))/(mkc*x)

C     g2
      g2 =  (32.d+00*(-1.d+00 + x)*(mmu**2 + mkc*x*
     $     (Ep*(-1.d+00 + x) + El*(-4.d+00 + 2.d+00*x + y) + 
     $     mkc*(1.d+00 + x - x**2 - x*y))))/
     $     x

C      g3 

      g3 = (32.d+00*mkc*(-1.d+00 + x)**2)

C     phase space factor 

      Psf = Pl*(M0q-mp0**2)/(b*M0q)

C     amplitude      

      effe = (1.d+00+lamda*(mkc**2+mp0**2-2.d+00*mkc*Ep)/mpi**2)

      Am = -Psf*(g0+g1*Eg+g2*Eg**2+g3*Eg**3)*effe**2/mkc**5/52.d+00

      Call Ranlux(ampli,1)
      if (Am.gt.1.d+00) then
C         write (*,*) 'KCMU3G:Am>1',Am
         goto 10
      Endif
      If (Am.lt.0.d+00) then
C         write (*,*) 'KCMU3G:Am<0',Am
         goto 10
      Endif

      If (Ampli(1).gt.sngl(Am)) goto 10      

C      charged lepton

      RCM(1,1)=0.
      RCM(2,1)=0.
      RCM(3,1)=sngl(Pl)
      RCM(4,1)=sngl(El)

C     pion

      RCM(1,2)=piox
      RCM(2,2)=pioy
      RCM(3,2)=pioz
      RCM(4,2)=Ep

C     photon

      RCM(1,4)=Eg*stk*dcos(pk)
      RCM(2,4)=Eg*stk*dsin(pk)
      RCM(3,4)=Eg*ctk
      RCM(4,4)=Eg

C     final rotation

      cfe = dcos(pe)
      sfe = dsin(pe)

C     lepton
      PCM(1,1) = sngl(cfe*cte*RCM(1,1)-sfe*RCM(2,1)+cfe*ste*RCM(3,1))
      PCM(2,1) = sngl(sfe*cte*RCM(1,1)+cfe*RCM(2,1)+sfe*ste*RCM(3,1))
      PCM(3,1) = sngl(-ste*RCM(1,1)+cte*RCM(3,1))
      PCM(4,1) = sngl(RCM(4,1))

C     pion
      PCM(1,2) = sngl(cfe*cte*RCM(1,2)-sfe*RCM(2,2)+cfe*ste*RCM(3,2))
      PCM(2,2) = sngl(sfe*cte*RCM(1,2)+cfe*RCM(2,2)+sfe*ste*RCM(3,2))
      PCM(3,2) = sngl(-ste*RCM(1,2)+cte*RCM(3,2))
      PCM(4,2) = sngl(RCM(4,2))

C     photon
      PCM(1,4) = sngl(cfe*cte*RCM(1,4)-sfe*RCM(2,4)+cfe*ste*RCM(3,4))
      PCM(2,4) = sngl(sfe*cte*RCM(1,4)+cfe*RCM(2,4)+sfe*ste*RCM(3,4))
      PCM(3,4) = sngl(-ste*RCM(1,4)+cte*RCM(3,4))
      PCM(4,4) = sngl(RCM(4,4))

C     neutrino
      PCM(1,3) = -PCM(1,1)-PCM(1,2)-PCM(1,4)
      PCM(2,3) = -PCM(2,1)-PCM(2,2)-PCM(2,4)
      PCM(3,3) = -PCM(3,1)-PCM(3,2)-PCM(3,4)
      PCM(4,3) = sngl(mkc-Eg-El-Ep)

      Return
      End
