      SUBROUTINE kch2pipi0g_ib (JKAON, Pi0DECAY, IBEMIN, IBEMAX, 
     $        ggMaxAngle, TotalMomentumCut, NTrials)

C-----------------------------------------------------------------C
C K+ --> pi+ pi0 gamma (IB) generator                             C
C It corresponds to NA62MC decay mode 2                           C
C This wrapper calls the KLOE generators by Claudio Gatti,        C
C boosts daughters into lab frame & interfaces to GEANT4          C
C The lower and upper limits on the photon energy in the K frame, C
C the upper limit on the min photon-photon angle in lab frame,    C
C and the lower limit on the sum of track momenta in lab frame    C
C are set via the following data cards:                           C
C    /decay/RadiativePhotonMinEnergy [MeV]                        C
C    /decay/RadiativePhotonMaxEnergy [MeV]                        C
C    /decay/twoPhotonsMaxAngle [radians]                          C
C    /decay/MinTracksMomentum [MeV]                               C
C                                                                 C
C E.Goudzovski 9/08/2011                                          C
C-----------------------------------------------------------------C

#include "common_blocks.f"
#include "masses.f"

      integer i, j, istat, jpic, jpi0, jgam
      integer PI0DECAY, PZMODE, NTrials
      real*8  PPCM(4,3)
      REAL*8  p4pic(4), p4pi0(4), p4gam(4), IBEMIN, IBEMAX
      REAL*8  ggMaxAngle, TotalMomentumCut
      logical gamma_generated
      logical GOODCOMBINATION
      REAL*8  PHOTILEN, PHOTJLEN, photonsAngle, scalarProd
      REAL*8  p4kaon(4),mom, momtot
      INTEGER KCPPG

      NTrials = 0

c ... save the kaon momentum as it's overwritten and biasing cannot work otherwise
      do j = 1, 4
         p4kaon(j) = p4ini(j,jkaon)
      enddo

 1    continue
      NTrials  = NTrials+1

      istat = KCPPG (PPCM,IBEMIN,IBEMAX) ! all units are [GeV]
      do i = 1,4
         p4pic(i) = PPCM(i,1)
         p4pi0(i) = PPCM(i,2)
         p4gam(i) = PPCM(i,3)
      enddo

      gamma_generated = .true.
      if (p4gam(4).lt.1e-12) gamma_generated = .false.

c --- FILL MC PARTICLE LIST
      JPIC = MCADD4GEN(IDPIP, p4pic, 0)
      JPI0 = MCADD4GEN(IDPIZ, p4pi0, 0)
      JGAM = MCADD4GEN(IDGAM, p4gam, 0)

c --- BOOST TO THE LAB-SYSTEM
      CALL DBOOST(p4kaon,MKCH,p4pic,p4pic)
      CALL DBOOST(p4kaon,MKCH,p4pi0,p4pi0)
      if (gamma_generated) CALL DBOOST(p4kaon,MKCH,p4gam,p4gam)

c --- FILL MC PARTICLE LIST
      JPIC = MCADD4(IDPIP, p4pic)
      JPI0 = MCADD4(IDPIZ, p4pi0)
      if (gamma_generated) JGAM = MCADD4(IDGAM, p4gam)

c --- FORCED PI0 DECAY
      pzmode = mod(pi0decay, 100)
      call pi0decay_manager(jpi0, p4pi0, pzmode)

c ... Biasing based on the photon-photon angle
      gamma_generated = .true.
      if (gamma_generated.AND.ggMaxAngle.GT.0) then
         GOODCOMBINATION = .FALSE.
         DO I = 1, NPART
            DO J = I+1, NPART
               if (pid(i).eq.IDGAM.and.pid(j).eq.IDGAM) then
                  PHOTILEN = p4ini(1,i)**2+p4ini(2,i)**2+p4ini(3,i)**2
                  PHOTJLEN = p4ini(1,j)**2+p4ini(2,j)**2+p4ini(3,j)**2
                  scalarProd = p4ini(1,i)*p4ini(1,j)
     $                       + p4ini(2,i)*p4ini(2,j)
     $                       + p4ini(3,i)*p4ini(3,j)
                  photonsAngle=ACOS(scalarProd/sqrt(PHOTILEN*PHOTJLEN))
                  IF (photonsAngle.LT.ggMaxAngle) GOODCOMBINATION=.TRUE.
               endif
            ENDDO
         ENDDO
         IF (.NOT.GOODCOMBINATION) then
            NPARTGEN = 0
            NPART = 0
            goto 1
         endif
      endif

c ... Biasing: require mimimum total momentum of pi+- and e+- particles in lab frame
c ... E Goudzovski, 18/10/2018
      if (TotalMomentumCut.gt.0.0) then ! already converted into [GeV] in CMC.cc
         momtot = 0.0 ! [GeV] as the standard unit used in the generators
         do j = 1, npart
            if (pid(j).eq.IDPIP.or.pid(j).eq.IDPIM.or.
     >          pid(j).eq.IDELEP.or.pid(j).eq.IDELEM) then
               mom = sqrt(p4ini(1,j)**2 + p4ini(2,j)**2 + p4ini(3,j)**2)
               momtot = momtot + mom
            endif
         enddo
         if (momtot.lt.TotalMomentumCut) then ! [GeV]
            NPARTGEN = 0
            NPART = 0
            goto 1
         endif
      endif

      RETURN
      END

c--------------------------------------------------

      Integer Function KCPPG(PCM,IBEMIN,IBEMAX)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Author: C. Gatti     28/11/03
C     MC generator for K+ --> pi+ pi0 gamma IB
C     DE is not included
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Input: none
C
C     Output: PCM(4,Npart)  Npart = 3 number of particles
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C     K --> pi+(P1) pi0(P0) gamma(q)
C
C
C     Amplitude
C     A_IB = i e A_0 (P1/P1*q - Pk/Pk*q)  from Low theorem
C
C     dGamma = (2 pi)^4/(2.*Mk) * |A_IB|^2 dF
C
C     Phase Space factor
C     dF = dP1/((2*pi)^3*2*E1) *dP0/((2*pi)^3*2*E0)*dq/((2*pi)^3*2*q) d^4(Mk-p1-
C      p0-q)
C     dF = 1./(2*pi)^7 *1./4. * q*dq dCteta * P+^2/(E0P1+E1P1+E1 q Cteta)
C
C     dGamma/(dqdCteta) = alfa/(8*Mk)*1./(2*pi)^3 |A_0|^2*|(P1/P1*q - Pk/Pk*q)|^
C      2
C                         * P1^2/(Mk*P1 -  q (p1 - E1 Cteta)) * q
C
C     Maximum in q=0, P1*Cteta = sqrt((Mk/2)^2-2 mp^2)
C     Max = alfa * sqrt(mk0**2 - 4.*mpi**2) / ((2 pi)^6 * 32 * mp^2)
C
C     Code description:
C     Dplot = F(q,Cteta)/(q/qmax) ---> F(q,Cteta)/(q/qmax)^(1-bfactor)
C
C
C     bfactor = -2 alfa/ pi (1-ln((1+beta)/(1-beta))/(2beta))
C     beta = sqrt(1-(2 mk mp+/(mk^2-mp0^2+mp+^2))**2)
C
C
C
C
C     1) extract Cteta with flat distribution and q according to
C        cumulative distribution  k^bfactor.
C
C     2) perform hit or miss inside Dalitz Plot boundary.
C
C     3) weight the event with the function F(k,Cteta) performing
C        a new hit or miss.
C
C     4) calculate the momenta
C
C     5) finally rotate randomly the momenta
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      Implicit none

#include "masses.f"

C     Output
      real*8 PCM(4,3),IBEMIN,IBEMAX
C     Local
      Integer status
      real*8 pgs(2),pb(1),rot(3)
      real*8 randIBInterval
      real*8 Cteta,Steta,Emax
      real*8 kk,delta,Ene,Pp
      real*8 bfactor,beta
      real*8 Edp,boundary
      real*8 valore
      real*8 Pg1,Pg2,Pg3
      real*8 C1,S1,Phik,Phi1
      real*8 PE(4),P1(4),P2(4)
      real*8 pi,alfa
      Parameter (pi = 3.1415927d+00)
      Parameter (alfa=1.d+00/137.03599976d+00)
C     Beginning of the code
      kcppg = 1
      Cteta = 0.d+00
      CALL VZERO(PE,4)
      CALL VZERO(P1,4)
      CALL VZERO(P2,4)
C                 ------> Pion+(1)
C     gamma <--- K
C                 ------> Pion0(2)
C     maximum energy of the photon
      Emax = 0.5d+00*(mkch-(mp0+mpi)**2/mkch)
C     bond factor
      beta = dsqrt(1.d+00
     $     -(2.d+00*mkch*mpi/(mkch**2+mpi**2-mp0**2))**2)
      bfactor = -2.d+00*alfa/pi *
     $     (1.d+00-dlog((1.d+00+beta)/(1.d+00-beta))*0.5d+00/beta)
      status = 0
      Do while (status.eq.0)
C     Energy Distribution + Angular Flat
         CALL RANLUX(pgs,2)
C     Angle
         Cteta = dble(pgs(2))*2.d+00-1.d+00
         if ((1.d+00 - Cteta**2).lt.0.d+00) then
           Steta = 0.d+00
         else
           Steta = dsqrt(1.d+00 - Cteta**2)
         end if
C      pion+ energy which gives the maximum photon energy given Cteta
         Edp = mpi *( 2.d+00*Cteta**2*mkch*mpi*(mkch**2-mp0**2+mpi**2)-
     $      (mkch**2+mp0**2-mpi**2)*dsqrt((1.d+00-Cteta**2)*mkch**4
     $      + 2.d+00*( (1.d+00+Cteta**2)*mp0**2-(1.d+00-Cteta**2)
     $      *mpi**2)*mkch**2+(1.d+00-Cteta**2)*(mp0**2-mpi**2)**2 ))
     $      /((Cteta**2-1.d+00)*mkch**4-2.d+00*(Cteta**2+1.d+00)*
     $      (mp0**2-mpi**2)*mkch**2+(Cteta**2-1.d+00)
     $      *(mp0**2-mpi**2)**2)
C     boundary is the photon energy as a function of E+ and Cteta
C     when E+ is Edp
         boundary = (mkch**2 - 2.d+00*mkch*Edp-mp0**2+mpi**2)/
     $        (2.d+00*(mkch+dsqrt(Edp**2-mpi**2)*Cteta - Edp))
C     photon Energy
         randIBInterval = dble(pgs(1))*((IBEMAX/Emax)**bfactor-
     $        (IBEMIN/Emax)**bfactor)+(IBEMIN/Emax)**bfactor
C        kk = Emax*dble(pgs(1))**(1.d+00/bfactor)
         kk = Emax*dble(randIBInterval)**(1.d+00/bfactor)

C If inside the dalitz plot
         If (kk.le.boundary) Then
C     Weight with P(Cteta,K)/(1/K)
C     pion energy
            delta = mkch**4-4.d+00*kk*mkch**3 -
     $           2.d+00*(mp0**2+mpi**2-2.d+00*kk**2)*mkch**2+
     $           4.d+00*(mp0**2+mpi**2)*kk*mkch+mp0**4
     $           -2.d+00*mp0**2*mpi**2+mpi**2*(mpi**2
     $           +4.d+00*(Cteta**2-1.d+00)*kk**2)
C it should never happen
            If (delta.lt.0.d+00) then
C               write (0,*) 'KCPPG:ERROR Delta<0'
            endif
            Ene = ((mkch-kk)*(mkch**2-2.d+00*kk*mkch-mp0**2+mpi**2)
     $           -Cteta*kk*dsqrt(delta))
     $           /(2.d+00*((mkch-kk)**2-Cteta**2*kk**2))
            Pp = Ene**2-mpi**2
            If (Pp.lt.0.d+00) then
C               write (0,*) 'KCPPG:ERROR P<0'
C it should not happen
               Pp = 0.d+00
            Endif
            Pp =dsqrt(Pp)
C     Dplot*k/Dmax
            valore = sngl(Pp**2/(mkch*Pp-kk*(Pp-Cteta*Ene))
     $        *((mpi/(Ene-Pp*Cteta))**2+1.d+00
     $        -2.d+00*Ene/(Ene-Pp*Cteta))
     $        *(-4.d+00*mkch**3*mpi**2)
     $        /dsqrt(((mkch**2-mp0**2+mpi**2)/(2.d+00*mkch))**2-mpi**2)
     $        /(mkch**4-2.d+00*(mp0**2+mpi**2)*mkch**2
     $        +(mp0**2-mpi**2)**2))
            If (valore.gt.1.01) then
C               write (0,*) 'KCPPG:ERROR prob>1',valore
            Endif
C hit or miss
            CALL RANLUX(pb,1)
            If (pb(1)*1.01.le.valore) then
               Status = 1
            Endif
         Endif
C End while
      Enddo

      
C-- Prevent underflow problems with optimizer
      if (kk.lt.1.d-12) kk = 0.d+00

C     Kinematics
C     Rotations
C
C     plane Z = 0
C
C     Pi2         Pi1
C     <-----K-----> x axis     ====>   dPhi(k) dPhi(1) dCteta(1)
C           | Cteta
C           |
C           V Gamma
C
C
      CALL RANLUX(rot,3)
C azimuthal angle of the photon around pion 1
      Phik = rot(1)*2.*pi
C azimuthal angle of the pion 1
      Phi1 = rot(2)*2.*pi
C polar angle of pion 1
      C1 = rot(3)*2.-1.
      if ((1.-C1**2).lt.0.) then
        S1 = 0.
      else
        S1 = sqrt(1.-C1**2)
      end if
C     Pion 1 along x direction, plane (x,y)
      Pg1 = kk*Cteta
      Pg2 = kk*Steta*cos(Phik)
      Pg3 = kk*Steta*sin(Phik)
      PE(1) = Pg1*C1 - Pg2*S1
      PE(2) = Pg1*Cos(Phi1)*S1 + Pg2*Cos(Phi1)*C1 - Pg3*Sin(Phi1)
      PE(3) = Pg1*Sin(Phi1)*S1 + Pg2*Sin(Phi1)*C1 + Pg3*Cos(Phi1)
      PE(4) = kk
C     First pion (charged)
      P1(1) = dsqrt(Ene**2-mpi**2)*C1
      P1(2) = dsqrt(Ene**2-mpi**2)*S1*cos(Phi1)
      P1(3) = dsqrt(Ene**2-mpi**2)*S1*sin(Phi1)
      P1(4) = Ene
C     Second pion (neutral)
      P2(1) = - PE(1) - P1(1)
      P2(2) = - PE(2) - P1(2)
      P2(3) = - PE(3) - P1(3)
      P2(4) = mkch - PE(4) - P1(4)
C
C     Gamma
      PCM(1,3) = PE(1)
      PCM(2,3) = PE(2)
      PCM(3,3) = PE(3)
      PCM(4,3) = PE(4)
C     pion 1  charged
      PCM(1,1) = P1(1)
      PCM(2,1) = P1(2)
      PCM(3,1) = P1(3)
      PCM(4,1) = P1(4)
C     pion 2   neutral
      PCM(1,2) = P2(1)
      PCM(2,2) = P2(2)
      PCM(3,2) = P2(3)
      PCM(4,2) = P2(4)
      Return
      End
