      subroutine kch2pi0pi0enu(jkaon,RADFLG,PI0DECAY)
c----------------------------------------------------------------
c
c      Generate Ke4 decay (neutral mode)
c      K+  ---> pi0 pi0 e+ neutrino
c      with the assumption of negl. electron mass
c      only one Form factor with energy variation as measured in
c      charged Ke4 analysis
c      BB december 2009 - december 2010
c======================

#include "common_blocks.f"
#include "masses.f"

      INTEGER RADFLG, pi0decay, pzmode1, pzmode2

      integer j,k,jpi1,jpi2,je,jn,jg
      integer ip01,ip02,ipel,ipnu,ngam,i
      real*8  ppi1(4), ppi2(4), pel(4), pnu(4), dp(4,99)

c  begin
      call Ke4genn

C recover 4 momenta 
      do 10 j = 1, 4
         ppi1(j) = pcm(j,1)
         ppi2(j) = pcm(j,2)
         pel(j)  = pcm(j,3)
         pnu(j)  = pcm(j,4)
 10   continue

C  here we will need PHOTOS call at some point with proper interface
      ngam = 0
      nhep = 5
      IF (RADFLG.EQ.0)    go to 999
c
C.-----     Apply  the radiative corrections in ke4 decay process
C           work in Kaon rest frame to avoid unnecessary Lorentz boosts
C           back and forth by Photos
C.          --------------------------------------

c
c        fill hepevt common block for Photos interface
c        ------------------------
c
c               K  =  first entry
C     ip_mother = 1             ! in MCLIST
      isthep(1) = 2             ! status  2 is decayed ...
      idhep(1)  = 321           ! pdg identifier for Kplus
      jmohep(1,1) = 0           ! no mother for K+/-
      jmohep(2,1) = 0
      jdahep(1,1) = 2           !  first daughter index
      jdahep(2,1) = 5           !  last  daughter index   
      phep(1,1) = 0.
      phep(2,1) = 0.
      phep(3,1) = 0.
      phep(4,1) = mkch
      phep(5,1) = mkch          !  was xkmass ( K0 mass from MDB) fixed by BB
      do i = 1,4
         vhep(i,1) = 0.
      enddo
c
c          4  decay products : pi01 pi02 elec neutrino
c
c ... first pi 0
      isthep(2)   = 1  ! pi0 is stable at this point, decays later
      idhep(2)    = 111 ! pdg identifier for neutral pion
      jmohep(1,2) = 1    ! mother particle is kaon
      jmohep(2,2) = 0
      jdahep(1,2) = 0    !  first daughter index
      jdahep(2,2) = 0    !  last  daughter index
      phep(1,2)   = PPI1(1)
      phep(2,2)   = PPI1(2)
      phep(3,2)   = PPI1(3)
      phep(4,2)   = PPI1(4)
      phep(5,2)   = MP0
      do i = 1,4
         vhep(i,2) = 0.
      enddo
c ... second pi 0
      isthep(3)   = 1  ! pi0 is stable at this point, decays later
      idhep(3)    = 111 ! pdg identifier for neutral pion
      jmohep(1,3) = 1    ! mother particle: kaon
      jmohep(2,3) = 0
      jdahep(1,3) = 0    !  first daughter index
      jdahep(2,3) = 0    !  last  daughter index
      phep(1,3)   = PPI2(1)
      phep(2,3)   = PPI2(2)
      phep(3,3)   = PPI2(3)
      phep(4,3)   = PPI2(4)
      phep(5,3)   = MP0
      do i = 1,4
         vhep(i,3) = 0.
      enddo
c ... electron
      isthep(4)   = 1    ! electron is stable
      idhep(4)    = -11  ! pdg identifier for e+
      jmohep(1,4) = 1    ! mother particle: kaon
      jmohep(2,4) = 0
      jdahep(1,4) = 0    !  first daughter index
      jdahep(2,4) = 0    !  last  daughter index
      phep(1,4)   = Pel(1)
      phep(2,4)   = Pel(2)
      phep(3,4)   = Pel(3)
      phep(4,4)   = Pel(4)
      phep(5,4)   = MEL
      do i = 1,4
         vhep(i,4) = 0.
      enddo
c ... neutrino is needed in HEPEVT record for energy-momentum conservation
      isthep(5)   = 1    ! nu is stable 
      idhep(5)    = 12   ! pdg identifier for neutrino_e
      jmohep(1,5) = 1    ! mother particle: kaon
      jmohep(2,5) = 0
      jdahep(1,5) = 0    !  first daughter index
      jdahep(2,5) = 0    !  last  daughter index
      phep(1,5)   = Pnu(1)
      phep(2,5)   = Pnu(2)
      phep(3,5)   = Pnu(3)
      phep(4,5)   = Pnu(4)
      phep(5,5)   = 0.
      do i = 1,4
         vhep(i,5) = 0.
      enddo
      call photos(1)
      ngam = nhep-5
      if (ngam.eq.0) go to 999 ! nothing has changed
C reload 4-momenta of daughters after radiation, identify particles
      ip01 = 0
      ip02 = 0
      ipel = 0
      IPNU = 0
      do j = 2,nhep
        if (abs(idhep(j)).eq.11) ipel = j
        if (abs(idhep(j)).eq.12) ipnu = j
        if (idhep(j).eq.111) then
           if (ip01.eq.0) then
              ip01 = j
           elseif (ip02.eq.0) then
              ip02 = j
           endif
        endif
      enddo
      do i = 1,4
         Pel(i)  = phep(i,ipel)
         PPI1(i) = phep(i,ip01)
         PPI2(i) = phep(i,ip02)
         pnu(i)  = phep(i,ipnu)
      enddo

 999  continue

C     Save original particles in K rest frame into GeneParts
      jpi1  = mcadd4gen(idpiz,  ppi1, 0)
      jpi2  = mcadd4gen(idpiz,  ppi2, 0)
      je    = mcadd4gen(idelep, pel,  0)
      jn    = mcadd4gen(idnu,   pnu,  0)

c     Boost particles to lab frame
      call dboost( p4ini(1,jkaon), MKCH, ppi1, ppi1 )
      call dboost( p4ini(1,jkaon), MKCH, ppi2, ppi2 )
      call dboost( p4ini(1,jkaon), MKCH, pel, pel )
c      call dboost( p4ini(1,jkaon), MKCH, pnu, pnu )

C add radiative  gammas if any, boost to the lab and add
      if (ngam.gt.0) then
C boost also the photons if any and add to mclist
        do  j = 2,nhep
          if (idhep(j).eq.22) then
            do i = 1,4
              dp(i,j) = phep(i,j)
            enddo
            jg = MCADD4GEN(IDGAM, dp(1,j), 2)
            call DBOOST(P4INI(1,jkaon),MKCH,dp(1,j),dp(1,j))
          endif
        enddo
      endif

C     Register particles for tracking
      jpi1  = mcadd4(idpiz,  ppi1)
      jpi2  = mcadd4(idpiz,  ppi2)
      je    = mcadd4(idelep, pel)
c      jn    = mcadd4(idnu,   pnu)
      if (ngam.gt.0) then
        do  j = 2,nhep
          if (idhep(j).eq.22) jg = MCADD4(IDGAM, dp(1,j))
        enddo
      endif

C --- FORCED PI0 DECAYS

      pzmode1 = mod (pi0decay, 100)
      pzmode2 = mod (pi0decay/100, 100)
      call pi0decay_manager(jpi1, ppi1, pzmode1)
      call pi0decay_manager(jpi2, ppi2, pzmode2)

      return
      end


      SUBROUTINE KE4GENN
c******************************************************************
C
C            M. De Beer  +  R.Turlay
C
C            Generation of ke4 decays
C            modified for tw0 pi0's mode by BB
C            using the empirical description as in JHEP 08 (2014) 159
C           
C******************************************************************

#include "common_blocks.f"
#include "masses.f"

        real*8 PPM2,PPM,ENM2,ENM
        real*8 PIPID(3),PIPIN(3),ENUD(3),ENUN(3)
        real*8 TEMPI(4),TEMPO(4),TEMPM(4)
        real*8 PIPIC(3)
c        real*8 CPIPI(3),CENU(3)
        real*8 VMOD,VDOTN,PITH,ETH,X
        real*8 s2tl
        real*8 spipi,p2,q2,sen,pl
        real*8 wdmx,w,evtwt,wr,fsq,ftotal,bq2
        real*8 gamma,gamma2
        real*8 pi,scal,fs1,fs2,fe,fc,scalpi
C        real*8 wmax
        real*8 RANF
C        common/weimax/wmax(4)
        integer ifoi,iok,i
        data ifoi/0/
        DATA PI  /3.141592653/
        DATA WDMX/0.0083/           !  max value of total weight

        save ifoi       ! to get it back at each call
        ifoi = ifoi +1
        scal = 1.   ! to give a value to fs(0)^2
C  Fs form factor, energy variation as measured in charged Ke4 
        fs1 =  0.152
        fs2 = -0.073
        fe =   0.068
C or better as measured from Ke400 data
        fs1 =  0.149
        fs2 = -0.070
        fe  =  0.113
        fc  = -0.256

C            Generate the decay (phase space in CM system)
C            ------------------ -------------------------

        NP = 4
        tecm = mkch            ! k+- mass PDG 08
        amass(1) = mp0         ! pi0 mass
        amass(2) = mp0         ! pi0 mass
        amass(3) = mel         ! e  mass
        amass(4) = 0.0         ! nu mass
        KGENEV   = 1
C
C THIS ROUTINE GENERATE NEUTRAL KE4 DECAY.
C
 101    continue
        CALL GENBOD_fix(iok)
        if(iok.eq.0)  go  to  101
C
C DEFINE THE 3 KINEMATIC VARIABLES FOR THE DECAY
C---------------------------------------------------
C       1. PI-PI INVARIANT MASS
C       2. E -NU INVARIANT MASS
C       THE REMAINING 3rd VARIABLE is cosTh_el , cosTh_pi is also computed
C       but no physics attached to it
C
c             pipi invariant mass   (ppm)
c             -------------------
c
        PPM2=2.*PCM(4,1)*PCM(4,2)
     1      + AMASS(1)*AMASS(1)
     1      + AMASS(2)*AMASS(2)
     1      - 2.*(PCM(1,1)*PCM(1,2)+PCM(2,1)*PCM(2,2)+PCM(3,1)*PCM(3,2))
        PPM =SQRT(PPM2)
c
c             e - nu  invariant mass  (enm)
c             ----------------------
c  
        ENM2=2.*PCM(4,3)*PCM(4,4)
     1      + AMASS(3)*AMASS(3)
     1      - 2.*(PCM(1,3)*PCM(1,4)+PCM(2,3)*PCM(2,4)+PCM(3,3)*PCM(3,4))
        ENM =SQRT(ENM2)
c
c                theta pions (the cosine is pith)
c                ------------------------------
c
        CALL VZERO(PIPID,3)
        CALL VZERO(PIPIN,3)
        DO 9001 I=1,3
          PIPID(I)=PCM(I,1)+PCM(I,2)
 9001   CONTINUE
        IF( VMOD(PIPID,3).EQ.0.)GOTO                    101
        CALL VUNIT(PIPID,PIPIN,3)
 
        CALL VZERO(TEMPI,4)
        CALL VZERO(TEMPM,4)
        CALL VZERO(TEMPO,4)
C   take the first pi0 to define theta_pi ( arbitrary)
        DO  I=1,4
          TEMPI(I)=PCM(I,1)
        ENDDO
 
        DO I=1,3
          TEMPM(I)=PIPID(I)
        ENDDO

        TEMPM(4)=SQRT(TEMPM(1)**2+TEMPM(2)**2+TEMPM(3)**2+PPM2)
 
        CALL LOREN4(TEMPM,TEMPI,TEMPO)
        IF (VMOD(TEMPO,3).EQ.0.)GOTO                    101
        CALL VZERO(PIPIC,3)
        CALL VUNIT (TEMPO,PIPIC,3)
 
        PITH=VDOTN(TEMPO,TEMPM,3)
c
c
c               theta leptons  ( the cosine is eth)
c               -----------------------------------
c 
        CALL VZERO(ENUD,3)
        CALL VZERO(ENUN,3)
        DO  I=1,3
          ENUD (I)=PCM(I,3)+PCM(I,4)
        ENDDO
        IF(VMOD(ENUD,3).EQ.0.)GOTO                      101
        CALL VUNIT(ENUD,ENUN,3)
 
        CALL VZERO(TEMPI,4)
        CALL VZERO(TEMPM,4)
        CALL VZERO(TEMPO,4)
        DO  I=1,4
          TEMPI(I)=PCM(I,3)
        ENDDO
 
        DO I=1,3
          TEMPM(I)=ENUD(I)
        ENDDO
        TEMPM(4)=SQRT(TEMPM(1)**2+TEMPM(2)**2+TEMPM(3)**2+ENM2)
 
        CALL LOREN4(TEMPM,TEMPI,TEMPO)
        IF(VMOD(TEMPO,3).EQ.0.)GOTO                     101
 
        ETH =VDOTN(TEMPO,TEMPM,3)


        s2tl = 1.- eth*eth   ! sin2 = 1 - cos2

C       HERE COMES THE FORM-FACTOR:
c       only one in this case = Fs
c           hadronic invariants
c           -------------------
c
        spipi = ppm2
        p2 = -ppm2

        sen = enm2
        pl = -0.5*(tecm**2 - spipi - sen )
        x = sqrt( pl**2 - (spipi * sen) )

        gamma = x / (tecm**2 )
        gamma2 = gamma**2
c     
c       decay amplitude : same for K+ and K- 
c
       ftotal = gamma2 * s2tl        

       scalpi = 4.*mpi*mpi
C  fs variable ( q2 is to be measured with the charge Ke4 scale =pi+mass)
C  q2 = Spi/4mpi+^2  -1   and qe2 = Se/4mpi+^2
       bq2 =   (spipi/scalpi)  -1. 
C introduce parametrisation as measured
       if (bq2.gt.0.) then  ! above threshold 
         fsq = (1.+ fs1*bq2 + fs2*bq2*bq2 + fe*sen/scalpi)**2
       else   ! below threshold  
         fsq = (1.+ fc*sqrt(abs(bq2/(1+bq2))) + fe*sen/scalpi)**2
       endif

       ftotal = ftotal*fsq*scal

       if (WT*FTOTAL.gt.wdmx) then
         write(0,*) 'warning,wt*ftot,max',wt*ftotal,wdmx,' no update'
       endif 
c
c         the phase space weight ( wt ) is corrected by using
c         the computed matrix element (ftotal)
c
        EVTWT =WT*FTOTAL
        W   =EVTWT
c
c          is the generated event  a good candidate   ???
c
        WR  =WDMX*ranf()
        IF(WR.GT.W)GOTO                                 101
c
c          yes  it is ..... ( ouf !!!! )
c
c
C   fill true values of cama variables in particle 5
       pcm(1,5) = ppm
       pcm(2,5) = enm
       pcm(3,5) = pith
       pcm(4,5) = eth
       pcm(5,5) = 0.
       np = 5 

       RETURN
       END
