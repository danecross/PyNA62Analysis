      subroutine kch2pipienu(jkaon,RADFLG)
c----------------------------------------------------------------
C
C    generate Ke4 decays in K rest frame
C    call Photos and store gammas if any
*===================

#include "common_blocks.f"
#include "masses.f"

      INTEGER RADFLG
      logical kplus, kminus, radcor
      integer jpi1, jpi2, je, j
      Integer jgamma
      Integer jn
      Real*8 pnu(4),dp(4,99)

      Integer nhep_before,nhep_after,nhep_rad
      Integer ip_mother,n_decay,ip_last,i

      REAL WCOUL,WCOUL1,WCOUL2,WCOUL3,bet1,bet2,bet3
      COMMON/COULOMB/WCOUL,WCOUL1,WCOUL2,WCOUL3,bet1,bet2,bet3

      INTEGER IPION1,IPION2,IELEC
      real*8  ppi1(4), ppi2(4), pel(4)

      real*8 sumcm,sumlab,ecmin
      integer ifoi,ikaon,imopion,imolept,nh,ipi1,ipi2,ipnu,ipel
      data ifoi/0/,ecmin/5./
      save ifoi

      ifoi = ifoi +1

      kplus  = .true.
      kminus = .false.
      radcor = .false.
      if (radflg.gt.0) radcor = .true.

c     Ke4 decay.............

      call Ke4genc
 
      do 10 j = 1, 4
          ppi1(j) = pcm(j,1)
          ppi2(j) = pcm(j,2)
          pel(j) = pcm(j,3)
          pnu(j) = pcm(j,4)
 10   continue

c     Store particles in /MCLIST/..........as in K rest frame

      if (kplus)   then
       ipion1 = idpip
       ipion2 = idpim
       ielec = idelep
      endif
      if (kminus)  then
       ipion1 = idpim
       ipion2 = idpip
       ielec = idelem
      endif

      nhep_before = 5
      nhep_after = nhep_before

      IF ( .NOT. RADCOR )    go to 999
       
c
C.-----     Apply  the radiative corrections in ke4 decay process
C           work in Kaon rest frame
C.          --------------------------------------
c
c        fill hepevt common block
c        ------------------------
c
         ip_last = npart
         n_decay = 4
         nhep = 1 + n_decay     ! 5 particles (K pi+ pi- e neu)
         nhep_before = nhep
c               K   first
         ip_mother = 1         ! in MCLIST
         isthep(1) = 2    !    was 1 ( =stable) 2 is decayed ...
         idhep(1)  = 321   ! pdg identifier for Kplus
         if(kminus)  idhep(1) = -321
         jmohep(1,1) = 0   ! no mother for K+/-
         jmohep(2,1) = 0
         jdahep(1,1) = 2   !  first daughter index
         jdahep(2,1) = 5   !  last  daughter index   BB: update 4 to 5
         phep(1,1) = 0.
         phep(2,1) = 0.
         phep(3,1) = 0.
         phep(4,1) = mkch
         phep(5,1) = mkch   !  was xkmass ( K0 mass from MDB) fixed by BB
         do i = 1,4
            vhep(i,1) = 0.
         enddo 
c
c          4  decay products :
c
C--------  pion   pion   electron+/- and neutrino

         nhep = 1
         nhep_before = nhep
         ikaon = 1    
         nh = 1                        
C          if ( ifoi.le.5)     print *,' filling pions'
            imopion = ikaon
            call addhep(2,3,imopion,0)
            nh =  nhep 
            ipi2 =  nhep
            ipi1 =  nhep-1
C         if ( ifoi.le.5)call phodmp
C         if ( ifoi.le.5) print *,' filling leptons'
           imolept = ikaon
           call addhep(4,5,imolept,0)
           jdahep(2,ikaon) = nhep   !  last  daughter index
           nh =  nhep 
           ipnu =  nhep
           ipel =  nhep-1
         if ( ifoi.le.2)  call phodmp

c
         nhep_before = nhep
c
         call photos(1)
c
         nhep_after = nhep
c        if ( ifoi.le.3)  print*,' nhep_after =',nhep_after
c
c              something new in the decay  ????
c              if yes record the new parameters
c              in MCLIST common block
c
         nhep_rad = 0
         if(nhep_after.gt.nhep_before)   then
           nhep_rad = nhep_after - nhep_before ! number of rad particles
C. in case of intermediate states ( w, rho..) the photons appear in the
C  middle of the eventt record ==> careful with updates !!!
c           update parameters of the decay products in MCLIST
           do j=2,nhep
           if (kplus) then
             if(idhep(j).eq. 211) ipi1=j
             if(idhep(j).eq.-211) ipi2=j
             if(idhep(j).eq.-11)  ipel=j
             if(idhep(j).eq. 12)  ipnu=j
           elseif(kminus) then
             if(idhep(j).eq.-211) ipi1=j
             if(idhep(j).eq. 211) ipi2=j
             if(idhep(j).eq. 11)  ipel=j
             if(idhep(j).eq.-12)  ipnu=j
           endif
           enddo
c            if ( ifoi.le.3) then
c              print*,' rest frame radiative = ',nhep_rad
c              call phodmp
c              print*,' update pion1,2 from position ',ipi1,ipi2
c              print*,' update elec,neut from position ',ipel,ipnu
c            endif
           do i = 1,4
             ppi1(i) = phep(i,ipi1)     !  pion 1
             ppi2(i) = phep(i,ipi2)     !  pion 2
             pel (i) = phep(i,ipel)     !  el
             pnu (i) = phep(i,ipnu)     !  nu
           enddo
        endif
 999   continue

C     Save original particles in K rest frame into GeneParts
      jpi1  = mcadd4gen(ipion1, ppi1, 0)
      jpi2  = mcadd4gen(ipion2, ppi2, 0)
      je    = mcadd4gen(ielec,  pel,  0)
      jn    = mcadd4gen(idnu,   pnu,  0)

C boost to the lab
      call dboost( p4ini(1,jkaon), MKCH, ppi1, ppi1 )
      call dboost( p4ini(1,jkaon), MKCH, ppi2, ppi2 )
      call dboost( p4ini(1,jkaon), MKCH, pel, pel )
c      call dboost( p4ini(1,jkaon), MKCH, pnu, pnu )

       if(nhep_after.gt.nhep_before) then
C
C.          add radiative gammas in MCLISt after boost
c
          sumcm = 0.
          sumlab = 0.
          do i = 2,nhep
            if(idhep(i).eq.22) then
              do j=1,4
                dp(j,i) = phep(j,i)
              enddo
              sumcm = sumcm + dp(4,i)
              if(dp(4,i).lt.ecmin) then
c                print *,' new min Ecm ',dp(4,i)
                ecmin = dp(4,i)
              endif
              jgamma = mcadd4gen(idgam,dp(1,i),2)
c              if (ifoi.le.3) print *,'E photon cm ',dp(4,i)
              call dboost(p4ini(1,jkaon),mkch,dp(1,i),dp(1,i))
c              if (ifoi.le.3) print *,'E photon lab ',dp(4,i)
              ip_last = ip_last + 1
              sumlab = sumlab + dp(4,i)
            endif
          enddo

C         npart = ip_last
      endif  !  end nhep_after > nhep_before

C     Register particles for tracking
      jpi1 = mcadd4 (IPION1, ppi1)
      jpi2 = mcadd4 (IPION2, ppi2)
      je   = mcadd4 (ielec,  pel)
c      jn   = mcadd4 (idnu,   pnu)
      if(nhep_after.gt.nhep_before) then
        do i = 2,nhep
          if(idhep(i).eq.22) jgamma = mcadd4(idgam,dp(1,i))
        enddo
      endif

c      if(ifoi.le.2) print *,' final number of part ',npart
c      if(ifoi.le.1) call mcdump

      return
      end


       SUBROUTINE KE4GENC
c******************************************************************
C
C            M. De Beer  +  R.Turlay
C
C            Generation of ke4 decays
C
C              Modified  Nov 10,2003  G-Marel
C              and more since then by BB
C******************************************************************	

#include "common_blocks.f"
#include "masses.f"

        real*8 pcm8(5,4)
        real*8 enm28,ppm28,enm8,ppm8
 
        real*8 PIPID(3),PIPIN(3),ENUD(3),ENUN(3)
        real*8 PIPIC(3),ENUC(3)
        real*8 CPIPI(3),CENU(3)
        real*8 TEMP1(3),TEMP2(3)

        real*8 ctrpi(3),ctrnu(3),ctpipin(3)
        real*8 PPM2,PPM,ENM2,ENM
        real*8 sin2d,ak,aq2,sm4mpi2
        real*8 a00,a20,x,acgld,berncor,coriso
        real*8 cij(3),DP1(4),DP2(4),DP3(4),wtcoul
        real*8 fsq
        real*8 aux
        real*8 g_bar0,h_bar0,g_slop,b00,delta,scal,gprim_bar
        real*8 wmax
        common/weimax/wmax(4)
        integer ifoi,ivers,icoul,isocor,iset,iok,i,j,iprim
        real*8 TEMPI(4),TEMPO(4),TEMPM(4)
        real*8 tl,ctl,c2tl,stl,s2tl,tpi,ctpi,stpi,s2tpi,spipi,p2
        real*8 alpi,alnu,cosphi,sinphi,aphif,cphi,c2phi,sphi,s2phi 
        real*8 c2tpi,q2,sen,pl,ql,bq2,bprim
        real*8 b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12
        real*8 cj1,cj2,cj3,cj4,cj5,cj6,cj7,cj8,cj9,cj10,cj11,cj12
        real*8 pith,eth,wdmx,w,evtwt,wr,ftotal
        real*8 vmod,vdotn
        real*8 alpha,alpha2,beta,beta2,gamma,gamma2
c        real*8 xpi
c        parameter(xpi =3.141592653) 
        real*8 dftot
        real*8 fs,fs1,fs2,fe,g_bar,h_bar,fp_bar,r_bar,g0,slog,f0
cc        common/ffaux/iset,fs1,fs2,fe,f0,g_bar,h_bar,fp_bar,r_bar,g0,slog
        real*8 ph00,ph11,ph20
cc        common/phasaux/ph00,ph11,ph20

        real*8 RANF

        data ifoi/0/
c
c     max total weight for rejection
       Data WDMX / 0.016  / 

      save ifoi
      ifoi = ifoi+1

      iset = 2    ! best NA48 measurements
C      scal = 5.75*5.75   ! to give a value to fs(0)^2 for BR integration
      scal = 1.
      if(iset.eq.1) then  ! FF setup with S118 values
       f0 = 1.
       fs1 =0.
       fs2 = 0.
       fe = 0.
       fp_bar = 0.
       g_bar0 = 0.93
       g_slop = 0.
       gprim_bar = 0.84
       iprim = 1 ! grprim is to be used as such
       h_bar0 = -0.37
       a00 = 0.25
       b00 = 0.19-(a00-0.15)**2   ! slope from constrained fit
       g_slop = 0.
       ivers = 0          !  to use BFP param of delta
      elseif(iset.eq.2) then  !  best NA48 measurements
       f0 = 1.
       fs1 = 0.152
       fs2 = -0.073
       fe = 0.068
       fp_bar = -0.048
       g_bar0 = 0.868  !  
       g_slop = 0.089  !  
       gprim_bar = 0.
       iprim = 0        ! gprim to be computed grom g anf fp on the fly
       h_bar0 = -0.398  ! 
       a00 = 0.25                ! 
       b00 = 0.19-(a00-0.15)**2  !
       ivers = 2 ! 0 to use BFP,   1 to use ACGL, 2 to use ACGL + isospin
      endif
C
C            Generate the decay (phase space in CM system)
C            ------------------ -------------------------
c
        NP = 4
        tecm     = mkch
        amass(1) = mpi
        amass(2) = mpi
        amass(3) = mel        ! e  mass
        amass(4) = .0             ! nu mass
        KGENEV   = 1
C
C GENERATE   KE4 DECAY charged K  and pi+ pi- final state
C REFERENCES: PHYS. REV. 168,1858; PAIS AND TREIMAN
C             PHYS. REV. 137,B438; CABIBBO AND MAKSYMOWICZ
C             
C
 101    continue
        CALL GENBOD_fix(iok)
        if(iok.eq.0)  go  to  101

      do i = 1,4
      do j = 1,5
        pcm8(j,i) = pcm(j,i)
      enddo
      enddo
C
C DEFINE THE FIVE KINEMATICS VARIABLES FOR THE DECAY
C---------------------------------------------------
C       1. PI-PI INVARIANT MASS
C       2. E -NU INVARIANT MASS
C       THE REMAINING 3 VARIABLE DEFINITIONS ARE EASY TO UNDERSTAND IF
C       ONE LOOK UP REF 1,2.
C
c             pipi invariant mass   (ppm)
c             -------------------
       ppm28 = 2.*pcm8(4,1)*PCM8(4,2)
     1      + AMASS(1)*AMASS(1)
     1      + AMASS(2)*AMASS(2)
     1 -2.*(PCM8(1,1)*PCM8(1,2)+PCM8(2,1)*PCM8(2,2)+PCM8(3,1)*PCM8(3,2))
        PPM8 =SQRT(PPM28)
c
        PPM2= ppm28
        PPM = ppm8
c
c             e - nu  invariant mass  (enm)
c             ----------------------
        ENM28=2.*PCM8(4,3)*PCM8(4,4)
     1      + AMASS(3)*AMASS(3)
     1 -2.*(PCM8(1,3)*PCM8(1,4)+PCM8(2,3)*PCM8(2,4)+PCM8(3,3)*PCM8(3,4))
        ENM8 =SQRT(ENM28)

        ENM2= enm28
        ENM = enm8
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
        DO 9002 I=1,4
          TEMPI(I)=PCM(I,1)
 9002   CONTINUE

        DO 9003 I=1,3
          TEMPM(I)=PIPID(I)
 9003   CONTINUE
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
        DO 9011 I=1,3
          ENUD (I)=PCM(I,3)+PCM(I,4)
 9011   CONTINUE
        IF(VMOD(ENUD,3).EQ.0.)GOTO                      101
        CALL VUNIT(ENUD,ENUN,3)

        CALL VZERO(TEMPI,4)
        CALL VZERO(TEMPM,4)
        CALL VZERO(TEMPO,4)
        DO 9012 I=1,4
          TEMPI(I)=PCM(I,3)
 9012   CONTINUE

        DO 9013 I=1,3
          TEMPM(I)=ENUD(I)
 9013   CONTINUE
        TEMPM(4)=SQRT(TEMPM(1)**2+TEMPM(2)**2+TEMPM(3)**2+ENM2)

        CALL LOREN4(TEMPM,TEMPI,TEMPO)
        IF(VMOD(TEMPO,3).EQ.0.)GOTO                     101
        CALL VZERO(ENUC,3)
        CALL VUNIT(TEMPO,ENUC,3)

        ETH =VDOTN(TEMPO,TEMPM,3)
C
C       THE LAST ANGLE !  ( APHI)
C       ----------------
c
        CALL VZERO(TEMP1,3)
        CALL VZERO(TEMP2,3)
        CALL VZERO(CPIPI,3)
        CALL VZERO(CENU ,3)

        DO 921 I=1,3
          TEMP1(I)=PCM  (I,1)
          TEMP2(I)=PCM  (I,2)
 921    CONTINUE
        IF(VMOD(TEMP1,3).EQ.0..OR.VMOD(TEMP2,3).EQ.0.)GOTO 101
        CALL CROSS (TEMP1,TEMP2,CPIPI)

        DO 922 I=1,3
          TEMP1(I)=PCM  (I,3)
          TEMP2(I)=PCM  (I,4)
 922    CONTINUE
        IF(VMOD(TEMP1,3).EQ.0..OR.VMOD(TEMP2,3).EQ.0.)GOTO 101
        CALL CROSS (TEMP1,TEMP2,CENU)
*
* --- 05/08/2004
*
* --- Correct aphif definition according to Cabibbo-Maksymowicz convention and
*     Pais-Treiman formulation 
*
         alpi = vdotn(pipin,pipic,3)
         alnu = vdotn(pipin,enuc ,3)
         do i = 1,3
          ctrpi(i) = pipic(i) - alpi*pipin(i)
          ctrnu(i) = enuc(i)  - alnu*pipin(i)
         enddo

         cosphi = vdotn(ctrpi,ctrnu,3)
         call cross(ctrpi,pipin,ctpipin)
         sinphi = vdotn(ctpipin,ctrnu,3)
         aphif = atan2(sinphi,cosphi)
c
c             For K- generation , follow the LEE - WU prescription
c             see paper : Ann.Rev.Nucl.Sci. 16,471 (1966)
c             equation 8.54
c
c      if(kminus)    then
c        aphif = aphif + xpi
c      endif
*
        cphi = cos(aphif)
        c2phi = cphi**2
        sphi = sin(aphif)
        s2phi = sphi**2

        tl  = acos(eth)    !   theta lepton

      h_bar = h_bar0
c      if(kminus)    then
c         tl = xpi - tl
c         h_bar = -h_bar0
c      endif
        ctl = cos(tl)
        c2tl = ctl**2
        stl = sin(tl)
        s2tl = stl**2
c
        tpi = acos(pith)   !   theta pion
        ctpi = cos(tpi)
        c2tpi = ctpi**2
        stpi = sin(tpi)
        s2tpi = stpi**2
C
c
c           hadronic invariants
c           -------------------
c
        spipi = ppm2
        p2 = -ppm2
        q2 = spipi - 4.*amass(1)**2
        sen = enm2
        
        pl = -0.5*(tecm**2 - spipi - sen )
        x = sqrt( pl**2 - (spipi * sen) )
        ql = -sqrt(q2/spipi) * x * cos(tpi)
c
        alpha = -sqrt(q2/spipi)
        alpha = ( alpha * pl ) / ( tecm**2 )
        alpha2 = alpha**2
        beta = sqrt(q2*sen)
        beta = beta / (tecm**2 )
        beta2 = beta**2
        gamma = x / (tecm**2 )
        gamma2 = gamma**2
C        print*,' alpha ..',alpha,beta,gamma
c  compute amplitude as Pais-Treiman    Phys Rev 168(1968) 1858   
c        12  functions
c        -------------
C       here ze = me^2/Se = 0
c
        b1 = gamma2 * s2tl
        b2 = beta2 * s2tpi * (1.-s2tl*c2phi)
        b3 = alpha2 * c2tpi * s2tl
        b4 = beta2 * gamma2 * s2tpi * (1.-s2tl*s2phi)
        b5 = 2. * alpha * gamma * ctpi * s2tl
        b6 = 2. * beta * gamma * stpi * stl * ctl * cphi
        b7 = -2. * beta * gamma2 * stpi * stl * cphi
        b8 = 2. * alpha * beta * stpi * ctpi * stl * ctl * cphi
        b9 = -2. * alpha * beta * gamma * stpi * ctpi * stl * cphi
        b10 = -2. * beta2 * gamma * s2tpi * ctl
        b11 = 2. * beta * gamma * stpi * stl * sphi
        b12 = -2. * beta * gamma2 * stpi * stl * ctl * sphi
        bq2 = q2/(4.*amass(1)**2)
        g_bar = g_bar0 + g_slop*bq2   ! test 1 slope for g
        if(iprim.lt.1) gprim_bar = g_bar + gamma*fp_bar/alpha ! from g anf fp
        if(iprim.eq.1) fp_bar = (gprim_bar - g_bar)*alpha/gamma
c
c         12  coeffs
c         -----------
c
c
c            delta is now a function of spipi
c            sin(2delta) = 2k * (a00 + b'q2)
c
        sm4mpi2 = spipi - 4.*(amass(1)**2)
        ak = sqrt(sm4mpi2/spipi)
        aq2 = sm4mpi2/(4.*amass(1)**2)
C
        bprim = 0.19 - (a00-0.15)**2
        bprim = b00 !  to use the 2p fit or a 1p fit already computed  
      if (ivers.eq.0) then    ! BFP param for delta 
          sin2d = 2.*ak*(a00+bprim*aq2)
C         if (sin2d.gt.1.) print *,' ==sin2d ',ak,aq2,a00+bprim*aq2,sin2d
         if(abs(sin2d).gt.0.98) goto 101
          delta = 0.5*asin(sin2d)
C description of delta according to latest ACGL param =Phys Rep 353 (2001)
      else     ! ACGL param for delta
        isocor = ivers - 1
        if (isocor.eq.0) then   ! effective scattering lengths
          a00 = 0.2425
          a20 = -0.0395
        else                    ! true scattering lengths
          a00 = 0.220
          a20 = -0.0444
        endif
        x = ppm
        delta = acgld(a00,a20,x,1)  ! computes phases and phase shift
        coriso = 0.
        if (isocor.gt.0) then  !   isospin corrections
          x = spipi
          coriso = berncor(x)
          delta = delta+coriso
          ph00 = ph00 + coriso
        endif
      endif
*
       fs = 1.+ fs1 *bq2 + fs2*bq2*bq2+ fe*sen/(4.*amass(1)**2)
       fsq = fs*fs
C        print *,' paistre g,gpr,fp,h ',g_bar,gprim_bar,fp_bar,h_bar 
        cj1 = fsq
        cj2 = g_bar**2
        cj3 = gprim_bar**2
        cj4 = h_bar**2
        cj5 = fs*gprim_bar * cos(delta)
        cj6 = fs*g_bar * cos(delta)
        cj7 = fs*h_bar * cos(delta)
        cj8 = g_bar * gprim_bar 
        cj9 = gprim_bar * h_bar 
        cj10 = g_bar * h_bar 
        cj11 = fs*g_bar * sin(delta)
        cj12 = fs*h_bar * sin(delta)
c
        ftotal = cj1*b1 + cj2*b2 + cj3*b3 + cj4*b4 + cj5*b5
     >         + cj6*b6 + cj7*b7 + cj8*b8 + cj9*b9 + cj10*b10
     >         + cj11*b11 + cj12*b12 

c    form factor vs q2
c       aux = fs
c       call hfill(301,bq2,aux,1.)
c       aux = fp_bar
c       call hfill(302,bq2,aux,1.)
c       aux = g_bar
c       call hfill(303,bq2,aux,1.)
c       aux = h_bar
c       call hfill(304,bq2,aux,1.)
c       aux = wt
c       call hfill(101,aux,dum,1.)   ! Phase space weight
c       aux = fsq
c       call hfill(102,aux,dum,1.)
c       call hfill(104,ftotal,dum,1.)

       dftot = ftotal*scal

c      if(ifoi.lt.2) then
c       print *,' ==ke4genc: iset,g0,h0,gprim,slope ',iset,g_bar0,h_bar0,
c     &    gprim_bar,g_slop
c       print *,' ==ke4genc: g,gp,fp,h',g_bar,gprim_bar,fp_bar,h_bar
c       print *,' ==ke4genc: isocor,a0,a2,coriso,d ',
c     &  isocor,a00,a20,coriso,delta,ph00,ph11
c      endif

C   Coulomb correction
      
       icoul = 1

      if(ifoi.eq.0) then
       wdmx = wdmx*scal
      endif

      wtcoul = 1.
      if(icoul.gt.0) then
        do i= 1,4
         dp1(i)= pcm8(i,1)
         dp2(i)= pcm8(i,2)
         dp3(i)= pcm8(i,3)
        enddo

C coulomb corrections, give it a try
        call corcoul2(1,dp1,dp2,dp3,cij)
        wtcoul = cij(1)*cij(2)*cij(3)
c        aux = wtcoul
c        call hfill(103,aux,dum,1.)
c        aux = cij(1)
c        call hfill(111,aux,dum,1.)
c        aux = cij(2)
c        call hfill(112,aux,dum,1.)
c        aux = cij(3)
c        call hfill(113,aux,dum,1.)

      endif
        aux = wt
        if (aux.gt.wmax(1)) then
c           print *,' ++ event ',ifoi,' max phase space weight ',aux
           wmax(1) = aux
        endif
        if (dftot.gt.wmax(2)) then
c           print *,' ++ event ',ifoi,' max FF weight ',dftot
           wmax(2) = dftot
        endif
        if (wtcoul.gt.wmax(3)) then
c           print *,' ++ event ',ifoi,' max coulomb weight ',wtcoul
           wmax(3) = wtcoul
        endif
c
c         the phase space weight ( wt ) is corrected by using
c         the computed matrix element (dftot) and the coulomb factor
c

        EVTWT =WT*dftot*wtcoul

c        aux = evtwt
c        call hfill(107,aux,dum,1.)

       if (evtWT.gt.wmax(4)) then
c         print *,' ++ event ',ifoi,' max total weight ',wt*dftot*wtcoul
         wmax(4) = wt*dftot*wtcoul
        endif

      if (evtwt.gt.wdmx) then
      write (0,*) 'warning wt*ftot*coul ',wt,dftot,wtcoul,evtwt,wdmx
      endif


        W   =EVTWT         ! phase space * physic
 102    continue
c
c          is the generated event  a good candidate   ???
c
        WR  =WDMX*ranf()
        IF(WR.GT.W)GOTO                                 101
c
c          yes  it is ..... ( ouf !!!! )
c
c
c     if (kminus) then    ! store the original value
c        aphif = aphif - xpi
c     endif

c  keep the original true values
      pcm(1,5) = ppm
      pcm(2,5) = enm
      pcm(3,5) = pith
      pcm(4,5) = eth
      pcm(5,5) = aphif
      np = 5                 

C some control histos
c      aux = ppm*ppm
c      call hfill(201,aux,dum,1.)
c      aux = enm*enm
c      call hfill(202,aux,dum,1.)
c      aux = pith
c      call hfill(203,aux,dum,1.)
c      aux = eth
c      call hfill(204,aux,dum,1.)
c      aux = aphif/xpi
c      call hfill(205,aux,dum,1.)
c      if (bq2.lt.0.2) then
c          call hfill(206,aux,dum,1.)
c      elseif (bq2.gt.0.6) then      
c          call hfill(207,aux,dum,1.)
c      endif
c      aux = ph00
c      call hfill(401,bq2,aux,1.)
c      aux = ph11
c      call hfill(402,bq2,aux,1.)
c      call hfill(403,bq2,aux,1.)
c      call hfill(404,bq2,aux,1.)
c      call hfill(405,bq2,aux,1.)

C        if(ifoi .le.5) then
C            print *,' cama true ',ppm,enm,pith,eth,aphif
C        endif
        RETURN
        END


      subroutine corcoul2(ifl,dp1,dp2,dp3,cij)
      implicit none
C   Coulomb correction for the 3 charge combinations
C   part 1, 2 are pions +/-
C   third part is lepton e (ifl=1), mu (ifl=2)
C
      real*8 DP1(4),DP2(4),DP3(4),CIJ(3)
      real*8 pij,vij(3),wtcoul,xm1,xm2,xm3
      real*8 aux
      integer i,ifl
      real*8 pi,alphas,xmpi,xmel,xmmu
      parameter(pi = 3.141592653589, alphas=0.007297352533 )
#include "masses.f"
      parameter(xmpi = MPI, xmel = MEL, xmmu = MMU)

C coulomb corrections, give it a try
C compute relative velocities
       pij = 0.
       xm1 = xmpi
       xm2 = xmpi
       if (ifl.eq.1) xm3 = xmel
       if (ifl.eq.2) xm3 = xmmu

       pij = 0.
       do  i=1,3
          pij = pij +dp1(i)*dp2(i)
       enddo
        pij = dp1(4)*dp2(4)-pij
        pij = sqrt(1.-(xm1*xm2/pij)**2)
        vij(1) = pij

       pij = 0.
       do  i=1,3
          pij = pij +dp2(i)*dp3(i)
       enddo
        pij = dp2(4)*dp3(4)-pij
        pij = sqrt(1.-(xm2*xm3/pij)**2)
        vij(2) = pij

       pij = 0.
       do  i=1,3
          pij = pij +dp3(i)*dp1(i)
       enddo
        pij = dp3(4)*dp1(4)-pij
        pij = sqrt(1.-(xm3*xm1/pij)**2)
        vij(3) = pij

C  product of 3 terms   1= pi+ 2 = pi- 3 = e+  ( for K+)
C                       1= pi- 2 = pi+ 3 = e-  ( for K-)
C  product of charges is -,-,+ for 12,23,31
      do i = 1,3
      wtcoul = -2.*pi*alphas/vij(i)
      if(i.eq.3) wtcoul = -wtcoul
      cij(i) = wtcoul/(exp(wtcoul)-1.)
      enddo

C     print *,' coulomb beta, cij ',cij
      return
      end

      subroutine addhep(i1,i2,imo,iup)
c
*===================
*
      IMPLICIT NONE
*
c===========   FILLED IN PHOTOS
      INTEGER NMXHEP
      PARAMETER (NMXHEP=2000)
      INTEGER IDHEP,ISTHEP,JDAHEP,JMOHEP,NEVHEP,NHEP
      REAL*8 PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
*
C======================
      INTEGER   MXPART,   NONE,  TERM , IFIRST_PART
      PARAMETER(MXPART=500,NONE=0,TERM=-1)
      INTEGER MCQUIT,MPART,EVTVAL,PARVAL,PID,PARENT,CHILD,PARTIME
      INTEGER IPARTVER,IMED
      INTEGER MXPTRA
      INTEGER NACCI,NACCI_CODE(MXPART)
      REAL EVTWT,EINI,PINI,DIRINI,EPART,PPART,DIR,XYZ,CURVERT,IPREV_VERT
      REAL TACCI,EVT_TIME
      REAL*8 P4INI
      REAL ESIMP(MXPART)
C            MPART   : NUMBER OF PARTICLES IN THE PARTICLE LIST
C            MXPTRA  : NUMBER OF PARTICLES TO BE TRACKED
C            EINI    : INITIAL ENERGY OF THE PARTICLE
C            PINI    : INITIAL MOMENTUM OF THE PARTICLE
C            DIRINI  : INITIAL DIRECTION OF THE PARTICLE (DZ=+-1)
C            P4INI   : INITIAL 4-VECTOR OF THE PARTICLE
C            PARTIME : TIME STAMP OF THE PARTICLE (NANOSECOND)
C                      0 = ON TIME
C            EVT_TIME : time of the event
C            TACCI   : TIME OF PREVIOUS ACCIDENTAL ( SEE ACCIDE )
      COMMON/MCLIST/MCQUIT,EVTWT,MPART,MXPTRA,EVTVAL,PARVAL(MXPART),
     +               PID(MXPART),PARENT(MXPART),CHILD(MXPART),
     +               EINI(MXPART),PINI(MXPART),DIRINI(3,MXPART),
     +               EPART(MXPART),PPART(MXPART),DIR(3,MXPART),
     +               XYZ(3,MXPART),PARTIME(MXPART),TACCI,EVT_TIME,
     +               CURVERT,IPREV_VERT,IFIRST_PART,IPARTVER(MXPART),
     +               IMED(MXPART),
     +               NACCI,NACCI_CODE
      COMMON / MCP4IN / P4INI(4,MXPART)
      COMMON / MCHHHH / ESIMP
      REAL MCPARTSPIN
      COMMON / MCSPIN / MCPARTSPIN(3,MXPART)      
      integer       NP,KGENEV
      real          TECM,AMASS
      COMMON/GENIN /NP,TECM,AMASS(18),KGENEV
      real  pcm,wt
      COMMON/GENOUT/PCM(5,18),WT
      integer i1,i2,imo,iup,i,k
      if (iup.gt.0) then
C   reset the resulting particle 5-vector  
         do k=1,5     
           phep(k,imo)= 0.           
         enddo           
      endif
      do i = i1,i2
        nhep = nhep+1
C        print *,' now filling entry ',nhep

           isthep(nhep) = 1
           jmohep(1,nhep) = imo
           jmohep(2,nhep) = 0
           jdahep(1,nhep) = 0
           jdahep(2,nhep) = 0
           do k=1,4 
              phep(k,nhep) = pcm(k,i-1)
           enddo 
           phep(5,nhep) = amass(i-1)
           vhep(1,nhep) = 0.
           vhep(2,nhep) = 0.
           vhep(3,nhep) = 0.
           vhep(4,nhep) = 0.
           idhep(nhep) = 211     ! pi +   (default)
           if(pid(i).eq.8) idhep(nhep) =  211     ! pi+
           if(pid(i).eq.9) idhep(nhep) = -211     ! pi-
           if(pid(i).eq.2) idhep(nhep) =  -11     ! e+ 
           if(pid(i).eq.3) idhep(nhep) =   11     ! e-
           if(pid(i).eq.4) idhep(nhep) =   12*sign(1,idhep(1)) ! neut
           if (iup.gt.0) then
C   compute the resulting particle 5-vector  
              do k=1,4     
                phep(k,imo)=phep(k,imo)+pcm(k,i-1)             
              enddo           
           endif
      enddo
      if (iup.gt.0) then
          phep(5,imo)=sqrt(phep(4,imo)**2-phep(1,imo)**2
     &                   -phep(2,imo)**2-phep(3,imo)**2)
          jdahep(2,imo) = nhep
          jdahep(1,imo) = nhep-i2+i1
      endif
C      print *,' nhep after filling ',nhep
      return
      end

      real*8 function acgld(a00,a20,x,ivers)
      implicit none
C compute phase shift and store phases in common /phasaux/
      integer ifl,ivers
      real*8 a00,a20,x
      real*8 d00,d002,d003
      real*8 p00,p11,p20
      real*8 ph00,ph11,ph20
      common/phasaux/ph00,ph11,ph20
      ifl =ivers

      if(ifl.le.0) then  ! UB center, top and bottom
         if(ifl.eq.0)   a20 = -0.0849 +0.232*a00 -0.0865*a00*a00
         if(ifl.eq.-10) a20 = -0.0774 +0.240*a00 -0.0881*a00*a00
         if(ifl.eq.-20) a20 = -0.0922 +0.225*a00 -0.0847*a00*a00
      elseif(ifl.eq.2) then   ! ChPT central band
         d00 = a00 - 0.220
         d002 = d00*d00
         d003 = d002*d00
         a20 = 0.236*d00 -0.61*d002 -9.9*d003  - 0.0444
      endif
C   ifl = 1 no constraint, use a0 and a2 values
       call thephases(x,a00,a20,p00,p11,p20)
C
      acgld = p00-p11
C store individual phases as well
      ph00 = p00
      ph11 = p11
      ph20 = p20
      return
      end

      subroutine thephases(ene,a0,a2,ph00,ph11,ph20)
c------------------------------------------------
c ene in units of GeV
      implicit double precision (a-h,o-z)
      dimension          xb00(10),xc00(10),xd00(10),xs00(10)
      dimension xa11(10),xb11(10),xc11(10),xd11(10),xs11(10)
      dimension          xb20(10),xc20(10),xd20(10),xs20(10)
      common/cread/iread
      common/bread/xb00,xc00,xd00,xs00,xa11,xb11,xc11,xd11,xs11,
     &                  xb20,xc20,xd20,xs20
      real*8 fpi
      data iread/1/
#include "masses.f"
C     print *,' entry the phases for ',ene,a0,a2
C      print *,' entry the phases iread ',iread
c data
       real*4 bern(130)
C input coefficients from Gilberto (ACGL Phys rep 353 (2001) 207-279)
      data bern /
     &     0.2463,  0.1985  ,  0.1289  ,   0.01426,  0.008717,
     &    0.05058, -0.004266, -0.004658, -0.005358, -0.002555,
     &   -0.01665,  0.003283,  0.01142 ,  0.01400 ,  0.01613 ,
     &    0.03000, -0.004045,  0.002110,  0.01095 ,  0.004249,
     & -0.0006403, -0.004136, -0.003699, -0.003980, -0.003152,
     &  -0.007354, -0.001212, -0.004544, -0.004558, -0.001271,
     &      36.72,     1.339,    0.6504,    -3.211,    -1.396,
     &     -4.114,    -3.447,    -8.428,    -6.350,    -1.486,
     &    0.03626,   0.01834,   0.01081, -0.003195, 0.0001670,
     & -0.0009543, 0.0005049, 4.595E-05,-9.000E-05,-1.198E-05, 
     &  0.0001337, -0.002336,-0.0008563, 0.0001678, 4.147E-05,
     &  8.402E-05,-9.308E-05,-0.0002755,-0.0002308,-6.120E-05,
     & -6.976E-05, 0.0001965, 3.268E-05, 2.173E-05, 3.267E-06,
     &  2.059E-05, 1.070E-05, 5.554E-05, 5.307E-05, 1.519E-05,
     &  1.408E-06,-1.974E-05,-8.821E-06,-6.047E-07,-1.617E-06,
     & -3.125E-06,-1.257E-06,-4.432E-06,-4.415E-06,-1.344E-06,
     &      30.74,   -0.2459,   -0.1733,   0.06323, -0.001090,
     &    0.02724, -0.007218,   0.01483,   0.01813,  0.005016,
     &   -0.08553,  -0.01236, -0.006673,  0.004901,   0.02810,
     &    0.04010,  -0.01663,  -0.06784,  -0.05429,  -0.01178,
     &  -0.007542,   0.03466,   0.02857,  0.002674,   0.01477,
     &    0.02458,  -0.03030,  -0.09512,  -0.08744,  -0.02535,
     &  0.0001987, -0.002524, -0.001993,  0.001506, 0.0002915,
     &   0.001325, 0.0008759,  0.004713,  0.005313,  0.001730,
     &     -11.92,    -40.40,    -34.57,    -98.79,    -98.56,
     &     -207.2,    -158.9,    -525.9,    -536.6,   -172.3/

      fpi=acos(-1.)
      xmp = mpi
      pi = fpi
C   phases calculation valid between 2 mpi and matching point (0.8 GeV)
       if(ene.lt.2.*xmp.or.ene.gt.0.8) then
         write(6,*) 'energy out of range in thephases: ene= ',ene
         call exit()
       endif

      pa0=0.225
      pa2=-0.03706  ! as in phys rep 353
      xa0=a0/pa0-1.
      xa2=a2/pa2-1.

c the entries from appendix C

      if(iread.eq.1) then

      do i=1,10
         xb00(i) = bern(i)
      enddo
      do i=1,10
         xc00(i) = bern(i+10)
      enddo
      do i=1,10
         xd00(i) = bern(i+20)
      enddo
      do i=1,10
         xs00(i) = bern(i+30)
      enddo

      do i=1,10
         xa11(i) = bern(i+40)
      enddo
      do i=1,10
         xb11(i) = bern(i+50)
      enddo
      do i=1,10
         xc11(i) = bern(i+60)
      enddo
      do i=1,10
         xd11(i) = bern(i+70)
      enddo
      do i=1,10
         xs11(i) = bern(i+80)
      enddo

      do i=1,10
         xb20(i) = bern(i+90)
      enddo
      do i=1,10
         xc20(i) = bern(i+100)
      enddo
      do i=1,10
         xd20(i) = bern(i+110)
      enddo
      do i=1,10
         xs20(i) = bern(i+120)
      enddo

      iread=10

      end if

c now apply formula Appendix C.1
c---------------------------------

      b00= xb00(1)
     &    +xb00(2)*xa0
     &    +xb00(3)*xa2
     &    +xb00(4)*xa0**2
     &    +xb00(5)*xa2**2
     &    +xb00(6)*xa0*xa2
     &    +xb00(7)*xa0**3
     &    +xb00(8)*xa0**2*xa2
     &    +xb00(9)*xa0**1*xa2**2
     &    +xb00(10)*xa2**3

      c00= xc00(1)
     &    +xc00(2)*xa0
     &    +xc00(3)*xa2
     &    +xc00(4)*xa0**2
     &    +xc00(5)*xa2**2
     &    +xc00(6)*xa0*xa2
     &    +xc00(7)*xa0**3
     &    +xc00(8)*xa0**2*xa2
     &    +xc00(9)*xa0**1*xa2**2
     &    +xc00(10)*xa2**3

      d00= xd00(1)
     &    +xd00(2)*xa0
     &    +xd00(3)*xa2
     &    +xd00(4)*xa0**2
     &    +xd00(5)*xa2**2
     &    +xd00(6)*xa0*xa2
     &    +xd00(7)*xa0**3
     &    +xd00(8)*xa0**2*xa2
     &    +xd00(9)*xa0**1*xa2**2
     &    +xd00(10)*xa2**3

      s00= xs00(1)
     &    +xs00(2)*xa0
     &    +xs00(3)*xa2
     &    +xs00(4)*xa0**2
     &    +xs00(5)*xa2**2
     &    +xs00(6)*xa0*xa2
     &    +xs00(7)*xa0**3
     &    +xs00(8)*xa0**2*xa2
     &    +xs00(9)*xa0**1*xa2**2
     &    +xs00(10)*xa2**3

      a11= xa11(1)
     &    +xa11(2)*xa0
     &    +xa11(3)*xa2
     &    +xa11(4)*xa0**2
     &    +xa11(5)*xa2**2
     &    +xa11(6)*xa0*xa2
     &    +xa11(7)*xa0**3
     &    +xa11(8)*xa0**2*xa2
     &    +xa11(9)*xa0**1*xa2**2
     &    +xa11(10)*xa2**3

      b11= xb11(1)
     &    +xb11(2)*xa0
     &    +xb11(3)*xa2
     &    +xb11(4)*xa0**2
     &    +xb11(5)*xa2**2
     &    +xb11(6)*xa0*xa2
     &    +xb11(7)*xa0**3
     &    +xb11(8)*xa0**2*xa2
     &    +xb11(9)*xa0**1*xa2**2
     &    +xb11(10)*xa2**3

      c11= xc11(1)
     &    +xc11(2)*xa0
     &    +xc11(3)*xa2
     &    +xc11(4)*xa0**2
     &    +xc11(5)*xa2**2
     &    +xc11(6)*xa0*xa2
     &    +xc11(7)*xa0**3
     &    +xc11(8)*xa0**2*xa2
     &    +xc11(9)*xa0**1*xa2**2
     &    +xc11(10)*xa2**3

      d11= xd11(1)
     &    +xd11(2)*xa0
     &    +xd11(3)*xa2
     &    +xd11(4)*xa0**2
     &    +xd11(5)*xa2**2
     &    +xd11(6)*xa0*xa2
     &    +xd11(7)*xa0**3
     &    +xd11(8)*xa0**2*xa2
     &    +xd11(9)*xa0**1*xa2**2
     &    +xd11(10)*xa2**3

      s11= xs11(1)
     &    +xs11(2)*xa0
     &    +xs11(3)*xa2
     &    +xs11(4)*xa0**2
     &    +xs11(5)*xa2**2
     &    +xs11(6)*xa0*xa2
     &    +xs11(7)*xa0**3
     &    +xs11(8)*xa0**2*xa2
     &    +xs11(9)*xa0**1*xa2**2
     &    +xs11(10)*xa2**3

      b20= xb20(1)
     &    +xb20(2)*xa0
     &    +xb20(3)*xa2
     &    +xb20(4)*xa0**2
     &    +xb20(5)*xa2**2
     &    +xb20(6)*xa0*xa2
     &    +xb20(7)*xa0**3
     &    +xb20(8)*xa0**2*xa2
     &    +xb20(9)*xa0**1*xa2**2
     &    +xb20(10)*xa2**3

      c20= xc20(1)
     &    +xc20(2)*xa0
     &    +xc20(3)*xa2
     &    +xc20(4)*xa0**2
     &    +xc20(5)*xa2**2
     &    +xc20(6)*xa0*xa2
     &    +xc20(7)*xa0**3
     &    +xc20(8)*xa0**2*xa2
     &    +xc20(9)*xa0**1*xa2**2
     &    +xc20(10)*xa2**3

      d20= xd20(1)
     &    +xd20(2)*xa0
     &    +xd20(3)*xa2
     &    +xd20(4)*xa0**2
     &    +xd20(5)*xa2**2
     &    +xd20(6)*xa0*xa2
     &    +xd20(7)*xa0**3
     &    +xd20(8)*xa0**2*xa2
     &    +xd20(9)*xa0**1*xa2**2
     &    +xd20(10)*xa2**3

      s20= xs20(1)
     &    +xs20(2)*xa0
     &    +xs20(3)*xa2
     &    +xs20(4)*xa0**2
     &    +xs20(5)*xa2**2
     &    +xs20(6)*xa0*xa2
     &    +xs20(7)*xa0**3
     &    +xs20(8)*xa0**2*xa2
     &    +xs20(9)*xa0**1*xa2**2
     &    +xs20(10)*xa2**3

       y=(ene/xmp)**2
       sig=sqrt(1.-4./y)
       x=y/4-1.

       ph00=atan(
     &            sig*(a0+b00*x+c00*x**2+d00*x**3)*(4d0-s00)/(y-s00)
     &          )

       if(y.gt.s00)ph00=ph00+pi

       ph11=atan(
     &            sig*x*(a11+b11*x+c11*x**2+d11*x**3)*(4d0-s11)/(y-s11)
     &          )

       if(y.gt.s11)ph11=ph11+pi

       ph20=atan(
     &            sig*(a2+b20*x+c20*x**2+d20*x**3)*(4d0-s20)/(y-s20)
     &          )

       return
       end

      function berncor(s)
c--------------------------------
C   isospin correction to be added to delta00
C input is Mpipi in Gev/c2
c--------------------------------
      implicit none
#include "masses.f"
      real*8 mpp2,mp02,fpi,berncor
      real*8 R
      real*8 delta,norm,s,sigc,sig0
      real*8 dsym,dreal,pi,corr1,corr2
c
c data
c
      pi= 3.141592653589793d0    ! always true...
      fpi=.0862d0     ! hep-ph/0409222

      R = 37.d0   !    +- ,4 due to quark masses uncertainty
      mpp2=SQMPI
      mp02=SQMP0
      delta=mpp2-mp02
      norm=1./(32.*pi*fpi*fpi) ! in rad
c dsym is symmetric phase, at one loop (called tree earlier)
c-------------------------
      sigc=sqrt(1.d0-4.d0*mpp2/s)
      sig0=sqrt(1.d0-4.d0*mp02/s)
      dsym=(2.*s-mpp2)*sigc*norm

c dreal is the real one, at 1 loop (called tree earlier)
c------------------------------------
      dreal=(4.*delta+s)*sigc+(s-mp02)*(1.+3./(2.*R))*sig0
      dreal=dreal*norm
      corr1=dreal-dsym
C      print *,' mass ',sqrt(s),' correction ',corr1
      berncor = corr1  ! corr in  radians
      return
      end
