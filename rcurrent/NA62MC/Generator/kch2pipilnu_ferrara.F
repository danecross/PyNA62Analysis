      subroutine KCH2PIPILNU_FERRARA(jkaon,IVAL,pi0decay)
c
c     Marcella Scarpa, March 2004
c
c      Generate Kl4 decay      one loop CHPT
c      ival=1 pi+ pi- mu nu
c      ival=2 pi0 pi0 mu nu
c      ival=3 pi0 pi0 e nu
c      ival=4 pi+ pi- e nu

include "common_blocks.f"
#include "masses.f"

      integer jpi1,jpi2,jlep,j
      integer pi0decay, pzmode1, pzmode2
      INTEGER IPION1,IPION2,ILEP,IVAL
      real*8  ppi1(4),ppi2(4),plep(4)

      call Kmu4genc(ival)

      do j = 1, 4
         ppi1(j) = pcm(j,1)
         ppi2(j) = pcm(j,2)
         plep(j) = pcm(j,3)
      enddo

c ... particle codes
      if(ival.eq.1 .or. ival.eq.4)then
         ipion1 = idpip
         ipion2 = idpim
         if(ival.eq.1)ilep = idmup
         if(ival.eq.4)ilep = idelep
      else
         ipion1 = idpiz
         ipion2 = idpiz
         if(ival.eq.2)ilep = idmup
         if(ival.eq.3)ilep = idelep
      endif

c ... store generated particles
      jpi1 = mcadd4gen (IPION1, ppi1, 0)
      jpi2 = mcadd4gen (IPION2, ppi2, 0)
      jlep = mcadd4gen (Ilep,   plep, 0)

c ... boost particles to lab frame
      call dboost( p4ini(1,jkaon), MKCH, ppi1, ppi1 )
      call dboost( p4ini(1,jkaon), MKCH, ppi2, ppi2 )
      call dboost( p4ini(1,jkaon), MKCH, plep, plep )

c ... register particles for tracking
      jpi1 = mcadd4 (IPION1, ppi1)
      jpi2 = mcadd4 (IPION2, ppi2)
      jlep = mcadd4 (Ilep,   plep)

C --- FORCED PI0 DECAYS

      if (ival.eq.2.or.ival.eq.3) then ! two pi0's
         pzmode1 = mod (pi0decay, 100)
         pzmode2 = mod (pi0decay/100, 100)
         call pi0decay_manager(jpi1, ppi1, pzmode1)
         call pi0decay_manager(jpi2, ppi2, pzmode2)
      endif

      return
      end

      SUBROUTINE KMU4GENC(IVAL)
c******************************************************************
C
C              C.Damiani M.Fiorini A.Gianoli M.Scarpa  
C  ke4genc-like(M.De Beer+R.Turlay) + one loop CHPT by G.Colangelo
C Main reference: Bijnens,Colangelo,Gasser Nucl. Phys. B 427, 427 (1994)
C
C     Generation of: kmu4  & Kmu42pi0 & Ke42pi0 & ke4-bis
C     March 2004
C     Aprel/2017 Korotkova Anna JINR
C       -modified kmu4(00) decay according to the theoretical requirements:
C       -In first approximation, only S ­wave term contributes.
C       -G and H expansions begin from P ­wave consequently G=0 and H=0
C-------------------------------------------------------------------

#include "common_blocks.f"
#include "masses.f"

        real*8 VMOD,VDOTN,ranf
        integer IVAL
        real*8 PIPID(3),PIPIN(3),ENUD(3),ENUN(3)
        real*8 TEMPI(4),TEMPO(4),TEMPM(4)        
        real*8 CPIPI(3),CENU(3)
        real*8 TEMP1(3),TEMP2(3)
        real*8 TEMP3(3)         
        real*8 ppm2,ppm,enm2,enm,x,y,ftotal
	real*8 eth,phi,phi2,aphi,aphif
	real*8 cphi,c2phi,sphi,s2phi,sphi2,cphi2
        real*8 tl,tl2,ctl,c2tl,stl,s2tl,ctl2,stl2
        real*8 tpi,ctpi,c2tpi,stpi,s2tpi,pith
        real*8 i1,i2,i3,i4,i5,i6,i7,i8,i9
        real*8 pl,zl,qq
        real*8 evtwt,w,wr,WDMX
        real*8 sl,tvar,uvar,spipi
        real*8 sigmapi
        integer iok,i
        real*8 pi
        complex f,g,h,r,f1,f2,f3,f4
        real*8 kmass2
        complex zf,zg,zr,zh

        DATA PI  /3.141592653/

        NP = 4
        TECM=MKCH

        DATA WDMX /0.023 /      ! ke4 max(wt*ftotal)

        if(ival.eq.1)WDMX=0.020              ! kmu4 max(wt*ftotal)
        if(ival.eq.2.or.ival.eq.3)WDMX=0.013 ! kmu/e400 max(wt*ftotal)

C             Generate the decay (phase space in CM system)
C             ------------------ -------------------------
c amass used by genbod_fix to calculate PCM(I,iparticle) I=px,py,pz,E
        
        if(ival.eq.1 .or. ival.eq.4)then 
         amass(1) = MPI     
         amass(2) = MPI
        else
         amass(1) = MP0    
         amass(2) = MP0  
        endif        
        if(ival.eq.3 .or. ival.eq.4)amass(3)=MEL
        if(ival.eq.1 .or. ival.eq.2)amass(3)=MMU
        amass(4) = .0           

        KGENEV   = 1

 101    continue
        CALL GENBOD_fix(iok)
        if(iok.eq.0) goto 101
C
C DEFINE THE FIVE KINEMATICS VARIABLES FOR THE DECAY
C---------------------------------------------------
C        1. PI-PI INVARIANT MASS
C        2. MU/ELEC-NU INVARIANT MASS
C        3. 3 angles: look up 
C           Cabibbo,Maksymowicz Phys.Rev. 137, B438 (1965)
C           Bijnens,Colangelo,Gasser Nucl. Phys. B 427, 427 (1994)
                            
c              pipi invariant mass   (ppm) 
c              -------------------
c
        PPM2=2.*PCM(4,1)*PCM(4,2)
     1      + AMASS(1)*AMASS(1)
     1      + AMASS(2)*AMASS(2)
     1      - 2.*(PCM(1,1)*PCM(1,2)+PCM(2,1)*PCM(2,2)+PCM(3,1)*PCM(3,2))
        PPM =SQRT(PPM2)
        
        tvar=(PCM(4,1)-tecm)**2 -(PCM(1,1)**2+PCM(2,1)**2+PCM(3,1)**2)
        uvar=(PCM(4,2)-tecm)**2 -(PCM(1,2)**2+PCM(2,2)**2+PCM(3,2)**2)
        
c
c              mu - nu  invariant mass  (enm)
c              ----------------------
c  
        ENM2=2.*PCM(4,3)*PCM(4,4)
     1      + AMASS(3)*AMASS(3)
     1      - 2.*(PCM(1,3)*PCM(1,4)+PCM(2,3)*PCM(2,4)+PCM(3,3)*PCM(3,4))
        ENM =SQRT(ENM2)
c        
c                 theta pions (the cosine is pith)
c                 ------------------------------
c
        CALL VZERO(PIPID,3)
        CALL VZERO(PIPIN,3)
        DO I=1,3
          PIPID(I)=PCM(I,1)+PCM(I,2)
 	ENDDO
        IF (VMOD(PIPID,3).EQ.0.) GOTO 101
        CALL VUNIT(PIPID,PIPIN,3)
 
        CALL VZERO(TEMPI,4)
        CALL VZERO(TEMPM,4)
        CALL VZERO(TEMPO,4)

        DO  I=1,4
          TEMPI(I)=PCM(I,1)
 	ENDDO
 
        DO  I=1,3
          TEMPM(I)=PIPID(I)
 	ENDDO
        TEMPM(4)=SQRT(TEMPM(1)**2+TEMPM(2)**2+TEMPM(3)**2+PPM2)
 
        CALL LOREN4(TEMPM,TEMPI,TEMPO)
        IF (VMOD(TEMPO,3).EQ.0.) GOTO 101

        PITH=VDOTN(TEMPO,TEMPM,3)
c
c                theta leptons  ( the cosine is eth)
c                -----------------------------------
c 
        CALL VZERO(ENUD,3)
        CALL VZERO(ENUN,3)
        DO  I=1,3
          ENUD (I)=PCM(I,3)+PCM(I,4)
 	ENDDO
        IF (VMOD(ENUD,3).EQ.0.) GOTO 101
        CALL VUNIT(ENUD,ENUN,3)
 
        CALL VZERO(TEMPI,4)
        CALL VZERO(TEMPM,4)
        CALL VZERO(TEMPO,4)

        DO I=1,4
          TEMPI(I)=PCM(I,3)  
 	ENDDO
 
        DO I=1,3
          TEMPM(I)=ENUD(I)  
        ENDDO 
        TEMPM(4)=SQRT(TEMPM(1)**2+TEMPM(2)**2+TEMPM(3)**2+ENM2)
 
        CALL LOREN4(TEMPM,TEMPI,TEMPO)
        IF (VMOD(TEMPO,3).EQ.0.) GOTO 101
 
        ETH =VDOTN(TEMPO,TEMPM,3)
C
C        THE LAST ANGLE !  ( APHIF)
C        ----------------
c
        CALL VZERO(TEMP1,3)
        CALL VZERO(TEMP2,3)
        CALL VZERO(CPIPI,3)
        CALL VZERO(CENU ,3)
 
        DO  I=1,3
          TEMP1(I)=PCM(I,1) 
          TEMP2(I)=PCM(I,2) 
 	ENDDO
        IF(VMOD(TEMP1,3).EQ.0..OR.VMOD(TEMP2,3).EQ.0.)GOTO 101
        CALL CROSS (TEMP1,TEMP2,CPIPI) 
 
        DO I=1,3
          TEMP1(I)=PCM(I,3)
          TEMP2(I)=PCM(I,4)
	ENDDO
        IF(VMOD(TEMP1,3).EQ.0..OR.VMOD(TEMP2,3).EQ.0.)GOTO 101
        CALL CROSS (TEMP1,TEMP2,CENU) 

        PHI =VDOTN(CPIPI,CENU,3)        
        APHI=ACOS(PHI)

        X=PHI
        CALL VUNIT (CPIPI,TEMP1,3) ! TEMP1=unit vector c
        CALL VUNIT (CENU ,TEMP2,3) ! TEMP2=unit vector d

        CALL CROSS (TEMP1,PIPIN,TEMP3) !TEMP3 = c x v 
        Y=VDOTN(TEMP3,TEMP2,3)         !Y = (c x v)d

        IF (X.EQ.0..AND.Y.EQ.0.) GOTO 101

c        APHIF=ATAN2(Y,X)  ! (-pi,+pi]
        APHIF=ATAN2(Y,X)+PI  ! (0,2pi]
     
        cphi = cos(aphif)
        c2phi = cphi**2
        sphi = sin(aphif)
        s2phi = sphi**2
        phi2=2.*aphif
        cphi2=cos(phi2)
        sphi2=sin(phi2)

        tl  = acos(eth)    !   theta lepton          
        ctl = cos(tl)
        tl2 = 2.*tl
        ctl2= cos(tl2)
        c2tl = ctl**2
        stl = sin(tl)
        stl2 = sin(tl2)
        s2tl = stl**2
        
        tpi = acos(pith)   !   theta pion
        ctpi = cos(tpi)                      
        c2tpi = ctpi**2
        stpi = sin(tpi)
        s2tpi = stpi**2

        spipi = ppm2          !   invariant pi-pi mass squared
        sl = enm2             !   invariant lepton-nu mass squared

        PL = 0.5*(tecm**2 - spipi - sl )  ! (p1+p2)(pl+pnu)                
        X = sqrt( (PL**2) - (spipi*sl) ) 
        zl = (amass(3)**2)/sl                 
        sigmapi = sqrt(1-(4.*amass(1)**2)/spipi)                         
        qq=(spipi -4.* amass(1)**2)/(4. * amass(1)**2)
        kmass2 = tecm*tecm

        CALL COLANGELOOP(spipi,tvar,uvar,zf,zg,zr,zh)

	F=zf
	G=zg
	H=zh
	R=zr

c K+,- -> pi0pi0 l+,- nu : (F,G,H,R)_(00) = -(F+,G-,H-,R+)_(+-)
        if(ival.eq.2)then  ! kmu400
	 G=0
	 H=0
         CALL COLANGELOOP(spipi,uvar,tvar,zf,zg,zr,zh)
	 F=-0.5*(F+zf)
	 R=-0.5*(R+zr)         
	endif

	if(ival.eq.3)then
         CALL COLANGELOOP(spipi,uvar,tvar,zf,zg,zr,zh)
	 F=-0.5*(F+zf)
	 G=-0.5*(G-zg)
	 H=-0.5*(H-zh)
	 R=-0.5*(R+zr)         
        endif

ccccccccccccccc f,g,h,r complex  ccccccccccccccccccccccccccccccccccc

        f1 = X*F + PL*sigmapi*G*ctpi
        f2 = sigmapi*sqrt(spipi*sl)*G
        f3 = sigmapi*sqrt(spipi*sl)*X*(H/(kmass2))
        f4 = - PL*F - sl*R - X*sigmapi*G*ctpi

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	i1 =0.25*(   (1.+zl)*(cabs(f1))**2 
     &  + 0.5*(3.+zl)*s2tpi*((cabs(f2))**2 + (cabs(f3))**2) 
     +  +2.*zl*( (cabs(f4))**2) )    
	i2 = -0.25*(1.-zl)*( (cabs(f1))**2 - 0.5*s2tpi* 
     +       ( (cabs(f2))**2 + (cabs(f3))**2 ) )     
	i3 = -0.25*(1.-zl)*s2tpi*((cabs(f2))**2 - (cabs(f3))**2)       
	i4 = 0.5*(1.-zl)*real((conjg(f1))*f2)*stpi        
	i5 = -(real((conjg(f1))*f3)+zl*real((conjg(f4))*f2) )*stpi
	i6 = -(real((conjg(f2))*f3) *s2tpi 
     +       - zl*real((conjg(f1)) * f4) )
	i7 = -(imag((conjg(f1))*f2) + zl*imag((conjg(f4))*f3) )*stpi
	i8 = 0.5*(1.-zl)*imag((conjg(f1))*f3)*stpi
	i9 = -0.5*(1.-zl)*imag((conjg(f2))*f3)*s2tpi

       ftotal = 2.*(1.-zl)*( i1+ i2*ctl2+
     > i3*s2tl*cphi2+ i4*stl2*cphi+ i5*stl*cphi+
     > i6*ctl + i7*stl*sphi+ i8*stl2*sphi +
     > i9*(s2tl*sphi2) )

        if(ival.eq.2)then  ! kmu400
            ftotal = 2.*(1.-zl)*(i1+i2*ctl2+i6*ctl)
        endif

c          the phase space weight ( wt ) is corrected by using
c          the computed matrix element (ftotal)
c
        EVTWT =WT*FTOTAL
        W   =EVTWT
c
c           is the generated event  a good candidate   ???
c
        WR  =WDMX*ranf()
        IF(WR.GT.W) GOTO 101
c
c           yes  it is ..... 
c
       RETURN
       END

**********************************************************
*        one loop integral calculation                   *
*        SUBROUTINE COLANGELOOP(->INITCOL,FFKL4,FLOOP)   *
*        original verision by G.Colangelo                *
*        adapted by C.Damiani & M.Scarpa  Jan.2004       *
**********************************************************
      subroutine COLANGELOOP(s,t,u,zf,zg,zr,zh)

      implicit real*8(a-y), complex(z)
      common mp2,mk2,me2,ml2,fpi2,scal2,xl1,xl2,xl3,xl4,xl5,
     .xl6,xl8,xl9,pi
      
      call initcol
      
      call ffkl4(s,t,u,zf,zg,zr,zh)

      return
      end

**********************************************************
*        SUBROUTINE INITCOL called by COLANGELOOP        *
*        original verision by G.Colangelo                *
*        adapted by C.Damiani & M.Scarpa  Jan.2004       *
**********************************************************
      SUBROUTINE INITCOL 

      implicit real*8(a-y), complex(z)
      common mp2,mk2,me2,ml2,fpi2,scal2,xl1,xl2,xl3,xl4,xl5,
     .xl6,xl8,xl9,pi

#include "masses.f"

      mpc = MPI
      mpn = MP0
      mkc = MKCH
      mkn = 0.497672    ! K0
      meta= 0.54745     ! eta

      mp2 = mpc*mpc
      mk2 = mkc*mkc
      me2 = meta*meta

      fpi = 0.0924 
      fpi2= fpi*fpi

      scal2 = 0.77**2   ! rho**2

C     numbers from BCG, NPB 427 1994
      xl1 = 0.4E-3  ! new from NPB 427 1994
      xl2 = 1.3E-3  ! new from NPB 427 1994
      xl3 =-3.5E-3  ! new from NPB 427 1994
      xl4 =-0.3E-3
      xl5 = 1.4E-3
      xl6 =-0.2E-3
      xl8 = 0.9E-3
      xl9 = 6.9E-3

      pi = 3.141592653

      return
      end

**********************************************************
*        SUBROUTINE FFKL4 called by COLANGELOOP          *
*        original version by G.Colangelo                 *
*        adapted by C.Damiani & M.Scarpa  Jan.2004       *
**********************************************************

      SUBROUTINE FFKL4(sp,t,u,zf,zg,zr,zh)
c-------------------------------------------
      implicit real*8(a-y), complex(z)
      common mp2,mk2,me2,ml2,fpi2,scal2,xl1,xl2,xl3,xl4,xl5,
     .xl6,xl8,xl9,pi

      scale=sqrt(scal2) ! mrho
      fpi=sqrt(fpi2)

change l3
c      xl3 = -3.6E-3*(1.+ sp/(scal2-sp))
cendchange

      vt=t
      sl=t+u+sp-2.*mp2-mk2         

      call floop(mp2,mp2,fpi,scale,xl9,sp,zjb,zjrpp,zh,zk,zl,zmpp)
      call floop(mk2,mk2,fpi,scale,xl9,sp,zjb,zjrkk,zh,zk,zl,zmkk)
      call floop(me2,me2,fpi,scale,xl9,sp,zjb,zjree,zh,zk,zl,zm)
      call floop(mk2,mp2,fpi,scale,xl9,t ,zjb,zjrkp,zx,zkkp,zlkp,zmkp)
      call floop(me2,mk2,fpi,scale,xl9,t ,zjb,zjrek,zx,zkek,zlek,zmek)
      call floop(mk2,mp2,fpi,scale,xl9,u ,zjb,zjrkpu,zx,zx,zx,zx)

      mup=1./(32.*pi**2)/fpi2*mp2*Log(mp2/scal2)    
      muk=1./(32.*pi**2)/fpi2*mk2*Log(mk2/scal2)
      mue=1./(32.*pi**2)/fpi2*me2*Log(me2/scal2)
c
      zd0=1./2.*(2.*sp-mp2)*zjrpp +3.*sp/4.*zjrkk +mp2/2.*zjree ! delta0
c
      af   =1./32.*(14.*mk2+14.*mp2-19.*vt)*zjrkp
     & +1./16.*(3.*mk2-7.*mp2+5.*vt)*zkkp
     & - 1./8.*(9.*zlkp +(3.*mk2-3.*mp2-9.*vt)*zmkp)
     & +1./32.*(2.*mk2+2.*mp2-3.*vt)*zjrek
     & +1./16.*(mk2-5.*mp2+3.*vt)*zkek
     & -1./8.*(9.*zlek +(3.*mk2-3.*mp2-9.*vt)*zmek)

      pf =32.*(sp-2.*mp2)*xl1+
     & 8.*(mk2+sp-sl)*xl2+
     & 2.*(mk2-8.*mp2+5*sp-sl -vt+u)*xl3 +
     & 32.*mp2*xl4+
     & 4.*mp2*xl5+
     & 2.*sl*xl9

 
      cf =1./8.*(5.*mup -2.*muk -3.*mue)  


      zadd = -1./2.*(mk2+mp2-vt)*zjrkp +1./2.*(mk2+mp2-u)*zjrkpu  ! B(t,u)

c
      zf =1.+(1./fpi2)*(zd0+2.*af  +pf+zadd) +cf 
      zf=(sqrt(mk2)/(sqrt(2.)*fpi))*zf

comment now g

      zd1 =2.*sp*(zmpp +1./2.*zmkk)

      ag =1./32.*(2.*mk2+2.*mp2+3.*vt)*zjrkp
     & +1./16.*(-3.*mk2+7.*mp2-5.*vt)*zkkp
     & -3./8.*(zlkp -(mk2-mp2+vt)*zmkp)
     & -1./32.*(2.*mk2+2.*mp2-3.*vt)*zjrek
     & +1./16.*(-mk2+5.*mp2-3.*vt)*zkek
     & -3./8.*(zlek -(mk2-mp2+vt)*zmek)

c      pg =-2.*(mk2+sp-sl)*xl3
c     & +4.*mp2*xl5
c     & +2.*sl*xl9
c     & +8.*(vt-u)*(xl2 +1./4.*xl3)

      pg =8.*(vt-u)*xl2
     & +4.*(vt-mk2-mp2)*xl3
     & +4.*mp2*xl5
     & +2.*sl*xl9

      cg =-cf

      zg =1.+1./fpi2*(zd1+2.*ag   +pg+zadd) +cg 

      zg=sqrt(mk2)/sqrt(2.)/sqrt(fpi2)*zg

      zh=-sqrt(2.)/(8.*pi**2)*((mk2/fpi2))**1.5

       sig=mk2+mp2
       del=mk2-mp2
       d=mk2-sl
       vnu=vt-u
       
       zuz=sp*zd0 +vnu*zd1 -4./9.*mk2*mp2*zjree
     &  +1./32.*(11.*(sp-vnu)**2 -20.*sig*(sp-vnu)+12.*sig**2) *zjrkp
     &  +1./4.*(sp+vnu)**2*zjrkpu
     &  +1./96.*(3.*(sp-vnu) -2.*sig)**2*zjrek
     &  +1./4.*(5.*(sp-vnu) -6.*sig)*del*zkkp
     &  +1./4.*(3.*(sp-vnu) -2.*sig)*del*zkek
     &  +3./8.*(2.*sp*(vnu+4.*sig) -3.*sp**2 +vnu**2 -16.*mp2*mk2)*
     &   (zmkp +zmek)
     &  -3./4.*(3.*sp+vnu-2.*sig)*(zlkp+zlek)
       pz=
     &  +32.*(sp-2.*mk2)*(sp-2.*mp2)*xl1
     &  +8.*(sp**2+vnu**2)*xl2
     &  -2.*( 2.*(vnu+4.*sig)*sp -5.*sp**2 -vnu**2 -16.*mk2*mp2)*xl3
     &  +32.*(sig*sp -4.*mk2*mp2)*xl4
     &  +4.*( (sp+vnu)*sig -8.*mk2*mp2)*xl5
     &  +128.*mk2*mp2*xl6
     &  +64.*mk2*mp2*xl8
       cz=
     &  -1./4.*del*(3.*mup-2.*muk-mue)

       zz=sp+vnu+(zuz+pz)/fpi2+cz

       zuq=zd0 +11./32.*(mk2-sl)*zjrkp
     &  +1./4.*d*zjrkpu
     &  +3./32.*d*zjrek
     &  -1./8.*( 5.*(sp-vnu) +5.*d -6.*sig)*zkkp
     &  -1./8.*(3.*(sp-vnu) +3.*d -2.*sig) *zkek
     &  +3./8.*(4.*(vnu+2.*mp2) -3.*d)*zmkp
     &  +3./8.*(4.*(vnu+2.*mp2) -3.*d)*zmek
     &  -9./4.*(zlkp+zlek)
       pq=
     &  +32.*(sp-2.*mp2)*xl1
     &  + 8.*(mk2-sl)*xl2
     &  +2.*( 4.*(sp-2.*mp2) +d)*xl3
     &  +32.*mp2*xl4
     &  +4.*sig*xl5
     &  +2.*( sp+vnu -d)*xl9
       cq=
     &  +1./4.*(5.*mup-2.*muk-3.*mue)

      zq=1.+(zuq+pq)/fpi2 +cq
      zr=sqrt(mk2)/(2.*sqrt(2.))/sqrt(fpi2)*( zz/(sl-mk2) +zq)

      return
      end

******************************************************************
*  SUBROUTINE FLOOP called by FFKL4 called by COLANGELOOP        *
*        original verision by G.Colangelo                        *
*        adapted by C.Damiani & M.Scarpa  Jan.2004               *
******************************************************************
      SUBROUTINE FLOOP(mp2,mq2,fpi,scale,l9,s,zjb,zjr,zh,zk,zl,zm)
c--------------------------------------------------------------
      implicit real*8(a-y), complex(z)

      ln(x)=log(x)

      pi=4.*Atan(1.)
      pi2=pi**2
      pi16=16.*pi**2
      mp=sqrt(mp2)
      mq=sqrt(mq2)
      imjb=0.
      scal2=scale**2
      fpi2=fpi**2
c
c**
c**
      if(abs(s).lt.1E-12)then
       write(*,*)'s near zero: floop does not handle this case',s
       return
      end if
c**
c**
      x1=mp/mq
      x2=mq/mp
c i do evaluate both ratios. if any of the masses should be zero,
c the computer complains
c next line jbar has simpler form for mp=mq
c
      if(abs(1.-x1).lt.1E-12) goto 1000
c
      del=mp2-mq2
      sig=mp2+mq2
      arg=( s-(mp+mq)**2 ) * ( s-(mp-mq)**2 )
      nu=sqrt(abs(arg))
c
c**
c**
      if( (mp-mq)**2.le.s.and.s.le.(mp+mq)**2)then

        helog=-4.*nu/s*Atan(sqrt( s-(mp-mq)**2 ) /
     &                   sqrt(-s+(mp+mq)**2 ))

      else
        helog= -nu/s*Log( abs(  ( (s+nu)**2 -del**2) /
     &          ( (s-nu)**2 -del**2) ) )
        
      end if
c**

      rejb=2.+del/s*Log(mq2/mp2)
     &    -sig/del*Log(mq2/mp2)
     &    +helog
c
      rejb=rejb/(32.*pi**2)
c
c**
c**
      if(s.gt.(mp+mq)**2)then
       imjb=1./(16.*pi)*nu/s
      end if
c**
c**
c****************************
      goto 2000
c*****************************
c
 1000   continue
c jb for mp=mq
c******************************
      sig=sqrt(abs(1.-4.*mp2/s))

      rejb= sig* Log(abs( (sig-1.)/(sig+1.) ))
     &      +2.
c
      rejb=rejb/(16.*pi2)
c
      imjb=0.
      if(s.gt.(4.*mp2)) imjb=sig/(16.*pi)
c
      if((s.lt.0.).or.(s.ge. 4.*mp2)) go to 2000
      rejb=-2.*sig*Atan( 1./sig ) +2.
      rejb=rejb/(16.*pi2)
c
 2000   continue
c
      zjb=cmplx(rejb,imjb)   
c
c evaluate function jr(s)
c------------------------
c next line k has simpler form for mp=mq
c**
c**
      if(abs(1.-x1).lt.1E-12)
     &  then
        k=1./(32.*pi**2) *( Log(mp2/scal2)  +1.)
          else
          k=1./(32.*pi**2) * (
     &    mp2*Log(mp2/scal2) -mq2*Log(mq2/scal2)
     &                         )/
     &             (mp2-mq2)
      end if
c**
c**
      zjr=zjb -2.*k
c
c evaluate function zk(s)
c------------------------
      x=mp2
      y=mq2
      zk=
     & (x-y)/(2.*s)*zjr +
     & 1./(2.*pi16*s)*( x*Log(x/scal2) -y*Log(y/scal2) )
c
c function zl(s)
c-----------------
      x=mp2
      y=mq2
      zl=
     & ( (x-y)**2 )/(4.*s)*zjr +
     & (x-y)/s/(4.*pi16)*( x*Log(x/scal2)-y*Log(y/scal2) )

c function zm(s)
c------------------
c
      x=mp2
      y=mq2
      zm=
     & 1./(12.*s)*(s-2.*(x+y))*zjr
     & +1./(3.*s**2)*(x-y)**2 *zjr
     &     -1./(6.*pi16)/s*( x*Log(x/scal2) +y*Log(y/scal2) +x+y )
     &     +1./(3.*pi16)*(x-y)/s**2*(x*Log(x/scal2) -y*Log(y/scal2) )
     &     +1./(18.*pi16)

c
c evaluate function zh(s)
c-------------------------
      zh=1./fpi2*(s*zm -zl) +2./3./fpi2*l9*s
c
      return
      end
