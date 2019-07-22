
      SUBROUTINE DEC000 (xcha,ycha,count,gain,nSMPL)
C.
C. - Finds out the type of shape for the digital filter 
C. - Gives a first approximation of Energy and Time
C.
C.----------------------------------------------------------------------
C*****Iquality values indicates gain switching/saturation problems
C     Iflag    signals general info related to cell/pulse 
C*****Principle: any sample following gain switching is assumed to 
C*****be unreliable.
C     Iquality value       meaning 
C  
C            1             all O.K.
C            2             use max and max+1 
C            3             use max and max-1
C            4             use max+1 and max+2
C            5             use max-1 and max+1
C            6             use max and max+2 
C   In the folowing cases the maximum sample is saturated          
C           14             Use max+1 and max+2 
C           15             Use max-1 and max+1
C   In the following there is a saturation but in the maximum sample 
C           21             use max-1 max and max+1 
C           22             use max and max+1          
C           23             use max and max-1
C           24             use max+1 and max+2
C           25             use max-1 and max+1
C           26             use max and max+2 
C           98             2 or more saturations
C   Special cases
C           -1             maximum outside selected window
C           99             bad case
C
C        Iflag bit          Meaning 
C
C            1              pulse below zero suppression threshold
C            2              pulse is saturated
C            3              cell dead (HV)
C            4              cell dead (Preamplifier)
C            5              cell malade
C            6              no calibration (by jzLKEN)
C            7              non trivial pulse (gain gt 0)                 
C            8              Q(iplkr+3) = max count (instead of count at fixed
C                           time)  
C-----------------------------------------------------------------------------
      IMPLICIT NONE

C-------------------------------------------------------------------------------
       Integer     nMAxSMPL
       Parameter   (nMaxSMPL=256) 

       Integer Aped, Bped
       Integer APul, BPul, minPul
       Integer NusedSMPL
       Integer SatLevel
       Integer LKR_minNdata, LKR_maxNdata, IwantAna, nDCP
       Integer Iflags,Time,Energy,icase

       Common /LKRpara/ Aped,Bped,APul,BPul,minPul,NusedSMPL,SatLevel,
     +                   LKR_minNdata,LKR_maxNdata,IwantAna,nDCP

      INTEGER          kafIHEA
      INTEGER KAFENH,KAFENE,KAFACTIVE
      PARAMETER (KAFENH=0, KAFENE=1, KAFACTIVE=2)
c      INTEGER    KAFHVDE, KAFPADE, KAFDEAD
c      PARAMETER  (KAFHVDE=3, KAFPADE=4, KAFDEAD=5)
c      INTEGER    LOSTCALI, G3MALA, MALADE
c      PARAMETER  (LOSTCALI=6,G3MALA=9,MALADE=10)
c      INTEGER BADCHANNEL, CPDDEAD, PERIMETRAL
c      PARAMETER (BADCHANNEL=11,PERIMETRAL=12 ,CPDDEAD=13)

      COMMON /kafCDE/ kafIHEA(0:127, 0:127)         
C------------------------------------------------------------------------------
 

      Integer   COUNT(1), GAIN(1) 
      Integer   isamp(nMaxSMPL),ig,ig2,j
      Integer   Ped0
      Integer   maxgain, maxcount
      Integer   nefsmpl, maxsmpl, satflag, lkrcell
      Integer   ZS

      Integer   xcha, ycha, nSMPL, iSMPL
      Integer   igtest(-2:2), ibads, ngood

      INTEGER CELLID,IQUALITY,IFLAG
      REAL PEAKENE,PEAKTIME
      COMMON /adcCDE/CELLID,PEAKENE,PEAKTIME,IQUALITY,
     &               IFLAG

        nefSMPL = nSMPL         ! number of effective samples (see ERR)

C	write(*,*)'Ichan:',ichan 

C 	write(*,*)'Gain: ',GAIN(iSMPL),' COUNT: ',COUNT(iSMPL)

C
C   is it an active cell ?     
        CELLID = -1
        PEAKENE = 0 
        PEAKTIME = 0
        IQUALITY = -1
        IF(.NOT. BTEST(kafIHEA(xcha,ycha), KAFACTIVE)) THEN
                 Energy = 0
                 Time = 0
                 return
        ENDIF

        ngood = 0
        maxcount=0
        maxgain=0
        maxsmpl=0
        DO iSMPL = MAX(APul-1,1), MIN(BPul+1,nefSMPL)
          ig = GAIN(iSMPL)
          IF(ig .EQ. 0) THEN
            IF(iSMPL.GE.2) THEN
               ig2= GAIN(iSMPL-1)
            ELSE
               ig2=ig
            ENDIF
            IF(ig2.EQ.ig) THEN
               ngood=ngood+1
               isamp(ngood)=iSMPL
            ENDIF
          ELSE
            IF(iSMPL.GE.2) THEN
               ig2= GAIN(iSMPL-1)                   
               IF(ig2.EQ.ig) THEN
                 ngood=ngood+1
                 isamp(ngood)=iSMPL
               ENDIF
            ENDIF
          ENDIF
        ENDDO

        DO iSMPL=2,ngood-1
          IF (GAIN(isamp(iSMPL)) .GT. maxgain) THEN
             maxgain = GAIN (isamp(iSMPL))
          ENDIF
        ENDDO
          
        DO iSMPL=2,ngood-1
          IF(GAIN(isamp(iSMPL))  .EQ. maxgain) THEN
             IF(COUNT(isamp(iSMPL)) .GT. maxcount) THEN
                maxcount=COUNT(isamp(iSMPL))
                maxsmpl=isamp(iSMPL)
             ENDIF
          ENDIF
        ENDDO

C Check saturations
        satflag=0
        IF(maxcount .GE. SatLevel) satflag = 1

        If (IwantAna.ne.0) Then               ! Only if analysis is requested

        Ped0=0       
        icase=-1
C Check gain switching quality and saturation
C Compute pedestal in gain 0
        DO j = APed, BPed
          Ped0 = Ped0+COUNT(j)
        ENDDO
        Ped0 = Ped0/(BPed-APed+1)

        CALL wzero(igtest,5)
        ibads=0
        DO iSMPL=-2,2
          IF((maxsmpl+iSMPL).GT.APul .AND.
     +       (maxsmpl+iSMPL).LE. MIN0(BPul,nefSMPL)) THEN
             igtest(iSMPL)=GAIN(maxsmpl+iSMPL)
             IF(COUNT(maxsmpl+iSMPL).GE.SatLevel) THEN 
               igtest(iSMPL)=100+igtest(iSMPL)
               ibads=ibads+1
             ENDIF
          ENDIF
        ENDDO
C 
        IF(ibads.eq.0) THEN                     ! no saturation
               IF   (igtest(-2).eq.maxgain.AND.
     &               igtest(-1).eq.maxgain.AND.
     &               igtest( 1).eq.maxgain) THEN
                    icase = 1                    ! all OK
               ELSEIF(igtest(-2).ne.maxgain.AND.
     &                igtest(-1).eq.maxgain.AND.
     &                igtest( 1).eq.maxgain) THEN
                     icase = 2                   ! use max and max+1
               ELSEIF(igtest(-2).eq.maxgain.AND.
     &                igtest(-1).eq.maxgain.AND.
     &                igtest( 1).ne.maxgain) THEN
                     icase = 3                   ! use max and max-1
               ELSEIF(igtest(-2).ne.maxgain.AND.
     &                igtest(-1).eq.maxgain.AND.
     &                igtest( 1).ne.maxgain.AND.
     &                igtest( 2).eq.igtest(1)) THEN
                     icase=6                     ! use max and max+2
               ELSEIF(igtest(-1).ne.maxgain.AND.
     &                igtest( 1).eq.maxgain) THEN
                      IF(igtest(-2).eq.igtest(-1)) THEN
                        icase=5                  ! use max-1 and max+1
                      ELSEIF(igtest(2).eq.maxgain) THEN
                        icase=4                  ! use max+1 and max+2
                      ELSE
c                       write(iqprnt,*)
c     &               ' Bad case 1 ',EvtNrun,evtNbur,EvtNumb,igtest
                       icase=99
                      ENDIF
               ELSE
c                       write(iqprnt,*)
c     &               ' Bad case 2 ',EvtNrun,evtNbur,EvtNumb,igtest
                      icase=99
               ENDIF

        ELSEIF(ibads.eq.1) THEN                  ! saturation

               IF(igtest(0).ge.100) THEN         ! maximum saturated

                 IF  (igtest(-2).eq.maxgain.AND.
     &                igtest(-1).eq.maxgain.AND.
     &                igtest( 1).eq.maxgain) THEN
                      icase=15                  ! use max-1 and max+1
                 ELSEIF(igtest(-2).ne.maxgain.AND.
     &                  igtest( 1).eq.maxgain.AND.
     &                  igtest( 2).eq.maxgain) THEN
                      icase=14                  ! use max+1 and max+2
                 ELSE
c                       write(iqprnt,*)
c     &               ' Bad case 3 ',EvtNrun,evtNbur,EvtNumb,igtest
                      icase=99
                 ENDIF
               ELSE                             ! maximum not saturated
                 IF  (igtest(-2).eq.maxgain.AND.
     &                igtest(-1).eq.maxgain.AND.
     &                igtest( 1).eq.maxgain) THEN
                      icase=21
                 ELSEIF(igtest(-2).ne.maxgain.AND.
     &                  igtest(-1).eq.maxgain.AND.
     &                  igtest( 1).eq.maxgain) THEN
                      icase=22
                 ELSEIF(igtest(-2).ne.maxgain.AND.
     &                 mod(igtest(-1),100).eq.maxgain.AND.
     &                 igtest( 1).eq.maxgain) THEN
                       icase=22
                 ELSEIF(igtest(-2).eq.maxgain.AND.
     &                  igtest(-1).eq.maxgain) THEN
                       icase=23
                 ELSEIF(igtest(-2).ne.maxgain.AND.
     &                  igtest(-1).eq.maxgain.AND.
     &                  igtest( 1).ge.100.AND.
     &                  mod(igtest(1),100).eq.igtest(2)) THEN
                       icase=26          ! use max and Max+2 to measure energy
                 ELSE
c                       write(iqprnt,*)
c     &               ' Bad case 4 ',EvtNrun,evtNbur,EvtNumb,igtest
                   icase=99
                 ENDIF
               ENDIF

        ELSEIF(ibads.ge.2) THEN
c            write(iqprnt,*) ' 2 or more saturations in LKR pulse',
c     &            evtNrun,evtNbur,evtNumb
             icase=98
        ENDIF
C 
        ENDIF               !End of Analysis
        
C
        CELLID = lkrcell
        PEAKENE = maxcount+10000*maxgain
        PEAKTIME = maxsmpl
        IQUALITY = icase 
C
        Iflags=0
C        IF(ZS.eq.1) NZScell = NZScell+1
        ZS = IABS(1-ZS)         
        Iflags=ior(ZS,('80'X))
        If(satflag.EQ.1) Iflags=ior(Iflags,('2'X))
        IF(maxgain.GT.0) Iflags=ior(Iflags,('40'X))
        IFLAG = Iflags
C

      RETURN
      END

