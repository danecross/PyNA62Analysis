      SUBROUTINE TURTLEINIT()                                               
CDC   PROGRAM TURTLE (INPUT, OUTPUT, TAPE5=INPUT, TAPE6=OUTPUT)                 
C     (TRACE UNLIMITED RAYS THROUGH LUMPED ELEMENTS)                            
C     MAKES ONE AND TWO DIMENSIONAL HISTOGRAMS FROM TRANSPORT DECK              
C     INCLUDES EFFECTS OF MANY HIGHER ORDER ABERRATIONS                         
C                                                                               
C     FNAL VERSION, APR 1971 - ORIGINAL PROGRAM - BY D. C. CAREY.               
C     CERN VERSION 2.0, AUG 1973 - DECAY INCLUDED - BY CH. ISELIN.              
C     CERN VERSION 2.1, NOV 1976 - ANSI COMPATIBLE - BY CH. ISELIN.             
C                                                                               
C ----------------------------------------------------------------------        
C ----------------------------------------------------------------------        
C                                                                               
C      DESCRIPTION OF FUNCTIONS AND SUBROUTINES                                 
C                                                                               
C ----------------------------------------------------------------------        
C S/R APLIM        PRINTS TABLE OF PARTICLES STOPPED BY APERTURES               
C FCT BCDW         CONVERTS AN INTEGER INTO A BCD CHARACTER                     
C S/R BEND         TRACKS CHARGED PARTICLES THRU A BENDING MAGNET               
C FCT BSTOP        TESTS FOR BENDING MAGNET APERTURES                           
C S/R DECAY        GENERATES THE DECAY   PI (OR K) --> MU + NU                  
C S/R DECAYP       PREPARES DATA FOR DECAY                                      
C S/R DECAYZ       GENERATES A DECAY ABSCISSA                                   
C FCT DIGIT        RETURNS ONE DIGIT OF AN INTEGER AS A BCD WORD                
C S/R EHIST        ENTERS A RAY INTO THE REQUESTED HISTOGRAMS                   
C S/R EMIT         GENERATES A PARENT PARTICLE (PI OR K)                        
C S/R ENTMAT       ENTERS A PAIR OF VALUES INTO A 2-D HISTOGRAM                 
C S/R FOCUS        FINDS THE FOCUSSING MATRIX OF A QUADRUPOLE                   
C S/R HEDING       PRINTS THE TABLE OF HISTOGRAM REQUESTS                       
C S/R HISCAP       PRINTS HISTOGRAM CAPTIONS                                    
C S/R HISTO        ENTERS A VALUE INTO A 1-D HISTOGRAM                          
C S/R HSTUN        RETURNS VALUE NAME, UNIT NAME, AND CONVERSION FACTOR         
C                  FOR A VALUE TO BE HISTOGRAMMED                               
C FCT HSTVAL       RETURNS A VALUE TO BE HISTOGRAMMED                           
C S/R INITZE       SETS UP DEFAULT VALUES FOR A PARTICLE BEFORE ENTERING        
C                  THE BEAM LINE                                                
C S/R MULTI        APPLIES THE MULTIPOLE ERRORS AT ENTRY AND EXIT OF A          
C                  QUADRUPOLE                                                   
C FCT NU           RETURNS THE STORAGE BLOCK SIZE NEEDED FOR AN ELEMENT         
C S/R OHIST        PRINTS THE HISTOGRAMS                                        
C S/R OUTMAT       PRINTS A 2-D HISTOGRAM (SCATTER PLOT)                        
C S/R PRINT1       PRINTS THE DATA FOR ONE ELEMENT                              
C S/R QUAD         TRACKS CHARGED PARTICLES THRU A QUADRUPOLE                   
C FCT QSTOP        TESTS FOR QUADRUPOLE APERTURES                               
C S/R RAYRUN       TRACKS ONE PARENT PARTICLE AND ITS TWO DAUGHTERS             
C S/R RDELMT       READS DATA FOR ONE ELEMENT                                   
C S/R RDFIX        READS ONE INTEGER                                            
C S/R RDFLT        READS ONE REAL NUMBER                                        
C S/R RDNEXT       RETURNS NEXT NON-BLANK CHARACTER ON INPUT                    
C S/R RDSKIP       SKIPS COMMENTS ON INPUT                                      
C S/R RDSTRG       READS AN ALPHANUMERIC STRING                                 
C S/R REPETE       KEEPS TRACK OF REPETE ELEMENTS                               
C S/R RESET        COPIES A VECTOR                                              
C S/R SCATT        APPLIES MULTIPLE SCATTERING                                  
C S/R SETUP        READS INPUT AND PROCESSES DATA FOR TRACKING                  
C S/R SEXT         TRACKS CHARGED PARTICLES THRU A SEXTUPOLE                    
C S/R SOLO         TRACKS CHARGED PARTICLES THRU A SOLENOID                     
C S/R TRHIST       PRINTS A 1-D HISTOGRAM                                       
C S/R ZERO         CLEARS HISTOGRAM STORAGE                                     
C ----------------------------------------------------------------------        
C ----------------------------------------------------------------------        
C                                                                               
C      DESCRIPTION OF COMMON BLOCKS AND VARIABLES                               
C                                                                               
C ----------------------------------------------------------------------        
C BLOC1               REPRESENTATION OF BEAM LINE                               
      COMMON /BLOC1/ DATA(3001), I, I1                                          
      INTEGER IDATA(3001)                                                       
      EQUIVALENCE (IDATA(1), DATA(1))                                           
C DATA(3001)             STORAGE FOR TYPE CODES, RELATED MATRIX                 
C                        ELEMENTS, ETC.                                         
C I                      POINTER WITHIN DATA ARRAY.                             
C I1                     MAXIMUM VALUE OF I.                                    
C ----------------------------------------------------------------------        
C BLOC2               UNITS CHANGES                                             
      REAL*8 UNIT, NAME
      COMMON /BLOC2/ UNIT(11), NAME(11)
C UNIT(11)               CONVERSION UNITS.                                      
C NAME(11)               NAMES OF UNITS.                                        
C ----------------------------------------------------------------------        
C BLOC3               FORTRAN I/O UNITS                                         
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
C NIN                    UNIT NUMBER FOR INPUT DATA.                            
C NOUT                   UNIT NUMBER FOR PRINTOUT.                              
C ----------------------------------------------------------------------        
C BLOC4               INPUT PHASE SPACE                                         
      COMMON /BLOC4/ RAYMAX(6), PBEAM, P0, NORAYS                               
C RAYMAX(6)              MAXIMUM EXTENTS OF ORIGINAL PHASE SPACE.               
C PBEAM                  ORIGINAL CENTRAL MOMENTUM OF THE BEAM.                 
C P0                     DESIGN MOMENTUM OF THE BEAM LINE.                      
C NORAYS                 NUMBER OF RAYS.                                        
C ----------------------------------------------------------------------        
C BLOC5               SPECIAL THINGS                                            
      COMMON /BLOC5/ SEC, CAP(3), MPA                                           
      LOGICAL SEC, CAP, MPA                                                     
C SEC          LOGICAL   TRUE IF THIS IS A SECOND ORDER RUN.                    
C CAP(3)       LOGICAL   TELLS IF APERTURES ARE TO BE OBSERVED.                 
C MPA          LOGICAL   TRUE IF MULTIPOLES FOR QUARS ARE USED.                 
C ----------------------------------------------------------------------        
C BLOC6               COUNTS OF HISTOGRAMS @ STORAGE                            
      COMMON /BLOC6/ NHISTS, NHIST, LIMNO                                       
C NHISTS                 TOTAL NUMBER OF HISTOGRAMS.                            
C NHIST                  NUMBER OF CURRENT HISTOGRAMS.                          
C LIMNO                  AMOUNT OF STORAGE USED FOR HISTOGRAMS.                 
C ----------------------------------------------------------------------        
C BLOC7               INFORMATION ABOUT HISTOGRAMS                              
      COMMON /BLOC7/ MTYPE(100), NENTRY(100), NCOOR(100), INTVLS(100),          
     A               BEGIN(100), END(100), STEP(100), ZHIST(100)                
C MTYPE(100)             TYPE OF HISTOGRAM. (50, 51, OR 52)                     
C NENTRY(100)            BEGINNING OF HISTOGRAM STORAGE IN MTABLE.              
C NCOOR(100)             COORDINATE TO BE HISTOGRAMED.                          
C INTVLS(100)            NUMBER OF INTERVALS TO BE IN HISTOGRAM.                
C BEGIN(100)             LOWER LIMIT.                                           
C END(100)               UPPER LIMIT.                                           
C STEP(100)              STEP SIZE.                                             
C ZHIST(100)             LONGITUDINAL POSITION OF HISTOGRAM REQUEST.            
C ----------------------------------------------------------------------        
C BLOC8               HISTOGRAM STORAGE                                         
      COMMON /BLOC8/ MTABLE(1000000)                                              
C MTABLE(1000000)                                                                 
C ----------------------------------------------------------------------        
C BLOC9               QUANTITIES DESCRIBING DIPOLES                             
      COMMON /BLOC9/ L, H, N, BDB                                               
      REAL L, N                                                                 
C L            REAL      LENGTH OF ELEMENT.                                     
C H                      1/RADIUS OF CURVATURE.                                 
C N            REAL      NORMALIZED FIELD GRADIENT.                             
C BDB                    NORMALIZED SECOND DERIVATIVE OF B.                     
C ----------------------------------------------------------------------        
C BLOC10              PERTAINS TO FRINGING FIELDS                               
      COMMON /BLOC10/ APB(2), LAYL, LAYX, RAB1, RAB2, FACE                      
      REAL LAYL, LAYX                                                           
C APB(2)                 PARAMETER ON 16. 4. AND 16. 5. CARDS.                  
C LAYL         REAL      K(1) ON 16. 7. CARD, PAGE 16-4 SLAC 91.                
C LAYX         REAL      K(2) ON 16. 8. CARD, PAGE 16-4 SLAC 91.                
C RAB1                   CODE 12 ON PAGE 16-3 SLAC 91.                          
C RAB2                   CODE 13 ON PAGE 16-3 SLAC 91.                          
C FACE                   BETA ANGLE OF POLE FACE ROTATION.                      
C ----------------------------------------------------------------------        
C BLOC11              ELLIPTICAL APERTURE FOR QUADRAPOLES                       
      COMMON /BLOC11/ QAP(3), ELLIPS, HYPER                                     
      LOGICAL ELLIPS, HYPER                                                     
C QAP(3)                 DIMENSIONS FOR QUADRUPOLE APERTURES.                   
C ELLIPS       LOGICAL   TRUE IF ELLIPTICAL APERTURE FOR QUADS HAS              
C                        BEEN SPECIFIED.                                        
C HYPER        LOGICAL   TRUE IF HYPERBOLIC APERTURE FOR QUADS HAS              
C                        BEEN SPECIFIED.                                        
C ---------------------------------------------------------------------         
C BLOC12              PERTAINS TO HISTOGRAM FLAGGING                            
      COMMON /BLOC12/ VALUE(100), XMOM(100), XMOM2(100), NFLAG(100),            
     A                SET(100), KFLAG(100), IFLAG(3,9), ZFLAG(9)                
      LOGICAL SET, IFLAG                                                        
C VALUE(100)             VALUES TO BE HISTOGRAMMED.                             
C XMOM(100)              FIRST MOMENTS.                                         
C XMOM2(100)             SECOND MOMENTS.                                        
C NFLAG(100)             FLAG NUMBER FOR HISTOGRAM.                             
C SET(100)     LOGICAL   TELLS IF COORDINATE TO BE HISTOGRAMMED HAS             
C                        BEEN STORED INTO VALUE                                 
C KFLAG(100)             PARTICLE KIND TO BE HISTOGRAMMED                       
C IFLAG(3,9)   LOGICAL   IFLAG(I,K) TELLS IF PARTICLE I HAS ENCOUNTERED         
C                        THE FLAG K.                                            
C ZFLAG(9)               LOCATION OF FLAG.                                      
C --------------------------------------------------------------------          
C BLOC13              DATA FOR THE CURRENT PARTICLE TO BE TRACKED.              
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
C RAY(6)                 CURRENT RAY COORDINATES                                
C RAYC(6)                STARTING CONDITIONS FOR CHARGED DAUGHTER               
C RAYN(6)                STARTING CONDITIONS FOR NEUTRAL DAUGHTER               
C P                      MOMENTUM OF PARENT                                     
C PC                     MOMENTUM OF CHARGED DAUGHTER                           
C PN                     MOMENTUM OF NEUTRAL DAUGHTER                           
C FLD                    P / DECAY LENGTH                                       
C MASS         REAL      PARENT MASS                                            
C MASSC        REAL      CHARGED DAUGHTER MASS                                  
C MASSN        REAL      NEUTRAL DAUGHTER MASS                                  
C FC                     ETA* FOR CHARGED DAUGHTER                              
C FN                     ETA* FOR NEUTRAL DAUGHTER                              
C PSTAR                  PSTAR FOR THE PARTICULAR DECAY                         
C ZMAX                   IF NON-ZERO, MAXIMUM LENGTH OF DECAY CHANNEL           
C DKFLAG       LOGICAL   TRUE, IF DECAY IS TO BE CONSIDERED                     
C Z                      CURRENT LONGITUDINAL POSITION OF PARTICLE              
C                        TRACKED.                                               
C ----------------------------------------------------------------------        
C BLOC14              DATA FOR MULTIPOLE ABERRATIONS IN QUADRUPOLES             
      COMMON /BLOC14/ MMAX, GTOP, DB(20), COSM(20), SINM(20)                    
C MMAX                   LARGEST ORDER OF MULTIPOLES TO BE CONSIDERED           
C GTOP                   FACTOR FOR MULTIPOLES                                  
C DB(20)                 MULTIPOLE COEFFICIENTS                                 
C COSM(20)               COSINE OF PHASE ANGLES                                 
C SINM(20)               SINE OF PHASE ANGLES                                   
C ----------------------------------------------------------------------        
C BLOC15              MISCELLANEOUS DATA FOR TRACKING                           
      COMMON /BLOC15/ LL, KH2, KV2, KH, KV, KHL, KVL, RAY2(6),                  
     A                RH(4), RV(4)                                              
      REAL LL, KH2, KV2, KH, KV, KHL, KVL                                       
C LL           REAL      LENGTH OR FRACTIONAL LENGTH OF AN ELEMENT              
C KH2          REAL      KX ** 2                                                
C KV2          REAL      KY ** 2                                                
C KH           REAL      KX                                                     
C KV           REAL      KY                                                     
C KHL          REAL      KX * L                                                 
C KVL          REAL      KY * L                                                 
C RAY2(6)                TEMPORARY STORE FOR RAY COORDINATES                    
C RH(4)                  HORIZONTAL FIRST ORDER MATRIX                          
C RV(4)                  VERTICAL FIRST ORDER MATRIX                            
C ----------------------------------------------------------------------        
C BLOC16              TABLE OF PARTICLES HITTING THE APERTURES                  
      COMMON /BLOC16/ KPART(3), ISTOP(3,500)                                    
C KPART(3)               PARTICLE NAMES                                         
C ISTOP(3,100)           NUMBER OF PARTICLES HITTING APERTURES                  
C ----------------------------------------------------------------------        
C BLOC17              PERTAINS TO REPETE ELEMENT                                
      COMMON /BLOC17/ IP, IC(4), IS(4)                                          
C IP                     LEVEL OF NESTED REPETES                                
C IC(4)                  REPETITION COUNT                                       
C IS(4)                  POINTS TO BEGIN OF REPETITION SECTIONS                 
C ----------------------------------------------------------------------        
C BLOC18              LOSS HISTOGRAMS                                           
      COMMON /BLOC18/ LOSS, NBLK(10), KFLG(10),                                 
     A                XSTRT(10), XEND(10), LTHX(10), XBIN(10), NUNX(10),        
     B                YSTRT(10), YEND(10), LTHY(10), YBIN(10), NUNY(10)         
C LOSS                   NUMBER OF LOSS HISTOGRAMS SPECIFIED                    
C NBLK (10)              BLOCK ADDRESS OF COUNTER AREA                          
C KFLG (10)              PARTICLE FLAG                                          
C XSTRT(10)              LOWER X COORDINATE                                     
C XEND (10)              UPPER X COORDINATE                                     
C LTHX (10)              NUMBER OF X BINS                                       
C XBIN (10)              X BIN SIZE                                             
C NUNX (10)              NUMBER OF X UNIT                                       
C YSTRT(10)              LOWER Y COORDINATE                                     
C YEND (10)              UPPER Y COORDINATE                                     
C LTHY (10)              NUMBER OF Y BINS                                       
C YBIN (10)              Y BIN SIZE                                             
C NUNY (10)              NUMBER OF Y UNIT                                       
C ----------------------------------------------------------------------        
C BLOC41              CONTAINS DATA FOR ELEMENT JUST READ                       
      COMMON /BLOC41/ LW, IMAGE(20), FLUSH, INDIC, NTYPE, LABLE, LENGTH,        
     A                NWORD, NVARY, DATUM(30), VARY(30)                         
      INTEGER VARY, TEXT(30)                                                    
      LOGICAL FLUSH                                                             
      EQUIVALENCE (TEXT(1), DATUM(1))                                           
C LW                     LENGTH OF TITLE CARD IN WORDS                          
C IMAGE(20)              CONTENT OF TITLE CARD                                  
C FLUSH        LOGICAL   TELLS IF RUN HAS BEEN FLUSHED                          
C INDIC                  NOT USED                                               
C NTYPE                  TYPE CODE OF ELEMENT CURRENTLY BEING READ              
C LABLE                  LABEL OF THIS ELEMENT                                  
C LENGTH                 EXPECTED NUMBER OF DATA ITEMS                          
C NWORD                  ACTUAL NUMBER OF DATA ITEMS                            
C NVARY                  NUMBER OF VARY CODES                                   
C DATUM(30)              DATA ITEMS READ FOR CURRENT ELEMENT                    
C VARY(30)     INTEGER   VARY CODES FOR CURRENT ELEMENT                         
C --------------------------------------------------------------------          
C BLOC42              HOLLERITH LITTERALS                                       
      COMMON /BLOC42/ TABLE(36), PLUS(2), MINUS, BLANK, PERIOD,                 
     A                SEMI(5), QUOTE(6), PAR1(2), PAR2(2)                       
      INTEGER TABLE, PLUS, BLANK, PERIOD, SEMI, QUOTE, PAR1, PAR2               
C TABLE(36)    INTEGER   DIGITS AND LETTERS                                     
C PLUS(2)      INTEGER   + &                                                    
C MINUS                  -                                                      
C BLANK        INTEGER   BLANK                                                  
C PERIOD       INTEGER   .                                                      
C SEMI(5)      INTEGER   ; , * $ !                                              
C QUOTE(6)     INTEGER   " ' / = @ #                                            
C PAR1(2)      INTEGER   ( :                                                    
C PAR2(2)      INTEGER   ) <                                                    
C --------------------------------------------------------------------          
C BLOC43              CURRENT INPUT CARD BEING DECODED                          
      COMMON /BLOC43/ NCD, CARD(80), MC, EMPTY, ITEM                            
      INTEGER CARD                                                              
      LOGICAL EMPTY                                                             
C NCD                    NUMBER OF CURRENT INPUT CARD                           
C CARD(80)     INTEGER   CURRENT INPUT CARD IMAGE                               
C MC                     CURRENT INPUT CHARACTER POSITION                       
C EMPTY        LOGICAL   TELLS IF CURRENT CARD IS EMPTY                         
C ITEM                   CURRENT INPUT CHARACTER VALUE                          
C ----------------------------------------------------------------------       
C TURTLINT
      INTEGER NPART
      COMMON /TURTLEINT/ NPART                            
C ----------------------------------------------------------------------       
C TURTLEDOUBLE
      REAL*8 XPOS,YPOS,ZPOS,XPMOM,YPMOM,ZPMOM
      COMMON /TURTLEDOUBLE/ XPOS, YPOS, ZPOS, XPMOM, YPMOM, ZPMOM                            
C ----------------------------------------------------------------------        
C ----------------------------------------------------------------------        
C                                                                               
      CHARACTER*100 FILENAME
      COMMON /TURTLECHAR/ FILENAME
C                                                                               
C                                                                               
C     PRINT PROGRAM VERSION IDENTIFICATION                                      
C                                                                               
c      WRITE (NOUT, 10)                                                          
c   10 FORMAT(42H1DECAY TURTLE, CERN VERSION B87, FEB 1995.,/                    
c     A       40H0THIS PROGRAM IS DESCRIBED IN CERN 74-2.,/                      
c     B       49H0TIME LIMIT RECOVERY IS INCLUDED IN THIS VERSION.,/             
c     C       50H0NOTE THAT THE PROGRAM NOW NEEDS THE CERN LIBRARY.,/            
c     D       54H0THE FOLLOWING ROUTINES MAY BE REPLACED BY THE USER...,/        
c     E       43H      EMIT     (GENERATE A PARENT PARTICLE),/                   
c     F       54H      HISUN    (NAME, UNIT NAME AND CONVERSION FACTOR ,         
c     G       42HFOR USER-DEFINED VALUE TO BE HISTOGRAMMED),/                    
c     H       54H      HISVAL   (USER-DEFINED VALUE TO BE HISTOGRAMMED),/        
c     I       42H      SCATT    (APPLY MULTIPLE SCATTERING),/                    
c     Y       1H0,/                                                              
c     Z       52H0FOR FURTHER INFORMATION CALL CH. ISELIN, EXT. 3657.)           
C                                                                               
C     READ DATA AND PREPARE FOR TRACKING                                        
C                                                                               
C     PRINT*,FILENAME
      OPEN(unit=11,file=FILENAME)
  100 CALL INITZE                                                               
      CALL SETUP                                                                
      IF(NHISTS + LOSS .EQ. 0) GO TO 100                                        
c      CALL HEDING                                                               
      CALL ZERO                                                                 
      LEFT = 1000000 - LIMNO                                                    
c      WRITE (NOUT, 1000) LEFT                                                   
c 1000 FORMAT(1H0,I10,2X,33HPLACES LEFT FOR HISTOGRAM STORAGE)                   
      RETURN
      END
      SUBROUTINE TURTLELOOP
      COMMON /BLOC4/ RAYMAX(6), PBEAM, P0, NORAYS                               
      COMMON /BLOC7/ MTYPE(100), NENTRY(100), NCOOR(100), INTVLS(100),          
     A               BEGIN(100), END(100), STEP(100), ZHIST(100)                
      COMMON /BLOC12/ VALUE(100), XMOM(100), XMOM2(100), NFLAG(100),            
     A                SET(100), KFLAG(100), IFLAG(3,9), ZFLAG(9)                
      LOGICAL SET, IFLAG                                                        
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      INTEGER NPART
      COMMON /TURTLEINT/ NPART                            
      REAL*8 XPOS,YPOS,ZPOS,XPMOM,YPMOM,ZPMOM
      COMMON /TURTLEDOUBLE/ XPOS, YPOS, ZPOS, XPMOM, YPMOM, ZPMOM                  
      REAL*8 PMOM,XPRI,YPRI
C                                                                               
C     LOOP FOR PARTICLE TRACKING                                                
C                                                                               
200   NORAYS = 1                                                                
      CALL RAYRUN                                                               
      CALL EHIST                                                                
      IF (Z.LT.ZHIST(1)) GO TO 200
C
C     FILL THE COMMON BLOCK FOR DATA EXPORT
C
      NPART = NORAYS 
      XPOS = VALUE(1)
      YPOS = VALUE(2)
      ZPOS = ZHIST(1)
      XPRI = VALUE(3)
      YPRI = VALUE(4) 
      PMOM = VALUE(5)
      ZPMOM = PMOM/SQRT(1.+XPRI*XPRI+YPRI*YPRI)
      XPMOM = ZPMOM*XPRI
      YPMOM = ZPMOM*YPRI
CC      PRINT*,'TURTLEVAR ',Z,XPOS,YPOS,ZPOS,XPMOM,YPMOM,ZPMOM,PMOM
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TURTLECLOSE
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
c      CALL OHIST                                                                
c      CALL APLIM                                                                
      CLOSE(NIN) 
      RETURN
      END
      BLOCK DATA                                                                
C                                                                               
      REAL*8 UNIT, NAME
      COMMON /BLOC2/ UNIT(11), NAME(11)
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      COMMON /BLOC42/ TABLE(36), PLUS(2), MINUS, BLANK, PERIOD,                 
     A                SEMI(5), QUOTE(6), PAR1(2), PAR2(2)                       
      INTEGER TABLE, PLUS, BLANK, PERIOD, SEMI, QUOTE, PAR1, PAR2               
C                                                                               
      DATA UNIT / 0.01, 0.001, 0.01, 0.001, 1.0, 0.01, 0.01745329,              
     A            1.0, 1.0, 1.0, 1.0 /                                          
      DATA NAME / 4HCM  , 4HMR  , 4HCM  , 4HMR  , 4H????, 4HPC  ,               
     A            4HDEG , 4HM   , 4HKG  , 4HEM  , 4HGEVC /                      
      DATA NIN, NOUT /11,6/                                                     
      DATA TABLE / 1H0, 1H1, 1H2, 1H3, 1H4, 1H5, 1H6, 1H7, 1H8, 1H9,            
     A             1HA, 1HB, 1HC, 1HD, 1HE, 1HF, 1HG, 1HH, 1HI, 1HJ,            
     B             1HK, 1HL, 1HM, 1HN, 1HO, 1HP, 1HQ, 1HR, 1HS, 1HT,            
     C             1HU, 1HV, 1HW, 1HX, 1HY, 1HZ /                               
C ----------------------------------------------------------------------        
C                                                                               
C THE DATA STATEMENTS BELOW REFLECT THE FOLLOWING CODE CONVENTIONS              
C FOR SPECIAL CHARACTERS...                                                     
C                                                                               
C   CARD PUNCH     USED AS     026 CHAR.     029 CHAR.                          
C    12               +            +             &                              
C    12-8-6           +                          +                              
C    11               -            -             -                              
C    BLANK          BLANK        BLANK         BLANK                            
C    12-8-3           .            .             .                              
C    11-8-6           ;                          ;                              
C     0-8-3           ;            ,             ,                              
C    11-8-4           ;            *             *                              
C    11-8-3           ;            $             $                              
C    12-8-7           ;            ;             !                              
C     8-7             "            '             "                              
C     8-5             "                          '                              
C     0-1             "            /             /                              
C     8-6             "                          =                              
C     8-4             "            "             @                              
C     8-3             "            =             #                              
C    12-8-5           (                          (                              
C     0-8-4           (            (             :                              
C    11-8-5           )                          )                              
C    12-8-4           )            )             <                              
C ----------------------------------------------------------------------        
      DATA    PLUS  / 1H+, 1H& /                                                
      DATA    MINUS / 1H- /                                                     
      DATA    BLANK / 1H  /                                                     
      DATA    PERIOD/ 1H. /                                                     
      DATA    SEMI  / 1H;, 1H,, 1H*, 1H$, 1H! /                                 
      DATA    QUOTE / 1H", 1H', 1H/, 1H=, 1H@, 1H# /                            
      DATA    PAR1  / 1H(, 1H: /                                                
      DATA    PAR2  / 1H), 1H< /                                                
C                                                                               
      END                                                                       
      SUBROUTINE APLIM                                                          
C                                                                               
C S/R APLIM        PRINTS TABLE OF PARTICLES STOPPED BY APERTURES               
C                                                                               
      COMMON /BLOC1/ DATA(3001), I, I1                                          
      INTEGER IDATA(3001)                                                       
      EQUIVALENCE (IDATA(1), DATA(1))
      REAL*8 UNIT, NAME
      COMMON /BLOC2/ UNIT(11), NAME(11)
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      COMMON /BLOC16/ KPART(3), ISTOP(3,500)                                    
      INTEGER TYPE(3), KSTOP(3)                                                 
      DATA TYPE / 4HBEND, 4HQUAD, 4HSLIT /                                      
C                                                                               
      LAST = 1                                                                  
      IF(DKFLAG) LAST = 3                                                       
      WRITE (NOUT, 900) (KPART(J), J = 1, LAST)                                 
      DO 10 J = 1, LAST                                                         
   10 KSTOP(J) = 0                                                              
      IAP = 0                                                                   
      I = 1                                                                     
      ZC = 0.0                                                                  
      FZ = 1.0 / UNIT(8)                                                        
   20 NTYPE = IDATA(I)                                                          
      IF(NTYPE .LE. 0 .OR. NTYPE .GT. 21) GO TO 90                              
      GO TO (90, 90, 50, 30, 30, 30, 90, 90, 60, 90, 90, 90, 90, 90,            
     A       90, 90, 90, 50, 50, 90, 90), NTYPE                                 
   30 IAP = IAP + 1                                                             
      DO 40 J = 1, LAST                                                         
   40 KSTOP(J) = KSTOP(J) + ISTOP(J,IAP)                                        
      Z = ZC * FZ                                                               
      WRITE (NOUT, 910) Z, NAME(8), TYPE(NTYPE-3),                              
     A                  (ISTOP(J,IAP), J = 1, LAST)                             
   50 IF(NTYPE .EQ. 6) GO TO 90                                                 
      ZC = ZC + DATA(I+1)                                                       
      GO TO 90                                                                  
   60 CALL REPETE                                                               
   90 I = I + NU(NTYPE)                                                         
      IF(I .LE. I1) GO TO 20                                                    
      WRITE (NOUT, 920) (KSTOP(J), J = 1, LAST)                                 
      RETURN                                                                    
C                                                                               
  900 FORMAT(42H1SUMMARY OF PARTICLES STOPPED BY APERTURES/1X,41(1H-)/          
     A       1H0,10X,7HPOSITON,8X,4HTYPE,3(8X,A4))                              
  910 FORMAT(1X,F15.3,1X,A4,5X,A4,3I12)                                         
  920 FORMAT(6H0TOTAL,24X,3I12)                                                 
      END                                                                       
      INTEGER FUNCTION BCDW (INTGER)                                            
C                                                                               
C FCT BCDW         CONVERTS AN INTEGER INTO A BCD CHARACTER                     
C                                                                               
      COMMON /BLOC42/ TABLE(36), PLUS(2), MINUS, BLANK, PERIOD,                 
     A                SEMI(5), QUOTE(6), PAR1(2), PAR2(2)                       
      INTEGER TABLE, PLUS, BLANK, PERIOD, SEMI, QUOTE, PAR1, PAR2               
C                                                                               
      IF (INTGER .GT. 0) GO TO 100                                              
      BCDW = BLANK                                                              
      RETURN                                                                    
  100 IF (INTGER .GT. 35) GO TO 200                                             
      BCDW = TABLE(INTGER+1)                                                    
      RETURN                                                                    
  200 BCDW = SEMI(4)                                                            
      RETURN                                                                    
      END                                                                       
      SUBROUTINE BEND (KSTOP)                                                   
C                                                                               
C S/R BEND         TRACKS CHARGED PARTICLES THRU A BENDING MAGNET               
C                                                                               
      COMMON /BLOC1/ DATA(3001), I, I1                                          
      INTEGER IDATA(3001)                                                       
      EQUIVALENCE (IDATA(1), DATA(1))                                           
      COMMON /BLOC4/ RAYMAX(6), PBEAM, P0, NORAYS                               
      COMMON /BLOC5/ SEC, CAP(3), MPA                                           
      LOGICAL SEC, CAP, MPA                                                     
      COMMON /BLOC9/ L, H, N, BDB                                               
      REAL L, N                                                                 
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      COMMON /BLOC15/ LL, KH2, KV2, KH, KV, KHL, KVL, RAY2(6),                  
     A                RH(4), RV(4)                                              
      REAL LL, KH2, KV2, KH, KV, KHL, KVL                                       
C                                                                               
      KSTOP = 0                                                                 
      H = DATA(I+2)                                                             
      N = DATA(I+3)                                                             
      BDB = DATA(I+5)                                                           
      IF(N .EQ. 0.0 .AND. BDB .EQ. 0.0) GO TO 300                               
      KH2 = H ** 2 * (1.0 - N)                                                  
      KV2 = H ** 2 * N                                                          
      CALL FOCUS (KH2, LL, KH, KHL, RH)                                         
      CALL FOCUS (KV2, LL, KV, KVL, RV)                                         
      CX = RH(1)                                                                
      SX = RH(2)                                                                
      CY = RV(1)                                                                
      SY = RV(2)                                                                
      IF(ABS(KHL) .LT. 0.01) GO TO 10                                           
      X10 = (1.0 - CX) / KH2                                                    
      Q1  = (LL  - SX) / KH2                                                    
      Q2  = (3.0 * Q1 - SX * X10) / KH2                                         
      GO TO 20                                                                  
   10 SKL = KHL ** 2                                                            
      X10 = LL ** 2 * (0.5 - SKL / 24.0)                                        
      Q1  = LL ** 3 * (1.0 / 6.0 - SKL / 120.0)                                 
      Q2  = LL ** 5 * (0.1 - SKL / 84.0)                                        
   20 IF(ABS(KVL) .LT. 0.01) GO TO 30                                           
      Y34 = 0.5 * (SY - LL * CY) / KV2                                          
      GO TO 40                                                                  
   30 Y34 = LL ** 3 * (1.0 / 6.0 - KV2 * LL ** 2 / 60.0)                        
   40 X11 = 0.5 * LL * SX                                                       
      X12 = 0.5 * (LL * X10 - Q1)                                               
      X16 = 0.5 * (X10 ** 2 - SX * Q1)                                          
      X21 = 0.5 * (SX + LL * CX)                                                
      X22 = X11                                                                 
      X26 = X12                                                                 
      Y33 = 0.5 * LL * SY                                                       
      Y43 = 0.5 * (SY + LL * CY)                                                
      Y44 = Y33                                                                 
      D = KH2 - 4.0 * KV2                                                       
      IF(ABS(D * LL ** 2) .LT. 1.0E-6) GO TO 50                                 
      X144 = (SY ** 2 - 2.0 * X10) / D                                          
      X244 = 2.0 * (SY * CY - SX) / D                                           
      Y324 = (2.0 * CY * X10 - SX * SY) / D                                     
      Y424 = (SX * CY - CX * SY + 2.0 * RV(3) * X10) / D                        
      GO TO 100                                                                 
   50 X144 = 2.0 * X16                                                          
      X244 = 2.0 * X12                                                          
      Y324 = 0.5 * SY * Q1                                                      
      Y424 = 0.5 * (SY * X10 + CY * Q1)                                         
  100 X112 = SX * X10 / 3.0                                                     
      X116 = 0.5 * SX * Q1 - X10 ** 2 / 6.0                                     
      X122 = X10 ** 2 / 3.0                                                     
      X126 = (SX * X10 ** 2 - CX * Q2) / 6.0                                    
      X166 = (X10 ** 3 - SX * Q2) / 3.0                                         
      X212 = (2.0 * SX ** 2 - X10) / 3.0                                        
      X216 = 2.0 * X112 - 0.5 * (LL * X10 - Q1)                                 
      X222 = 2.0 * X112                                                         
      X226 = SX * Q1 - X116                                                     
      X266 = 2.0 * X126                                                         
      Y323 = SY * X10 - Y424                                                    
      Y423 = CY * X10 + KV2 * Y324                                              
      Y336 = 0.5 * (SY * Q1 - Y324)                                             
      Y436 = 0.5 * (CY * Q1 + Y323)                                             
      IF(ABS(KH2) .LT. 0.0001) GO TO 110                                        
      Y346 = (Y34 + SY * X10 - 2.0 * Y424) / KH2                                
      GO TO 120                                                                 
  110 Y346 = 0.5 * (SY * X10 - CY * Q1 - Y424) / KV2                            
  120 Y446 = 0.5 * (SY * Q1 + Y324)                                             
      A1 = H ** 3 * (N + N - BDB - 1.0) * 2.0                                   
      A2 = H * (2.0 - N)                                                        
      C1 = H ** 3 * (BDB - N) * 2.0                                             
      C2 = H * N                                                                
      T116 = A1*X116 - KH2*H*X122 + A2*X11                                      
      T216 = A1*X216 - KH2*H*X222 + A2*X21 - H * (RH(1)*SX + RH(3)*X10)         
      T126 = A1*X126 + H*X112 + A2*X12                                          
      T226 = A1*X226 + H*X212 + A2*X22 - H * (RH(2)*SX + RH(4)*X10)             
      T166 = 0.5 * (A1*X166 + H*X122) + A2*X16                                  
      T266 = 0.5 * (A1*X266 + H*X222) + A2*X26 - H*X10*SX                       
      T336 = C1*Y336 - KV2*H*Y324 + C2*Y33                                      
      T436 = C1*Y436 - KV2*H*Y424 + C2*Y43 - H*X10*RV(3)                        
      T346 = C1*Y346 + H*Y323 + C2*Y34                                          
      T446 = C1*Y446 + H*Y423 + C2*Y44 - H*X10*RV(4)                            
      DH = H * (P - P0) / P                                                     
      RAY2(1) = (RH(1) + DH*T116) * RAY(1) + (RH(2) + DH*T126) * RAY(2)         
     A        + (X10  + DH*T166) * DH                                           
      RAY2(2) = (RH(3) + DH*T216) * RAY(1) + (RH(4) + DH*T226) * RAY(2)         
     A        + (SX   + DH*T266) * DH                                           
      RAY2(3) = (RV(1) + DH*T336) * RAY(3) + (RV(2) + DH*T346) * RAY(4)         
      RAY2(4) = (RV(3) + DH*T436) * RAY(3) + (RV(4) + DH*T446) * RAY(4)         
      IF(.NOT. SEC) GO TO 200                                                   
      X111 = X10 - KH2 * X122                                                   
      X133 = X10 - KV2 * X144                                                   
      X134 = 0.5 * X244                                                         
      X211 = SX  - KH2 * X222                                                   
      X233 = SX  - KV2 * X244                                                   
      X234 = X133 - KV2 * X144                                                  
      Y313 = 0.5 * (SX * SY + KH2 * Y324)                                       
      Y314 = 2.0 * Y424 - SY * X10                                              
      Y413 = 0.5 * (CX * SY + SX * CY + KH2 * Y424)                             
      Y414 = 0.5 * (SX * SY - KH2 * Y324)                                       
      T111 = 0.5 * (A1*X111 + KH2**2*H*X122)                                    
      T211 = 0.5 * (A1*X211 + KH2**2*H*X222) - H*RH(1)*RH(3)                    
      T112 = (A1 - KH2*H) * X112 + H*RH(2)                                      
      T212 = (A1 - KH2*H) * X212 + H * (RH(4)-RH(1)*RH(4)-RH(3)*RH(2))          
      T122 = 0.5 * (A1*X122 + H*X111)                                           
      T222 = 0.5 * (A1*X222 + H*X211) - H*RH(2)*RH(4)                           
      B1 = H ** 3 * (BDB + BDB - N)                                             
      T133 = 0.5 * (B1*X133 - KV2**2*H*X144)                                    
      T233 = 0.5 * (B1*X233 - KV2**2*H*X244)                                    
      T134 = (B1 + KV2*H) * X134                                                
      T234 = (B1 + KV2*H) * X234                                                
      T144 = 0.5 * (B1*X144 - H*X133)                                           
      T244 = 0.5 * (B1*X244 - H*X233)                                           
      T313 = C1*Y313 + KH2*KV2*H*Y324                                           
      T413 = C1*Y413 + KH2*KV2*H*Y424 - H*RH(1)*RV(3)                           
      T314 = C1*Y314 - KH2*H*Y323 + H*RV(2)                                     
      T414 = C1*Y414 - KH2*H*Y423 + H * (RV(4) - RH(1)*RV(4))                   
      T323 = C1*Y323 - KV2*H*Y314                                               
      T423 = C1*Y423 - KV2*H*Y414 - H*RH(2)*RV(3)                               
      T324 = C1*Y324 + H*Y313                                                   
      T424 = C1*Y424 + H*Y413 - H*RH(2)*RV(4)                                   
      RAY2(1) = RAY2(1) +                                                       
     A     T111*RAY(1)**2 + T112*RAY(1)*RAY(2) + T122*RAY(2)**2 +               
     B     T133*RAY(3)**2 + T134*RAY(3)*RAY(4) + T144*RAY(4)**2                 
      RAY2(2) = RAY2(2) +                                                       
     A     T211*RAY(1)**2 + T212*RAY(1)*RAY(2) + T222*RAY(2)**2 +               
     B     T233*RAY(3)**2 + T234*RAY(3)*RAY(4) + T244*RAY(4)**2                 
      RAY2(3) = RAY2(3) + (T313*RAY(3) + T314*RAY(4)) * RAY(1) +                
     A                    (T323*RAY(3) + T324*RAY(4)) * RAY(2)                  
      RAY2(4) = RAY2(4) + (T413*RAY(3) + T414*RAY(4)) * RAY(1) +                
     A                    (T423*RAY(3) + T424*RAY(4)) * RAY(2)                  
  200 CALL RESET (RAY, RAY2)                                                    
      RETURN                                                                    
  300 ALFA = LL * H                                                             
      RHO0 = DATA(I+4)                                                          
      RHO = RHO0 * P / P0                                                       
      THETA1 = ATAN(RAY(2))                                                     
      COST1 = COS(THETA1)                                                       
      SINT2 = SIN(THETA1+ALFA) - SIN(ALFA) * (RAY(1) + RHO0) / RHO              
      IF(ABS(SINT2) .GE. 1.0) GO TO 310                                         
      COST2 = SQRT(1.0 - SINT2 ** 2)                                            
      THETA2 = ATAN2(SINT2,COST2)                                               
      PHI = ALFA + THETA1 - THETA2                                              
      PSI = ALFA + THETA1 + THETA2                                              
      RAY(1) = RAY(1) + 2.0 * (RHO * SIN(0.5 * PHI) * SIN(0.5 * PSI) -          
     A                         (RHO0 + RAY(1)) * SIN(0.5 * ALFA) ** 2)          
      RAY(2) = SINT2 / COST2                                                    
      RAY(3) = RAY(3) + RHO * RAY(4) * COST1 * PHI                              
      RAY(4) = RAY(4) * COST1 / COST2                                           
      RETURN                                                                    
  310 KSTOP = 1                                                                 
      RETURN                                                                    
      END                                                                       
      LOGICAL FUNCTION BSTOP(LFLAG)                                             
C                                                                               
C FCT BSTOP        TESTS FOR BENDING MAGNET APERTURES                           
C                                                                               
      COMMON /BLOC5/ SEC, CAP(3), MPA                                           
      LOGICAL SEC, CAP, MPA                                                     
      COMMON /BLOC10/ APB(2), LAYL, LAYX, RAB1, RAB2, FACE                      
      REAL LAYL, LAYX                                                           
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
C                                                                               
      IF(.NOT. CAP(LFLAG+1)) GO TO 20                                           
      IF(APB(1) .EQ. 0.0) GO TO 10                                              
      IF(ABS(RAY(1)) .GT. APB(1)) GO TO 30                                      
   10 IF(APB(2) .EQ. 0.0) GO TO 20                                              
      IF(ABS(RAY(3)) .GT. APB(2)) GO TO 30                                      
   20 BSTOP = .FALSE.                                                           
      RETURN                                                                    
   30 BSTOP = .TRUE.                                                            
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DECAY                                                          
C                                                                               
C S/R DECAY        GENERATES THE DECAY   PI (OR K) --> MU + NU                  
C                                                                               
      COMMON /BLOC4/ RAYMAX(6), PBEAM, P0, NORAYS                               
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG
      REAL*8  RANF                                                            
C                                                                               
      COST = 2.0 * RANF(- 1) - 1.0                                              
      SINT = SQRT (1.0 - COST ** 2)                                             
      FPC = P * (COST + FC) / MASS                                              
      THETAC = SINT / FPC                                                       
      PC = PSTAR * FPC                                                          
      FPN = P * (COST + FN) / MASS                                              
      THETAN = SINT / FPN                                                       
      PN = PSTAR * FPN                                                          
      PHI = 6.2831853 * RANF(- 1)                                               
      COSPHI = COS(PHI)                                                         
      SINPHI = SIN(PHI)                                                         
      RAYC(1) = RAY(1)                                                          
      RAYC(2) = RAY(2) + COSPHI * THETAC                                        
      RAYC(3) = RAY(3)                                                          
      RAYC(4) = RAY(4) + SINPHI * THETAC                                        
      RAYC(5) = RAY(5)                                                          
      RAYC(6) = (PC - P0) / P0                                                  
      RAYN(1) = RAY(1)                                                          
      RAYN(2) = RAY(2) - COSPHI * THETAN                                        
      RAYN(3) = RAY(3)                                                          
      RAYN(4) = RAY(4) - SINPHI * THETAN                                        
      RAYN(5) = RAY(5)                                                          
      RAYN(6) = (PN - P0) / P0                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DECAYP                                                         
C                                                                               
C S/R DECAYP       PREPARES DATA FOR DECAY                                      
C
      REAL*8 UNIT, NAME
      COMMON /BLOC2/ UNIT(11), NAME(11)
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      COMMON /BLOC41/ LW, IMAGE(20), FLUSH, INDIC, NTYPE, LABLE, LENGTH,        
     A                NWORD, NVARY, DATUM(30), VARY(30)                         
      INTEGER VARY, TEXT(30)                                                    
      LOGICAL FLUSH                                                             
      EQUIVALENCE (TEXT(1), DATUM(1))                                           
C                                                                               
      MASS = DATUM(2) * UNIT(11)                                                
      MASSC = DATUM(3) * UNIT(11)                                               
      MASSN = DATUM(4) * UNIT(11)                                               
      FLD = 0.299 * DATUM(5) / MASS                                             
      PSTAR = (MASS ** 2 - MASSC ** 2) / (2.0 * MASS)                           
      FC = (MASS ** 2 + MASSC ** 2) / (MASS ** 2 - MASSC ** 2)                  
      FN = 1.0                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DECAYZ (ZD)                                                    
C                                                                               
C S/R DECAYZ       GENERATES A DECAY ABSCISSA                                   
C                                                                               
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      REAL LD                                                                   
      REAL*8  RANF
C                                                                               
      LD = FLD * P                                                              
      X = RANF (-1)                                                             
      IF(ZMAX .NE. 0.0) X = 1.0 - X * (1.0 - EXP (-ZMAX / LD))                  
      ZD = - LD * ALOG (X)                                                      
      RETURN                                                                    
      END                                                                       
      INTEGER FUNCTION DIGIT (INT, IR)                                          
C                                                                               
C FCT DIGIT        RETURNS ONE DIGIT OF AN INTEGER AS A BCD WORD                
C                                                                               
      COMMON /BLOC42/ TABLE(36), PLUS(2), MINUS, BLANK, PERIOD,                 
     A                SEMI(5), QUOTE(6), PAR1(2), PAR2(2)                       
      INTEGER TABLE, PLUS, BLANK, PERIOD, SEMI, QUOTE, PAR1, PAR2               
C                                                                               
      IF (INT .GT. 9999) INT = 9999                                             
      MODEL = 10**(4-IR)                                                        
      MITE = INT/MODEL                                                          
      ITEN = MOD(MITE,10)                                                       
      IF (ITEN .GT. 0) GO TO 50                                                 
      DIGIT = BLANK                                                             
      IF(IR .EQ. 4 .OR. MITE .GE. 10) DIGIT = TABLE(1)                          
      RETURN                                                                    
   50 DIGIT = TABLE(ITEN+1)                                                     
      RETURN                                                                    
      END                                                                       
      SUBROUTINE EHIST                                                          
C                                                                               
C S/R EHIST        ENTERS A RAY INTO THE REQUESTED HISTOGRAMS                   
C                                                                               
      COMMON /BLOC6/ NHISTS, NHIST, LIMNO                                       
      COMMON /BLOC7/ MTYPE(100), NENTRY(100), NCOOR(100), INTVLS(100),          
     A               BEGIN(100), END(100), STEP(100), ZHIST(100)                
      COMMON /BLOC8/ MTABLE(1000000)                                              
      COMMON /BLOC12/ VALUE(100), XMOM(100), XMOM2(100), NFLAG(100),            
     A                SET(100), KFLAG(100), IFLAG(3,9), ZFLAG(9)                
      LOGICAL SET, IFLAG                                                        
      LOGICAL SCALX                                                             
C                                                                               
      SCALX = .FALSE.                                                           
      DO 400 NH = 1, NHISTS                                                     
      NFL = NFLAG(NH)                                                           
      KFL = KFLAG(NH) + 1                                                       
      NAVEL = MTYPE(NH)                                                         
      GO TO (100,200,300), NAVEL                                                
C                                                                               
C     ONE DIMENSIONAL HISTOGRAM                                                 
C                                                                               
  100 IF(NFL .GT. 0 .AND. .NOT. IFLAG(KFL,NFL)) GO TO 400                       
      IF(.NOT. SET(NH)) GO TO 400                                               
      VAL = VALUE(NH)                                                           
      M = NENTRY(NH)                                                            
      N = INTVLS(NH)                                                            
      XBEG = BEGIN(NH)                                                          
      XEND = END(NH)                                                            
      DX = STEP(NH)                                                             
      CALL HISTO(VAL,MTABLE(M),N,XBEG,XEND,DX)                                  
      XMOM(NH) = XMOM(NH) + VAL                                                 
      XMOM2(NH) = XMOM2(NH) + VAL ** 2                                          
      GO TO 400                                                                 
C                                                                               
C     X COORDINATE OF TWO DIMENSIONAL HISTOGRAM                                 
C                                                                               
  200 SCALX = .FALSE.                                                           
      IF(NFL .GT. 0 .AND. .NOT. IFLAG(KFL,NFL)) GO TO 400                       
      IF(.NOT. SET(NH)) GO TO 400                                               
      SCALX = .TRUE.                                                            
      A = VALUE(NH)                                                             
      NA = INTVLS(NH)                                                           
      AMIN = BEGIN(NH)                                                          
      AMAX = END(NH)                                                            
      DA = STEP(NH)                                                             
      GO TO 400                                                                 
C                                                                               
C     Y COORDINATE OF TWO DIMENSIONAL HISTOGRAM                                 
C                                                                               
  300 IF(NFL .GT. 0 .AND. .NOT. IFLAG(KFL,NFL)) GO TO 400                       
      IF(.NOT. (SET(NH) .AND. SCALX)) GO TO 400                                 
      D = VALUE(NH)                                                             
      M = NENTRY(NH)                                                            
      ND = INTVLS(NH)                                                           
      DMIN = BEGIN(NH)                                                          
      DMAX = END(NH)                                                            
      DD = STEP(NH)                                                             
      CALL ENTMAT(A,D,MTABLE(M),AMIN,AMAX,NA,DA,DMIN,DMAX,ND,DD)                
  400 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE EMIT (RAY, P, P0, RAYMAX)                                      
C                                                                               
C S/R EMIT         GENERATES A PARENT PARTICLE (PI OR K)                        
C                                                                               
C PARAMETERS...    RAY(1)    X  IN METERS                                       
C                  RAY(2)    X' IN RADIANS                                      
C                  RAY(3)    Y  IN METERS                                       
C                  RAY(4)    Y' IN RADIANS                                      
C                  RAY(5)    NOT USED                                           
C                  RAY(6)    PARTICLE MOMENTUM IN GEV/C                         
C                  P         PARTICLE MOMENTUM IN GEV/C                         
C                  P0        REFERENCE MOMENTUM OF BEAM LINE IN GEV/C           
C                  RAYMAX(1) EXTENT OF X                                        
C                  RAYMAX(2) EXTENT OF X'                                       
C                  RAYMAX(3) EXTENT OF Y                                        
C                  RAYMAX(4) EXTENT OF Y'                                       
C                  RAYMAX(5) NOT USED                                           
C                  RAYMAX(6) EXTENT OF P                                        
C                                                                               
      REAL RAY(6), RAYMAX(6), VAL(4)                                            
      REAL*8  RANF
C     
   10 DO 20 IR = 1, 4                                                           
      VAL(IR) = 2.0 * RANF(IR) - 1.0                                            
   20 CONTINUE                                                                  
      RN1 = VAL(1) ** 2 + VAL(3) ** 2                                           
      RN2 = VAL(2) ** 2 + VAL(4) ** 2                                           
      IF(RN1 .GT. 1.0 .OR. RN2 .GT. 1.0) GO TO 10                               
      DO 30 IR = 1, 4                                                           
      RAY(IR) = VAL(IR) * RAYMAX(IR)                                            
   30 CONTINUE                                                                  
      RAY(5) = 0.0                                                              
      RAY(6) = RAYMAX(6) * (2.0 * RANF(- 1) - 1.0)                              
      P = P0 * (1.0 + RAY(6))                                                   
      RETURN                                                                    
      END                                                                       
      SUBROUTINE ENTMAT (A,D,MATRIX,AMIN,AMAX,NA,DA,DMIN,DMAX,ND,DD)            
C                                                                               
C S/R ENTMAT       ENTERS A PAIR OF VALUES INTO A 2-D HISTOGRAM                 
C                                                                               
      DIMENSION MATRIX(2)                                                       
C                                                                               
      NAA = NA + 1                                                              
      NTT = NAA*(ND + 1)                                                        
      MTT = NTT + 5                                                             
C                                                                               
C     OVERFLOW OF X COORDINATE                                                  
C                                                                               
      IF (A - AMIN) 5, 10, 10                                                   
    5 NRET = 1                                                                  
      GO TO 35                                                                  
   10 IF (A - AMAX) 20, 20, 15                                                  
   15 NRET = 2                                                                  
      GO TO 35                                                                  
C                                                                               
C     OVERFLOW OF Y COORDINATE                                                  
C                                                                               
   20 IF (D - DMIN) 25, 30, 30                                                  
   25 NRET = 3                                                                  
      GO TO 35                                                                  
   30 IF (D - DMAX) 40, 40, 32                                                  
   32 NRET = 4                                                                  
C                                                                               
   35 NOV = NTT + NRET                                                          
   36 MATRIX(NOV) = MATRIX(NOV) + 1                                             
      MATRIX(MTT) = MATRIX(MTT) + 1                                             
      RETURN                                                                    
C                                                                               
C     ENTRY INTO HISTOGRAM                                                      
C                                                                               
 40   CONTINUE                                                                  
      IA = (A - AMIN)/DA + 1.0                                                  
      ID = (D - DMIN)/DD + 1.0                                                  
      MM = (ID - 1) * NAA + IA                                                  
      NAT = ID*NAA                                                              
      NDT = ND*NAA + IA                                                         
 60   MATRIX(MM) = MATRIX(MM) + 1                                               
      MATRIX(NTT) = MATRIX(NTT) + 1                                             
      MATRIX(NAT) = MATRIX(NAT) + 1                                             
      MATRIX(NDT) = MATRIX(NDT) + 1                                             
      MATRIX(MTT) = MATRIX(MTT) + 1                                             
C                                                                               
 200  RETURN                                                                    
      END                                                                       
      SUBROUTINE FOCUS (K2, L, K, KL, R)                                        
C                                                                               
C S/R FOCUS        FINDS THE FOCUSSING MATRIX OF A QUADRUPOLE                   
C                                                                               
      REAL K2, L, K, KL, R(4)                                                   
      K = SQRT(ABS(K2))                                                         
      KL = K * L                                                                
      IF(K2) 20, 10, 10                                                         
   10 R(1) = COS(KL)                                                            
      R(3) = - K * SIN(KL)                                                      
      R(4) = R(1)                                                               
      IF(ABS(KL) .LT. 0.01) GO TO 30                                            
      R(2) = - R(3) / K2                                                        
      RETURN                                                                    
   20 EXPKL = EXP(KL)                                                           
      EXIKL = 1.0 / EXPKL                                                       
      R(1) = 0.5 * (EXPKL + EXIKL)                                              
      R(3) = 0.5 * (EXPKL - EXIKL) * K                                          
      R(4) = R(1)                                                               
      IF(ABS(KL) .LT. 0.01) GO TO 30                                            
      R(2) = - R(3) / K2                                                        
      RETURN                                                                    
   30 R(2) = L * (1.0 - K2 * L ** 2 / 6.0)                                      
      RETURN                                                                    
      END                                                                       
      SUBROUTINE HEDING                                                         
C                                                                               
C S/R HEDING       PRINTS THE TABLE OF HISTOGRAM REQUESTS                       
C
      REAL*8 UNIT, NAME
      COMMON /BLOC2/ UNIT(11), NAME(11)                                         
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      COMMON /BLOC4/ RAYMAX(6), PBEAM, P0, NORAYS                               
      COMMON /BLOC6/ NHISTS, NHIST, LIMNO                                       
      COMMON /BLOC7/ MTYPE(100), NENTRY(100), NCOOR(100), INTVLS(100),          
     A               BEGIN(100), END(100), STEP(100), ZHIST(100)                
      COMMON /BLOC12/ VALUE(100), XMOM(100), XMOM2(100), NFLAG(100),            
     A                SET(100), KFLAG(100), IFLAG(3,9), ZFLAG(9)                
      LOGICAL SET, IFLAG                                                        
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      COMMON /BLOC16/ KPART(3), ISTOP(3,500)                                    
      COMMON /BLOC18/ LOSS, NBLK(10), KFLG(10),                                 
     A                XSTRT(10), XEND(10), LTHX(10), XBIN(10), NUNX(10),        
     B                YSTRT(10), YEND(10), LTHY(10), YBIN(10), NUNY(10)         
      COMMON /BLOC41/ LW, IMAGE(20), FLUSH, INDIC, NTYPE, LABLE, LENGTH,        
     A                NWORD, NVARY, DATUM(30), VARY(30)                         
      INTEGER VARY, TEXT(30)                                                    
      LOGICAL FLUSH                                                             
      EQUIVALENCE (TEXT(1), DATUM(1))                                           
C                                                                               
      WRITE (NOUT, 9006) IMAGE                                                  
 9006 FORMAT(1H1,20A4)                                                          
      WRITE (NOUT,9007) NORAYS                                                  
 9007 FORMAT(35H0THE FOLLOWING HISTOGRAMS REFER TO ,I6,                         
     A       41H RAYS PASSING THROUGH THE ABOVE BEAM LINE)                      
 9008 FORMAT(1X,81(1H*))                                                        
      WRITE (NOUT,9008)                                                         
      KH = 0                                                                    
      DO 400 K = 1, NHISTS                                                      
      NAVEL = MTYPE(K)                                                          
      GO TO (100,200,300), NAVEL                                                
C                                                                               
C     ONE DIMENSIONAL HISTOGRAM                                                 
C                                                                               
  100 KH = KH + 1                                                               
      N = NCOOR(K)                                                              
      Z = ZHIST(K)/UNIT(8)                                                      
      KFL = KFLAG(K) + 1                                                        
      NFL = NFLAG(K)                                                            
      WRITE (NOUT, 9400) KH                                                     
 9400 FORMAT(13H0HISTOGRAM NO,I3)                                               
      CALL HSTUN (N, NMVAL, NMUN, FACT)                                         
      CALL HISCAP (1, NMVAL, NMUN, KFL, Z, NFL)                                 
      GO TO 400                                                                 
C                                                                               
C     X COORDINATE FOR TWO DIMENSIONAL HISTOGRAM                                
C                                                                               
  200 NA = NCOOR(K)                                                             
      ZA = ZHIST(K)/UNIT(8)                                                     
      KFLA = KFLAG(K) + 1                                                       
      NFLA = NFLAG(K)                                                           
      GO TO 400                                                                 
C                                                                               
C     Y COORDINATE FOR TWO DIMENSIONAL HISTOGRAM                                
C                                                                               
  300 KH = KH + 1                                                               
      ND = NCOOR(K)                                                             
      ZD = ZHIST(K) /UNIT(8)                                                    
      KFLD = KFLAG(K) + 1                                                       
      NFLD = NFLAG(K)                                                           
      WRITE (NOUT, 9400) KH                                                     
      CALL HSTUN (NA, NMVAL, NMUN, FACT)                                        
      CALL HISCAP (2, NMVAL, NMUN, KFLA, ZA, NFLA)                              
      CALL HSTUN (ND, NMVAL, NMUN, FACT)                                        
      CALL HISCAP (3, NMVAL, NMUN, KFLD, ZD, NFLD)                              
  400 CONTINUE                                                                  
C                                                                               
C     HISTOGRAMS OF PARTICLES STOPPED                                           
C                                                                               
      IF(LOSS .EQ. 0) GO TO 710                                                 
      DO 700 K = 1, LOSS                                                        
      KH = KH + 1                                                               
      KFL = KFLG(K) + 1                                                         
      WRITE (NOUT, 9500) KH, KPART(KFL)                                         
 9500 FORMAT(13H0HISTOGRAM NO,I3,4H OF ,A4,5H LOST)                             
      N = NUNX(K)                                                               
      NA = NUNY(K)                                                              
      IF(NA .NE. 0) GO TO 600                                                   
      CALL HSTUN (N, NMVAL, NMUN, FACT)                                         
      CALL HISCAP (4, NMVAL, NMUN, 0, 0.0, 0)                                   
      GO TO 700                                                                 
  600 CALL HSTUN (N, NMVAL, NMUN, FACT)                                         
      CALL HISCAP (5, NMVAL, NMUN, 0, 0.0, 0)                                   
      CALL HSTUN (NA, NMVAL, NMUN, FACT)                                        
      CALL HISCAP (6, NMVAL, NMUN, 0, 0.0, 0)                                   
  700 CONTINUE                                                                  
  710 IF(.NOT. DKFLAG .OR. ZMAX .EQ. 0.0) RETURN                                
      ZMAXX = ZMAX / UNIT(8)                                                    
      FAC = 1.0 / (1.0 - EXP(-ZMAX / (FLD*PBEAM)))                              
      WRITE (NOUT, 9700) ZMAXX, NAME(8), FAC                                    
 9700 FORMAT(1H0,84(1H*)/43H * ALL PARENT PARTICLES ARE FORCED TO DECAY,        
     A       8H WITHIN ,F10.3,1X,A4,19H FROM THE TARGET. */                     
     B       56H * THE NUMBER OF DECAYS WILL BE ENHANCED BY A FACTOR OF         
     C       ,F8.3,21H (AVERAGE VALUE)    */1X,84(1H*))                         
      RETURN                                                                    
      END                                                                       
      SUBROUTINE HISCAP (IOPT, N1, N2, KFL, ZH, NFL)                            
C                                                                               
C S/R HISCAP       PRINTS HISTOGRAM CAPTIONS                                    
C
      REAL*8 UNIT, NAME
      COMMON /BLOC2/ UNIT(11), NAME(11)                                         
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      COMMON /BLOC12/ VALUE(100), XMOM(100), XMOM2(100), NFLAG(100),            
     A                SET(100), KFLAG(100), IFLAG(3,9), ZFLAG(9)                
      LOGICAL SET, IFLAG                                                        
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      COMMON /BLOC16/ KPART(3), ISTOP(3,500)                                    
      INTEGER LABEL(4,3), FOR, PART(2), BLANK                                   
      DATA LABEL(1,1), LABEL(2,1), LABEL(3,1), LABEL(4,1)                       
     A       / 4HDIST, 4HRIBU, 4HTION, 4H OF  /                                 
      DATA LABEL(1,2), LABEL(2,2), LABEL(3,2), LABEL(4,2)                       
     A       / 4HHORI, 4HZONT, 4HAL A, 4HXIS  /                                 
      DATA LABEL(1,3), LABEL(2,3), LABEL(3,3), LABEL(4,3)                       
     A       / 4HVERT, 4HICAL, 4H AXI, 4HS    /                                 
      DATA FOR, BLANK / 4HFOR , 4H     /                                        
C                                                                               
      PART(1) = BLANK                                                           
      PART(2) = BLANK                                                           
      IF(.NOT. DKFLAG) GO TO 30                                                 
      PART(1) = FOR                                                             
      PART(2) = KPART(KFL)                                                      
   30 IF(IOPT .GT. 3) GO TO 50                                                  
      J = IOPT                                                                  
      IF(NFL .NE. 0) GO TO 40                                                   
      WRITE (NOUT,110) (LABEL(I,J), I=1,4), N1, N2, PART, ZH, NAME(8)           
      RETURN                                                                    
   40 ZFL = ZFLAG(NFL) / UNIT(8)                                                
      WRITE (NOUT,120) (LABEL(I,J), I=1,4), N1, N2, PART, ZH, NAME(8),          
     A                  ZFL, NAME(8)                                            
      RETURN                                                                    
   50 J = IOPT - 3                                                              
      WRITE (NOUT, 110) (LABEL(I,J), I=1,4), N1, N2                             
      RETURN                                                                    
C                                                                               
  110 FORMAT(1X,5A4,4H IN ,A4,5X,2A4,5X,F12.3,1X,A4,                            
     A       16H FROM THE TARGET)                                               
  120 FORMAT(1X,5A4,4H IN ,A4,5X,2A4,5X,F12.3,1X,A4,                            
     A       26H FROM THE TARGET, FLAG AT ,F12.3,1X,A4)                         
      END                                                                       
      SUBROUTINE HISTO (X,NHST,M,A,B,DX)                                        
C                                                                               
C S/R HISTO        ENTERS A VALUE INTO A 1-D HISTOGRAM                          
C                                                                               
      DIMENSION NHST(2)                                                         
C                                                                               
      NHST(M+3) = NHST(M+3) + 1                                                 
C                                                                               
C     OVERFLOW OF HISTOGRAM                                                     
C                                                                               
      IF (X - B) 104, 102, 102                                                  
  102 NHST(M+1) = NHST(M+1) + 1                                                 
      RETURN                                                                    
  104 IF (X - A) 106, 108, 108                                                  
  106 NHST(M+2) = NHST(M+2) + 1                                                 
      RETURN                                                                    
C                                                                               
C     FILL IN HISTOGRAM                                                         
C                                                                               
  108 L = (X - A)/DX + 1.00001                                                  
      NHST(L) = NHST(L) + 1                                                     
      RETURN                                                                    
      END                                                                       
      SUBROUTINE HSTUN (NCO, NVAL, NUN, FACT)                                   
C                                                                               
C S/R HSTUN        RETURNS VALUE NAME, UNIT NAME, AND CONVERSION FACTOR         
C                  FOR A VALUE TO BE HISTOGRAMMED                               
C
      REAL*8 UNIT, NAME
      COMMON /BLOC2/ UNIT(11), NAME(11)                                         
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      INTEGER NOM(11), NOM1(4)                                                  
      DATA NOM  / 4H   X, 4H  X', 4H   Y, 4H  Y', 4H????, 4HDP/P,               
     A            4H????, 4H   Z, 4H????, 4H????, 4H   P /                      
      DATA NOM1 / 4H   R, 4H  XI, 4H PSI, 4HZETA /                              
      DATA IZERO / 4HZERO /                                                     
C                                                                               
      IF(NCO .GT. 0 .AND. NCO .LE. 6) GO TO 20                                  
      IF(NCO .EQ. 8) GO TO 30                                                   
      IF(NCO .EQ. 11) GO TO 30                                                  
      IF(NCO .GT. 20 .AND. NCO .LE. 24) GO TO 40                                
   10 CONTINUE                                                                  
      FACT = 1.0                                                                
      NVAL = IZERO                                                              
      NUN  = NOM(5)                                                             
      RETURN                                                                    
   20 IF(NCO .EQ. 5) GO TO 10                                                   
   30 FACT = UNIT(NCO)                                                          
      NVAL = NOM(NCO)                                                           
      NUN = NAME(NCO)                                                           
      RETURN                                                                    
   40 J = NCO - 20                                                              
      NVAL = NOM1(J)                                                            
      IF(J .GT. 2) GO TO 50                                                     
      FACT = UNIT(J)                                                            
      NUN = NAME(J)                                                             
      RETURN                                                                    
   50 FACT = UNIT(7)                                                            
      NUN = NAME(7)                                                             
      RETURN                                                                    
      END                                                                       
      REAL FUNCTION HSTVAL (NCO)                                                
C                                                                               
C FCT HSTVAL       RETURNS A VALUE TO BE HISTOGRAMMED                           
C                                                                               
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
C                                                                               
      IF(NCO .LE. 6) GO TO 10                                                   
      IF(NCO .EQ. 8) GO TO 20                                                   
      IF(NCO .EQ. 11) GO TO 30                                                  
      IF(NCO .GT. 20 .AND. NCO .LE. 24) GO TO 40                                
    5 CONTINUE                                                                  
      HSTVAL = 0.0                                                              
      RETURN                                                                    
   10 IF(NCO .EQ. 5) GO TO 5                                                    
      HSTVAL = RAY(NCO)                                                         
      RETURN                                                                    
   20 HSTVAL = Z                                                                
      RETURN                                                                    
   30 HSTVAL = P                                                                
      RETURN                                                                    
   40 IF(NCO .GT. 22) GO TO 50                                                  
      J = NCO - 20                                                              
      HSTVAL = SQRT (RAY(J)**2 + RAY(J+2)**2)                                   
      RETURN                                                                    
   50 J = NCO - 22                                                              
      HSTVAL = 0.0                                                              
      IF(RAY(J+2) .NE. 0.0 .OR. RAY(J) .NE. 0.0)                                
     A     HSTVAL = ATAN2 (RAY(J+2), RAY(J))                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE INITZE                                                         
C                                                                               
C S/R INITZE       SETS UP DEFAULT VALUES FOR A PARTICLE BEFORE ENTERING        
C                  THE BEAM LINE                                                
C                                                                               
      COMMON /BLOC5/ SEC, CAP(3), MPA                                           
      LOGICAL SEC, CAP, MPA                                                     
      COMMON /BLOC9/ L, H, N, BDB                                               
      REAL L, N                                                                 
      COMMON /BLOC10/ APB(2), LAYL, LAYX, RAB1, RAB2, FACE                      
      REAL LAYL, LAYX                                                           
      COMMON /BLOC12/ VALUE(100), XMOM(100), XMOM2(100), NFLAG(100),            
     A                SET(100), KFLAG(100), IFLAG(3,9), ZFLAG(9)                
      LOGICAL SET, IFLAG                                                        
C                                                                               
      DO 10 J = 1, 9                                                            
   10 ZFLAG(J) = - 1.0                                                          
      SEC = .FALSE.                                                             
      CAP(1) = .FALSE.                                                          
      CAP(2) = .FALSE.                                                          
      CAP(3) = .FALSE.                                                          
      MPA = .FALSE.                                                             
      BDB = 0.0                                                                 
      APB(1) = 0.0                                                              
      APB(2) = 0.0                                                              
      LAYL = 0.5                                                                
      LAYX = 0.0                                                                
      RAB1 = 0.0                                                                
      RAB2 = 0.0                                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE MULTI                                                          
C                                                                               
C S/R MULTI        APPLIES THE MULTIPOLE ERRORS AT ENTRY AND EXIT OF A          
C                  QUADRUPOLE                                                   
C                                                                               
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      COMMON /BLOC14/ MMAX, GTOP, DB(20), COSM(20), SINM(20)                    
C                                                                               
      DBX = 0.0                                                                 
      DBY = 0.0                                                                 
      DBC = 0.0                                                                 
      DBS = 1.0                                                                 
      X = RAY(1)                                                                
      Y = RAY(3)                                                                
      DO 10 NM = 1, MMAX                                                        
      IF(DB(NM) .EQ. 0.0) GO TO 5                                               
      DBX = DBX + DB(NM) * (DBC * COSM(NM) - DBS * SINM(NM))                    
      DBY = DBY + DB(NM) * (DBC * SINM(NM) + DBS * COSM(NM))                    
    5 DBT = DBC * X + DBS * Y                                                   
      DBS = DBS * X - DBC * Y                                                   
   10 DBC = DBT                                                                 
      RAY(2) = RAY(2) - GTOP * DBY                                              
      RAY(4) = RAY(4) + GTOP * DBX                                              
      RETURN                                                                    
      END                                                                       
      INTEGER FUNCTION NU(TYPE)                                                 
C                                                                               
C FCT NU           RETURNS THE STORAGE BLOCK SIZE NEEDED FOR AN ELEMENT         
C                                                                               
      INTEGER TYPE                                                              
      COMMON /BLOC1/ DATA(3001), I, I1                                          
      INTEGER IDATA(3001)                                                       
      EQUIVALENCE (IDATA(1), DATA(1))                                           
      COMMON /BLOC5/ SEC, CAP(3), MPA                                           
      LOGICAL SEC, CAP, MPA                                                     
      INTEGER NO1(21), NO2(4), TYP1                                             
C                                                                               
      DATA NO1 / 9,9,2,6,4,7,7,0,2,0,0,0,0,8,0,5,0,3,3,3,0 /                    
      DATA NO2 / 1,1,1,2 /                                                      
C                                                                               
      IF (SEC .AND. TYPE .EQ. 4) GO TO 200                                      
      IF(TYPE .EQ. 14 .AND. IDATA(I+8) .EQ. 0) GO TO 300                        
      IF(TYPE .EQ. 60) GO TO 400                                                
      IF (TYPE .GE. 50) GO TO 100                                               
      IABST = IABS(TYPE)                                                        
      NU = NO1(IABST)                                                           
      RETURN                                                                    
  100 TYP1 = TYPE - 49                                                          
      NU = NO2(TYP1)                                                            
      RETURN                                                                    
  200 NU = 41                                                                   
      RETURN                                                                    
  300 NU = 30                                                                   
      RETURN                                                                    
  400 NU = 0                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE OHIST                                                          
C                                                                               
C S/R OHIST        PRINTS THE HISTOGRAMS                                        
C
      REAL*8 UNIT, NAME
      COMMON /BLOC2/ UNIT(11), NAME(11)                                         
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      COMMON /BLOC6/ NHISTS, NHIST, LIMNO                                       
      COMMON /BLOC7/ MTYPE(100), NENTRY(100), NCOOR(100), INTVLS(100),          
     A               BEGIN(100), END(100), STEP(100), ZHIST(100)                
      COMMON /BLOC8/ MTABLE(1000000)                                              
      COMMON /BLOC12/ VALUE(100), XMOM(100), XMOM2(100), NFLAG(100),            
     A                SET(100), KFLAG(100), IFLAG(3,9), ZFLAG(9)                
      LOGICAL SET, IFLAG                                                        
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      COMMON /BLOC16/ KPART(3), ISTOP(3,500)                                    
      COMMON /BLOC18/ LOSS, NBLK(10), KFLG(10),                                 
     A                XSTRT(10), XEND(10), LTHX(10), XBIN(10), NUNX(10),        
     B                YSTRT(10), YEND(10), LTHY(10), YBIN(10), NUNY(10)         
C                                                                               
      NH = 0                                                                    
      DO 500 K = 1, NHISTS                                                      
      NAVEL = MTYPE(K)                                                          
      GO TO (100,200,300), NAVEL                                                
C                                                                               
C     ONE DIMENSIONAL HISTOGRAM                                                 
C                                                                               
  100 NH = NH + 1                                                               
      WRITE (NOUT, 9500) NH                                                     
 9500 FORMAT(13H1HISTOGRAM NO,I3)                                               
      M = NENTRY(K)                                                             
      NX = INTVLS(K)                                                            
      NUN = NCOOR(K)                                                            
      CALL HSTUN (NUN, NMVAL, NMUN, FACT)                                       
      XMIN = BEGIN(K) / FACT                                                    
      DX   = STEP (K) / FACT                                                    
      ZX   = ZHIST(K) / UNIT(8)                                                 
      KFLX = KFLAG(K) + 1                                                       
      NFLX = NFLAG(K)                                                           
      CALL HISCAP (1, NMVAL, NMUN, KFLX, ZX, NFLX)                              
      CALL TRHIST (MTABLE(M), NX, XMIN, DX)                                     
      MN = M + NX                                                               
      SUM = MTABLE(MN+2)                                                        
      IF(SUM .EQ. 0.0) GO TO 110                                                
      XM = XMOM(K) / (SUM * FACT)                                               
      XM2 = SQRT (XMOM2(K) / (SUM * FACT ** 2) - XM ** 2)                       
      WRITE (NOUT, 9100) XM, XM2                                                
 9100 FORMAT(1H0,10X,6HMEAN =,F8.3,3X,16HRMS HALF WIDTH =,F8.3)                 
  110 WRITE (NOUT, 9510) NH                                                     
 9510 FORMAT(13H0HISTOGRAM NO,I3)                                               
      CALL HISCAP (1, NMVAL, NMUN, KFLX, ZX, NFLX)                              
      GO TO 500                                                                 
C                                                                               
C     X COORDINATE OF TWO DIMENSIONAL HISTOGRAM                                 
C                                                                               
  200 NA = INTVLS(K)                                                            
      NUNA = NCOOR(K)                                                           
      CALL HSTUN (NUNA, NMVALA, NMUNA, FACT)                                    
      AMIN = BEGIN(K) / FACT                                                    
      DA   = STEP (K) / FACT                                                    
      ZA   = ZHIST(K) / UNIT(8)                                                 
      KFLA = KFLAG(K) + 1                                                       
      NFLA = NFLAG(K)                                                           
      GO TO 500                                                                 
C                                                                               
C     Y COORDINATE OF TWO DIMENSIONAL HISTOGRAM                                 
C                                                                               
  300 NH = NH + 1                                                               
      WRITE (NOUT, 9500) NH                                                     
      M = NENTRY(K)                                                             
      ND = INTVLS(K)                                                            
      NUND = NCOOR(K)                                                           
      CALL HSTUN (NUND, NMVALD, NMUND, FACT)                                    
      DMIN = BEGIN(K) / FACT                                                    
      DD   = STEP (K) / FACT                                                    
      ZD   = ZHIST(K) / UNIT(8)                                                 
      KFLD = KFLAG(K) + 1                                                       
      NFLD = NFLAG(K)                                                           
      CALL HISCAP (2, NMVALA, NMUNA, KFLA, ZA, NFLA)                            
      CALL HISCAP (3, NMVALD, NMUND, KFLD, ZD, NFLD)                            
      CALL OUTMAT (MTABLE(M), AMIN, NA, DA, DMIN, ND, DD)                       
      WRITE (NOUT, 9510) NH                                                     
      CALL HISCAP (2, NMVALA, NMUNA, KFLA, ZA, NFLA)                            
      CALL HISCAP (3, NMVALD, NMUND, KFLD, ZD, NFLD)                            
  500 CONTINUE                                                                  
C                                                                               
C     HISTOGRAMS OF PARTICLES LOST                                              
C                                                                               
      IF(LOSS .EQ. 0) RETURN                                                    
      DO 700 K = 1, LOSS                                                        
      NH = NH + 1                                                               
      KFL = KFLG(K) + 1                                                         
      WRITE (NOUT, 9520) NH, KPART(KFL)                                         
 9520 FORMAT(13H1HISTOGRAM NO,I3,4H OF ,A4,5H LOST)                             
      M = NBLK(K)                                                               
      IF(NUNY(K) .NE. 0) GO TO 600                                              
C                                                                               
C     Z DISTRIBUTION                                                            
C                                                                               
      NX = LTHX(K)                                                              
      NUN = NUNX(K)                                                             
      CALL HSTUN (NUN, NMVAL, NMUN, FACT)                                       
      XMIN = XSTRT(K) / FACT                                                    
      DX   = XBIN (K) / FACT                                                    
      CALL HISCAP (4, NMVAL, NMUN, 0, 0.0, 0)                                   
      CALL TRHIST (MTABLE(M), NX, XMIN, DX)                                     
      WRITE (NOUT, 9530) NH, KPART(KFL)                                         
 9530 FORMAT(13H0HISTOGRAM NO,I3,4H OF ,A4,5H LOST)                             
      CALL HISCAP (4, NMVAL, NMUN, 0, 0.0, 0)                                   
      GO TO 700                                                                 
C                                                                               
C     Z VERSUS ANOTHER COORDINATE                                               
C                                                                               
  600 NA = LTHX(K)                                                              
      NUNA = NUNX(K)                                                            
      CALL HSTUN (NUNA, NMVALA, NMUNA, FACT)                                    
      CALL HISCAP (5, NMVALA, NMUNA, 0, 0.0, 0)                                 
      AMIN = XSTRT(K) / FACT                                                    
      DA   = XBIN (K) / FACT                                                    
      ND = LTHY(K)                                                              
      NUND = NUNY(K)                                                            
      CALL HSTUN (NUND, NMVALD, NMUND, FACT)                                    
      CALL HISCAP (6, NMVALD, NMUND, 0, 0.0, 0)                                 
      DMIN = YSTRT(K) / FACT                                                    
      DD   = YBIN (K) / FACT                                                    
      CALL OUTMAT (MTABLE(M), AMIN, NA, DA, DMIN, ND, DD)                       
      WRITE (NOUT, 9530) NH, KPART(KFL)                                         
      CALL HISCAP (5, NMVALA, NMUNA, 0, 0.0, 0)                                 
      CALL HISCAP (6, NMVALD, NMUND, 0, 0.0, 0)                                 
  700 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE OUTMAT (IPLOT, AMIN,NA,DA, DMIN,ND,DD)                         
C                                                                               
C S/R OUTMAT       PRINTS A 2-D HISTOGRAM (SCATTER PLOT)                        
C                                                                               
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      INTEGER IPLOT(2), IAROW(110)                                              
      INTEGER BCDW, DIGIT                                                       
      REAL    AROW(110), TOT(6), ALABEL(15)                                     
      EQUIVALENCE (AROW(1),IAROW(1))                                            
      DATA TOT / 1HT, 1HO, 1HT, 1HA, 1HL, 1HS /                                 
      DATA IBLANK, IDASH, IEND, IAST / 1H , 1H-, 1HI, 1H* /                     
C                                                                               
      MTT = (NA + 1)*(ND + 1) + 5                                               
      IF(IPLOT(MTT)) 2,2,4                                                      
C                                                                               
C     UNFILLED HISTOGRAM                                                        
C                                                                               
    2 WRITE (NOUT, 9001)                                                        
      RETURN                                                                    
 9001 FORMAT(5(1H0,/),22H0HISTOGRAM IS UNFILLED,5(/,1H0))                       
C                                                                               
C     X COORDINATES                                                             
C                                                                               
    4 NLA = NA / 10 + 1                                                         
      ALABEL(1) = AMIN                                                          
      DO 15 N = 2, NLA                                                          
   15 ALABEL(N) = ALABEL(N - 1) + 10.0*DA                                       
      WRITE (NOUT,9002) (ALABEL(K), K = 1, NLA)                                 
 9002 FORMAT(1H0/16X,11F10.3)                                                   
      NALL = NA + 4                                                             
      DO 17 N = 1, NALL                                                         
   17 IAROW(N) = IBLANK                                                         
      DO 18 N = 1, 6                                                            
      NN = NALL + N                                                             
      AROW(NN) = TOT(N)                                                         
   18 CONTINUE                                                                  
      NALP = NALL + 6                                                           
      WRITE (NOUT,9003) (IAROW(N), N = 1, NALP)                                 
 9003 FORMAT (1H+,22X,110A1)                                                    
C                                                                               
C     UPPER X AXIS                                                              
C                                                                               
      ICT = 1                                                                   
      NAM = NA + 1                                                              
      DO 24 I = 1, NAM                                                          
      IAROW(I) = IDASH                                                          
      IF (ICT .EQ. 1 .OR. ICT .EQ. 5) IAROW(I) = IAST                           
      ICT = ICT + 1                                                             
      IF (ICT .EQ. 6) ICT = 1                                                   
   24 CONTINUE                                                                  
      IAROW(NA+2) = IEND                                                        
      NAM = NA + 3                                                              
      NAP = NA + 10                                                             
      DO 26 I = NAM, NAP                                                        
   26 IAROW(I) = IDASH                                                          
      WRITE (NOUT,9024) (IAROW(M), M = 1, NAP)                                  
 9024 FORMAT(21X,2HI*,110A1)                                                    
C                                                                               
C     BODY OF HISTOGRAM                                                         
C                                                                               
      MM = 0                                                                    
      NAP = NA + 9                                                              
      DLOW = DMIN                                                               
      DHIGH = DMIN + DD                                                         
      DO 60 N = 1, ND                                                           
      DO 30 K = 1, NAP                                                          
   30 IAROW(K) = IBLANK                                                         
      DO 48 K = 1, NA                                                           
      MM = MM + 1                                                               
      NN = IPLOT(MM)                                                            
      IAROW(K)= BCDW (NN)                                                       
   48 CONTINUE                                                                  
      IAROW(NA+1) = IBLANK                                                      
      IAROW(NA+2) = IEND                                                        
      MM = MM + 1                                                               
      NN = IPLOT(MM)                                                            
      DO 50 K = 1, 4                                                            
      NK = NA + K                                                               
      IAROW(NK+5)= DIGIT(NN,K)                                                  
   50 CONTINUE                                                                  
      WRITE (NOUT,9004) DLOW, DHIGH, (IAROW(K), K = 1, NAP)                     
 9004 FORMAT(1X,F8.3,3H TO,F8.3,3H I ,110A1)                                    
      DLOW = DLOW + DD                                                          
      DHIGH = DLOW + DD                                                         
   60 CONTINUE                                                                  
C                                                                               
C     LOWER X AXIS                                                              
C                                                                               
      ICT = 1                                                                   
      NAM = NA + 1                                                              
      DO 74 I = 1, NAM                                                          
      IAROW(I) = IDASH                                                          
      IF (ICT .EQ. 1 .OR. ICT .EQ. 5) IAROW(I) = IAST                           
      ICT = ICT + 1                                                             
      IF (ICT .EQ. 6) ICT = 1                                                   
   74 CONTINUE                                                                  
      IAROW(NA+2) = IEND                                                        
      NAM = NA + 3                                                              
      NAP = NA + 10                                                             
      DO 76 I = NAM, NAP                                                        
   76 IAROW(I) = IDASH                                                          
      WRITE (NOUT,9024) (IAROW(M), M = 1, NAP)                                  
C                                                                               
C     SUMS OF COLUMNS                                                           
C                                                                               
      NAM = NA + 2                                                              
      DO 85 K = 1, NAP                                                          
   85 IAROW(K) = IBLANK                                                         
      IAROW(NAM) = IEND                                                         
      WRITE (NOUT,9007) (IAROW(K), K = 1, NAP)                                  
      DO 90 IR = 1, 4                                                           
      DO 86   K = 1, NA                                                         
      MM = MM + 1                                                               
      NN = IPLOT(MM)                                                            
      IAROW(K)= DIGIT(NN,IR)                                                    
   86 CONTINUE                                                                  
      MM = MM + 1                                                               
      IF (IR .EQ. 4) GO TO 87                                                   
      WRITE (NOUT,9007) (IAROW(K), K = 1, NAP)                                  
      MM = MM - NA - 1                                                          
      GO TO 90                                                                  
   87 NN = IPLOT(MM)                                                            
      DO 88 K = 1, 4                                                            
      NK = NA + K                                                               
      IAROW(NK+5)= DIGIT(NN,K)                                                  
   88 CONTINUE                                                                  
      WRITE (NOUT,9006) (IAROW(K), K = 1, NAP)                                  
 9006 FORMAT(13X,10HTOTALS  I ,110A1)                                           
 9007 FORMAT(21X,2HI ,110A1)                                                    
   90 CONTINUE                                                                  
C                                                                               
C     SUMMARY                                                                   
C                                                                               
      IAROW(1) = IPLOT(MTT)                                                     
      DO 95 I = 1, 4                                                            
      N = MM + I                                                                
   95 IAROW(I+1) = IPLOT(N)                                                     
  111 WRITE (NOUT,9104) (IAROW(I), I = 1, 5)                                    
 9104 FORMAT ('0',10X,'TOTAL NUMBER OF ENTRIES =',I10,                          
     + ' INCLUDING UNDERFLOW AND OVERFLOW AS FOLLOWS '/                         
     + '0',32X,'UNDERFLOW',7X,'OVERFLOW'/                                       
     +  21X,'ACROSS',2(5X,I10) /21X,'DOWN  ',2(5X,I10)/)                        
 120  RETURN                                                                    
      END                                                                       
      SUBROUTINE PRINT1 (LABEL, NWORD, DATA, VARY)                              
C                                                                               
C S/R PRINT1       PRINTS THE DATA FOR ONE ELEMENT                              
C                                                                               
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      COMMON /BLOC42/ TABLE(36), PLUS(2), MINUS, BLANK, PERIOD,                 
     A                SEMI(5), QUOTE(6), PAR1(2), PAR2(2)                       
      INTEGER TABLE, PLUS, BLANK, PERIOD, SEMI, QUOTE, PAR1, PAR2               
      INTEGER VARY(30), CHAR(12), NV(21)                                        
      REAL    DATA(30)                                                          
      DATA NV / 6,1,1,3,2,0,0,6,0,1,0,0,0,6,0,2,0,2,2,1,0 /                     
C                                                                               
      NTYPE = DATA(1)                                                           
      KTYPE = IABS(NTYPE)                                                       
      IF(KTYPE .EQ. 15) GO TO 50                                                
      DO 10 J = 1, 12                                                           
   10 CHAR(J) = BLANK                                                           
      IF(NTYPE .LE. 0) GO TO 40                                                 
      KV = 1                                                                    
      IF(NTYPE .LE. 21) KV = NV(NTYPE)                                          
      IF(KV .EQ. 0) GO TO 40                                                    
      LV = 0                                                                    
      DO 30 JV = 1, KV                                                          
      K = IABS(VARY(JV+1))                                                      
      IF(VARY(JV+1) .GE. 0) GO TO 20                                            
      LV = LV + 1                                                               
      CHAR(LV) = MINUS                                                          
   20 LV = LV + 1                                                               
   30 CHAR(LV) = TABLE(K+1)                                                     
   40 IF(NWORD .LE. 1) GO TO 60                                                 
      WRITE (NOUT, 940) NTYPE, CHAR, LABEL,                                     
     A                  (BLANK, DATA(J), J = 2, NWORD), SEMI(1)                 
      RETURN                                                                    
   50 WRITE (NOUT, 950) NTYPE, LABEL, (DATA(J), J = 2, 4)                       
      RETURN                                                                    
   60 WRITE (NOUT, 960) NTYPE, CHAR, LABEL                                      
      RETURN                                                                    
C                                                                               
  940 FORMAT(1H0,I4,1H.,12A1,2H ",A4,2H" ,A1,8(F11.5,A1)/                       
     A       (27X,8(F11.5,A1)))                                                 
  950 FORMAT(1H0,I4,1H.,13X,1H",A4,2H" ,F12.5,4H   ",A4,4H"   ,                 
     A       F12.5,2H ;)                                                        
  960 FORMAT(1H0,I4,1H.,12A1,2H ",A4,3H" ;)                                     
      END                                                                       
      SUBROUTINE QUAD                                                           
C                                                                               
C S/R QUAD         TRACKS CHARGED PARTICLES THRU A QUADRUPOLE                   
C                                                                               
      COMMON /BLOC1/ DATA(3001), I, I1                                          
      INTEGER IDATA(3001)                                                       
      EQUIVALENCE (IDATA(1), DATA(1))                                           
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      COMMON /BLOC15/ LL, KH2, KV2, KH, KV, KHL, KVL, RAY2(6),                  
     A                RH(4), RV(4)                                              
      REAL LL, KH2, KV2, KH, KV, KHL, KVL                                       
C                                                                               
      KH2 = DATA(I+2) / P                                                       
      KV2 = - KH2                                                               
      CALL FOCUS (KH2, LL, KH, KHL, RH)                                         
      CALL FOCUS (KV2, LL, KV, KVL, RV)                                         
      RAY2(1) = RH(1) * RAY(1) + RH(2) * RAY(2)                                 
      RAY2(2) = RH(3) * RAY(1) + RH(4) * RAY(2)                                 
      RAY2(3) = RV(1) * RAY(3) + RV(2) * RAY(4)                                 
      RAY2(4) = RV(3) * RAY(3) + RV(4) * RAY(4)                                 
      CALL RESET (RAY, RAY2)                                                    
      RETURN                                                                    
      END                                                                       
      LOGICAL FUNCTION QSTOP(LFLAG)                                             
C                                                                               
C FCT QSTOP        TESTS FOR QUADRUPOLE APERTURES                               
C                                                                               
      COMMON /BLOC1/ DATA(3001), I, I1                                          
      INTEGER IDATA(3001)                                                       
      EQUIVALENCE (IDATA(1), DATA(1))                                           
      COMMON /BLOC5/ SEC, CAP(3), MPA                                           
      LOGICAL SEC, CAP, MPA                                                     
      COMMON /BLOC11/ QAP(3), ELLIPS, HYPER                                     
      LOGICAL ELLIPS, HYPER                                                     
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
C                                                                               
      IF(.NOT. CAP(LFLAG+1)) GO TO 30                                           
      IF(ELLIPS) GO TO 10                                                       
      IF(HYPER) GO TO 20                                                        
      IF(RAY(1) ** 2 + RAY(3) ** 2 .GT. DATA(I+3)) GO TO 40                     
      GO TO 30                                                                  
   10 IF((RAY(1)/QAP(1)) ** 2 + (RAY(3)/QAP(2)) ** 2 .GT. 1.0) GO TO 40         
      IF(.NOT. HYPER) GO TO 30                                                  
   20 IF(ABS(RAY(1) * RAY(3)) .GT. QAP(3)) GO TO 40                             
   30 QSTOP = .FALSE.                                                           
      RETURN                                                                    
   40 QSTOP = .TRUE.                                                            
      RETURN                                                                    
      END                                                                       
      SUBROUTINE RAYRUN                                                         
C                                                                               
C S/R RAYRUN       TRACKS ONE PARENT PARTICLE AND ITS TWO DAUGHTERS             
C                                                                               
      COMMON /BLOC1/ DATA(3001), I, I1                                          
      INTEGER IDATA(3001)                                                       
      EQUIVALENCE (IDATA(1), DATA(1))                                           
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      COMMON /BLOC4/ RAYMAX(6), PBEAM, P0, NORAYS                               
      COMMON /BLOC5/ SEC, CAP(3), MPA                                           
      LOGICAL SEC, CAP, MPA                                                     
      COMMON /BLOC6/ NHISTS, NHIST, LIMNO                                       
      COMMON /BLOC7/ MTYPE(100), NENTRY(100), NCOOR(100), INTVLS(100),          
     A               BEGIN(100), END(100), STEP(100), ZHIST(100)                
      COMMON /BLOC8/ MTABLE(1000000)                                              
      COMMON /BLOC10/ APB(2), LAYL, LAYX, RAB1, RAB2, FACE                      
      REAL LAYL, LAYX                                                           
      COMMON /BLOC11/ QAP(3), ELLIPS, HYPER                                     
      LOGICAL ELLIPS, HYPER                                                     
      COMMON /BLOC12/ VALUE(100), XMOM(100), XMOM2(100), NFLAG(100),            
     A                SET(100), KFLAG(100), IFLAG(3,9), ZFLAG(9)                
      LOGICAL SET, IFLAG                                                        
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      COMMON /BLOC14/ MMAX, GTOP, DB(20), COSM(20), SINM(20)                    
      COMMON /BLOC15/ LL, KH2, KV2, KH, KV, KHL, KVL, RAY2(6),                  
     A                RH(4), RV(4)                                              
      REAL LL, KH2, KV2, KH, KV, KHL, KVL                                       
      COMMON /BLOC16/ KPART(3), ISTOP(3,500)                                    
      COMMON /BLOC17/ IP, IC(4), IS(4)                                          
      COMMON /BLOC18/ LOSS, NBLK(10), KFLG(10),                                 
     A                XSTRT(10), XEND(10), LTHX(10), XBIN(10), NUNX(10),        
     B                YSTRT(10), YEND(10), LTHY(10), YBIN(10), NUNY(10)         
      LOGICAL ELLS, HYPS, BSTOP, QSTOP                                          
      INTEGER ICS(4), ISS(4)                                                    
      REAL LS, QAPS(3)                                                          
c                                                                               
      data npbct /0/                                                            
C                                                                               
C     SELECT RAY AT RANDOM                                                      
C                                                                               
      P0 = PBEAM                                                                
      CALL EMIT (RAY, P, P0, RAYMAX)                                            
      Z = 0.0                                                                   
      LFLAG = 0                                                                 
      IF(DKFLAG) CALL DECAYZ (ZD)                                               
      IAP = 0                                                                   
      NHIST = 0                                                                 
      IP = 0                                                                    
      ELLIPS = .FALSE.                                                          
      HYPER = .FALSE.                                                           
      APB(1) = 0.0                                                              
      APB(2) = 0.0                                                              
      MMAX = 0                                                                  
      INEXT = - 10                                                              
      IF(.NOT. MPA) GO TO 130                                                   
      DO 120 IM = 1, 20                                                         
  120 DB(IM) = 0.0                                                              
  130 I = 1                                                                     
      DO 140 KFL = 1, 3                                                         
      DO 140 IFL = 1, 9                                                         
  140 IFLAG(KFL,IFL) = .FALSE.                                                  
      DO 150 NH = 1, NHISTS                                                     
  150 SET(NH) = .FALSE.                                                         
C                                                                               
C     TRACK THROUGH ONE ELEMENT                                                 
C                                                                               
  200 NTYPE = IDATA(I)                                                          
      IF(NTYPE .EQ. 53) GO TO 530                                               
      IF(NTYPE .GE. 50) GO TO 500                                               
      GO TO (210, 220, 230, 240, 250, 260, 270, 600, 290, 600, 600,             
     A       600, 600, 340, 600, 360, 600, 380, 390, 400, 600), NTYPE           
C                                                                               
C     RMS ADDITION TO BEAM                                                      
C                                                                               
  210 IF(IDATA(I+8) .NE. LFLAG) GO TO 600                                       
  211 CALL SCATT (RAY, P, P0, DATA(I+1))                                        
      RAY(6) = (P - P0) / P0                                                    
      GO TO 600                                                                 
C                                                                               
C     FRINGE FIELD                                                              
C                                                                               
  220 IF(LFLAG .EQ. 2) GO TO 600                                                
      H = DATA(I+1) / P                                                         
      TANB = DATA(I+2)                                                          
      TANCOR = TANB - H * DATA(I+5)                                             
      RAY2(1) = RAY(1)                                                          
      RAY2(2) = RAY(2) + H * RAY(1) * TANB                                      
      RAY2(3) = RAY(3)                                                          
      RAY2(4) = RAY(4) - H * RAY(3) * TANCOR                                    
      IF(.NOT. SEC) GO TO 225                                                   
      SECB = DATA(I+3)                                                          
      SEX = DATA(I+4)                                                           
      RAY2(1) = RAY(1) + 0.5 * H * ((SECB*RAY(3))**2 - (TANB*RAY(1))**2)        
      RAY2(2) = RAY2(2) + H * (TANB**2 * (RAY(1)*RAY(2) - RAY(3)*RAY(4))        
     B     + (H * DATA(I+6) + SEX) * RAY(1) ** 2                                
     C     + (H * DATA(I+7) - SEX) * RAY(3) ** 2)                               
      RAY2(3) = RAY(3) + H * TANB * TANCOR * RAY(1) * RAY(3)                    
      RAY2(4) = RAY2(4) - H * (TANCOR * TANB * RAY(1) * RAY(4)                  
     B    + (H * DATA(I+8) + 2.0 * SEX) * RAY(1) * RAY(3)                       
     C    + SECB ** 2 * RAY(2) * RAY(3))                                        
  225 CALL RESET (RAY, RAY2)                                                    
      GO TO 600                                                                 
C                                                                               
C     DRIFT SPACE                                                               
C                                                                               
  230 LL = DATA(I+1)                                                            
      IF(.NOT. DKFLAG) GO TO 235                                                
      IF(LFLAG .NE. 0 .OR. Z + LL .LT. ZD) GO TO 235                            
      LL = ZD - Z                                                               
      RAY(1) = RAY(1) + LL * RAY(2)                                             
      RAY(3) = RAY(3) + LL * RAY(4)                                             
      GO TO 700                                                                 
  235 RAY(1) = RAY(1) + LL * RAY(2)                                             
      RAY(3) = RAY(3) + LL * RAY(4)                                             
      GO TO 590                                                                 
C                                                                               
C     BENDING MAGNET                                                            
C                                                                               
  240 IAP = IAP + 1                                                             
      IF(BSTOP(LFLAG)) GO TO 650                                                
      LL = DATA(I+1)                                                            
      IF(.NOT. DKFLAG) GO TO 244                                                
      IF(LFLAG - 1) 241, 244, 245                                               
  241 IF(Z + LL .LT. ZD) GO TO 244                                              
      LL = ZD - Z                                                               
      CALL BEND (KSTOP)                                                         
      IF(KSTOP) 650, 700, 650                                                   
  244 CALL BEND (KSTOP)                                                         
      IF(KSTOP) 650, 249, 650                                                   
  245 IF(DATA(I+4) .EQ. 0.0) GO TO 247                                          
      THETA = LL * DATA(I+2)                                                    
      COSTH = COS(THETA)                                                        
      SINTH = SIN(THETA)                                                        
      THE2 = (COSTH * RAY(2) + SINTH) / (COSTH - RAY(2) * SINTH)                
      F = SQRT ((1.0 + THE2 ** 2) / (1.0 + RAY(2) ** 2))                        
      RAY(1) = F * (RAY(1) + DATA(I+4)) - DATA(I+4)                             
      RAY(2) = THE2                                                             
      RAY(3) = RAY(3) + (DATA(I+4) + RAY(1)) * SIN(THETA) * RAY(4)              
      RAY(4) = F * RAY(4)                                                       
      GO TO 249                                                                 
  247 RAY(1) = RAY(1) + LL * RAY(2)                                             
      RAY(3) = RAY(3) + LL * RAY(4)                                             
  249 IF(BSTOP(LFLAG)) GO TO 650                                                
      GO TO 590                                                                 
C                                                                               
C     QUADRUPOLE                                                                
C                                                                               
  250 IAP = IAP + 1                                                             
      IF(QSTOP(LFLAG)) GO TO 650                                                
      LL = DATA(I+1)                                                            
      IF(LFLAG .EQ. 2) GO TO 255                                                
      GTOP = 0.5 * DATA(I+1) * DATA(I+2) / P                                    
      IF(MMAX .NE. 0) CALL MULTI                                                
      IF(.NOT. DKFLAG) GO TO 254                                                
      IF(LFLAG .NE. 0) GO TO 254                                                
      IF(Z + LL .LT. ZD) GO TO 254                                              
      LL = ZD - Z                                                               
      CALL QUAD                                                                 
      GO TO 700                                                                 
  254 CALL QUAD                                                                 
      IF(MMAX .NE. 0) CALL MULTI                                                
      GO TO 259                                                                 
  255 RAY(1) = RAY(1) + LL * RAY(2)                                             
      RAY(3) = RAY(3) + LL * RAY(4)                                             
  259 IF(QSTOP(LFLAG)) GO TO 650                                                
      GO TO 590                                                                 
C                                                                               
C     SLIT                                                                      
C                                                                               
  260 IAP = IAP + 1                                                             
      npbct=npbct+1                                                             
      IFP = IDATA(I+1) / 100                                                    
      IFF = IDATA(I+1) - 100 * IFP                                              
      IF(IFP .NE. LFLAG) GO TO 267                                              
      IFG = IDATA(I+3)                                                          
      IF(IFG .NE. 0) GO TO 265                                                  
      IF(ABS(RAY(IFF)) .LT. DATA(I+2)) GO TO 267                                
      GO TO 266                                                                 
  265 continue                                                                  
c                                                                               
c     Crystal                                                                   
c                                                                               
      if (data(i+6).ne.0.0) then                                                
c        Crystal type of cuts                                                   
         if (abs(ray(iff)).gt.abs(data(i+2))) goto 266                          
         x=ray(iff)                                                             
         y=ray(ifg)                                                             
         ymx=data(i+4)+(x/data(i+2)) * (data(i+6)-data(i+4))                    
         if (y.gt.ymx) goto 266                                                 
         ymn=ymx+data(i+5)-data(i+4)                                            
         if (y.lt.ymn) goto 266                                                 
         goto 267                                                               
      endif                                                                     
c                                                                               
      IF((RAY(IFF) / DATA(I+2)) ** 2 + (RAY(IFG) / DATA(I+4)) ** 2              
     A       .LT. 1.0) GO TO 267                                                
  266 I = I + NU(6)                                                             
      NTYPE = IDATA(I)                                                          
      IF(NTYPE .EQ. 1 .AND. IDATA(I+8) .EQ. LFLAG) GO TO 211                    
      GO TO 650                                                                 
  267 I = I + NU(6)                                                             
      IF(I .GT. I1) GO TO 610                                                   
      NTYPE = IDATA(I)                                                          
      IF(NTYPE .EQ. 1 .AND. IDATA(I+8) .EQ. LFLAG) GO TO 600                    
      GO TO 200                                                                 
C                                                                               
C     SHIFT                                                                     
C                                                                               
  270 DO 271 J = 1, 6                                                           
      IJ = I + J                                                                
      RAY(J) = RAY(J) + DATA(IJ)                                                
  271 CONTINUE                                                                  
      P = P + DATA(I+6) * P0                                                    
      GO TO 600                                                                 
C                                                                               
C     REPETE                                                                    
C                                                                               
  290 CALL REPETE                                                               
      GO TO 600                                                                 
C                                                                               
C     ARBITRARY MATRIX                                                          
C                                                                               
  340 NROW = IDATA(I+7)                                                         
      IF(I .NE. INEXT) CALL RESET(RAY2, RAY)                                    
      RAY2(NROW) = 0.0                                                          
      DO 342 IR = 1, 6                                                          
      IIR = I + IR                                                              
      RAY2(NROW) = RAY2(NROW) + RAY(IR) * DATA(IIR)                             
  342 CONTINUE                                                                  
      IF(.NOT. SEC .OR. IDATA(I+8) .NE. 0) GO TO 344                            
      IPL = I + 8                                                               
      DO 343 IB = 1, 6                                                          
      DO 343 JC = IB, 6                                                         
      IPL = IPL + 1                                                             
  343 RAY2(NROW) = RAY2(NROW) + RAY(IB) * RAY(JC) * DATA(IPL)                   
  344 INEXT = I + NU(14)                                                        
      IF(IDATA(INEXT) .NE. 14) CALL RESET (RAY, RAY2)                           
      GO TO 600                                                                 
C                                                                               
C     MULTIPOLE ABERRATIONS AND BENDING MAGNET PARAMETERS                       
C                                                                               
  360 NM = IDATA(I+1)                                                           
      IF(NM .GT. 0) GO TO 362                                                   
      IF(LFLAG .EQ. 2) GO TO 600                                                
      NM = - NM                                                                 
      DB(NM) = DATA(I+2)                                                        
      COSM(NM) = DATA(I+3)                                                      
      SINM(NM) = DATA(I+4)                                                      
      MAXX = MAX0(NM,MMAX)                                                      
      MMAX = 0                                                                  
      DO 361 IM = 1, MAXX                                                       
      IF(DB(IM) .NE. 0.0) MMAX = IM                                             
  361 CONTINUE                                                                  
      GO TO 600                                                                 
  362 IF(NM .EQ. 4 .OR. NM .EQ. 5) GO TO 363                                    
      IF (NM .EQ. 11) GO TO 366                                                 
      IF(NM .EQ. 100) GO TO 364                                                 
      IF(NM .EQ. 101) GO TO 365                                                 
      GO TO 600                                                                 
  363 APB(NM-3) = DATA(I+2)                                                     
      GO TO 600                                                                 
  364 ELLIPS = .TRUE.                                                           
      QAP(1) = DATA(I+2)                                                        
      QAP(2) = DATA(I+3)                                                        
      GO TO 600                                                                 
  365 HYPER = .TRUE.                                                            
      QAP(3) = DATA(I+2)                                                        
      GO TO 600                                                                 
  366 P0 = DATA(I+2)                                                            
      RAY(6) = (P - P0) / P0                                                    
      GO TO 600                                                                 
C                                                                               
C     SEXTUPOLE                                                                 
C                                                                               
  380 LL = DATA(I+1)                                                            
      IF(.NOT. DKFLAG) GO TO 384                                                
      IF(LFLAG - 1) 381, 384, 235                                               
  381 IF(Z + LL .LT. ZD) GO TO 384                                              
      LL = ZD - Z                                                               
      CALL SEXT                                                                 
      GO TO 700                                                                 
  384 CALL SEXT                                                                 
      GO TO 590                                                                 
C                                                                               
C     SOLENOID                                                                  
C                                                                               
  390 LL = DATA(I+1)                                                            
      IF(.NOT. DKFLAG) GO TO 394                                                
      IF(LFLAG - 1) 391, 394, 235                                               
  391 IF(Z + LL .LT. ZD) GO TO 394                                              
      LL = ZD - Z                                                               
      CALL SOLO                                                                 
      GO TO 700                                                                 
  394 CALL SOLO                                                                 
      GO TO 590                                                                 
C                                                                               
C     BEAM ROTATION                                                             
C                                                                               
  400 CS = DATA(I+1)                                                            
      SN = DATA(I+2)                                                            
      RAY2(1) = RAY(1) * CS + RAY(3) * SN                                       
      RAY2(3) = - RAY(1) * SN + RAY(3) * CS                                     
      RAY2(2) = RAY(2) * CS + RAY(4) * SN                                       
      RAY2(4) = - RAY(2) * SN + RAY(4) * CS                                     
      CALL RESET (RAY, RAY2)                                                    
      GO TO 600                                                                 
C                                                                               
C     SAVE VALUES FOR HISTOGRAM                                                 
C                                                                               
  500 NHIST = NHIST + 1                                                         
      IF(LFLAG .NE. KFLAG(NHIST)) GO TO 600                                     
      NCO = NCOOR(NHIST)                                                        
      VALUE(NHIST) = HSTVAL(NCO)                                                
      SET(NHIST) = .TRUE.                                                       
      GO TO 600                                                                 
C                                                                               
C     SET HISTOGRAM FLAG                                                        
C                                                                               
  530 IFL = IDATA(I+1)                                                          
      IFLAG(LFLAG+1,IFL) = .TRUE.                                               
      GO TO 600                                                                 
C                                                                               
C     NEXT ELEMENT                                                              
C                                                                               
  590 Z = Z + DATA(I+1)                                                         
  600 I = I + NU(NTYPE)                                                         
      IF(I .LE. I1) GO TO 200                                                   
C                                                                               
C     END OF SYSTEM REACHED. IF WANTED SWITCH TO NEUTRAL DAUGHTER               
C                                                                               
  610 IF(LFLAG .NE. 1 .OR. .NOT. DKFLAG) GO TO 690                              
      DO 620 J = 1, 6                                                           
  620 RAY(J) = RAYN(J)                                                          
      NHIST = NHS                                                               
      I = ISAVE                                                                 
      NTYPE = IDATA(I)                                                          
      IP = IPS                                                                  
      DO 625 J = 1, 4                                                           
      IC(J) = ICS(J)                                                            
  625 IS(J) = ISS(J)                                                            
      Z = ZS                                                                    
      LL = LS                                                                   
      P = PN                                                                    
      P0 = P0S                                                                  
      LFLAG = 2                                                                 
      IAP = IAPS                                                                
      ELLIPS = ELLS                                                             
      HYPER = HYPS                                                              
      APB(1) = APBS1                                                            
      APB(2) = APBS2                                                            
      DO 630 J = 1, 3                                                           
  630 QAP(J) = QAPS(J)                                                          
      GO TO (600, 600, 235, 245, 255, 600, 600, 600, 600, 600, 600,             
     A       600, 600, 600, 600, 600, 600, 235, 235, 600, 600), NTYPE           
C                                                                               
C     PARTICLE STOPPED BY APERTURE                                              
C                                                                               
  650 ISTOP(LFLAG+1,IAP) = ISTOP(LFLAG+1,IAP) + 1                               
      IF(LOSS .EQ. 0) GO TO 610                                                 
      DO 670 K = 1, LOSS                                                        
      IF(LFLAG .NE. KFLG(K)) GO TO 670                                          
      M = NBLK(K)                                                               
      XMIN = XSTRT(K)                                                           
      XMAX = XEND(K)                                                            
      XSTP = XBIN(K)                                                            
      NX = LTHX(K)                                                              
      NUX = NUNX(K)                                                             
      X = HSTVAL(NUX)                                                           
      NUY = NUNY(K)                                                             
      IF(NUY .NE. 0) GO TO 660                                                  
      CALL HISTO (X, MTABLE(M), NX, XMIN, XMAX, XSTP)                           
      GO TO 670                                                                 
  660 YMIN = YSTRT(K)                                                           
      YMAX = YEND(K)                                                            
      YSTP = YBIN(K)                                                            
      NY = LTHY(K)                                                              
      Y = HSTVAL(NUY)                                                           
      CALL ENTMAT (X,Y,MTABLE(M),XMIN,XMAX,NX,XSTP,YMIN,YMAX,NY,YSTP)           
  670 CONTINUE                                                                  
      GO TO 610                                                                 
C                                                                               
C     END OF RAY TRACKING                                                       
C                                                                               
  690 RETURN                                                                    
C                                                                               
C     DECAY                                                                     
C                                                                               
  700 CALL DECAY                                                                
      DO 710 J = 1, 6                                                           
  710 RAY(J) = RAYC(J)                                                          
      NHS = NHIST                                                               
      ISAVE = I                                                                 
      IPS = IP                                                                  
      DO 715 J = 1, 4                                                           
      ICS(J) = IC(J)                                                            
  715 ISS(J) = IS(J)                                                            
      ZS = Z                                                                    
      LL = DATA(I+1) - LL                                                       
      LS = LL                                                                   
      P = PC                                                                    
      P0S = P0                                                                  
      LFLAG = 1                                                                 
      IAPS = IAP                                                                
      ELLS = ELLIPS                                                             
      HYPS = HYPER                                                              
      APBS1 = APB(1)                                                            
      APBS2 = APB(2)                                                            
      DO 720 J = 1, 3                                                           
  720 QAPS(J) = QAP(J)                                                          
      GO TO (600, 600, 235, 244, 254, 600, 600, 600, 600, 600, 600,             
     A       600, 600, 600, 600, 600, 600, 384, 394, 600, 600), NTYPE           
      END                                                                       
      SUBROUTINE RDELMT                                                         
C                                                                               
C S/R RDELMT       READS DATA FOR ONE ELEMENT                                   
C                                                                               
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      COMMON /BLOC41/ LW, IMAGE(20), FLUSH, INDIC, NTYPE, LABLE, LENGTH,        
     A                NWORD, NVARY, DATUM(30), VARY(30)                         
      INTEGER VARY, TEXT(30)                                                    
      LOGICAL FLUSH                                                             
      EQUIVALENCE (TEXT(1), DATUM(1))                                           
      COMMON /BLOC42/ TABLE(36), PLUS(2), MINUS, BLANK, PERIOD,                 
     A                SEMI(5), QUOTE(6), PAR1(2), PAR2(2)                       
      INTEGER TABLE, PLUS, BLANK, PERIOD, SEMI, QUOTE, PAR1, PAR2               
      COMMON /BLOC43/ NCD, CARD(80), MC, EMPTY, ITEM                            
      INTEGER CARD                                                              
      LOGICAL EMPTY                                                             
      INTEGER NO1(21), UTABLE(14)
      INTEGER IWORD(1)
      REAL    CTABLE(14)                                                        
      LOGICAL SNTNL, SEPAR                                                      
      DATA SNTNL  / .FALSE. /                                                   
      DATA NO1    / 8,2,2,4,4,3,7,8,2,5,5,16,2,8,4,3,1,4,3,2,4 /                
      DATA UTABLE / 2HCM , 1HM  , 2HIN , 2HFT , 2HMM , 1HR  , 2HMR ,            
     A              2HPC , 4HP/10, 1HN , 3HMEV, 3HGEV, 2HKG , 1HG   /           
      DATA CTABLE / 1.0, 100.0, 2.54, 30.48, 0.1, 1000.0, 1.0, 1.0,             
     A              0.1, 100.0, 0.001, 1.0, 1.0, 0.001 /                        
      DATA IARROW / 1H* /                                                       
C                                                                               
C     SET DEFAULT VALUES                                                        
C                                                                               
   10 DO 20 J = 1, 30                                                           
      DATUM(J) = 0.0                                                            
   20 VARY (J) = 0                                                              
      NWORD = 0                                                                 
      NVARY = 1                                                                 
      LABLE = BLANK                                                             
C                                                                               
C     HAS SENTINEL BEEN READ AT PREVIOUS CALL                                   
C                                                                               
      IF(.NOT. SNTNL) GO TO 30                                                  
      SNTNL = .FALSE.                                                           
      RETURN                                                                    
   30 SEPAR = .TRUE.                                                            
      CALL RDSKIP                                                               
      GO TO 110                                                                 
C                                                                               
C     SEPARATORS                                                                
C                                                                               
  100 SEPAR = ITEM .EQ. BLANK                                                   
      CALL RDNEXT (- 1)                                                         
  110 DO 115 J = 1, 5                                                           
      IF(ITEM .EQ. SEMI(J)) GO TO 500                                           
  115 CONTINUE                                                                  
      DO 120 J = 1, 6                                                           
      IF(ITEM .EQ. QUOTE(J)) GO TO 350                                          
  120 CONTINUE                                                                  
      IF(ITEM .EQ. TABLE(29)) GO TO 450                                         
      IF(SEPAR) GO TO 150                                                       
  130 WRITE (0, 9130) CARD, (BLANK, J = 1, MC), IARROW                       
      CALL ABEND                                                                
  150 IF(NWORD) 300, 200, 300                                                   
C                                                                               
C     TYPE CODE                                                                 
C                                                                               
  200 CALL RDFIX (NTYPE, IFLAG)                                                 
      IF(IFLAG .NE. 0) GO TO 130                                                
      NWORD = 1                                                                 
      DATUM(1) = NTYPE                                                          
      NTYPE = IABS(NTYPE)                                                       
      IF(ITEM .NE. PERIOD) GO TO 100                                            
C                                                                               
C     VARY CODES                                                                
C                                                                               
  210 CALL RDNEXT (0)                                                           
      IFLAG = 0                                                                 
      IF(ITEM .EQ. MINUS) GO TO 220                                             
      ISIG = 1                                                                  
      IF(ITEM .EQ. PLUS(1) .OR. ITEM .EQ. PLUS(2)) GO TO 230                    
      GO TO 240                                                                 
  220 ISIG = - 1                                                                
  230 CALL RDNEXT (0)                                                           
      IFLAG = 1                                                                 
  240 DO 250 J = 1, 36                                                          
      IF(ITEM .EQ. TABLE(J)) GO TO 260                                          
  250 CONTINUE                                                                  
      IF(IFLAG) 130, 100, 130                                                   
  260 IF(NVARY .GE. 30) GO TO 210                                               
      NVARY = NVARY + 1                                                         
      VARY(NVARY) = ISIGN(J - 1, ISIG)                                          
      GO TO 210                                                                 
C                                                                               
C     DATA VALUE                                                                
C                                                                               
  300 CALL RDFLT (VALUE, IFLAG)                                                 
      IF(IFLAG .NE. 0) GO TO 130                                                
      NWORD = NWORD + 1                                                         
      IF(NWORD .LE. 30) DATUM(NWORD) = VALUE                                    
      GO TO 100                                                                 
C                                                                               
C     LABEL                                                                     
C                                                                               
  350 CALL RDSTRG (IWORD, 1, L)                                                 
      IF(L .GT. 1) WRITE (0, 9350)                                           
      SEPAR = .TRUE.                                                            
      IF(NWORD .EQ. 2 .AND. NTYPE .EQ. 15) GO TO 360                            
      IF(LABLE .NE. BLANK) WRITE (0, 9360) LABLE                             
      LABLE = IWORD(1)
      GO TO 110                                                                 
  360 TEXT(3) = IWORD(1)
      NWORD = 3                                                                 
      GO TO 110                                                                 
C                                                                               
C     SENTINEL                                                                  
C                                                                               
  450 EMPTY = .TRUE.                                                            
      IF(NWORD .EQ. 0) RETURN                                                   
      SNTNL = .TRUE.                                                            
      GO TO 502                                                                 
C                                                                               
C     CHECK VALIDITY OF ELEMENT JUST READ                                       
C                                                                               
  500 IF(NWORD .EQ. 0) GO TO 130                                                
      CALL RDNEXT (1)                                                           
  502 IF(NTYPE .EQ. 0) GO TO 510                                                
      IF(NTYPE .LE. 21) GO TO 530                                               
      IF(NTYPE .LT. 50) GO TO 510                                               
      IF(NTYPE .LE. 53) GO TO 540                                               
      IF(NTYPE .EQ. 60) GO TO 540                                               
  510 WRITE (0, 9510)                                                        
      FLUSH = .TRUE.                                                            
      GO TO 700                                                                 
  530 LENGTH = NO1(NTYPE)                                                       
      if (ntype.eq.6) then                                                      
        length=3                                                                
        if (datum(4).ne.0.0) length=5                                           
        if (datum(6).ne.0.0) length=7                                           
      endif                                                                     
      IF(NTYPE .NE. 16) GO TO 535                                               
      IF(DATUM(2) .GE. 0.0 .AND. DATUM(2) .LT. 99.5) GO TO 535                  
      LENGTH = 4                                                                
      IF(DATUM(2) .GE. 199.5) LENGTH = 2                                        
  535 IF(NWORD - LENGTH) 600, 700, 550                                          
  540 LENGTH = 5                                                                
      IF(NTYPE .EQ. 53) LENGTH = 1                                              
      IF(NWORD - LENGTH) 600, 700, 550                                          
C                                                                               
C     DATA OVERFLOW                                                             
C                                                                               
  550 IF(NTYPE .NE. 6) GO TO 555                                                
      LENGTH = 3                                                                
      IF(DATUM(4) .NE. 0.0) LENGTH = 5                                          
      IF(DATUM(6) .NE. 0.0) LENGTH = 7                                          
      IF(NWORD - LENGTH) 600, 700, 560                                          
  555 IF(NTYPE .EQ.  1) LENGTH = 9                                              
      IF(NTYPE .EQ. 14) LENGTH = 30                                             
      IF(NWORD - LENGTH) 600, 700, 560                                          
  560 WRITE (0, 9560)                                                        
      GO TO 690                                                                 
C                                                                               
C     FILL IN INCOMPLETE 15 ELEMENT                                             
C                                                                               
  600 IF(INDIC .NE. 0) GO TO 700                                                
      IF(NTYPE .NE. 15) GO TO 690                                               
      IF(NWORD - 2) 620, 610, 630                                               
  610 IF(DATUM(2) .NE. 0.0) GO TO 650                                           
  620 TEXT(3) = BLANK                                                           
      GO TO 690                                                                 
  630 DO 640 J = 1, 14                                                          
      IF(TEXT(3) .EQ. UTABLE(J)) GO TO 660                                      
  640 CONTINUE                                                                  
  650 WRITE (0, 9650)                                                        
      FLUSH = .TRUE.                                                            
      GO TO 700                                                                 
  660 DATUM(4) = CTABLE(J)                                                      
      IF(DATUM(2) .GT. 6.0 .AND. J .LT. 6) DATUM(4) = 0.01 * DATUM(4)           
  690 NWORD = LENGTH                                                            
C                                                                               
C     PRINT OUT                                                                 
C                                                                               
  700 continue
c     CALL PRINT1 (LABLE, NWORD, DATUM, VARY)                                   
      IF(FLUSH) GO TO 10                                                        
      RETURN                                                                    
C                                                                               
 9130 FORMAT(52H0SCANNING STOPS DUE TO ERROR AT POSITION SHOWN BELOW/           
     A       11X,80A1/10X,81A1)                                                 
 9350 FORMAT(33H0NEXT LABEL TRUNCATED TO 4 CHARS.)                              
 9360 FORMAT(8H0LABEL ",A4,33H" ON NEXT ELEMENT WAS OVERWRITTEN)                
 9510 FORMAT(38H0NEXT ELEMENT IS ILLEGAL - RUN FLUSHED)                         
 9560 FORMAT(39H0DATA LIST FOR NEXT ELEMENT IS TOO LONG)                        
 9650 FORMAT(31H0ERROR ON FOLLOWING 15. ELEMENT)                                
      END                                                                       
      SUBROUTINE RDFIX (IVALUE, IFLAG)                                          
C                                                                               
C S/R RDFIX        READS ONE INTEGER                                            
C                                                                               
      COMMON /BLOC42/ TABLE(36), PLUS(2), MINUS, BLANK, PERIOD,                 
     A                SEMI(5), QUOTE(6), PAR1(2), PAR2(2)                       
      INTEGER TABLE, PLUS, BLANK, PERIOD, SEMI, QUOTE, PAR1, PAR2               
      COMMON /BLOC43/ NCD, CARD(80), MC, EMPTY, ITEM                            
      INTEGER CARD                                                              
      LOGICAL EMPTY                                                             
C                                                                               
      IFLAG = - 1                                                               
      IVAL = 0                                                                  
      IF(ITEM .EQ. MINUS) GO TO 10                                              
      ISIG = 1                                                                  
      IF(ITEM .EQ. PLUS(1) .OR. ITEM .EQ. PLUS(2)) GO TO 20                     
      GO TO 30                                                                  
   10 ISIG = - 1                                                                
   20 CALL RDNEXT (0)                                                           
   30 DO 40 J = 1, 10                                                           
      IF(ITEM .EQ. TABLE(J)) GO TO 50                                           
   40 CONTINUE                                                                  
      IVALUE = ISIGN (IVAL, ISIG)                                               
      RETURN                                                                    
   50 IVAL = 10 * IVAL + J - 1                                                  
      IFLAG = 0                                                                 
      GO TO 20                                                                  
      END                                                                       
      SUBROUTINE RDFLT (VALUE, IFLAG)                                           
C                                                                               
C S/R RDFLT        READS ONE REAL NUMBER                                        
C                                                                               
      COMMON /BLOC42/ TABLE(36), PLUS(2), MINUS, BLANK, PERIOD,                 
     A                SEMI(5), QUOTE(6), PAR1(2), PAR2(2)                       
      INTEGER TABLE, PLUS, BLANK, PERIOD, SEMI, QUOTE, PAR1, PAR2               
      COMMON /BLOC43/ NCD, CARD(80), MC, EMPTY, ITEM                            
      INTEGER CARD                                                              
      LOGICAL EMPTY                                                             
C                                                                               
      IFLAG = - 1                                                               
      VAL = 0.0                                                                 
      JPL = 0                                                                   
      NEX = 0                                                                   
      IF(ITEM .EQ. MINUS) GO TO 10                                              
      SIG = 1.0                                                                 
      IF(ITEM .EQ. PLUS(1) .OR. ITEM .EQ. PLUS(2)) GO TO 20                     
      GO TO 30                                                                  
   10 SIG = - 1.0                                                               
   20 CALL RDNEXT (0)                                                           
   30 DO 40 J = 1, 10                                                           
      IF(ITEM .EQ. TABLE(J)) GO TO 50                                           
   40 CONTINUE                                                                  
      IF(ITEM .NE. PERIOD) GO TO 70                                             
      IF(JPL .NE. 0) GO TO 60                                                   
      JPL = 1                                                                   
      FF = 0.1                                                                  
      GO TO 20                                                                  
   50 IFLAG = 0                                                                 
      IF(JPL .NE. 0) GO TO 55                                                   
      VAL = 10.0 * VAL + FLOAT(J - 1)                                           
      GO TO 20                                                                  
   55 VAL = VAL + FF * FLOAT(J - 1)                                             
      FF = 0.1 * FF                                                             
      GO TO 20                                                                  
   60 IFLAG = 1                                                                 
      GO TO 90                                                                  
   70 IF(ITEM .NE. TABLE(15)) GO TO 80                                          
      CALL RDNEXT (0)                                                           
      CALL RDFIX (NEX, IFLAG)                                                   
      GO TO 90                                                                  
   80 IF(ITEM .EQ. PLUS(1) .OR. ITEM .EQ. PLUS(2) .OR. ITEM .EQ. MINUS)         
     A       CALL RDFIX (NEX, IFLAG)                                            
   90 VALUE = SIGN (VAL, SIG)                                                   
      IF(NEX .NE. 0) VALUE = VALUE * 10.0 ** NEX                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE RDNEXT (ISKIP)                                                 
C                                                                               
C S/R RDNEXT       RETURNS NEXT NON-BLANK CHARACTER ON INPUT                    
C                                                                               
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      COMMON /BLOC42/ TABLE(36), PLUS(2), MINUS, BLANK, PERIOD,                 
     A                SEMI(5), QUOTE(6), PAR1(2), PAR2(2)                       
      INTEGER TABLE, PLUS, BLANK, PERIOD, SEMI, QUOTE, PAR1, PAR2               
      COMMON /BLOC43/ NCD, CARD(80), MC, EMPTY, ITEM                            
      INTEGER CARD                                                              
      LOGICAL EMPTY                                                             
      LOGICAL LAST                                                              
      DATA LAST / .FALSE. /                                                     
C                                                                               
      IF(LAST) STOP                                                             
      IF(ISKIP .LT. 0) GO TO 40                                                 
      IF(ISKIP .GE. 2) GO TO 20                                                 
      IF(EMPTY) GO TO 20                                                        
   10 IF(MC .LT. 80) GO TO 30                                                   
      IF(ISKIP .NE. 0) GO TO 20                                                 
      EMPTY = .TRUE.                                                            
      ITEM = BLANK                                                              
      RETURN                                                                    
   20 MC = 0                                                                    
      EMPTY = .FALSE.                                                           
      READ(NIN,100,END=50) CARD                                                 
      NCD = NCD + 1                                                             
   30 MC = MC + 1                                                               
      ITEM = CARD(MC)                                                           
      IF(ISKIP .EQ. 0) RETURN                                                   
   40 IF(ITEM .EQ. BLANK) GO TO 10                                              
      IF(ITEM .EQ. TABLE(29) .AND. ISKIP .EQ. 2) STOP                           
      RETURN                                                                    
   50 IF(ISKIP .GE. 2) STOP                                                     
      LAST = .TRUE.                                                             
      ITEM = TABLE(29)                                                          
      RETURN                                                                    
C                                                                               
  100 FORMAT(80A1)                                                              
      END                                                                       
      SUBROUTINE RDSKIP                                                         
C S/R RDSKIP       SKIPS COMMENTS ON INPUT                                      
C                                                                               
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      COMMON /BLOC42/ TABLE(36), PLUS(2), MINUS, BLANK, PERIOD,                 
     A                SEMI(5), QUOTE(6), PAR1(2), PAR2(2)                       
      INTEGER TABLE, PLUS, BLANK, PERIOD, SEMI, QUOTE, PAR1, PAR2               
      COMMON /BLOC43/ NCD, CARD(80), MC, EMPTY, ITEM                            
      INTEGER CARD                                                              
      LOGICAL EMPTY                                                             
      INTEGER LINE(20)                                                          
C                                                                               
   10 IF(ITEM .NE. PAR1(1) .AND. ITEM .NE. PAR1(2)) RETURN                      
      CALL RDSTRG (LINE, 20, L)                                                 
      IF(L .GT. 20) WRITE (NOUT, 100)                                           
      WRITE (NOUT, 110) LINE                                                    
      GO TO 10                                                                  
  100 FORMAT(36H0NEXT COMMENT TRUNCATED TO 80 CHARS.)                           
  110 FORMAT(2H0(,20A4,1H))                                                     
      END                                                                       
      SUBROUTINE RDSTRG (STRING, LMAX, L)                                       
C S/R RDSTRG       READS AN ALPHANUMERIC STRING                                 
C                                                                               
      COMMON /BLOC42/ TABLE(36), PLUS(2), MINUS, BLANK, PERIOD,                 
     A                SEMI(5), QUOTE(6), PAR1(2), PAR2(2)                       
      INTEGER TABLE, PLUS, BLANK, PERIOD, SEMI, QUOTE, PAR1, PAR2               
      COMMON /BLOC43/ NCD, CARD(80), MC, EMPTY, ITEM                            
      INTEGER CARD                                                              
      LOGICAL EMPTY                                                             
      INTEGER STRING(LMAX), CHAR(99), ICPASS(9)
C                                                                               
      DO 10 L = 1, LMAX                                                         
   10 STRING(L) = BLANK                                                         
      L = 0                                                                     
      DO 12 J = 1, 2                                                            
      IF(ITEM .EQ. PAR1(J)) GO TO 20                                            
   12 CONTINUE                                                                  
      DO 14 J = 1, 6                                                            
      IF(ITEM .EQ. QUOTE(J)) GO TO 16                                           
   14 CONTINUE                                                                  
      RETURN                                                                    
   16 ISTOP = QUOTE(J)                                                          
      GO TO 30                                                                  
   20 ISTOP = PAR2(J)                                                           
   30 ISKIP = 1                                                                 
   40 IC = 0                                                                    
   50 CALL RDNEXT (ISKIP)                                                       
      IF(ITEM .EQ. ISTOP) GO TO 60                                              
      ISKIP = 0                                                                 
      IF(ITEM .EQ. BLANK .OR. LMAX .EQ. 1) ISKIP = 1                            
      IC = IC + 1                                                               
      CHAR(IC) = ITEM                                                           
      IF(IC - 4) 50, 70, 70                                                     
   60 IF(IC .EQ. 0) GO TO 80                                                    
   70 L = L + 1                                                                 
      ICPASS(1) = IC
      IF(L .LE. LMAX) CALL UBUNCH (CHAR, STRING(L), ICPASS)
      IF(ITEM .NE. ISTOP) GO TO 40                                              
   80 CALL RDNEXT (1)                                                           
      RETURN                                                                    
      END                                                                       
      SUBROUTINE REPETE                                                         
C                                                                               
C S/R REPETE       KEEPS TRACK OF REPETE ELEMENTS                               
C                                                                               
      COMMON /BLOC1/ DATA(3001), I, I1                                          
      INTEGER IDATA(3001)                                                       
      EQUIVALENCE (IDATA(1), DATA(1))                                           
      COMMON /BLOC17/ IP, IC(4), IS(4)                                          
C                                                                               
      IF (IDATA(I+1) .NE. 0) GO TO 1                                            
      IC(IP)  =  IC(IP)  -  1                                                   
      IF(  IC(IP)  .EQ.  0   )    GO TO   2                                     
      I  =  IS(IP)                                                              
      RETURN                                                                    
    2 IP  =  IP-1                                                               
      RETURN                                                                    
    1 IP  =  IP+1                                                               
      IC(IP) = IDATA(I+1)                                                       
      IS(IP) = I                                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE RESET (A,B)                                                    
C                                                                               
C S/R RESET        COPIES A VECTOR                                              
C                                                                               
      DIMENSION A(6), B(6)                                                      
C                                                                               
      DO 100 I = 1, 4                                                           
 100  A(I) = B(I)                                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SCATT (RAY, P, P0, AMPL)                                       
C                                                                               
C S/R SCATT        APPLIES MULTIPLE SCATTERING                                  
C                                                                               
C PARAMETERS...    RAY(1)    X  IN METERS                                       
C                  RAY(2)    X' IN RADIANS                                      
C                  RAY(3)    Y IN METERS                                        
C                  RAY(4)    Y' IN RADIANS                                      
C                  RAY(5)    NOT USED                                           
C                  RAY(6)    (P - P0) / P, DIMENSIONLESS                        
C                  P         MOMENTUM OF PARTICLE IN GEV/C                      
C                  P0        REFERENCE MOMENTUM OF BEAM LINE IN GEV/C           
C                  AMPL(1)   SIGMA(X)                                           
C                  AMPL(2)   SIGMA(X')                                          
C                  AMPL(3)   SIGMA(Y)                                           
C                  AMPL(4)   SIGMA(Y')                                          
C                  AMPL(5)   NOT USED                                           
C                  AMPL(6)   SIGMA(DP/P)                                        
C                  AMPL(7)   ENERGY LOSS IN GEV/C (NEGATIVE IF LOSS)            
C                                                                               
      REAL RAY(6), AMPL(7), VAL(4)                                              
      REAL*8  RANF
C                                                                               
   10 DO 20 IR = 1, 4                                                           
C      VAL(IR) = 2.0 * (RANF(IR) - 1.0)                                         
* CORREDCTION TO GET RMS HALF WIDTH OF 1                                        
      VAL(IR) = 2.0 * (RANF(IR) - 0.5)                                          
*                                                                               
   20 CONTINUE                                                                  
      RN1 = VAL(1)**2 + VAL(3)**2                                               
      RN2 = VAL(2)**2 + VAL(4)**2                                               
      IF(RN1 .GT. 1.0 .OR. RN2 .GT. 1.0) GO TO 10                               
      DO 30 IR = 1, 4                                                           
C      RAY(IR) = RAY(IR) + AMPL(IR) * VAL(IR)                                   
* CORRECT TO GET RMS DISTRIBUTION HALF WIDTH 1                                  
      RAY(IR) = RAY(IR) + 2.0 * AMPL(IR) * VAL(IR)                              
*                                                                               
   30 CONTINUE                                                                  
      P = P + P0 * AMPL(6) * 2.0 * (RANF(-1) - 1.0) + AMPL(7)                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SETUP                                                          
C                                                                               
C S/R SETUP        READS INPUT AND PROCESSES DATA FOR TRACKING                  
C                                                                               
      COMMON /BLOC1/ DATA(3001), I, I1                                          
      INTEGER IDATA(3001)                                                       
      EQUIVALENCE (IDATA(1), DATA(1))
      REAL*8 UNIT, NAME
      COMMON /BLOC2/ UNIT(11), NAME(11)                                         
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      COMMON /BLOC4/ RAYMAX(6), PBEAM, P0, NORAYS                               
      COMMON /BLOC5/ SEC, CAP(3), MPA                                           
      LOGICAL SEC, CAP, MPA                                                     
      COMMON /BLOC6/ NHISTS, NHIST, LIMNO                                       
      COMMON /BLOC7/ MTYPE(100), NENTRY(100), NCOOR(100), INTVLS(100),          
     A               BEGIN(100), END(100), STEP(100), ZHIST(100)                
      COMMON /BLOC8/ MTABLE(1000000)                                              
      COMMON /BLOC9/ L, H, N, BDB                                               
      REAL L, N                                                                 
      COMMON /BLOC10/ APB(2), LAYL, LAYX, RAB1, RAB2, FACE                      
      REAL LAYL, LAYX                                                           
      COMMON /BLOC11/ QAP(3), ELLIPS, HYPER                                     
      LOGICAL ELLIPS, HYPER                                                     
      COMMON /BLOC12/ VALUE(100), XMOM(100), XMOM2(100), NFLAG(100),            
     A                SET(100), KFLAG(100), IFLAG(3,9), ZFLAG(9)                
      LOGICAL SET, IFLAG                                                        
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      COMMON /BLOC16/ KPART(3), ISTOP(3,500)                                    
      COMMON /BLOC18/ LOSS, NBLK(10), KFLG(10),                                 
     A                XSTRT(10), XEND(10), LTHX(10), XBIN(10), NUNX(10),        
     B                YSTRT(10), YEND(10), LTHY(10), YBIN(10), NUNY(10)         
      COMMON /BLOC41/ LW, IMAGE(20), FLUSH, INDIC, NTYPE, LABLE, LENGTH,        
     A                NWORD, NVARY, DATUM(30), VARY(30)                         
      INTEGER VARY, TEXT(30)                                                    
      LOGICAL FLUSH                                                             
      EQUIVALENCE (TEXT(1), DATUM(1))                                           
      INTEGER TYPE                                                              
      REAL    CTABLE(14), UORIG(11), ZL(4), WORK(30)                            
      INTEGER UTABLE(14), ICC(4), NORIG(11)                                     
      EQUIVALENCE (TYPEC, WORK(1), DATUM(1))                                    
      EQUIVALENCE (TYPE, NTYPE), (NFL, VARY(2))                                 
C                                                                               
      DATA CTABLE     / 1.,100.,2.54,30.48,.1,1000., 1.,1.,.1,                  
     1 100.,.001, 1. ,1., .001  /                                               
      DATA UTABLE / 2HCM, 1HM, 2HIN, 2HFT, 2HMM, 1HR, 2HMR, 2HPC,               
     X              4HP/10, 1HN, 3HMEV, 3HGEV, 2HKG, 1HG /                      
      DATA NORIG / 4HCM  , 4HMR  , 4HCM  , 4HMR  , 4HCM  , 4HPC  ,              
     A             4HDEG , 4HM   , 4HKG  , 4HEM  , 4HGEVC /                     
      DATA UORIG / 0.01, 0.001, 0.01, 0.001, 0.01, 0.01, 0.01745329,            
     A             1.0, 1.0, 1.0, 1.0 /                                         
      DATA IRAYS / 4HRAYS /                                                     
      DATA IFACE /-10/, INEXT /-10/                                             
C                                                                               
C     READ TITLE                                                                
C                                                                               
      IMAX = 3001                                                               
      LMAX = 10000000                                                              
   10 CALL RDNEXT (2)                                                           
      CALL RDSKIP                                                               
      CALL RDSTRG (IMAGE, 20, LW)                                               
      WRITE (NOUT, 9000) IMAGE                                                  
      CALL RDSKIP                                                               
      CALL RDFIX (NORAYS, IFLG)                                                 
      CALL RDNEXT (- 1)                                                         
      WRITE (NOUT, 9005) NORAYS                                                 
      INDIC = 0                                                                 
 9000 FORMAT(2H1",20A4,1H")                                                     
 9005 FORMAT(1H0,I8)                                                            
C                                                                               
C     INITIALIZE                                                                
C                                                                               
      DO 50 ISBK = 1, IMAX                                                      
   50 DATA(ISBK) = 0.0                                                          
      I = 1                                                                     
      NHISTS = 0                                                                
      LIMNO = 0                                                                 
      ZC = 0.0                                                                  
      DKFLAG = .FALSE.                                                          
      LOSS = 0                                                                  
      DO 55 J = 1, 3                                                            
   55 KPART(J) = IRAYS                                                          
      ZMAX = 0.0                                                                
      IP = 0                                                                    
      NEL = 0                                                                   
      FLUSH = .FALSE.                                                           
      DO 60 J = 1, 11                                                           
      NAME(J) = NORIG(J)                                                        
   60 UNIT(J) = UORIG(J)                                                        
C                                                                               
C     MAIN LOOP FOR READING                                                     
C                                                                               
  100 CALL RDELMT                                                               
      IF(NWORD .EQ. 0) GO TO 600                                                
      IF(FLUSH) GO TO 100                                                       
      NEL = NEL + 1                                                             
      LEN = NU(TYPE)                                                            
      IF(I + LEN .LE. IMAX) GO TO 200                                           
      WRITE(NOUT, 1000) NEL, I                                                  
 1000 FORMAT(21H0DATA OVERFLOW, NEL =,I4,6H   I =,I6)                           
      FLUSH = .TRUE.                                                            
      GO TO 100                                                                 
  200 IF(TYPEC .LT. 0.0) GO TO 100                                              
      IF(TYPE .EQ. 60) GO TO 550                                                
      IF(TYPE .EQ. 53) GO TO 480                                                
      IF (TYPE .GE. 50) GO TO 450                                               
      GO TO (210,220,230,240,250,260,270,280,290,300,310,320,330,340,           
     A 350,360,370,380,390,400,410), TYPE                                       
C                                                                               
C     BEAM                                                                      
C                                                                               
  210 IF (NWORD .GE. 9) GO TO 212                                               
      PBEAM = WORK(8) * UNIT(11)                                                
      P0 = PBEAM                                                                
      DO 211 IB = 1, 6                                                          
  211 RAYMAX(IB) = WORK(IB+1)*UNIT(IB)                                          
      GO TO 100                                                                 
  212 IDATA(I) = TYPE                                                           
      DO 213 IB = 1, 6                                                          
      IIB = I + IB                                                              
      DATA(IIB) = WORK(IB+1) * UNIT(IB)                                         
  213 CONTINUE                                                                  
      DATA(I+7) = WORK(8)*UNIT(11)                                              
      IDATA(I+8) = WORK(9) / 100. + .001                                        
      GO TO 500                                                                 
C                                                                               
C     MAGNET POLE FACE ROTATION                                                 
C                                                                               
  220 IDATA(I) = TYPE                                                           
      FACE = WORK(2) * UNIT(7)                                                  
      TANB = SIN(FACE) / COS(FACE)                                              
      SECB = 1.0 / COS(FACE)                                                    
      TCOR = 2.0 * APB(2) * LAYL                                                
      SB = SECB * (1.0 + 2.0 * TANB ** 2)                                       
      IF (I .EQ. INEXT) GO TO 221                                               
      IFACE = I                                                                 
      GO TO 500                                                                 
C                                                                               
C     FRINGE FIELD AT MAGNET EXIT.                                              
C                                                                               
  221 DATA(I+1) = - H * P0                                                      
      DATA(I+2) = - TANB                                                        
      DATA(I+3) = SECB                                                          
      DATA(I+4) = N * H * TANB - 0.5 * RAB2 * SECB ** 3                         
      DATA(I+5) = TCOR * SB * (1.0 - H * LAYX * TCOR * TANB)                    
      DATA(I+6) = - 0.5 * TANB ** 3                                             
      DATA(I+7) = DATA(I+6)                                                     
      DATA(I+8) = - TANB * SECB ** 2                                            
      INEXT = - 10                                                              
      GO TO 500                                                                 
C                                                                               
C     DRIFT SPACE                                                               
C                                                                               
  230 IDATA(I) = TYPE                                                           
      L = WORK(2)*UNIT(8)                                                       
      DATA(I+1) = L                                                             
      GO TO 490                                                                 
C                                                                               
C     BENDING MAGNET                                                            
C                                                                               
  240 IDATA(I) = TYPE                                                           
      L = WORK(2)*UNIT(8)                                                       
      H = WORK(3)*UNIT(9)/(33.356*P0)                                           
      N = WORK(4)                                                               
      IF(I .NE. IFACE + NU(2)) GO TO 245                                        
C                                                                               
C     FRINGE FIELD AT MAGNET ENTRANCE.                                          
C                                                                               
      DATA(IFACE+1) = H * P0                                                    
      DATA(IFACE+2) = TANB                                                      
      DATA(IFACE+3) = SECB                                                      
      DATA(IFACE+4) = 0.5 * RAB1 * SECB ** 3 - N * H * TANB                     
      DATA(IFACE+5) = TCOR * SB * (1.0 - H * LAYX * TCOR * TANB)                
      DATA(IFACE+6) = 0.0                                                       
      DATA(IFACE+7) = TANB * (0.5 + TANB ** 2)                                  
      DATA(IFACE+8) = 0.0                                                       
  245 DATA(I+1) = L                                                             
      DATA(I+2) = H                                                             
      DATA(I+3) = N                                                             
      DATA(I+4) = 0.0                                                           
      DATA(I+5) = 0.0                                                           
      INEXT = I + NU(4)                                                         
      IF(DATA(I+2) .EQ. 0.0) GO TO 490                                          
      DATA(I+4) = 1.0 / DATA(I+2)                                               
      IF(SEC) DATA(I+5) = BDB * DATA(I+4) ** 2                                  
      GO TO 490                                                                 
C                                                                               
C     QUADRUPOLE                                                                
C                                                                               
  250 IDATA(I) = TYPE                                                           
      L = WORK(2)*UNIT(8)                                                       
      DATA(I+1) = L                                                             
      DATA(I+2) = WORK(3)*UNIT(9)/(WORK(4)*UNIT(1)*33.356)                      
      DATA(I+3) = (WORK(4) * UNIT(1)) ** 2                                      
      GO TO 490                                                                 
C                                                                               
C     SLIT                                                                      
C                                                                               
  260 IDATA(I) = TYPE                                                           
      IFF = WORK(2) + .001                                                      
      IDATA(I+1) = IFF                                                          
      IUN = MOD(IFF,100)                                                        
      IF(IUN .LE. 0 .OR. IUN .GT. 6) GO TO 100                                  
      DATA(I+2) = WORK(3) * UNIT(IUN)                                           
      IFG = WORK(4) + .001                                                      
      IUN = MOD(IFG, 100)                                                       
      data(i+3)=0.0                                                             
      data(i+4)=0.0                                                             
      data(i+5)=0.0                                                             
      data(i+6)=0.0                                                             
      IF(IUN .LT. 0 .OR. IUN .GT. 6) GO TO 100                                  
      IDATA(I+3) = IUN                                                          
      DATA(I+4) = 0.0                                                           
      IF(IUN .NE. 0) DATA(I+4) = WORK(5) * UNIT(IUN)                            
      if (work(6).eq.0.0) goto 500                                              
        data (i+5)=work(6)*unit(iun)                                            
        data (i+6)=work(7)*unit(iun)                                            
      GO TO 500                                                                 
C                                                                               
C     DISPLACEMENT OF RAY                                                       
C                                                                               
  270 IDATA(I) = TYPE                                                           
      DO 271 IB = 1, 6                                                          
      IIB = I + IB                                                              
      DATA(IIB) = WORK(IB+1) * UNIT(IB)                                         
  271 CONTINUE                                                                  
      GO TO 500                                                                 
C                                                                               
C     MISALIGNMENTS                                                             
C                                                                               
  280 WRITE (NOUT,1001)                                                         
 1001 FORMAT(39H THIS PROGRAM WILL NOT DO MISALIGNMENTS)                        
      STOP                                                                      
C                                                                               
C      REPETER                                                                   
C                                                                               
  290 IDATA(I) = TYPE                                                           
      IDATA(I+1) = WORK(2) + .001                                               
      IF (IDATA(I+1) .EQ. 0) GO TO 291                                          
      IP = IP + 1                                                               
      ICC(IP) = IDATA(I+1)                                                      
      ZL(IP) = ZC                                                               
      GO TO 500                                                                 
  291 ZC = ZC + FLOAT(ICC(IP) - 1)*(ZC - ZL(IP))                                
      IP = IP - 1                                                               
      GO TO 500                                                                 
C                                                                               
C     CONSTRAINTS                                                               
C                                                                               
  300 GO TO 100                                                                 
C                                                                               
C     ACCELERATOR                                                               
C                                                                               
  310 GO TO 100                                                                 
C                                                                               
C     BEAM ROTATED ELLIPSE                                                      
C                                                                               
  320 GO TO 100                                                                 
C                                                                               
C     INPUT OUTPUT OPTIONS                                                      
C                                                                               
  330 KODE = WORK(2) + .001                                                     
      KFL = KODE / 100                                                          
      INDEX = KODE - 100 * KFL                                                  
      IF(INDEX .NE. 10) GO TO 100                                               
      CAP(KFL+1) = .TRUE.                                                       
      GO TO 100                                                                 
C                                                                               
C     ARBITRARY MATRIX                                                          
C                                                                               
  340 IDATA(I) = TYPE                                                           
      NROW = WORK(8) + .001                                                     
      DO 341 IB = 1, 6                                                          
      IIB = I + IB                                                              
      DATA(IIB) = WORK(IB+1) * UNIT(NROW) / UNIT(IB)                            
  341 CONTINUE                                                                  
      IDATA(I+7) = NROW                                                         
      IF (SEC .AND. NWORD .GT. 8) GO TO 342                                     
      IDATA(I+8) = 1                                                            
      GO TO 500                                                                 
  342 IDATA(I+8) = 0                                                            
      IPL = 9                                                                   
      DO 343 IB = 1, 6                                                          
      DO 343 IC = IB, 6                                                         
      IPL = IPL + 1                                                             
      IST = IPL + I - 1                                                         
  343 DATA(IST) = WORK(IPL)*UNIT(NROW)/(UNIT(IB)*UNIT(IC))                      
      GO TO 500                                                                 
C                                                                               
C     UNITS                                                                     
C                                                                               
  350 NUN = WORK(2) + .001                                                      
      IF(NUN .EQ. 0) GO TO 355                                                  
      NAME(NUN) = TEXT(3)                                                       
      UNIT(NUN) = WORK(4) * UORIG(NUN)                                          
      IF(NUN .GT. 2) GO TO 100                                                  
      UNIT(NUN+2) = UNIT(NUN)                                                   
      NAME(NUN+2) = NAME(NUN)                                                   
      GO TO 100                                                                 
  355 DO 356 J = 1, 11                                                          
      NAME(J) = NORIG(J)                                                        
  356 UNIT(J) = UORIG(J)                                                        
      GO TO 100                                                                 
C                                                                               
C     MULTIPOLE ABERRATIONS IN QUADS AND BENDING MAGNET PARAMETERS              
C                                                                               
  360 IDATA(I) = TYPE                                                           
      NM = ABS(WORK(2)) + .001                                                  
      IF (WORK(2) .GT. 0.0) GO TO 361                                           
      IF (NM .LE. 20) GO TO 3601                                                
      WRITE (NOUT,1010)                                                         
 1010 FORMAT(37H INDEX OF MULTIPOLE MAY NOT EXCEED 20)                          
      STOP                                                                      
 3601 MPA = .TRUE.                                                              
      IDATA(I+1) = - NM                                                         
      DATA(I+2) = WORK(3)/UNIT(1)**(NM-2)                                       
      ANGLE = WORK(4)/57.29578                                                  
      DATA(I+3) = COS(ANGLE)                                                    
      DATA(I+4) = SIN(ANGLE)                                                    
      GO TO 500                                                                 
  361 IF (NM .EQ. 1) GO TO 362                                                  
      IF (NM .EQ. 4 .OR. NM .EQ. 5) GO TO 363                                   
      IF (NM .EQ. 6) GO TO 3635                                                 
      IF (NM .EQ. 7) GO TO 364                                                  
      IF (NM .EQ. 8) GO TO 365                                                  
      IF (NM .EQ. 9) GO TO 3655                                                 
      IF (NM .EQ. 11) GO TO 3657                                                
      IF (NM .EQ. 12) GO TO 366                                                 
      IF (NM .EQ. 13) GO TO 367                                                 
      IF (NM .EQ. 100) GO TO 368                                                
      IF(NM .EQ. 101) GO TO 369                                                 
      IF(NM .GE. 200) GO TO 3695                                                
      GO TO 100                                                                 
  362 BDB = WORK(3)/UNIT(1)**2                                                  
      GO TO 100                                                                 
  363 APB(NM-3) = WORK(3)*UNIT(2*NM-7)                                          
      IDATA(I+1) = NM                                                           
      DATA(I+2) = APB(NM-3)                                                     
      GO TO 500                                                                 
 3635 ZC = WORK(3) * UNIT(8)                                                    
      GO TO 100                                                                 
  364 LAYL = WORK(3)                                                            
      GO TO 100                                                                 
  365 LAYX = WORK(3)                                                            
      GO TO 100                                                                 
 3655 ZMAX = WORK(3) * UNIT(8)                                                  
      GO TO 100                                                                 
 3657 P0 = WORK(3) * UNIT(11)                                                   
      IDATA(I+1) = NM                                                           
      DATA(I+2) = P0                                                            
      GO TO 500                                                                 
  366 RAB1 = WORK(3)/UNIT(8)                                                    
      GO TO 100                                                                 
  367 RAB2 = WORK(3)/UNIT(8)                                                    
      GO TO 100                                                                 
  368 IDATA(I+1) = NM                                                           
      DATA(I+2) = WORK(3)*UNIT(1)                                               
      DATA(I+3) = WORK(4)*UNIT(3)                                               
      GO TO 500                                                                 
  369 IDATA(I+1) = NM                                                           
      DATA(I+2) = 0.5 * (WORK(3) * UNIT(1)) ** 2                                
      GO TO 500                                                                 
 3695 KPART(NM-199) = LABLE                                                     
      GO TO 100                                                                 
C                                                                               
C     SECOND ORDER                                                              
C                                                                               
  370 SEC = .TRUE.                                                              
      GO TO 500                                                                 
C                                                                               
C     SEXTUPOLE                                                                 
C                                                                               
  380 IDATA(I) = TYPE                                                           
      L = WORK(2)*UNIT(8)                                                       
      DATA(I+1) = L                                                             
      DATA(I+2) = WORK(3)*UNIT(9)/((WORK(4)*UNIT(1))**2*33.356)                 
      GO TO 490                                                                 
C                                                                               
C     SOLENOID                                                                  
C                                                                               
  390 IDATA(I) = TYPE                                                           
      L = WORK(2) * UNIT(8)                                                     
      DATA(I+1) = L                                                             
      DATA(I+2) = 0.5 * WORK(3) * UNIT(9) / 33.356                              
      GO TO 490                                                                 
C                                                                               
C     BEAM ROTATION                                                             
C                                                                               
  400 IDATA(I) = TYPE                                                           
      ANGLE = WORK(2) * UNIT(7)                                                 
      DATA(I+1) = COS(ANGLE)                                                    
      DATA(I+2) = SIN(ANGLE)                                                    
      GO TO 500                                                                 
C                                                                               
C     STRAY FIELD                                                               
C                                                                               
  410 GO TO 100                                                                 
C                                                                               
C     SET UP HISTOGRAM                                                          
C                                                                               
  450 KODE = WORK(2) + .001                                                     
      KFL = KODE / 100                                                          
      NUN = KODE - 100 * KFL                                                    
      CALL HSTUN (NUN, NMVAL, NMUN, FACT)                                       
      M = LIMNO + 1                                                             
      WORK(3) = WORK(3) * FACT                                                  
      WORK(4) = WORK(4) * FACT                                                  
      WORK(5) = WORK(5) * FACT                                                  
      IF(WORK(5) .EQ. 0.0) WORK(5) = (WORK(4) - WORK(3)) / 100.0                
      IF(WORK(5) .EQ. 0.0) WORK(5) = 0.001                                      
      INTVL = AMAX1((WORK(4) - WORK(3)) / WORK(5), 1.0) + 0.001                 
      NINNY = TYPE - 49                                                         
      GO TO (452, 454, 456), NINNY                                              
C                                                                               
C     ONE DIMENSIONAL HISTOGRAM                                                 
C                                                                               
  452 LIM = LIMNO + INTVL + 3                                                   
      IF(LIM .GT. LMAX) GO TO 470                                               
      LIMNO = LIM                                                               
      IF(NUN .NE. 8) GO TO 460                                                  
C                                                                               
C     Z HISTOGRAM OF LOSSES                                                     
C                                                                               
      LOSS = LOSS + 1                                                           
      XSTRT(LOSS) = WORK(3)                                                     
      XEND (LOSS) = WORK(4)                                                     
      LTHX (LOSS) = INTVL                                                       
      XBIN (LOSS) = WORK(5)                                                     
      NUNX (LOSS) = 8                                                           
      NUNY (LOSS) = 0                                                           
      NBLK (LOSS) = M                                                           
      KFLG (LOSS) = KFL                                                         
      GO TO 100                                                                 
C                                                                               
C     SET X SCALE (HORIZONTAL)                                                  
C                                                                               
  454 ASTRT = WORK(3)                                                           
      AEND  = WORK(4)                                                           
      NA    = MIN0 (INTVL, 100)                                                 
      DA    = (WORK(4) - WORK(3)) / FLOAT(NA)                                   
      NUNA = NUN                                                                
      IF(NUN - 8) 460, 100, 460                                                 
C                                                                               
C     Y SCALE (VERTICAL)                                                        
C                                                                               
  456 LIM = LIMNO + (NA + 1) * (INTVL + 1) + 5                                  
      IF(LIM .GT. LMAX) GO TO 470                                               
      LIMNO = LIM                                                               
      IF(NUN .NE. 8 .AND. NUNA .NE. 8) GO TO 460                                
C                                                                               
C     TWO DIMENSIONAL LOSS HISTOGRAM                                            
C                                                                               
      LOSS = LOSS + 1                                                           
      XSTRT(LOSS) = ASTRT                                                       
      XEND (LOSS) = AEND                                                        
      LTHX (LOSS) = NA                                                          
      XBIN (LOSS) = DA                                                          
      NUNX (LOSS) = NUNA                                                        
      YSTRT(LOSS) = WORK(3)                                                     
      YEND (LOSS) = WORK(4)                                                     
      LTHY (LOSS) = INTVL                                                       
      YBIN (LOSS) = WORK(5)                                                     
      NUNY (LOSS) = NUN                                                         
      NBLK (LOSS) = M                                                           
      KFLG (LOSS) = KFL                                                         
      GO TO 100                                                                 
C                                                                               
C     STORE HISTOGRAM DATA TO DATA ARRAY                                        
C                                                                               
  460 IDATA(I) = 50                                                             
      NHISTS = NHISTS + 1                                                       
      MTYPE (NHISTS) = NINNY                                                    
      NENTRY(NHISTS) = M                                                        
      NCOOR (NHISTS) = NUN                                                      
      BEGIN (NHISTS) = WORK(3)                                                  
      END   (NHISTS) = WORK(4)                                                  
      STEP  (NHISTS) = WORK(5)                                                  
      INTVLS(NHISTS) = INTVL                                                    
      ZHIST (NHISTS) = ZC                                                       
      NFLAG (NHISTS) = NFL                                                      
      KFLAG (NHISTS) = KFL                                                      
      GO TO 500                                                                 
C                                                                               
C     HISTOGRAM SPACE OVERFLOW                                                  
C                                                                               
  470 WRITE (NOUT, 1470)                                                        
      GO TO 100                                                                 
 1470 FORMAT(48H0THE HISTOGRAM SPECIFIED ABOVE CAUSED A STORAGE ,               
     A       29HOVERFLOW, IT HAS BEEN IGNORED)                                  
C                                                                               
C     FLAG FOR HISTOGRAM ENTRY                                                  
C                                                                               
  480 IDATA(I) = TYPE                                                           
      IDATA(I+1) = NFL                                                          
      ZFLAG(NFL) = ZC                                                           
      GO TO 500                                                                 
  490 ZC = ZC + L                                                               
  500 I = I + NU(TYPE)                                                          
      GO TO 100                                                                 
C                                                                               
C     PREPARE FOR DECAY CALCULATION                                             
C                                                                               
  550 DKFLAG = .TRUE.                                                           
      SEC = .TRUE.                                                              
      CALL DECAYP                                                               
      GO TO 100                                                                 
C                                                                               
C     END OF INPUT DATA                                                         
C                                                                               
  600 IF(FLUSH) GO TO 10                                                        
      I1 = I - 1                                                                
      WRITE (NOUT, 1103)                                                        
 1103 FORMAT(9H0SENTINEL)                                                       
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SEXT                                                           
C                                                                               
C S/R SEXT         TRACKS CHARGED PARTICLES THRU A SEXTUPOLE                    
C                                                                               
      COMMON /BLOC1/ DATA(3001), I, I1                                          
      INTEGER IDATA(3001)                                                       
      EQUIVALENCE (IDATA(1), DATA(1))                                           
      COMMON /BLOC5/ SEC, CAP(3), MPA                                           
      LOGICAL SEC, CAP, MPA                                                     
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      COMMON /BLOC15/ LL, KH2, KV2, KH, KV, KHL, KVL, RAY2(6),                  
     A                RH(4), RV(4)                                              
      REAL LL, KH2, KV2, KH, KV, KHL, KVL                                       
C                                                                               
      RAY2(1) = RAY(1) + LL * RAY(2)                                            
      RAY2(2) = RAY(2)                                                          
      RAY2(3) = RAY(3) + LL * RAY(4)                                            
      RAY2(4) = RAY(4)                                                          
      IF(.NOT. SEC) GO TO 10                                                    
      C = LL * DATA(I+2) / P                                                    
      U = RAY(1) ** 2 - RAY(3) ** 2                                             
      UP = LL * (RAY(1) * RAY(2) - RAY(3) * RAY(4))                             
      UPP = LL ** 2 * (RAY(2) ** 2 - RAY(4) ** 2)                               
      RAY2(1) = RAY2(1) - C * (0.5 * U + UP / 3.0 + UPP / 12.0) * LL            
      RAY2(2) = RAY2(2) - C * (U + UP + UPP / 3.0)                              
      U = RAY(1) * RAY(3)                                                       
      UP = LL * (RAY(1) * RAY(4) + RAY(2) * RAY(3))                             
      UPP = LL ** 2 * RAY(2) * RAY(4)                                           
      RAY2(3) = RAY2(3) + C * (U + UP / 3.0 + UPP / 6.0) * LL                   
      RAY2(4) = RAY2(4) + C * (U + U + UP + UPP / 1.5)                          
   10 CALL RESET (RAY, RAY2)                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SOLO                                                           
C                                                                               
C S/R SOLO         TRACKS CHARGED PARTICLES THRU A SOLENOID                     
C                                                                               
      COMMON /BLOC1/ DATA(3001), I, I1                                          
      INTEGER IDATA(3001)                                                       
      EQUIVALENCE (IDATA(1), DATA(1))                                           
      COMMON /BLOC13/ RAY(6), RAYC(6), RAYN(6), P, PC, PN, FLD, MASS,           
     A                MASSC, MASSN, FC, FN, PSTAR, ZMAX, DKFLAG, Z              
      REAL MASS, MASSC, MASSN                                                   
      LOGICAL DKFLAG                                                            
      COMMON /BLOC15/ LL, KH2, KV2, KH, KV, KHL, KVL, RAY2(6),                  
     A                RH(4), RV(4)                                              
      REAL LL, KH2, KV2, KH, KV, KHL, KVL                                       
C                                                                               
      KH = DATA(I+2) / P                                                        
      KHL = LL * KH                                                             
      C = COS(KHL)                                                              
      S = SIN(KHL)                                                              
      IF(KHL .LT. 0.01) GO TO 10                                                
      SOK = S / KH                                                              
      GO TO 20                                                                  
   10 SOK = LL * (1.0 - (KH * LL) ** 2 / 6.0)                                   
   20 SK = S * KH                                                               
      RAY2(1) = C * RAY(1) + SOK * RAY(2)                                       
      RAY2(2) = - SK * RAY(1) + C * RAY(2)                                      
      RAY2(3) = C * RAY(3) + SOK * RAY(4)                                       
      RAY2(4) = - SK * RAY(3) + C * RAY(4)                                      
      RAY(1) = C * RAY2(1) + S * RAY2(3)                                        
      RAY(3) = - S * RAY2(1) + C * RAY2(3)                                      
      RAY(2) = C * RAY2(2) + S * RAY2(4)                                        
      RAY(4) = - S * RAY2(2) + C * RAY2(4)                                      
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TRHIST (NHST, M, A, D)                                         
C                                                                               
C S/R TRHIST       PRINTS A 1-D HISTOGRAM                                       
C                                                                               
      DIMENSION NHST(2)                                                         
      INTEGER NIN, NOUT
      COMMON /BLOC3/ NIN, NOUT
      DATA X / 1HX /                                                            
C                                                                               
      NSUM = NHST(M+3)                                                          
      MORE = M+1                                                                
      LIM = M+2                                                                 
      AA = A                                                                    
      IF(NSUM .NE. 0) GO TO 106                                                 
C                                                                               
C     UNFILLED HISTOGRAM                                                        
C                                                                               
      WRITE (NOUT, 9005)                                                        
 9005 FORMAT(5(1H0,/),22H0HISTOGRAM IS UNFILLED,5(/,1H0))                       
      RETURN                                                                    
C                                                                               
C     DETERMINATION OF SCALE                                                    
C                                                                               
  106 NSCALE = 0                                                                
      DO 190 I = 1, LIM                                                         
      IF(NHST(I) .GT. NSCALE) NSCALE = NHST(I)                                  
  190 CONTINUE                                                                  
      WRITE (NOUT, 9010) NSCALE                                                 
 9010 FORMAT(1H0,/11X,8HINTERVAL,30X,28HSCALE FACTOR.. 100 X'S EQUAL,           
     A       I6,8H ENTRIES)                                                     
C                                                                               
C     PLOTTING OF HISTOGRAM                                                     
C                                                                               
      NUM = (NHST(LIM) * 100) / NSCALE                                          
      IF(NUM .LE. 0) WRITE (NOUT,9020) AA, NHST(LIM)                            
      IF(NUM .GT. 0) WRITE (NOUT,9020) AA, NHST(LIM), (X, J = 1, NUM)           
 9020 FORMAT(13H0LESS THAN   ,F8.3,4X,I5,3X,100A1)                              
      WRITE (NOUT,9025)                                                         
 9025 FORMAT(1X)                                                                
      DO 300 I = 1,M                                                            
      AB =AA + D                                                                
      NUM = (NHST(I) * 100) / NSCALE                                            
      IF(NUM .LE. 0) WRITE (NOUT,9030) AA, AB, NHST(I)                          
      IF(NUM .GT. 0) WRITE (NOUT,9030) AA, AB, NHST(I), (X, J = 1, NUM)         
 9030 FORMAT(1X,F8.3,4H TO ,F8.3,4X,I5,3X,100A1)                                
      AA= AB                                                                    
  300 CONTINUE                                                                  
      NUM = (NHST(MORE) * 100) / NSCALE                                         
      IF(NUM .LE. 0) WRITE (NOUT,9040) AA, NHST(MORE)                           
      IF(NUM .GT. 0) WRITE (NOUT,9040) AA, NHST(MORE), (X, J = 1, NUM)          
 9040 FORMAT (13H0GREATER THAN ,F8.3,4X,I5,3X,100A1)                            
C                                                                               
C     SUMMARY                                                                   
C                                                                               
      WRITE (NOUT, 9050) NSUM                                                   
 9050 FORMAT(1H0,10X,25HTOTAL NUMBER OF ENTRIES =,I10,5X,                       
     A       32HINCLUDING OVERFLOW AND UNDERFLOW)                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE ZERO                                                           
C                                                                               
C S/R ZERO         CLEARS HISTOGRAM STORAGE                                     
C                                                                               
      COMMON /BLOC6/ NHISTS, NHIST, LIMNO                                       
      COMMON /BLOC8/ MTABLE(1000000)                                              
      COMMON /BLOC12/ VALUE(100), XMOM(100), XMOM2(100), NFLAG(100),            
     A                SET(100), KFLAG(100), IFLAG(3,9), ZFLAG(9)                
      LOGICAL SET, IFLAG                                                        
      COMMON /BLOC16/ KPART(3), ISTOP(3,500)                                    
C                                                                               
      DO 50 NH = 1, NHISTS                                                      
      XMOM(NH) = 0.0                                                            
   50 XMOM2(NH) = 0.0                                                           
      DO 100 KK = 1, LIMNO                                                      
  100 MTABLE(KK) = 0                                                            
      DO 150 K = 1, 500                                                         
      DO 150 L = 1, 3                                                           
  150 ISTOP(L,K) = 0                                                            
      RETURN                                                                    
      END                                                                       
      SUBROUTINE UBUNCH (MS,MT,NCHP)
C
C CERN PROGLIB# M409    UBUNCH          .VERSION KERNFOR  4.30  910819
C ORIG. 05/12/89, FCA+JZ
C
      DIMENSION    MS(99), MT(99), NCHP(9)
*               a word of all blanks
C-  - PARAMETER    (IALLBL = X'20202020')
      PARAMETER    (IALLBL =  538976288 )
C-  - PARAMETER    (MASK1  = X'FF000000')
c      PARAMETER    (MASK1  =  -16777216 )
      PARAMETER    (MASK1  =  255 )

      NCH = NCHP(1)
      IF   (NCH)             91,39,11

   11 NWT    = ISHFT (NCH,-2)
      NTRAIL = IAND (NCH,3)

      JS     = 0
      IF (NWT.EQ.0)          GO TO 31

C--                Pack the initial complete words


      DO 24  JT=1,NWT
      MT(JT) = IOR (IOR (IOR (
     +                  IAND(MS(JS+1),MASK1),
     +           ISHFT (IAND(MS(JS+2),MASK1), 8)),
     +           ISHFT (IAND(MS(JS+3),MASK1),16)),
     +           ISHFT      (MS(JS+4),       24) )
   24 JS = JS + 4


      IF (NTRAIL.EQ.0)       RETURN

C--                Pack the trailing word

   31 MWD = IALLBL
      JS  = NCH

      DO 34 JT=1,NTRAIL
      MWD = IOR (ISHFT(MWD,8), IAND(MS(JS),MASK1))
   34 JS  = JS - 1
      MT(NWT+1) = MWD
   39 RETURN

   91 CALL ABEND
      END
