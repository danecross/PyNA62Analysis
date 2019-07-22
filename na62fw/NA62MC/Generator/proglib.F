c These are several functions from CERN PROGLIB used by some of the
c NA62 decay generators (mainly Kl4). They have been migrated from
c single to double precision to be compatible to the generators.
c E. Goudzovski, 18/7/2017. 

      FUNCTION VDOTN (X,Y,N)
C
C CERN PROGLIB# F121    VDOTN           .VERSION KERNFOR  2.09  751101
C ORIG. 01/07/71
C
      implicit none
      real*8 X(*), Y(*)
      real*8 vdotn, XX, XY, YY
      integer N,J
C
      IF (N.LE.0) GO TO 100
      XX= 0.
      XY= 0.
      YY= 0.
C
      DO 9 J=1,N
      XX = XX + X(J)*X(J)
      XY = XY + X(J)*Y(J)
    9 YY = YY + Y(J)*Y(J)
C
      VDOTN= XY / SQRT(XX*YY)
      IF (ABS (VDOTN).LT.1.) RETURN
      VDOTN= DSIGN (1.0d+1,VDOTN) ! the sign of vdotn, -1 or +1
      RETURN

  100 VDOTN = 0.
      RETURN
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      FUNCTION VMOD (A,N)
C
C CERN PROGLIB# F121    VMOD            .VERSION KERNFOR  1.0   710701
C ORIG. 01/07/71
C
      implicit none
      real*8 A(*)
      real*8 vmod, XX, YY
      integer I,N
C
      XX= 0.
      IF (N.LE.0)  GO TO 100
      DO 9 I= 1,N
    9 XX= XX + A(I)*A(I)
      IF (XX.LE.0.)  GO TO 100
      VMOD= SQRT (XX)
      RETURN
  100 VMOD = 0.
      RETURN
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE VUNIT (A,B,N)
C
C CERN PROGLIB# F121    VUNIT           .VERSION KERNFOR  1.0   710701
C ORIG. 01/07/71
C
      implicit none
      real*8 A(*),B(*)
      real*8 XX, YY
      integer i,n
C
      IF (N.LE.0)  RETURN
      XX= 0.
      DO 9 I=1,N
    9 XX= XX + A(I)*A(I)
      IF (XX.LE.0.)  RETURN
      YY=1./ SQRT (XX)
      DO 19 I= 1,N
   19 B(I)=A(I) * YY
      RETURN
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE VZERO (A,N)
C
C CERN PROGLIB# F121    VZERO           .VERSION KERNFOR  4.40  940929
C ORIG. 01/07/71, modif. 24/05/87 to set integer zero
C                 modif. 25/05/94 to depend on QINTZERO
C
      implicit none
      real*8 A(*)
      integer i,n
C
      IF (N.LE.0)  RETURN
      DO 9 I= 1,N
    9 A(I)= 0.
C
      RETURN
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE CROSS(X,Y,Z)
C
C CERN PROGLIB# F117    CROSS           .VERSION KERNFOR  2.02  730125
C ORIG. 01/01/64
C
      implicit none
      real*8 X(3),Y(3),Z(3),z1,z2

      Z1=X(2)*Y(3)-X(3)*Y(2)
      Z2=X(3)*Y(1)-X(1)*Y(3)
      Z(3)=X(1)*Y(2)-X(2)*Y(1)
      Z(1)=Z1
      Z(2)=Z2

      RETURN
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE LOREN4(DIR,P4IN,P4OUT)
C
C CERN PROGLIB# U101    LOREN4          .VERSION KERNFOR  4.07  830624
C ORIG. 20/02/68
C
      implicit none
      real*8 DIR(4),P4IN(4),P4OUT(4)
      real*8 pcm2, onmcm, prod, epbeta
      integer i
C
C--                VN(A) MEANS N-VECTOR A
C--                GAMMA=ECM/MCM
C--                EPBETA=ECM*V3(PCM)*V3(BETA)
C--                V3(BETA)=V3(PCM)/ECM
C
      PCM2=DIR(1)*DIR(1)+DIR(2)*DIR(2)+DIR(3)*DIR(3)
      ONMCM=1.D0/SQRT (DIR(4)*DIR(4)-PCM2)
      EPBETA=P4IN(1)*DIR(1)+P4IN(2)*DIR(2)+P4IN(3)*DIR(3)
      PROD=EPBETA*(DIR(4)*ONMCM-1.D0)/PCM2-P4IN(4)*ONMCM
      P4OUT(4)=ONMCM*(P4IN(4)*DIR(4)-EPBETA)
         DO 50 I=1,3
   50 P4OUT(I)=P4IN(I)+DIR(I)*PROD
      RETURN
      END
