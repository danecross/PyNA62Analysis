        REAL FUNCTION CLUSCELLT0(IX,IY)
C
C correct T0s of cell for trigger drift
C
      IMPLICIT NONE
      integer TRIGTYPE
      real  TRIGGERDRIFTT0
      real  CELLT0
      COMMON/ct0CDE/TRIGTYPE,TRIGGERDRIFTT0,CellT0(0:127,0:127)
C
C --------------------------------------------------------------
C
        INTEGER IX,IY
C
        CLUSCELLT0 = CELLT0(IX,IY)
        if (TRIGTYPE.EQ.16) CLUSCELLT0 = CELLT0(IX,IY) + TRIGGERDRIFTT0

        RETURN
        END
C
C ---------------------------------------------------------------------
C
