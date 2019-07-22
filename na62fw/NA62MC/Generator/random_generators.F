C ... All the random generators point to the NA62MC generator

      SUBROUTINE RANLUX(RVEC,LENV)
      IMPLICIT NONE
      INTEGER LENV
      REAL*8 RVEC(LENV)
      CALL RANLUXCPP(LENV,RVEC)
      END

c ... used by GENBOD, GENBOX_FIX
      FUNCTION RNDM(ISEED)
      REAL*8 OUTPUT
      INTEGER ISEED
      CALL RNDMCPP(OUTPUT)
      RNDM = OUTPUT
      RETURN
      END

c ... used by RAMBOS
      SUBROUTINE RANMAR(RVEC,LENV)
      IMPLICIT NONE
      INTEGER LENV
      REAL*8 RVEC(LENV)
      CALL RANLUX(RVEC,LENV)
      RETURN
      END

c ... to be used by everything else
      FUNCTION RANF()
      IMPLICIT NONE
      REAL*8 RANF, OUTPUT
      CALL RNDMCPP(OUTPUT)
      RANF = OUTPUT
      RETURN
      END
