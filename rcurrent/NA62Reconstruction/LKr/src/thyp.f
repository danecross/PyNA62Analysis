        REAL FUNCTION THYP(X)
C
C hyperbolic tan function
C
        IMPLICIT NONE
        REAL X
        THYP=(EXP(X)-EXP(-1*X))/(EXP(X)+EXP(-1.*X))
        RETURN
        END
C
C --------------------------------------------------------------------
C
