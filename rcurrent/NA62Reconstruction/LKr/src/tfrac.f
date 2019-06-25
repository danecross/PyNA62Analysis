       real Function Tfrac(t)
c
c returns normalized pulse shape
c  t=0 corresponds to maximum of pulse
c
c uses pulse shape from Title bank for now
c in the same way as NMC (code borrowed from jzPCAL)
c
      implicit none
      real t
      INTEGER NLMAX
      PARAMETER (NLMAX=3000)
      INTEGER  pulNent,pulNpeak
      REAL     pulScale,pulCons,pulLine,pulSTEP
      REAL     pbase
      COMMON/tcrCDE/pulNent,pulNpeak,pulScale,pulSTEP,pbase,
     +              pulCons(NLMAX),pulline(NLMAX)
      real ta,tb,dt
      integer ib

      ta=T
      tb = ta/pulStep
      ib = tb
      if (tb.lt.0.) ib = ib - 1
      dt = tb - ib
      ib = ib + pulNpeak/pulStep
      if (ib.ge.1.and.ib.le.PulNent) then
       Tfrac = PulLine(ib)*dt + PulCons(ib)
      else
       Tfrac=0.
      endif

      return
      end 
