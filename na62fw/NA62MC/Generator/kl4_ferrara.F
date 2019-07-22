C================================================================
      subroutine KL4_FERRARA(jkaon,IVAL)
c
c     Marcella Scarpa, March 2004
c----------------------------------------------------------------
c
c      Generate Kl4 decay      one loop CHPT
c      ival=1 pi+pi- mu nu
c      ival=2 pi0pi0 mu nu
c      ival=3 pi0pi0 e nu
c      ival=4 pi+ pi- e nu  

#include "common_blocks.f"
#include "masses.f"

      integer jpi1,jpi2,jlep,j
      INTEGER IPION1,IPION2,ILEP,IVAL
      real*8  ppi1(4),ppi2(4),plep(4)

      call Kmu4genc(ival)

      do j = 1, 4
         ppi1(j) = pcm(j,1)
         ppi2(j) = pcm(j,2)
         plep(j) = pcm(j,3)
      enddo

c ... store particles
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

C     Save original particles in K rest frame into GeneParts
      jpi1 = mcadd4gen(jkaon, IPION1, ppi1, 0)
      jpi2 = mcadd4gen(jkaon, IPION2, ppi2, 0)
      jlep = mcadd4gen(jkaon, Ilep  , plep, 0)

c     Boost particles to lab frame
      call dboost (p4ini(1,jkaon), MKCH, ppi1, ppi1)
      call dboost (p4ini(1,jkaon), MKCH, ppi2, ppi2)
      call dboost (p4ini(1,jkaon), MKCH, plep, plep)

C     Register particles for tracking
      jpi1 = mcadd4(jkaon, IPION1, ppi1)
      jpi2 = mcadd4(jkaon, IPION2, ppi2)
      jlep = mcadd4(jkaon, Ilep  , plep)

      return
      end
