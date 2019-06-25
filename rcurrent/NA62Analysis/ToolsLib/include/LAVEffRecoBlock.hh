#ifndef LAVEffRecoBlock_H
#define LAVEffRecoBlock_H

#include "LAVRecoHit.hh"
#include "LAVRecoBlock.hh"
#include "TLAVDigi.hh"


#define MAX_RECOHITS_PER_BLOCK 1000


class LAVEffRecoBlock : public LAVRecoBlock {

public:

  explicit LAVEffRecoBlock(Int_t BlockID);
  void CreateEffHits();


private:

  Bool_t fOpenHitLow, fOpenHitHigh;
  Bool_t fOverFlowLow, fOverFlowHigh;
  Int_t fNHitsLow, fNHitsHigh;
  
};

#endif

