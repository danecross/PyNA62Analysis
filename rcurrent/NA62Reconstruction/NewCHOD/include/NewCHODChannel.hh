// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------

#ifndef NewCHODChannel_H
#define NewCHODChannel_H 1

#include "NA62VChannel.hh"
#include "TH1D.h"
#include "TH2F.h"
#include "TGraph.h"
#include "TF1.h"

class NewCHODChannel : public NA62VChannel {

public:

  NewCHODChannel(Int_t, Int_t, Bool_t);
  ~NewCHODChannel() {}

  Bool_t   IsHigh()              { return fIsHigh; }
  void     SetIsHigh(Bool_t val) { fIsHigh = val;  }

private:

  Bool_t   fIsHigh; ///< Channel with the higher ID in tile (N+50 rather than N)?

  //  ClassDef(NewCHODChannel,1);
};

#endif
