// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-20
//
// ---------------------------------------------------------

#ifndef SACChannel_H
#define SACChannel_H 1

#include "NA62VChannel.hh"
#include "TH1D.h"
#include "TH2F.h"
#include "TProfile.h"
#include "TGraph.h"
#include "TF1.h"

class SACChannel : public NA62VChannel {

public:

  SACChannel(Int_t, Int_t, Bool_t, Double_t, Double_t);
  ~SACChannel();

  Double_t GetThreshold()                  { return fThreshold;               }
  void     SetThreshold(Double_t value)    { fThreshold = value;              }

private:

  Double_t fThreshold;

//   ClassDef(SACChannel,1);
};

#endif
