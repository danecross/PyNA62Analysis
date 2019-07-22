// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-20
//
// ---------------------------------------------------------

#ifndef MUV0Channel_H
#define MUV0Channel_H 1

#include "NA62VChannel.hh"
#include "TH1D.h"
#include "TH2F.h"
#include "TProfile.h"
#include "TGraph.h"
#include "TF1.h"

class MUV0Channel : public NA62VChannel {

public:

  MUV0Channel(Int_t, Int_t, Bool_t, Double_t, Double_t);
  ~MUV0Channel();

  Double_t GetThreshold()                  { return fThreshold;               }
  void     SetThreshold(Double_t value)    { fThreshold = value;              }

private:

  Double_t fThreshold;

//   ClassDef(MUV0Channel,1);
};

#endif
