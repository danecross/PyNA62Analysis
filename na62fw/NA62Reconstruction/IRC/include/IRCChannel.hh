// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-20
//
// ---------------------------------------------------------

#ifndef IRCChannel_H
#define IRCChannel_H 1

#include "NA62VChannel.hh"
#include "TH1D.h"
#include "TH2F.h"
#include "TProfile.h"
#include "TGraph.h"
#include "TF1.h"

class IRCChannel : public NA62VChannel {

public:

  IRCChannel(Int_t, Int_t, Bool_t, Double_t, Double_t);
  ~IRCChannel();

  Double_t GetThreshold()                  { return fThreshold;               }
  void     SetThreshold(Double_t value)    { fThreshold = value;              }

private:

  Double_t fThreshold;

//   ClassDef(IRCChannel,1);
};

#endif
