// ---------------------------------------------------------------//
// History:
//
// Major update: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-10-20
// Created by Karim Massri (karim.massri@cern.ch)          2014-02-03
//
// ----------------------------------------------------------------//

#ifndef MUV3Channel_H
#define MUV3Channel_H 1

#include "NA62VChannel.hh"
#include "TH1D.h"
#include "TH2F.h"
#include "TGraph.h"
#include "TF1.h"

class MUV3Channel : public NA62VChannel {

public:

  MUV3Channel(Int_t, Int_t, Bool_t);
  ~MUV3Channel() {}

  Int_t GetTileID()           { return fTileID;     }
  void  SetTileID(Int_t val)  { fTileID = val;      }
  Int_t GetHVGroup()          { return fHVGroup;    }
  void  SetHVGroup(Int_t val) { fHVGroup = val;     }

private:

  Int_t fTileID, fHVGroup;

  //  ClassDef(MUV3Channel,1);
};

#endif
