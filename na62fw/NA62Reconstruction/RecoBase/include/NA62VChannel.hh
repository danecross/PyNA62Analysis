// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-20
//
// ---------------------------------------------------------

#ifndef NA62VChannel_H
#define NA62VChannel_H 1

#include "TH1D.h"
#include "TH2F.h"
#include "TProfile.h"
#include "TGraph.h"
#include "TF1.h"
#include "TFile.h"

class NA62VChannel {

public:

  NA62VChannel(Int_t, Int_t, Bool_t, TString);
  ~NA62VChannel();

  void Reset();
  void AddHit();
  void InitHistograms();
  void FillTime(Double_t, Double_t, Double_t);

  void Write(TFile*);

  Int_t  GetGeoChannelID()          { return fGeoChannelID; }
  void   SetGeoChannelID(Int_t val) { fGeoChannelID = val;  }
  Int_t  GetROChannelID()           { return fROChannelID;  }
  void   SetROChannelID(Int_t val)  { fROChannelID = val;   }
  Bool_t GetEnabled()               { return fEnabled;      }
  void   SetEnabled(Bool_t val)     { fEnabled = val;       }
  void   Enable()                   { fEnabled = kTRUE;     }
  void   Disable()                  { fEnabled = kFALSE;    }
  Int_t  GetNHits()                 { return fNHits;        }
  void   SetNHits(Int_t val)        { fNHits = val;         }

  Double_t GetT0()                  { return fT0;           }
  void     SetT0(Double_t val)      { fT0 = val;            }

protected:

  Int_t  fGeoChannelID, fROChannelID, fNHits;
  Bool_t fEnabled;
  Bool_t fFillHistograms;
  TString fName;

  Double_t fT0;

  TProfile *fHTimeVsWidth;

//   ClassDef(NA62VChannel,1);
};

#endif
