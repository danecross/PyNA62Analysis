// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------
#ifndef TRecoCHODCandidate_H
#define TRecoCHODCandidate_H

#include "TRecoVCandidate.hh"

class TRecoCHODCandidate : public TRecoVCandidate {

public:

  TRecoCHODCandidate();
  ~TRecoCHODCandidate(){}

  void Clear(Option_t* = "");

  Double_t GetHitTime()                   { return fHitTime;      }
  void     SetHitTime(Double_t value)     { fHitTime = value;     }
  TVector2 GetHitPosition()               { return fHitPosition;  }
  void     SetHitPosition(TVector2 value) { fHitPosition = value; }
  Int_t    GetNHitPairs()                 { return fNHitPairs;      }
  void     SetNHitPairs(Int_t value)      { fNHitPairs = value;     }

private:

  Double_t fHitTime;
  TVector2 fHitPosition;
  Int_t fNHitPairs;

  ClassDef(TRecoCHODCandidate,1);
};
#endif
