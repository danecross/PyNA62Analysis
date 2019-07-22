// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-18
//
// ---------------------------------------------------------------

#ifndef TRecoCedarCandidate_H
#define TRecoCedarCandidate_H

#include "TRecoVCandidate.hh"

class TRecoCedarCandidate : public TRecoVCandidate {

public:

  TRecoCedarCandidate();
  ~TRecoCedarCandidate() {}

  void       Clear(Option_t* = "");
  void       UpdateTime();
  void       UpdateTime(Double_t);

  Int_t      GetNSectors()              { return fNSectors;  }
  void       SetNSectors(Double_t val)  { fNSectors = val;   }
  Bool_t     GetIsSelected()            { return fIsSelected;}
  void       SetIsSelected(Double_t val){ fIsSelected = val; }

  Double_t   GetDeltaTimeClosestCandidate()             { return fDeltaTimeClosestCandidate; }
  void       SetDeltaTimeClosestCandidate(Double_t val) { fDeltaTimeClosestCandidate = val;  }
  Double_t   GetNHitsClosestCandidate()                 { return fNHitsClosestCandidate;     }
  void       SetNHitsClosestCandidate(Double_t val)     { fNHitsClosestCandidate = val;      }

private:
  // fNHits and fTime already defined in TRecoVCandidate:
  // it is essential to use those ones.

  Int_t fNPMTs, fNSectors; // fNPMTs it's redundant, it should be removed!
  Bool_t fIsSelected;
  Double_t fDeltaTimeClosestCandidate, fNHitsClosestCandidate;

  ClassDef(TRecoCedarCandidate,1);
};

#endif
