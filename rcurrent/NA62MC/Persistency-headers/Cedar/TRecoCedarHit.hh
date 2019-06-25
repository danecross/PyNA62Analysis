// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-18
//
// ---------------------------------------------------------------
 
#ifndef TRecoCedarHit_H
#define TRecoCedarHit_H

#include "TRecoVHit.hh"
#include "CedarChannelID.hh"

class TRecoCedarHit : public TRecoVHit, public CedarChannelID {

public:

  TRecoCedarHit();
  ~TRecoCedarHit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  void SetWidth      (Double_t val) { fWidth  = val;       }
  void SetROChannelID(Int_t val)    { fROChannelID = val;  }

  Double_t GetWidth()               { return fWidth;       }
  Int_t    GetROChannelID()         { return fROChannelID; }

private:

  Double_t fWidth;
  Int_t    fROChannelID;

  ClassDef(TRecoCedarHit,1);
};

#endif
