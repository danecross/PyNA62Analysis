// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------
#ifndef TRecoCHODHit_H
#define TRecoCHODHit_H

#include "TRecoVHit.hh"
#include "CHODChannelID.hh"

class TRecoCHODHit : public TRecoVHit, public CHODChannelID {

public:

  TRecoCHODHit();
  ~TRecoCHODHit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Double_t GetTimeWidth()             { return fTimeWidth; }
  void     SetTimeWidth(Double_t val) { fTimeWidth = val;  }

private:
    
  Double_t fTimeWidth;

  ClassDef(TRecoCHODHit,1);
};
#endif
