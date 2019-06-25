// --------------------------------------------------------------
// History:
//
// Modified by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoGigaTrackerHit_H
#define TRecoGigaTrackerHit_H

#include "TRecoVHit.hh"
#include "GigaTrackerChannelID.hh"

class TRecoGigaTrackerHit : public TRecoVHit, public GigaTrackerChannelID {
  
public:
  
  TRecoGigaTrackerHit();
  TRecoGigaTrackerHit(const TRecoGigaTrackerHit &);
  ~TRecoGigaTrackerHit(){};

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();
  
  Double_t             GetToT()                         { return fToT;            };
  void                 SetToT(Double_t value)           { fToT = value;           };

  Double_t             GetRawTime()                     { return fRawTime;        };
  void                 SetRawTime(Double_t value)       { fRawTime = value;       };

  Bool_t               GetIsPileUpHit()                 { return fIsPileUpHit;    };
  void                 SetIsPileUpHit(Bool_t value)     { fIsPileUpHit = value;   };
private:
  
  Double_t fToT;
  Double_t fRawTime;

  // This member is last in the list: it has been introduced in v2 of the class
  Bool_t fIsPileUpHit;
  
  ClassDef(TRecoGigaTrackerHit,2);

};
#endif
