// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-04-24
//
// --------------------------------------------------------------
#ifndef TRecoVCandidate_H
#define TRecoVCandidate_H

#include "TVCandidate.hh"
#include "TRecoVHit.hh"

class TRecoVCandidate : public TVCandidate {

public:

  TRecoVCandidate();
  TRecoVCandidate(const TRecoVCandidate &);
  explicit TRecoVCandidate(Int_t);
  virtual ~TRecoVCandidate();
  void Clear(Option_t* = "");

  TRecoVHit * GetHit(Int_t);

  Double_t GetTime() const         { return fTime;  }
  void     SetTime(Double_t value) { fTime = value; }

private:

  Double_t fTime;
  ClassDef(TRecoVCandidate,1);
};
#endif
