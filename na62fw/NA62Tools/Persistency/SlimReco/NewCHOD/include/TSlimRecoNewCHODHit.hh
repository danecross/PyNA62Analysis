// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#ifndef TSLIMRECONEWCHODHIT_H
#define TSLIMRECONEWCHODHIT_H

#include "TSlimRecoVHit.hh"

class TRecoNewCHODHit;

class TSlimRecoNewCHODHit : public TSlimRecoVHit {

public:
  TSlimRecoNewCHODHit()                 {}
  explicit TSlimRecoNewCHODHit(TRecoNewCHODHit*) {}
  virtual ~TSlimRecoNewCHODHit()        {}

  // Conversion functions
  virtual void FromReco(TRecoVHit*) {}
  virtual void ToReco  (TRecoVHit*) {}

private:

  ClassDef(TSlimRecoNewCHODHit, 1)
};

#endif
