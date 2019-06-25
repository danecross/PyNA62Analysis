// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------

#ifndef TRecoNewCHODCandidate_H
#define TRecoNewCHODCandidate_H

#include "TRecoVCandidate.hh"

class TRecoNewCHODCandidate : public TRecoVCandidate {

public:

  TRecoNewCHODCandidate();
  ~TRecoNewCHODCandidate() {}

  void Clear(Option_t* = "");

private:

  ClassDef(TRecoNewCHODCandidate,1);
};
#endif
