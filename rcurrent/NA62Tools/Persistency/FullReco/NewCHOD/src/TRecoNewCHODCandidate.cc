// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------
/// \class TRecoNewCHODCandidate
/// \Brief
/// NewCHOD candidate: empty; the information is stored in TRecoNewCHODHit
/// \EndBrief

#include "TRecoNewCHODCandidate.hh"

ClassImp(TRecoNewCHODCandidate)

TRecoNewCHODCandidate::TRecoNewCHODCandidate() : TRecoVCandidate() {}

void TRecoNewCHODCandidate::Clear(Option_t* option){
  TRecoVCandidate::Clear(option);
}
