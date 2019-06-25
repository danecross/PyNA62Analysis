// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-04-24
//
// --------------------------------------------------------------

/// \class TRecoVCandidate
/// \Brief
/// Base class for all candidates.
/// \EndBrief
/// 
/// \Detailed
/// It implements the base container corresponding to a group of hits, such as a cluster or a track.\n
/// TRecoVCandidate::GetHit allows direct access to the hits of which the cluster is composed, but it relies on fEvent 
/// to be set to the pointer of the TDetectorVEvent actually containing the hits. This pointer is not
/// saved on disk, and must be restored manually when reading back the content from a TTree.\n
/// Here is a simple example:
/// \code
/// for(Int_t iCandidate = 0; iCandidate < NCandidates; iCandidate++){
///    TRecoVCandidate * Cand = static_cast<TRecoVCandidate*>(Event->GetCandidate(iCandidate));
///    Cand->SetEvent(Event);
///    for(Int_t iHit = 0; iHit < Cand->GetNHits(); iHit++)
///       TVHit * Hit = Cand->GetHit(iHit);
/// }
/// \endcode
/// \EndDetailed

#include "TRecoVCandidate.hh"

ClassImp(TRecoVCandidate)

TRecoVCandidate::TRecoVCandidate() :
  TVCandidate(),
  fTime(0)
{}

TRecoVCandidate::TRecoVCandidate(const TRecoVCandidate & cand) :
  TVCandidate((TVCandidate &)cand),
  fTime(cand.fTime)
{
}

TRecoVCandidate::TRecoVCandidate(Int_t NMaxHits) :
  TVCandidate(NMaxHits),
  fTime(0)
{}

TRecoVCandidate::~TRecoVCandidate() {
  Clear();
}

void TRecoVCandidate::Clear(Option_t* option){
  TVCandidate::Clear(option);
}

TRecoVHit * TRecoVCandidate::GetHit(Int_t iHit){
  return static_cast<TRecoVHit*>(TVCandidate::GetHit(iHit));
}
