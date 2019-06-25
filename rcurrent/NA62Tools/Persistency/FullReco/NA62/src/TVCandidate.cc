// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------

/// \class TVCandidate
/// \Brief
/// Base class for all candidates.
/// \EndBrief
/// 
/// \Detailed
/// It implements the base container corresponding to a group of hits, such as a cluster or a track.\n
/// TVCandidate::GetHit allows direct access to the hits of which the cluster is composed, but it relies on fEvent 
/// to be set to the pointer of the TDetectorVEvent actually containing the hits. This pointer is not
/// saved on disk, and must be restored manually when reading back the content from a TTree.\n
/// Here is a simple example:
/// \code
/// for(Int_t iCandidate = 0; iCandidate < NCandidates; iCandidate++){
///    TVCandidate * Cand = Event->GetCandidate(iCandidate);
///    Cand->SetEvent(Event);
///    for(Int_t iHit = 0; iHit < Cand->GetNHits(); iHit++)
///       TVHit * Hit = Cand->GetHit(iHit);
/// }
/// \endcode
/// \EndDetailed

#include "TVCandidate.hh"

#include "Riostream.h"

ClassImp(TVCandidate)

TVCandidate::TVCandidate() :
    fNHits(0),
    fNMaxHits(10),
    fEvent(nullptr)
{
    fHitsIndexes.Set(fNMaxHits);
}

TVCandidate::TVCandidate(const TVCandidate & cand) :
    TObject(cand),
    fNHits(cand.fNHits),
    fNMaxHits(cand.fNMaxHits),
    fEvent(cand.fEvent)
{
    fHitsIndexes.Set(fNMaxHits);
}

TVCandidate::TVCandidate(Int_t NMaxHits) :
    fNHits(0),
    fNMaxHits(NMaxHits),
    fEvent(0)
{
    fHitsIndexes.Set(fNMaxHits);
}

TVCandidate::~TVCandidate() {

}

void TVCandidate::Clear(Option_t * /*option*/) {
    
    fHitsIndexes.Reset();
    fNHits = 0;

}

Bool_t TVCandidate::AddHit(Int_t HitIndex){
    if( HitIndex>=0 ){
      if ( fNHits >= fNMaxHits){
        fNMaxHits += 10;
        fHitsIndexes.Set(fNMaxHits);
      }

      fHitsIndexes[fNHits] = HitIndex;
      fNHits++;
    }else{
        return kFALSE;
    }

    return kTRUE;
}

TDetectorVHit * TVCandidate::GetHit(Int_t iHit){
    if(fEvent && iHit>=0 && iHit < fNMaxHits && iHit < fNHits)
        return static_cast<TDetectorVHit*>(fEvent->GetHit(fHitsIndexes[iHit]));
    else
        return nullptr;
}

void TVCandidate::RemoveHit(Int_t iHit){
  if(iHit>=0){
    fNHits--;
    for(Int_t jHit = iHit; jHit < fNHits && jHit < fNMaxHits-1; jHit++)
        fHitsIndexes[jHit] = fHitsIndexes[jHit + 1];
  }
}

void TVCandidate::Merge(TVCandidate* Candidate){

  for(Int_t iHit=0; iHit < Candidate->GetNHits() ; iHit++){
    AddHit(Candidate->fHitsIndexes[iHit]);
  }
}
