#include "Riostream.h"

#include "StrawHitCollector.hh"

/// \class StrawHitCollector 
/// \Brief
/// Ensemble of the reconstructed hits per plane.
/// \EndBrief
/// 
/// \Detailed
/// This class groups the tube-hits belonging to the same view-plane. 
/// It returns:
/// - StrawHitCollector::GetN().
/// - StrawHitCollector::GetHitId(Int_t jHit).  
/// \EndDetailed

StrawHitCollector::StrawHitCollector()
{ }

StrawHitCollector::~StrawHitCollector()
{ }

void StrawHitCollector::Reset()
{
  fHitId.clear();
  fHitFlag.clear();
  nGoodHit = 0;
}

void StrawHitCollector::AddHit(Int_t hitID, Int_t hitFlag, Bool_t good)
{
  fHitId.push_back(hitID);
  fHitFlag.push_back(hitFlag);
  if (good) nGoodHit++;
}

