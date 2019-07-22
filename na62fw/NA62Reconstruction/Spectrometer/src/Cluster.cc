#include "Riostream.h"

#include "Cluster.hh"
#include "SpectrometerParameters.hh"

/// \class Cluster 
/// \Brief
/// Spectrometer view-clusters. 
/// \EndBrief
/// 
/// \Detailed
/// Class which returns the variables defining a view-cluster built from the tube-hits.
/// \EndDetailed

Cluster::Cluster() :
  fLocalSlope   (.0),
  fError        (.0),
  fUsedForHit   (0),
  fFlagUsed     (0),
  fTrailingTime (.0),
  fQuality      (.0),
  fEdge         (0)
{ }

Cluster::~Cluster()
{ }

Cluster::Cluster(const Cluster& right):
  fHitId        (right.fHitId),
  fLocalPosition(right.fLocalPosition),
  fPosition     (right.fPosition),
  fLocalSlope   (right.fLocalSlope),
  fError        (right.fError),
  fUsedForHit   (right.fUsedForHit),
  fFlagUsed     (right.fFlagUsed),
  fTrailingTime (right.fTrailingTime),
  fQuality      (right.fQuality),
  fEdge         (right.fEdge)
{
}

void Cluster::AddHit(Int_t jHit)
{
/// \MemberDescr
/// \param jHit Id of a tube-hit.
///
/// Store the id of the tube-hits in the private member data Cluster::fHitId. 
/// \EndMemberDescr

  fHitId.push_back(jHit);
}

void Cluster::Reset()
{ 
/// \MemberDescr
/// Clear the Cluster::fHitId data member and initialize Cluster::fLocalPosition and 
/// Cluster::fPosition. 
/// \EndMemberDescr

  Double_t fNull = SpectrometerParameters::GetInstance().GetNullCoordinate();
  fHitId.clear();
  fLocalPosition = TVector3(fNull,fNull,fNull); 
  fPosition = TVector3(fNull,fNull,fNull); 
  fLocalSlope = fNull;
  fError = 0;
  fUsedForHit = 0;
  fFlagUsed = 0;
  fTrailingTime = -99999.;
  fQuality = 999999.;
  fEdge = 0;
}
