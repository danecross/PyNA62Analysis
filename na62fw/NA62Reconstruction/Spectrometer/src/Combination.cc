#include "Riostream.h"
#include "Combination.hh"

/// \class Combination
/// \Brief
/// Container class for combination of chamber hits forming a straw candidate.
/// \EndBrief
///
/// \Detailed
/// Container class for combination of chamber hits forming a straw candidate.
/// Selected in the pattern recognition methods of the TrackCollector class.
/// \EndDetailed

void Combination::AddCluster(Int_t jCluster, Int_t jView, Int_t jChamber)
{
/// \MemberDescr
/// \param jCluster id of the cluster to add to the combination.
/// \param jView id of the view which the cluster belongs to.
/// \param jChamber id of the chamber which the cluster belongs to.
///
/// \EndMemberDescr

  fClusterId.push_back(jCluster);
  fViewId.push_back(jView);
  fChamberId.push_back(jChamber);
}

void Combination::Reset()
{
/// \MemberDescr
/// Reset the main variables.
/// \EndMemberDescr
  fClusterId.clear();
  fViewId.clear();
  fChamberId.clear();
  fChamberHitId.clear();
  fNTotalHits = -1;
  fType = -1;
  fNCommon = -1;
  fX0 = -9999.;
  fY0 = -9999.;
  fZ0 = -9999.;
  fThetaX = -9999.;
  fThetaY = -9999.;
  fP = -9999.;
  fHDelta = -9999.;
  fDeltaX = -9999.;
  fQuality = -9999.;
  fTrailingTime = -9999.;
}

void Combination::Sort()
{
/// \MemberDescr
/// Method sorting the clusters on the basis of the chamber and on the views.
/// \EndMemberDescr

  SortChamber();
  for (Int_t j=0; j<4; j++) SortView(j);
}

void Combination::SortChamber()
{
/// \MemberDescr
/// Sort the clusters on the basis of the most downstream chamber.
/// \EndMemberDescr

  Int_t nClusters = (Int_t)fClusterId.size();
  for (Int_t i=0; i<nClusters; i++)
  {
    for (Int_t j=i+1; j<nClusters; j++)
    {
      if (fChamberId[j]>fChamberId[i])
      {
        Int_t chambstore = fChamberId[i];
        Int_t chambstore2 = fChamberId[j];
        Int_t viewstore = fViewId[i];
        Int_t viewstore2 = fViewId[j];
        Int_t clusstore = fClusterId[i];
        Int_t clusstore2 = fClusterId[j];
        fChamberId[i] = chambstore2;
        fChamberId[j] = chambstore;
        fViewId[i] = viewstore2;
        fViewId[j] = viewstore;
        fClusterId[i] = clusstore2;
        fClusterId[j] = clusstore;
      }
    }
  }
}

void Combination::SortView(Int_t chamberId)
{
/// \MemberDescr
/// \param chamberId id of the chamber which the view belongs to.
///
/// Sort the clusters on the basis of the most downstream view.
/// \EndMemberDescr

  Int_t nClusters = (Int_t)fClusterId.size();
  for (Int_t i=0; i<nClusters; i++)
  {
    if (fChamberId[i]!=chamberId) continue;
    for (Int_t j=i+1; j<nClusters; j++)
    {
      if (fChamberId[j]!=chamberId) continue;
      if (fViewId[j]>fViewId[i])
      {
        Int_t chambstore = fChamberId[i];
        Int_t chambstore2 = fChamberId[j];
        Int_t viewstore = fViewId[i];
        Int_t viewstore2 = fViewId[j];
        Int_t clusstore = fClusterId[i];
        Int_t clusstore2 = fClusterId[j];
        fChamberId[i] = chambstore2;
        fChamberId[j] = chambstore;
        fViewId[i] = viewstore2;
        fViewId[j] = viewstore;
        fClusterId[i] = clusstore2;
        fClusterId[j] = clusstore;
      }
    }
  }

}

Int_t Combination::GetNChambers() const
{
/// \MemberDescr
/// \return Number of chambers used to form the candidate combination.
/// \EndMemberDescr

  Int_t nChambers = 0;
  Int_t chamberIdSave = -1;
  for (Int_t j=0; j<(Int_t)fChamberId.size(); j++)
  {
    if (fChamberId[j]!=chamberIdSave) nChambers++;
    chamberIdSave = fChamberId[j];
  }
  return nChambers;
}

void Combination::SetQuality(Double_t hd, Double_t dx)
{
  fHDelta = hd;
  fDeltaX = dx;
  fQuality = sqrt(fHDelta*fHDelta/4.+fDeltaX*fDeltaX/49.);
}
