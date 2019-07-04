#include "Riostream.h"

#include "Combination.hh"
#include "SpectrometerParameters.hh"
#include "SpectrometerGeometry.hh"
#include "TVector3.h"

/// \class Combination
/// \Brief
/// Class describing the combination candidate.
/// \EndBrief
///
/// \Detailed
/// This class describes the candidate combinations selected in the pattern recognition methods of the TrackCollector class.
/// \EndDetailed

Combination::Combination() :
  fX0              (.0),
  fY0              (.0),
  fZ0              (.0),
  fThetaX          (.0),
  fThetaY          (.0),
  fP               (.0),
  fNViewAfterMagnet(0),
  fChi2            (.0),
  fSigmaX0         (.0),
  fSigmaY0         (.0),
  fSigmaThetaX     (.0),
  fSigmaThetaY     (.0),
  fSigmaP          (.0),
  fNTotalHits      (0),
  fDMag            (0.1*SpectrometerGeometry::GetInstance()->GetMagnetZLength()),
  fZMag            (0.1*SpectrometerGeometry::GetInstance()->GetMagnetZPosition()-fDMag/2.),
  fBMag            (SpectrometerGeometry::GetInstance()->GetMagnetFieldStrength()*10000),
  fEC              (SpectrometerParameters::GetInstance()->GetEC()),
  fType            (0),
  fHDelta          (.0),
  fDeltaX          (.0),
  fQuality         (.0),
  fTrailingTime    (.0),
  fNCommon         (0)
{
/// \MemberDescr
/// Constructor.
/// \EndMemberDescr
}

Combination::Combination(const Combination& right) :
  fClusterId       (right.fClusterId),
  fViewId          (right.fViewId),
  fChamberId       (right.fChamberId),
  fX0              (right.fX0),
  fY0              (right.fY0),
  fZ0              (right.fZ0),
  fThetaX          (right.fThetaX),
  fThetaY          (right.fThetaY),
  fP               (right.fP),
  fNViewAfterMagnet(right.fNViewAfterMagnet),
  fChi2            (right.fChi2),
  fSigmaX0         (right.fSigmaX0),
  fSigmaY0         (right.fSigmaY0),
  fSigmaThetaX     (right.fSigmaThetaX),
  fSigmaThetaY     (right.fSigmaThetaY),
  fSigmaP          (right.fSigmaP),
  fNTotalHits      (right.fNTotalHits),
  fDMag            (right.fDMag),
  fZMag            (right.fZMag),
  fBMag            (right.fBMag),
  fEC              (right.fEC),
  fType            (right.fType),
  fHDelta          (right.fHDelta),
  fDeltaX          (right.fDeltaX),
  fQuality         (right.fQuality),
  fTrailingTime    (right.fTrailingTime),
  fChamberHitId    (right.fChamberHitId),
  fNCommon         (right.fNCommon)
{
/// \MemberDescr
/// \param right Combination structure.
///
/// Constructor.
/// \EndMemberDescr

  for (Int_t jj=0; jj<4; jj++)
  {
    fSubHDelta[jj]        = right.fSubHDelta[jj];
    fSubThetaY[jj]        = right.fSubThetaY[jj];
    fSubType[jj]          = right.fSubType[jj];
  }
}

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

  Double_t fNull = SpectrometerParameters::GetInstance()->GetNullCoordinate();
  fClusterId.clear();
  fViewId.clear();
  fChamberId.clear();
  fX0 = fNull;
  fY0 = fNull;
  fZ0 = fNull;
  fThetaX = fNull;
  fThetaY = fNull;
  fP = 0.;
  fNViewAfterMagnet = 0;
  fChi2 = 0;
  fSigmaX0 = fNull;
  fSigmaY0 = fNull;
  fSigmaThetaX = fNull;
  fSigmaThetaY = fNull;
  fSigmaP = fNull;
  fHDelta = fNull;
  fDeltaX = fNull;
  fTrailingTime = fNull;
  fChamberHitId.clear();
  fQuality = fNull;
  fType = -1;
  for (Int_t jj=0; jj<4; jj++)
  {
    fSubHDelta[jj] = fNull;
    fSubThetaY[jj] = fNull;
    fSubType[jj] = -1;
  }
  fNCommon = 0;
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

Double_t Combination::Project(Double_t zEnd, Double_t angle)
{
/// \MemberDescr
/// \param zEnd Z coordinate of the projection point.
/// \param angle Angle of the projected coordinate.
///
/// Extrapolate the coordinate at a zEnd position on the basis of the track(let) parameters.
/// \EndMemberDescr

  TVector3 fPosExp;
  zEnd *= 0.1;
  Double_t xStart = 0.1*fX0;
  Double_t yStart = 0.1*fY0;
  Double_t zStart = 0.1*fZ0;
  Double_t pTrack = fabs(fP)/1000.;
  Double_t xTheta = fThetaX;
  Double_t yTheta = fThetaY;
  Int_t qTrack = fP>=0 ? 1 : -1;

  // fZEnd before magnet
  if (zEnd<=fZMag)
  {
    fPosExp.SetX(xStart+xTheta*(zEnd-zStart));
    fPosExp.SetY(yStart+yTheta*(zEnd-zStart));
    fPosExp.SetZ(zEnd);
    fPosExp *= 10.;
    fPosExp.RotateZ(angle);
    return angle<=1.57 ? fPosExp.X() : -fPosExp.X();
  }

  // fZEnd after MNP33
  fPosExp.SetX(xStart+xTheta*(fZMag-zStart));
  fPosExp.SetY(yStart+yTheta*(fZMag-zStart));
  fPosExp.SetZ(fZMag);
  TVector3 B,p;
  B.SetXYZ(0.,fBMag,0.);
  p.SetZ(pTrack/sqrt(1.+xTheta*xTheta+yTheta*yTheta));
  p.SetX(p.Z()*xTheta);
  p.SetY(p.Z()*yTheta);
  Int_t qb = B.Y()>0 ? 1 : -1;
  Double_t rho = (p.Cross(B)).Mag()/(qTrack*fEC*B.Mag2());
  Double_t delta = fDMag/rho;
  Double_t sint = sin(atan(xTheta));
  Double_t cost = cos(atan(xTheta));
  Double_t dx = qb*rho*(-cost+sqrt(1-(delta-qb*sint)*(delta-qb*sint)));
  fPosExp.SetX(fPosExp.X()+dx);
  fPosExp.SetY(fPosExp.Y()+yTheta*fDMag);
  fPosExp.SetZ(fPosExp.Z()+fDMag);
  Double_t xThetaAfter = -qb*(delta-qb*sint)/sqrt(1.-(delta-qb*sint)*(delta-qb*sint));
  fPosExp.SetX(fPosExp.X()+xThetaAfter*(zEnd-fPosExp.Z()));
  fPosExp.SetY(fPosExp.Y()+yTheta*(zEnd-fPosExp.Z()));
  fPosExp.SetZ(zEnd);
  fPosExp *= 10.;

  // Rotation according to the view
  fPosExp.RotateZ(angle);

  return angle<=1.57 ? fPosExp.X() : -fPosExp.X();
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
