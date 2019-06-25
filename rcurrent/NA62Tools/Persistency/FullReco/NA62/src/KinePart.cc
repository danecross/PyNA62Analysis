// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch)
//            Francesca Bucci (Francesca.Bucci@cern.ch) 2008-01-17
//
// Checkpoints added by E Goudzovski (eg@hep.ph.bham.ac.uk)
//
// Based on a project by Emanuele Leonardi (Emanuele Leonardi@cern.ch) 2007-01-10
//
// --------------------------------------------------------------

/// \class KinePart
/// \Brief
/// True positions and 4-momenta of particles along their trajectories
/// \EndBrief
/// \Detailed
/// Stores coordinates and 4-momenta at the production point, the end point, and
/// a number of "checkpoints" in front of most subdetectors, for particles recorded into the "MC truth".
/// \EndDetailed

#include "KinePart.hh"

#include <stdlib.h>
#include "Riostream.h"

ClassImp(KinePart)

KinePart::KinePart() : GenePart() {
  Clear();
}

void KinePart::Clear(Option_t* option) {
  GenePart::Clear(option);
  fID = fParentID = fParentIndex = -1;
  fFinalEnergy   = 0.0;
  fFinalMomentum = TVector3(0.0, 0.0, 0.0);
  fProdPos = TLorentzVector(0.0, 0.0, 0.0, 0.0);
  fEndPos  = TLorentzVector(0.0, 0.0, 0.0, 0.0);
  for (int i=0; i<15; i++) {
    SetPosAtCheckPoint(i, TVector3(0.0, 0.0, 0.0));
    SetMomAtCheckPoint(i, TLorentzVector(0.0, 0.0, 0.0, 0.0));
  }
  fProdProcessName = "";
  fEndProcessName = "";
  fParticleName = "";
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// (X,Y) coordinates extrapolated to any Z plane as a straight line from the prodution point.
// The nominal MNP33 momentum kick is (see SpectrometerGeometryParameters.cc):
// Px = -0.6928 T * 1.3 m * 299.792 (MeV/Tm) = -270.0 MeV.

Double_t KinePart::xAt(Double_t z, Double_t MagFieldScaleFactor) {
  /// \MemberDescr
  /// X position at a certain Z plane, extrapolated as a straight line from the production point.
  /// MNP33 momentum kick is taken into account approximately, field scale factor is a parameter (default=1).
  /// \EndMemberDescr
  if (z<fProdPos.Z()) return 0.0; // no backward extrapolation
  Double_t x = fProdPos.X() + (z-fProdPos.Z()) * fInitialMomentum.x() / fInitialMomentum.z();
  if (fCharge && fProdPos.Z()<196995.0 && z>196995.0) {
    x -= (fCharge * MagFieldScaleFactor * 270.0 / fInitialMomentum.z() * (z-196995.0));
  }
  return x;
}

Double_t KinePart::xAtBackwards(Double_t z) {
  /// \MemberDescr
  /// X position at a certain Z plane, extrapolated backwards as a straight line from the end point.
  /// \EndMemberDescr
  if (z>fEndPos.Z()) return 0.0; // no forward extrapolation
  return fEndPos.X() + (z-fEndPos.Z()) * fFinalMomentum.x() / fFinalMomentum.z();
}

Double_t KinePart::yAt(Double_t z) {
  /// \MemberDescr
  /// Y position at a certain Z plane, extrapolated as a straight line from the production point.
  /// \EndMemberDescr
  if (z<fProdPos.Z()) return 0.0; // no backward extrapolation
  return fProdPos.Y() + (z-fProdPos.Z()) * fInitialMomentum.y() / fInitialMomentum.z();
}

Double_t KinePart::yAtBackwards(Double_t z) {
  /// \MemberDescr
  /// Y position at a certain Z plane, extrapolated backwards as a straight line from the end point.
  /// \EndMemberDescr
  if (z>fEndPos.Z()) return 0.0; // no forward extrapolation
  return fEndPos.Y() + (z-fEndPos.Z()) * fFinalMomentum.y() / fFinalMomentum.z();
}

TVector3 KinePart::GetPosAtZ(Double_t z) {
  /// \MemberDescr
  /// Position at a certain Z plane, extrapolated as a straight line from the previous KinePart checkpoint.
  /// Returns zero vector if z is not in the interval [fProdPos.Z(), fEndPos.Z()]
  /// \EndMemberDescr
  if (z < fProdPos.Z() || z > fEndPos.Z()) return TVector3(0,0,0);
  TVector3 startPos(fProdPos.Vect());  // default = position at production
  TVector3 startMom(fInitialMomentum); // default = momentum at production
  Int_t NCheckPoints = 15;
  // loop until the latest checkpoint before the required z-plane
  for (int iCheck = 0; iCheck < NCheckPoints; iCheck++) {
    TVector3 checkPos(GetPosAtCheckPoint(iCheck));
    if (checkPos.Z() < fProdPos.Z()) continue; // particle could not pass the checkpoint
    if (checkPos.Z() < z) {                    // passed checkpoint before required z-plane
      startPos = checkPos;
      startMom = GetMomAtCheckPoint(iCheck).Vect();
    } else {
      break;
    }
  }
  double xpos = startPos.X() + (z - startPos.Z()) * startMom.X() / startMom.Z();
  double ypos = startPos.Y() + (z - startPos.Z()) * startMom.Y() / startMom.Z();
  return TVector3(xpos, ypos, z);
}

///////////////////////////////////////
// Positions and momenta at checkpoints

void KinePart::SetPosAtCheckPoint(Int_t Point, TVector3 val) {
  switch (Point) {
  case  0: fPosCedarEntry        = val; break;
  case  1: fPosCedarExit         = val; break;
  case  2: fPosGigaTrackerEntry  = val; break;
  case  3: fPosGigaTrackerExit   = val; break;
  case  4: fPosSpectrometerEntry = val; break;
  case  5: fPosMNP33Entry        = val; break;
  case  6: fPosMNP33Exit         = val; break;
  case  7: fPosRICHMirrorEntry   = val; break;
  case  8: fPosCHODEntry         = val; break;
  case  9: fPosLKrEntry          = val; break;
  case 10: fPosMUV1Entry         = val; break;
  case 11: fPosMUV2Entry         = val; break;
  case 12: fPosFISCEntry         = val; break;
  case 13: fPosMUV3Entry         = val; break;
  case 14: fPosXWCMEntry         = val; break;
  }
}

void KinePart::SetMomAtCheckPoint(Int_t Point, TLorentzVector val) {
  switch (Point) {
  case  0: fMomCedarEntry        = val; break;
  case  1: fMomCedarExit         = val; break;
  case  2: fMomGigaTrackerEntry  = val; break;
  case  3: fMomGigaTrackerExit   = val; break;
  case  4: fMomSpectrometerEntry = val; break;
  case  5: fMomMNP33Entry        = val; break;
  case  6: fMomMNP33Exit         = val; break;
  case  7: fMomRICHMirrorEntry   = val; break;
  case  8: fMomCHODEntry         = val; break;
  case  9: fMomLKrEntry          = val; break;
  case 10: fMomMUV1Entry         = val; break;
  case 11: fMomMUV2Entry         = val; break;
  case 12: fMomFISCEntry         = val; break;
  case 13: fMomMUV3Entry         = val; break;
  case 14: fMomXWCMEntry         = val; break;
  }
}

TVector3 KinePart::GetPosAtCheckPoint(Int_t Point) const {
  switch (Point) {
  case -1: return fEndPos.Vect();
  case  0: return fPosCedarEntry;
  case  1: return fPosCedarExit;
  case  2: return fPosGigaTrackerEntry;
  case  3: return fPosGigaTrackerExit;
  case  4: return fPosSpectrometerEntry;
  case  5: return fPosMNP33Entry;
  case  6: return fPosMNP33Exit;
  case  7: return fPosRICHMirrorEntry;
  case  8: return fPosCHODEntry;
  case  9: return fPosLKrEntry;
  case 10: return fPosMUV1Entry;
  case 11: return fPosMUV2Entry;
  case 12: return fPosFISCEntry;
  case 13: return fPosMUV3Entry;
  case 14: return fPosXWCMEntry;
  case 15: return fEndPos.Vect();
  }
  return TVector3(0,0,0);
}

TLorentzVector KinePart::GetMomAtCheckPoint(Int_t Point) const {
  switch (Point) {
  case -1: return TLorentzVector(fInitialMomentum, fInitialEnergy);
  case  0: return fMomCedarEntry;
  case  1: return fMomCedarExit;
  case  2: return fMomGigaTrackerEntry;
  case  3: return fMomGigaTrackerExit;
  case  4: return fMomSpectrometerEntry;
  case  5: return fMomMNP33Entry;
  case  6: return fMomMNP33Exit;
  case  7: return fMomRICHMirrorEntry;
  case  8: return fMomCHODEntry;
  case  9: return fMomLKrEntry;
  case 10: return fMomMUV1Entry;
  case 11: return fMomMUV2Entry;
  case 12: return fMomFISCEntry;
  case 13: return fMomMUV3Entry;
  case 14: return fMomXWCMEntry;
  case 15: return TLorentzVector(fFinalMomentum, fFinalEnergy);
  }
  return TLorentzVector(0,0,0,0);
}

///////////
// Printout

void KinePart::Print(Option_t* option) const {
  std::cout <<
    "PDG " << fPDGcode << " Name " << fParticleName <<
    " ID "  << fID << " ParentID " << fParentID << " ParentIndex "    << fParentIndex << std::endl <<
    "  Prod " << fProdPos.X() << " " << fProdPos.Y() << " " << fProdPos.Z() << " mm " << fProdProcessName <<
    " E "   << fInitialEnergy << " MeV" <<
    " P "   << fInitialMomentum.X() << " " << fInitialMomentum.Y() << " " << fInitialMomentum.Z() << " MeV" << std::endl <<
    "  End  "  << fEndPos.X() << " " << fEndPos.Y() << " " << fEndPos.Z() << " mm " << fEndProcessName <<
    " E "   << fFinalEnergy << " MeV" <<
    " P "   << fFinalMomentum.X() << " " << fFinalMomentum.Y() << " " << fFinalMomentum.Z() << " MeV" << std::endl;
  if (TString(option).EqualTo("all")) PrintAllCheckPoints();
}

void KinePart::PrintCheckPoint(Int_t iCP) const {
  if (iCP<-1 || iCP>15) return;
  TVector3 v = GetPosAtCheckPoint(iCP);
  if (v.Z()<1.0) return; // no information at this checkpoint
  TLorentzVector p = GetMomAtCheckPoint(iCP);
  std::cout <<
    "  CP" <<iCP<<": "<< v.X() << " " << v.Y() << " " << v.Z() << " mm " <<
    " E "   << p.E() << " MeV" << " P "   << p.Px() << " " << p.Py() << " " << p.Pz() << " MeV" << std::endl;
}

void KinePart::PrintAllCheckPoints() const {
  for (Int_t i=0; i<15; i++) PrintCheckPoint(i);
}
