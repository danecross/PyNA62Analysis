// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Giuseppe Ruggiero (giuseppe.ruggiero@cern.ch) 2011-05-04
// Modified by Giuseppe Ruggiero (giuseppe.ruggiero@cern.ch) 2015-21-04
//
// --------------------------------------------------------------

#include "TRecoSpectrometerCandidate.hh"
#include "BlueTubeTracker.hh"

/// \class TRecoSpectrometerCandidate
/// \Brief
/// Class containing a reconstructed track.
/// \EndBrief
///
/// \Detailed
/// This class stores the reconstructed tracks. The class returns the following track variables:
///  -# TRecoSpectrometerCandidate::GetSlopeXBeforeFit()
///  -# TRecoSpectrometerCandidate::GetSlopeYBeforeFit()
///  -# TRecoSpectrometerCandidate::GetPositionBeforeFit()
///  -# TRecoSpectrometerCandidate::GetMomentumBeforeFit()
///  -# TRecoSpectrometerCandidate::GetCombinationTotalQuality()
///  -# TRecoSpectrometerCandidate::GetCombinationHoughQuality()
///  -# TRecoSpectrometerCandidate::GetSlopeXBeforeMagnet()
///  -# TRecoSpectrometerCandidate::GetSlopeYBeforeMagnet()
///  -# TRecoSpectrometerCandidate::GetPositionBeforeMagnet()
///  -# TRecoSpectrometerCandidate::GetSlopeXAfterMagnet()
///  -# TRecoSpectrometerCandidate::GetSlopeYAfterMagnet()
///  -# TRecoSpectrometerCandidate::GetPositionAfterMagnet()
///  -# TRecoSpectrometerCandidate::GetMomentum()
///  -# TRecoSpectrometerCandidate::GetCharge()
///  -# TRecoSpectrometerCandidate::GetChi2()
///  -# TRecoSpectrometerCandidate::GetNChambers()
///  -# TRecoSpectrometerCandidate::GetChamberId(Int_t i)   i from 1 to 4 (Chambers)
///  -# TRecoSpectrometerCandidate::GetNTotalHitPerChamber(Int_t i)   i from 1 to 4 (Chambers)
///  -# TRecoSpectrometerCandidate::GetNViewsPerChamber(Int_t i)   i from 1 to 4 (Chambers)
///  -# TRecoSpectrometerCandidate::GetN2HitClusterPerChamber(Int_t i)   i from 1 to 4 (Chambers)
///  -# TRecoSpectrometerCandidate::GetN3HitClusterPerChamber(Int_t i)   i from 1 to 4 (Chambers)
///  -# TRecoSpectrometerCandidate::GetCovariance(Int_t i, Int_t j) i,j from 1 to 5 (Track parameters)
/// \EndDetailed

ClassImp(TRecoSpectrometerCandidate)

TRecoSpectrometerCandidate::TRecoSpectrometerCandidate() : TRecoVCandidate() {
   fSlopeXBeforeFit = -9999.;
   fSlopeYBeforeFit = -9999.;
   fPositionBeforeFit.SetXYZ(-9999,-9999,-9999);
   fMomentumBeforeFit = -9999.;
   fCombinationTotalQuality = -9999;
   fCombinationHoughQuality = -9999;
   fSlopeXBeforeMagnet = -9999.;
   fSlopeYBeforeMagnet = -9999.;
   fPositionBeforeMagnet.SetXYZ(-9999,-9999,-9999);
   fSlopeXAfterMagnet = -9999.;
   fSlopeYAfterMagnet = -9999.;
   fPositionAfterMagnet.SetXYZ(-9999,-9999,-9999);
   fMomentum = -9999.;
   fCharge = 0;
   fChi2 = 0;
   fLeadingTime = 0;
   for (Int_t j=0; j<4; j++) {
     fChamberId[j]=0;
     fNTotalHitPerChamber[j]=0;
     fNViewsPerChamber[j]=0;
     fN2HitClusterPerChamber[j]=0;
     fN3HitClusterPerChamber[j]=0;
   }
   for (Int_t ix=0; ix<5; ix++) {
     for (Int_t iy=0; iy<5; iy++) {
       fCovariance[ix][iy] = 0.;
     }
   }
}

void TRecoSpectrometerCandidate::Clear(Option_t* option){
   TRecoVCandidate::Clear(option);
   fSlopeXBeforeFit = -9999.;
   fSlopeYBeforeFit = -9999.;
   fPositionBeforeFit.SetXYZ(-9999,-9999,-9999);
   fMomentumBeforeFit = -9999.;
   fCombinationTotalQuality = -9999;
   fCombinationHoughQuality = -9999;
   fSlopeXBeforeMagnet = -9999.;
   fSlopeYBeforeMagnet = -9999.;
   fPositionBeforeMagnet.SetXYZ(-9999,-9999,-9999);
   fSlopeXAfterMagnet = -9999.;
   fSlopeYAfterMagnet = -9999.;
   fPositionAfterMagnet.SetXYZ(-9999,-9999,-9999);
   fMomentum = -9999.;
   fCharge = 0;
   fChi2 = 0;
   for (Int_t j=0; j<4; j++) {
     fChamberId[j]=0;
     fNTotalHitPerChamber[j]=0;
     fNViewsPerChamber[j]=0;
     fN2HitClusterPerChamber[j]=0;
     fN3HitClusterPerChamber[j]=0;
   }
   for (Int_t ix=0; ix<5; ix++) {
     for (Int_t iy=0; iy<5; iy++) {
       fCovariance[ix][iy] = 0.;
     }
   }
}

Double_t TRecoSpectrometerCandidate::xAtBeforeMagnet(Double_t z) {
  return fPositionBeforeMagnet.X()+ fSlopeXBeforeMagnet*(z-fPositionBeforeMagnet.Z());
}

Double_t TRecoSpectrometerCandidate::yAtBeforeMagnet(Double_t z) {
  return fPositionBeforeMagnet.Y()+ fSlopeYBeforeMagnet*(z-fPositionBeforeMagnet.Z());
}

Double_t TRecoSpectrometerCandidate::xAtAfterMagnet(Double_t z) {
  return fPositionAfterMagnet.X()+ fSlopeXAfterMagnet*(z-fPositionAfterMagnet.Z());
}

Double_t TRecoSpectrometerCandidate::yAtAfterMagnet(Double_t z) {
  return fPositionAfterMagnet.Y()+ fSlopeYAfterMagnet*(z-fPositionAfterMagnet.Z());
}

Double_t TRecoSpectrometerCandidate::xAt(Double_t z) {
  return (z<196995.0) ? xAtBeforeMagnet(z) : xAtAfterMagnet(z);
}

Double_t TRecoSpectrometerCandidate::yAt(Double_t z) {
  return (z<196995.0) ? yAtBeforeMagnet(z) : yAtAfterMagnet(z);
}

TVector3 TRecoSpectrometerCandidate::GetThreeMomentumBeforeMagnet() {
  Double_t Norm =
    1.0/sqrt(1.0 + fSlopeXBeforeMagnet*fSlopeXBeforeMagnet + fSlopeYBeforeMagnet*fSlopeYBeforeMagnet);
  return
    TVector3(fMomentum*Norm*fSlopeXBeforeMagnet, fMomentum*Norm*fSlopeYBeforeMagnet, fMomentum*Norm);
}

TVector3 TRecoSpectrometerCandidate::GetThreeMomentumAfterMagnet() {
  Double_t Norm =
    1.0/sqrt(1.0 + fSlopeXAfterMagnet*fSlopeXAfterMagnet + fSlopeYAfterMagnet*fSlopeYAfterMagnet);
  return
    TVector3(fMomentum*Norm*fSlopeXAfterMagnet, fMomentum*Norm*fSlopeYAfterMagnet, fMomentum*Norm);
}

TVector3 TRecoSpectrometerCandidate::GetBlueFieldCorrectedPositionAtZ(Double_t z) {
  BlueTubeTracker::GetInstance()->SetCharge(fCharge);
  BlueTubeTracker::GetInstance()->SetInitialPosition(fPositionBeforeMagnet);
  BlueTubeTracker::GetInstance()->SetInitialMomentum(GetThreeMomentumBeforeMagnet());
  BlueTubeTracker::GetInstance()->SetZFinal(z);
  BlueTubeTracker::GetInstance()->TrackParticle();
  return BlueTubeTracker::GetInstance()->GetFinalPosition();
}

TVector3 TRecoSpectrometerCandidate::GetBlueFieldCorrectedMomentumAtZ(Double_t z) {
  BlueTubeTracker::GetInstance()->SetCharge(fCharge);
  BlueTubeTracker::GetInstance()->SetInitialPosition(fPositionBeforeMagnet);
  BlueTubeTracker::GetInstance()->SetInitialMomentum(GetThreeMomentumBeforeMagnet());
  BlueTubeTracker::GetInstance()->SetZFinal(z);
  BlueTubeTracker::GetInstance()->TrackParticle();
  return BlueTubeTracker::GetInstance()->GetFinalMomentum();
}
