// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Giuseppe Ruggiero (giuseppe.ruggiero@cern.ch) 2011-05-04
//
// --------------------------------------------------------------
#include "TRecoSpectrometerHit.hh"

/// \class TRecoSpectrometerHit
/// \Brief
/// Class containing a reconstructed hit.
/// \EndBrief
///
/// \Detailed
/// This class contains tube-hits after digitization and absolute coordinate reconstruction
/// as provided in the SpectrometerDigitizer and SpectrometerReconstruction classes, respectively.
/// \EndDetailed

ClassImp(TRecoSpectrometerHit)

TRecoSpectrometerHit::TRecoSpectrometerHit() :
  TRecoVHit(),
  fTime            (0),
  fEnergy          (0),
  fStrawID         (0),
  fPlaneID         (0),
  fRadius          (0),
  fRecoID          (0),
  fID              (0),
  fTDCID           (0),
  fMCID            (0),
  fChannelID       (0),
  fSingle          (0),
  fEdgeStatus      (0),
  fHalfViewID(0),
  fViewID(0),
  fChamberID(0),
  fWireAverageDistance(0),
  fTimeWidth       (0),
  fWireDistance(0),
  fNUsedDigis(0)
{
}

TRecoSpectrometerHit::TRecoSpectrometerHit(const TRecoSpectrometerHit& right) :
  TRecoVHit(right),
  fLocalPosition   (right.fLocalPosition),
  fPosition        (right.fPosition),
  fOriginalPosition(right.fOriginalPosition),
  fTime            (right.fTime),
  fEnergy          (right.fEnergy),
  fStrawID         (right.fStrawID),
  fPlaneID         (right.fPlaneID),
  fRadius          (right.fRadius),
  fRecoID          (right.fRecoID),
  fID              (right.fID),
  fTDCID           (right.fTDCID),
  fMCID            (right.fMCID),
  fChannelID       (right.fChannelID),
  fSingle          (right.fSingle),
  fEdgeStatus      (right.fEdgeStatus),
  fDirection(right.fDirection),
  fHalfViewID(right.fHalfViewID),
  fViewID(right.fViewID),
  fChamberID(right.fChamberID),
  fWireAverageDistance(right.fWireAverageDistance),
  fTimeWidth       (right.fTimeWidth),
  fWireDistance(right.fWireDistance),
  fNUsedDigis(0)
{
}

void TRecoSpectrometerHit::Clear(Option_t* option){
  TRecoVHit::Clear(option);
}
