#include "MUV0Hit.hh"
#include "G4ios.hh"
#include "G4VVisManager.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4LogicalVolume.hh"
#include "G4Circle.hh"
//
// --------------------------------------------------------------------
// History:
//
// Created by Giuseppe Ruggiero 04-09-2012 
//
// --------------------------------------------------------------------

/// \class MUV0Hit
/// \Brief
/// MUV0 MC Hit: position, energy, channel ID, etc
/// \EndBrief

G4Allocator<MUV0Hit> MUV0HitAllocator;

MUV0Hit::MUV0Hit() :
  fChannelID(-99),
  fTrackID (-99),
  fTime(0.0),
  fEnergy (0.0)
{
}

MUV0Hit::MUV0Hit(const MUV0Hit &right) :
  G4VHit(),
  fChannelID (right.fChannelID),
  fTrackID   (right.fTrackID),
  fTime      (right.fTime),
  fEnergy    (right.fEnergy),
  fPosition  (right.fPosition)
{
}

MUV0Hit& MUV0Hit::operator=(const MUV0Hit &right) {
  fChannelID = right.fChannelID;
  fTrackID   = right.fTrackID;
  fTime       = right.fTime;
  fEnergy     = right.fEnergy;
  fPosition   = right.fPosition;
  return *this;
}

G4int MUV0Hit::operator==(const MUV0Hit &right) const {
  return (this==&right) ? 1 : 0;
}

void MUV0Hit::Draw() {
  G4VVisManager* pVVisManager = G4VVisManager::GetConcreteInstance();
  if (pVVisManager) {
    G4Colour colour(1.,0.,0.);
    G4VisAttributes attribs(colour);
    attribs.SetForceWireframe(false);
    attribs.SetForceSolid(true);
    G4Circle circle(fPosition);
    circle.SetVisAttributes(attribs);
    pVVisManager->Draw(circle);
  }
}
