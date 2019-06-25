#include "HACHit.hh"
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

G4Allocator<HACHit> HACHitAllocator;

HACHit::HACHit() :
  G4VHit(),
  fChannelID(0),
  fTrackID(0),
  fTime(0),
  fEnergy(0)
{
}

HACHit::~HACHit()
{;}

HACHit::HACHit(const HACHit &right) :
  G4VHit(),
  fChannelID(right.fChannelID),
  fTrackID(right.fTrackID),
  fTime(right.fTime),
  fEnergy(right.fEnergy),
  fPosition(right.fPosition)
{
}

HACHit& HACHit::operator=(const HACHit &right)
{
  fChannelID = right.fChannelID;
  fTrackID = right.fTrackID;

  fTime = right.fTime;
  fEnergy = right.fEnergy;

  fPosition = right.fPosition;

  return *this;
}

G4int HACHit::operator==(const HACHit &right) const
{
  return (this==&right)?1:0;
}

void HACHit::Draw()
{
  G4VVisManager* pVVisManager = G4VVisManager::GetConcreteInstance();
  if(pVVisManager)
  {
    G4Colour colour(1.,0.,0.);
    G4VisAttributes attribs(colour);
    attribs.SetForceWireframe(false);
    attribs.SetForceSolid(true);
    G4Circle circle(fPosition);
    circle.SetVisAttributes(attribs);
    pVVisManager->Draw(circle);
  }
}

void HACHit::Print()
{;}


