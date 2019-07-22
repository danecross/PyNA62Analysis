#include "RICHPMTHit.hh"
#include "G4ios.hh"
#include "G4VVisManager.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4Circle.hh"

G4Allocator<RICHPMTHit> RICHPMTHitAllocator;

RICHPMTHit::RICHPMTHit():
  G4VHit(),
  fTime(0),
  fEnergy(0),
  fTrackID(0),
  fPositionID(0)
{;}

RICHPMTHit::~RICHPMTHit()
{;}

RICHPMTHit::RICHPMTHit(const RICHPMTHit &right) :
  G4VHit(),
  fPosition(right.fPosition),
  fTime(0),
  fEnergy(0),
  fTrackID(0),
  fPositionID(0)
{
}

RICHPMTHit& RICHPMTHit::operator=(const RICHPMTHit &right)
{
  fPosition = right.fPosition;
  return *this;
}

G4int RICHPMTHit::operator==(const RICHPMTHit &right) const
{
  return (this==&right) ? 1 : 0;
}

void RICHPMTHit::Draw()
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

void RICHPMTHit::Print()
{;}


