// --------------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
#include "CedarHit.hh"
#include "G4ios.hh"
#include "G4VVisManager.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4LogicalVolume.hh"
#include "G4Circle.hh"

G4Allocator<CedarHit> CedarHitAllocator;

CedarHit::CedarHit() :
  G4VHit(),
  fTime      (0),
  fEnergy    (0),
  fTrackID   (0),
  fPositionID(0),
  iPMType    (0)
{}

CedarHit::~CedarHit() {}

CedarHit::CedarHit(const CedarHit &right) :
  G4VHit(),
  fPosition  (right.fPosition),
  fTime      (right.fTime),
  fEnergy    (right.fEnergy),
  fTrackID   (right.fTrackID),
  fPositionID(right.fPositionID),
  iPMType    (right.iPMType)
{
}

CedarHit& CedarHit::operator=(const CedarHit &right) {
  fPositionID = right.fPositionID;
  fTrackID    = right.fTrackID;
  fTime       = right.fTime;
  fEnergy     = right.fEnergy;
  fPosition   = right.fPosition;
  return *this;
}

G4int CedarHit::operator==(const CedarHit &right) const {
  return (this==&right) ? 1 : 0;
}

void CedarHit::Draw() {
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

void CedarHit::Print() {}
