// LAVHit.cc
// --------------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// 2009-03-02 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - First implementation of LAV hit structure
// 2010-03-15 - Domenico Di Filippo (difilippo@na.infn.it)
//   - Optical photons on photocatode
// 2011-01-24 - Domenico Di Filippo (difilippo@na.infn.it)
//   - Switch to std::vector to store optical photons data
//
// --------------------------------------------------------------------
#include "LAVHit.hh"
#include "G4ios.hh"
#include "G4VVisManager.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4LogicalVolume.hh"
#include "G4Circle.hh"

G4Allocator<LAVHit> LAVHitAllocator;

LAVHit::LAVHit() :
  G4VHit(),
  fChannelID(0),
  fTrackID(0),
  fTime(0),
  fEnergy(0),
  fBeta(0),
  fStepLength(0)
{;}

LAVHit::~LAVHit()
{;}

LAVHit::LAVHit(const LAVHit &right) :
  G4VHit(),
  fChannelID(right.fChannelID),
  fTrackID(right.fTrackID),
  fTime(right.fTime),
  fEnergy(right.fEnergy),
  fPosition(right.fPosition),
  fLocalPosition(right.fLocalPosition),
  fLocalDirection(right.fLocalDirection),
  fBeta(right.fBeta),
  fStepLength(right.fStepLength)
{
  for (unsigned int i=0; i<right.fPhotonsEnergy.size(); i++)
     fPhotonsEnergy[i] = right.fPhotonsEnergy[i];
  for (unsigned int i=0; i<right.fPhotonsTime.size(); i++)
     fPhotonsTime[i] = right.fPhotonsTime[i];

}

LAVHit& LAVHit::operator=(const LAVHit &right)
{

  fChannelID = right.fChannelID;
  fTrackID = right.fTrackID;

  fTime = right.fTime;
  fEnergy = right.fEnergy;

  fPosition = right.fPosition;

  fLocalPosition = right.fLocalPosition;
  fLocalDirection = right.fLocalDirection;
  fBeta = right.fBeta;
  fStepLength = right.fStepLength;

  for (unsigned int i=0; i<right.fPhotonsEnergy.size(); i++)
     fPhotonsEnergy[i] = right.fPhotonsEnergy[i];
  for (unsigned int i=0; i<right.fPhotonsTime.size(); i++)
     fPhotonsTime[i] = right.fPhotonsTime[i];

  return *this;

}

G4int LAVHit::operator==(const LAVHit &right) const
{
  return (this==&right)?1:0;
}

void LAVHit::Draw()
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

void LAVHit::Print()
{
  G4cout << "LAV hit "
	 << fChannelID << " " << fTrackID << " " << fTime << "ns "
	 << fEnergy << "MeV " << fPosition << "mm " << fLocalPosition << "mm "
	 << fLocalDirection << " " << fBeta << " " << fStepLength << "mm"
	 << " Optical Photons " << fPhotonsEnergy.size() << G4endl;

  for (unsigned int i=0; i<fPhotonsEnergy.size(); i++){
    G4cout << "photon " << i << " E = " << fPhotonsEnergy[i] << " T = " << fPhotonsTime[i] << G4endl;
  }

}
