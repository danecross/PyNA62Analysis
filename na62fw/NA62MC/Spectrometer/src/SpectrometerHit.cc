//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// --------------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------------
#include "SpectrometerHit.hh"
#include "G4ios.hh"
#include "G4VVisManager.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4LogicalVolume.hh"
#include "G4Circle.hh"

G4Allocator<SpectrometerHit> SpectrometerHitAllocator;

SpectrometerHit::SpectrometerHit() :
  G4VHit(),
  fStrawID(0),
  fHalfViewID(0),
  fViewID(0),
  fChamberID(0),
  fTrackID(0),

  fWireDistance(0),
  fTime(0),
  fEnergy(0)
{;}

SpectrometerHit::~SpectrometerHit()
{;}

SpectrometerHit::SpectrometerHit(const SpectrometerHit &right) :
  G4VHit(),
  fStrawID(right.fStrawID),
  fHalfViewID(right.fHalfViewID),
  fViewID(right.fViewID),
  fChamberID(right.fChamberID),
  fTrackID(right.fTrackID),

  fWireDistance(right.fWireDistance),
  fTime(right.fTime),
  fEnergy(right.fEnergy),

  fPosition(right.fPosition),
  fLocalPosition(right.fLocalPosition),
  
  fDirection(right.fDirection)
{
}

SpectrometerHit& SpectrometerHit::operator=(const SpectrometerHit &right)
{
  fStrawID = right.fStrawID;
  fHalfViewID = right.fHalfViewID;
  fViewID = right.fViewID;
  fChamberID = right.fChamberID;
  fTrackID = right.fTrackID;

  fWireDistance = right.fWireDistance;
  fTime = right.fTime;
  fEnergy = right.fEnergy;

  fPosition = right.fPosition;
  fLocalPosition = right.fLocalPosition;
  
  fDirection = right.fDirection;

  return *this;
}

G4int SpectrometerHit::operator==(const SpectrometerHit &right) const
{
  return (this==&right)?1:0;
}

void SpectrometerHit::Draw()
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

void SpectrometerHit::Print()
{;}


