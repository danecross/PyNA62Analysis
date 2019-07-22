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
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// 
// Vito Palladino added the Strip and Ring ID     10.02.1009
// --------------------------------------------------------------------
#include "CHANTIHit.hh"
#include "G4ios.hh"
#include "G4VVisManager.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4LogicalVolume.hh"
#include "G4Circle.hh"

G4Allocator<CHANTIHit> CHANTIHitAllocator;

CHANTIHit::CHANTIHit() :
  G4VHit(),
  fChannelID(0),
  fTrackID(0),
  fParticleID(0),
  fRingID (0),
  fStripID(0),
  fTime(0),
  fEnergy(0)
{;}

CHANTIHit::~CHANTIHit()
{;}

CHANTIHit::CHANTIHit(const CHANTIHit &right) :
  G4VHit(),
  fChannelID(right.fChannelID),
  fTrackID(right.fTrackID),
  fParticleID(right.fParticleID),
  fRingID (right.fRingID),
  fStripID(right.fStripID),
  fTime(right.fTime),
  fEnergy(right.fEnergy),
  fPosition(right.fPosition)
{
}

CHANTIHit& CHANTIHit::operator=(const CHANTIHit &right)
{
  fChannelID = right.fChannelID;
  fTrackID = right.fTrackID;
  fParticleID = right.fParticleID;

  fTime = right.fTime;
  fEnergy = right.fEnergy;

  fPosition = right.fPosition;

  return *this;
}

G4int CHANTIHit::operator==(const CHANTIHit &right) const
{
  return (this==&right)?1:0;
}

void CHANTIHit::Draw()
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

void CHANTIHit::Print()
{;}


