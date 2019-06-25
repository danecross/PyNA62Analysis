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
//
// $Id: MCTruthTrackInformation.cc,v 1.1.1.1 2008/04/07 18:27:35 sergiant Exp $
// GEANT4 tag $Name:  $
//
//
// --------------------------------------------------------------
//      GEANT 4 - MCTruthTrackInformation class
// --------------------------------------------------------------
//
// Author: Witold POKORSKI (Witold.Pokorski@cern.ch)
//
// 2011-03-11 Spasimir Balev (Spasimir.Balev@cern.ch)
//            - Define interaction level
// --------------------------------------------------------------

#include "MCTruthTrackInformation.hh"
#include "G4ios.hh"

G4Allocator<MCTruthTrackInformation> aTrackInformationAllocator;

MCTruthTrackInformation::MCTruthTrackInformation() :
  originalTrackID(0),
  particleDefinition(nullptr),
  originalEnergy(-1),
  originalTime(-1),
  depthLevel(0),
  directParent(false),
  fSavedParentID(-1)
{
  //particleDefinition = 0;
  //originalPosition = G4ThreeVector(0.,0.,0.);
  //originalMomentum = G4ThreeVector(0.,0.,0.);
  //originalEnergy = 0.;
  //originalTime = 0.;
  for (G4int i=0; i<20; i++) {
    fPos[i] = TVector3(0,0,0);
    fMom[i] = TLorentzVector(0,0,0,0);
  }

}

MCTruthTrackInformation::MCTruthTrackInformation(const G4Track* aTrack) :
  originalTrackID (aTrack->GetTrackID()),
  particleDefinition(nullptr),
  originalEnergy(-1),
  originalTime(-1),
  depthLevel(-1),
  directParent(false),
  fSavedParentID(-1)
{
  //particleDefinition = aTrack->GetDefinition();
  //originalPosition = aTrack->GetPosition();
  //originalMomentum = aTrack->GetMomentum();
  //originalEnergy = aTrack->GetTotalEnergy();
  //originalTime = aTrack->GetGlobalTime();

  for (G4int i=0; i<20; i++) {
    fPos[i] = TVector3(0,0,0);
    fMom[i] = TLorentzVector(0,0,0,0);
  }
}

MCTruthTrackInformation::MCTruthTrackInformation(const MCTruthTrackInformation* aTrackInfo) :
  originalTrackID(aTrackInfo->originalTrackID),
  particleDefinition(nullptr),
  originalEnergy(-1),
  originalTime(-1),
  depthLevel(aTrackInfo->depthLevel),
  directParent(false),
  fSavedParentID(aTrackInfo->fSavedParentID)
{
  //particleDefinition = aTrackInfo->particleDefinition;
  //originalPosition = aTrackInfo->originalPosition;
  //originalMomentum = aTrackInfo->originalMomentum;
  //originalEnergy = aTrackInfo->originalEnergy;
  //originalTime = aTrackInfo->originalTime;
  for (G4int i=0; i<20; i++) {
    fPos[i] = TVector3(0,0,0);
    fMom[i] = TLorentzVector(0,0,0,0);
  }

}

MCTruthTrackInformation::~MCTruthTrackInformation() {}

void MCTruthTrackInformation::Print() const {
  G4cout << "Original track ID " << originalTrackID 
	 << " at " << originalPosition << G4endl;
}
