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
// --------------------------------------------------------------------
#include "CHANTISDSiPM.hh"
#include "CHANTIHit.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4Track.hh"
#include "G4Step.hh"
#include "G4ParticleDefinition.hh"
#include "G4VTouchable.hh"
#include "G4TouchableHistory.hh"
#include "G4ios.hh"
#include "G4SDManager.hh"
#include "G4HCofThisEvent.hh"
#include "G4ParticleTypes.hh"
#include "G4VSolid.hh"

CHANTISDSiPM::CHANTISDSiPM(G4String name,G4String ):G4VSensitiveDetector(name)
{;}

CHANTISDSiPM::~CHANTISDSiPM()
{;}

void CHANTISDSiPM::Initialize(G4HCofThisEvent*)
{;}

G4bool CHANTISDSiPM::ProcessHits(G4Step*aStep,G4TouchableHistory*)
{
  aStep->GetTrack()->SetTrackStatus(fStopAndKill);
   G4TrackVector *v = aStep->GetfSecondary();
   G4TrackVector::iterator i=v->begin();
   while (i != v->end()){
     delete *i;
     // cppcheck-suppress eraseDereference symbolName=i
     v->erase(i);
   }
  return false;
}

void CHANTISDSiPM::EndOfEvent(G4HCofThisEvent*)
{}

void CHANTISDSiPM::clear()
{} 


void CHANTISDSiPM::DrawAll()
{} 

void CHANTISDSiPM::PrintAll()
{} 





