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
#include "IRCSD.hh"
#include "IRCHit.hh"
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
#include "MCTruthTrackInformation.hh"
#include "IRCGeometryParameters.hh"

IRCSD::IRCSD(G4String name,G4String colName)
:G4VSensitiveDetector(name) {
  G4String HCname;
  collectionName.insert(HCname=colName);
  ReadGeometryParameters();
}

IRCSD::~IRCSD() {
  delete[] fHitArray;
}

void IRCSD::ReadGeometryParameters() {
  IRCGeometryParameters* GeoPars = IRCGeometryParameters::GetInstance();
  fSDnSegmentsX = GeoPars->GetSDnSegmentsX();
  fSDnSegmentsY = GeoPars->GetSDnSegmentsY();
  fSDnSegmentsZ = GeoPars->GetSDnSegmentsZ();
  fHitArraySize = fSDnSegmentsX*fSDnSegmentsY*fSDnSegmentsZ;
  fHitArray = new IRCHit*[fHitArraySize];
  for(int i=0; i<fHitArraySize; ++i) fHitArray[i] = nullptr;

  fIRCLengthX = 2.*GeoPars->GetIRCStation1OuterRadius();
  fIRCLengthY = 2.*GeoPars->GetIRCStation1OuterRadius();
  fIRCLengthZ = GeoPars->GetIRCDetectorTotalLength();
  fIRCFront = GeoPars->GetIRCDetectorZFrontPosition();
}

void IRCSD::Initialize(G4HCofThisEvent*HCE) {
  static int HCID = -1;
  Collection = new IRCHitsCollection(SensitiveDetectorName,collectionName[0]);
  verboseLevel = 0;
  nHits=0;
  if(HCID<0)
    { HCID = GetCollectionID(0); }
  HCE->AddHitsCollection( HCID, Collection );

  for(G4int i=0;i<fHitArraySize;i++)
    if(fHitArray[i] != 0) fHitArray[i]=0;
}

G4bool IRCSD::ProcessHits(G4Step*aStep,G4TouchableHistory*) {

  G4double edep = aStep->GetTotalEnergyDeposit();
  if(edep==0.) return false;
  //G4TouchableHistory* TouchableHistory = (G4TouchableHistory*)(aStep->GetPreStepPoint()->GetTouchable());

  G4int TrackID = aStep->GetTrack()->GetTrackID();
  G4ThreeVector HitPosition = aStep->GetPreStepPoint()->GetPosition();
  
  /*
  IRCHit* ircHit = new IRCHit();
  ircHit->SetTrackID(TrackID);
  ircHit->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
  ircHit->SetEnergy(edep);
  ircHit->SetPosition(HitPosition);
  Collection->insert( ircHit );
  nHits++;
  */
  G4int ChannelID;
  if(fSDnSegmentsX != 2 || fSDnSegmentsY != 2 ) {
    //The segmentation doesn't correspond the physical 4 PMTs (2x2)
    //Keep the linear channel ordering
    // Save hits in array
    G4double stepX = fIRCLengthX/(G4double)fSDnSegmentsX;
    G4double stepY = fIRCLengthY/(G4double)fSDnSegmentsY;
    G4double stepZ = fIRCLengthZ/(G4double)fSDnSegmentsZ;

    G4int iHit = (G4int)((HitPosition.x()+fIRCLengthX/2.)/stepX);
    G4int jHit = (G4int)((HitPosition.y()+fIRCLengthY/2.)/stepY);
    G4int kHit = (G4int)((HitPosition.z()-fIRCFront)/stepZ);
    if(iHit<0) iHit=0;
    if(jHit<0) jHit=0;
    if(kHit<0) kHit=0;
    if(iHit>(fSDnSegmentsX-1)) iHit = fSDnSegmentsX-1;
    if(jHit>(fSDnSegmentsY-1)) jHit = fSDnSegmentsY-1;
    if(kHit>(fSDnSegmentsZ-1)) kHit = fSDnSegmentsZ-1;
    /*
    G4ThreeVector HitPositionToStore = G4ThreeVector
      (-fIRCLengthX/2. + (G4double)iHit*stepX + stepX/2.,
       -fIRCLengthY/2. + (G4double)jHit*stepY + stepY/2.,
       fIRCFront + (G4double)kHit*stepZ + stepZ/2.);
    */
    ChannelID = fSDnSegmentsX*fSDnSegmentsY*kHit + fSDnSegmentsX*jHit + iHit;
  }
  else {
    //The segmentation corresponds to the physical division of the SAC with 4 PMTs
    //Use the online/reco channel mapping with a stupid set of IFs...
    if(HitPosition.x() >= 0 && HitPosition.y() >= 0 ) ChannelID = 0;
    if(HitPosition.x()  < 0 && HitPosition.y() >= 0 ) ChannelID = 1;
    if(HitPosition.x()  < 0 && HitPosition.y()  < 0 ) ChannelID = 2;
    if(HitPosition.x() >= 0 && HitPosition.y()  < 0 ) ChannelID = 3;
  }

  if(fHitArray[ChannelID] == 0) {
    fHitArray[ChannelID] = new IRCHit;
    fHitArray[ChannelID]->SetTrackID(TrackID);
    fHitArray[ChannelID]->SetChannelID(ChannelID);
    fHitArray[ChannelID]->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
    fHitArray[ChannelID]->SetPosition(HitPosition);
    fHitArray[ChannelID]->SetTime(0.);
    Collection->insert(fHitArray[ChannelID] );
    nHits++; 
  }
  fHitArray[ChannelID]->SetTime( (aStep->GetPreStepPoint()->GetGlobalTime()* edep +
		  fHitArray[ChannelID]->GetEnergy()*	fHitArray[ChannelID]->GetTime())
		  / (edep + fHitArray[ChannelID]->GetEnergy()) );
  fHitArray[ChannelID]->AddEnergy(edep);

  return true;
}

void IRCSD::EndOfEvent(G4HCofThisEvent*) {}

void IRCSD::clear() {}

void IRCSD::DrawAll() {}

void IRCSD::PrintAll(){}




