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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2008-03-11
//            Francesca Bucci (Francesca.Bucci@cern.ch) 
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// Modified (MUV -> MUV1) Rainer Wanke              2010-11-26
//
// Modified Mario Vormstein (mario.vormstein@uni-mainz.de)  2011-06-27
// --------------------------------------------------------------------
#include "MUV1Hit.hh"
#include "G4ios.hh"
#include "G4VVisManager.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4LogicalVolume.hh"
#include "G4Circle.hh"

G4Allocator<MUV1Hit> MUV1HitAllocator;

MUV1Hit::MUV1Hit() :
	G4VHit(),
	fChannelID(0),
	fScintillatorID(0),
	fTrackID(0),
	fTime(0),
	fEnergy(0),
	fStepLength(),
	fPhotons(),
	fPositionInScintillator(0)
{
}

MUV1Hit::~MUV1Hit() {
	;
}

MUV1Hit::MUV1Hit(const MUV1Hit &right) :
	G4VHit(),
	fChannelID(right.fChannelID),
	fScintillatorID(right.fScintillatorID),
	fTrackID(right.fTrackID),
	fTime(right.fTime),
	fEnergy(right.fEnergy),
	fStepLength(right.fStepLength),
	fPhotons(right.fPhotons),
	fPosition(right.fPosition),
	fPositionInScintillator(right.fPositionInScintillator)
{
}

MUV1Hit& MUV1Hit::operator=(const MUV1Hit &right) {
	fChannelID = right.fChannelID;
	fTrackID = right.fTrackID;

	fTime = right.fTime;
	fEnergy = right.fEnergy;


	fPosition = right.fPosition;
	fPositionInScintillator = right.fPositionInScintillator;
	fPhotons = right.fPhotons;
	fStepLength = right.fStepLength;

	return *this;
}

G4int MUV1Hit::operator==(const MUV1Hit &right) const {
	return (this == &right) ? 1 : 0;
}

void MUV1Hit::Draw() {

	G4VVisManager* pVVisManager = G4VVisManager::GetConcreteInstance();
	if (pVVisManager) {
		G4Colour colour(1., 0., 0.);
		G4VisAttributes attribs(colour);
		attribs.SetForceWireframe(false);
		attribs.SetForceSolid(true);
		G4Circle circle(fPosition);
		circle.SetVisAttributes(attribs);
		pVVisManager->Draw(circle);
	}

}

void MUV1Hit::Print() {
	/*
	G4cout << " MUV1 hit " << "ChannelID: " << fChannelID << "\n TrackID: "
			<< fTrackID << "\nZeit " << fTime << "ns \nEnergie: " << fEnergy
			<< "MeV \nPosition: " << fPosition << "mm \nVolumen: "
			<< fVolumeName << G4endl;
			*/
}

G4double MUV1Hit::SumUp(G4double summenPuffer) {

	summenPuffer += fEnergy;
	return summenPuffer;
}

G4double MUV1Hit::ChannelSum(G4double fChannel)
// Looking for each channel to sum up the energy
{
	G4double fChannelEnergy = 0;
	for (int i = 0; i < 12; i++) {
		if (fChannelID - i * 100 == fChannel) {
			fChannelEnergy += fEnergy;
		}
	}
	return fChannelEnergy;
}

void MUV1Hit::AddHit (G4ThreeVector HitPosition, G4double Energy, G4double Time, G4double PositionOfHitInScintillatorFrame, G4int TrackID){
    
    fPosition = (HitPosition*Energy + fPosition*fEnergy)/(Energy+fEnergy);
    fTime = (fTime*fEnergy + Time*Energy)/(fEnergy+Energy);
    fPositionInScintillator = (fPositionInScintillator*fEnergy + PositionOfHitInScintillatorFrame*Energy)/(fEnergy+Energy);
    fTrackID = TrackID;
    fEnergy += Energy;
    
}
