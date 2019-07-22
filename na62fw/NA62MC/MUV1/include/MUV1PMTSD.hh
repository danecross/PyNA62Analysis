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
// --------------------------------------------------------------------
#ifndef MUV1PMTSD_h
#define MUV1PMTSD_h 1

#include "G4VSensitiveDetector.hh"
#include "MUV1Hit.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class MUV1PMTSD: public G4VSensitiveDetector {

public:

	MUV1PMTSD(G4String name, G4String colName);
	~MUV1PMTSD();

	//This function will be called automatically at the beginning of each event
	void Initialize(G4HCofThisEvent *HCE);

	//This function will be called whenever a particle passes through the detector
	G4bool ProcessHits(G4Step *aStep, G4TouchableHistory*);

	//This function will be called automatically at the end of each event
	void EndOfEvent(G4HCofThisEvent *HCE);
	//PhtonsLeft calculates the photons in "left" (-x) PMT
	void clear();
	void DrawAll();
	void PrintAll();

private:

	MUV1HitsCollection *PMTCollection;
	MUV1Hit* HitMap[500];
	G4int nHits;
	G4int nChannelHits;
	int HCID;
	G4int fPhoton;



};

#endif

