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
// --------------------------------------------------------------
// History:
//
// Created by Bob Velghe (bob.velghe@cern.ch) 2012-01-09
// --------------------------------------------------------------
//

#include "GigaTrackerBumpBondingParameterisation.hh"

GigaTrackerBumpBondingParameterisation::GigaTrackerBumpBondingParameterisation() 
{
  fGeoPars = GigaTrackerGeometryParameters::GetInstance();
  fSensorXLength = fGeoPars->GetGigaTrackerSensorXLength(0);
  fSensorYLength = fGeoPars->GetGigaTrackerSensorYLength(0);
}

void GigaTrackerBumpBondingParameterisation::ComputeTransformation(G4int iCopy, G4VPhysicalVolume* physVol) const 
{
  //Compute bump bond position
  G4double Xpos = fGeoPars->GetBumpBondXPosition(iCopy)-0.5*fSensorXLength; 
  G4double Ypos = fGeoPars->GetBumpBondYPosition(iCopy)-0.5*fSensorYLength;
  G4double Zpos = 0;
  physVol->SetTranslation(G4ThreeVector(Xpos,Ypos,Zpos));
  physVol->SetRotation(0);
}
