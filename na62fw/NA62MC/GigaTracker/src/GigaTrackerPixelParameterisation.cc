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
// Created by Bob Velghe (bob.velghe@cern.ch) 2012-01-13
// --------------------------------------------------------------
//

#include "GigaTrackerPixelParameterisation.hh"

GigaTrackerPixelParameterisation::GigaTrackerPixelParameterisation() : fGeoPars(0) 
{
  fGeoPars = GigaTrackerGeometryParameters::GetInstance();
  fActiveSensorXLength = fGeoPars->GetGigaTrackerActiveSensorXLength(0);
  fActiveSensorYLength = fGeoPars->GetGigaTrackerActiveSensorYLength(0);
  fPixelZLength = fGeoPars->GetGigaTrackerPixelZLength();
}

void GigaTrackerPixelParameterisation::ComputeTransformation(G4int copyNo, G4VPhysicalVolume* physVol) const 
{
  // ! GetPixel{X,Y}Position() give the position relative to the corner of the sensor
  G4double Xpos = fGeoPars->GetPixelXPosition(copyNo)-0.5*fActiveSensorXLength; 
  G4double Ypos = fGeoPars->GetPixelYPosition(copyNo)-0.5*fActiveSensorYLength;
  G4double Zpos = 0;    
  physVol->SetTranslation(G4ThreeVector(Xpos,Ypos,Zpos));
  physVol->SetRotation(0);
}

void GigaTrackerPixelParameterisation::ComputeDimensions (G4Box & pixel, const G4int copyNo, const G4VPhysicalVolume *) const 
{
  pixel.SetXHalfLength(0.5*fGeoPars->GetPixelXLength(copyNo));
  pixel.SetYHalfLength(0.5*fGeoPars->GetPixelYLength(copyNo));
  pixel.SetZHalfLength(0.5*fPixelZLength);
}
