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
// $Id: MCTruthConfig.cc,v 1.1.1.1 2008/04/07 18:27:35 sergiant Exp $
// GEANT4 tag $Name:  $
//
//
// --------------------------------------------------------------
//      GEANT 4 - MCTruthConfig class
// --------------------------------------------------------------
//
// Author: Witold POKORSKI (Witold.Pokorski@cern.ch)
//
// --------------------------------------------------------------

#include "MCTruthConfig.hh"

MCTruthConfig::MCTruthConfig() :
  fSaveTracksForRegeneration(false),
  fSaveAllTracks(true),
  fSaveAllSteps(true),
  fZMin(0.0),
  fZMax(500000.0),
  fEMin(0.0),
  fMaxInteractionLevel(0),
  fSaveTrackVerbose(false)
{
  fBeamParticleEndProcessToReject.clear();
  fParticleTypeToSave.clear();
  fParticleTypeToReject.clear();
  fParticleCreatorProcessToSave.clear();
  fParticleCreatorProcessToReject.clear();
  fParticleEndProcessToSave.clear();
  fParticleEndProcessToReject.clear();
}
