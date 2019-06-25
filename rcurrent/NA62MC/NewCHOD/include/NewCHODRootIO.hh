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
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// --------------------------------------------------------------------
#ifndef NewCHODRootIO_HH
#define NewCHODRootIO_HH 1

#include "NA62VRootIO.hh"

#include "TTree.h"
#include "TBranch.h"
#include "globals.hh"

class TNewCHODEvent;
class NewCHODGeometryParameters;
class NewCHODMaterialParameters;

class NewCHODRootIO : public NA62VRootIO {
public:

  NewCHODRootIO();
  virtual ~NewCHODRootIO();

  static NewCHODRootIO* GetInstance();
  void NewRun();
  void EndRun();
  void SaveEvent(const G4Event*);
  void Close() {}

private:

  TBranch* fNewCHODBranch;
  TNewCHODEvent* fEvent;
  NewCHODGeometryParameters* fGeoPars;
  NewCHODMaterialParameters* fMatPars;

};
#endif
