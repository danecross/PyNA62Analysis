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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef NA62VRootIO_H
#define NA62VRootIO_H 1

#include "globals.hh"
#include "G4Event.hh"
#include "TFile.h"
#include "TTree.h"
#include "NA62VNamed.hh"

class NA62VRootIO : public NA62VNamed {

public:

  NA62VRootIO(G4String);
  // In the concrete fInstance you need to implement
  // the mandatory virtual methods
  virtual ~NA62VRootIO();
  virtual void NewRun() = 0;
  virtual void EndRun() = 0;
  virtual void SaveEvent(const G4Event*) = 0;
  virtual void Close() = 0;

public:

  Int_t                GetBufSize()                                       { return fBufSize;                      };
  void                 SetBufSize(Int_t value)                            { fBufSize = value;                     };
  Int_t                GetBranchStyle()                                   { return fBranchStyle;                  };
  void                 SetBranchStyle(Int_t value)                        { fBranchStyle = value;                 };
  G4int                GetVerbose()                                       { return fVerbose;                      };
  void                 SetVerbose(G4int value)                            { fVerbose = value;                     };

protected:

  Int_t fBufSize;
  Int_t fBranchStyle;
  G4int fVerbose;
  TTree* fEventTree; // Tree to store Hits in one run
};

#endif
