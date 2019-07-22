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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-03-12
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
//
// Changes to MUV2 by ykohl in March 2010
//
// --------------------------------------------------------------------
#ifndef MUV2RootIO_HH
#define MUV2RootIO_HH 1

#include "NA62VRootIO.hh"

#include "TTree.h"
#include "TBranch.h"
#include "globals.hh"

class TMUV2Event;
class MUV2GeometryParameters;
class MUV2MaterialParameters;

class MUV2RootIO : public NA62VRootIO {
public:

  MUV2RootIO();
  virtual ~MUV2RootIO();

  static MUV2RootIO* GetInstance();
  void NewRun();
  void EndRun();
  void SaveEvent( const G4Event* );
  void Close();

private:

  TBranch    *fMUV2Branch;
  TMUV2Event *fEvent;
  MUV2GeometryParameters *fGeoPars;
  MUV2MaterialParameters *fMatPars;
};

#endif