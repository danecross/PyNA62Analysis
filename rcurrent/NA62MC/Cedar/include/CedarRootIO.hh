// --------------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
#ifndef CedarRootIO_HH
#define CedarRootIO_HH 1

#include "NA62VRootIO.hh"
#include "TTree.h"
#include "TBranch.h"
#include "globals.hh"

class TCedarEvent;
class CedarGeometryParameters;
class CedarMaterialParameters;

class CedarRootIO : public NA62VRootIO {

public:

  CedarRootIO();
  virtual ~CedarRootIO();

  static CedarRootIO* GetInstance();
  void NewRun();
  void EndRun();
  void SaveEvent(const G4Event*);
  void Close();

private:

  TBranch* fCedarBranch;
  TCedarEvent* fEvent;
  CedarGeometryParameters* fGeoPars;
  CedarMaterialParameters* fMatPars;
};

#endif
