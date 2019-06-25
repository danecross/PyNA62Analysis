// LAVRootIO.hh
// --------------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// 2009-03-02 Emanuele Leonardi (Emanuele.Leonardi@roma1.infn.it)
//   - handle LAV-specific hit info
//
// --------------------------------------------------------------------
#ifndef LAVRootIO_HH
#define LAVRootIO_HH 1

#include "NA62VRootIO.hh"

#include "TTree.h"
#include "TBranch.h"

#include "globals.hh"

class TLAVEvent;
class LAVGeometryParameters;
class LAVMaterialParameters;

class LAVRootIO : public NA62VRootIO {
public:

  LAVRootIO();
  virtual ~LAVRootIO();

  static LAVRootIO* GetInstance();
  void NewRun();
  void EndRun();
  void SaveEvent(const G4Event*);
  void Close();

private:

  TBranch* fLAVBranch;
  TLAVEvent *fEvent;
  LAVGeometryParameters * fGeoPars;
  LAVMaterialParameters * fMatPars;

};
#endif // LAVRootIO_H
