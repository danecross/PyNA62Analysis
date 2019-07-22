#ifndef HACRootIO_HH
#define HACRootIO_HH 1

#include "NA62VRootIO.hh"

#include "TTree.h"
#include "TBranch.h"

#include "globals.hh"

class THACEvent;
class HACGeometryParameters;
class HACMaterialParameters;

class HACRootIO : public NA62VRootIO {
public:

  HACRootIO();
  virtual ~HACRootIO();

  static HACRootIO* GetInstance();
  void NewRun();
  void EndRun();
  void SaveEvent(const G4Event*);
  void Close();

private:

  TBranch* fHACBranch;
  THACEvent *fEvent;
  HACGeometryParameters * fGeoPars;
  HACMaterialParameters * fMatPars;
};
#endif // HACRootIO_H
