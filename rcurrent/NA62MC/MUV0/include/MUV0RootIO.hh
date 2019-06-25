#ifndef MUV0RootIO_HH
#define MUV0RootIO_HH 1

#include "NA62VRootIO.hh"
#include "TTree.h"
#include "TBranch.h"
#include "globals.hh"

class TMUV0Event;
class MUV0GeometryParameters;
class MUV0MaterialParameters;

class MUV0RootIO : public NA62VRootIO {

public:

  MUV0RootIO();
  virtual ~MUV0RootIO();

  static MUV0RootIO* GetInstance();
  void NewRun();
  void EndRun();
  void SaveEvent(const G4Event*);
  void Close() {}

private:

  TBranch *fMUV0Branch;
  TMUV0Event *fEvent;

};
#endif // MUV0RootIO_H
