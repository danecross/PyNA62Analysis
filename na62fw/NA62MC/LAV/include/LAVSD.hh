// LAVSD.hh
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// 2009-03-02 - Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - create LAV version of hits
// 2010-03-15 - Domenico Di Filippo (difilippo@na.infn.it)
//   - using OpticalTracker to manage Optical Photons
// 2011-01-24 - Domenico Di Filippo (difilippo@na.infn.it)
//   - New attribute to store the number of volume to the container
//   - New method to access the OpticalTracker
//
// --------------------------------------------------------------
#ifndef LAVSD_h
#define LAVSD_h 1

#include "G4VSensitiveDetector.hh"
#include "LAVHit.hh"
#include "LAVOptTrack.hh"

#include <map>

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class LAVSD : public G4VSensitiveDetector
{

public:
  LAVSD(G4String name, G4String colName);
  ~LAVSD();

  LAVOptTrack *GetOptTrack() {return &OpticalTracker;}
  void SetHierarchy(int h)   {fHierarchy=h;}

  void   Initialize(G4HCofThisEvent*);
  G4bool ProcessHits(G4Step*, G4TouchableHistory*);
  void   EndOfEvent(G4HCofThisEvent*);
  void   clear();
  void   DrawAll();
  void   PrintAll();

private:

  static std::map<int,LAVHit*> fChMap;
  static LAVHitsCollection* Collection;
  int HCID;

  int fHierarchy;
  LAVOptTrack OpticalTracker;

};

#endif

