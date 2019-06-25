#ifndef MUV0SD_h
#define MUV0SD_h 1

#include "G4VSensitiveDetector.hh"
#include "MUV0Hit.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class MUV0SD : public G4VSensitiveDetector {

public:

  MUV0SD(G4String, G4String);
  ~MUV0SD() {}

  void   Initialize (G4HCofThisEvent*);
  G4bool ProcessHits(G4Step*, G4TouchableHistory*);
  void   EndOfEvent (G4HCofThisEvent*) {}
  
private:

  MUV0HitsCollection *Collection;
  MUV0Hit *HitMap[10];
  G4int nHits;
  G4int HCID;
};

#endif
