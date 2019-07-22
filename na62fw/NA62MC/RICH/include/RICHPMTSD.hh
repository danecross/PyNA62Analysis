

#ifndef RICHPMTSD_h
#define RICHPMTSD_h 1

#include "G4VSensitiveDetector.hh"
#include "RICHPMTHit.hh"
class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class RICHPMTSD : public G4VSensitiveDetector
{

  public:
  RICHPMTSD(G4String name, G4String colName);
  ~RICHPMTSD();
  
  void Initialize(G4HCofThisEvent*HCE);
  G4bool ProcessHits(G4Step*aStep,G4TouchableHistory*);
  void EndOfEvent(G4HCofThisEvent*HCE);
  void clear();
  void DrawAll();
  void PrintAll();
  
  private:
      RICHPMTHitsCollection *Collection;
      G4int nHits;
      int HCID;
};




#endif

