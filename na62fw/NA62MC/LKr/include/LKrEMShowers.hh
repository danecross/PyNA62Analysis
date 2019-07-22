// --------------------------------------------------------------------
// History:
//
//           15.11.2012 Sergey Podolsky (siarhei.padolski@cern.ch)
//            
//
// --------------------------------------------------------------------

#ifndef LKrEMShowers_h
#define LKrEMShowers_h 1
#include "G4SystemOfUnits.hh"
#include "G4VFastSimulationModel.hh"
#include "G4Step.hh"
#include "G4TouchableHandle.hh"
#include "LKrEnergySpot.hh"
#include <vector>
#include <sqlite3.h>
#include "boost/numeric/ublas/storage.hpp"
#include "boost/fusion/container/vector.hpp"
#include "boost/fusion/include/vector.hpp"
#include "boost/fusion/container/vector/vector_fwd.hpp"
#include "boost/fusion/include/vector_fwd.hpp"

class LKrEMShowers : public G4VFastSimulationModel {

  typedef struct
  {
    int PDGid;
    float x;
    float y;
    float z;
    float px;
    float py;
    float pz;
      
    template <typename Archive>
    void serialize(Archive& ar, const unsigned int /*version*/)
    {
      ar & PDGid;
      ar & x;
      ar & y;
      ar & z;
      ar & px;
      ar & py;
      ar & pz;
    }
  } TrackType;


//using namespace std;

struct HitsType
  {
    int cellID;
    float Energy;
      
      
    template <typename Archive>
    void serialize(Archive& ar, const unsigned int /*version*/)
    {
      ar & cellID;
      ar & Energy;
    }
      
  } ;

  
typedef std::vector<HitsType> HitsTypeList;
typedef std::vector<TrackType> TrackTypeList;
  
public :
    LKrEMShowers(G4String, G4Region*); 
    ~LKrEMShowers();

    G4bool IsApplicable(const G4ParticleDefinition&);
    G4bool ModelTrigger(const G4FastTrack &);
    void DoIt(const G4FastTrack&, G4FastStep&);

    void AssignSpotAndCallHit(const LKrEnergySpot &eSpot);
    void FillFakeStep(const LKrEnergySpot &eSpot);
    void BuildDetectorResponse();
    bool CheckContainment(const G4FastTrack& fastTrack);

private:
      sqlite3 *db;
      G4bool issuewithfile;
      int showersEnergy[50];
      int showersCount[50];
      double showersweight[2000][50];
      double maxweight[50];
      int countEnergies;
      G4double globaltime;

      G4ThreeVector NewPositionShower;
      G4bool TryToFillSpots(const G4FastTrack&);

      std::vector<LKrEnergySpot> feSpotList;

      G4Step                         *fFakeStep;
      G4StepPoint                    *fFakePreStepPoint, *fFakePostStepPoint;
      G4TouchableHandle              fTouchableHandle;
      G4Navigator                    *fpNavigator;
      G4bool                         fNaviSetup;
      TrackTypeList tracksvec;
      G4double PhiRandomRot;
      double enecorrection;
      double alpha;
      G4ThreeVector TrackRotAxis;
      double lastE;
      int lastEcount;
      bool particleisinstuck;

};
#endif
