#ifndef MatrixGun_h
#define MatrixGun_h 1

#include "G4VUserPrimaryGeneratorAction.hh"
#include "globals.hh"
#include "G4PhysicalConstants.hh"

#include "G4ParticleTable.hh"
#include "G4ParticleDefinition.hh"

#include "LAVSampleMatrix.hh"

#include "G4UImessenger.hh"
#include "G4UIcommand.hh"
#include "G4UIdirectory.hh"
#include "G4UIcmdWithAString.hh"
#include "G4UIcmdWithAnInteger.hh"
#include "G4UIcmdWithoutParameter.hh"
#include "LAVGeometryParameters.hh"

class MatrixGun : public G4VUserPrimaryGeneratorAction, G4UImessenger {

  public:
    MatrixGun();
   ~MatrixGun();

    void GeneratePrimaryVertex(G4Event*e) {GeneratePrimaries(e);}
    void GeneratePrimaries(G4Event*);

    void SetParticleByName(G4String name);
    void SetParticlesPerBin(G4int n);

    void SetRandomBin(){RandomBin=true;FixedBin=false;LoopBin=false;}
    void SetFixedBin(){RandomBin=false;FixedBin=true;LoopBin=false;}
    void SetLoopBin(){RandomBin=false;FixedBin=false;LoopBin=true;}

    LAVSampleMatrix *GetGunMatrix(){return &GunMatrix;}

    void MakeReady();

    void SetNewValue(G4UIcommand * command,G4String newValues);

  private:

    void RefreshGeometry(int);

    bool RandomBin,FixedBin,LoopBin;
    int CurrentBin;
    LAVGeometryParameters *geopar;
    LAVSampleMatrix GunMatrix;
    G4int ParticlesPerBin;
    G4ParticleDefinition *particle_definition;

    G4UIdirectory* matrixgunDir;
    G4UIcmdWithAString* particleCmd;
    G4UIcmdWithAnInteger* particlePerBinCmd;
    G4UIcmdWithAnInteger* channelIdCmd;
    G4UIcmdWithAString* setMatrixCmd;
    G4UIcmdWithAnInteger* setBinCmd;
    G4UIcmdWithoutParameter* setRandomBinCmd;
    G4UIcmdWithoutParameter* setFixedBinCmd;
    G4UIcmdWithoutParameter* setLoopBinCmd;

    G4ThreeVector xver,yver,zver,center;
    int chid;

};


#endif
