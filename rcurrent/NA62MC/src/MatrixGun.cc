#include "globals.hh"
#include "Randomize.hh"
#include "MatrixGun.hh"

#include "G4Track.hh"
#include "G4Event.hh"
#include "G4ParticleGun.hh"
#include "G4PrimaryVertex.hh"

#include "G4RandomDirection.hh"

#include "G4ParticleTable.hh"

#include <iostream>
using namespace std;

MatrixGun::MatrixGun(){
  geopar = LAVGeometryParameters::GetInstance();

  RefreshGeometry(10000);
  SetParticleByName("opticalphoton");
  SetParticlesPerBin(1);
  SetFixedBin();
  CurrentBin = 0;

  G4ParticleTable* particleTable = G4ParticleTable::GetParticleTable();

  matrixgunDir = new G4UIdirectory("/MatrixGun/");
  matrixgunDir->SetGuidance("Matrix Gun control commands.");

  particleCmd = new G4UIcmdWithAString("/MatrixGun/particle",this);
  particleCmd->SetGuidance("Set particle to be generated.");
  particleCmd->SetGuidance(" (geantino is default)");
  particleCmd->SetParameterName("particleName",true);
  particleCmd->SetDefaultValue("geantino");
  G4String candidateList;
  G4int nPtcl = particleTable->entries();
  for(G4int i=0;i<nPtcl;i++)
  {
    if(!(particleTable->GetParticle(i)->IsShortLived()))
    {
      candidateList += particleTable->GetParticleName(i);
      candidateList += " ";
    }
  }
  particleCmd->SetCandidates(candidateList);

  particlePerBinCmd = new G4UIcmdWithAnInteger("/MatrixGun/particlePerBin",this);
  particlePerBinCmd->SetGuidance("Set the number of particle generated in a event.");
  particlePerBinCmd->SetGuidance("(Every event change matrix bin: see setBin command).");
  particlePerBinCmd->SetParameterName("ParNum",true);
  particlePerBinCmd->SetDefaultValue(1);
  particlePerBinCmd->SetRange("ParNum>0");

  channelIdCmd = new G4UIcmdWithAnInteger("/MatrixGun/channelId",this);
  channelIdCmd->SetGuidance("Select in witch LAV block generate the events.");
  channelIdCmd->SetParameterName("CHANID",true);
  channelIdCmd->SetDefaultValue(0);

  setBinCmd = new G4UIcmdWithAnInteger("/MatrixGun/setBin",this);
  setBinCmd->SetGuidance("Select in witch matrix bin will generate the events.");
  setBinCmd->SetParameterName("ParNum",true);
  setBinCmd->SetDefaultValue(0);

  setRandomBinCmd = new G4UIcmdWithoutParameter("/MatrixGun/setRandomBin",this);
  setRandomBinCmd->SetGuidance("Select a random matrix bin to every generation.");

  setFixedBinCmd = new G4UIcmdWithoutParameter("/MatrixGun/setFixedBin",this);
  setFixedBinCmd->SetGuidance("Use single bin for all generation.");

  setLoopBinCmd = new G4UIcmdWithoutParameter("/MatrixGun/setLoopBin",this);
  setLoopBinCmd->SetGuidance("Looping on the bin.");

  setMatrixCmd = new G4UIcmdWithAString("/MatrixGun/matrixLoad",this);
  setMatrixCmd->SetGuidance("Select the file path of matrix to load.");
  setMatrixCmd->SetParameterName("MatrixPath",false);

}

MatrixGun::~MatrixGun(){
}

void MatrixGun::SetParticleByName(G4String name){
   particle_definition = G4ParticleTable::GetParticleTable()->FindParticle(name);
}

void MatrixGun::SetParticlesPerBin(G4int n){
   if (n<0) n=-n;
   ParticlesPerBin=n;
}

void MatrixGun::RefreshGeometry(int CHID){

   LAVGeometryParameters *geopar = LAVGeometryParameters::GetInstance();
   chid = CHID;
   xver = geopar->GetBlockXVersor(chid);
   yver = geopar->GetBlockYVersor(chid);
   zver = geopar->GetBlockZVersor(chid);
   center = geopar->GetLeadglassCenter(chid);

}

void MatrixGun::GeneratePrimaries(G4Event* anEvent){

  if (particle_definition == 0) return;
  if (ParticlesPerBin == 0) return;

  double min_dz = -1;
  double max_dz = 1;
  double min_pz = -230;
  double max_pz = 188;
  double min_en = 1.89e-6;
  double max_en = 3.1e-6;
  double min_pr = 0;
  double max_pr = 70;
  double min_df = -pi;
  double max_df = pi;
  double min_pf = -pi;
  double max_pf = pi;
  const int DimNum = GunMatrix.GetDimensionsNumber();

  if (RandomBin) GunMatrix.SetIndex(G4UniformRand()*GunMatrix.GetSize());
  if (FixedBin || LoopBin) {
     if (CurrentBin<0 || CurrentBin>=(int)GunMatrix.GetSize())
        CurrentBin = 0;
     GunMatrix.SetIndex(CurrentBin);
  }

  if (DimNum > 0) {
     min_dz = GunMatrix.GetDimension(0)->GetBinLowEdge();
     max_dz = GunMatrix.GetDimension(0)->GetBinTopEdge();
  }
  if (DimNum > 1) {
     min_pz = GunMatrix.GetDimension(1)->GetBinLowEdge();
     max_pz = GunMatrix.GetDimension(1)->GetBinTopEdge();
  }
  if (DimNum > 2) {
     min_en = GunMatrix.GetDimension(2)->GetBinLowEdge();
     max_en = GunMatrix.GetDimension(2)->GetBinTopEdge();
  }
  if (DimNum > 3) {
     min_pr = GunMatrix.GetDimension(3)->GetBinLowEdge();
     max_pr = GunMatrix.GetDimension(3)->GetBinTopEdge();
  }
  if (DimNum > 4) {
     min_df = GunMatrix.GetDimension(4)->GetBinLowEdge();;
     max_df = GunMatrix.GetDimension(4)->GetBinTopEdge();;
  }
  if (DimNum > 5) {
     min_pf = GunMatrix.GetDimension(5)->GetBinLowEdge();;
     max_pf = GunMatrix.GetDimension(5)->GetBinTopEdge();;
  }

  double dz,pz,en,pr,df,pf,dr;

  dz = G4UniformRand() * (max_dz - min_dz) + min_dz;
  if (dz>1) dz=1;
  if (dz<-1) dz=-1;
  pz = G4UniformRand() * (max_pz - min_pz) + min_pz;
  en = G4UniformRand() * (max_en - min_en) + min_en;
  pr = G4UniformRand() * (max_pr - min_pr) + min_pr;
  df = G4UniformRand() * (max_df - min_df) + min_df;
  pf = G4UniformRand() * (max_pf - min_pf) + min_pf;

  dr = sqrt(1 - dz*dz);

  G4ThreeVector pos = pr*cos(pf)*xver + pr*sin(pf)*yver + pz*zver + center;
  G4ThreeVector mom = dr*cos(df)*xver + dr*sin(df)*yver + dz*zver;
  const double m = particle_definition->GetPDGMass();
  mom *= sqrt(en*en - m*m);

  // Add particle to vertex and add vertex to event
  G4PrimaryVertex* vertex = new G4PrimaryVertex(pos,0);
  for (int i=0; i<ParticlesPerBin; i++)
     vertex->SetPrimary(new G4PrimaryParticle(particle_definition,mom.x(),mom.y(),mom.z()));
  anEvent->AddPrimaryVertex(vertex);

  if (LoopBin) {
     CurrentBin++;
     if (CurrentBin<0 || CurrentBin>=(int)GunMatrix.GetSize())
        CurrentBin = 0;
  }

}

void MatrixGun::SetNewValue(G4UIcommand * command, G4String newValue){

    if (command == particleCmd) {

      SetParticleByName(newValue);

    } else if (command == channelIdCmd) {

      RefreshGeometry(channelIdCmd->GetNewIntValue(newValue));

    } else if (command == particlePerBinCmd){

      SetParticlesPerBin(particlePerBinCmd->GetNewIntValue(newValue));

    } else if (command == setBinCmd){

      CurrentBin = setBinCmd->GetNewIntValue(newValue);

    } else if (command == setMatrixCmd){

      GunMatrix.Load(newValue.c_str());

    } else if (command == setRandomBinCmd){

      SetRandomBin();

    } else if (command == setFixedBinCmd){

      SetFixedBin();

    }  else if (command == setLoopBinCmd){

      SetLoopBin();

    }

}

