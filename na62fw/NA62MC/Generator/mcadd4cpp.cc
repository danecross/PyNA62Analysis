#include "../include/CMC.hh"
#include <iostream>

#define pi0decay_manager pi0decay_manager_
#define type_of_call
extern "C" {
  void type_of_call pi0decay_manager (int&, double[4], int&);
}

//////////////////////////////////////////////////////////////////
// Pass a particle to Geant4 for tracking.
// Products of beam particle decays are also saved into KineParts.

int mcadd4cpp(int ipid, const TLorentzVector &momentum) {
  CMC *c = CMC::GetInstance();
  int ipart = c->GetNParticles();
  c->SetPDGCode(ipart, ipid);
  c->SetKeepFlag(ipart, 1);
  c->Set4Momentum(ipart, momentum);
  c->SetPolarization(ipart, TVector3(0.,0.,0.));
  c->IncrementNParticles();
  return ipart+1;
}

//////////////////////////////////////////////////////////////////
// Pass a particle and its polarization to Geant4 for tracking.
// Products of beam particle decays are also saved into KineParts.

int mcadd4pol3cpp(int ipid, const TLorentzVector &momentum, const TVector3 &polar) {
  CMC *c = CMC::GetInstance();
  int ipart = c->GetNParticles();
  c->SetPDGCode(ipart, ipid);
  c->SetKeepFlag(ipart, 1);
  c->Set4Momentum(ipart, momentum);
  c->SetPolarization(ipart, polar);
  c->IncrementNParticles();
  return ipart+1;
}

//////////////////////////////////////////////////////////////////////////////
// Save a particle (in its mother rest frame) into GeneParts.
// Flag = particle group:
// 0 = particle from K decay, 1 = particle from pi0 decay;
// 2 = radiative particle from K decay; 3 = radiative particle from pi0 decay.

int mcadd4gencpp(int ipid, const TLorentzVector &momentum, int flag=0) {
  CMC *c = CMC::GetInstance();
  int ipart = c->GetNGeneratedParticles();
  c->SetGeneratedPDGCode(ipart, ipid);
  c->SetFlag(ipart, flag);
  c->SetGenerated4Momentum(ipart, momentum);
  c->IncrementNGeneratedParticles();
  return ipart+1;
}

//////////////////////////////////////////////////////////////////////////////
// Set keep flag for the particle with index partIndex in the cmcint array
// keep flag:
// 0: do not pass particle to Geant4
// 1: pass particle to Geant4 to manage its decay

void SetKeepFlag(int partIndex, int flag) {
  CMC *c = CMC::GetInstance();
  c->SetKeepFlag(partIndex, flag);
}

/////////////////////////////////////////////////////////////////////////////////////////////
// Boost momentum of particle 1 (p1) to reference frame in which particle 0 has 4-momentum p0

void dboost(const TLorentzVector& p0, const double massP0, TLorentzVector& p1) {
  double ep   = (p0.E()*p1.E() + p0.X()*p1.X() + p0.Y()*p1.Y() + p0.Z()*p1.Z()) / massP0;
  double cons = (p1.E() + ep) / (massP0 + p0.E());
  p1.SetX(p1.X() + cons * p0.X());
  p1.SetY(p1.Y() + cons * p0.Y());
  p1.SetZ(p1.Z() + cons * p0.Z());
  p1.SetE(ep);
}

///////////////////////////////////

void pi0decay_manager_cpp(int jpi0, TLorentzVector Mom4pi0, int DecayMode) {
  double vec[4];
  vec[0] = Mom4pi0.Px();
  vec[1] = Mom4pi0.Py();
  vec[2] = Mom4pi0.Pz();
  vec[3] = Mom4pi0.Energy();
  pi0decay_manager(jpi0, vec, DecayMode);
}
