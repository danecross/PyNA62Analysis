// --------------------------------------------------------------------
// History:
//
// Created for LAV by Domenico Di Filippo (difilippo@na.infn.it) 2010-03-15
// 
// Modified for RICH
//
// --------------------------------------------------------------------
#ifndef RICHOptTrack_H
#define RICHOptTrack_H

#include "globals.hh"
#include "CLHEP/Random/JamesRandom.h"
#include "G4MaterialPropertyVector.hh"
#include "G4ThreeVector.hh"
#include "G4ParticleDefinition.hh"
#include "G4Step.hh"
// #include "SampleMatrix.hh"
// #include "G4AffineTransform.hh"

#include <TVector3.h>
#include <TVector2.h>
#include "CherenkovPhotonProd.hh"
#include "Photon.hh"
#include "RichAcceptance.hh"
#include "TwoMirror.hh"
#include "PMTMap.hh"
#include <vector>



class RICHOptTrack{

public:
    RICHOptTrack();
    ~RICHOptTrack();
   

  void  ProduceOpticalPhoton(G4Step *);

  void ProcessOpticalPhoton(G4int index);
    
  //  void SetEfficiencyMultiplier(G4double a){fEffMul=a;}  // not yet used
  //    void SetPhiOscillation(G4double a){fPhiOsc = a;} // not yet used
  
  G4int GetPhotonsNumber() {return fPhotonsNumber;}
  G4double GetPhotonsEnergy() {return fPhotonsEnergy;}
  G4ThreeVector GetPhotonsMomentum() {return fPhotonsMomentum;} 
  G4ThreeVector GetPhotonsPosition() {return fPhotonsPosition;} 
 // G4ThreeVector GetPhotonsPositionAcc() {return fPhotonsPositionAcc;} 
 // G4ThreeVector GetPMPosition() {return fPMPosition;} 
  G4double GetPhotonsTime() {return fPhotonsDelay;}

// not yet used:
  
  //    SampleMatrix* GetDelayMatrix() {return &fDelay;}
  //  SampleMatrix* GetEfficiencyMatrix() {return &fEfficiency;}

private:

 
  //SampleMatrix fDelay, fEfficiency;  // not yet used

    

  G4ParticleDefinition *fOpticalPhoton;

  G4double fPhiOsc, fEffMul;      // not yet used
  CLHEP::HepJamesRandom fRandEng;
  
  G4int    fPhotonsNumber;  
  G4double fPhotonsEnergy;
  G4ThreeVector fPhotonsMomentum;
  G4ThreeVector fPhotonsPosition;
 // G4ThreeVector fPhotonsPositionAcc;
 // G4ThreeVector fPMPosition;
  G4double fPhotonsDelay; 
  
  
  
  CherenkovPhotonProd fCHP;//object to generate the Cherencov photons
  TwoMirror fTwoMirror;//The Object of the two mirror system needed for transportation of the created photons
  PMTMap *fPMTMap; // this is the object of the PMT, can retrun pos of the PMT or do the acceptance cut of the PMT area
  RichAcceptance fRichAcceptance; //checks the acceptance of the RICH in general. Calcualtes the length a particle has to generate cherncov light
				  //in ideal case from entrance up to th mirror

  //Vector with all transported photons
  std::vector<TVector3> fPMTPhotons;
  std::vector<double> fPhotonEnergy;
  std::vector<double> fTravelTime;
  
};

#endif

