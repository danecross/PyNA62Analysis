//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publicationVs,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// --------------------------------------------------------------
// History:
//
// Created by Cari Cesarotti 2017-02-10
// Modified by Babette Dobrich 2017-04-26 remove Exotic from propagation in daughters.size()==2
// --------------------------------------------------------------
#include "globals.hh"
#include "RandomGenerator.hh"
#include "ExoticParticleGun.hh"
#include "G4Track.hh"

#include "MCTruthTrackInformation.hh"

#include "MCTruthManager.hh"
#include "MCTruthTrackInformation.hh"
#include "G4ParticleGun.hh"
#include "G4PrimaryVertex.hh"
#include "G4ParticleTable.hh"
#include "G4ParticleDefinition.hh"
#include "TH2D.h"
#include "G4Event.hh"
#include "EventAction.hh"
#include "RootIOManager.hh"
#include "G4SystemOfUnits.hh"
#include "DatacardManager.hh"

#include "ExoticProductionMode.hh"
#include "ExoticParticle.hh"
#include "ExoticProductionTable.hh"
#include "ExoticParticleGunMessenger.hh"
#include "RandomGenerator.hh"
#include "TMath.h"
#include "RootIOManager.hh"
#include <typeinfo>

#include <iostream>
using namespace std;

const G4double cspeed = 299792458*m/s;

ExoticParticleGun::ExoticParticleGun() :
  fSaveKine(false),
  fParticleTable(nullptr),
  fCouplingE(0),
  fCouplingM(0),
  fCouplingU(0),
  fDecayZmin(0),
  fDecayZmax(0),
  fMesonDaughter(false),
  fPart1(nullptr),
  fPart2(nullptr)
{

  fMessenger = new ExoticParticleGunMessenger(this);
  fExoticProdTable = NULL;
  fTotalCrossSec=0;
  fUseTarget=true;
  fPropExotic = true;
  fVertex = NULL;
  
  //Set for Daughter Acceptances
  fZmagnet = 197645.;
  fZfinal = 239009.;
  fRhoMin = 120.;
  fRhoMax = 1110.;
  fpKick = 270.;
  
  for (std::vector<G4ParticleDefinition*>::iterator iter = daughtersList.begin(); iter !=daughtersList.end(); ++iter){
    delete (*iter);
  }
  daughtersList.clear();
  daughters.clear();
}

ExoticParticleGun::~ExoticParticleGun(){
  delete fMessenger;  
}

void ExoticParticleGun::GeneratePrimaries(G4Event* anEvent, KinePart* exoticKP){

  G4ParticleDefinition *exoticPart = ExoticParticle::Definition(0);
  G4double dart = (RandomGenerator::GetInstance())->GetRndmDecay()*fTotalCrossSec; 
  G4double random = 0;
  
  for (std::vector<ExoticProductionMode*>::iterator iter = fExoticProdModes.begin(); iter !=fExoticProdModes.end(); ++iter){
    if ((*iter)->GetModeCrossSection() < dart){ //selecting which process to use
      random+=(*iter)->GetModeCrossSection();
        continue;
    }
    G4int weight=0;
    G4bool accept=false;
    while (!accept){   
      (*iter)->GenerateKinematics(); //Generate from scratch if not in accept
      G4ThreeVector momentum = (*iter)->GetExoticMom();
      weight=(*iter)->GetCounts(); // # of tries to get meson accept

      if (fPropExotic){ // if CHOD MC is working

        fSaveKine = false; 
        accept = true; //won't check daughters don't worry about it 
        fVertex = new G4PrimaryVertex((*iter)->GetExoticPos(),
                                      (*iter)->GetMesonFlyTime());

        G4PrimaryParticle* exoticPrim = new G4PrimaryParticle
	  (exoticPart, momentum.x(), momentum.y(), momentum.z(),
	   (*iter)->GetExoticEng());
  
        G4LorentzVector exotic4Mom(momentum.x(), momentum.y(), momentum.z(), (*iter)->GetExoticEng());
        exoticPrim->SetProperTime(0.);
        fVertex->SetPrimary(exoticPrim);
        break;
      }
      // Doesn't propagate Exotic, just saves to KinePart
  
      fSaveKine = true;
      fExoticProdPos.SetXYZT((*iter)->GetExoticPos().x(), (*iter)->GetExoticPos().y(), (*iter)->GetExoticPos().z(), (*iter)->GetMesonFlyTime());
  
      G4LorentzVector exotic4Mom(momentum.x(), momentum.y(), momentum.z(), (*iter)->GetExoticEng());
      fExoticMom.SetPxPyPzE(momentum.x(), momentum.y(), momentum.z(), (*iter)->GetExoticEng());

      if (daughtersList.size()!=2){
        G4cout << "[ExoticParticleGun] Error: Only two body exotic decays are available!" << G4endl;
        std::abort();
      }
        
      fPart1 = daughtersList.at(0);
      fPart2 = daughtersList.at(1);

      G4double qValue = exoticPart->GetPDGMass() - fPart1->GetPDGMass() - fPart2->GetPDGMass();

      if (qValue < 0) {
        G4cerr << "[ExoticProductionMode] Error: Wanted exotic decay not kinematically allowed: " <<
          " m(exotic) = " << exoticPart->GetPDGMass() << "MeV," <<
          " m(daughter1) = " << fPart1->GetPDGMass() << "MeV," <<
          " m(daughter2) = " << fPart2->GetPDGMass() << "MeV" << G4endl;
        std::abort();
      }
      
      (*iter)->ExoticDaughters(fPart1->GetPDGMass(), fPart2->GetPDGMass(), fDecayZmin, fDecayZmax);
      if (fabs(fPart1->GetPDGCharge())>0&&fabs(fPart2->GetPDGCharge())>0){ //only check charged
        accept = DaughtersInAcceptance((*iter)->GetExoticDecayPos(), 
                                       (*iter)->GetExoticDaughter1Mom(), 
                                       (*iter)->GetExoticDaughter2Mom());      
        if (!accept){
          weight++;
          continue; //in while loop
        }
      }
    
      else //doesn't check acceptances of photons & neutrinos
        accept=true;
    
      G4ThreeVector exoticPos((*iter)->GetExoticDecayPos().x(),
                              (*iter)->GetExoticDecayPos().y(),
                              (*iter)->GetExoticDecayPos().z());
    
   

      fExoticEndPos.SetXYZT(exoticPos.x(), exoticPos.y(), exoticPos.z(), 
                            (*iter)->GetMesonFlyTime()+(*iter)->GetExoticFlyTime());
    
      G4PrimaryParticle* daughter1 = new G4PrimaryParticle(fPart1,
                                                           (*iter)->GetExoticDaughter1Mom().x(),
                                                           (*iter)->GetExoticDaughter1Mom().y(),
                                                           (*iter)->GetExoticDaughter1Mom().z(),
                                                           (*iter)->GetExoticDaughter1Eng());
    
      G4PrimaryParticle* daughter2 = new G4PrimaryParticle(fPart2,
                                                           (*iter)->GetExoticDaughter2Mom().x(),
                                                           (*iter)->GetExoticDaughter2Mom().y(),
                                                           (*iter)->GetExoticDaughter2Mom().z(),
                                                           (*iter)->GetExoticDaughter2Eng());
    
      fVertex = new G4PrimaryVertex(exoticPos,
                                    (*iter)->GetMesonFlyTime()+(*iter)->GetExoticFlyTime());
      fVertex->SetPrimary(daughter2);
      fVertex->SetPrimary(daughter1);
      
    } // exits after acceptance
    
    //add kineparts here?
    if(fSaveKine){
      exoticKP->SetPDGcode(999);
      exoticKP->SetParentID(-1); 
      exoticKP->SetParentIndex(-2);
      exoticKP->SetParticleName(fExoticName);
      exoticKP->SetInitial4Momentum(fExoticMom);
      exoticKP->SetFinal4Momentum(fExoticMom);
      exoticKP->SetProdPos(fExoticProdPos);
      exoticKP->SetEndPos(fExoticEndPos);
      if ((*iter)->IsDirect()) exoticKP->SetProdProcessName((TString)"Direct");
      else exoticKP->SetProdProcessName((*iter)->GetMesonName()+(G4String)fExoticName+(G4String)daughters.at(0));
      exoticKP->SetEndProcessName((TString)"Decay");
    }
    

    Event *Evt = MCTruthManager::GetInstance()->GetEvent();
    //G4cout << "The weight of this event is: " << weight << G4endl;
    Evt->SetEventWeight(weight);

    anEvent->AddPrimaryVertex(fVertex); //exotic daughters added to vertex

    if ((*iter)->IsPartner()){ //add muon from parent meson
      G4ThreeVector momentumP = (*iter)->GetPartnerMom();
      G4PrimaryParticle *decayPartner = new G4PrimaryParticle(fParticleTable->FindParticle((*iter)->GetDaughterNames().at(0)),
                                                              momentumP.x(), momentumP.y(), momentumP.z(), (*iter)->GetPartnerEng());
      if (fMesonDaughter && fabs(decayPartner->GetPDGcode()) == 13) {
        G4PrimaryParticle *parentMeson  = new G4PrimaryParticle(fParticleTable->FindParticle((*iter)->GetMesonName()),0,0,0);
	parentMeson->SetProperTime(0.); 
        parentMeson->SetDaughter(decayPartner);
        G4PrimaryVertex* vertexP = new G4PrimaryVertex((*iter)->GetPartnerPos(), (*iter)->GetMesonFlyTime());
        vertexP->SetPrimary(parentMeson);
        anEvent->AddPrimaryVertex(vertexP); // add muon from parent meson
      }
    }
    break;
  }
}




void ExoticParticleGun::AddExoticProductionMode(G4String mode){
  if(std::find(fProductionModes.begin(),fProductionModes.end(),
               mode) != fProductionModes.end()) {
    G4cerr << "[ExoticParticleGun] Mode " << mode << " already selected! Skipping"
           << G4endl;
    return;
  }

  G4String exotic; G4String meson; 
  G4String pwd; G4String dataFile;
  G4String fileType;
  
  TString Line = Form(mode.data());
  TObjArray * line;
  line = Line.Tokenize(" ");
  exotic = ((TObjString*)(line->At(1)))->GetString();
  fExoticName = exotic;
  
  if (((TObjString*)(line->At(0)))->GetString() == "direct"){
    std::cout << "[ExoticParticleGun] Direct" << std::endl;
    meson = "";
    if (exotic == "A'"){
      daughters.push_back("proton");
    }
  }
  else{
    meson = ((TObjString*)(line->At(0)))->GetString();
    daughters.push_back((((TObjString*)(line->At(2)))->GetString()).Data());
  }
  delete line;
  ExoticProductionMode* ExoticProd = new ExoticProductionMode(exotic, meson, daughters.size(), daughters);
  if (exotic == "Axion")
    ExoticProd->SetIOInfo(pwd=((G4String)std::getenv("NA62MCSOURCE"))+"/config",dataFile="AxionETheta.dat", fileType="txtFile");

  fProductionModes.push_back(mode);     
  ExoticProd->SetHNLCouplingE(fCouplingE);
  ExoticProd->SetHNLCouplingM(fCouplingM);
  ExoticProd->SetUCouplingG(fCouplingU);
  ExoticProd->SetTargetOrTax(fUseTarget);
  ExoticProd->InitializeMode();
  fExoticProdModes.push_back(ExoticProd);
  fTotalCrossSec+=ExoticProd->GetModeCrossSection();
  G4cout << "[ExoticParticleGun] The cross section of mode "<< mode << " is " << ExoticProd->GetModeCrossSection()  <<G4endl;

}


G4double ExoticParticleGun::ProperTime(G4LorentzVector part, G4double tau, G4double z0) {
  TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();
  G4double bgc  = part.beta()*part.gamma()*c_light;
  G4double bgct = bgc*tau;
  G4double x1   = (200000-z0) / bgct; //!!!!!!
  G4double x2   = (200010-z0) / bgct;
  if (x1<0.0) x1 = 0.0; // protection against Zmin below the Geant4 handover point
  G4double DecayLength = -bgct*log(RandomDecay->Uniform(exp(-x2), exp(-x1)));
  return DecayLength/bgc;
}

void ExoticParticleGun::TaxOrTarget(G4String& answer){
  if (answer == "target")
    fUseTarget = true;
  else if (answer == "TAX")
    fUseTarget = false;
}

void ExoticParticleGun::AddDaughter(G4String part){
  G4ParticleDefinition* part1 =  fParticleTable->FindParticle(part);
  daughtersList.push_back(part1);
}

void ExoticParticleGun::PropExotic(G4String val){
  val.toLower();
  if (val =="true")
    fPropExotic = true;
  else if(val =="false")
    fPropExotic = false;
  else
    G4cerr << "[ExoticParticleGun] Not a valid choice. Choose true or false for propagating exotic" << G4endl;
}

G4bool ExoticParticleGun::DaughtersInAcceptance(G4ThreeVector exoticPos, G4ThreeVector mom1, G4ThreeVector mom2){

  G4ThreeVector d1Pos1(exoticPos.x()+mom1.x()*(fZmagnet-exoticPos.z())/mom1.z(),
                       exoticPos.y()+mom1.y()*(fZmagnet-exoticPos.z())/mom1.z(),
                       fZmagnet); 
  G4ThreeVector d2Pos1(exoticPos.x()+mom2.x()*(fZmagnet-exoticPos.z())/mom2.z(),
                       exoticPos.y()+mom2.y()*(fZmagnet-exoticPos.z())/mom2.z(),
                       fZmagnet);
  G4ThreeVector d1Pos2(d1Pos1.x()-fPart1->GetPDGCharge()*fpKick/mom1.mag()*(fZfinal-fZmagnet),
                       d1Pos1.x()-fPart1->GetPDGCharge()*fpKick/mom1.mag()*(fZfinal-fZmagnet),
                       fZfinal);
  G4ThreeVector d2Pos2(d2Pos1.x()-fPart2->GetPDGCharge()*fpKick/mom2.mag()*(fZfinal-fZmagnet),
                       d2Pos1.x()-fPart2->GetPDGCharge()*fpKick/mom2.mag()*(fZfinal-fZmagnet),
                       fZfinal);

  G4double rho1 = sqrt(d1Pos2.x()*d1Pos2.x()+d1Pos2.y()*d1Pos2.y());
  G4double rho2 = sqrt(d2Pos2.x()*d2Pos2.x()+d2Pos2.y()*d2Pos2.y());

  if ((rho1>fRhoMin&&rho1<fRhoMax)&&(rho2>fRhoMin&&rho2<fRhoMax))
    return true;

  return false;
}
