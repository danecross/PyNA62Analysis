// --------------------------------------------------------------------
// History:
//
// Created by Tommaso Spadaro (Tommaso.Spadaro@cern.ch) 2017-01-18
//
// Edited by Cari Cesarotti Feb 2017
// Updated by Babette Dobrich (babette.dobrich@cern.ch) March 13th 2017: ALP production and new 2body decay
// --------------------------------------------------------------------

#include "NA62Global.hh"
#include "ExoticProductionMode.hh"
#include "G4SystemOfUnits.hh"
#include "G4ParticleTable.hh"
#include "G4ParticlePropertyTable.hh"
#include "DatacardManager.hh"
#include "ExoticParticle.hh"
#include "NA62ConditionsService.hh"

using namespace std;

ExoticProductionMode::ExoticProductionMode(G4String& ExoticParticleName, G4String& MesonName, G4int nDaughters, std::vector<G4String>& DaughterNames){
  fParticleTable = G4ParticleTable::GetParticleTable();
  exoticMass = ExoticParticle::Definition(0)->GetPDGMass(); //MeV
  SetExoticParticle(ExoticParticleName);
  fNDaughters = nDaughters;
  fDaughterNames = DaughterNames;
  if (MesonName=="") {
    fModeType = kDirect;
  }
  else {
    if (MesonName=="eta'") MesonName = "eta_prime";
    SetMesonParticle(MesonName);
    SetMesonDecayType(MesonName,nDaughters,DaughterNames);
    fModeType = kViaMesonDecay;
  }

  fUseTarget = true;
  fHAxionETheta = nullptr;
  fHDPzPt2 = nullptr;
  fHDSPlusPlPt = nullptr;
  fHDPlusPlPt = nullptr;
  fHDMinusPlPt = nullptr;
  fHDSMinusPlPt = nullptr;
  fHTotalEntries = nullptr;
  fHPvTEta = nullptr;
  fHPvTEtaP = nullptr;
  fHPvTPhi = nullptr;
  fHPvTRho = nullptr;
  fHPvTOmega = nullptr;
  fHPvTPi0 = nullptr;

  fMesonTime = 0.;
  fCrossSection =0;
  fModeCrossSection=0;
}

ExoticProductionMode::~ExoticProductionMode(){}

void ExoticProductionMode::SetExoticParticle(G4String& ExoticParticleName){

  G4ParticleDefinition* ExoticParticleDef = ExoticParticle::Definition(0);
  if (!ExoticParticleDef) {
    G4cout << "[ExoticProductionMode] Error: No Exotic particle specified: " << ExoticParticleName << G4endl;
    exit(kWrongConfiguration);
  }

  if      (ExoticParticleName == "Axion") fExoticParticleID = kAxion;
  else if (ExoticParticleName == "HNL") fExoticParticleID = kHeavyNeutralLepton;
  else if (ExoticParticleName == "A'") fExoticParticleID = kDarkPhoton;
  else {
    G4cout << "[ExoticProductionMode] Error: NoProductionMode for Exotic Particle Name provided " << ExoticParticleName << G4endl;
    exit(kWrongConfiguration);
  }
}

void ExoticProductionMode::SetMesonParticle(G4String& mesonName){
  // check that the name corresponds to a valid particle
  G4ParticleDefinition* MesonParticleDef = fParticleTable->FindParticle(mesonName);
  if (!MesonParticleDef) {
    G4cout << "[ExoticProductionMode] Error: Invalid meson particle specified: " << mesonName << G4endl;
    exit(kWrongConfiguration);
  }

  //Check that mesons are correct
  if (fExoticParticleID == kAxion) {
    G4cout << "[ExoticProductionMode] Error: Invalid meson particle specified for an Axion " << mesonName << G4endl;
    exit(kWrongConfiguration);
  }

  if (fExoticParticleID == kHeavyNeutralLepton) {
    if (!(mesonName=="D+" || mesonName=="D-" || mesonName=="Ds+" ||
    mesonName=="Ds-")) {
      G4cout << "[ExoticProductionMode] Error: Invalid meson particle specified for a HNL " << mesonName << G4endl;
      exit(kWrongConfiguration);
    }
  }
  if (fExoticParticleID == kDarkPhoton) {
    if (!(mesonName=="pi0" || mesonName =="eta" || mesonName =="eta_prime" ||
    mesonName=="phi" || mesonName =="rho0" || mesonName=="omega")) {
      G4cout << "[ExoticProductionMode] Error: Invalid meson particle specified for a DarkPhoton " << mesonName << G4endl;
      exit(kWrongConfiguration);
    }
  }
  fMesonName = mesonName;
}

void ExoticProductionMode::SetMesonDecayType(G4String& mesonN, G4int nDaughters,
               std::vector<G4String>& daughterNames){
  SetMesonParticle(mesonN);
  if (nDaughters != 1) {
    G4cout << "[ExoticProductionMode] Error: Only implemented for direct production or two body decay for exotic production "
           << fExoticParticleID << " " << fMesonName << " " << nDaughters << G4endl;
    exit(kWrongConfiguration);
  }

  // Check that daughter has a valid name
  G4ParticleDefinition* DaughterParticleDef = fParticleTable->FindParticle(daughterNames.at(0));
  if (!DaughterParticleDef) {
    G4cout << "[ExoticProductionMode] Error: Invalid daughter particle specified: " << daughterNames.at(0) << G4endl;
    exit(kWrongConfiguration);
  }

  G4ParticleDefinition* MesonParticleDef = fParticleTable->FindParticle(fMesonName);
  
  // Check charge conservation (valid for neutral exotic particles)
  if (MesonParticleDef->GetPDGCharge() != DaughterParticleDef->GetPDGCharge()){
    G4cout << "[ExoticProductionMode] Error: Invalid daughter particle specified, charge violation: meson = "
           << fMesonName << " daughter = " << daughterNames.at(0) << G4endl;
    exit(kWrongConfiguration);
  }

  // Check if the decay is among those considered here: leptonic decays for HNL...
  if (fExoticParticleID == kHeavyNeutralLepton) {
    if (TMath::Abs(DaughterParticleDef->GetPDGEncoding()) != 11 && TMath::Abs(DaughterParticleDef->GetPDGEncoding()) != 13) {
      G4cout << "[ExoticProductionMode] Error: Invalid daughter particle specified, only leptonic modes allowed for HNL: meson = "
             << fMesonName << " daughter = " << daughterNames.at(0) << G4endl;
      exit(kWrongConfiguration);
    }
  }

  // ... decays with accompanying photon/pi0/eta for the DarkPhoton
  if (fExoticParticleID == kDarkPhoton) {
    if (DaughterParticleDef->GetPDGEncoding() != 22 && DaughterParticleDef->GetPDGEncoding() != 111 && DaughterParticleDef->GetPDGEncoding() != 221) {
      G4cout << "[ExoticProductionMode] Error: Invalid daughter particle specified, only g/pi0/eta modes allowed for dark photon: meson = "
             << fMesonName << " daughter = " << daughterNames.at(0) << G4endl;
      exit(kWrongConfiguration);
    }
  }

  // Check if the decay mode is kinematically allowed
  G4ParticlePropertyData*  ExoticParticleData =
    ((G4ParticlePropertyTable*) G4ParticlePropertyTable::GetParticlePropertyTable())->GetParticleProperty(ExoticParticle::Definition(0));
  Double_t exoMass = ExoticParticleData->GetPDGMass();
  G4double qValue = MesonParticleDef->GetPDGMass() - DaughterParticleDef->GetPDGMass() - exoMass;

  if (qValue < 0) {
    G4cout << "[ExoticProductionMode] Error: Wanted decay not kinematically allowed: " <<
      " m(" << mesonN << ") = " << MesonParticleDef->GetPDGMass() <<
      " m(" << daughterNames.at(0) << ") = " << DaughterParticleDef->GetPDGMass() <<
      " m(exotic) = " << exoMass << G4endl;
    exit(kWrongConfiguration);
  }

  // Finally, for the DarkPhoton check if the mode is among those allowed
  if (fExoticParticleID==kDarkPhoton) {
    if(mesonN=="eta"||mesonN=="pi0"){
      if (daughterNames.at(0)!="gamma"){
        G4cout << "[ExoticProductionMode] Error: Invalid Dark Photon production mode: "
               << mesonN << " to A' and " << daughterNames.at(0) << G4endl; 
        exit(kWrongConfiguration);
      }
    }
    if(mesonN=="eta_prime"){
      if(daughterNames.at(0)!="gamma" && daughterNames.at(0)!="pi0"){
        G4cout << "[ExoticProductionMode] Error: Invalid Dark Photon production mode: "
               << mesonN << " to A' and " << daughterNames.at(0) << G4endl; 
        exit(kWrongConfiguration);
      }
    }
    if(mesonN=="phi" || mesonN=="rho0" || mesonN=="omega"){
      if(daughterNames.at(0)!="pi0" && daughterNames.at(0)!="eta"){
        G4cout << "[ExoticProductionMode] Error: Invalid Dark Photon production mode: "
               << mesonN << " to A' and " << daughterNames.at(0) << G4endl; 
        exit(kWrongConfiguration);
      }
    }
  }
}

void ExoticProductionMode::SetIOInfo(G4String& fileName, G4String& mode, G4String& histogramName){
  fIOFileName = fileName;         ///< FileName where the Production mode information is stored
  fIOMode = mode;                 ///< IOMode: "textFile", or "rootFile"
  fHistogramName = histogramName; ///< Name of histogram generated from textFile or taken from the rootFile
}

G4bool ExoticProductionMode::IsDirect(){
  if (fModeType==kDirect) return kTRUE;
  else return kFALSE;
}

G4bool ExoticProductionMode::IsValid(){
  //Check if Direct for A', axion or Indirect for A', HNL
  if ((fModeType==kDirect) && (fExoticParticleID==kHeavyNeutralLepton ||
         (fExoticParticleID==kDarkPhoton && fMesonName !=""))) return kFALSE;
  return kTRUE;
}

void ExoticProductionMode::GenerateKinematics(){

  fPartners = true; // Is there another particle to propagate besides the exotic

  //Define start position
  G4double x0 = (RandomGenerator::GetInstance())->GetRndmDecay(); // Scale
  G4double y0 = (RandomGenerator::GetInstance())->GetRndmDecay();
  G4double Prob, z0;
  
  //Calculate position
  if (fUseTarget){ 
    Prob = (RandomGenerator::GetInstance())->GetRndmDecay()*.678;
    z0 = -352.8*TMath::Log(1-Prob)-200; //mm
    y0=y0*2;
    x0=x0*2;
    fMesonTime=0.*s;
  }
  else {
    Prob = (RandomGenerator::GetInstance())->GetRndmDecay()*.9998;
    z0 = -14.36*TMath::Log(1-Prob)+(23070.-150.); 
    y0 = 3.3*y0-20.6; //move down 2 cm
    x0 = 3.75*x0;
    fMesonTime=z0/(2.997916337202889*1e11/s); //for a 400 GeV/c proton beam along beamline
  }

  G4double phi = (RandomGenerator::GetInstance())->GetRndmDecay()*2.0*TMath::Pi();
  fAcceptCounter=1;
  G4bool inAcceptance = kFALSE;
  G4double mesonMass = 0;
  G4double partnerMass = 0;

  if (fMesonName!=""){ // Produced from Meson
    mesonMass = fParticleTable->FindParticle(fMesonName)->GetPDGMass(); //MeV
  }

  if (fDaughterNames.size()!=0){ // If exotic decays
    partnerMass = fParticleTable->FindParticle(fDaughterNames.at(0))->GetPDGMass(); //MeV
  }

  while(!inAcceptance){ // Checks geometric acceptance

    if (fModeType==kDirect) {
      if (fExoticParticleID==kAxion){ // Axion
        fPartners = false;
  
        if(!fHAxionETheta) {
          G4cout << "[ExoticProductionMode] Reading the spectrum here" <<G4endl;
          ReadAxionSpectrum(exoticMass);
        }
  
        G4double energyA, thetaA;
        RandomGenerator::GetInstance()->GetRandom2(fHAxionETheta, thetaA, energyA);

        energyA = energyA*1e03;  // Put in MeV  
        G4double mom = sqrt(energyA*energyA - exoticMass*exoticMass);
        fExoticEng = energyA;
        fExoticPos = G4ThreeVector(x0, y0, z0);
        fExoticMom = G4ThreeVector(mom*cos(phi)*sin(thetaA),
                                   mom*sin(phi)*sin(thetaA),
                                   mom*cos(thetaA));

        if (InAcceptance(1500, 105000, fExoticPos, fExoticMom)){ //Acceptance in mm
          inAcceptance = InAcceptance(1500, 105000, fExoticPos, fExoticMom);
        }
        else { //reweight based off of how many events failed
          fAcceptCounter++;   continue;
        }
      } //End of Axion

      else if (fExoticParticleID==kDarkPhoton){ // Bremsstrahlung
        fPartners = true;
        if(!fHDPzPt2) {
          G4cout << "[ExoticProductionMode] Reading the spectrum... " << G4endl;
          ReadDPBremSpectrum(exoticMass);
        }

        fMesonTime = 0.0;
        G4double zVal = 0;
        G4double pt2Val = 0;
        RandomGenerator::GetInstance()->GetRandom2(fHDPzPt2, pt2Val, zVal);

        G4double beamEng = 400000.*MeV; 
        fExoticEng = zVal*beamEng;
        G4double momMag2 = fExoticEng*fExoticEng - exoticMass*exoticMass; 
        G4double momFor2 = momMag2 - pt2Val*1e06;
        if (momFor2<0.){
          G4cout << "[ExoticProductionMode] Not a valid A' production" << G4endl;
          exit(kWrongConfiguration);
        }
 
        fExoticMom = G4ThreeVector(sqrt(pt2Val)*TMath::Cos(phi),
                                   sqrt(pt2Val)*TMath::Sin(phi),
                                   sqrt(momFor2));

        fExoticPos =  G4ThreeVector(x0, y0, z0);
        inAcceptance = InAcceptance(1500, 105000, fExoticPos, fExoticMom); //Acceptance in mm
        if (!inAcceptance) { //reweight based off of how many events failed
          fAcceptCounter++;
          continue;
        }

        fPartnerPos = fExoticPos; //Partner is proton
        partnerMass = 938.28*MeV;
        fPartnerMom = G4ThreeVector(-fExoticMom.x(), -fExoticMom.y(), 400000-fExoticMom.z());
        fPartnerEng = sqrt(fPartnerMom.mag2()+partnerMass*partnerMass);
      } //End of Direct A'
    } //End of Direct Productions

    else { // Indirect production
      if(fExoticParticleID==kDarkPhoton){
        if(!fHTotalEntries) {
          ReadDPMesonSpectrum();
          G4cout << "[ExoticProductionMode] Reading Meson Spectrum" << G4endl;
        }
        G4double pM, thetaM;
        if(fMesonName=="pi0"){
          RandomGenerator::GetInstance()->GetRandom2(fHPvTPi0, pM, thetaM);
          cTau = 8.4e-17*s*cspeed;
        }
        else if(fMesonName=="eta"){
          RandomGenerator::GetInstance()->GetRandom2(fHPvTEta, pM, thetaM); 
          cTau = 5.0e-19*s*cspeed; 
        }
        else if(fMesonName=="eta_prime"){
          RandomGenerator::GetInstance()->GetRandom2(fHPvTEtaP, pM, thetaM);
          cTau = 3.2e-21*s*cspeed;
        }
        else if(fMesonName=="phi"){
          RandomGenerator::GetInstance()->GetRandom2(fHPvTPhi, pM, thetaM); 
          cTau = 1.55e-22*s*cspeed;
        }
        else if(fMesonName=="rho0"){
          RandomGenerator::GetInstance()->GetRandom2(fHPvTRho, pM, thetaM);
          cTau = 4.5e-24*s*cspeed; 
        }
        else if(fMesonName=="omega"){
          RandomGenerator::GetInstance()->GetRandom2(fHPvTOmega, pM, thetaM); 
          cTau=  7.753e-23*s*cspeed;
        }
        else {
          G4cout << "[ExoticProductionMode] Error: Not a valid indirect decay! You cannot decay a " <<
	    fMesonName << " to a A'"  << G4endl;
          exit(kWrongConfiguration);
        }

        pM = pM*1e03; //GeV to MeV
        mesonPos = G4ThreeVector(x0, y0, z0);
        mesonMom = G4ThreeVector(pM*TMath::Sin(thetaM)*TMath::Cos(phi),
                                 pM*TMath::Sin(thetaM)*TMath::Sin(phi),
                                 pM*TMath::Cos(thetaM));

        TLorentzVector mesonMom4;
        mesonMom4.SetPx(mesonMom.x());
        mesonMom4.SetPy(mesonMom.y());
        mesonMom4.SetPz(mesonMom.z());
        mesonMom4.SetE(sqrt(pM*pM+mesonMass*mesonMass));

        if (InAcceptance(1500, 105000, mesonPos, mesonMom)){ //Acceptance in mm
          inAcceptance = kTRUE;
          PropagateMeson(phi, thetaM); //sets the exotic production position.

          //Adjust for flight time of meson
          G4double mesonV = (pM/sqrt(pM*pM+mesonMass*mesonMass))*cspeed;
          G4ThreeVector mesonDist = fExoticPos-mesonPos;
          fMesonTime += mesonDist.mag()/mesonV;

          Double_t x=0.0, y=0.0, z=0.0;
          TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();
          RandomDecay->Sphere(x, y, z, 1.0);

          G4double unbstedEN = TwoBodyDecayEnergy(mesonMass,
                                                  exoticMass,
                                                  partnerMass);
          G4double unbstedEP = mesonMass-unbstedEN;
          G4double unbstedPN = sqrt(unbstedEN*unbstedEN-exoticMass*exoticMass);
          TLorentzVector Mom4_DP( unbstedPN*x,  unbstedPN*y,  unbstedPN*z, unbstedEN);
          TLorentzVector Mom4_partner(-unbstedPN*x,  -unbstedPN*y,  -unbstedPN*z, unbstedEP);

          // Boost decay products into lab frame
          Mom4_DP.Boost(mesonMom4.BoostVector());
          Mom4_partner.Boost(mesonMom4.BoostVector());

          fExoticMom = G4ThreeVector(Mom4_DP.X(), Mom4_DP.Y(), Mom4_DP.Z());
          fPartnerMom = G4ThreeVector(Mom4_partner.X(), Mom4_partner.Y(), Mom4_partner.Z());

          fExoticEng = Mom4_DP.E();
          fPartnerEng = Mom4_partner.E();

          fPartnerPos = fExoticPos; //already set in meson propagation
        }
        else { //reweight based off of how many events failed
          fAcceptCounter++;
          continue;
        }

        fCrossSection = fModeCrossSection/fAcceptCounter;

      }

      if(fExoticParticleID==kHeavyNeutralLepton){

        if(!fHDSPlusPlPt) {
          ReadCharmedSpectrum();
          G4cout << "[ExoticProductionMode] Reading Charmed Spectrum..." << G4endl;
        }

        G4double ptmHNL, plmHNL;
        if(fMesonName=="D+"){
          RandomGenerator::GetInstance()->GetRandom2(fHDPlusPlPt, plmHNL, ptmHNL);
          cTau=kDMeson;
        }
        else if(fMesonName=="D-") {
          RandomGenerator::GetInstance()->GetRandom2(fHDMinusPlPt, plmHNL, ptmHNL);
          cTau=kDMeson;
        }
        else if(fMesonName=="Ds+"){
          RandomGenerator::GetInstance()->GetRandom2(fHDSPlusPlPt, plmHNL, ptmHNL);
          cTau=kDSMeson;
        }
        else if(fMesonName=="Ds-") {
          RandomGenerator::GetInstance()->GetRandom2(fHDSMinusPlPt, plmHNL, ptmHNL);
          cTau=kDSMeson;
        }
        else {
          G4cout << "[ExoticProductionMode] Not a valid indirect decay! You cannot decay a " <<
	    fMesonName << " to a HNL" << G4endl;
          exit(kWrongConfiguration);
        }

        G4double mMomS = sqrt(ptmHNL*ptmHNL+plmHNL*plmHNL)*1e03; //MeV;
        G4double thetaM = TMath::ATan2(ptmHNL, plmHNL);

        mesonPos = G4ThreeVector(x0, y0, z0);
        mesonMom = G4ThreeVector(mMomS*TMath::Sin(thetaM)*TMath::Cos(phi),
                                 mMomS*TMath::Sin(thetaM)*TMath::Sin(phi),
                                 mMomS*TMath::Cos(thetaM));

        TLorentzVector mesonMom4;
        mesonMom4.SetPx(mesonMom.x());
        mesonMom4.SetPy(mesonMom.y());
        mesonMom4.SetPz(mesonMom.z());
        mesonMom4.SetE(sqrt(mMomS*mMomS+mesonMass*mesonMass));

        if (InAcceptance(1500, 105000, mesonPos, mesonMom)){ //Acceptance in mm
          inAcceptance = kTRUE;
          PropagateMeson(phi, thetaM);
          //Only implemented for meson ----> HNL l; e.g. one decay partner

          //Adjust for flight time of meson
          G4double mesonV = (mMomS/sqrt(mMomS*mMomS+mesonMass*mesonMass))*cspeed;
          G4ThreeVector mesonDist = fExoticPos-mesonPos;
          fMesonTime += mesonDist.mag()/mesonV;

          Double_t x=0.0, y=0.0, z=0.0;
          TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();
          RandomDecay->Sphere(x, y, z, 1.0);

          G4double unbstedEN = TwoBodyDecayEnergy(mesonMass,
                                                  exoticMass,
                                                  partnerMass);
          G4double unbstedEP = mesonMass-unbstedEN;//TwoBodyDecayEnergy(mesonMass,partnerMass,exoticMass);
          G4double unbstedPN = sqrt(unbstedEN*unbstedEN-exoticMass*exoticMass);
          TLorentzVector Mom4_HNL( unbstedPN*x,  unbstedPN*y,  unbstedPN*z, unbstedEN);
          TLorentzVector Mom4_partner(-unbstedPN*x,  -unbstedPN*y,  -unbstedPN*z, unbstedEP);


          // Boost decay products into lab frame
          Mom4_HNL.Boost(mesonMom4.BoostVector());
          Mom4_partner.Boost(mesonMom4.BoostVector());

          fExoticMom = G4ThreeVector(Mom4_HNL.X(), Mom4_HNL.Y(), Mom4_HNL.Z());
          fPartnerMom = G4ThreeVector(Mom4_partner.X(), Mom4_partner.Y(), Mom4_partner.Z());
          fExoticEng = Mom4_HNL.E();
          fPartnerEng = Mom4_partner.E();
          fPartnerPos = fExoticPos; //already set in meson propagation
        }
        else { //reweight based off of how many events failed
          fAcceptCounter++;
          continue;
        }
      } //HNL
    } //Indirect
  } //Acceptance

  fCrossSection = fModeCrossSection;
}


void ExoticProductionMode::EvaluateTotalCrossSection(){

  // if axion return 1
  
  fAcceptCounter=1;
  G4double mesonMass = 0;
  G4double partnerMass =0;
  G4double taxFact = 39.2718;
  G4double tarFact = 7.36519;
  
  if (fMesonName!=""){
    mesonMass = fParticleTable->FindParticle(fMesonName)->GetPDGMass(); //MeV
  }

  if (fDaughterNames.size()!=0){
    partnerMass = fParticleTable->FindParticle(fDaughterNames.at(0))->GetPDGMass(); //MeV
    if (fParticleTable->FindParticle(fDaughterNames.at(0))->GetPDGEncoding()==11||
        fParticleTable->FindParticle(fDaughterNames.at(0))->GetPDGEncoding()==-11)
      fCoupling = fCouplingE;
    else if
      (fParticleTable->FindParticle(fDaughterNames.at(0))->GetPDGEncoding()==13 ||
       fParticleTable->FindParticle(fDaughterNames.at(0))->GetPDGEncoding()==-13)
      fCoupling = fCouplingMu;
    else if 
      (fExoticParticleID==kDarkPhoton){
      fCoupling = fCouplingU;
    }
    else
      G4cout << "[ExoticProductionMode] Error: Illegal HNL decay partner"
             << G4endl;
  }

  if (fModeType==kDirect) {
    if (fExoticParticleID==kAxion){
      fModeCrossSection =1.0;
    }
    else if (fExoticParticleID==kDarkPhoton){
      if(!fHDPzPt2)
        ReadDPBremSpectrum(exoticMass);
      G4double materialFact = 1.; 
      if (fUseTarget){
        materialFact = tarFact;
      }
      else 
        materialFact = taxFact;
      fModeCrossSection = materialFact*fBremSigma;
    }
  } //Direct Productions
  
  else {
    if(fExoticParticleID==kDarkPhoton){
      if(!fHTotalEntries)
        ReadDPMesonSpectrum();        
      if(fMesonName=="pi0")
        fModeCrossSection=fSigmaPi0*mesonXSecPhoton(134.9766, exoticMass, fCouplingU);
      if(fMesonName=="eta")
        fModeCrossSection=fSigmaEta*mesonXSecPhoton(547.862, exoticMass, fCouplingU);
      if(fMesonName=="eta_prime"){
        if(fDaughterNames.at(0)=="gamma")
          fModeCrossSection=fSigmaEtaP*mesonXSecPhoton(957.78, exoticMass, fCouplingU);
        else // eta' ->pi0 A'
          fModeCrossSection=fSigmaEtaP*fCouplingU*fCouplingU;
      }
      if(fMesonName=="phi"){
        if(fDaughterNames.at(0)=="pi0")
          fModeCrossSection=fSigmaPhi*1.27e-05*fCouplingU*fCouplingU;
        else
          fModeCrossSection=fSigmaPhi*1.309e-02*fCoupling*fCouplingU;
      }
      if(fMesonName=="rho0"){
        if(fDaughterNames.at(0)=="pi0")
          fModeCrossSection=fSigmaRho*mesonXSecVector(775.49, exoticMass, 134.9766, fCouplingU);
        else 
          fModeCrossSection=fSigmaRho*mesonXSecVector(775.49, exoticMass, 547.862, fCouplingU);
      }
      if(fMesonName=="omega"){
        if(fDaughterNames.at(0)=="pi0"){
          fModeCrossSection=fSigmaOmega*mesonXSecVector(782.65, exoticMass, 134.9766, fCouplingU);
        }
        else {
          fModeCrossSection=fSigmaOmega*mesonXSecVector(782.65, exoticMass, 547.862, fCouplingU);
        }
      }
    }
    
    if(fExoticParticleID==kHeavyNeutralLepton){
      if(!fHDSPlusPlPt) {
        ReadCharmedSpectrum();
        G4cout << "[ExoticProductionMode] Reading Charmed Spectrum..." << G4endl;
      }
      //Associate with mesons
      if(fMesonName=="D+"){
        fModeCrossSection=fSigmaDPlus;
      }
      else if(fMesonName=="D-") {
        fModeCrossSection=fSigmaDMinus;
      }
      else if(fMesonName=="Ds+"){
        fModeCrossSection=fSigmaDSPlus;
      }
      else if(fMesonName=="Ds-") {
        fModeCrossSection=fSigmaDSMinus;
      }
      else {
        G4cout << "[ExoticProductionMode] Not a valid indirect decay! You cannot decay a " << fMesonName << " to a HNL"
               << G4endl;
        fModeCrossSection = 0.;
      }
      fModeCrossSection *= BranchingRatioHNL(fCoupling,mesonMass,partnerMass,exoticMass);
      G4cout << "[ExoticProductionMode] BranchingRatioHNL is " << BranchingRatioHNL(fCoupling, mesonMass, partnerMass, exoticMass) << G4endl;

    } //HNL
  } //Indirect

  G4cout << "[ExoticProductionMode] The mode cross section is: " << fModeCrossSection << G4endl;
}

void ExoticProductionMode::ExoticDaughters(G4double mass1, G4double mass2, G4double zMin, G4double zMax){

   G4double localMinimum = TMath::Max(zMin,fExoticPos.z());
   G4double decaySpot = localMinimum +  (RandomGenerator::GetInstance())->GetRndmDecay()*(zMax-localMinimum); //Flat decay to be reweighted

  G4double t = (decaySpot - fExoticPos.z())/fExoticMom.z();
  G4double dx = fExoticPos.x()+t*fExoticMom.x();
  G4double dy = fExoticPos.y()+t*fExoticMom.y();
  G4ThreeVector exoticDecayPosi(dx,dy,decaySpot);
  G4ThreeVector travelD = exoticDecayPosi - fExoticPos;
  G4double exoticV = (fExoticMom.mag()/fExoticEng)*cspeed;
  fExoticDecayPos = exoticDecayPosi;
  fExoticTime = travelD.mag()/exoticV;

  G4double m0 = exoticMass;
  G4double m1 = mass1;
  G4double m2 = mass2;
  TLorentzVector Mom4Parent;
  Mom4Parent.SetPx(fExoticMom.x());
  Mom4Parent.SetPy(fExoticMom.y());
  Mom4Parent.SetPz(fExoticMom.z());
  Mom4Parent.SetE(sqrt(pow(m0,2)+ fExoticMom.mag2()));

  // Energies and momenta of the daughters
  G4double E1 = 0.5*(m0*m0 + m1*m1 - m2*m2) / m0;
  G4double E2 = 0.5*(m0*m0 + m2*m2 - m1*m1) / m0;
  G4double P  = TMath::Sqrt(E1*E1 - m1*m1);

  Double_t x=0.0, y=0.0, z=0.0;
  TRandom3* RandomDecay = (RandomGenerator::GetInstance())->GetRandomDecay();
  RandomDecay->Sphere(x, y, z, 1.0);
  TLorentzVector Mom4_1 ( P*x,  P*y,  P*z, E1);
  TLorentzVector Mom4_2 (-P*x, -P*y, -P*z, E2);

  // Boost decay products into lab frame
  Mom4_1.Boost(Mom4Parent.BoostVector());
  Mom4_2.Boost(Mom4Parent.BoostVector());

  fMomD1.set(Mom4_1.X(),Mom4_1.Y(),Mom4_1.Z());

  fDaughter1Eng = Mom4_1.E();
  fDaughter2Eng = Mom4_2.E();

  fPos1 = exoticDecayPosi; //already set in meson propagation
  fMomD2.set(Mom4_2.X(),Mom4_2.Y(),Mom4_2.Z());
  /*
  G4cout << "check mom cons" << G4endl;
  G4cout << "daugters sum energy "<< Mom4_1.E() + Mom4_2.E() << " Compare to " <<  fExoticEng << G4endl;
  G4cout << "daugt sum momentum " << Mom4_1.X() + Mom4_2.X()  << " y " << Mom4_1.Y() +  Mom4_2.Y()    << " z " << Mom4_1.Z() + Mom4_2.Z() << G4endl;
  G4cout << "Compare to dark momentum: " << fExoticMom.x() << " y " << fExoticMom.y() << " z " << fExoticMom.z() << G4endl;
  G4cout << "inv mass check " << (Mom4_1+Mom4_2).Mag() << G4endl;
  G4cout << "Check rho of daughters: " << exoticDecayPosi.rho()/mm << G4endl;
  G4cout << "check slopes: " << Mom4_1.X()/Mom4_1.Z() << " and " << Mom4_2.X()/Mom4_2.Z() << G4endl;
  */
}

void ExoticProductionMode::InitializeMode(){
  if (!IsValid()) {
    G4cout << "[ExoticProductionMode] Error: The mode is invalid" << G4endl;
    exit(kWrongConfiguration);
  }
  EvaluateTotalCrossSection();
}

G4bool ExoticProductionMode::InAcceptance(G4double acceptR, G4double acceptZ, G4ThreeVector posV, G4ThreeVector momV){

  G4double dist = acceptZ - posV.z();

  // G4cout << "Distance is:  " << dist << G4endl;
  //G4cout << "rho starts as: " << posV.rho() << G4endl;

  G4ThreeVector mesonFinal = G4ThreeVector(posV.x()+dist*(momV.x()/momV.z()),
             posV.y()+dist*(momV.y()/momV.z()),
             posV.z()+dist);

  //G4cout << "Checking acceptances. The meson rho is: " <<
  //  sqrt(mesonFinal.x()*mesonFinal.x()+mesonFinal.y()*mesonFinal.y()) << "compared to the cut off: " << acceptR/mm << G4endl;

  if (sqrt(mesonFinal.x()*mesonFinal.x()+mesonFinal.y()*mesonFinal.y()) <
      acceptR)
    return kTRUE;
  
  return kFALSE;
}

void ExoticProductionMode::PropagateMeson(G4double phi, G4double theta){
  G4double dist = ExponentialDistribution(cTau);
  fExoticPos = G4ThreeVector(mesonPos.x()+dist*cos(phi)*sin(theta),
          mesonPos.y()+dist*sin(phi)*sin(theta),
          mesonPos.z()+dist*cos(theta));
}

void ExoticProductionMode::DefaultInit(G4String& ExoticParticleName){
  SetExoticParticle(ExoticParticleName);
  fModeType = kViaMesonDecay;
  fMesonName="";
  fHistogramName="";
  fCrossSection=0;
  fCoupling=0;
  fCouplingE=0;
  fCouplingMu=0;
}

void ExoticProductionMode::ReadAxionSpectrum(G4double mass) { //mass in MeV
  Double_t energyMinInFile = 4.;
  Double_t energyMaxInFile = 200.;
  G4int energyValuesInFile = 51; // check: one more than steps in ALP Mathematica output
  Double_t energyWid = (energyMaxInFile-energyMinInFile)/(energyValuesInFile-1); // used to be log scale

  Double_t thetaMinInFile = 0.0005;
  Double_t thetaMaxInFile = 0.0056;
  Int_t thetaValuesInFile = 23;
  Double_t thetaWid = (thetaMaxInFile-thetaMinInFile)/(thetaValuesInFile-1); // used to be log scale

  //Lin scale
  G4double mAMinInFile = 0.01; //GeV
  G4double mAMaxInFile = 0.11; //GeV
  G4int mAValuesInFile = 16;
  G4double mAWid = (mAMaxInFile-mAMinInFile)/(mAValuesInFile-1);

  G4double massBinTrue = (mass*0.001-mAMinInFile)/mAWid; // avoid issues with C++ not correctly rounding
  G4int massBin = massBinTrue+0.5;
   if (massBin < 0) massBin = 0;
   if (massBin >= mAValuesInFile){
     G4cout << "[ExoticProductionMode] Mass specified for Axion is out of range" << G4endl;
     exit(kWrongConfiguration);
     //massBin = mAValuesInFile-1;
   }

   //G4cout << "since mass " << mass << " setting massbin to  " << massBin <<  " width " << mAWid << G4endl;
   //G4double massVal   = mAMinInFile+mAWid*massBin;

   fHAxionETheta = new TH2D("AxionETheta", "AxionETheta",
          thetaValuesInFile, thetaMinInFile-0.5*thetaWid,thetaMaxInFile+0.5*thetaWid,
          energyValuesInFile, energyMinInFile-0.5*energyWid,energyMaxInFile+0.5*energyWid); // Assure that Bin spacing does not overshoot to negative bin edge!
   ifstream infile(NA62ConditionsService::GetInstance()->GetFullPath("Distribution_EPA_all_linear.dat"));
   G4double Theta, Energy, MassFile, val;
   G4int lineCounter = 0;
   G4int offset = thetaValuesInFile*energyValuesInFile;

   //   G4cout << "ReadAxionETheta >> looking for massBin = " << massBin << " with mass " << massVal << G4endl;
   //   G4cout << "AxionETheta >> will read file from line " << offset*massBin + 1 << " to " << offset*(massBin+1) << G4endl;

   while (infile >> Theta >> Energy >> MassFile >> val) { // Theta[rad], Energy[GeV], MassFile[GeV], Val
     lineCounter++;
     if (lineCounter < offset*massBin+1) continue;
     if (lineCounter > offset*(massBin+1)) continue;

     G4int iEnergy = (lineCounter - offset*massBin - 1)/thetaValuesInFile;
     G4int iTheta = lineCounter - offset*massBin -1 - iEnergy*thetaValuesInFile;

     fHAxionETheta->SetBinContent(iTheta+1, iEnergy+1, val);
   }
   infile.close();
}

void ExoticProductionMode::ReadDPBremSpectrum(G4double mass) { //mass in MeV

  Double_t zMinInFile = 0.04;
  Double_t zMaxInFile = 0.96;
  G4int zValuesInFile = 51; 
  Double_t zWid = (TMath::Log10(zMaxInFile)-TMath::Log10(zMinInFile))/(zValuesInFile-1);

  Double_t pt2MinInFile = 0.001;
  Double_t pt2MaxInFile = 1.0;
  Int_t pt2ValuesInFile = 21;
  Double_t pt2Wid = (TMath::Log10(pt2MaxInFile)-TMath::Log10(pt2MinInFile))/(pt2ValuesInFile-1); 

  G4double mAMinInFile = 0.002; //GeV
  G4double mAMaxInFile = 1.0; //GeV
  G4int mAValuesInFile = 23;
  G4double mAWid = (TMath::Log10(mAMaxInFile)-TMath::Log10(mAMinInFile))/(mAValuesInFile-1);

  G4double massBinTrue = (TMath::Log10(mass*0.001)-TMath::Log10(mAMinInFile))/mAWid; // avoid issues with C++ not correctly rounding
  G4int massBin = massBinTrue+0.5;
   if (massBin < 0) massBin = 0;
   if (massBin >= mAValuesInFile){
     G4cout << "[ExoticProductionMode] Mass specified for Dark Photon is out of range" << G4endl;
     exit(kWrongConfiguration);
     //massBin = mAValuesInFile-1;
   }

   fHDPzPt2 = new TH2D("DarkPhotonZpt2", "DarkPhotonZpt2",
		       pt2ValuesInFile, pow(10, TMath::Log10(pt2MinInFile)-0.5*pt2Wid), pow(10, TMath::Log10(pt2MaxInFile)+0.5*pt2Wid),
		       zValuesInFile, pow(10, TMath::Log10(zMinInFile)-0.5*zWid), pow(10, TMath::Log10(zMaxInFile)+0.5*zWid));

   ifstream infile(NA62ConditionsService::GetInstance()->GetFullPath("DarkPhoton_Brem_NA62.dat"));
   ifstream infileXs(NA62ConditionsService::GetInstance()->GetFullPath("DarkPhoton_Brem_xSec_BIG.dat"));
   G4double pt2, z, MassFile, val;
   G4int lineCounter = 0;
   G4int offset = pt2ValuesInFile*zValuesInFile;
   G4int crossBin =0;
   G4double crossSec, massCheck; 
   while (infileXs >> massCheck >> crossSec ) {
     crossBin=crossBin+1;
     if (crossBin==massBin){
       fBremSigma = crossSec;
     }
   }
   G4cout << "[ExoticProductionMode] The bremsstrahlung cross section is: " << fBremSigma << G4endl;

   while (infile >> pt2 >> z >> MassFile >> val) {
     lineCounter++;
     if (lineCounter < offset*massBin+1) continue;
     if (lineCounter > offset*(massBin+1)) continue;

     G4int iZval = (lineCounter - offset*massBin - 1)/pt2ValuesInFile;
     G4int iPt2 = lineCounter - offset*massBin -1 - iZval*pt2ValuesInFile;
     fHDPzPt2->SetBinContent(iPt2, iZval, val);
   }
   infile.close();
}

void ExoticProductionMode::ReadDPMesonSpectrum(){

  G4double totalXSecPP = 40; //in mbarns
  TFile *file = new TFile(NA62ConditionsService::GetInstance()->GetFullPath("mesons_production_Aprime.root"));
  fHTotalEntries = (TH1D*)file->Get("NParticlesPerEvt");
  G4double nTotEntries = 0;
  for (int j=0; j<6; j++){
    nTotEntries+=j*fHTotalEntries->GetBinContent(j);
    G4cout << "[ExoticProductionMode] Entries: " << nTotEntries << " for " << j << G4endl;
  }
  fHPvTEta = (TH2D *)file->Get("PvsThetaeta");
  fHPvTEta->SetDirectory(0);
  fSigmaEta = totalXSecPP*fHPvTEta->GetEntries()/nTotEntries;
  fHPvTEtaP = (TH2D *)file->Get("PvsThetaeta_prime");
  fHPvTEtaP->SetDirectory(0);
  fSigmaEtaP = totalXSecPP*fHPvTEtaP->GetEntries()/nTotEntries;
  fHPvTPhi = (TH2D *)file->Get("PvsThetaphi");
  fHPvTPhi->SetDirectory(0);
  fSigmaPhi = totalXSecPP*fHPvTPhi->GetEntries()/nTotEntries;
  fHPvTRho = (TH2D *)file->Get("PvsThetarho0");
  fHPvTRho->SetDirectory(0);
  fSigmaRho = totalXSecPP*fHPvTRho->GetEntries()/nTotEntries;
  fHPvTOmega = (TH2D *)file->Get("PvsThetaomega");
  fHPvTOmega->SetDirectory(0);
  fSigmaOmega = totalXSecPP*fHPvTOmega->GetEntries()/nTotEntries;
  fHPvTPi0 = (TH2D *)file->Get("PvsThetapi0");
  fHPvTPi0->SetDirectory(0);
  fSigmaPi0 = totalXSecPP*fHPvTPi0->GetEntries()/nTotEntries;
  file->Close();
}

void ExoticProductionMode::ReadCharmedSpectrum(){
  fHDPlusPlPt   = new TH2D("DPlusPlPt",   "DPlusPlPt",   50, 0., 250., 10, 0., 5.); // [GeV]
  fHDMinusPlPt  = new TH2D("DMinusPlPt",  "DMinusPlPt",  50, 0., 250., 10, 0., 5.); // [GeV]
  fHDSPlusPlPt  = new TH2D("DSPlusPlPt",  "DSPlusPlPt",  50, 0., 250., 10, 0., 5.); // [GeV]
  fHDSMinusPlPt = new TH2D("DSMinusPlPt", "DSMinusPlPt", 50, 0., 250., 10, 0., 5.); // [GeV]
  fSigmaDPlus=0;
  fSigmaDSPlus=0;
  fSigmaDMinus=0;
  fSigmaDSMinus=0;

  ifstream infile(NA62ConditionsService::GetInstance()->GetFullPath("PtVsPlDPlus.dat"));
  G4double Pl, Pt, val;
  while (infile >> Pl >> Pt >> val) { // Pl, Pt are in [GeV], so is the bin assignment
    G4int iPl = (G4int)(Pl / 5.0);
    G4int iPt = (G4int)(Pt / 0.5);
    fHDPlusPlPt->SetBinContent(iPl+1, iPt+1, val);
    fSigmaDPlus+=val;
  }
  infile.close();

  ifstream infile2(NA62ConditionsService::GetInstance()->GetFullPath("PtVsPlDMinus.dat"));
  while (infile2 >> Pl >> Pt >> val) { // Pt, Pl are in [GeV], so is the bin assignment
    G4int iPl = (G4int)(Pl / 5.0);
    G4int iPt = (G4int)(Pt / 0.5);
    fHDMinusPlPt->SetBinContent(iPl+1, iPt+1, val);
    fSigmaDMinus+=val;
  }
  infile2.close();

  ifstream infile3(NA62ConditionsService::GetInstance()->GetFullPath("PtVsPlDSPlus.dat"));
  while (infile3 >> Pl >> Pt >> val) { // Pt, Pl are in [GeV], so is the bin assignment
    G4int iPl = (G4int)(Pl / 5.0);
    G4int iPt = (G4int)(Pt / 0.5);
    fHDSPlusPlPt->SetBinContent(iPl+1, iPt+1, val);
    fSigmaDSPlus+=val;
  }
  infile3.close();

  ifstream infile4(NA62ConditionsService::GetInstance()->GetFullPath("PtVsPlDSMinus.dat"));
  while (infile4 >> Pl >> Pt >> val) { // Pt, Pl are in [GeV], so is the bin assignment
    G4int iPl = (G4int)(Pl / 5.0);
    G4int iPt = (G4int)(Pt / 0.5);
    fHDSMinusPlPt->SetBinContent(iPl+1, iPt+1, val);
    fSigmaDSMinus+=val;
  }
  infile4.close();
}

G4double ExoticProductionMode::ExponentialDistribution(G4double decayL){
  G4double r = (RandomGenerator::GetInstance())->GetRndmDecay();
  return decayL*TMath::Log(1.0-r);
}

G4double ExoticProductionMode::BranchingRatioHNL
(G4double U, G4double Mm, G4double Ml, G4double Mh){
  G4double deltaA = Mh*Mh/(Mm*Mm);
  G4double deltaI = Ml*Ml/(Mm*Mm);
  return U*U*f(deltaA, deltaI)*sqrt(lambda(1, deltaA, deltaI))/
    (deltaA*(1-deltaA)*(1-deltaA));
}

G4double ExoticProductionMode::TwoBodyDecayEnergy(G4double Mm, G4double Mh, G4double Ml){
  return (Mh*Mh+Mm*Mm-Ml*Ml)/(2.0*Mm);
}

G4double ExoticProductionMode::lambda(G4double a, G4double b, G4double c){
  return a*a + b*b + c*c - 2.*a*b -2.*a*c -2.*b*c;
}

G4double ExoticProductionMode::f(G4double x, G4double y){
  return x+y-(x-y)*(x-y);
}

G4double ExoticProductionMode::mesonXSecPhoton(G4double mMass, G4double uMass, G4double e){
  return e*e*pow(1-uMass*uMass/(mMass*mMass), 3);
}

G4double ExoticProductionMode::mesonXSecVector(G4double mMass, G4double uMass, G4double pMass, G4double e){
  G4double part1 = (mMass*mMass-uMass*uMass-pMass*pMass)*(mMass*mMass-uMass*uMass-pMass*pMass);
  G4double part2 = (mMass-pMass-uMass)*(mMass+pMass-uMass)*(mMass-pMass+uMass)*(mMass+pMass+uMass);
  G4double part4 = (mMass*mMass-uMass*uMass)*(mMass*mMass-uMass*uMass)*(mMass*mMass-uMass*uMass);
  return e*e*part1*sqrt(part2)/part4;
}
