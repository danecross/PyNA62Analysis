//
// --------------------------------------------------------------
// History:
//
// Created by Tommaso Spadaro (Tommaso.Spadaro@cern.ch) 2017-01-18
//
// Edited by Cari Cesarotti 2017-02-05
//
// --------------------------------------------------------------

#include "ExoticProductionTable.hh"

ExoticProductionTable* ExoticProductionTable::fInstance = 0;

ExoticProductionTable::ExoticProductionTable() {

  G4String exotic; G4String meson;  std::vector<G4String> daughters;
  G4String pwd; G4String dataFile; G4String fileType;

  //Axion Production
  ExoticProductionMode* axionProd = 
    new ExoticProductionMode(exotic="Axion", meson="", 0, daughters);
  axionProd->SetDirectType(0);
  axionProd->SetIOInfo(pwd=((G4String)std::getenv("NA62MCSOURCE"))+"/config",dataFile="AxionETheta.dat",
		       fileType="txtFile");
 fNProductionProcesses.push_back(1);         
  fAllProductionModes.push_back(axionProd); // indxsex 0
  G4cout << "Axion direct" << G4endl;
 

  //HNL Production
  //D+ --> HNL e+
  std::vector<G4String> daughters1;
  daughters1.push_back("e+");
  ExoticProductionMode* DpTOHNLep = 
    new ExoticProductionMode(exotic="HeavyNeutralLepton", meson="D+", 1, daughters1); 
  fAllProductionModes.push_back(DpTOHNLep); //index 1
  G4cout << "D+ -> HNL e+" << G4endl;
  
  //D- --> HNL e-
  std::vector<G4String> daughters2;
  daughters2.push_back("e-");
  ExoticProductionMode* DmTOHNLem = 
    new ExoticProductionMode(exotic="HeavyNeutralLepton", meson="D-", 1, daughters2);
  fAllProductionModes.push_back(DmTOHNLem); //index 2
  G4cout << "D- --> HNL  e-" << G4endl;

  //Ds+ --> HNL e+
  std::vector<G4String> daughters3;
  daughters3.push_back("e+");
  ExoticProductionMode* DspTOHNLep = 
    new ExoticProductionMode(exotic="HeavyNeutralLepton", meson="Ds+", 1, daughters3);

  fAllProductionModes.push_back(DspTOHNLep); //index 3
  G4cout << "Ds+ -> HNL e+" << G4endl;
 
  //Ds- --> HNL e-
  std::vector<G4String> daughters4;
  daughters4.push_back("e-");
  ExoticProductionMode* DsmTOHNLem = 
    new ExoticProductionMode(exotic="HeavyNeutralLepton", meson="Ds-",1, daughters4);
  fAllProductionModes.push_back(DsmTOHNLem); //index 4
  G4cout << "Ds- -> HNL e-" << G4endl;

  //D+ --> HNL mu+
  std::vector<G4String> daughters5;
  daughters5.push_back("mu+");
  ExoticProductionMode* DpTOHNLmup = 
    new ExoticProductionMode(exotic="HeavyNeutralLepton", meson="D+", 1, daughters5); 
  fAllProductionModes.push_back(DpTOHNLmup); //index 5
  G4cout << "D+ -> HNL m+" << G4endl;

  //D- --> HNL mu-
  std::vector<G4String> daughters6;
  daughters6.push_back("mu-");
  ExoticProductionMode* DmTOHNLmum = 
    new ExoticProductionMode(exotic="HeavyNeutralLepton", meson="D-", 1, daughters6); 
  fAllProductionModes.push_back(DmTOHNLmum); //index 6
  G4cout << "D- -> HNL e-" << G4endl;

  //Ds+ --> HNL mu+ 
  std::vector<G4String> daughters7;
  daughters7.push_back("mu+");
  ExoticProductionMode* DspTOHNLmup = 
    new ExoticProductionMode(exotic="HeavyNeutralLepton", meson="Ds+", 1, daughters7);
  fAllProductionModes.push_back(DspTOHNLmup); //index 7
  G4cout << "Ds+ -> HNL m+" << G4endl;

  //Ds- --> HNL mu- 
  std::vector<G4String> daughters8;
  daughters8.push_back("mu-");
  ExoticProductionMode* DsmTOHNLmum = 
    new ExoticProductionMode(exotic="HeavyNeutralLepton", meson="Ds-", 1, daughters8);
  fAllProductionModes.push_back(DsmTOHNLmum); //index 8 
  G4cout << "Ds- -> HNL m-" << G4endl;


  fNProductionProcesses.push_back(8); // DS+-, D+-, final states e HN, mu HN

  //Dark Photon Indirect 
  // pi0 --> gamma A'
  daughters.push_back("gamma");
  ExoticProductionMode* pi0TOdpgammaProd = 
    new ExoticProductionMode(exotic="DarkPhoton", meson="pi0", 1, daughters); //gamma
  fAllProductionModes.push_back(pi0TOdpgammaProd); //index 9

  // eta --> gamma A'
  ExoticProductionMode* etaTOdpgammaProd = 
    new ExoticProductionMode(exotic="DarkPhoton", meson="eta", 1, daughters); //gamma

  fAllProductionModes.push_back(etaTOdpgammaProd); //index 10

  // etaP --> gamma A'
  ExoticProductionMode* etaPTOdpgammaProd = 
    new ExoticProductionMode(exotic="DarkPhoton", meson="eta_prime", 1, daughters); //gamma

  fAllProductionModes.push_back(etaPTOdpgammaProd); //index 11

  daughters.pop_back();
  daughters.push_back("pi0");
  // etaP --> pi0 A'
  ExoticProductionMode* etaPTOdpomegaProd = 
    new ExoticProductionMode(exotic="DarkPhoton", meson="eta_prime", 1, daughters); //omega

  fAllProductionModes.push_back(etaPTOdpomegaProd); //index 12

  daughters.pop_back();
  daughters.push_back("pi0");
  // omega --> pi0 A' 
  ExoticProductionMode* omegaTOdppi0Prod = 
    new ExoticProductionMode(exotic="DarkPhoton", meson="omega", 1, daughters); //pi0

  fAllProductionModes.push_back(omegaTOdppi0Prod); //index 13

  daughters.pop_back();
  daughters.push_back("eta");
  //omega -->eta A'
  ExoticProductionMode* omegaTOdpetaProd = 
    new ExoticProductionMode(exotic="DarkPhoton", meson="omega", 1, daughters);

  fAllProductionModes.push_back(omegaTOdpetaProd); //index 14
  
  // phi --> eta A'
  ExoticProductionMode* phiTOdpetaProd = 
    new ExoticProductionMode(exotic="DarkPhoton", meson="phi", 1, daughters); //eta

  fAllProductionModes.push_back(phiTOdpetaProd); //index 15
  
  daughters.pop_back(); 
  daughters.push_back("pi0");
  //phi --> pi0 A'
  ExoticProductionMode* phiTOdppi0Prod = 
    new ExoticProductionMode(exotic="DarkPhoton", meson="phi", 1, daughters);

  fAllProductionModes.push_back(phiTOdppi0Prod); // index 16

  // rho0 --> A' pi0 
  // Note to Cari: high mass hidden sector particle needs high mass meson
  ExoticProductionMode* rho0TOdppi0 = 
    new ExoticProductionMode(exotic="DarkPhoton", meson="rho0", 1, daughters);

  fAllProductionModes.push_back(rho0TOdppi0); //index 17

  //rho0 --> A' eta
  daughters.pop_back();
  daughters.push_back("eta");
  ExoticProductionMode* rho0TOdpeta = 
    new ExoticProductionMode(exotic="DarkPhoton", meson="rho0", 1, daughters);

  fAllProductionModes.push_back(rho0TOdpeta); // index 18

  fNProductionProcesses.push_back(10);    
  //Dark Photon Direct
  daughters.pop_back(); 
  daughters.push_back("proton"); 
  // Bremsstrahlung
  ExoticProductionMode* darkBremProd = 
    new ExoticProductionMode(exotic="DarkPhoton", meson="", 1, daughters);

  fAllProductionModes.push_back(darkBremProd); // index 19
  fNProductionProcesses.push_back(1);

  G4cout << "ExoticProductionTable: Initialized" << G4endl;
}

ExoticProductionTable* ExoticProductionTable::GetInstance() {
  if (!fInstance) fInstance = new ExoticProductionTable();
  return fInstance;
}

ExoticProductionMode* ExoticProductionTable::GetProductionProcess(G4int i){
  if ((unsigned)i<fAllProductionModes.size() && i>=0) return fAllProductionModes.at(i);
  G4cerr << "[ExoticProductionTable] Wrong index provided in input " << i << G4endl;
  exit(0);
}
