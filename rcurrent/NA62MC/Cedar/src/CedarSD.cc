// --------------------------------------------------------------------
// History:
//
// 2012-02-22 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - added the old (pre-2012) PMTs
//
// 2011-08-09 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - hit definition
//
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------

#include "CedarSD.hh"
#include "CedarHit.hh"
#include "CedarGeometryParameters.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4Track.hh"
#include "G4Step.hh"
#include "G4ParticleDefinition.hh"
#include "G4TouchableHistory.hh"
#include "G4ios.hh"
#include "G4SDManager.hh"
#include "G4HCofThisEvent.hh"
#include "G4ParticleTypes.hh"
#include "MCTruthTrackInformation.hh"
#include "Randomize.hh"
#include "TMath.h"
#include "TRandom.h"

CedarSD::CedarSD(G4String name,G4String colName) :
  G4VSensitiveDetector(name),
  fCollection(nullptr),
  nHits(0),
  HCID(0)
{
  G4String HCname;
  collectionName.insert(HCname=colName);
  iLGType = CedarGeometryParameters::GetInstance() ->GetLightGuideType();
}

CedarSD::~CedarSD() {}

void CedarSD::Initialize (G4HCofThisEvent *HCE) {
  static int HCID = -1;
  fCollection = new CedarHitsCollection(SensitiveDetectorName,collectionName[0]);
  verboseLevel = 0;
  nHits = 0;
  if (HCID<0) HCID = GetCollectionID(0);
  HCE->AddHitsCollection (HCID, fCollection);
}

G4bool CedarSD::ProcessHits (G4Step* aStep, G4TouchableHistory*) {

  // select optical photons
  G4ParticleDefinition* particle = aStep->GetTrack()->GetDefinition();
  if (particle != G4OpticalPhoton::OpticalPhotonDefinition())
    return false;

  G4int TrackID = 0;
  if (particle == G4OpticalPhoton::OpticalPhotonDefinition())
    TrackID = aStep->GetTrack()->GetParentID();
  else
    TrackID = aStep->GetTrack()->GetTrackID();

#ifndef CEDARRAYTRACING

  // Default treatment for hits in PMTs

  G4int PositionID =
    aStep->GetPreStepPoint()->GetPhysicalVolume()->GetCopyNo();

  // Encoding of the PMT types:
  // 1 = EMI-9820-QB, 2 = R7400U-03, 3 = R9880U-110, 4 = R9880U-210

  G4int iPMType = 1; // EMI-9820-QB
  if (iLGType==3 || iLGType==32) { // Test beam 2011; tech run 2012
    iPMType = 2; // R7400U-03
  }
  else if (iLGType==48) { // Run 2014: a hybrid PMT matrix
    iPMType = 3; // R9880U-110
    G4int PlaceID = PositionID%100;
    if (PlaceID==22 || PlaceID==27 || PlaceID==32 || PlaceID==38 ||
       PlaceID==41 || PlaceID==48 || PlaceID==51 || PlaceID==59 ||
       PlaceID==52 || PlaceID==58 || PlaceID==61 || PlaceID==68 ||
       PlaceID==72 || PlaceID==78 || PlaceID==82 || PlaceID==87)
      iPMType = 2; //  R7400U-03
  }
  else if (iLGType==64) { // this option is not currently foreseen
    iPMType = 2; // R7400U-03
  }

#else

  // Ray tracing treatment

  // These hits are not in PMs
  G4int iPMType = 0;
  G4int PositionID = 0;

  // Kill hits in LightGuide to avoid double counting

  CedarGeometryParameters* GeoPars = CedarGeometryParameters::GetInstance();

  if (aStep->GetPreStepPoint()->GetPosition().perp()
      > GeoPars->GetLightGuideInnerRadius()) {
    aStep->GetTrack()->SetTrackStatus(fStopAndKill);
  }
#endif

#ifdef CEDARMCQE
  // Apply QE here rather at the digitization stage
  Double_t Energy = aStep->GetTrack()->GetTotalEnergy(); // photon energy
  Double_t Wavelength = 1.986446e-25*joule*m/ Energy;
  if (gRandom->Rndm() > QE(Wavelength, iPMType)) return false;

  // Negate PM Type so CedarReconstruction knows not to apply QE again
  iPMType *= -1;
#endif

  // Create a hit (QE is applied later, at the digitization stage)
  CedarHit* Hit = new CedarHit();
  Hit->SetPosition(aStep->GetPreStepPoint()->GetPosition());
  Hit->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
  Hit->SetEnergy(aStep->GetTrack()->GetTotalEnergy()); // photon energy
  Hit->SetPositionID(PositionID);
  Hit->SetPMType(iPMType);
  Hit->SetTrackID(TrackID);

  fCollection->insert(Hit);
  nHits++;

  return true;
}

///////////////////////////////////////////////////////////

void CedarSD::EndOfEvent(G4HCofThisEvent*) {}
void CedarSD::clear() {}
void CedarSD::DrawAll() {}
void CedarSD::PrintAll() {}

///////////////////////////////////////////////////////////

Double_t CedarSD::QE (Double_t wavelength, Int_t PMType) {
  if (PMType==1) return QE_EMI_9820_QB (wavelength);
  if (PMType==2) return QE_R7400U_03   (wavelength);
  if (PMType==3) return QE_R9880U_110  (wavelength);
  if (PMType==4) return QE_R9880U_210  (wavelength);
  return 0;
}

// Hamamatsu R7400U-03 quantum efficiency
// Parameterized by Evgueni, July 2011

Double_t CedarSD::QE_R7400U_03 (Double_t wavelength) {
  Double_t par[10] =
  {-58.41145755814, 1.450540667766, -0.01561331198442,
    9.545010080831e-05, -3.648461145542e-07, 9.047599515597e-10,
    -1.457151808585e-12, 1.471328774241e-15, -8.46121819724e-19,
    2.11384701372e-22};

  Double_t x = wavelength/nm;
  if (x<180) return 0;
  if (x>660) return 0;
  Double_t qe = 0;
  for (int i=0; i<10; i++) qe += par[i]*TMath::Power(x,i);
  if (qe<0) qe = 0;
  return qe;
}

// Hamamatsu R9880U-110 quantum efficiency
// Parameterized by Angela, July 2011

Double_t CedarSD::QE_R9880U_110 (Double_t wavelength) {
  Double_t par[15] =
  {1489.053765000671, -20.61340505642701, 0.09607362916193821,
    -0.000144918944048782, -1.087924475686453e-07, 3.619104979507752e-10,
    2.742092765095943e-13, -1.067200613381487e-15, 6.333980140159196e-19,
    4.675391577876988, 505.1903283978535, 15.37334879108591,
    -23.08738129086531, 358.7521218115685, 53.63424346389683};

  Double_t x  = wavelength/nm;
  Double_t x1 = (x<650) ? x : 650;
  Double_t qe = 0;
  for (int i=0; i<9; i++) qe += par[i]*TMath::Power(x1,i);
  qe += par[9]*TMath::Gaus(x1, par[10], par[11]);
  qe += par[12]*TMath::Gaus(x1, par[13], par[14]);
  qe *= 0.01;
  if (x>650) qe *= (1 - (x-650)/(675-650));
  if (qe<0 || x<200) qe = 0;
  return qe;
}

// Hamamatsu R9880U-210 quantum efficiency
// Parameterized by Evgueni, March 2013

Double_t CedarSD::QE_R9880U_210 (Double_t wavelength) {
  double par[9] = 
  {277.3385690654, -5.360192324445, 0.04415739632667,
    -0.0002031054657811, 5.721437395991e-07, -1.012602804374e-09,
    1.100802213492e-12, -6.72600529683e-16, 1.769806940956e-19};

  Double_t x  = wavelength/nm;
  Double_t x1 = (x<680) ? ((x>240) ? x : 240) : 680;
  Double_t qe = 0;
  for (int i=0; i<9; i++) qe += par[i]*TMath::Power(x1,i);
  if (x>680) qe *= (1 - (x-680)/(700-680));
  if (x<240) qe *= (1 - (240-x)/(240-225));
  if (qe<0)  qe = 0;
  return qe;
}

/** Original Cedar PMT quantum efficiency **/

// 1) Lau's parameterization

Double_t CedarSD::QE_EMI_9820_QB_Lau (Double_t wavelength) {
  Double_t wl = wavelength/nm;
  Double_t qe = 0.25 - TMath::Power((wl-400)/500., 2);
  if (qe<0) qe = 0;
  return qe;
}

// 2) Francis's parameterization following the data sheet (March 2013)

Double_t CedarSD::QE_EMI_9820_QB (Double_t wavelength) {

  Double_t wlraw = wavelength/nm;
  Double_t wl = (wlraw < 141.0 ) ? 141.0 : (wlraw > 649.0) ? 649.0 : wlraw;

  Double_t wls[24] =
  {140, 160, 180, 200, 220, 240, 260, 280, 300, 320,
    340, 360, 380, 400, 420, 440, 460, 480, 500, 520,
    540, 560, 600, 650};
  Double_t qes[24] =
  {0, 20, 21.6, 21.6, 21.4, 21.4, 22, 24, 25, 25.6,
    26, 26, 26.4, 26, 24.8, 23.2, 20.8, 18, 15.2, 12,
    8, 5.6, 2, 0};

  unsigned int i = 0;
  while (true) {
    if (wl > wls[i] && wl < wls[i+1])
    { break; }
    else
    { ++i; }
  }
  return 0.01 * (qes[i] + ( wl - wls[i] )
      / ( wls[i+1] - wls[i] ) * ( qes[i+1] - qes[i] ) );
}
