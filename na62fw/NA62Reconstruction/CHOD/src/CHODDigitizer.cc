#include "Riostream.h"

#include "CHODGeometry.hh"
#include "TCHODHit.hh"
#include "TCHODDigi.hh"
#include "TCHODEvent.hh"
#include "CHODDigitizer.hh"
#include "CHODReconstruction.hh"
#include "TDCEvent.hh"
#include "TSpecialTriggerEvent.hh"
#include "TDCBRawDecoder.hh"
#include "NA62RecoManager.hh"

#include "TVector3.h"
#include "TF1.h"

#include "CLHEP/Units/SystemOfUnits.h"
using namespace CLHEP;

CHODDigitizer::CHODDigitizer(NA62VReconstruction* Reco) :
  NA62VDigitizer(Reco, "CHOD"),
  fInverseVelocity(nullptr)
{

  CHODGeometry* gm = CHODGeometry::GetInstance();
  fNumberOfChannel = gm->GetNumberOfChannel();

  // hit energy threshold for a hit (low threshold)
  fHitEnergyThreshold = static_cast<CHODReconstruction*>(fReco)->GetHitEnergyThreshold();

  // read parameters for the digitization
  for(Int_t i=0; i<fNumberOfChannel; i++){
    // read slewing corrections (only slope) from file
    for(Int_t j=0; j<16; j++) {
      fSlewCorrSlope[i][j] = static_cast<CHODReconstruction*>(fReco)->GetSlewCorrSlope(i,j);
      fSlewCorrConst[i][j] = static_cast<CHODReconstruction*>(fReco)->GetSlewCorrConst(i,j);
    }
    // read light velocity
    fLightVelocity[i] = static_cast<CHODReconstruction*>(fReco)->GetLightVelocity(i);
    // read TOT at PM
    fTOTAtPM[i] = static_cast<CHODReconstruction*>(fReco)->GetTOTAtPM(i);
    // read TOT slope
    fTOTSlope[i] = static_cast<CHODReconstruction*>(fReco)->GetTOTSlope(i);
  }
  // read TOT pdf
  for(Int_t j=0; j<12; j++) {
    fTOTpdf[j] = static_cast<CHODReconstruction*>(fReco)->GetTOTpdf(j);
  }

  // functions for TOT smearing (parameters taken from the TR data)
  TOTPdfMainPeak = new TF1("TOTPdfMainPeak","gaus(0)+gaus(3)",-5.,5.);
  TOTPdfSecondPeak = new TF1("TOTPdfSecondPeak","gaus(0)+gaus(3)",-5.,5.);
  // first (main) peak
  for(Int_t i=0; i<6; i++){
    TOTPdfMainPeak->SetParameter(i, fTOTpdf[i] );
  }
  // second peak (high energies, afterpulse above threshold)
  for(Int_t i=0; i<6; i++){
    TOTPdfSecondPeak->SetParameter(i, fTOTpdf[i+6] );
  }

  fDigiEvent = new TDCEvent(TCHODDigi::Class());
}

CHODDigitizer::~CHODDigitizer() {}

TDetectorVEvent * CHODDigitizer::ProcessEvent(TDetectorVEvent * tEvent)
{
  if(tEvent->GetHits()->GetClass()->InheritsFrom("TVDigi") || tEvent->IsA() == TSpecialTriggerEvent::Class()) return tEvent; 

  // Initialization
  TCHODEvent * CHODEvent = static_cast<TCHODEvent*>(tEvent);
  fDigiEvent->Clear();
  (*(TVEvent*)fDigiEvent)=(*(TVEvent*)CHODEvent);

  Double_t depEnergy[fNumberOfChannel];
  Int_t hitID[fNumberOfChannel];
  Double_t hitTime[fNumberOfChannel];
  TVector3 hitCenter[fNumberOfChannel];

  for (Int_t j=0;j<fNumberOfChannel;j++){
    depEnergy[j] = 0.;
    hitID[j] = -1;
    hitTime[j] = 1e5;
    hitCenter[j].SetXYZ(0.,0.,0.);
  }


  // Sum the energy in each slab and take the earliest energy deposit time as a hit time
  Int_t thisChannelMCID = -1;
  for (Int_t jCHODHit=0; jCHODHit<CHODEvent->GetNHits(); jCHODHit++){
    TCHODHit *Hit = static_cast<TCHODHit*>(CHODEvent->GetHits()->At(jCHODHit)); ////
    thisChannelMCID = Hit->GetChannelID();
    if (thisChannelMCID >= fNumberOfChannel) continue; // new CHOD hits are currently ignored

    depEnergy[thisChannelMCID] += Hit->GetEnergy();
    hitCenter[thisChannelMCID] += Hit->GetEnergy()*Hit->GetPosition();
    Hit->SetEnergy(depEnergy[thisChannelMCID]);
    hitID[thisChannelMCID] = jCHODHit;
    if(Hit->GetTime()<hitTime[thisChannelMCID]) hitTime[thisChannelMCID] = Hit->GetTime();

  } 

  // normalizing hit center coordinate
  for(Int_t MCID=0; MCID<fNumberOfChannel; MCID++){
    if (depEnergy[MCID] < fHitEnergyThreshold) continue;

    TCHODHit *Hit = static_cast<TCHODHit*>(CHODEvent->GetHits()->At(hitID[MCID])); ////

    hitCenter[MCID].SetX(hitCenter[MCID].X() / depEnergy[MCID]);
    hitCenter[MCID].SetY(hitCenter[MCID].Y() / depEnergy[MCID]);
    hitCenter[MCID].SetZ(hitCenter[MCID].Z() / depEnergy[MCID]);

    Hit->SetPosition(hitCenter[MCID]);
  }

  // Digitization for hits with a cut on energy, digis are created for low energy FEE 
  // MCID - CHOD channel number (ID according to the MC channel map)
  for (Int_t MCID=0; MCID<fNumberOfChannel; MCID++){
    // Reco ID
    Int_t RecoID = GetRecoID(MCID);

    // CHOD plane
    Int_t ThisPlane = static_cast<CHODReconstruction*>(fReco)->GetPlane(RecoID);
    // CHOD quadrant
    Double_t ThisQuadrant = static_cast<CHODReconstruction*>(fReco)->GetQuadrant(RecoID);

    if (depEnergy[MCID] < fHitEnergyThreshold) continue; 

    TCHODHit *Hit = static_cast<TCHODHit*>(CHODEvent->GetHits()->At(hitID[MCID]));

    // Convert position ID into RO channel ID
    Int_t iROch    =                      // RO channel ID
      static_cast<TDCBRawDecoder*>(static_cast<CHODReconstruction*>(fReco)->GetRawDecoder()->GetDecoder())->GetChannelRO(RecoID);
    if (iROch<0) continue; // channel not instrumented

    // Create a new Digi 
    TCHODDigi *Digi = static_cast<TCHODDigi*>(fDigiEvent->AddDigi(Hit));
    Digi->SetChannelID(RecoID);

    // hit position in the slab, counts from 0 (at the PM) to a slab length
    Double_t HitPosition = (ThisPlane == 0) ? // V-plane
      CHODGeometry::GetInstance()->GetSlabLength(MCID%16) - TMath::Abs(Hit->GetPosition().Y()) :
      CHODGeometry::GetInstance()->GetSlabLength(MCID%16) - TMath::Abs(Hit->GetPosition().X());

    // searching IP (counts from 0 to 15) - intersection point between two slabs from different planes
    // IP will be used for correct calculation of the time smearing
    Int_t IP;
    Double_t ThisCoordinate;
    if(ThisPlane==0) ThisCoordinate = TMath::Abs( Hit->GetPosition().Y() );
    else ThisCoordinate = TMath::Abs( Hit->GetPosition().X() );
    // CHOD quadrant = 11 narrow slabs (in the center) + 5 wide slabs
    Int_t NumberOfNarrowSlabs = 11;
    Double_t NarrowSlabWidth = CHODGeometry::GetInstance()->GetSlabWidth(0);
    Double_t WideSlabWidth = CHODGeometry::GetInstance()->GetSlabWidth(15);
    Double_t CoordinateThreshold = NarrowSlabWidth * NumberOfNarrowSlabs;
    // IP for quadrants where transverse slab numbering starts from narrow slabs
    if(ThisCoordinate < CoordinateThreshold) IP = ThisCoordinate/NarrowSlabWidth;
    else IP = NumberOfNarrowSlabs + (ThisCoordinate - CoordinateThreshold)/WideSlabWidth;
    // IP for quadrantswhere transverse slab numbering starts from wide slabs
    if(ThisQuadrant==0 || ThisQuadrant==2 || ThisQuadrant==5 || ThisQuadrant==7) IP = 15 - IP;

    // signal propagation time (SPTime) from the hit position to the PM
    Double_t SPTime = HitPosition * fLightVelocity[RecoID];

    Double_t FineTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256.;

    // subtracting beam TOF from the target to the first CHOD plane, adding SPTime, T0 and StationsT0
    Double_t CorrectedTime = hitTime[MCID] + FineTime 
      - static_cast<CHODReconstruction*>(fReco)->GetStationMCToF(Hit->GetStationID())
      + SPTime*ns + fChannels[RecoID]->GetT0() + fReco->GetT0Correction(Digi);
    if (ThisPlane == 1) CorrectedTime -= static_cast<CHODReconstruction*>(fReco)->GetTOFBetweenCHODPlanes(); // correct beam ToF for the H-plane

    // calculating TOT as a function of the coordinate 
    Double_t TwoParticleTOTThreshold = 15.;
    Double_t TOTTwoParticlesAverageValue = 22.58;

    Double_t TOT = fTOTAtPM[RecoID] - fTOTSlope[RecoID] * HitPosition;

    // adding smearing to the TOT
    // smearing TOT using 2 special functions (for the first and the second peak) from TR data
    gRandom = fRandom; // set gRandom to fRandom to ensure reproducibility of GetRandom()
    Double_t dTOT1 = TOTPdfMainPeak->GetRandom() + fRandom->Gaus(0, 0.5);
    Double_t dTOT2 = TOTPdfSecondPeak->GetRandom();

    if(TOT <= TwoParticleTOTThreshold) {
      TOT += dTOT1;
      //CorrectedTime += fSlewCorrSlope[RecoID][IP] * dTOT1;
      CorrectedTime += fSlewCorrSlope[RecoID][IP] * TOT + fSlewCorrConst[RecoID][IP];
    }
    else {
      TOT = TOTTwoParticlesAverageValue + dTOT2;
      //CorrectedTime += fSlewCorrSlope[RecoID][IP] * dTOT2;
      //CorrectedTime += fSlewCorrSlope[RecoID][IP] * TOT + fSlewCorrConst[RecoID][IP];
      CorrectedTime += fSlewCorrSlope[RecoID][IP] * 15 + fSlewCorrConst[RecoID][IP];
    }

    Double_t TOT_offset = 0.;
    if (RecoID == 79) TOT_offset = 7.; // special offset for slab #79
    TOT += TOT_offset;

    // Hit time with TDC correction (TdcCalib = ClockPeriod/256)                                                                                                     
    Double_t DigitizedLeading = (Int_t)(CorrectedTime/ns/TdcCalib)*TdcCalib;

    // trailing (digitized)
    Double_t DigitizedTrailing = (Int_t)((CorrectedTime+TOT)/ns/TdcCalib)*TdcCalib;

    // write digitized TOT to the Width variable (if > 5ns)
    Double_t Width = 0.;
    if( (DigitizedTrailing - DigitizedLeading) > 5.) Width = DigitizedTrailing - DigitizedLeading;

    Digi->SetLeadingEdge(DigitizedLeading);
    Digi->SetTrailingEdge(DigitizedLeading + Width);
    if(Width>0.) Digi->SetDetectedEdge(3);
    else Digi->SetDetectedEdge(1);

  }

  return fDigiEvent;
}

Int_t CHODDigitizer::GetRecoID(Int_t MCID)
{
  Int_t RecoID = -1;

  // V-plane, quadrants (the same for MC and Reco) 1 and 3
  if((MCID/16)==0 || (MCID/16)==2) RecoID = MCID;
  // V-plane, quadrants (the same for MC and Reco) 2 and 4
  if((MCID/16)==1 || (MCID/16)==3) RecoID = (15 - MCID%16) + (MCID/16)*16;

  // H-plane, MC quadrant 1 (Reco quadrant 2)
  if((MCID/16)==4) RecoID = MCID + 16;
  // H-plane, MC quadrant 2 (Reco quadrant 3)
  if((MCID/16)==5) RecoID = (15 - MCID%16) + 96;
  // H-plane, MC quadrant 3 (Reco quadrant 4)
  if((MCID/16)==6) RecoID = MCID + 16;
  // H-plane, MC quadrant 4 (Reco quadrant 1)
  if((MCID/16)==7) RecoID = (15 - MCID%16) + 64;


  return RecoID;
}
