// --------------------------------------------------------------
// History:
// v 0.3 Mario Bragadireanu  (MUV3, LAV & SAC used as example for the RecoHit class and T0 manipulation) 16 August 2016
// v 0.2 Mario Bragadireanu  (LAV & SAC used as example for the RecoHit class) 08 August 2016
// v 0.1 Mario Bragadireanu 04 August 2016
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
// --------------------------------------------------------------
/// \class HACReconstruction
/// \Brief
/// HASC event reconstruction \n
/// \EndBrief
/// \Detailed
/// New version: \n
/// - read the HASC digis; \n
/// - create 3 vectors: lead times, trail times, digi id; and order them after lead time; \n
/// - loop over all digis and add all digis in the same SiPM to the reconstruction of hits; \n
/// - loop over all selected digis and create a reco hit each time there is a digi \n
///   in a lower threshold than the one before it (this does not need any time requirement \n
///   since the digis are already time ordered); \n
/// - add all digis that do not meet the above requirement to the newly created reco hit; \n
/// - correct reco hit time for slewing; \n
/// - compute charge of signal in SiPM; \n
/// \n
/// Previous version: \n
/// - read the HASC digis (the digitized time when the leading and trailing edge's of LAVFEE outputs occurred); \n
/// - check that there is leading + trailing edge associated to the channel id; \n
/// - copy the time information into a 3D array (dimensioned according to HASC geometry and number of thresholds/SiPM channel (4)); \n
/// - if only the first (lowest) threshold is passed the leading time is atributed to the hit time; \n
/// - if the first two thresholds are passed the hit time is attributed to the intersection of the line passing \n
/// between the two points with coordinates (threshold amplitude (mV), threshold leading time) and the time axis; \n
/// - the charge (SiPM analog signal integral) is aproximated from the areas of the triangles formed assuming that the leading edge is linear \n
/// (defined by a pair of subsequent theresholds) and the trailing edge is aproximated by 1, 2 or 3 lines with slopes defined by the trailing \n
/// time information and the thresholds amplitude;\n
/// - the section hit time and the charge associated to the signal are recorded as hits \n
/// \n
///   HASC in ECN3 - ascii drawing \n
///\n
///                  .---.---.---. \n
///         *  *     | 6 | 7 | 8 | \n
///      *        *  .---.---.---. \n
///     *     x    * | 3 | 4 | 5 | \n
///     *  beam    * .---.---.---. \n
///      *        *  | 0 | 1 | 2 | \n
///         *  *     '---'---'---' \n
///         pipe     HASC Modules  \n
///  \n
/// 9 Modules x 10 SiPM's /Module X 4 Thresholds/SiPM = 360 TDC channels  \n
///  \n
/// 1 SiPM "reads" - 6 scintillator tiles interleaved with 6 lead tiles \n
/// 6 scintillator tiles interleaved with 6 lead tiles = > HASC section \n
/// Section 9 (of each module) is facing the beam. \n
/// \n
/// \n
/// Real life conversion of HASC.conf channels \n
///  \n
///     Module     0      1        2   \n
///    +-----+ \n
///  S |  0  |   0- 3  40-43    80-83     T \n
///  I +-----+                            D \n
///  P |  1  |   4- 7  44-47    84-87     C \n
///  M +-----+ \n
///    |  2  |   8-11  48-51    88-91     C \n
///  C +-----+ \n
///  H |  3  |  12-15  52-55    92-95     H \n
///  A +-----+ \n
///  N |  4  |  16-19  56-59    96-99     A \n
///  N +-----+ \n
///  E |  5  |  20-23  60-63  100-103     N \n
///  L +-----+ \n
///  S |  6  |  24-27  64-67  104-107     N \n
///    +-----+ \n
///    |  7  |  28-31  68-71  108-111     E \n
///    +-----+ \n
///    |  8  |  32-35  72-75  112-115     L \n
///    +-----+ \n
///    |  9  |  36-39  76-79  116-119     S \n
///    +-----+ \n
///  beam ^ \n
///   \n
///    Module       3        4        5    \n
///    +-----+   \n
///  S |  0  |   120-123  160-163  200-203   T \n
///  I +-----+                               D \n
///  P |  1  |   124-127  164-167  204-207   C \n
///  M +-----+                              \n
///    |  2  |   128-131  168-171  208-211   C \n
///  C +-----+                              \n
///  H |  3  |   132-135  172-175  212-215   H \n
///  A +-----+                              \n
///  N |  4  |   136-139  176-179  216-219   A \n
///  N +-----+                              \n
///  E |  5  |   140-143  180-183  220-223   N \n
///  L +-----+                              \n
///  S |  6  |   144-147  184-187  224-227   N \n
///    +-----+                              \n
///    |  7  |   148-151  188-191  228-231   E \n
///    +-----+                              \n
///    |  8  |   152-155  192-195  232-235   L \n
///    +-----+                              \n
///    |  9  |   156-159  196-199  236-239   S \n
///    +-----+ \n
///  beam ^ \n
///   \n
///    Module      6        7        8 \n
///    +-----+  \n
///  S |  0  |   240-243  280-283  320-323   T \n
///  I +-----+                               D \n
///  P |  1  |   244-247  284-287  324-327   C \n
///  M +-----+                             \n
///    |  2  |   248-251  288-291  328-331   C \n
///  C +-----+                             \n
///  H |  3  |   252-255  292-295  332-335   H \n
///  A +-----+                             \n
///  N |  4  |   256-259  296-299  336-339   A \n
///  N +-----+                             \n
///  E |  5  |   260-263  300-303  340-343   N \n
///  L +-----+                             \n
///  S |  6  |   264-267  304-307  344-347   N \n
///    +-----+                             \n
///    |  7  |   268-271  308-311  348-351   E \n
///    +-----+                             \n
///    |  8  |   272-275  312-315  352-355   L \n
///    +-----+                             \n
///    |  9  |   276-279  316-319  356-359   S \n
///    +-----+ \n
///  beam ^ \n
/// \n
/// The correspondence between SiPM sensors and Modules, Sections is: \n
/// SiPM Id:  0- 9 => Module 0 => Section 0-9 \n
/// SiPM Id: 10-19 => Module 1 => Section 0-9 \n
/// SiPM Id: 20-29 => Module 2 => Section 0-9 \n
/// SiPM Id: 30-39 => Module 3 => Section 0-9 \n
/// SiPM Id: 40-49 => Module 4 => Section 0-9 \n
/// SiPM Id: 50-59 => Module 5 => Section 0-9 \n
/// SiPM Id: 60-69 => Module 6 => Section 0-9 \n
/// SiPM Id: 70-79 => Module 7 => Section 0-9 \n
/// SiPM Id: 80-89 => Module 8 => Section 0-9 \n
/// \n
///\author Mario Bragadireanu (mario@ifin.nipne.ro) \n
///\EndDetailed

#include "vector"
#include "Riostream.h"
#include "HACGeometry.hh"
#include "HACReconstruction.hh"
#include "HACChannel.hh"
#include "THACDigi.hh"
#include "TDCBRawDecoder.hh"
#include "TRecoHACEvent.hh"
#include "TSpecialTriggerEvent.hh"
#include "NA62ConditionsService.hh"
#include "NA62Reconstruction.hh"
#include "TString.h"
#include "TRegexp.h"

HACReconstruction::HACReconstruction(TFile* HistoFile, TString ConfigFileName) :
  NA62VReconstruction(HistoFile, "HAC", ConfigFileName),
  fHGoodMap(nullptr),
  fHBadMap(nullptr),
  fCandidate(nullptr),
  fTimeWindow(-999.)
{
  // Initialize variables and histos

  fRecoEvent = new TRecoHACEvent();

  fNSelectedCandidatesInTimeSlots.clear();
  ParseConfFile(ConfigFileName);
  ResetHistograms();
}

HACReconstruction::~HACReconstruction() {
  if (fRecoEvent) {
    delete fRecoEvent;
    fRecoEvent = 0;
  }
  DeleteHistograms();
}

// Read HAC reconstruction parameters from a configuration file

void HACReconstruction::ParseConfFile(TString ConfFileName) {

  std::ifstream confFile(ConfFileName.Data());
  if (!confFile.is_open()) {
    perror(ConfFileName);
    exit(kWrongConfiguration);
  }

  TString Line;
  while (Line.ReadLine(confFile)) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("EnableSlewingCorrection")) {
      fEnableSlewingCorr = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    } else if (Line.BeginsWith("SlewingCorrectionFileInput")) {
      TObjArray *l = Line.Tokenize(" ");
      fSlewingCorrFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    } else if (Line.BeginsWith("ThresholdsFileInput")) {
      TObjArray *l = Line.Tokenize(" ");
      fThresholdsFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    } else if (Line.BeginsWith("SignalParameterFileInput")) {
      TObjArray *l = Line.Tokenize(" ");
      fSignalParametersFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    } else if (Line.BeginsWith("CandidateTimeWindow")) {
      fTimeWindow = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * ns;
      continue;
    }
      /* Preparation for digitization

    } else if (Line.BeginsWith("ChargeToTOffset=")) {
      TObjArray *l = Line.Tokenize(" ");
      for (Int_t ithr = 0; ithr < 4; ithr++)
        fChargeToTOffset.push_back(static_cast<TObjString*>( (l->At(ithr + 1)))->GetString().Atof());
      delete l;
      continue;
    } else if (Line.BeginsWith("ChargeToTConstant=")) {
      TObjArray *l = Line.Tokenize(" ");
      for (Int_t ithr = 0; ithr < 4; ithr++)
        fChargeToTConstant.push_back(static_cast<TObjString*>( (l->At(ithr + 1)))->GetString().Atof());
      delete l;
      continue;
    } else if (Line.BeginsWith("ChargeToTSlope=")) {
      TObjArray *l = Line.Tokenize(" ");
      for (Int_t ithr = 0; ithr < 4; ithr++)
        fChargeToTSlope.push_back(static_cast<TObjString*>( (l->At(ithr + 1)))->GetString().Atof());
      delete l;
      continue;
    }*/
  }
  confFile.close();
  if (!fSlewingCorrFileName.Length()) {
    std::cerr << "[HACReconstruction] ERROR: slewing correction file not defined" << std::endl;
    exit(kWrongConfiguration);
  }
  if (!fThresholdsFileName.Length()) {
    std::cerr << "[HACReconstruction] ERROR: thresholds file not defined" << std::endl;
    exit(kWrongConfiguration);
  }
  if (fTimeWindow < 0.){
    std::cerr << "[HACReconstruction] ERROR: Time window < 0."<< std::endl;
    exit(kWrongConfiguration);
  }
  /* Preparation for Digitization
  if (!fSignalParametersFileName.Length()) {
    std::cerr << "[HACReconstruction] ERROR: signal parameters file not defined" << std::endl;
    exit(kWrongConfiguration);
  }
   */
  ReadThresholds(); //Needs to be here for HACDigitizer
}

void HACReconstruction::Init(NA62VReconstruction* MainReco) {
  //common part for all the subdetectors
  NA62VReconstruction::Init(MainReco);

  for (Int_t ich = 0; ich < fNChannels; ich++) {
    Int_t PositionID = fRawDecoder->GetDecoder()->GetChannelRemap(ich);
    fChannels[ich] = new HACChannel(PositionID, ich, fChannelHistograms);
  }
  ReadT0s();
  ReadSlewCorrParams();
  InitHistograms();

  Double_t NSlotsMax = 0.;
  for(UInt_t iROMezzanine=0;iROMezzanine<fRawDecoder->GetDecoder()->GetNROMezzanines();iROMezzanine++){
    if(NSlotsMax<fRawDecoder->GetDecoder()->GetNSlots(iROMezzanine)) NSlotsMax = fRawDecoder->GetDecoder()->GetNSlots(iROMezzanine);
  }
  fNSelectedCandidatesInTimeSlots.resize(NSlotsMax);
}
///////////////////////////////////////
// Read the channel slewing corrections

void HACReconstruction::ReadSlewCorrParams() {

  NA62ConditionsService::GetInstance()->Open(fSlewingCorrFileName);
  TString Line;
  Int_t ch = 0;
  Int_t threshold = 0;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fSlewingCorrFileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    if (ch < 90) {
      Int_t nPars = threshold == 0 ? 7 : 6;
      for (Int_t iPar = 0; iPar < nPars; iPar++){
        if (threshold == 0)
          fSlewCorrParamThr0[ch][iPar] = static_cast<TObjString*>(l->At(iPar+1))->GetString().Atof();
        else
          fSlewCorrParamThr1[ch][iPar] = static_cast<TObjString*>(l->At(iPar+1))->GetString().Atof();
      }

      if (threshold == 0)
          threshold++;
      else{
          ch++;
          threshold = 0;
      }
    }
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(fSlewingCorrFileName);
}

//////////////////////////////////////////////////////////

void HACReconstruction::ReadThresholds() {

  NA62ConditionsService::GetInstance()->Open(fThresholdsFileName);
  TString Line;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fThresholdsFileName))) {
    if (Line.BeginsWith("#"))
      continue;
    TObjArray *l = Line.Tokenize(" ");
    Int_t channelID = static_cast<TObjString*>(l->At(0))->GetString().Atoi();
    for (Int_t ithr = 1; ithr <= 4; ithr++)
      fThresholdValues[4 * channelID + ithr - 1 ] = static_cast<TObjString*>(l->At(ithr))->GetString().Atof();
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(fThresholdsFileName);
}

TDetectorVEvent * HACReconstruction::Trigger(TDetectorVEvent * tEvent, Event* /*tGenEvent*/) {
  return tEvent;
}

void HACReconstruction::StartOfBurst() {
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
  for(UInt_t iSlot=0;iSlot<fNSelectedCandidatesInTimeSlots.size();iSlot++) fNSelectedCandidatesInTimeSlots[iSlot]=0;
}

void HACReconstruction::EndOfBurst() {
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors
  //Particle rate evaluation
  Double_t TotalTimePerSlot = static_cast<NA62Reconstruction*>(fMainReco)->GetNProcessedEventsInFile()*1.e-3*ClockPeriod; //us

  UInt_t NSlotsUsed = 0;
  Double_t NSelectedCandidatesInTimeSlotsMean  = 0.;
  Int_t LastSlotID = fRawDecoder->GetDecoder()->GetLastSlotID(0);
  for(UInt_t iSlot=1; iSlot<fNSelectedCandidatesInTimeSlots.size()-1; iSlot++){ // Skip first and last slot
    if(iSlot==fNSelectedCandidatesInTimeSlots.size()-LastSlotID-1) continue; // Skip trigger slot
    NSelectedCandidatesInTimeSlotsMean  += fNSelectedCandidatesInTimeSlots[iSlot];
    NSlotsUsed++;
  }
  NSelectedCandidatesInTimeSlotsMean /= NSlotsUsed;

  Double_t NSelectedCandidatesInTimeSlotsSigma = 0.;
  for(UInt_t iSlot=1; iSlot<fNSelectedCandidatesInTimeSlots.size()-1; iSlot++){ // Skip first and last slot
    if(iSlot==fNSelectedCandidatesInTimeSlots.size()-LastSlotID-1) continue; // Skip trigger slot
    NSelectedCandidatesInTimeSlotsSigma += pow(fNSelectedCandidatesInTimeSlots[iSlot]-NSelectedCandidatesInTimeSlotsMean,2.);
  }
  NSelectedCandidatesInTimeSlotsSigma = sqrt(NSelectedCandidatesInTimeSlotsSigma)/(NSlotsUsed*(NSlotsUsed-1));

  Double_t fParticleRateMean  = NSelectedCandidatesInTimeSlotsMean/TotalTimePerSlot; //MHz
  Double_t fParticleRateSigma = NSelectedCandidatesInTimeSlotsSigma/TotalTimePerSlot; //MHz
  std::cout << "[HACReconstruction] Mean Particle Rate: (" << fParticleRateMean << "+/-" << fParticleRateSigma << ") MHz" << std::endl;
}

TRecoVEvent * HACReconstruction::ProcessEvent(TDetectorVEvent* tEvent, Event* tGenEvent) {
  if (tEvent->IsA() == TSpecialTriggerEvent::Class()) {
    return 0;
  }

  //common part for all the subdetectors
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  fTdcEvent = static_cast<TDCEvent*>( tEvent);
  Int_t nDigis = fTdcEvent->GetNHits();
  if (!nDigis) return fRecoEvent;

  TClonesArray& Digis = (*(fTdcEvent->GetHits()));
  for (Int_t iDigi = 0; iDigi < nDigis; iDigi++) { // Main loop
    THACDigi *Digi = static_cast<THACDigi*>( Digis[iDigi]);
    Int_t ROch = static_cast<TDCBRawDecoder*>( fRawDecoder->GetDecoder())->GetChannelRO(Digi->GetChannelID());
    if (ROch < 0)
      continue; //Maked channel
    if (Digi->GetDetectedEdge() == 3) { //leading+trailing edges
      fHChID->Fill(Digi->GetChannelID());
      fHRawLeadTimevsChID->Fill(Digi->GetChannelID(), Digi->GetLeadingEdge() - GetT0Correction(Digi)); // corrected for global T0
      fHRawTrailTimevsChID ->Fill(Digi->GetChannelID(), Digi->GetTrailingEdge() - GetT0Correction(Digi)); // corrected for global T0

      if (0 <= Digi->GetModuleID() && Digi->GetModuleID() < 9 && 0 <= Digi->GetSiPMID() && Digi->GetSiPMID() < 10) {
        Double_t T0 = (fEnableT0) ? fChannels[ROch]->GetT0() : 0.0;
        fHLeadTimevsChID->Fill(Digi->GetChannelID(), Digi->GetLeadingEdge() - GetT0Correction(Digi) - T0);
        fHTrailTimevsChID ->Fill(Digi->GetChannelID(), Digi->GetTrailingEdge() - GetT0Correction(Digi) - T0);
      }
    }
  }
  //Time sort good digis and create reco hits
  Digis.Sort(nDigis);
  GroupDigis(Digis);

  //Candidates reconstruction starts here.
  //Cedar used as example.
  Int_t fNHits = fRecoEvent->GetNHits();
  Int_t fNIterations = 2;
  for (Int_t iIter = 0; iIter < fNIterations; iIter++) {
    for (Int_t iHit = 0; iHit < fNHits; iHit++) {
      TRecoHACHit *fHit = static_cast<TRecoHACHit*>( fRecoEvent->GetHit(iHit));

      Double_t fHitTime = fHit->GetTime();
      Bool_t HitFromNewCluster = kTRUE;
      Double_t HitCandidateTime = 1.e28;
      Int_t CandidateID = -1;

      for (Int_t iCand = 0; iCand < fRecoEvent->GetNCandidates(); iCand++) {
        fCandidate = static_cast<TRecoHACCandidate*>( fRecoEvent->GetCandidate(iCand));

        if (fabs(fCandidate->GetTime() - fHitTime) < HitCandidateTime) {
          HitCandidateTime = fabs(fCandidate->GetTime() - fHitTime);
          CandidateID = iCand;
        }
      }

      if (CandidateID >= 0) {
        fCandidate = static_cast<TRecoHACCandidate*>( fRecoEvent->GetCandidate(CandidateID));
        if (fabs(fCandidate->GetTime() - fHitTime) < 0.5 * fTimeWindow) HitFromNewCluster = kFALSE;
      }

      if (HitFromNewCluster) { //hit not consistent with previous candidates: create a new candidate
        CandidateID = fRecoEvent->GetNCandidates();
        fCandidate = static_cast<TRecoHACCandidate*>( fRecoEvent->AddCandidate());
      }

      Bool_t MissingHit = kTRUE;
      if (iIter > 0) { //second iteration: check to see if current hit is already in a candidate
        for (Int_t i = 0; i < fCandidate->GetNHits(); i++) {
          if (fCandidate->GetHitsIndexes()[i] == iHit) MissingHit = kFALSE;
        }
      }

      if (MissingHit) {
        fCandidate->AddHit(iHit);
        fCandidate->UpdateTime(fHitTime);
        fCandidate->SetCharge(fCandidate->GetCharge() + fHit->GetChargeModuleSection());
      }
    }

    for (Int_t kCand = 0; kCand < fRecoEvent->GetNCandidates(); kCand++) {
      fCandidate = static_cast<TRecoHACCandidate *>(fRecoEvent->GetCandidate(kCand));
      Double_t CandidateTime = fCandidate->GetTime();

      fCandidate->SetIsSelected(kFALSE);
      if (fCandidate->GetNHits() > 1) fCandidate->SetIsSelected(kTRUE); //Candidates with more than one hit are expected

      Double_t DeltaTimeClosestCandidate = 1.e28; //ns
      Int_t NHitsClosestCandidate = 0;

      for (Int_t jCand = 0; jCand < fRecoEvent->GetNCandidates(); jCand++) {
        TRecoHACCandidate *OtherCandidate = static_cast<TRecoHACCandidate*>( fRecoEvent->GetCandidate(jCand));

        if (kCand != jCand && fabs(OtherCandidate->GetTime() - CandidateTime) < fabs(DeltaTimeClosestCandidate)) {
          DeltaTimeClosestCandidate = OtherCandidate->GetTime() - CandidateTime;
          NHitsClosestCandidate = OtherCandidate->GetNHits();
        }
      }
      fCandidate->SetDeltaTimeClosestCandidate(DeltaTimeClosestCandidate);
      fCandidate->SetNHitsClosestCandidate(NHitsClosestCandidate);
    }

    if (iIter < fNIterations - 1) {
      for (Int_t kCand = 0; kCand < fRecoEvent->GetNCandidates(); kCand++) {
        fCandidate = static_cast<TRecoHACCandidate*>( fRecoEvent->GetCandidate(kCand));

        if (!fCandidate->GetIsSelected() || (fCandidate->GetNHitsClosestCandidate() > fCandidate->GetNHits() &&
                fabs(fCandidate->GetDeltaTimeClosestCandidate()) < fTimeWindow)) {
          fRecoEvent->RemoveCandidate(kCand);
          kCand--;
        } else {//remove bad hits
          for (Int_t iHit = 0; iHit < fCandidate->GetNHits(); iHit++) {
            TRecoHACHit *fHit = static_cast<TRecoHACHit*>( fCandidate->GetHit(iHit));
            Double_t HitTime = fHit->GetTime();

            if (fabs(HitTime - fCandidate->GetTime()) > 0.5 * fTimeWindow) {
              fCandidate->RemoveHit(iHit);
              fCandidate->UpdateTime();
              fCandidate->SetCharge(fCandidate->GetCharge() - fHit->GetChargeModuleSection());
              iHit--;
            }
          }
        }
      }
    }
  }

  for(Int_t iCand = 0; iCand < fRecoEvent->GetNCandidates(); iCand++){
    fCandidate = static_cast<TRecoHACCandidate*>( fRecoEvent->GetCandidate(iCand));

    //Compute particle rate with candidates with at least 4 hits
    if(fCandidate->GetNHits() >= 4){
      Int_t NSlots = fNSelectedCandidatesInTimeSlots.size();
      Int_t LastSlotID = fRawDecoder->GetDecoder()->GetLastSlotID(0);

      for(Int_t iSlot=LastSlotID-NSlots+1; iSlot<=LastSlotID; iSlot++){
        if (iSlot*ClockPeriod<fCandidate->GetTime() && fCandidate->GetTime()<=(iSlot+1.)*ClockPeriod)
	  fNSelectedCandidatesInTimeSlots[NSlots-1+iSlot-LastSlotID]++;
      }
    }
  }
  return fRecoEvent;
}

void HACReconstruction::EndProcessing() {

  NA62VReconstruction::EndProcessing(); // call base class for raw hist output
  // Write histo's
  SaveHistograms();
}

void HACReconstruction::FillTimes(Double_t ReferenceTime) {
  //common part for all the subdetectors

  NA62VReconstruction::FillTimes(ReferenceTime);

  //////////////////////////////////////////////////////////
  // RecoHit times wrt the reference time

  Int_t NRecoHits = fRecoEvent->GetNHits();
  TClonesArray &Hits = (*(fRecoEvent->GetHits()));

  // RecoHit times wrt the reference time
  for (Int_t iHit = 0; iHit < NRecoHits; iHit++) {
    TRecoHACHit *Hit = static_cast<TRecoHACHit*>( Hits[iHit]);
    Int_t ROch = static_cast<TDCBRawDecoder*>( fRawDecoder->GetDecoder())->GetChannelRO(Hit->GetChannelID());
    if (ROch < 0){
      std::cerr<<"[HACReconstruction] WARNING: Channel "<<Hit->GetChannelID()<<" not instrumented"<<std::endl;
      continue; //Masked channel
    }
    Double_t T0 = (fEnableT0) ? fChannels[ROch]->GetT0() : 0.0;
    Double_t HitTime = Hit->GetTime(); // corrected for global T0, channel T0s and slewing corrections
    Double_t HitTimeNoT0 = Hit->GetTime() + T0; // corrected for global T0 and slewing corrections only
    Double_t dT = HitTime - ReferenceTime;
    Double_t dT_NoT0 = HitTimeNoT0 - ReferenceTime;
    // The standard input histograms for T0 computation defined in NA62VReconstruction
    for (UInt_t iThrCh = 0; iThrCh < 4; iThrCh++) {
      //use T0 of lowest thresholds for all the thresholds
      if (Hit->GetChannelID() == 83 && iThrCh == 3) continue; //exception
      if (fHRecoHitTimeWrtReferenceVsROChannel) fHRecoHitTimeWrtReferenceVsROChannel->Fill(ROch + iThrCh, dT);
      if (fHRecoHitTimeWrtReferenceVsROChannelNoT0) fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(ROch + iThrCh, dT_NoT0);
    }
  }
}

void HACReconstruction::InitHistograms() {

  // HASC histograms
  GetOrMakeDir(fHistoFile, "HACMonitor")->cd();
  //
  fHChID = new TH1F("ChID", "TDCB Digitized Channels", 390, -0.5, 390 - 0.5);
  fHChID->GetXaxis()->SetTitle("HASC Channel ID (4 thr./ SiPM)");
  //
  fHEdgeFlagvsChID = new TH2F("EdgeFlagvsChID", "HASC Edge Flag vs HASC Channel ID", 390, -0.5, 390 - 0.5, 3, .5, 3.5);
  fHEdgeFlagvsChID->GetXaxis()->SetTitle("HASC Channel ID ( 4 thr. / SiPM)");
  fHEdgeFlagvsChID->GetYaxis()->SetTitle("Edge Status: 1=leading edge only, 2=trailing edge only, 3=both");
  //
  fHModuleID = new TH1F("ModuleID", "Hits/HASC modules", 9, 0., 9.);
  fHModuleID->GetXaxis()->SetTitle("HASC Module Id.");
  //
  fHModulevsSection = new TH2F("ModulEvsSection", "HASC Hit map Module vs Section", 9, 0., 9., 10, 0., 10.);
  fHModulevsSection->GetXaxis()->SetTitle("HASC Module Id.");
  fHModulevsSection->GetYaxis()->SetTitle("HASC Section Id.");
  //
  fHRawLeadTimevsChID = new TH2F("RawLeadTimevsChID", "HASC RAW leading times vs HASC Channel ID", 390, -0.5, 390 - 0.5, 400, -40., 40.);
  fHRawLeadTimevsChID->GetXaxis()->SetTitle("HASC Channel ID (4 thr./ SiPM)");
  fHRawLeadTimevsChID->GetYaxis()->SetTitle("Raw TDC Lead time [ns]");
  //
  fHRawTrailTimevsChID = new TH2F("RawTrailTimevsChID", "HASC RAW trailing times vs HASC Channel ID", 390, -0.5, 390. - 0.5, 400, -40., 40.);
  fHRawTrailTimevsChID->GetXaxis()->SetTitle("HASC Channel ID (4 thr./ SiPM)");
  fHRawTrailTimevsChID->GetYaxis()->SetTitle("Raw TDC Trail time [ns]");
  //
  fHLeadTimevsChID = new TH2F("LeadTimevsChID", "HASC leading times (fine T0 subtracted)vs HASC Channel ID", 390, -0.5, 390 - 0.5, 400, -40., 40.);
  fHLeadTimevsChID->GetXaxis()->SetTitle("HASC Channel ID (4 thr./ SiPM)");
  fHLeadTimevsChID->GetYaxis()->SetTitle("TDC Lead time (fine T0 subtracted) [ns]");
  //
  fHTrailTimevsChID = new TH2F("TrailTimevsChID", "HASC trailing times (fine T0 subtracted) vs HASC Channel ID", 390, -0.5, 390. - 0.5, 400, -40., 40.);
  fHTrailTimevsChID->GetXaxis()->SetTitle("HASC Channel ID (4 thr./ SiPM)");
  fHTrailTimevsChID->GetYaxis()->SetTitle("TDC Trail time (fine T0 subtracted)[ns]");
  //
  fHTimevsSiPMid = new TH2F("TimevsSiPMid", "HASC Hit time (with fine T0 subtracted) vs HASC SiPM id", 90, -0.5, 90. - 0.5, 300., -150., 150.);
  fHTimevsSiPMid->GetXaxis()->SetTitle("HASC SiPM Id. (10 SiPM's/ HASC Module)");
  fHTimevsSiPMid->GetYaxis()->SetTitle("SiPM hit time fine T0 subtracted[ns]");
  //
  fHSlewCorrTimevsSiPMid = new TH2F("SlewCorrTimevsSiPMid", "HASC Hit time (with fine T0 subtracted and Slew) vs HASC SiPM id", 90, -0.5, 90. - 0.5, 300., -150., 150.);
  fHSlewCorrTimevsSiPMid->GetXaxis()->SetTitle("HASC SiPM Id. (10 SiPM's/ HASC Module)");
  fHSlewCorrTimevsSiPMid->GetYaxis()->SetTitle("SiPM hit time fine T0 subtracted and Slew corrected[ns]");
  //
  fHToTvsSiPMid = new TH2F("ToTvsSiPMid", "HASC summed ToT's vs HASC SiPMid", 90, -0.5, 90. - 0.5, 500, 0., 500.);
  fHToTvsSiPMid ->GetXaxis()->SetTitle("HASC SiPM Id. (10 SiPM's/ HASC Module)");
  fHToTvsSiPMid ->GetYaxis()->SetTitle("Summ of ToT's [ns]");
  //
  fHChargevsSiPMid = new TH2F("ChargevsSiPMid", "HASC Section Charge vs HASC SiPM id", 90, -0.5, 90. - 0.5, 400., 0., 400.);
  fHChargevsSiPMid->GetXaxis()->SetTitle("HASC SiPM Id. (10 SiPM's/ HASC Module)");
  fHChargevsSiPMid->GetYaxis()->SetTitle("SiPM Out (geom.) integral [pC]");
  //
}

void HACReconstruction::SaveHistograms() {

  fHistoFile->cd("HACMonitor");
  // HASC histograms
  fHChID -> Write();
  fHEdgeFlagvsChID -> Write();
  fHModuleID -> Write();
  fHModulevsSection-> Write();
  fHRawLeadTimevsChID -> Write();
  fHRawTrailTimevsChID -> Write();
  fHLeadTimevsChID -> Write();
  fHTrailTimevsChID -> Write();
  fHTimevsSiPMid->Write();
  fHSlewCorrTimevsSiPMid->Write();
  fHToTvsSiPMid->Write();
  fHChargevsSiPMid->Write();
  fHistoFile->cd("/");
}

void HACReconstruction::DeleteHistograms() {

  if (fHChID) delete fHChID;
  if (fHEdgeFlagvsChID) delete fHEdgeFlagvsChID;
  if (fHModuleID) delete fHModuleID;
  if (fHModulevsSection) delete fHModulevsSection;
  if (fHRawLeadTimevsChID) delete fHRawLeadTimevsChID;
  if (fHRawTrailTimevsChID) delete fHRawTrailTimevsChID;
  if (fHLeadTimevsChID) delete fHLeadTimevsChID;
  if (fHTrailTimevsChID) delete fHTrailTimevsChID;
  if (fHTimevsSiPMid) delete fHTimevsSiPMid;
  if (fHSlewCorrTimevsSiPMid) delete fHSlewCorrTimevsSiPMid;
  if (fHToTvsSiPMid) delete fHToTvsSiPMid;
  if (fHChargevsSiPMid) delete fHChargevsSiPMid;
  ResetHistograms();
}

void HACReconstruction::ResetHistograms() {

  fHChID = NULL;
  fHEdgeFlagvsChID = NULL;
  fHModuleID = NULL;
  fHModulevsSection = NULL;
  fHRawLeadTimevsChID = NULL;
  fHRawTrailTimevsChID = NULL;
  fHLeadTimevsChID = NULL;
  fHTrailTimevsChID = NULL;
  fHTimevsSiPMid = NULL;
  fHSlewCorrTimevsSiPMid = NULL;
  fHToTvsSiPMid = NULL;
  fHChargevsSiPMid = NULL;
}

//Method to group all digis in one physical channel into one or more RecoHits

void HACReconstruction::GroupDigis(TClonesArray& Digis) {
  Int_t nGoodDigis = Digis.GetEntriesFast();
  std::vector<Int_t> isAdded(nGoodDigis, 0); //flag to keep track of digis inclusion in RecoHits
  for (Int_t iDigi = 0; iDigi < nGoodDigis; iDigi++) {
    if (isAdded[iDigi] == 1)
      continue;
    THACDigi *digi1 = static_cast<THACDigi*>( Digis[iDigi]);
    //if (fabs(digi1->GetLeadingEdge()) > 900. || fabs(digi1->GetTrailingEdge()) > 900.)
    if (digi1->GetDetectedEdge() != 3)
      continue;

    isAdded[iDigi] = 1;
    //vectors to store time information in each module-section pair
    std::vector<Int_t> thresholdID;
    std::vector<Double_t> thresholdLeadTime;
    std::vector<Double_t> thresholdTrailTime;
    //=============================================================
    Int_t channelID = digi1->GetChannelID();
    Int_t ROch = static_cast<TDCBRawDecoder*>( fRawDecoder->GetDecoder())->GetChannelRO(channelID);
    if (ROch < 0)
      continue; //Masked channel
    Double_t T0 = (fEnableT0) ? fChannels[ROch]->GetT0() : 0.0;

    thresholdID.push_back(digi1->GetThresholdType());
    thresholdLeadTime.push_back(digi1->GetLeadingEdge() - GetT0Correction(digi1) - T0);
    thresholdTrailTime.push_back(digi1->GetTrailingEdge() - GetT0Correction(digi1) - T0);

    for (Int_t jDigi = iDigi + 1; jDigi < nGoodDigis; jDigi++) { //loop over the rest of digis and keep the ones in same SiPM
      if (isAdded[jDigi] == 1)
        continue;
      THACDigi *digi2 = static_cast<THACDigi*>( Digis[jDigi]);
      if (digi2->GetDetectedEdge() != 3)
        continue;
      if ((Int_t) digi2->GetChannelID() % 100 != channelID % 100) //same module and section requirement
        continue;

      isAdded[jDigi] = 1;
      thresholdID.push_back(digi2->GetThresholdType());
      ROch = static_cast<TDCBRawDecoder*>( fRawDecoder->GetDecoder())->GetChannelRO(digi2->GetChannelID());
      if (ROch < 0)
        continue; //Masked channel
      T0 = (fEnableT0) ? fChannels[ROch]->GetT0() : 0.0;

      thresholdLeadTime.push_back(digi2->GetLeadingEdge() - GetT0Correction(digi2) - T0);
      thresholdTrailTime.push_back(digi2->GetTrailingEdge() - GetT0Correction(digi2) - T0);
    }
    CreateRecoStructure(thresholdLeadTime, thresholdTrailTime, thresholdID, channelID % 100);

    thresholdID.clear();
    thresholdLeadTime.clear();
    thresholdTrailTime.clear();
  }
  isAdded.clear();
}

//Method to create reco hit structure from times and threshold id in one physical channel

void HACReconstruction::CreateRecoStructure(std::vector<Double_t>& leadTimes, std::vector<Double_t>& trailTimes, std::vector<Int_t>& thresholdID, Int_t channelID) {
  std::vector<Int_t> isInReco(leadTimes.size(), 0); //flag to keep track of digi inclusion in reco hit
  for (UInt_t iDigi = 0; iDigi < leadTimes.size(); iDigi++) { //loop over all digis in a given SiPM (ChannelID)
    if (isInReco[iDigi] == 1)
      continue;
    isInReco[iDigi] = 1;
    //If the digi is not already included in a reco hit, it is because it is part of a new reco hit
    //Initialization of the new Reco Hit
    TRecoHACHit *recoHit = static_cast<TRecoHACHit*>( static_cast<TDetectorVEvent*>( fRecoEvent)->AddHit());
    recoHit->Clear();
    recoHit->SetLeadingEdge(thresholdID[iDigi], leadTimes[iDigi]);
    recoHit->SetTrailingEdge(thresholdID[iDigi], trailTimes[iDigi]);
    recoHit->SetTime(leadTimes[iDigi]);
    recoHit->SetChannelID(channelID);
    recoHit->DecodeChannelID(); //method used to set ModuleID and SectionID

    //Digis are time ordered, this means that the next closest time to present digi
    //is the time of one of the next digis (or none <999.>) which is in a lower
    //or equal threshold as the present digi.
    Double_t closestTime = 1.e28;
    Double_t minDt = 1.e28;
    for (UInt_t jDigi = iDigi + 1; jDigi < leadTimes.size(); jDigi++) {
      if (leadTimes[jDigi] - leadTimes[iDigi] < minDt && thresholdID[jDigi] <= thresholdID[iDigi]) {
        closestTime = leadTimes[jDigi];
        minDt = leadTimes[jDigi] - leadTimes[iDigi];
      }
    }

    //Loop over all remaining digis and add their time info to the newly created
    //reco hit if their time is between the present digi and next closest time.
    for (UInt_t jDigi = iDigi + 1; jDigi < leadTimes.size(); jDigi++) {
      if (isInReco[jDigi] == 1 ||
              thresholdID[jDigi] <= thresholdID[iDigi] || leadTimes[jDigi] > closestTime)
        continue;
      Int_t kIsInThisHit = 1;
      for (Int_t ithr = 0; ithr < 4; ithr++) {
        if (recoHit->IsGrade(ithr))
          continue;
        if (ithr == thresholdID[jDigi] && recoHit->IsGrade(ithr)) {
          kIsInThisHit = 0;
          continue;
        }
        if (ithr < thresholdID[jDigi] && (leadTimes[jDigi] < recoHit->GetLeadingEdge(ithr) ||
                trailTimes[jDigi] >= recoHit->GetTrailingEdge(ithr))) {
          kIsInThisHit = 0;
          continue;
        }
      }
      if (kIsInThisHit == 0)
        continue;

      isInReco[jDigi] = 1;
      recoHit->SetLeadingEdge(thresholdID[jDigi], leadTimes[jDigi]);
      recoHit->SetTrailingEdge(thresholdID[jDigi], trailTimes[jDigi]);
    }
    FillRecoHit(recoHit);
  }
  isInReco.clear();
  return;
}

void HACReconstruction::FillRecoHit(TRecoHACHit* recoHit) {
  SlewCorrection(recoHit); //Method to correct and set the hit time for slewing and
  ComputeCharge(recoHit); //Method to compute the charge of the signal in SiPM

  Double_t totSum = 0.;
  for (Int_t ithr = 0; ithr < 4; ithr++) {
    if (!recoHit->IsGrade(ithr))
      continue;
    totSum += recoHit->GetToT(ithr);
  }
  recoHit->SetToTsumm(totSum);

  fHModuleID ->Fill(recoHit->GetModuleID());
  fHModulevsSection ->Fill(recoHit->GetModuleID(), recoHit->GetSiPMID());
  fHToTvsSiPMid->Fill(recoHit->GetChannelID(), totSum);
  fHSlewCorrTimevsSiPMid->Fill(recoHit->GetChannelID(), recoHit->GetTime());
  fHChargevsSiPMid->Fill(recoHit->GetChannelID(), recoHit->GetChargeModuleSection());
  if (recoHit->IsGrade0()) {
    fHTimevsSiPMid->Fill(recoHit->GetChannelID(), recoHit->GetLeadingEdge(0));
  }
}

void HACReconstruction::SlewCorrection(TRecoHACHit* recoHit/*, Int_t fNThresholds, Int_t fChannelID*/) {
  //Determine which is the first threshold
  if (!fEnableSlewingCorr)
    return;
  Int_t kFirstThreshold = 0;
  for (Int_t iThr = 0; iThr < 4; iThr++){
    if (recoHit->IsGrade(iThr)){
      kFirstThreshold = iThr;
      break;
    }
  }
  //Determine the region in order to use the apropiate fit function
  Double_t tot = recoHit->GetToT(kFirstThreshold);
  Bool_t isRegion1 = kTRUE;
  if (tot >= 32.49 || (kFirstThreshold != 0 && kFirstThreshold != 1))
    isRegion1 = kFALSE;

  Int_t nParameters = (kFirstThreshold == 0 && !isRegion1) ? 4 : 3;
  std::vector<Double_t> pars(nParameters);
  Int_t channelID = recoHit->GetChannelID();
  Double_t correction = 0.;
  for (Int_t iPar = 0; iPar < nParameters; iPar++){
    if (nParameters == 4)
      pars[iPar] = fSlewCorrParamThr0[channelID][3 + iPar];
    else if (kFirstThreshold == 0)
      pars[iPar] = fSlewCorrParamThr0[channelID][iPar];
    else if (isRegion1)
      pars[iPar] = fSlewCorrParamThr1[channelID][iPar];
    else pars[iPar] = fSlewCorrParamThr1[channelID][iPar+3];

    correction += pars[iPar]*pow(tot, iPar);
  }

  recoHit->SetTime(recoHit->GetLeadingEdge(kFirstThreshold) - correction);
}

Double_t HACReconstruction::SlewCorrection(const Double_t &tot, const Int_t &chID) {
  Double_t correction = 0.;
  //if(chID/100 > 1)
  //  return correction; //correction only applied for the first two thresholds

  Bool_t isRegion1 = kTRUE;
  if (tot >= 32.49)
    isRegion1 = kFALSE;

  Int_t nParameters = (chID/100 == 0 && !isRegion1) ? 4 : 3;
  std::vector<Double_t> pars(nParameters);
  for (Int_t iPar = 0; iPar < nParameters; iPar++){
    if (nParameters == 4)
      pars[iPar] = fSlewCorrParamThr0[chID%100][3 + iPar];
    else if (chID/100 == 0)
      pars[iPar] = fSlewCorrParamThr0[chID%100][iPar];
    else if (isRegion1)
      pars[iPar] = fSlewCorrParamThr1[chID%100][iPar];
    else pars[iPar] = fSlewCorrParamThr1[chID%100][iPar+3];

    correction += pars[iPar]*pow(tot, iPar);
  }

  return correction;
}


//Geometric method which computes the area of the signal using the aproximation
//of the waveform with a triangle.

void HACReconstruction::ComputeCharge(TRecoHACHit* recoHit) {
  Double_t charge = 0.;
  Int_t ich = recoHit->GetChannelID();
  Double_t thr[4];
  for (Int_t ithr = 0; ithr < 4; ithr++)
    thr[ithr] = fThresholdValues[4 * ich + ithr];
  recoHit->SetChargeModuleSection(0.);
  //Determine the number of thresholds on this reco hit
  //and fill the vectors with corresponding info.
  Int_t highestThreshold = recoHit->GetHighestThreshold();
  if (highestThreshold < 1) return;
  std::vector<Double_t> leadTime;
  std::vector<Double_t> trailTime;
  std::vector<Int_t> thresholdID;

  int kThresholdsWithSignal = 0;
  for (Int_t i = 0; i < highestThreshold; i++) {
    if (!recoHit->IsGrade(i))
      continue;
    leadTime.push_back(recoHit->GetLeadingEdge(i));
    trailTime.push_back(recoHit->GetTrailingEdge(i));
    thresholdID.push_back(i);
    kThresholdsWithSignal++;
  }
  if (kThresholdsWithSignal == 1) {
    //The area under the signal is approximated by a equilateral triangle
    //with ToT parallel to one of the sides at distance thr
    //This is done because there is not enough info for a better approximation
    Double_t tot = recoHit->GetToT(thresholdID[0]);
    Double_t area = TMath::Power(thr[thresholdID[0]] + 0.5 * TMath::Sqrt(3) * tot, 2) / TMath::Sqrt(3);
    charge = area / 50.;

  } else {
    charge = 0.;
    Double_t tStart;
    Double_t tStop = -999.;
    Double_t vIntersection = -999.;
    Double_t tIntersection = -999.;
    Double_t vStart = -999., vStop = -999.;
    //recursively compute the area under various regions of the signal.
    //for each two consecutive threshold, the signal is approximated with a triangle
    //The area is computed right away using Heron formula.
    for (Int_t ithr = 1; ithr < kThresholdsWithSignal; ithr++) {

      Double_t leadA = leadTime[ithr];
      Double_t vLeadA = thr[thresholdID[ithr]];

      Double_t leadB = tIntersection < -900. ? leadTime[ithr - 1] : tIntersection;
      Double_t vLeadB = vIntersection < -900. ? thr[thresholdID[ithr - 1]] : vIntersection;

      Double_t trailA = trailTime[ithr];
      Double_t vTrailA = thr[thresholdID[ithr]];

      Double_t trailB = trailTime[ithr - 1];
      Double_t vTrailB = thr[thresholdID[ithr - 1]];

      Double_t slopeLead = (vLeadA - vLeadB) / (leadA - leadB);
      Double_t slopeTrail = (vTrailA - vTrailB) / (trailA - trailB);

      tStart = tIntersection < -900. ? leadA - vLeadA / slopeLead : tIntersection;
      tStop = tStop < -900. ? trailB - vTrailB / slopeTrail : trailB;

      vStart = vStart < -900. ? 0. : vIntersection;
      vStop = vStop < -900. ? 0. : vTrailB;

      tIntersection = (vTrailA - vLeadA + slopeLead * leadA - slopeTrail * trailA) / (slopeLead - slopeTrail);
      vIntersection = vLeadA + slopeLead * (tIntersection - leadA);

      Double_t times[3] = {tStart, tIntersection, tStop};
      Double_t voltages[3] = {vStart, vIntersection, vStop};
      Double_t area = ComputeArea(times, voltages);
      charge += area / 50.;
    }
  }
  recoHit->SetChargeModuleSection(charge);
}

Double_t HACReconstruction::ComputeArea(Double_t* times, Double_t* voltages) {
  Double_t area = 0;
  Double_t side[3];
  for (Int_t i = 0; i < 3; i++) {
    side[i] = sqrt(pow(times[i % 3] - times[(i + 1) % 3], 2) + pow(voltages[i % 3] - voltages[(i + 1) % 3], 2));
  }
  Double_t semiPerim = 0.5 * (side[0] + side[1] + side[2]);
  area = sqrt(semiPerim * (semiPerim - side[0]) * (semiPerim - side[1]) * (semiPerim - side[2]));
  return area;
}
