// ---------------------------------------------------------
// History:
//
// LKrMatching added by Michele Corvino (michele.corvino@cern.ch) 2019-03-20
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-03-05
//
// ---------------------------------------------------------

/// \class PhotonVetoHandler
/// \Brief
/// Wrapper for the LKr, LAV and SAV matching tools
/// \EndBrief
/// \Detailed
/// This analyser is a wrapper for LKr, LAV and SAV matching tools, allowing the exploitation of the
/// features of the analysers (e.g. handling of the run/burst dependence).
/// It returns in output the pointers to LKr/LAV/SAVMatching, initialised with the correct run-dependence.
/// The LKr/LAV/SAVMatching outputs can be used in the same way as when used in standalone mode:
/// \code
/// LAVMatching* pLAVMatching = *(LAVMatching**)GetOutput("PhotonVetoHandler.LAVMatching");
/// pLAVMatching->SetReferenceTime(ReferenceTime);
/// if (pLAVMatching->LAVHasTimeMatching(LAVEvent)) return;
/// pLAVMatching->PrintNoisyChannels();
/// \endcode
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

#include "PhotonVetoHandler.hh"
#include "NA62ConditionsService.hh"

using namespace NA62Analysis;
using namespace NA62Constants;

PhotonVetoHandler::PhotonVetoHandler(Core::BaseAnalysis *ba) : Analyzer(ba, "PhotonVetoHandler") {
  fLAVMatching = new LAVMatching();
  fSAVMatching = new SAVMatching();
  fLKrMatching = new LKrMatching();
  AddParam("LAVNoisyChannelsFileName", &fLAVNoisyChannelsFileName, "LAV-NoisyChannels.dat");
}

PhotonVetoHandler::~PhotonVetoHandler() {
  if (fLAVMatching) delete fLAVMatching;
  if (fSAVMatching) delete fSAVMatching;
  if (fLKrMatching) delete fLKrMatching;
}

void PhotonVetoHandler::InitOutput() {
  RegisterOutput("LAVMatching", &fLAVMatching);
  RegisterOutput("SAVMatching", &fSAVMatching);
  RegisterOutput("LKrMatching", &fLKrMatching);
  SetOutputState("LAVMatching", kOValid);
  SetOutputState("SAVMatching", kOValid);
  SetOutputState("LKrMatching", kOValid);
}

void PhotonVetoHandler::StartOfRunUser() {

  // Initialize SAV matching
  fSAVMatching->SetIsMC(GetWithMC());
  fSAVMatching->InitializeInputForSlewing(); // for data only

  // Initialize LAV noisy channels: data only
  fLAVMatching->ClearNoisyChannels();
  if (GetWithMC()) return;

  if (GetRunID()<=4173) { // special treatment of the 2015 data
    fLAVMatching->InitializeNoisyChannels2015();
  }
  // Read LAV noisy channels for all bursts of the current run from the CDB
  else if (NA62ConditionsService::GetInstance()->Open(fLAVNoisyChannelsFileName)==kSuccess) {
    TString Line;
    Int_t NLinesRead = 0;
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fLAVNoisyChannelsFileName))) {
      if (Line.BeginsWith("#")) continue;
      TObjArray *l = Line.Tokenize(" ");
      Int_t RunID   = ((TObjString*)(l->At(0)))->GetString().Atoi();
      Int_t BurstID = ((TObjString*)(l->At(1)))->GetString().Atoi();
      if (RunID==GetRunID()) { // current run number
        for(Int_t iNoisyCh=2;iNoisyCh<l->GetEntries();iNoisyCh++){
          Int_t NoisyChannel = ((TObjString*)(l->At(iNoisyCh)))->GetString().Atoi();
          if(NoisyChannel<0) continue;
          fLAVMatching->SetNoisyChannel(BurstID, NoisyChannel);
        }
        NLinesRead++;
      }
      delete l;
    }
    NA62ConditionsService::GetInstance()->Close(fLAVNoisyChannelsFileName);
    cout << "[PhotonVetoHandler] LAV noisy channel lists found for " <<
      NLinesRead << " bursts of run " << GetRunID() << endl;
  }

  
}

void PhotonVetoHandler::StartOfBurstUser() {
  if (GetWithMC()) return;
  fLAVMatching->SetBurstID(GetBurstID());
  fLKrMatching->SetCurrentRunAndBurstID(GetRunID(),GetBurstID());
  fLKrMatching->ClearJitters();
  // Initialize LKr Matching: jitters
  TString JitterFileName = "LKr-Jitters.dat";
  TString Line;
  if(NA62ConditionsService::GetInstance()->Open(JitterFileName)!=kSuccess) return;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(JitterFileName))) {
    if (Line.BeginsWith("#")) continue;
    // Format: RunID BurstID NCheckedCREAMS NJitters <CrateID-SlotID NEntries MinRatio MaxRatio Amin Amax TShift>_0...<CrateID-SlotID...>_NJitters-1
    TObjArray *Tokens = Line.Tokenize("[ -]");
    Int_t Run = ((TObjString*)Tokens->At(0))->GetString().Atoi();
    Int_t Burst = ((TObjString*)Tokens->At(1))->GetString().Atoi();
    Int_t NJitters = ((TObjString*)Tokens->At(3))->GetString().Atoi();
    for(int i=0; i<NJitters; i++){
      Int_t JitterCrate    = ((TObjString*)Tokens->At( 4+8*i))->GetString().Atoi();
      Int_t JitterSlot     = ((TObjString*)Tokens->At( 5+8*i))->GetString().Atoi();
      Double_t JitterTime  = ((TObjString*)Tokens->At(11+8*i))->GetString().Atof();
      Int_t TimeSlotsShift = round(JitterTime/ClockPeriod);
      fLKrMatching->SetJitter(Run,Burst,JitterCrate,JitterSlot,TimeSlotsShift);
    }
    delete Tokens;
  }
  NA62ConditionsService::GetInstance()->Close(JitterFileName);




}
