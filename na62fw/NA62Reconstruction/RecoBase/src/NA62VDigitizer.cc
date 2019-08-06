#include "NA62VDigitizer.hh"

NA62VDigitizer::NA62VDigitizer(NA62VReconstruction* Reco, TString Name) : NA62VNamedModule(Name), fReco(Reco), fRandom(nullptr), fDigiEvent(nullptr), fStationsMCToF(nullptr), fT0(nullptr) {
  if(fReco){
    fNChannels = fReco->GetNChannels();
    fChannels  = fReco->GetChannels();
  }
  fRandom = new TRandom3();
}

NA62VDigitizer::~NA62VDigitizer(){
  std::cout << "Deleting " << fName << " Digitizer.." << std::endl;
  if(fDigiEvent) {
    delete fDigiEvent;
    fDigiEvent = nullptr;
  }
  if(fRandom) {
    delete fRandom;
    fRandom = nullptr;
  }
}

void NA62VDigitizer::StartOfBurst() {
}

void NA62VDigitizer::EndOfBurst() {
}
