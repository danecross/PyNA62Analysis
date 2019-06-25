#include "SAVRawEncoder.hh"
#include "SAVReconstruction.hh"
#include "TDCBRawEncoder.hh"

SAVRawEncoder::SAVRawEncoder(NA62VReconstruction* Reco) : NA62VRawEncoder(Reco, "SAV") 
{
  //fLibrary.push_back(new TDCBRawEncoder(Reco));
  fEncoder = static_cast<NA62VRawEncoder*>(Find("TDCB"));
  if(fEncoder) {
    fBinaryEvent = fEncoder->GetBinaryEvent();
    fSpecialTriggerEvent = fEncoder->GetSpecialTriggerEvent();
  }
}

SAVRawEncoder::~SAVRawEncoder() {}

BinaryEvent* SAVRawEncoder::EncodeNextEvent(TDetectorVEvent *tEvent, Bool_t fIsRawData){
  if (fEncoder) return fEncoder->EncodeNextEvent(tEvent,fIsRawData);
  else return 0;    
}

void SAVRawEncoder::Init(){
  if(fEncoder) fEncoder->Init();
}

void SAVRawEncoder::End(){
  if(fEncoder) fEncoder->End();
}
