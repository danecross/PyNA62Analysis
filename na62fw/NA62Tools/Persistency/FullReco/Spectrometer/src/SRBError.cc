#include "SRBError.hh"
ClassImp(SRBError)

SRBError::SRBError():TDigiVError(){
  fStrawAddr = -1;
  fFlag = 0;
}

void SRBError::Clear(Option_t * option){
  TDigiVError::Clear(option);
  fStrawAddr = -1;
  fFlag = 0;
}
