#include "SRBVHit.hh"
ClassImp(SRBVHit)

SRBVHit::SRBVHit() :
  TDCVHit(),
  fSRBAddr(0),
  fStrawAddr(0),
  fMultiHit(false)
{

}

SRBVHit::SRBVHit(Int_t iCh) :
  TDCVHit(iCh),
  fSRBAddr(999999),
  fStrawAddr(9999999),
  fMultiHit(false)
{
}

SRBVHit::SRBVHit(TVHit* MCHit) :
  TDCVHit(MCHit),
  fSRBAddr(999999),
  fStrawAddr(9999999),
  fMultiHit(false)
{
}

void SRBVHit::Clear(Option_t* option){
 TDCVHit::Clear(option);
}
