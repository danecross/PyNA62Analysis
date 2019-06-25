// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2007-03-30
//
// --------------------------------------------------------------
#include "TDCVHit.hh"
ClassImp(TDCVHit)

TDCVHit::TDCVHit() :
    TVDigi(),
    fLeadingEdge(-1.e28),
    fTrailingEdge(-1.e28),
    fDetectedEdge(0),
    fFPGAID(0),
    fSlot(0),
    fSlotTS(0)
{
}

TDCVHit::TDCVHit(Int_t iCh) :
    TVDigi(iCh),
    fLeadingEdge(-1.e28),
    fTrailingEdge(-1.e28),
    fDetectedEdge(0),
    fFPGAID(0),
    fSlot(0),
    fSlotTS(0)
{
}

TDCVHit::TDCVHit(TVHit* MCHit) :
    TVDigi(MCHit),
    fLeadingEdge(-1.e28),
    fTrailingEdge(-1.e28),
    fDetectedEdge(0),
    fFPGAID(0),
    fSlot(0),
    fSlotTS(0)
{
}

Int_t TDCVHit::Compare(const TObject *obj) const {
  if (fLeadingEdge > static_cast<const TDCVHit*>(obj)->GetLeadingEdge()) return +1;
  if (fLeadingEdge < static_cast<const TDCVHit*>(obj)->GetLeadingEdge()) return -1;
  return 0;
}

void TDCVHit::Clear(Option_t * option) {
  TVDigi::Clear(option);
  fDetectedEdge = 0;
  fLeadingEdge = -1.e28;
  fTrailingEdge = -1.e28;
  fFPGAID = 0;
  fSlot = 0;
  fSlotTS = 0;
}
