// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoRICHHit.hh"

ClassImp(TRecoRICHHit)

TRecoRICHHit::TRecoRICHHit() :
  TRecoVHit(),
  fHitQuality(-99),
  fTimeWidth(0),
  fROChannelID(0),
  fPtolemy(-1),
  fIsOnCircle(kFALSE),
  fFitPosition(0.,0.,0.)
{
}

TRecoRICHHit::TRecoRICHHit(Int_t iCh) :
  TRecoVHit(iCh),
  fHitQuality(-99),
  fTimeWidth(0),
  fROChannelID(0),
  fPtolemy(-1),
  fIsOnCircle(kFALSE),
  fFitPosition(0.,0.,0.)
{
}

TRecoRICHHit::TRecoRICHHit(TRICHDigi * Digi) :
  TRecoVHit(Digi),
  fHitQuality(-99),
  fTimeWidth(Digi->GetTrailingEdge() - Digi->GetLeadingEdge()),
  fROChannelID(0),
  fPtolemy(-1),
  fIsOnCircle(kFALSE),
  fFitPosition(0.,0.,0.)
{
}

void TRecoRICHHit::Clear(Option_t* option){
  TRecoVHit::Clear(option);
  fHitQuality = -99;
  fTimeWidth = 0;
  fPtolemy = -1;
  fIsOnCircle = kFALSE;
  fFitPosition = TVector3(0.,0.,0.);
}

Int_t TRecoRICHHit::EncodeChannelID() {
  fChannelID=RICHChannelID::EncodeChannelID();
  return fChannelID;
}

void TRecoRICHHit::DecodeChannelID() {
  RICHChannelID::DecodeChannelID(fChannelID);
}

