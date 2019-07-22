// --------------------------------------------------------------
// 
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// 2015-01-22 Totally modified and revised by T. Spadaro and E. Leonardi
// Created by Vito Palladino 2011-18-03
//
// --------------------------------------------------------------

#include "TLAVDigi.hh"

#include "TMath.h"
#include "iostream"

using namespace std;

ClassImp(TLAVDigi)

Int_t TLAVDigi::EncodeChannelID() {
  return LAVChannelID::EncodeChannelID();
}

void TLAVDigi::DecodeChannelID() {
  Int_t geoChannel = fChannelID%1000000;
  LAVChannelID::DecodeChannelID(geoChannel); // the geographic decoding can be applied
}

Int_t TLAVDigi::GetElectronicChannelID(){

  Int_t iSt = LAVChannelID::GetLAVID();

// Obviously here, the missing geometry in the persistency is forcing us to do so. Will do better in future.

  Int_t numberOfBananas;
  if (iSt < 6) numberOfBananas = 8;
  else if (iSt < 9) numberOfBananas = 12;
  else if (iSt < 12) numberOfBananas = 15;
  else numberOfBananas = 16;
  
  Int_t ilay = LAVChannelID::GetLayerID();
  Int_t iban = LAVChannelID::GetBananaID(); 
  Int_t ich = LAVChannelID::GetBlockID(); 
  Int_t threshold = fChannelID/1000000;
  return 1000*iSt + (ich + 4*iban + numberOfBananas*4*ilay)*2+threshold;  
}

void TLAVDigi::SetEdges(Int_t DetectedEdge, Double_t LeadingEdge, Double_t TrailingEdge){
  if (DetectedEdge == 3) {
    if (TrailingEdge < LeadingEdge) {
      cerr << "TLAVDigi >> Error from input info: a) " << DetectedEdge  << " " <<LeadingEdge << " " << TrailingEdge << " " << EncodeChannelID() << endl;
      return;
    }
    SetDetectedEdge(DetectedEdge);
    SetLeadingEdge(LeadingEdge);
    SetTrailingEdge(TrailingEdge);
  }
  else if (DetectedEdge == 1) {
    SetDetectedEdge(DetectedEdge);
    SetLeadingEdge(LeadingEdge);
  }
  else if (DetectedEdge == 2) {
    SetDetectedEdge(DetectedEdge);
    SetTrailingEdge(TrailingEdge);
  }
  else {
    cerr << "TLAVDigi >> Error from input info: b) " << DetectedEdge  << " " <<LeadingEdge << " " << TrailingEdge << endl;
    return;
  }
}

void TLAVDigi::Clear(Option_t* option){
  TDCVHit::Clear(option);
  LAVChannelID::Clear(option);
}
