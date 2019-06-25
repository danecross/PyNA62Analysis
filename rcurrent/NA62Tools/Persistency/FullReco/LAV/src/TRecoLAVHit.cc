// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - adjusting static geometry methods to "as build geometry"
// 2015-01-22 Totally modified and revised by T. Spadaro and E. Leonardi
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoLAVHit.hh"
#include "iostream"

ClassImp(TRecoLAVHit)

TRecoLAVHit::TRecoLAVHit() : TRecoVHit(){
  fEdgeMask = 0; 
  fLeadingEdgeLow = 0; 
  fTrailingEdgeLow = 0;
  fLeadingEdgeHigh = 0;
  fTrailingEdgeHigh = 0;
}

TRecoLAVHit::~TRecoLAVHit(){ }

void TRecoLAVHit::Clear(Option_t* option){
  TRecoVHit::Clear(option);
  fEdgeMask = 0;
}

Int_t TRecoLAVHit::EncodeChannelID(){
  fChannelID = LAVChannelID::EncodeChannelID();
  return fChannelID;
}

void TRecoLAVHit::DecodeChannelID(){
  LAVChannelID::DecodeChannelID(fChannelID);
}


void TRecoLAVHit::GetBlockPosition(TVector3 &position){
  GetBlockPositionFromCh(fChannelID,position);
}

void TRecoLAVHit::GetBlockPositionFromCh(Int_t channelID, TVector3 &position){
/// \MemberDescr
/// Fill TVector3 input with the nominal block position at its center, evaluated from hardcoded geometry.
/// \EndMemberDescr
  Int_t b  = LAVChannelID::GetBlockIDFromCh(channelID);
  Int_t BB = LAVChannelID::GetBananaIDFromCh(channelID);
  Int_t L  = LAVChannelID::GetLayerIDFromCh(channelID);
  Int_t SS = LAVChannelID::GetLAVIDFromCh(channelID)-1; // geometry wants station numbering from 0

  Double_t LAV_Station_ZofFrontFace[12];
  LAV_Station_ZofFrontFace[0]  = 120.595; // m
  LAV_Station_ZofFrontFace[1]  = 128.205; // m
  LAV_Station_ZofFrontFace[2]  = 135.815; // m
  LAV_Station_ZofFrontFace[3]  = 143.425; // m
  LAV_Station_ZofFrontFace[4]  = 151.035; // m
  LAV_Station_ZofFrontFace[5]  = 164.545; // m
  LAV_Station_ZofFrontFace[6]  = 172.055; // m
  LAV_Station_ZofFrontFace[7]  = 179.565; // m
  LAV_Station_ZofFrontFace[8]  = 191.941; // m
  LAV_Station_ZofFrontFace[9]  = 202.334; // m
  LAV_Station_ZofFrontFace[10] = 216.760; // m
  LAV_Station_ZofFrontFace[11] = 238.150; // m


  Int_t LAV_Station_NBananasPerRing;
  Double_t LAV_StationPhiReference;
  Double_t LAV_Station_PhiRotationBetweenLayers;
  Double_t LAV_Station_InnerRadius;
  Double_t LAV_Station_FirstRingZPos;
 
  if (SS<5) {
    LAV_Station_NBananasPerRing = 8;
    LAV_StationPhiReference = 98.86; // in MC before Rev of March 19 2015, it was 99
    LAV_Station_PhiRotationBetweenLayers = -2.25;
    LAV_Station_InnerRadius = 536.5; // in MC before Rev of March 19 2015, it was 532 mm
    LAV_Station_FirstRingZPos = 768; // mm
  }
  else if (SS < 8) {
    LAV_Station_NBananasPerRing = 12;
    LAV_StationPhiReference = 92.28; // in MC before Rev of March 19 2015, it was 99
    LAV_Station_PhiRotationBetweenLayers = -1.50;
    LAV_Station_InnerRadius = 767.5; // in MC before Rev of March 19 2015, it was 772 mm
    LAV_Station_FirstRingZPos = 768; // mm
  }
  else if (SS < 11) {
    LAV_Station_NBananasPerRing = 15;
    LAV_StationPhiReference = 92.25; // in MC before Rev of March 19 2015, it was 99 
    if (SS==9) LAV_StationPhiReference = 104.25;
    LAV_Station_PhiRotationBetweenLayers = -1.50;
    LAV_Station_InnerRadius = 980.; // in MC before Rev of March 19 2015, it was 972 mm
    LAV_Station_FirstRingZPos = 768; // mm
  }
  else {
    LAV_Station_NBananasPerRing = 16;
    LAV_StationPhiReference = 91.125; // ? 96.1875; // in MC before Rev of March 19 2015, it  was 99
    LAV_Station_PhiRotationBetweenLayers = 1.5; //-1.40625;
    LAV_Station_InnerRadius = 1070.; // in MC before Rev of March 19 2015, it was 1072 mm
    LAV_Station_FirstRingZPos = 115; // in MC before Rev of January 22 2015 (rev 407 and older versions), it was 230 mm
  }

  Double_t BananaPhiSpan = 360./((Double_t) LAV_Station_NBananasPerRing);
  Double_t BlockPhiSpan = ((Double_t) BananaPhiSpan)/4.;
  Double_t Phi = 0
    // VVV phibanana VVV
    + LAV_StationPhiReference
    + L * LAV_Station_PhiRotationBetweenLayers
    + (BB-0.5)*BananaPhiSpan
      // VVV phiblock VVV
    + (b+0.5) * BlockPhiSpan;
  if (Phi >= 360) Phi -= 360;
  if (Phi < 0) Phi += 360.;

  Double_t zBlockWidth = 110.; // mm
  Double_t zLayerSpacing = 120.; // mm
  Double_t z = LAV_Station_ZofFrontFace[SS]*1000. + 
    LAV_Station_FirstRingZPos +
    zBlockWidth*0.5 + 
    L*zLayerSpacing;

  Double_t BlockZLength = 370; // mm
  Double_t r = LAV_Station_InnerRadius + 
    BlockZLength*0.5;
  
  position.SetXYZ(r*TMath::Cos(Phi*TMath::Pi()/180.),r*TMath::Sin(Phi*TMath::Pi()/180.),z);  
  return;
}


Double_t TRecoLAVHit::GetBlockPhiSpan(Int_t station){ // station in [1,12]
/// \MemberDescr
/// Retrieve the block phi span, from input station[1,12]
/// \param[In] station (LAV id, between 1 and 12)
/// \EndMemberDescr

  Int_t SS = station-1; // geometry wants station numbering from 0
  Int_t LAV_Station_NBananasPerRing;
  if (SS<5) {
    LAV_Station_NBananasPerRing = 8;
  }
  else if (SS < 8) {
    LAV_Station_NBananasPerRing = 12;
  }
  else if (SS < 11) {
    LAV_Station_NBananasPerRing = 15;
  }
  else {
    LAV_Station_NBananasPerRing = 16;
  }

  return 2.*TMath::Pi()/((Double_t) LAV_Station_NBananasPerRing*4);
}


Int_t TRecoLAVHit::GetBlockIDFromPhi(Double_t phi, Int_t station, Int_t L){ // phi in [-pi,pi); station in [1,12], layer in [0,4/5]
/// \MemberDescr
/// Retrieve the block number, from input azimuthal angle[-pi,pi), station[1,12], and layer [0,4/5]
/// \param[In] phi azimuthal angle (rad, between pi and pi)
/// \param[In] station (LAV id, between 1 and 12)
/// \param[In] L (layer id, between 0 and 4/5 according to the LAV id)
/// \EndMemberDescr

  Int_t SS = station-1; // geometry wants station numbering from 0

  Int_t LAV_Station_NBananasPerRing;
  Double_t LAV_StationPhiReference;
  Double_t LAV_Station_PhiRotationBetweenLayers;
 
  if (SS<5) {
    LAV_Station_NBananasPerRing = 8;
    LAV_StationPhiReference = 98.86; // in MC before Rev of March 19 2015, it was 99
    LAV_Station_PhiRotationBetweenLayers = -2.25;
  }
  else if (SS < 8) {
    LAV_Station_NBananasPerRing = 12;
    LAV_StationPhiReference = 92.28; // in MC before Rev of March 19 2015, it was 99
    LAV_Station_PhiRotationBetweenLayers = -1.50;
  }
  else if (SS < 11) {
    LAV_Station_NBananasPerRing = 15;
    LAV_StationPhiReference = 92.25; // in MC before Rev of March 19 2015, it was 99 
    if (SS==9) LAV_StationPhiReference = 104.25; // in MC before Rev of March 19 2015, it was 99 
    LAV_Station_PhiRotationBetweenLayers = -1.50;
  }
  else {
    LAV_Station_NBananasPerRing = 16;
    LAV_StationPhiReference = 91.125; // ? 98.4375; // ? 96.1875 was wrong ; // in MC before Rev of March 19 2015 was 99
    LAV_Station_PhiRotationBetweenLayers = 1.5; //-1.40625;
  }

  Double_t BananaPhiSpan = 360./((Double_t) LAV_Station_NBananasPerRing);
  Double_t BlockPhiSpan = ((Double_t) BananaPhiSpan)/4.;
  Double_t phiDeg = phi*180./TMath::Pi();
  if (phiDeg < 0) phiDeg += 360.; // between 0 and 360
  Double_t dPhi =  phiDeg -
    ( LAV_StationPhiReference
      + L * LAV_Station_PhiRotationBetweenLayers -0.5*BananaPhiSpan); // subtract phi of left edge of banana
  if (dPhi >= 360) dPhi -= 360;
  if (dPhi < 0) dPhi += 360.;
  Int_t BB = dPhi/BananaPhiSpan;

  Int_t b = (dPhi - BB*BananaPhiSpan)/BlockPhiSpan;

  Int_t channelID = (SS+1)*10000 + L*1000 + BB*10 + b;
  return channelID;
}
