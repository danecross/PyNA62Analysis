#include "TSlimRecoSAVHit.hh"

#include "TRecoSAVHit.hh"
#include "SAVChannelID.hh"

ClassImp(TSlimRecoSAVHit)

TSlimRecoSAVHit::TSlimRecoSAVHit(TRecoSAVHit* hitReco) {
    FromReco(hitReco);
}

TVector2 TSlimRecoSAVHit::GetChannelPosition() const{
  Double_t coordinate[2] = {50.,70.};

  Int_t detectorChannel = GetChannelDetector();
  Int_t detectorID = GetDetector();

  if (detectorChannel == 1) return  TVector2(-coordinate[detectorID], coordinate[detectorID]);
  if (detectorChannel == 2) return  TVector2( coordinate[detectorID], coordinate[detectorID]);
  if (detectorChannel == 3) return  TVector2( coordinate[detectorID],-coordinate[detectorID]);
  if (detectorChannel == 4) return  TVector2(-coordinate[detectorID],-coordinate[detectorID]);

  return TVector2(0.,0.);
}

TVector3 TSlimRecoSAVHit::GetPosition() const {
    TVector2 channelPos = GetChannelPosition();
    return TVector3(channelPos.X(), channelPos.Y(), 0.);
}

void TSlimRecoSAVHit::FromReco(TRecoVHit* hitVReco) {
    TRecoSAVHit *hitReco = static_cast<TRecoSAVHit*>(hitVReco);

    fChannelID = hitReco->GetChannelID();
    fTime      = hitReco->GetTime();
    fAmplitude = hitReco->GetAmplitude();
    fBaseline  = hitReco->GetBaseline();
    fEnergy    = hitReco->GetEnergy();
}

void TSlimRecoSAVHit::ToReco(TRecoVHit* hitVReco) {
    TRecoSAVHit *hitReco = static_cast<TRecoSAVHit*>(hitVReco);

    hitReco->SetChannelID(fChannelID);
    hitReco->SetTime(fTime);
    hitReco->SetAmplitude(fAmplitude);
    hitReco->SetBaseline(fBaseline);
    hitReco->SetEnergy(fEnergy);
    hitReco->SetPosition(GetPosition());
    hitReco->DecodeChannelID(fChannelID);
}

Int_t TSlimRecoSAVHit::GetChannelDetector() const {
    return SAVChannelID::DecodeChannelID_Static(fChannelID).fDetectorChannel;
}

Int_t TSlimRecoSAVHit::GetDetector() const {
    return SAVChannelID::DecodeChannelID_Static(fChannelID).fDetectorID;
}

