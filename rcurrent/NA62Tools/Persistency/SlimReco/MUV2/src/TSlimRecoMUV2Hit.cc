#include <iostream>
#include "TSlimRecoMUV2Hit.hh"
#include "TRecoMUV2Hit.hh"
#include "TString.h"
#include "MUV2ChannelID.hh"

ClassImp(TSlimRecoMUV2Hit)

TSlimRecoMUV2Hit::TSlimRecoMUV2Hit (TRecoVHit *hit){
	this->FromReco(hit);
}

Int_t TSlimRecoMUV2Hit::GetQuadrant() const{

	Int_t Quadrant = 0;

	if      ((fChannelID > 100 && fChannelID < 112) || (fChannelID>150 && fChannelID<162)) Quadrant = 1;
	else if ((fChannelID > 111 && fChannelID < 123) || (fChannelID>250 && fChannelID<262)) Quadrant = 2;
	else if ((fChannelID > 211 && fChannelID < 223) || (fChannelID>261 && fChannelID<273)) Quadrant = 3;
	else if ((fChannelID > 200 && fChannelID < 212) || (fChannelID>161 && fChannelID<173)) Quadrant = 4;
	return Quadrant;
}


Float_t TSlimRecoMUV2Hit::GetScintillatorPosition() const{

	Int_t fScintillatorNumber = GetScintillatorNumber();
	Double_t position = -1238.5;

	if (fScintillatorNumber<11) position += (fScintillatorNumber-1) * 119;
	else if (fScintillatorNumber>12) position += (22-fScintillatorNumber) * 119;
	else position = -54.;

	if (fScintillatorNumber>11) position *= -1;

	return position;

}

void TSlimRecoMUV2Hit::FromReco (TRecoVHit *hit){

	if (!TString(hit->ClassName()).EqualTo("TRecoMUV2Hit")){
		std::cerr <<"[TSlimRecoMUV2Hit.cc:21] Trying to import a Reco hit of class "<<hit->ClassName()<<" into a TSlimRecoMUV2Hit!"<<std::endl;
		return;
	}

	TRecoMUV2Hit *RecoHit = static_cast<TRecoMUV2Hit*>(hit);

	this->Clear();

	fChannelID = RecoHit->GetChannelID();
	this->SetPeakAmplitudeError(RecoHit->GetPeakAmplitude(), RecoHit->GetAmplitudeError());
	this->SetSigmaError(RecoHit->GetSigma(), RecoHit->GetSigmaError());
	this->SetTimeError(RecoHit->GetTime(), RecoHit->GetTimeError());
	this->SetChargeError(RecoHit->GetCharge(), RecoHit->GetChargeError());
}


void TSlimRecoMUV2Hit::ToReco (TRecoVHit *hit){

	if (!TString(hit->ClassName()).EqualTo("TRecoMUV2Hit")){
		std::cerr <<"[TSlimRecoMUV2Hit.cc:48] Trying to export into a Reco hit of class "<<hit->ClassName()<<" from a TSlimRecoMUV2Hit!"<<std::endl;
		return;
	}

	TRecoMUV2Hit *RecoHit = static_cast<TRecoMUV2Hit*>(hit);

	RecoHit->Clear();

	RecoHit->SetChannelID(fChannelID);
	RecoHit->DecodeChannelID(fChannelID);

	RecoHit->SetPeakAmplitude(fPeakAmplitude);
	RecoHit->SetCharge(fCharge);
	RecoHit->SetSigma(fSigma);
	RecoHit->SetTime(fTime);

	RecoHit->SetAmplitudeError(GetPeakAmplitudeError());
	RecoHit->SetSigmaError(GetSigmaError());
	RecoHit->SetChargeError(GetChargeError());
	RecoHit->SetTimeError(GetTimeError());
	RecoHit->SetEnergy(GetEnergy());

	RecoHit->SetPosition(GetPosition());

}

Int_t TSlimRecoMUV2Hit::GetScintillatorNumber () const {
    return MUV2ChannelID::DecodeChannelID_Static(fChannelID).fScintillatorNumber;
}
Int_t TSlimRecoMUV2Hit::GetSide () const {
    return MUV2ChannelID::DecodeChannelID_Static(fChannelID).fSide;
}

TVector3 TSlimRecoMUV2Hit::GetPosition() const {
    return (GetSide()%2==0) ? TVector3(GetScintillatorPosition(),0,0) : TVector3(0,GetScintillatorPosition(),0);
}
