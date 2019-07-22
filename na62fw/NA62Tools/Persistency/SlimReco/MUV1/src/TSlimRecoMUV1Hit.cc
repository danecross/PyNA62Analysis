#include <iostream>
#include "TSlimRecoMUV1Hit.hh"
#include "TRecoMUV1Hit.hh"
#include "TString.h"
#include "MUV1ChannelID.hh"

ClassImp(TSlimRecoMUV1Hit)

TSlimRecoMUV1Hit::TSlimRecoMUV1Hit (TRecoVHit *hit){
	this->FromReco(hit);
}

Int_t TSlimRecoMUV1Hit::GetQuadrant() const{

	Int_t Quadrant = 0;

	if ((fChannelID > 100 && fChannelID < 123) || (fChannelID>150 && fChannelID<173)) Quadrant = 1;
	else if ((fChannelID > 122 && fChannelID < 145) || (fChannelID>250 && fChannelID<273)) Quadrant = 2;
	else if ((fChannelID > 222 && fChannelID < 245) || (fChannelID>272 && fChannelID<295)) Quadrant = 3;
	else if ((fChannelID > 200 && fChannelID < 223) || (fChannelID>172 && fChannelID<195)) Quadrant = 4;

	return Quadrant;
}

Bool_t TSlimRecoMUV1Hit::IsLongScintillator() const{

	if 		(GetSide()%2==0 && (GetScintillatorNumber()<18 || GetScintillatorNumber()>27)) return true;
	else if (GetSide()%2==1 && (GetScintillatorNumber()<19 || GetScintillatorNumber()>26)) return true;

	return false;
}

Float_t TSlimRecoMUV1Hit::GetScintillatorPosition() const{

	Double_t position = -1278;
	Int_t fScintillatorNumber = GetScintillatorNumber();

	if (fScintillatorNumber<21) position += (fScintillatorNumber - 1)*60.;
	else if (fScintillatorNumber>24) position += (44 - fScintillatorNumber)*60.;
	else if (fScintillatorNumber==21 || fScintillatorNumber==24) position = -81;
	else position = -27;

	if (fScintillatorNumber>22) position *= -1;

	return position;

}

void TSlimRecoMUV1Hit::FromReco (TRecoVHit *hit){

	if (!TString(hit->ClassName()).EqualTo("TRecoMUV1Hit")){
		std::cerr <<"[TSlimRecoMUV1Hit.cc:21] Trying to import a Reco hit of class "<<hit->ClassName()<<" into a TSlimRecoMUV1Hit!"<<std::endl;
		return;
	}

	TRecoMUV1Hit *RecoHit = static_cast<TRecoMUV1Hit*>(hit);

	this->Clear();

	fChannelID = RecoHit->GetChannelID();
	this->SetPeakAmplitudeError(RecoHit->GetPeakAmplitude(), RecoHit->GetAmplitudeError());
	this->SetSigmaError(RecoHit->GetSigma(), RecoHit->GetSigmaError());
	this->SetTimeError(RecoHit->GetTime(), RecoHit->GetTimeError());
	this->SetChargeError(RecoHit->GetCharge(), RecoHit->GetChargeError());

}


void TSlimRecoMUV1Hit::ToReco (TRecoVHit *hit){

	if (!TString(hit->ClassName()).EqualTo("TRecoMUV1Hit")){
		std::cerr <<"[TSlimRecoMUV1Hit.cc:48] Trying to export into a Reco hit of class "<<hit->ClassName()<<" from a TSlimRecoMUV1Hit!"<<std::endl;
		return;
	}

	TRecoMUV1Hit *RecoHit = static_cast<TRecoMUV1Hit*>(hit);

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

Int_t TSlimRecoMUV1Hit::GetScintillatorNumber () const {
    return MUV1ChannelID::DecodeChannelID_Static(fChannelID).fScintillatorNumber;
}
Int_t TSlimRecoMUV1Hit::GetSide () const {
    return MUV1ChannelID::DecodeChannelID_Static(fChannelID).fSide;
}

TVector3 TSlimRecoMUV1Hit::GetPosition() const {
    return (GetSide()%2==0) ? TVector3(GetScintillatorPosition(),0,0) : TVector3(0,GetScintillatorPosition(),0);
}
