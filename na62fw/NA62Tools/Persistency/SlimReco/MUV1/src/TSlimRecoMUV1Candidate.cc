#include <iostream>
#include "TSlimRecoMUV1Candidate.hh"
#include "TSlimRecoMUVCandidate.hh"
#include "TRecoMUV1Candidate.hh"

ClassImp(TSlimRecoMUV1Candidate)

TSlimRecoMUV1Candidate::TSlimRecoMUV1Candidate (TRecoVCandidate *cand){
	this->FromReco(cand);
}

void TSlimRecoMUV1Candidate::FromReco(TRecoVCandidate *cand){

	if (!TString(cand->ClassName()).EqualTo("TRecoMUV1Candidate")){
		std::cerr <<"[TSlimRecoMUV1Candidate.cc:13] Trying to import a Reco candidate of class "<<cand->ClassName()<<" into a TSlimRecoMUV1Candidate!"<<std::endl;
		return;
	}

	TRecoMUV1Candidate *RecoCand = static_cast<TRecoMUV1Candidate*>(cand);
	this->Clear();

	this->SetChargeH(RecoCand->GetChargeHorizontal());
	this->SetChargeV(RecoCand->GetChargeVertical());

	this->SetEnergy(RecoCand->GetEnergy());
	this->SetEnergyH(RecoCand->GetEnergyHorizontal());
	this->SetEnergyV(RecoCand->GetEnergyVertical());

	this->SetIndexH(RecoCand->GetHorizontalChannel(),RecoCand->GetHorizontalIndex());
	this->SetIndexV(RecoCand->GetVerticalChannel(),RecoCand->GetVerticalIndex());

	this->SetInnerEnergyH(RecoCand->GetInnerEnergyHorizontal());
	this->SetInnerEnergyV(RecoCand->GetInnerEnergyVertical());

	this->SetSeedEnergyH (RecoCand->GetSeedEnergyHorizontal());
	this->SetSeedEnergyV (RecoCand->GetSeedEnergyVertical());

	this->SetSeedIndexes(RecoCand->GetSeedIndexHorizontal(),RecoCand->GetSeedIndexVertical());

	this->SetShowerWidthH(RecoCand->GetShowerWidthHorizontal());
	this->SetShowerWidthV(RecoCand->GetShowerWidthVertical());

	this->SetTimeH(RecoCand->GetTimeHorizontal());
	this->SetTimeV(RecoCand->GetTimeVertical());

	this->SetTimeSigmaH(RecoCand->GetTimeSigmaHorizontal());
	this->SetTimeSigmaV(RecoCand->GetTimeSigmaVertical());

	this->SetPosition(RecoCand->GetPosition());

	this->SetHitsIndexes(RecoCand->GetNHits(),RecoCand->GetHitsIndexes());

}

void TSlimRecoMUV1Candidate::ToReco(TRecoVCandidate *cand){

	if (!TString(cand->ClassName()).EqualTo("TRecoMUV1Candidate")){
		std::cerr <<"[TSlimRecoMUV1Candidate.cc:56] Trying to export to a Reco candidate of class "<<cand->ClassName()<<" fram a TSlimRecoMUV1Candidate!"<<std::endl;
		return;
	}

	TRecoMUV1Candidate *RecoCand = static_cast<TRecoMUV1Candidate*>(cand);
	RecoCand->Clear();

	RecoCand->SetCharge(this->GetCharge());
	RecoCand->SetChargeHorizontal(this->GetChargeH());
	RecoCand->SetChargeVertical(this->GetChargeV());

	RecoCand->SetEnergy(this->GetEnergy());
	RecoCand->SetEnergyHorizontal(this->GetEnergyH());
	RecoCand->SetEnergyVertical(this->GetEnergyV());

	RecoCand->SetHorizontalIndex(this->GetHorizontalIndex());
	RecoCand->SetHorizontalChannel(this->GetHorizontalChannel());

	RecoCand->SetVerticalIndex(this->GetVerticalIndex());
	RecoCand->SetVerticalChannel(this->GetVerticalChannel());

	RecoCand->SetQuadrant(this->GetQuadrant());

	RecoCand->SetInnerEnergyHorizontal(this->GetInnerEnergyH());
	RecoCand->SetInnerEnergyVertical(this->GetInnerEnergyV());

	RecoCand->SetSeedEnergyHorizontal (this->GetSeedEnergyH());
	RecoCand->SetSeedEnergyVertical (this->GetSeedEnergyV());

	RecoCand->SetSeedIndexHorizontal(this->GetSeedIndexH());
	RecoCand->SetSeedIndexVertical(this->GetSeedIndexV());

	RecoCand->SetShowerWidth(this->GetShowerWidth());
	RecoCand->SetShowerWidthHorizontal(this->GetShowerWidthH());
	RecoCand->SetShowerWidthVertical(this->GetShowerWidthV());

	RecoCand->SetTime(this->GetTime());
	RecoCand->SetTimeHorizontal(this->GetTimeH());
	RecoCand->SetTimeVertical(this->GetTimeV());

	RecoCand->SetTimeSigmaHorizontal(this->GetTimeSigmaHorizontal());
	RecoCand->SetTimeSigmaVertical(this->GetTimeSigmaVertical());

	RecoCand->SetPosition(this->GetPosition());

	for (const auto h : fHitsIndexes) RecoCand->AddHit(h);

}
