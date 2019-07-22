#include <iostream>
#include "TSlimRecoMUV2Candidate.hh"
#include "TSlimRecoMUVCandidate.hh"
#include "TRecoMUV2Candidate.hh"

ClassImp(TSlimRecoMUV2Candidate)

TSlimRecoMUV2Candidate::TSlimRecoMUV2Candidate (TRecoVCandidate *cand){
	this->FromReco(cand);
}

void TSlimRecoMUV2Candidate::FromReco(TRecoVCandidate *cand){

	if (!TString(cand->ClassName()).EqualTo("TRecoMUV2Candidate")){
		std::cerr <<"[TSlimRecoMUV2Candidate.cc:9] Trying to import a Reco candidate of class "<<cand->ClassName()<<" into a TSlimRecoMUV2Candidate!"<<std::endl;
		return;
	}

	TRecoMUV2Candidate *RecoCand = static_cast<TRecoMUV2Candidate*>(cand);
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

void TSlimRecoMUV2Candidate::ToReco(TRecoVCandidate *cand){

	if (!TString(cand->ClassName()).EqualTo("TRecoMUV2Candidate")){
		std::cerr <<"[TSlimRecoMUV2Candidate.cc:52] Trying to export to a Reco candidate of class "<<cand->ClassName()<<" fram a TSlimRecoMUV2Candidate!"<<std::endl;
		return;
	}

	TRecoMUV2Candidate *RecoCand = static_cast<TRecoMUV2Candidate*>(cand);
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
