#include "TSlimRecoMUVCandidate.hh"

ClassImp(TSlimRecoMUVCandidate)

void TSlimRecoMUVCandidate::Clear(Option_t *option){
    ClearBase(option);
}

void TSlimRecoMUVCandidate::ClearBase(Option_t *){

	fIndexH = fIndexV = 0;
	fSeedIndexes = 0;

	fTimeH = fTimeV = 0.;
	fEnergy = fEnergyH = fEnergyV = 0.;
	fInnerEnergyH = fInnerEnergyV =  0.;
	fSeedEnergyH = fSeedEnergyV = 0.;
	fChargeH = fChargeV = 0.;
	fShowerWidthH = fShowerWidthV = 0.;
	fTimeSigmaH = fTimeSigmaV = 0.;
	fPosition[0] = fPosition[1] = 0.;

	fHitsIndexes.clear();
}

Int_t TSlimRecoMUVCandidate::GetQuadrant() const{

	Int_t qindex = GetHorizontalIndex()*10+GetVerticalIndex();
	Int_t quadrant = 0;

	switch (qindex) {
		case 1:
			quadrant=1;
			break;
		case 3:
			quadrant=2;
			break;
		case 23:
			quadrant=3;
			break;
		case 21:
			quadrant=4;
			break;
		default:
			break;
	}

	return quadrant;
}
