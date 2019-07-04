#include "TSlimRecoMUVHit.hh"

ClassImp(TSlimRecoMUVHit)

void TSlimRecoMUVHit::Clear (Option_t * option){
    ClearBase(option);
}

void TSlimRecoMUVHit::ClearBase (Option_t *){

	fChannelID = fTimeError= fPeakAmplitudeError = fSigmaError = 0;
	fTime = fPeakAmplitude = fSigma = 0.;

}
