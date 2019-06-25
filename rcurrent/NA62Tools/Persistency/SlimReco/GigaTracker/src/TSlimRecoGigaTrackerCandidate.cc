#include "TSlimRecoGigaTrackerCandidate.hh"
#include "TRecoGigaTrackerCandidate.hh"
#include "GigaTrackerChannelID.hh"

ClassImp(TSlimRecoGigaTrackerCandidate)

TSlimRecoGigaTrackerCandidate::TSlimRecoGigaTrackerCandidate(TRecoGigaTrackerCandidate* candReco)
{
    FromReco(candReco);
}

Int_t TSlimRecoGigaTrackerCandidate::GetArrayIndexCovarianceMatrix(Int_t i, Int_t j)
{
    // index conversion: symmetric 2D matrix of size N=5 -> 1D array
    Int_t index = -1;
    if (i <= j) {
        index = 5*i - (i-1)*i/2 + (j - i);
    } else {
        index = 5*j - (j-1)*j/2 + (i - j);
    }
    return index;
}

void TSlimRecoGigaTrackerCandidate::SetCovariance(Int_t i, Int_t j, Double_t value)
{
    Int_t index = TSlimRecoGigaTrackerCandidate::GetArrayIndexCovarianceMatrix(i, j);
    if (index >= 0 && index < TSlimRecoGigaTrackerCandidate::kCovarianceDimension) {
        fCovariance[index] = value;
    }
}

Double_t TSlimRecoGigaTrackerCandidate::GetCovariance(int i, int j) const
{
    Int_t index = TSlimRecoGigaTrackerCandidate::GetArrayIndexCovarianceMatrix(i, j);
    if (index >= 0 && index < TSlimRecoGigaTrackerCandidate::kCovarianceDimension) {
        return fCovariance[index];
    } else {
        return 0;
    }
}

void TSlimRecoGigaTrackerCandidate::FromReco(TRecoVCandidate* candVReco)
{
    TRecoGigaTrackerCandidate *candReco = static_cast<TRecoGigaTrackerCandidate*>(candVReco);
    fTime = candReco->GetTime();
    fTimeError = candReco->GetTimeError();
    fChi2 = candReco->GetChi2();
    fChi2X = candReco->GetChi2X();
    fChi2Y = candReco->GetChi2Y();
    fChi2Time = candReco->GetChi2Time();
    for (Int_t iS(0); iS<3; ++iS) {
      fTimeStation[iS] = candReco->GetTimeStation(iS);
      this->SetPosition(iS, candReco->GetPosition(iS));
    }
    this->SetMomentum(candReco->GetMomentum());

    for (int i = 0; i < 5; i++)
        for (int j = 0; j < 5; j++)
            this->SetCovariance(i, j, candReco->GetCovariance(i, j));

    fHitsIndexes.clear();
    for (int ihit = 0; ihit < candReco->GetNHits(); ihit++)
        this->AddHitIndex(candReco->GetHitsIndexes()[ihit]);
}

void TSlimRecoGigaTrackerCandidate::ToReco(TRecoVCandidate* candVReco)
{
    TRecoGigaTrackerCandidate *candReco = static_cast<TRecoGigaTrackerCandidate*>(candVReco);
    candReco->SetMomentum(this->GetMomentum());
    candReco->SetTime(fTime);
    candReco->SetTimeError(fTimeError);
    for (Int_t iS(0); iS<3; ++iS) {
      candReco->SetTimeStation(iS, fTimeStation[iS]);
      candReco->SetPosition(iS, this->GetPosition(iS));
    }
    candReco->SetChi2(fChi2);
    candReco->SetChi2X(fChi2X);
    candReco->SetChi2Y(fChi2Y);
    candReco->SetChi2Time(fChi2Time);
    candReco->SetType(123);

    for (int i = 0; i < 5; i++)
        for (int j = 0; j < 5; j++)
            candReco->SetCovariance(i, j, this->GetCovariance(i,j));

    for (const Short_t hitIndex : fHitsIndexes)
        candReco->AddHit(hitIndex);
}

void TSlimRecoGigaTrackerCandidate::SetMomentum(TVector3 momentum)
{
  fMomentumX = momentum.X();
  fMomentumY = momentum.Y();
  fMomentumZ = momentum.Z();
}

void TSlimRecoGigaTrackerCandidate::SetPosition(Int_t station, TVector3 position)
{
  fPositionX[station] = position.X();
  fPositionY[station] = position.Y();
}

TVector3 TSlimRecoGigaTrackerCandidate::GetMomentum() const
{
  TVector3 momentum(fMomentumX, fMomentumY, fMomentumZ);
  return momentum;
}

TVector3 TSlimRecoGigaTrackerCandidate::GetPosition(Int_t station) const
{
  Float_t PositionZ = GigaTrackerChannelID::GetRawPosition(station, fPositionX[station], fPositionY[station]).Z();
  return TVector3(fPositionX[station], fPositionY[station], PositionZ);
}
