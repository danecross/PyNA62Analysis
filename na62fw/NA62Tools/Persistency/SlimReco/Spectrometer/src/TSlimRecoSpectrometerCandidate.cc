#include "TSlimRecoSpectrometerCandidate.hh"
#include "TRecoSpectrometerCandidate.hh"

ClassImp(TSlimRecoSpectrometerCandidate)

TSlimRecoSpectrometerCandidate::TSlimRecoSpectrometerCandidate(TRecoSpectrometerCandidate* candReco)
{
    FromReco(candReco);
}

Int_t TSlimRecoSpectrometerCandidate::GetArrayIndexCovarianceMatrix(Int_t i, Int_t j)
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

void TSlimRecoSpectrometerCandidate::SetCovariance(Int_t i, Int_t j, Double_t value)
{
    Int_t index = TSlimRecoSpectrometerCandidate::GetArrayIndexCovarianceMatrix(i, j);
    if (index >= 0 && index < TSlimRecoSpectrometerCandidate::kCovarianceDimension) {
        fCovariance[index] = value;
    }
}

Double_t TSlimRecoSpectrometerCandidate::GetCovariance(int i, int j) const
{
    Int_t index = TSlimRecoSpectrometerCandidate::GetArrayIndexCovarianceMatrix(i, j);
    if (index >= 0 && index < TSlimRecoSpectrometerCandidate::kCovarianceDimension) {
        return fCovariance[index];
    } else {
        return 0;
    }
}

void TSlimRecoSpectrometerCandidate::FromReco(TRecoVCandidate* candVReco)
{
    TRecoSpectrometerCandidate *candReco = static_cast<TRecoSpectrometerCandidate*>(candVReco);
    fTime = candReco->GetTime();
    fMomentum = candReco->GetMomentum();
    fMomentumBeforeFit = candReco->GetMomentumBeforeFit();
    fCharge = candReco->GetCharge();
    fNChambers = candReco->GetNChambers();
    fSlopeXBeforeMagnet = candReco->GetSlopeXBeforeMagnet();
    fSlopeYBeforeMagnet = candReco->GetSlopeYBeforeMagnet();
    fPositionXBeforeMagnet = candReco->GetPositionBeforeMagnet().X();
    fPositionYBeforeMagnet = candReco->GetPositionBeforeMagnet().Y();
    fSlopeXAfterMagnet = candReco->GetSlopeXAfterMagnet();
    fSlopeYAfterMagnet = candReco->GetSlopeYAfterMagnet();
    fPositionXAfterMagnet = candReco->GetPositionAfterMagnet().X();
    fPositionYAfterMagnet = candReco->GetPositionAfterMagnet().Y();
    fChi2 = candReco->GetChi2();
    fLeadingTime = candReco->GetLeadingTime();

    if (candReco->GetNChambers() == 4)
        fMissingChamberID = -1;
    else
        for (int ich = 0; ich < 4; ich++)
            if (candReco->GetChamberId(ich) == -1)
                fMissingChamberID = ich;

    for (int i = 0; i < 5; i++)
        for (int j = 0; j < 5; j++)
            this->SetCovariance(i, j, candReco->GetCovariance(i, j));

    fHitsIndexes.clear();
    for (int ihit = 0; ihit < candReco->GetNHits(); ihit++)
        this->AddHitIndex(candReco->GetHitsIndexes()[ihit]);
}

void TSlimRecoSpectrometerCandidate::ToReco(TRecoVCandidate* candVReco)
{
    TRecoSpectrometerCandidate *candReco = static_cast<TRecoSpectrometerCandidate*>(candVReco);
    candReco->SetTime(fTime);
    candReco->SetMomentum(fMomentum);
    candReco->SetMomentumBeforeFit(fMomentumBeforeFit);
    candReco->SetCharge(fCharge);
    candReco->SetNChambers(fNChambers);
    candReco->SetSlopeXBeforeMagnet(fSlopeXBeforeMagnet);
    candReco->SetSlopeYBeforeMagnet(fSlopeYBeforeMagnet);
    candReco->SetPositionBeforeMagnet(
        TVector3(fPositionXBeforeMagnet, fPositionYBeforeMagnet,
                 TRecoSpectrometerCandidate::kZReferenceBeforeMagnet));
    candReco->SetSlopeXAfterMagnet(fSlopeXAfterMagnet);
    candReco->SetSlopeYAfterMagnet(fSlopeYAfterMagnet);
    candReco->SetPositionAfterMagnet(
        TVector3(fPositionXAfterMagnet, fPositionYAfterMagnet,
                 TRecoSpectrometerCandidate::kZReferenceAfterMagnet));
    candReco->SetChi2(fChi2);
    candReco->SetLeadingTime(fLeadingTime);

    for (int ich = 0; ich < 4; ich++)
        if (fMissingChamberID == ich)
            candReco->SetChamberId(ich, -1);
        else
            candReco->SetChamberId(ich, ich);

    for (int i = 0; i < 5; i++)
        for (int j = 0; j < 5; j++)
            candReco->SetCovariance(i, j, this->GetCovariance(i,j));

    for (const Short_t hitIndex : fHitsIndexes)
        candReco->AddHit(hitIndex);
}

TVector3 TSlimRecoSpectrometerCandidate::GetPositionBeforeMagnet() const {
    return TVector3(fPositionXBeforeMagnet, fPositionYBeforeMagnet, TRecoSpectrometerCandidate::kZReferenceBeforeMagnet);
}

TVector3 TSlimRecoSpectrometerCandidate::GetPositionAfterMagnet() const {
    return TVector3(fPositionXAfterMagnet, fPositionYAfterMagnet, TRecoSpectrometerCandidate::kZReferenceAfterMagnet);
}

TVector3 TSlimRecoSpectrometerCandidate::GetThreeMomentumBeforeMagnet() const {
  Double_t Norm =
    1.0/sqrt(1.0 + fSlopeXBeforeMagnet*fSlopeXBeforeMagnet + fSlopeYBeforeMagnet*fSlopeYBeforeMagnet);
  return
    TVector3(fMomentum*Norm*fSlopeXBeforeMagnet, fMomentum*Norm*fSlopeYBeforeMagnet, fMomentum*Norm);
}

TVector3 TSlimRecoSpectrometerCandidate::GetThreeMomentumAfterMagnet() const {
  Double_t Norm =
    1.0/sqrt(1.0 + fSlopeXAfterMagnet*fSlopeXAfterMagnet + fSlopeYAfterMagnet*fSlopeYAfterMagnet);
  return
    TVector3(fMomentum*Norm*fSlopeXAfterMagnet, fMomentum*Norm*fSlopeYAfterMagnet, fMomentum*Norm);
}
