#include "TSlimRecoSAVCandidate.hh"

#include "TRecoSAVCandidate.hh"

ClassImp(TSlimRecoSAVCandidate)

TSlimRecoSAVCandidate::TSlimRecoSAVCandidate(TRecoSAVCandidate* candReco)
{
    FromReco(candReco);
}

void TSlimRecoSAVCandidate::FromReco(TRecoVCandidate* candVReco)
{
    TRecoSAVCandidate *candReco = static_cast<TRecoSAVCandidate*>(candVReco);

    fTime   = candReco->GetTime();
    fEnergy = candReco->GetEnergy();
    fX      = candReco->GetX();
    fY      = candReco->GetY();

    fHitsIndexes.clear();
    for (int ihit = 0; ihit < candReco->GetNHits(); ++ihit)
        this->AddHitIndex(candReco->GetHitsIndexes()[ihit]);
}

void TSlimRecoSAVCandidate::ToReco(TRecoVCandidate* candVReco)
{
    TRecoSAVCandidate *candReco = static_cast<TRecoSAVCandidate*>(candVReco);
    // Member variables

    candReco->SetTime(fTime);
    candReco->SetEnergy(fEnergy);
    candReco->SetPosition(fX, fY);

    for (const Short_t hitIndex : fHitsIndexes)
        candReco->AddHit(hitIndex);
}
