#include "TSlimRecoCHODCandidate.hh"
#include "TRecoCHODCandidate.hh"

ClassImp(TSlimRecoCHODCandidate)

TSlimRecoCHODCandidate::TSlimRecoCHODCandidate(TRecoCHODCandidate* candReco)
{
    FromReco(candReco);
}

void TSlimRecoCHODCandidate::FromReco(TRecoVCandidate* candVReco)
{
    TRecoCHODCandidate *candReco = static_cast<TRecoCHODCandidate*>(candVReco);
    fTime = candReco->GetTime();
    SetPosition(candReco->GetHitPosition());
    fNHitPairs = candReco->GetNHitPairs();

    fHitsIndexes.clear();
    for (int ihit = 0; ihit < candReco->GetNHits(); ihit++)
        this->AddHitIndex(candReco->GetHitsIndexes()[ihit]);
}

void TSlimRecoCHODCandidate::ToReco(TRecoVCandidate* candVReco)
{
    TRecoCHODCandidate *candReco = static_cast<TRecoCHODCandidate*>(candVReco);
    candReco->SetTime(fTime);
    candReco->SetHitPosition(GetPosition());
    candReco->SetNHitPairs(fNHitPairs);

    for (const Short_t hitIndex : fHitsIndexes)
        candReco->AddHit(hitIndex);
}
