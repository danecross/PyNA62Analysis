#include "TSlimRecoCHANTICandidate.hh"
#include "TRecoCHANTICandidate.hh"

ClassImp(TSlimRecoCHANTICandidate)

TSlimRecoCHANTICandidate::TSlimRecoCHANTICandidate(TRecoCHANTICandidate* candReco)
{
    FromReco(candReco);
}

void TSlimRecoCHANTICandidate::FromReco(TRecoVCandidate* candVReco)
{
    TRecoCHANTICandidate *candReco = static_cast<TRecoCHANTICandidate*>(candVReco);
    fTime = candReco->GetTime();
    fXYMult = candReco->GetXYMult();
    fXPCharge = candReco->GetXPCharge();
    fYPCharge = candReco->GetYPCharge();
    fXPos = candReco->GetXPos();
    fYPos = candReco->GetYPos();
    for (int ihit = 0; ihit < candReco->GetNHits(); ihit++)
        this->AddHitIndex(candReco->GetHitsIndexes()[ihit]);
}

void TSlimRecoCHANTICandidate::ToReco(TRecoVCandidate* candVReco)
{
    TRecoCHANTICandidate *candReco = static_cast<TRecoCHANTICandidate*>(candVReco);
    candReco->SetTime(fTime);
    candReco->SetXYMult(fXYMult);
    candReco->SetXPCharge(fXPCharge);
    candReco->SetYPCharge(fYPCharge);
    candReco->SetXPos(fXPos);
    candReco->SetYPos(fYPos);
    for (const Short_t hitIndex : fHitsIndexes)
        candReco->AddHit(hitIndex);
}
