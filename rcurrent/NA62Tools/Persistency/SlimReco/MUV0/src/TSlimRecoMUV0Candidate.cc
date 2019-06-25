#include "TSlimRecoMUV0Candidate.hh"
#include "TRecoMUV0Hit.hh"
#include "MUV0ChannelID.hh"

ClassImp(TSlimRecoMUV0Candidate)

TSlimRecoMUV0Candidate::TSlimRecoMUV0Candidate(TRecoMUV0Hit* hitReco)
{
    FromReco(hitReco);
}

Bool_t TSlimRecoMUV0Candidate::HasAllTimesInOrder() const {
    if (HasAll4EdgesDetected() && fLeadingEdgeLow < fLeadingEdgeHigh
            && fLeadingEdgeHigh < ComputeTrailingEdgeHigh()
            && ComputeTrailingEdgeHigh() < ComputeTrailingEdgeLow())
        return true;
    return false;
}

Float_t TSlimRecoMUV0Candidate::ComputeTrailingEdgeLow() const {
    if (HasLeadingEdgeLow() && HasTrailingEdgeLow())
        return fTimeOvThrLow + fLeadingEdgeLow;
    return -999.;
}

Float_t TSlimRecoMUV0Candidate::ComputeTrailingEdgeHigh() const {
    if (HasLeadingEdgeHigh() && HasTrailingEdgeHigh())
        return fLeadingEdgeHigh + fTimeOvThrHigh;
    return -999.;
}

void TSlimRecoMUV0Candidate::FromReco(TRecoVHit* hitVReco)
{
    TRecoMUV0Hit *hitReco = static_cast<TRecoMUV0Hit*>(hitVReco);

    fEdgeMask = hitReco->GetEdgeMask();
    fTimeOvThrLow = hitReco->GetTimeOverThresholdLowThr();
    fTimeOvThrHigh = hitReco->GetTimeOverThresholdHighThr();
    fLeadingESlewingSlope = hitReco->GetLeadingESlewingSlope();
    fTrailingESlewingSlope = hitReco->GetTrailingESlewingSlope();

    fLeadingEdgeLow =  (fEdgeMask & 1) ? hitReco->GetLeadingEdgeLow() : -999.;
    fLeadingEdgeHigh = (fEdgeMask & 2) ? hitReco->GetLeadingEdgeHigh(): -999.;
    fChannelID = hitReco->GetChannelID();
    fTime = hitReco->GetTime();
}

void TSlimRecoMUV0Candidate::ToReco(TRecoVHit* hitVReco)
{
    TRecoMUV0Hit *hitReco = static_cast<TRecoMUV0Hit*>(hitVReco);
    // Member variables
    hitReco->SetChannelID(fChannelID);
    hitReco->SetTimeOverThresholdLowThr(fTimeOvThrLow);
    hitReco->SetTimeOverThresholdHighThr(fTimeOvThrHigh);
    hitReco->SetLeadingESlewingSlope(fLeadingESlewingSlope);
    hitReco->SetTrailingESlewingSlope(fTrailingESlewingSlope);
    if (fEdgeMask & 0x1) hitReco->SetLeadingEdgeLow(fLeadingEdgeLow);
    if (fEdgeMask & 0x8) hitReco->SetTrailingEdgeLow(ComputeTrailingEdgeLow());
    if (fEdgeMask & 0x2) hitReco->SetLeadingEdgeHigh(fLeadingEdgeHigh);
    if (fEdgeMask & 0x4) hitReco->SetTrailingEdgeHigh(ComputeTrailingEdgeHigh());
    hitReco->SetChannelID(fChannelID);
    hitReco->SetTime(fTime);
    hitReco->DecodeChannelID();
}

Short_t TSlimRecoMUV0Candidate::GetTileID() const {
    return MUV0ChannelID::DecodeChannelID_Static(fChannelID%1000);
}
