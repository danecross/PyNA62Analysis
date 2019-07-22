#include "TSlimRecoIRCCandidate.hh"
#include "TRecoIRCHit.hh"
#include "IRCChannelID.hh"

ClassImp(TSlimRecoIRCCandidate)

TSlimRecoIRCCandidate::TSlimRecoIRCCandidate(TRecoIRCHit* hitReco)
{
    FromReco(hitReco);
}

Bool_t TSlimRecoIRCCandidate::HasAllTimesInOrder() const {
    if (HasAll4EdgesDetected() && fLeadingEdgeLow < fLeadingEdgeHigh
            && fLeadingEdgeHigh < ComputeTrailingEdgeHigh()
            && ComputeTrailingEdgeHigh() < ComputeTrailingEdgeLow())
        return true;
    return false;
}

Float_t TSlimRecoIRCCandidate::ComputeTrailingEdgeLow() const {
    if (HasLeadingEdgeLow() && HasTrailingEdgeLow())
        return fTimeOvThrLow + fLeadingEdgeLow;
    return -999.;
}

Float_t TSlimRecoIRCCandidate::ComputeTrailingEdgeHigh() const {
    if (HasLeadingEdgeHigh() && HasTrailingEdgeHigh())
        return fLeadingEdgeHigh + fTimeOvThrHigh;
    return -999.;
}

void TSlimRecoIRCCandidate::FromReco(TRecoVHit* hitVReco)
{
    TRecoIRCHit *hitReco = static_cast<TRecoIRCHit*>(hitVReco);

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

void TSlimRecoIRCCandidate::ToReco(TRecoVHit* hitVReco)
{
    TRecoIRCHit *hitReco = static_cast<TRecoIRCHit*>(hitVReco);
    // Member variables
    hitReco->SetChannelID(fChannelID);
    hitReco->SetTimeOverThresholdLowThr(fTimeOvThrLow);
    hitReco->SetTimeOverThresholdHighThr(fTimeOvThrHigh);
    hitReco->SetLeadingESlewingSlope(fLeadingESlewingSlope);
    hitReco->SetTrailingESlewingSlope(fTrailingESlewingSlope);

    hitReco->SetLeadingEdgeLow(fLeadingEdgeLow);
    hitReco->SetTrailingEdgeLow(ComputeTrailingEdgeLow());
    hitReco->SetLeadingEdgeHigh(fLeadingEdgeHigh);
    hitReco->SetTrailingEdgeHigh(ComputeTrailingEdgeHigh());
    hitReco->SetChannelID(fChannelID);
    hitReco->SetTime(fTime);
    hitReco->DecodeChannelID();
}

Short_t TSlimRecoIRCCandidate::GetPMTID() const {
    return IRCChannelID::DecodeChannelID_Static(fChannelID%1000);
}
