#include "TSlimRecoSACCandidate.hh"
#include "TRecoSACHit.hh"
#include "SACChannelID.hh"

ClassImp(TSlimRecoSACCandidate)

TSlimRecoSACCandidate::TSlimRecoSACCandidate(TRecoSACHit* hitReco) {
    FromReco(hitReco);
}

Float_t TSlimRecoSACCandidate::ComputeTrailingEdgeLow() const{
    if (HasLeadingEdgeLow() && HasTrailingEdgeLow())
        return fTimeOvThrLow + fLeadingEdgeLow;
    return -999.;
}

Bool_t TSlimRecoSACCandidate::HasAllTimesInOrder() const {
    if (HasAll4EdgesDetected() && fLeadingEdgeLow < fLeadingEdgeHigh
            && fLeadingEdgeHigh < ComputeTrailingEdgeHigh()
            && ComputeTrailingEdgeHigh() < ComputeTrailingEdgeLow())
        return true;
    return false;
}

Float_t TSlimRecoSACCandidate::ComputeTrailingEdgeHigh() const{
    if (HasLeadingEdgeHigh() && HasTrailingEdgeHigh())
        return fLeadingEdgeHigh + fTimeOvThrHigh;
    return -999.;
}

void TSlimRecoSACCandidate::FromReco(TRecoVHit* hitVReco) {
    TRecoSACHit *hitReco = static_cast<TRecoSACHit*>(hitVReco);

    fEdgeMask = hitReco->GetEdgeMask();
    fTimeOvThrLow = hitReco->GetTimeOverThresholdLowThr();

    fLeadingEdgeLow =  (fEdgeMask & 1) ? hitReco->GetLeadingEdgeLow() : -999.;
    fLeadingEdgeHigh = (fEdgeMask & 2) ? hitReco->GetLeadingEdgeHigh(): -999.;
    fLeadingESlewingSlope = hitReco->GetLeadingESlewingSlope();
    fTrailingESlewingSlope = hitReco->GetTrailingESlewingSlope();
    fChannelID = hitReco->GetChannelID();
    fTime = hitReco->GetTime();
}

void TSlimRecoSACCandidate::ToReco(TRecoVHit* hitVReco) {
    TRecoSACHit *hitReco = static_cast<TRecoSACHit*>(hitVReco);
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

Short_t TSlimRecoSACCandidate::GetPMTID() const {
    return SACChannelID::DecodeChannelID_Static(fChannelID%1000);
}
