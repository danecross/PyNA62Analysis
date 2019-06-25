#include "TSlimRecoCHANTIHit.hh"
#include "TRecoCHANTIHit.hh"
#include "CHANTIGeometry.hh"
#include "CHANTIChannelID.hh"

ClassImp(TSlimRecoCHANTIHit)

TSlimRecoCHANTIHit::TSlimRecoCHANTIHit(TRecoCHANTIHit* hitReco)
{
    FromReco(hitReco);
}

void TSlimRecoCHANTIHit::FromReco(TRecoVHit* hitVReco)
{
    TRecoCHANTIHit *hitReco = static_cast<TRecoCHANTIHit*>(hitVReco);
    fChannelID = hitReco->GetChannelID();
    fTime = hitReco->GetTime();
    fTimeWidth = hitReco->GetTimeWidth();
    fDeltaTime = hitReco->GetDeltaTime();
    fDeltaWidth = hitReco->GetDeltaWidth();
    fQualityFlag = hitReco->GetQualityFlag();
    fThresholdFlag = hitReco->GetThresholdFlag();
    fMult = hitReco->GetMult();
}

Short_t TSlimRecoCHANTIHit::GetBarID() const
{
    return CHANTIChannelID::DecodeChannelID_Static(fChannelID).fBarID;
}

UShort_t TSlimRecoCHANTIHit::GetPlaneID() const {
    return CHANTIChannelID::DecodeChannelID_Static(fChannelID).fPlaneID;
}

UShort_t TSlimRecoCHANTIHit::GetRingType() const {
    return CHANTIChannelID::DecodeChannelID_Static(fChannelID).fRingType;
}

UShort_t TSlimRecoCHANTIHit::GetSideID() const {
    return CHANTIChannelID::DecodeChannelID_Static(fChannelID).fSideID;
}

UShort_t TSlimRecoCHANTIHit::GetRingID() const {
    return GetRingType() + 2*GetPlaneID();
}

Float_t TSlimRecoCHANTIHit::GetX() const
{
    Float_t Step           = CHANTIGeometry::GetInstance()->GetTriangleBase()/2;
    Float_t GlobalShift    = CHANTIGeometry::GetInstance()->GetSquareLength()/2 - Step;
	Short_t BarID = this->GetBarID();
	if(this->GetRingType() == kX) return (abs(BarID)*Step - GlobalShift);
    else return (-999);
}

Float_t TSlimRecoCHANTIHit::GetY() const
{
	Float_t Step           = CHANTIGeometry::GetInstance()->GetTriangleBase()/2;
    Float_t GlobalShift    = CHANTIGeometry::GetInstance()->GetSquareLength()/2 - Step;
	Short_t BarID = this->GetBarID();
	if(this->GetRingType() == kX) return (-999);
    else return (abs(BarID)*Step - GlobalShift);
}

Float_t TSlimRecoCHANTIHit::GetZ() const
{
	UShort_t Plane = this->GetPlaneID();
    UShort_t RingType = this->GetRingType();
	UShort_t RingID = RingType + 2*Plane;
    return (CHANTIGeometry::GetInstance()->GetZPos_Ring(RingID));
}

void TSlimRecoCHANTIHit::ToReco(TRecoVHit* hitVReco)
{
    TRecoCHANTIHit *hitReco = static_cast<TRecoCHANTIHit*>(hitVReco);
    // Member variables
    hitReco->SetChannelID(fChannelID);
    hitReco->SetTime(fTime);
    hitReco->SetTimeWidth(fTimeWidth);
    hitReco->SetDeltaTime(fDeltaTime);
    hitReco->SetDeltaWidth(fDeltaWidth);
    hitReco->SetQualityFlag(fQualityFlag);
    hitReco->SetThresholdFlag(fThresholdFlag);
    hitReco->SetMult(fMult);
    // Various geometric info
    hitReco->DecodeChannelID();
    hitReco->SetX(this->GetX());
    hitReco->SetY(this->GetY());
    hitReco->SetZ(this->GetZ());
    // remaining variables are left initialized to their default constructor values
}

