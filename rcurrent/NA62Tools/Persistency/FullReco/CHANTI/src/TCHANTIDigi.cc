//----------------------------------------------------------------
// History:
//
// Created by Antonio Cassese (Antonio.Cassese@cern.ch) 2012-10-19
//
//----------------------------------------------------------------
#include "TCHANTIDigi.hh"
ClassImp(TCHANTIDigi)


Int_t TCHANTIDigi::EncodeChannelID() {
  fChannelID =  CHANTIChannelID::EncodeChannelID() + fThresholdType;
  return fChannelID;
}

void TCHANTIDigi::DecodeChannelID() {
    CHANTIChannelID::DecodeChannelID(fChannelID);
    fThresholdType = fChannelID%10;
}


Int_t TCHANTIDigi::CompareChannel(const TObject *obj) const{
    if(fDigiSortFlag == 0){
      if(fChannelID < static_cast<const TDCVHit*>(obj)->GetChannelID()) return -1;
      else if(fChannelID > static_cast<const TDCVHit*>(obj)->GetChannelID()) return 1;
      //else return 0; 
      else{
	/*
	if(fLeadingEdge > -1e20 && ((TDCVHit*)obj)->GetLeadingEdge() > -1e20 && fLeadingEdge < ((TDCVHit*)obj)->GetLeadingEdge()) return -1;
	else if(fLeadingEdge > -1e20 && ((TDCVHit*)obj)->GetLeadingEdge() < -1e20  && fLeadingEdge < ((TDCVHit*)obj)->GetTrailingEdge()) return -1;
	else if(fLeadingEdge < -1e20 && ((TDCVHit*)obj)->GetLeadingEdge() > -1e20 && fTrailingEdge < ((TDCVHit*)obj)->GetLeadingEdge()) return -1;
	else if(fLeadingEdge < -1e20 && ((TDCVHit*)obj)->GetLeadingEdge() < -1e20 && fTrailingEdge < ((TDCVHit*)obj)->GetTrailingEdge()) return -1;
	*/
	if(fLeadingEdge > -1e20 && static_cast<const TDCVHit*>(obj)->GetLeadingEdge() > -1e20 && fLeadingEdge < static_cast<const TDCVHit*>(obj)->GetLeadingEdge()) return -1;
	else if(fLeadingEdge > -1e20 && fTrailingEdge > -1e20 && static_cast<const TDCVHit*>(obj)->GetLeadingEdge() < -1e20  && fTrailingEdge < static_cast<const TDCVHit*>(obj)->GetTrailingEdge()) return -1;
	else if(fLeadingEdge > -1e20 && fTrailingEdge < -1e20 && static_cast<const TDCVHit*>(obj)->GetLeadingEdge() < -1e20  && fLeadingEdge <  static_cast<const TDCVHit*>(obj)->GetTrailingEdge()) return -1;
	else if(fLeadingEdge < -1e20 && static_cast<const TDCVHit*>(obj)->GetTrailingEdge() > -1e20 && fTrailingEdge < static_cast<const TDCVHit*>(obj)->GetTrailingEdge()) return -1;
	else if(fLeadingEdge < -1e20 && static_cast<const TDCVHit*>(obj)->GetTrailingEdge() < -1e20 && fTrailingEdge < static_cast<const TDCVHit*>(obj)->GetLeadingEdge()) return -1;
	else return 1;
	}
    }
    else{
      if(int(fChannelID/10) < int(static_cast<const TDCVHit*>(obj)->GetChannelID()/10)) return -1;
      else if(int(fChannelID/10) > int(static_cast<const TDCVHit*>(obj)->GetChannelID()/10)) return 1;
      else{
	if(fLeadingEdge > -1e20 && static_cast<const TDCVHit*>(obj)->GetLeadingEdge() > -1e20 && fLeadingEdge < static_cast<const TDCVHit*>(obj)->GetLeadingEdge()) return -1;
	else if(fLeadingEdge > -1e20 && fTrailingEdge > -1e20 && static_cast<const TDCVHit*>(obj)->GetLeadingEdge() < -1e20  && fTrailingEdge < static_cast<const TDCVHit*>(obj)->GetTrailingEdge()) return -1;
	else if(fLeadingEdge > -1e20 && fTrailingEdge < -1e20 && static_cast<const TDCVHit*>(obj)->GetLeadingEdge() < -1e20  && fLeadingEdge <  static_cast<const TDCVHit*>(obj)->GetTrailingEdge()) return -1;
	else if(fLeadingEdge < -1e20 && static_cast<const TDCVHit*>(obj)->GetTrailingEdge() > -1e20 && fTrailingEdge < static_cast<const TDCVHit*>(obj)->GetTrailingEdge()) return -1;
	else if(fLeadingEdge < -1e20 && static_cast<const TDCVHit*>(obj)->GetTrailingEdge() < -1e20 && fTrailingEdge < static_cast<const TDCVHit*>(obj)->GetLeadingEdge()) return -1;
	else return 1;
	}
      //else return 0;
    }
    //Int_t CompareChannel(const TObject *obj) const {if(int(fChannelID) < int(((TDCVHit*)obj)->GetChannelID())) return -1;
    //else if(int(fChannelID) > int(((TDCVHit*)obj)->GetChannelID())) return 1;
}

void TCHANTIDigi::Clear(Option_t* option){
  TDCVHit::Clear(option);
  fThresholdType = -1;
  fDigiSortFlag = 0;
}
