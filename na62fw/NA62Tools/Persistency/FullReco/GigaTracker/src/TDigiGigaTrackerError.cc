#include "TDigiGigaTrackerError.hh"
ClassImp(TDigiGigaTrackerError)


int TDigiGigaTrackerError::GetErrorType(std::string s){

  if( s == "GigaTrackerDAQBoardTimeStamp_DataStatus") return e_GigaTrackerDAQBoardTimeStamp_DataStatus;
  if( s == "GigaTrackerDAQBoardTimeStamp_ErrorChip") return e_GigaTrackerDAQBoardTimeStamp_ErrorChip;
  if( s == "GigaTrackerDAQBoardTimeStamp_ErrorFlag") return e_GigaTrackerDAQBoardTimeStamp_ErrorFlag;
  if( s == "GigaTrackerDAQBoardTimeStamp_ErrorIncFcL1") return e_GigaTrackerDAQBoardTimeStamp_ErrorIncFcL1;
  if( s == "GigaTrackerDAQBoardTimeStamp_ErrorqChip") return e_GigaTrackerDAQBoardTimeStamp_ErrorqChip;
  if( s == "GigaTrackerDAQBoardTimeStamp_ErrorStation") return e_GigaTrackerDAQBoardTimeStamp_ErrorStation;
  if( s == "GigaTrackerDAQBoardTimeStamp_HitArbiter") return e_GigaTrackerDAQBoardTimeStamp_HitArbiter;
  if( s == "GigaTrackerDAQBoardTimeStamp_PixAddress") return e_GigaTrackerDAQBoardTimeStamp_PixAddress;
  if( s == "GigaTrackerDAQBoardTimeStamp_PUAddress") return e_GigaTrackerDAQBoardTimeStamp_PUAddress;
  if( s == "GigaTrackerDAQBoardTrailerOne_ErrorFlag") return e_GigaTrackerDAQBoardTrailerOne_ErrorFlag;
  if( s == "GigaTrackerDAQBoardTrailerOne_ErrorIncNHitLength") return e_GigaTrackerDAQBoardTrailerOne_ErrorIncNHitLength;
  if( s == "GigaTrackerDAQBoardTrailerTwo_Flag") return e_GigaTrackerDAQBoardTrailerTwo_Flag;
  if( s == "GigaTrackerNa62HeaderL0_ErrorFlag") return e_GigaTrackerNa62HeaderL0_ErrorFlag;
  if( s == "GigaTrackerNa62HeaderL1_Flag") return e_GigaTrackerNa62HeaderL1_Flag;
  if( s == "GigaTrackerNa62HeaderL1_SubId") return e_GigaTrackerNa62HeaderL1_SubId;
  if( s == "GTKRawDecoder_DuplicatedHits") return e_GTKRawDecoder_DuplicatedHits;
  if( s == "GTKRawDecoder_ErrorIncNTrigg") return e_GTKRawDecoder_ErrorIncNTrigg;
  if( s == "GTKRawDecoder_ErrorIncTSHitHeader") return e_GTKRawDecoder_ErrorIncTSHitHeader;
  if( s == "GTKRawDecoder_EvtNb") return e_GTKRawDecoder_EvtNb;
  if( s == "GTKRawDecoder_FCLSB") return e_GTKRawDecoder_FCLSB;
  if( s == "GTKRawDecoder_IncChip") return e_GTKRawDecoder_IncChip;
  if( s == "GTKRawDecoder_Limiter") return e_GTKRawDecoder_Limiter;
  if( s == "GTKRawDecoder_SubId") return e_GTKRawDecoder_SubId;
  if( s == "GTKRawDecoder_Trigger") return e_GTKRawDecoder_Trigger;
  if( s == "GTKRawDecoder_TS") return e_GTKRawDecoder_TS;
  if( s == "GTKRawDecoder_MissingChip") return  e_GTKRawDecoder_MissingChip;
  if( s == "GigaTrackerDAQBoardTrailerOne_Chip") return  e_GigaTrackerDAQBoardTrailerOne_Chip;
  if( s == "GigaTrackerDAQBoardTrailerTwo_Chip") return  e_GigaTrackerDAQBoardTrailerTwo_Chip;
  if( s == "GigaTrackerDAQBoardTrailerThree_Chip") return  e_GigaTrackerDAQBoardTrailerThree_Chip;
  
  else return 63;

}

std::string TDigiGigaTrackerError::GetErrorName(int i){

  if( i == e_GigaTrackerDAQBoardTimeStamp_DataStatus) return std::string("GigaTrackerDAQBoardTimeStamp_DataStatus");
  if( i == e_GigaTrackerDAQBoardTimeStamp_ErrorChip) return std::string("GigaTrackerDAQBoardTimeStamp_ErrorChip");
  if( i == e_GigaTrackerDAQBoardTimeStamp_ErrorFlag) return std::string("GigaTrackerDAQBoardTimeStamp_ErrorFlag");
  if( i == e_GigaTrackerDAQBoardTimeStamp_ErrorIncFcL1) return std::string("GigaTrackerDAQBoardTimeStamp_ErrorIncFcL1");
  if( i == e_GigaTrackerDAQBoardTimeStamp_ErrorqChip) return std::string("GigaTrackerDAQBoardTimeStamp_ErrorqChip");
  if( i == e_GigaTrackerDAQBoardTimeStamp_ErrorStation) return std::string("GigaTrackerDAQBoardTimeStamp_ErrorStation");
  if( i == e_GigaTrackerDAQBoardTimeStamp_HitArbiter) return std::string("GigaTrackerDAQBoardTimeStamp_HitArbiter");
  if( i == e_GigaTrackerDAQBoardTimeStamp_PixAddress) return std::string("GigaTrackerDAQBoardTimeStamp_PixAddress");
  if( i == e_GigaTrackerDAQBoardTimeStamp_PUAddress) return std::string("GigaTrackerDAQBoardTimeStamp_PUAddress");
  if( i == e_GigaTrackerDAQBoardTrailerOne_ErrorFlag) return std::string("GigaTrackerDAQBoardTrailerOne_ErrorFlag");
  if( i == e_GigaTrackerDAQBoardTrailerOne_ErrorIncNHitLength) return std::string("GigaTrackerDAQBoardTrailerOne_ErrorIncNHitLength");
  if( i == e_GigaTrackerDAQBoardTrailerTwo_Flag) return std::string("GigaTrackerDAQBoardTrailerTwo_Flag");
  if( i == e_GigaTrackerNa62HeaderL0_ErrorFlag) return std::string("GigaTrackerNa62HeaderL0_ErrorFlag");
  if( i == e_GigaTrackerNa62HeaderL1_Flag) return std::string("GigaTrackerNa62HeaderL1_Flag");
  if( i == e_GigaTrackerNa62HeaderL1_SubId) return std::string("GigaTrackerNa62HeaderL1_SubId");
  if( i == e_GTKRawDecoder_DuplicatedHits) return std::string("GTKRawDecoder_DuplicatedHits");
  if( i == e_GTKRawDecoder_ErrorIncNTrigg) return std::string("GTKRawDecoder_ErrorIncNTrigg");
  if( i == e_GTKRawDecoder_ErrorIncTSHitHeader) return std::string("GTKRawDecoder_ErrorIncTSHitHeader");
  if( i == e_GTKRawDecoder_EvtNb) return std::string("GTKRawDecoder_EvtNb");
  if( i == e_GTKRawDecoder_FCLSB) return std::string("GTKRawDecoder_FCLSB");
  if( i == e_GTKRawDecoder_IncChip) return std::string("GTKRawDecoder_IncChip");
  if( i == e_GTKRawDecoder_Limiter) return std::string("GTKRawDecoder_Limiter");
  if( i == e_GTKRawDecoder_SubId) return std::string("GTKRawDecoder_SubId");
  if( i == e_GTKRawDecoder_Trigger) return std::string("GTKRawDecoder_Trigger");
  if( i == e_GTKRawDecoder_TS) return std::string("GTKRawDecoder_TS");
  if( i == e_GTKRawDecoder_MissingChip) return std::string("GTKRawDecoder_MissingChip");
  if( i == e_GigaTrackerDAQBoardTrailerOne_Chip) return std::string("GigaTrackerDAQBoardTrailerOne_Chip");
  if( i == e_GigaTrackerDAQBoardTrailerTwo_Chip) return std::string("GigaTrackerDAQBoardTrailerTwo_Chip");
  if( i == e_GigaTrackerDAQBoardTrailerThree_Chip) return std::string("GigaTrackerDAQBoardTrailerThree_Chip");

  else return std::string("");

}
