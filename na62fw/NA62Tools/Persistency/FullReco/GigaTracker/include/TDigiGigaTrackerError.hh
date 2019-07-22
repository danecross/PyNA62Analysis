#ifndef TDigiGigaTrackerError_H
#define TDigiGigaTrackerError_H
#include "TDigiVError.hh"

class TDigiGigaTrackerError : public TDigiVError {

public:

  //  TDigiGigaTrackerError();
  // ~TDigiGigaTrackerError(){};



public:
  
  static int GetErrorType( std::string );
  static std::string GetErrorName( int );


private:

  enum error_types {
    e_GigaTrackerDAQBoardTimeStamp_DataStatus = 1,
    e_GigaTrackerDAQBoardTimeStamp_ErrorChip,
    e_GigaTrackerDAQBoardTimeStamp_ErrorFlag,
    e_GigaTrackerDAQBoardTimeStamp_ErrorIncFcL1,
    e_GigaTrackerDAQBoardTimeStamp_ErrorqChip,
    e_GigaTrackerDAQBoardTimeStamp_ErrorStation,
    e_GigaTrackerDAQBoardTimeStamp_HitArbiter,
    e_GigaTrackerDAQBoardTimeStamp_PixAddress,
    e_GigaTrackerDAQBoardTimeStamp_PUAddress,
    e_GigaTrackerDAQBoardTrailerOne_ErrorFlag,
    e_GigaTrackerDAQBoardTrailerOne_ErrorIncNHitLength,
    e_GigaTrackerDAQBoardTrailerTwo_Flag,
    e_GigaTrackerNa62HeaderL0_ErrorFlag,
    e_GigaTrackerNa62HeaderL1_Flag,
    e_GigaTrackerNa62HeaderL1_SubId,
    e_GTKRawDecoder_DuplicatedHits,
    e_GTKRawDecoder_ErrorIncNTrigg,
    e_GTKRawDecoder_ErrorIncTSHitHeader,
    e_GTKRawDecoder_EvtNb,
    e_GTKRawDecoder_FCLSB,
    e_GTKRawDecoder_IncChip,
    e_GTKRawDecoder_Limiter,
    e_GTKRawDecoder_SubId,
    e_GTKRawDecoder_Trigger,
    e_GTKRawDecoder_TS,
    e_GTKRawDecoder_MissingChip,
    e_GigaTrackerDAQBoardTrailerOne_Chip,
    e_GigaTrackerDAQBoardTrailerTwo_Chip,
    e_GigaTrackerDAQBoardTrailerThree_Chip
  };

  ClassDef(TDigiGigaTrackerError,1);

};
#endif
