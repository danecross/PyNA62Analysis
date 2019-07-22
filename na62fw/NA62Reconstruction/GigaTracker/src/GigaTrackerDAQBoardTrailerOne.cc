#include "GigaTrackerDAQBoardTrailerOne.hh" 
#include "RG_Utilities.hh"
#include <iomanip>
#include "GigaTrackerErrorsHandler.hh"

namespace GTK
{

  const int GigaTrackerDAQBoardTrailerOne::buffLength  ; 

  const u32 GigaTrackerDAQBoardTrailerOne::iTag = 0;
  const u32 GigaTrackerDAQBoardTrailerOne::mTag = 0xF0000000;
  const u32 GigaTrackerDAQBoardTrailerOne::sTag = 28;
  const u32 GigaTrackerDAQBoardTrailerOne::rTag = 0xA;
  
  const u32 GigaTrackerDAQBoardTrailerOne::iChipID = 0;
  const u32 GigaTrackerDAQBoardTrailerOne::mChipID = 0x0F000000;
  const u32 GigaTrackerDAQBoardTrailerOne::sChipID = 24;
  
  const u32 GigaTrackerDAQBoardTrailerOne::iL1AEvtCounter = 0;
  const u32 GigaTrackerDAQBoardTrailerOne::mL1AEvtCounter = 0x00FFFFFF;
  const u32 GigaTrackerDAQBoardTrailerOne::sL1AEvtCounter = 0;
  
  const u32 GigaTrackerDAQBoardTrailerOne::iL1ATimeStamp = 1;
  const u32 GigaTrackerDAQBoardTrailerOne::mL1ATimeStamp = 0xFFFFFFFF;
  const u32 GigaTrackerDAQBoardTrailerOne::sL1ATimeStamp = 0;


  //  ==============================================  
  GigaTrackerDAQBoardTrailerOne::GigaTrackerDAQBoardTrailerOne()
  {
    fErrorFlag      = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTrailerOne_ErrorFlag");
    fErrorChip      = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTrailerOne_Chip");
  }

  //  ==============================================  
  GigaTrackerDAQBoardTrailerOne::~GigaTrackerDAQBoardTrailerOne()
  {
   
  }

  //==============================================  
  int GigaTrackerDAQBoardTrailerOne::SetBuffer(u8* buf){
    RG_CHECK_PTR_RETURN_INT(buf,1);
    for(int i(0); i<buffLength; i++){
      fBuffer[i] = buf[i];
    }
    //CONSISTENCY CHECK - tag must be 0x1010
    if (this->GetTag() != GigaTrackerDAQBoardTrailerOne::rTag){
      char errBuff[500];
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "Tag is not compatible with being a TrailerOne: %u  Ref: %u. Offending Word: %8.8x",this->GetTag(),GigaTrackerDAQBoardTrailerOne::rTag,Word[GigaTrackerDAQBoardTrailerOne::iTag]);
      REPORT_NEW_ERROR_RETURN(fErrorFlag,errBuff);
    }

    //CONSISTENCY CHECK - chip
    if (this->GetChipId() >9){
      char errBuff[500];
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "Invalid Chip Number %d. Offending Word: %8.8x",this->GetChipId(),Word[GigaTrackerDAQBoardTrailerOne::iChipID]);
      REPORT_NEW_ERROR_RETURN(fErrorChip,errBuff);
    }

    return 0;
  }


  //==============================================  
  int GigaTrackerDAQBoardTrailerOne::GetBufferLength(){
    return buffLength;
  }

  //==============================================
  u32 GigaTrackerDAQBoardTrailerOne::GetTag()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iTag],mTag,sTag, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTrailerOne::GetChipId()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iChipID],mChipID,sChipID, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTrailerOne::GetL1AEvtCounter()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iL1AEvtCounter],mL1AEvtCounter,sL1AEvtCounter, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTrailerOne::GetL1ATimeStamp()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iL1ATimeStamp],mL1ATimeStamp,sL1ATimeStamp, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

  //==============================================
  std::ostream& operator<< (std::ostream &out, GigaTrackerDAQBoardTrailerOne & t){
    u32* pU32;
    pU32 = (u32*) &(t.fBuffer[0]);
    for(int i(0); i<t.GetBufferLength()/4;i++){
      out << "  0x"<< std::hex<<std::setfill('0')<<std::setw(8)<<pU32[i]<<"\n"<<std::dec;
     }

    out<<"   Tag           :"<<t.GetTag()<<"\n";
    out<<"   ChipID        :"<<t.GetChipId()<<"\n";
    out<<"   L1AEvtCounter :"<<t.GetL1AEvtCounter()<<" = "<<std::hex<<"0x"<< t.GetL1AEvtCounter()<<"\n"<<std::dec;
    out<<"   L1ATimeStamp  :"<<t.GetL1ATimeStamp()<<" = "<<std::hex<<"0x"<< t.GetL1ATimeStamp()<<std::dec;
    return out;
  }
}//~namespace IImaS


