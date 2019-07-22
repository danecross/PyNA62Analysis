#include "GigaTrackerDAQBoardTrailerTwo.hh" 
#include "RG_Utilities.hh"
#include <iomanip>
#include "GigaTrackerErrorsHandler.hh"

namespace GTK
{

  const int GigaTrackerDAQBoardTrailerTwo::buffLength  ; 

  const u32 GigaTrackerDAQBoardTrailerTwo::iTag = 0;
  const u32 GigaTrackerDAQBoardTrailerTwo::mTag = 0xF0000000;
  const u32 GigaTrackerDAQBoardTrailerTwo::sTag = 28;
  const u32 GigaTrackerDAQBoardTrailerTwo::rTag = 0xF;
  
  const u32 GigaTrackerDAQBoardTrailerTwo::iChipID = 0;
  const u32 GigaTrackerDAQBoardTrailerTwo::mChipID = 0x0F000000;
  const u32 GigaTrackerDAQBoardTrailerTwo::sChipID = 24;

  const u32 GigaTrackerDAQBoardTrailerTwo::iHitNbFromPrev = 0;
  const u32 GigaTrackerDAQBoardTrailerTwo::mHitNbFromPrev = 0x00FFFF00;
  const u32 GigaTrackerDAQBoardTrailerTwo::sHitNbFromPrev = 8;

  const u32 GigaTrackerDAQBoardTrailerTwo::iTriggerType = 0;
  const u32 GigaTrackerDAQBoardTrailerTwo::mTriggerType = 0x000000FF;
  const u32 GigaTrackerDAQBoardTrailerTwo::sTriggerType = 0;

  // timestamp and sob specific
  const u32 GigaTrackerDAQBoardTrailerTwo::EOBTriggerType = 0x8C;
  const u32 GigaTrackerDAQBoardTrailerTwo::iHitNb = 1;
  const u32 GigaTrackerDAQBoardTrailerTwo::mHitNb = 0xFF000000;
  const u32 GigaTrackerDAQBoardTrailerTwo::sHitNb = 24;

  const u32 GigaTrackerDAQBoardTrailerTwo::iL1ANb = 1;
  const u32 GigaTrackerDAQBoardTrailerTwo::mL1ANb = 0x00FFFFFF;
  const u32 GigaTrackerDAQBoardTrailerTwo::sL1ANb = 0;

  // eob specific
  const u32 GigaTrackerDAQBoardTrailerTwo::iErrCounter = 0;
  const u32 GigaTrackerDAQBoardTrailerTwo::mErrCounter = 0x00FF0000;
  const u32 GigaTrackerDAQBoardTrailerTwo::sErrCounter = 16;

  const u32 GigaTrackerDAQBoardTrailerTwo::iChokeCounter = 0;
  const u32 GigaTrackerDAQBoardTrailerTwo::mChokeCounter = 0x0000FF00;
  const u32 GigaTrackerDAQBoardTrailerTwo::sChokeCounter = 8;

  const u32 GigaTrackerDAQBoardTrailerTwo::iHWTriggerTimeStamp = 1;
  const u32 GigaTrackerDAQBoardTrailerTwo::mHWTriggerTimeStamp = 0xFFFFFFFF;
  const u32 GigaTrackerDAQBoardTrailerTwo::sHWTriggerTimeStamp = 0;


  //  ==============================================  
  GigaTrackerDAQBoardTrailerTwo::GigaTrackerDAQBoardTrailerTwo()
  {
    fErrorFlag      = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTrailerTwo_Flag");
    fErrorChip      = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTrailerTwo_Chip");
  }

  //  ==============================================  
  GigaTrackerDAQBoardTrailerTwo::~GigaTrackerDAQBoardTrailerTwo()
  {
   
  }

  //==============================================  
  int GigaTrackerDAQBoardTrailerTwo::SetBuffer(u8* buf){
    RG_CHECK_PTR_RETURN_INT(buf,1);
    for(int i(0); i<buffLength; i++){
      fBuffer[i] = buf[i];
    }
    //CONSISTENCY CHECK - tag must be 0x1111
    if (this->GetTag() != GigaTrackerDAQBoardTrailerTwo::rTag){
      char errBuff[500];
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "Tag is not compatible with being a TrailerTwo: %u  Ref: %u. Offending Word: %8.8x",this->GetTag(),GigaTrackerDAQBoardTrailerTwo::rTag,Word[GigaTrackerDAQBoardTrailerTwo::iTag]);
      REPORT_NEW_ERROR_RETURN(fErrorFlag,errBuff);
    }

    //CONSISTENCY CHECK - chip
    if (this->GetChipId() >9){
      char errBuff[500];
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "Invalid Chip Number %d. Offending Word: %8.8x",this->GetChipId(),Word[GigaTrackerDAQBoardTrailerTwo::iChipID]);
      REPORT_NEW_ERROR_RETURN(fErrorChip,errBuff);
    }


    return 0;
  }


  //==============================================  
  int GigaTrackerDAQBoardTrailerTwo::GetBufferLength(){
    return buffLength;
  }




  //==============================================
  u32 GigaTrackerDAQBoardTrailerTwo::GetTag()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iTag],mTag,sTag, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTrailerTwo::GetChipId()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iChipID],mChipID,sChipID, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

  //==============================================
  u32 GigaTrackerDAQBoardTrailerTwo::GetTriggerType()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iTriggerType],mTriggerType,sTriggerType, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

  //==============================================
  bool GigaTrackerDAQBoardTrailerTwo::IsEOB(){
    u32 triggertype =  GetTriggerType();
    return (triggertype == EOBTriggerType);
  }

  // timestamp and sob specific
  //==============================================
  u32 GigaTrackerDAQBoardTrailerTwo::GetHitNbFromPrev()
  {
    RG_CHECK_ZERO_RETURN(IsEOB(),"Trailer is EOB type, function does NOT apply");
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iHitNbFromPrev], mHitNbFromPrev, sHitNbFromPrev, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

  //==============================================
  u32 GigaTrackerDAQBoardTrailerTwo::GetHitNb()
  {
    RG_CHECK_ZERO_RETURN(IsEOB(),"Trailer is EOB type, function does NOT apply");
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iHitNb],mHitNb,sHitNb, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTrailerTwo::GetL1ANb()
  {
    RG_CHECK_ZERO_RETURN(IsEOB(),"Trailer is EOB type, function does NOT apply");
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iL1ANb],mL1ANb,sL1ANb, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

  // eob specific
  //==============================================
  u32 GigaTrackerDAQBoardTrailerTwo::GetErrCounter()
  {
    RG_CHECK_ZERO_RETURN( (!IsEOB()) ,"Trailer is NOT EOB type, function does NOT apply");
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iErrCounter],mErrCounter,sErrCounter, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTrailerTwo::GetChokeCounter()
  {
    RG_CHECK_ZERO_RETURN( (!IsEOB()) ,"Trailer is NOT EOB type, function does NOT apply");
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iChokeCounter],mChokeCounter,sChokeCounter, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

 //==============================================
  u32 GigaTrackerDAQBoardTrailerTwo::GetHWTriggerTimeStamp()
  {
    RG_CHECK_ZERO_RETURN( (!IsEOB()) ,"Trailer is NOT EOB type, function does NOT apply");
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iHWTriggerTimeStamp],mHWTriggerTimeStamp,sHWTriggerTimeStamp, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }



  //==============================================
  std::ostream& operator<< (std::ostream &out, GigaTrackerDAQBoardTrailerTwo & t){
    u32* pU32;
    pU32 = (u32*) &(t.fBuffer[0]);
    for(int i(0); i<t.GetBufferLength()/4;i++){
      out << "  0x"<< std::hex<<std::setfill('0')<<std::setw(8)<<pU32[i]<<"\n"<<std::dec;
    }

    out<<"   Tag                :"<<t.GetTag()<<"\n";
    out<<"   ChipID             :"<<t.GetChipId()<<"\n";
    out<<"   TriggerType        :"<<t.GetTriggerType()<<" (EOB? "<< t.IsEOB()<<")\n";

    if(t.IsEOB()){
      out<<"   ErrCounter         :"<<t.GetErrCounter()<<"\n";
      out<<"   ChokeCounter       :"<<t.GetChokeCounter()<<"\n";
      out<<"   HWTriggerTimeStamp :"<<t.GetHWTriggerTimeStamp()<<"\n";

    }
    else{
      out<<"   HitNb              :"<<t.GetHitNb()<<"\n";
      out<<"   L1ANb              :"<<t.GetL1ANb();
    }

    return out;
  }

}//~namespace IImaS


