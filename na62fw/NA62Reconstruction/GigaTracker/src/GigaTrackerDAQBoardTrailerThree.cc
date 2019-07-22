#include "GigaTrackerDAQBoardTrailerThree.hh" 
#include "RG_Utilities.hh"
#include <iomanip>
#include "GigaTrackerErrorsHandler.hh"

namespace GTK
{

  const int GigaTrackerDAQBoardTrailerThree::buffLength  ; 

  const u32 GigaTrackerDAQBoardTrailerThree::iTag = 0;
  const u32 GigaTrackerDAQBoardTrailerThree::mTag = 0xF0000000;
  const u32 GigaTrackerDAQBoardTrailerThree::sTag = 28;
  const u32 GigaTrackerDAQBoardTrailerThree::rTag = 0xE;
  
  const u32 GigaTrackerDAQBoardTrailerThree::iChipID = 0;
  const u32 GigaTrackerDAQBoardTrailerThree::mChipID = 0x0F000000;
  const u32 GigaTrackerDAQBoardTrailerThree::sChipID = 24;

  const u32 GigaTrackerDAQBoardTrailerThree::iFW = 0;
  const u32 GigaTrackerDAQBoardTrailerThree::mFW = 0x00FFFFFF;
  const u32 GigaTrackerDAQBoardTrailerThree::sFW = 0;

  const u32 GigaTrackerDAQBoardTrailerThree::iHitNb = 1;
  const u32 GigaTrackerDAQBoardTrailerThree::mHitNb = 0xFFFFFFFF;
  const u32 GigaTrackerDAQBoardTrailerThree::sHitNb = 0 ;


  //  ==============================================  
  GigaTrackerDAQBoardTrailerThree::GigaTrackerDAQBoardTrailerThree()
  {
    fErrorFlag      = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTrailerThree_Flag");
    fErrorChip      = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTrailerThree_Chip");
    memset(fBuffer,0,buffLength);
  }

  //  ==============================================  
  GigaTrackerDAQBoardTrailerThree::~GigaTrackerDAQBoardTrailerThree()
  {
   
  }

  //==============================================  
  int GigaTrackerDAQBoardTrailerThree::SetBuffer(u8* buf){
    RG_CHECK_PTR_RETURN_INT(buf,1);
    for(int i(0); i<buffLength; i++){
      fBuffer[i] = buf[i];
    }
    //CONSISTENCY CHECK - tag must be 0x1110
    if (this->GetTag() != GigaTrackerDAQBoardTrailerThree::rTag){
      char errBuff[500];
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "Tag is not compatible with being a TrailerThree : %u  Ref: %u. Offending Word: %8.8x",this->GetTag(),GigaTrackerDAQBoardTrailerThree::rTag,Word[GigaTrackerDAQBoardTrailerThree::iTag]);
      REPORT_NEW_ERROR_RETURN(fErrorFlag,errBuff);
    }

    //CONSISTENCY CHECK - chip
    if (this->GetChipId() >9){
      char errBuff[500];
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "Invalid Chip Number %d. Offending Word: %8.8x",this->GetChipId(),Word[GigaTrackerDAQBoardTrailerThree::iChipID]);
      REPORT_NEW_ERROR_RETURN(fErrorChip,errBuff);
    }

    return 0;
  }


  //==============================================  
  int GigaTrackerDAQBoardTrailerThree::GetBufferLength(){
    return buffLength;
  }




  //==============================================
  u32 GigaTrackerDAQBoardTrailerThree::GetTag()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iTag],mTag,sTag, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerDAQBoardTrailerThree::GetChipId()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iChipID],mChipID,sChipID, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

  //==============================================
  u32 GigaTrackerDAQBoardTrailerThree::GetFW()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iFW],mFW,sFW, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

  //==============================================
  u32 GigaTrackerDAQBoardTrailerThree::GetHitNb()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iHitNb],mHitNb,sHitNb, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

  //==============================================
  std::ostream& operator<< (std::ostream &out, GigaTrackerDAQBoardTrailerThree & t){
    u32* pU32;
    pU32 = (u32*) &(t.fBuffer[0]);
    for(int i(0); i<t.GetBufferLength()/4;i++){
      out << "  0x"<< std::hex<<std::setfill('0')<<std::setw(8)<<pU32[i]<<"\n"<<std::dec;
    }

    out<<"   Tag                :"<<t.GetTag()<<"\n";
    out<<"   ChipID             :"<<t.GetChipId()<<"\n";
    out<<"   FW                 :"<<t.GetFW()<<")\n";
    out<<"   HitNb              :"<<t.GetHitNb()<<"\n";

    return out;
  }

}//~namespace IImaS


