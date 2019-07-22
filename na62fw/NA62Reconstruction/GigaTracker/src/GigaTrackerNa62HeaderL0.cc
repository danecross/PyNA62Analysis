#include "GigaTrackerNa62HeaderL0.hh" 
#include "RG_Utilities.hh"
#include <iomanip>
#include <string.h> //memset
#include "GigaTrackerErrorsHandler.hh"

namespace GTK
{

  //initialise static constant
  const int GigaTrackerNa62HeaderL0::buffLength ; //value initialised in the header

  const u32 GigaTrackerNa62HeaderL0::iFlag = 0;
  const u32 GigaTrackerNa62HeaderL0::mFlag = 0xFF000000;
  const u32 GigaTrackerNa62HeaderL0::sFlag = 24;
  const u32 GigaTrackerNa62HeaderL0::rFlag = 0x01;
  
  const u32 GigaTrackerNa62HeaderL0::iSourceSubID = 0;
  const u32 GigaTrackerNa62HeaderL0::mSourceSubID = 0x00FF0000;
  const u32 GigaTrackerNa62HeaderL0::sSourceSubID = 16;
  
  const u32 GigaTrackerNa62HeaderL0::iByteNb = 0;
  const u32 GigaTrackerNa62HeaderL0::mByteNb = 0x0000FFFF;
  const u32 GigaTrackerNa62HeaderL0::sByteNb = 0;
  
  const u32 GigaTrackerNa62HeaderL0::iTimeStamp = 1;
  const u32 GigaTrackerNa62HeaderL0::mTimeStamp = 0xFFFFFFFF;
  const u32 GigaTrackerNa62HeaderL0::sTimeStamp = 0;


  //  ==============================================  
  GigaTrackerNa62HeaderL0::GigaTrackerNa62HeaderL0()
  {
    fErrorFlag      = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerNa62HeaderL0_ErrorFlag");
  }

  //  ==============================================  
  GigaTrackerNa62HeaderL0::~GigaTrackerNa62HeaderL0()
  {
   
  }


  //  ==============================================  
  int  GigaTrackerNa62HeaderL0::Clear(){
    memset(fBuffer, 0, sizeof(buffLength));
    return 0;
  }

 //==============================================  
  int GigaTrackerNa62HeaderL0::SetBuffer(u8* buf){
    RG_CHECK_PTR_RETURN_INT(buf,1);
    for(int i(0); i<buffLength; i++){
      fBuffer[i]=buf[i];
    }
    //CONSISTENCY CHECK - flag must be 0x00000001
    if (this->GetFlag() != GigaTrackerNa62HeaderL0::rFlag){
      char errBuff[500];
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "Flag is not compatible with being a NA62Header: %u  Ref: %u. Offending Word: %8.8x",this->GetFlag(),GigaTrackerNa62HeaderL0::rFlag,Word[GigaTrackerNa62HeaderL0::iFlag]);
      REPORT_NEW_ERROR_RETURN(fErrorFlag,errBuff);
    }



    return 0;
  }

  //==============================================  
  int GigaTrackerNa62HeaderL0::GetBufferLength(){
    return buffLength;
  }

  //==============================================
  u32 GigaTrackerNa62HeaderL0::GetFlag()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iFlag],mFlag,sFlag, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerNa62HeaderL0::GetSourceSubID()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iSourceSubID],mSourceSubID,sSourceSubID, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerNa62HeaderL0::GetByteNb()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iByteNb],mByteNb,sByteNb, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }
  //==============================================
  u32 GigaTrackerNa62HeaderL0::GetTimeStamp()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iTimeStamp],mTimeStamp,sTimeStamp, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }


  // ==============================================
  void GigaTrackerNa62HeaderL0::Print(std::ostream & out){
    out<<"Header Type: L0\n";
    out<< "   Flags         : "<<GetFlag()<<"\n";
    out<< "   SubDetectorId : "<<GetSourceSubID()<<"\n";
    out<< "   Nb Byte       : "<<GetByteNb()<<"\n";
    out<< "   Timestamp     : "<<GetTimeStamp()<<"\n";
    return;
  }

  /*
  //==============================================
  std::ostream& operator<< (std::ostream &out, GigaTrackerNa62HeaderL0 & h){
    out<<"Na62Header: \n";
    u32* pU32 =  reinterpret_cast<u32*>(h.fBuffer);
    //    pU32 = (u32*) &(h.fBuffer[0]);a
    for(int i(0); i<(h.GetBufferLength()/4); i++){
      printf    ("  0x%8.8x\n",pU32[i]);
     }
    out<< "   Flags         : "<<h.GetFlag()<<"\n";
    out<< "   SubDetectorId : "<<h.GetSourceSubID()<<"\n";
    out<< "   Nb Byte       : "<<h.GetByteNb()<<"\n";
    out<< "   Timestamp     : "<<h.GetTimestamp()<<"\n";
    return out;
  */


  }

//~namespace IImaS


