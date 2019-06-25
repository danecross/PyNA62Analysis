#include "GigaTrackerNa62HeaderL1.hh" 
#include "RG_Utilities.hh"
#include <iomanip>
#include <string.h> //memset
#include "GigaTrackerErrorsHandler.hh"

namespace GTK
{

  //initialise static constant
  const int GigaTrackerNa62HeaderL1::buffLength; //value initialised in the header

  const u32 GigaTrackerNa62HeaderL1::iSourceID = 0;
  const u32 GigaTrackerNa62HeaderL1::mSourceID = 0xff000000;
  const u32 GigaTrackerNa62HeaderL1::sSourceID = 24;
  const u32 GigaTrackerNa62HeaderL1::rSourceID = 0x08;

  const u32 GigaTrackerNa62HeaderL1::iEvtNb = 0;
  const u32 GigaTrackerNa62HeaderL1::mEvtNb = 0x00ffffff;
  const u32 GigaTrackerNa62HeaderL1::sEvtNb = 0;

  const u32 GigaTrackerNa62HeaderL1::iWordNb = 1;
  const u32 GigaTrackerNa62HeaderL1::mWordNb = 0x0000ffff;
  const u32 GigaTrackerNa62HeaderL1::sWordNb = 0;

  const u32 GigaTrackerNa62HeaderL1::iTimeStamp = 2;
  const u32 GigaTrackerNa62HeaderL1::mTimeStamp = 0xffffffff;
  const u32 GigaTrackerNa62HeaderL1::sTimeStamp = 0;

  const u32 GigaTrackerNa62HeaderL1::iSourceSubID = 3;
  const u32 GigaTrackerNa62HeaderL1::mSourceSubID = 0x0000ffff;
  const u32 GigaTrackerNa62HeaderL1::sSourceSubID = 0;

  const u32 GigaTrackerNa62HeaderL1::iErrorBits = 3;
  const u32 GigaTrackerNa62HeaderL1::mErrorBits = 0xff000000;
  const u32 GigaTrackerNa62HeaderL1::sErrorBits = 24;

  const u32 GigaTrackerNa62HeaderL1::iL0TriggerWord = 3;
  const u32 GigaTrackerNa62HeaderL1::mL0TriggerWord = 0x00ff0000;
  const u32 GigaTrackerNa62HeaderL1::sL0TriggerWord = 16;


  //  ==============================================  
  GigaTrackerNa62HeaderL1::GigaTrackerNa62HeaderL1()
  {
    fErrorFlag      = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerNa62HeaderL1_Flag");
    fErrorSubId     = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerNa62HeaderL1_SubId");
  }

  //  ==============================================  
  GigaTrackerNa62HeaderL1::~GigaTrackerNa62HeaderL1()
  {
   
  }


  //  ==============================================  
  int  GigaTrackerNa62HeaderL1::Clear(){
    memset(fBuffer, 0, sizeof(buffLength));
    return 0;
  }

 //==============================================  
  int GigaTrackerNa62HeaderL1::SetBuffer(u8* buf){
    RG_CHECK_PTR_RETURN_INT(buf,1);
    for(int i(0); i<buffLength; i++){
      fBuffer[i]=buf[i];
    }
    //CONSISTENCY CHECK - Source must be 0x08
    if (this->GetSourceID() != GigaTrackerNa62HeaderL1::rSourceID){
      char errBuff[500];
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "Source is not compatible with being a GTK NA62HeaderL1: %u  Ref: %u. Offending Word: %8.8x",this->GetSourceID(),GigaTrackerNa62HeaderL1::rSourceID,Word[GigaTrackerNa62HeaderL1::iSourceID]);
      REPORT_NEW_ERROR_RETURN(fErrorFlag,errBuff);
    }


    //CONSISTENCY CHECK - Source-sub ID must be 0 - 6
    if (/*this->GetSourceSubID() <0 || unsigned value, always >0 by definition*/ this->GetSourceSubID()>5){
      char errBuff[500];
      u32* Word = reinterpret_cast<u32*>(buf);
      sprintf(errBuff, "SubSource is not valid: %u Offending Word: %8.8x",this->GetSourceSubID(),Word[GigaTrackerNa62HeaderL1::iSourceSubID]);
      REPORT_NEW_ERROR_RETURN(fErrorSubId,errBuff);
    }



    return 0;
  }

  //==============================================  
  int GigaTrackerNa62HeaderL1::GetBufferLength(){
    return buffLength;
  }

  //==============================================
  u32 GigaTrackerNa62HeaderL1::GetSourceID()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iSourceID],mSourceID,sSourceID, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

  //==============================================
  u32 GigaTrackerNa62HeaderL1::GetEvtNb()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iEvtNb],mEvtNb,sEvtNb, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

  //==============================================
  u32 GigaTrackerNa62HeaderL1::GetByteNb(){
    return GetWordNb()*4;
  }

  //==============================================
  u32 GigaTrackerNa62HeaderL1::GetWordNb()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iWordNb],mWordNb,sWordNb, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }


  //==============================================
  u32 GigaTrackerNa62HeaderL1::GetTimeStamp()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iTimeStamp],mTimeStamp,sTimeStamp, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }


  //==============================================
  u32 GigaTrackerNa62HeaderL1::GetSourceSubID()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iSourceSubID],mSourceSubID,sSourceSubID, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }


  //==============================================
  u32 GigaTrackerNa62HeaderL1::GetErrorBits()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iErrorBits],mErrorBits,sErrorBits, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }


  //==============================================
  u32 GigaTrackerNa62HeaderL1::GetL0TriggerWord()
  {
    u32 value;
    u32* buffer = reinterpret_cast<u32*>(fBuffer);
    int rv = GetUInt(buffer[iL0TriggerWord],mL0TriggerWord,sL0TriggerWord, value);
    RG_CHECK_ZERO_RETURN(rv,"");
    return value;
  }

  // ==============================================
  bool GigaTrackerNa62HeaderL1::IsEoB(){
    u32 tw = GetL0TriggerWord();
    //    return( (tw&0x3) == 0x2);
    return(tw == 0x23);
  }


  // ==============================================
  void GigaTrackerNa62HeaderL1::Print(std::ostream & out){
    out<<"Header Type: L1\n";
    out<< "   SourceID      : "<<GetSourceID()<<"\n";
    out<< "   Event Number  : "<<GetEvtNb()<<"\n";
    out<< "   Words Number  : "<<GetWordNb()<<"\n";
    out<< "   Timestamp     : "<<GetTimeStamp()<<"\n";
    out<< "   SourceSubID   : "<<GetSourceSubID()<<"\n";
    out<< "   Error Bits    : "<<GetErrorBits()<<"\n";
    out<< "   L0TriggerWord : "<<GetL0TriggerWord()<<"\n";
    out<<"    Nb Bytes      : "<< GetByteNb()<<"\n";
    return;
  }
}
  // ==============================================
/*
  std::ostream& operator<< (std::ostream &out, GigaTrackerNa62HeaderL1 & h){
      out<<"Na62Header: \n";
      u32* pU32 =  reinterpret_cast<u32*>(h.fBuffer);
      for(int i(0); i<(h.GetBufferLength()/4); i++){
	printf    ("  0x%8.8x\n",pU32[i]);
      }
      out<< "   SourceID      : "<<h.GetSourceID()<<"\n";
      out<< "   Event Number  : "<<h.GetEvtNb()<<"\n";
      out<< "   Words Number  : "<<h.GetWordNb()<<"\n";
      out<< "   Timestamp     : "<<h.GetTimeStamp()<<"\n";
      out<< "   SourceSubID   : "<<h.GetSourceSubID()<<"\n";
      out<< "   Error Bits    : "<<h.GetErrorBits()<<"\n";
      out<< "   L0TriggerWord : "<<h.GetL0TriggerWord()<<"\n";
      
      return out;
    }
  }*/
  //  std::ostream& operator<< (std::ostream &out, GigaTrackerNa62HeaderL1 & h){
  /*
  std::ofstream& GigaTrackerNa62HeaderL1::Print(std::ofstream & out){
    out<<"Na62Header: \n";
    u32* pU32 =  reinterpret_cast<u32*>(fBuffer);
    for(int i(0); i<(GetBufferLength()/4); i++){
      printf    ("  0x%8.8x\n",pU32[i]);
     }
    out<< "   SourceID      : "<<GetSourceID()<<"\n";
    out<< "   Event Number  : "<<GetEvtNb()<<"\n";
    out<< "   Words Number  : "<<GetWordNb()<<"\n";
    out<< "   Timestamp     : "<<GetTimeStamp()<<"\n";
    out<< "   SourceSubID   : "<<GetSourceSubID()<<"\n";
    out<< "   Error Bits    : "<<GetErrorBits()<<"\n";
    out<< "   L0TriggerWord : "<<GetL0TriggerWord()<<"\n";

    return out;



  }
  */
//~namespace IImaS


