#include "GigaTrackerNa62DataBlock.hh" 
#include "RG_Utilities.hh"
#include "GigaTrackerErrorsHandler.hh"
#include "GigaTrackerNa62HeaderL0.hh"
#include "GigaTrackerNa62HeaderL1.hh"
#include <string.h>

namespace GTK
{
  //  ==============================================  
  GigaTrackerNa62DataBlock::GigaTrackerNa62DataBlock( int format )
  {
    fDataFormat = format;
    if(format == 0 )       fEvtHeader =  new GigaTrackerNa62HeaderL0(); //2015 L0
    else if (format == 1 || format == 2 ) fEvtHeader =  new GigaTrackerNa62HeaderL1(); // 2016 L1
    else RG_REPORT_ERROR_MSG("Data format type is invalid, must be 1 (L1) or 0 (L0)");
    fGigaTrackerHitsBlock = new GigaTrackerHitsBlock(format);
    fErrorIncNHitLength      = GigaTrackerErrorsHandler::GetInstance()->RegisterError("GigaTrackerDAQBoardTrailerOne_ErrorIncNHitLength");
  }

  //  ==============================================  
  GigaTrackerNa62DataBlock::~GigaTrackerNa62DataBlock()
  {
    //clear memory
    delete fEvtHeader;
    delete fGigaTrackerHitsBlock;
  }

  // ==============================================  
  int GigaTrackerNa62DataBlock::ReadFromBuffer(u8* buffer, int& newoffset){
    Clear();

    int rv(0);
    int hLength(0);
    int evtLength(0);
    bool eob(0);

    //read header
    rv = fEvtHeader->SetBuffer(buffer);
    if(rv!=0) REPORT_ERROR_RETURN(rv,"Unable to set buffer");

    hLength = fEvtHeader->GetBufferLength();
    evtLength = fEvtHeader->GetByteNb();
    newoffset = evtLength; //in byte
    if(fDataFormat == 2) eob = static_cast<GigaTrackerNa62HeaderL1*>(fEvtHeader)->IsEoB();
    //std::cout<<"DataBlock: Trigger: "<< ((GigaTrackerNa62HeaderL1*)fEvtHeader)->GetL0TriggerWord()<<" EOB?:"<<eob<<std::endl;
    //read hits
    if( int(fEvtHeader->GetByteNb()) < fEvtHeader->GetBufferLength() ){
      char errBuff[500]; 
      sprintf(errBuff, "Evt length shorter then header length: %d vs %d", int(fEvtHeader->GetByteNb()) ,fEvtHeader->GetBufferLength());
      REPORT_NEW_ERROR_RETURN(fErrorIncNHitLength,errBuff);
    }

    rv = fGigaTrackerHitsBlock->ReadMultiEvent(buffer + hLength, evtLength - hLength, eob);
    REPORT_ERROR_RETURN(rv,"Cannot read HitsBlock");
    return rv;
  }

  //==============================================  
  GigaTrackerNa62HeaderBase* GigaTrackerNa62DataBlock::GetHeader(){
    return fEvtHeader;    
  }

  //==============================================
  GigaTrackerHitsBlock* GigaTrackerNa62DataBlock::GetGigaTrackerHitsBlock(){
    return fGigaTrackerHitsBlock;
  }

  //==============================================  
  void GigaTrackerNa62DataBlock::Clear(){
    fGigaTrackerHitsBlock->Clear();
    fEvtHeader->Clear();
  }



  //==============================================
  std::ostream& operator<< (std::ostream &out, GigaTrackerNa62DataBlock & b){
    //printer header
    out<< (*b.fEvtHeader);
    out<< (*b.fGigaTrackerHitsBlock);
    return out;
  }


}//~namespace IImaS


