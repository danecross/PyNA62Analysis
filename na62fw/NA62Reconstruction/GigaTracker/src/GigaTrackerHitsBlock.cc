#include "GigaTrackerHitsBlock.hh"
#include "RG_Utilities.hh"
#include <cstdio> //sprintf
#include <algorithm>    // std::sort
#include "GigaTrackerErrorsHandler.hh"

namespace GTK
{


  //==============================================
  bool GigaTrackerHitsBlock::CompareTSChipID(GigaTrackerDAQBoardTimeStamp* a, GigaTrackerDAQBoardTimeStamp* b){
    return (a->GetChipId()) < (b->GetChipId());
  }

  // ==============================================
  GigaTrackerHitsBlock::GigaTrackerHitsBlock(int format)
  {
    fDataFormat = format;
    fSorted = false;
  }

  // ==============================================
  GigaTrackerHitsBlock::~GigaTrackerHitsBlock()
  {
    int rv;
    rv = Clear();
    if(rv!=0) RG_REPORT_ERROR_MSG("Cannot clear memomry");

  }

  // ==============================================
  int  GigaTrackerHitsBlock::Clear(){


    std::vector<GigaTrackerDAQBoardTimeStamp*>::iterator  itTS =  fvpTimeStamp.begin();
    std::vector<GigaTrackerDAQBoardTrailerOne*>::iterator itT1 =  fvpTrailerOne.begin();
    std::vector<GigaTrackerDAQBoardTrailerTwo*>::iterator itT2 =  fvpTrailerTwo.begin();
    std::vector<GigaTrackerDAQBoardTrailerThree*>::iterator itT3 =  fvpTrailerThree.begin();

    for(;itTS!=fvpTimeStamp.end();++itTS)  delete *itTS;
    for(;itT1!=fvpTrailerOne.end();++itT1) delete *itT1;
    for(;itT2!=fvpTrailerTwo.end();++itT2) delete *itT2;
    for(;itT3!=fvpTrailerThree.end();++itT3) delete *itT3;

    fvpTimeStamp.clear();
    fvpTrailerOne.clear();
    fvpTrailerTwo.clear();
    fvpTrailerThree.clear();

    return 0;
  }


  // ==============================================
  int GigaTrackerHitsBlock::ReadSingleEvent(u8* buffer, int & offset, bool eob){

    int rv(0);
    //first read trailers
    if(fDataFormat==2 && eob){
      GigaTrackerDAQBoardTrailerThree* TrailerThree = new GigaTrackerDAQBoardTrailerThree();
      offset = offset - TrailerThree->GetBufferLength();
      rv = TrailerThree->SetBuffer( buffer + offset );
      if(rv!=0) delete TrailerThree;
      REPORT_ERROR_RETURN(rv,"TrailerThree buffer cannot be set");
      fvpTrailerThree.push_back(TrailerThree);
    }

    GigaTrackerDAQBoardTrailerTwo* TrailerTwo = new GigaTrackerDAQBoardTrailerTwo();
    offset = offset - TrailerTwo->GetBufferLength();
    rv = TrailerTwo->SetBuffer( buffer + offset );
    if(rv!=0) delete TrailerTwo;
    REPORT_ERROR_RETURN(rv,"TrailerTwo buffer cannot be set");
    fvpTrailerTwo.push_back(TrailerTwo);
    //chipID = TrailerTwo->GetChipId();

    GigaTrackerDAQBoardTrailerOne* TrailerOne = new GigaTrackerDAQBoardTrailerOne();
    offset = offset - TrailerOne->GetBufferLength();
    rv = TrailerOne->SetBuffer( buffer + offset );
    if(rv!=0) delete TrailerOne;
    REPORT_ERROR_RETURN(rv,"TrailerOne buffer cannot be set");
    fvpTrailerOne.push_back(TrailerOne);

    if(TrailerTwo->IsEOB())  return rv;

    //read hits
    u32 nHits = TrailerTwo->GetHitNb();
    for (u32 i(0); i<nHits;i++){
      GigaTrackerDAQBoardTimeStamp* ts = new GigaTrackerDAQBoardTimeStamp();
      offset = offset - ts->GetBufferLength();
      ts->SetTrailerOne(TrailerOne);
      ts->SetTrailerTwo(TrailerTwo);
      rv = ts->SetBuffer( buffer + offset );
      if(rv!=0) delete ts;
      REPORT_ERROR_RETURN(rv,"Timestamp buffer cannot be set");
      fvpTimeStamp.push_back(ts);
    }


    return rv;

  }

  // ==============================================
  int GigaTrackerHitsBlock::ReadMultiEvent(u8* buffer, u32 bufflength, bool eob){
    int rv(0);
    int offset = bufflength;

    while(offset>0){
      rv = ReadSingleEvent(buffer, offset, eob);
      REPORT_ERROR_RETURN(rv,"Cannot read event");
    }
    return rv;
  }

  //==============================================
  int GigaTrackerHitsBlock::SortByChipID(){
    if (fSorted == true) return 0;
    std::sort(fvpTimeStamp.begin(), fvpTimeStamp.end(),CompareTSChipID);
    fSorted = true;
    return 0;
  }

  //==============================================
  std::vector<GigaTrackerDAQBoardTimeStamp*>   GigaTrackerHitsBlock::GetTimeStamps(){
    return fvpTimeStamp;
  }
  //==============================================
  std::vector<GigaTrackerDAQBoardTimeStamp*>   GigaTrackerHitsBlock::GetTimeStamps(u32 chip){
    SortByChipID();
    std::vector<GigaTrackerDAQBoardTimeStamp*>::iterator itA = fvpTimeStamp.begin();
    while(itA!= fvpTimeStamp.end() && (*itA)->GetChipId() != chip) ++itA;

    std::vector<GigaTrackerDAQBoardTimeStamp*>::iterator itB = itA;
    while(itB!= fvpTimeStamp.end() && (*itB)->GetChipId() == chip) ++itB;

    return std::vector<GigaTrackerDAQBoardTimeStamp*>(itA,itB);
  }
  //==============================================
  std::vector<GigaTrackerDAQBoardTrailerOne*>  GigaTrackerHitsBlock::GetTrailersOne(){
    return fvpTrailerOne;
  }
  //==============================================
  std::vector<GigaTrackerDAQBoardTrailerTwo*>  GigaTrackerHitsBlock::GetTrailersTwo(){
    return fvpTrailerTwo;
  }
  //==============================================
  std::vector<GigaTrackerDAQBoardTrailerThree*>  GigaTrackerHitsBlock::GetTrailersThree(){
    return fvpTrailerThree;
  }

  //==============================================
  std::ostream& operator<< (std::ostream &out, GigaTrackerHitsBlock & b){


    //print the time stamps
    std::vector<GigaTrackerDAQBoardTimeStamp*>::iterator itTS;
    out<<"\nTime Stamps: \n";
    if(b.fvpTimeStamp.size()==0) out<< "  none\n";
    for(itTS = b.fvpTimeStamp.begin(); itTS != b.fvpTimeStamp.end();++itTS){
      out<<(**itTS)<<"\n";
    }

    std::vector<GigaTrackerDAQBoardTrailerOne*>::iterator itT1;
    out<<"\nTrailers One: \n";
    for(itT1 = b.fvpTrailerOne.begin(); itT1 != b.fvpTrailerOne.end();++itT1){
      out<<(**itT1)<<"\n";
    }

    std::vector<GigaTrackerDAQBoardTrailerTwo*>::iterator itT2;
    out<<"\nTrailers Two: \n";
    for(itT2 = b.fvpTrailerTwo.begin(); itT2 != b.fvpTrailerTwo.end();++itT2){
      out<<(**itT2)<<"\n";
    }

    std::vector<GigaTrackerDAQBoardTrailerThree*>::iterator itT3;
    out<<"\nTrailers Three: \n";
    for(itT3 = b.fvpTrailerThree.begin(); itT3 != b.fvpTrailerThree.end();++itT3){
      out<<(**itT3)<<"\n";
    }

    return out;
  }





}//~namespace IImaS


