#include "GigaTrackerTLDataBlock.hh" 
#include "GigaTrackerTLMasks.hh"
#include <stdio.h>
#include <algorithm>
#include "RG_Utilities.hh"
#include <string.h>
#include "NA62Global.hh"

namespace GTK
{

  GigaTrackerTLDataBlock::GigaTrackerTLDataBlock()
  {
    fSourceId = 0;
    fNBytes = GigaTrackerTLMask::l_word * GigaTrackerTLMask::l_block; //header size
    fNStations = 0;
    printf("Constructors Data Block\n");
  }
  
  GigaTrackerTLDataBlock::~GigaTrackerTLDataBlock()
  {
  }

  void GigaTrackerTLDataBlock::SetSourceId(unsigned int id){
    fSourceId = id;
  }

  unsigned int GigaTrackerTLDataBlock::GetSourceId()const{
    return fSourceId;
  }

  void GigaTrackerTLDataBlock::SetNBytes(unsigned int n){
    fNBytes = n;
  }

  unsigned int GigaTrackerTLDataBlock::GetNBytes()const{
    return fNBytes;
  }

  void GigaTrackerTLDataBlock::SetNStations(unsigned int n){
    fNStations = n;
  }

  unsigned int GigaTrackerTLDataBlock::GetNStations()const{
    return fNStations;
  }


  void GigaTrackerTLDataBlock::Add(GigaTrackerTLHit  &hit){
    if(fStations.size()==0) fSourceId = hit.GetSourceId();
    else if(fSourceId != hit.GetSourceId()) RG_REPORT_ERROR_MSG("Mixing hits from different detectors");

    //check if there is already a station with this id
    unsigned int StationId = hit.GetStationId();
    unsigned int NNewBytes = 0;
    DBG(printf("Looking in %lu Stations\n",fStations.size()));
    for(unsigned int i(0); i<fStations.size(); i++) DBG(printf("  Station %u\n",fStations[i].GetStationId()));
    std::vector < GigaTrackerTLStation >::iterator iS =  fStations.begin();
    while( iS !=  fStations.end() && (*iS).GetStationId()!=StationId ) ++iS;

    if (iS == fStations.end()){
      DBG(printf("  Adding Station %u in Block\n", StationId));
      GigaTrackerTLStation Station;
      DBG(printf("    Adding Hit in Station %u\n", StationId));
      NNewBytes = Station.Add(hit) + GigaTrackerTLMask::l_word * GigaTrackerTLMask::l_block;
      fNStations++;
      fStations.push_back(Station);
    }
    else{
      DBG(printf("    Adding Hit in Station %u\n", iS->GetStationId()));
      NNewBytes = iS->Add(hit);
    }
    fNBytes += NNewBytes; ///FIXME   
    return;
  }
  
  //  const GigaTrackerTLStation & operator[](unsigned int idx)const;
  
  std::ostream & operator<<(std::ostream & os, 
				   const GigaTrackerTLDataBlock & block){

    os<<"--------- Data Block ---------\n";
    os<<"  Source Id  : "<<block.GetSourceId()<<"\n";
    os<<"  Nb Bytes   : "<<block.GetNBytes()<<"\n";
    os<<"  Nb Stations: "<<block.GetNStations()<<"\n";
    for(unsigned int i(0);i<block.GetNStations();i++) {os << block.fStations[i] <<"\n";}
    return os;

  }
  
  

  unsigned int GigaTrackerTLDataBlock::ReadFromBuffer(unsigned int* Buff, std::vector<GigaTrackerTLHit>& Hits){
    if(Buff == NULL){
      RG_REPORT_ERROR_MSG("Empty Buffer");
      exit(kGenericError);
    }
    DBG(printf("Decode Header:\n"));
    DBG(printf("0x%08x\n",*Buff));
    DBG(printf("0x%08x\n",*(Buff+1)));

    //read header
    fSourceId = GigaTrackerTLMask::GetUInt(Buff[0],GigaTrackerTLMask::m_block_SourceId,GigaTrackerTLMask::s_block_SourceId);
    fNBytes = GigaTrackerTLMask::GetUInt(Buff[0],GigaTrackerTLMask::m_block_NBytes,GigaTrackerTLMask::s_block_NBytes);
    fNStations = GigaTrackerTLMask::GetUInt(Buff[1],GigaTrackerTLMask::m_block_NStations,GigaTrackerTLMask::s_block_NStations);

    DBG(printf("Source Id: %u\n",fSourceId));
    DBG(printf("NBytes   : %u\n",fNBytes));
    DBG(printf("NStations: %u\n",fNStations));

    //get hits from stations
    Hits.reserve(fNBytes/4 - 2);
    unsigned int OffSet(2);
    for(unsigned int i(0); i<fNStations; i++){
      GigaTrackerTLStation Station;
      Station.SetSourceId(fSourceId);
      Station.ReadFromBuffer(Buff, Hits, OffSet);
    }
    return OffSet;
  }

  void GigaTrackerTLDataBlock::WriteToBuffer(unsigned int* Buff){
    // nota make sure the buff was allocated enought memory before 
    // FIXME
    DBG(printf("BLOCK \n"));
    DBG(printf(" Source Id: %u\n",fSourceId));
    DBG(printf(" NBytes   : %u\n",fNBytes));
    DBG(printf(" NStations: %u\n",fNStations));

    GigaTrackerTLMask::SetUInt(Buff,fSourceId, GigaTrackerTLMask::m_block_SourceId,GigaTrackerTLMask::s_block_SourceId);
    GigaTrackerTLMask::SetUInt(Buff,fNBytes,GigaTrackerTLMask::m_block_NBytes,GigaTrackerTLMask::s_block_NBytes);
    GigaTrackerTLMask::SetUInt(Buff+1,fNStations,GigaTrackerTLMask::m_block_NStations,GigaTrackerTLMask::s_block_NStations);

    DBG(printf("0x%08x\n",*Buff));
    DBG(printf("0x%08x\n",*(Buff+1)));
    //call the write to buffer of stations??
    unsigned int OffSet(2);
    for(unsigned int i(0); i<fNStations; i++){
      fStations[i].WriteToBuffer(Buff,OffSet);
    }
    return;
  }

}//~namespace IImaS


