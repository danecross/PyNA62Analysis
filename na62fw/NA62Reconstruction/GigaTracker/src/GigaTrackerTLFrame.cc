#include "GigaTrackerTLFrame.hh" 
#include "GigaTrackerTLMasks.hh"
#include "RG_Utilities.hh"
#include <algorithm>

namespace GTK
{

  GigaTrackerTLFrame::GigaTrackerTLFrame() :
    fSourceId(0),
    fStationId(0),
    fDelay(.0),
    fFrameCounter(0),
    fSensorUpstream(false)
  {
  }

  GigaTrackerTLFrame::~GigaTrackerTLFrame()
  {
   
  }


  void GigaTrackerTLFrame::SetSourceId(unsigned int id){
    fSourceId = id;
  }

  unsigned int GigaTrackerTLFrame::GetSourceId()const{
    return fSourceId;
  }

  void GigaTrackerTLFrame::SetStationId(unsigned int id){
    fStationId = id;
  }

  unsigned int GigaTrackerTLFrame::GetStationId()const{
    return fStationId;
  }


  void GigaTrackerTLFrame::SetFrameCounter(unsigned int n){
    fFrameCounter = n;
  }

  unsigned int GigaTrackerTLFrame::GetFrameCounter()const{
    return fFrameCounter;
  }



  unsigned int GigaTrackerTLFrame::GetNHits()const{
    return fHits.size();
  }


  void GigaTrackerTLFrame::SetDelay(float d){
    fDelay = d;
  }

  float GigaTrackerTLFrame::GetDelay()const{
    return fDelay;
  }

  void GigaTrackerTLFrame::SetSensorUpstream(bool v){
    fSensorUpstream = v;
  }

  bool GigaTrackerTLFrame::GetSensorUpstream()const{
    return fSensorUpstream;
  }


  void GigaTrackerTLFrame::ReadFromBuffer(unsigned int* Buff, std::vector<GigaTrackerTLHit>&  Hits, unsigned int& OffSet){
    //    f = GigaTrackerTLMask::GetUInt(Buff[0],GigaTrackerTLMask::m_station_X,GigaTrackerTLMask::s_station_X);
    DBG(printf("    0x%08x = %u\n",Buff[OffSet],Buff[OffSet]));
    DBG(printf("    0x%08x = %u\n\n",Buff[OffSet+1],Buff[OffSet+1]));

    fFrameCounter = GigaTrackerTLMask::GetUInt(Buff[0+OffSet],GigaTrackerTLMask::m_frame_FrameCounter,GigaTrackerTLMask::s_frame_FrameCounter);
    unsigned int NHits = GigaTrackerTLMask::GetUInt(Buff[1+OffSet],GigaTrackerTLMask::m_frame_NHits,GigaTrackerTLMask::s_frame_NHits);

    DBG(printf("    Frame Counter: %u\n",fFrameCounter));
    DBG(printf("    NHits        : %u\n",NHits));
    OffSet+=2;
     for (unsigned int i(0); i<NHits;i++){
       GigaTrackerTLHit Hit;
       Hit.SetSourceId(fSourceId);
       Hit.SetStationId(fStationId);
       Hit.SetDelay(fDelay);
       Hit.SetSensorUpstream(fSensorUpstream);
       Hit.SetFrameCounter(fFrameCounter);
       Hit.ReadFromBuffer(Buff,OffSet);
       Hits.push_back(Hit);
     }
  }

  void GigaTrackerTLFrame::WriteToBuffer(unsigned int* Buff, unsigned int& OffSet){

    unsigned int NHits = fHits.size();
    DBG(printf("   FRAME\n"));
    DBG(printf("    Offset    : %u\n",OffSet ));
    DBG(printf("    Frame Counter: %u\n",fFrameCounter));
    DBG(printf("    NHits        : %u\n",NHits));

    GigaTrackerTLMask::SetUInt(Buff+OffSet,fFrameCounter,GigaTrackerTLMask::m_frame_FrameCounter,GigaTrackerTLMask::s_frame_FrameCounter);
    GigaTrackerTLMask::SetUInt(Buff+OffSet+1,NHits,GigaTrackerTLMask::m_frame_NHits,GigaTrackerTLMask::s_frame_NHits);

    DBG(printf("    0x%08x = %u\n",Buff[OffSet],Buff[OffSet]));
    DBG(printf("    0x%08x = %u\n\n",Buff[OffSet+1],Buff[OffSet+1]));

    OffSet += 2;
    for(unsigned int i(0); i<NHits; i++){
      fHits[i].WriteToBuffer(Buff,OffSet);
    }
    return;
  }

  unsigned int GigaTrackerTLFrame::Add(GigaTrackerTLHit &hit){
    DBG(printf("          NHit in Frame %lu\n",fHits.size()));
    if(fHits.size()==0){
      fSourceId = hit.GetSourceId();
      fStationId = hit.GetStationId();
      fFrameCounter = hit.GetFrameCounter();
      fDelay = hit.GetDelay();
      fSensorUpstream = hit.GetSensorUpstream();
      DBG(printf("          Setting Source Id      :%u\n",fSourceId));
      DBG(printf("          Setting Station Id     :%u\n",fStationId));
      DBG(printf("          Setting SensorUpstream :%u\n",fSensorUpstream));
      DBG(printf("          Setting Frame Counter  :%u\n",fFrameCounter));
      //????delay???? FIXME
    }
    else if( fFrameCounter != hit.GetFrameCounter()) RG_REPORT_ERROR_MSG("Mixing hits frome different frames");
    fHits.push_back(hit);
    unsigned int NNewBytes = GigaTrackerTLMask::l_word * GigaTrackerTLMask::l_hit;
    return NNewBytes;

  }

  std::ostream & operator<<(std::ostream & os, 
				   const GigaTrackerTLFrame & frame){

    os<<"    --------- Frame ---------\n";
    os<<"      Frame Counter :"<< frame.GetFrameCounter() <<"-------\n";
    os<<"      Nb Hits       :"<< frame.GetNHits()<<"\n";
    for(unsigned int i(0);i<frame.GetNHits();i++) {os << frame.fHits[i] <<"\n";}
    return os;

  }

  bool operator == (const GTK::GigaTrackerTLFrame & f1, const GTK::GigaTrackerTLFrame & f2){
    unsigned int  fc1 = f1.GetFrameCounter();
    unsigned int  fc2 = f2.GetFrameCounter();
    return fc1==fc2;
  }

  bool operator == (const GTK::GigaTrackerTLFrame & f1, unsigned int fc2){
    unsigned int  fc1 = f1.GetFrameCounter();
    return fc1==fc2;
  }

}//~namespace IImaS


