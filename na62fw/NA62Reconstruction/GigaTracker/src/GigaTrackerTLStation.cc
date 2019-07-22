#include "GigaTrackerTLStation.hh" 
#include "GigaTrackerTLMasks.hh"
#include <algorithm>
#include "RG_Utilities.hh"

namespace GTK
{

  GigaTrackerTLStation::GigaTrackerTLStation() :
    fSourceId(0),
    fSensorUpstream(0),
    fStationId(0),
    fNFrames(0),
    fDelay(.0)
  {
  }

  GigaTrackerTLStation::~GigaTrackerTLStation()
  {
   
  }

  void GigaTrackerTLStation::SetSourceId(unsigned int id){
    fSourceId = id;
  }

  unsigned int GigaTrackerTLStation::GetSourceId()const{
    return fSourceId;
  }

  void GigaTrackerTLStation::SetStationId(unsigned int id){
    fStationId = id;
  }

  unsigned int GigaTrackerTLStation::GetStationId()const{
    return fStationId;
  }


  void GigaTrackerTLStation::SetNFrames(unsigned int n){
    fNFrames = n;
  }

  unsigned int GigaTrackerTLStation::GetNFrames()const{
    return fNFrames;
  }


  void GigaTrackerTLStation::SetDelay(float d){
    fDelay = d;
  }

  float GigaTrackerTLStation::GetDelay()const{
    return fDelay;
  }

  void GigaTrackerTLStation::SetSensorUpstream(bool v){
    fSensorUpstream = v;
  }

  bool GigaTrackerTLStation::GetSensorUpstream()const{
    return fSensorUpstream;
  }



  void GigaTrackerTLStation::ReadFromBuffer(unsigned int* Buff, std::vector<GigaTrackerTLHit>& Hits, unsigned int& OffSet){

    DBG(printf("  0x%08x\n",Buff[OffSet]));
    DBG(printf("  0x%08x\n",Buff[OffSet+1]));

    fSensorUpstream = GigaTrackerTLMask::GetUInt(Buff[0+OffSet],GigaTrackerTLMask::m_station_SensorUpstream,GigaTrackerTLMask::s_station_SensorUpstream);
    fStationId = GigaTrackerTLMask::GetUInt(Buff[0+OffSet],GigaTrackerTLMask::m_station_StationId,GigaTrackerTLMask::s_station_StationId);
    fNFrames = GigaTrackerTLMask::GetUInt(Buff[0+OffSet],GigaTrackerTLMask::m_station_NFrames,GigaTrackerTLMask::s_station_NFrames);
    fDelay = GigaTrackerTLMask::GetFloat(Buff[1+OffSet]);


    DBG(printf("  Station Id     : %u\n",fStationId));
    DBG(printf("  SensorUpstream : %u\n",fSensorUpstream));
    DBG(printf("  NFrames        : %u\n",fNFrames));
    DBG(printf("  NDelay         : %f\n",fDelay));

    OffSet+=2;
    for(unsigned int i(0); i<fNFrames; i++){
      GigaTrackerTLFrame Frame;
      Frame.SetSourceId(fSourceId);
      Frame.SetStationId(fStationId);
      Frame.SetDelay(fDelay);
      Frame.SetSensorUpstream(fSensorUpstream);
      Frame.ReadFromBuffer(Buff, Hits, OffSet);
    }
    
  }


  void GigaTrackerTLStation::WriteToBuffer(unsigned int* Buff, unsigned int& OffSet){

    DBG(printf(" Station\n" ));
    DBG(printf("  Offset        : %u\n",OffSet ));
    DBG(printf("  Station Id    : %u\n",fStationId));
    DBG(printf("  SensorUpstream: %u\n",fSensorUpstream));
    DBG(printf("  NFrames       : %u\n",fNFrames));
    DBG(printf("  NDelay        : %f\n",fDelay));

    GigaTrackerTLMask::SetUInt(Buff+OffSet,fSensorUpstream,GigaTrackerTLMask::m_station_SensorUpstream,GigaTrackerTLMask::s_station_SensorUpstream);
    GigaTrackerTLMask::SetUInt(Buff+OffSet,fStationId,GigaTrackerTLMask::m_station_StationId,GigaTrackerTLMask::s_station_StationId);
    GigaTrackerTLMask::SetUInt(Buff+OffSet,fNFrames,GigaTrackerTLMask::m_station_NFrames,GigaTrackerTLMask::s_station_NFrames);
    GigaTrackerTLMask::SetFloat(Buff+OffSet+1,fDelay); 

    DBG(printf("  0x%08x\n",Buff[OffSet]));
    DBG(printf("  0x%08x\n",Buff[OffSet+1]));

    OffSet = OffSet+2;
    for(unsigned int i(0); i<fNFrames; i++){
      fFrames[i].WriteToBuffer(Buff,OffSet);
    }
    return;    
    
  }

  unsigned int GigaTrackerTLStation::Add(GigaTrackerTLHit &hit){
    if(fFrames.size() == 0 ){
    fSourceId = hit.GetSourceId();
    fStationId = hit.GetStationId();
    fDelay = hit.GetDelay();
    fSensorUpstream = hit.GetSensorUpstream();
    }
    else if(fSourceId != hit.GetSourceId() || fStationId != hit.GetStationId()){
      RG_REPORT_ERROR_MSG("Mixing hits from different stations or detectors");
    }

    unsigned int NNewBytes = 0;
    unsigned int FrameCounter = hit.GetFrameCounter();

    DBG(printf("    Looking in %lu Frames\n",fFrames.size()));
    for(unsigned int i(0); i<fFrames.size(); i++) DBG(printf("    ->Frame %u\n",fFrames[i].GetFrameCounter()));

    std::vector < GigaTrackerTLFrame >::iterator iF =  fFrames.begin();
    while( iF !=  fFrames.end() && (*iF).GetFrameCounter()!=FrameCounter ) ++iF;
    if (iF == fFrames.end()){
      DBG(printf("      Adding Frame %u in Station\n",FrameCounter ));
      GigaTrackerTLFrame Frame;
      DBG(printf("        Adding Hit in Frame %u\n", FrameCounter));
      NNewBytes = Frame.Add(hit) + GigaTrackerTLMask::l_word * GigaTrackerTLMask::l_frame;
      DBG(printf("         Frame Counter is set to %u\n",Frame.GetFrameCounter() ));
      fFrames.push_back(Frame);
      fNFrames++;
    }
    else  {
      NNewBytes = iF->Add(hit);
      DBG(printf("        Adding Hit in Frame %u\n", iF->GetFrameCounter()));
    }
    
    return NNewBytes;
  }

  std::ostream & operator<<(std::ostream & os, 
				   const GigaTrackerTLStation & station){

    os<<"  --------- Gtk Station "<< station.GetStationId() <<"-------\n";
    os<<"    Nb Frames     : "<<station.GetNFrames()<<"\n";
    os<<"    Delay wrt L0  : "<<station.GetDelay()<<"\n";
    os<<"    SensorUpstream: "<<station.GetSensorUpstream()<<"\n";
    for(unsigned int i(0);i<station.GetNFrames();i++) {os << station.fFrames[i] <<"\n";}
    return os;

  }
  
  bool operator == (const GigaTrackerTLStation & s1, const GigaTrackerTLStation & s2){
    unsigned int id1 = s1.GetStationId();
    unsigned int id2 = s2.GetStationId();
    return id1==id2;
  }
  bool operator == (const GigaTrackerTLStation & s1, unsigned int id2){
    unsigned int id1 = s1.GetStationId();
    return id1==id2;
  }

}//~namespace IImaS


