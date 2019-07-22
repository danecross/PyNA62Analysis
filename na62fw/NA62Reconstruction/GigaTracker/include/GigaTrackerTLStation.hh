#ifndef GigaTrackerTLStation_HH_
#define GigaTrackerTLStation_HH_
//#include "GTKTypeDefs.hh"
#include <iostream>
#include "GigaTrackerTLHit.hh"
#include "GigaTrackerTLFrame.hh"
#include <vector>

namespace GTK
{

  class GigaTrackerTLStation
  {
  public:
    GigaTrackerTLStation();
    virtual ~GigaTrackerTLStation();

    void ReadFromBuffer(unsigned int* Buff, std::vector<GigaTrackerTLHit>& Hits, unsigned int & OffSet);
    void WriteToBuffer(unsigned int* Buff, unsigned int& OffSet);

    void SetSourceId(unsigned int);
    unsigned int GetSourceId()const;

    void SetStationId(unsigned int);
    unsigned int GetStationId()const;

    void SetNFrames(unsigned int);
    unsigned int GetNFrames()const;

    void SetDelay(float);
    float GetDelay()const;

    bool GetSensorUpstream()const;
    void SetSensorUpstream(bool);

    unsigned int Add(GigaTrackerTLHit &hit);


    friend bool operator == (const GigaTrackerTLStation & s1, const GigaTrackerTLStation & s2);
    friend bool operator == (const GigaTrackerTLStation & s1, unsigned int id2);

    friend std::ostream & operator<<(std::ostream & os,
				     const GigaTrackerTLStation & Station);

  protected:
    unsigned int fSourceId;
    unsigned int fSensorUpstream;
    unsigned int fStationId;
    unsigned int fNFrames;
    float fDelay;
    std::vector<GigaTrackerTLFrame> fFrames;


  };


}//~namespace IImaS

#endif//~GigaTrackerTLStation_HH_

