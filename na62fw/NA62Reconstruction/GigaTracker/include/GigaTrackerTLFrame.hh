#ifndef GigaTrackerTLFrame_HH_
#define GigaTrackerTLFrame_HH_

#include <iostream>
#include "GigaTrackerTLHit.hh"
#include <vector>



namespace GTK
{

  class GigaTrackerTLFrame
  {
  public:
    GigaTrackerTLFrame();
    virtual ~GigaTrackerTLFrame();

    void ReadFromBuffer(unsigned int* Buff, std::vector<GigaTrackerTLHit>&  Hits, unsigned int& OffSet);
    void WriteToBuffer(unsigned int* Buff, unsigned int& OffSet);

    void SetSourceId(unsigned int);
    unsigned int GetSourceId()const;

    void SetStationId(unsigned int);
    unsigned int GetStationId()const;

    void SetDelay(float);
    float GetDelay()const;

    bool GetSensorUpstream()const;
    void SetSensorUpstream(bool);

    void SetNHits(unsigned int);
    unsigned int GetNHits()const;

    void SetFrameCounter(unsigned int);
    unsigned int GetFrameCounter()const;

    unsigned int Add(GigaTrackerTLHit &hit);

    const GigaTrackerTLHit & operator[](unsigned int idx)const;

    friend bool operator == (const GigaTrackerTLFrame & f1, const GigaTrackerTLFrame & f2);
    friend bool operator == (const GigaTrackerTLFrame & f1, unsigned int f2);



  protected:

    unsigned int fSourceId;
    unsigned int fStationId;
    float fDelay;
    unsigned int fFrameCounter;
    bool fSensorUpstream;

    std::vector<GigaTrackerTLHit> fHits;

  friend std::ostream & operator<<(std::ostream & os,
			    const GTK::GigaTrackerTLFrame & Frame);

  };



}//~namespace IImaS

#endif//~GigaTrackerTLFrame_HH_

