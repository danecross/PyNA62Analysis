#ifndef GigaTrackerTLHit_HH_
#define GigaTrackerTLHit_HH_
#include <iostream>
//#include "TDCPixTimeStamp.hh"

namespace GTK
{

  class GigaTrackerTLHit
  {
  public:
    GigaTrackerTLHit();
    //    GigaTrackerTLHit(TDCPixTimeStamp TS);
    virtual ~GigaTrackerTLHit();

    void ReadFromBuffer(unsigned int*, unsigned int&);
    void WriteToBuffer(unsigned int*, unsigned int&);

    void SetSourceId(unsigned int);
    unsigned int GetSourceId()const;

    void SetStationId(unsigned int);
    unsigned int GetStationId()const;

    void SetDelay(float);
    float GetDelay()const;

    void SetFrameCounter(unsigned int);
    unsigned int GetFrameCounter()const;

    unsigned int GetChipId()const;
    void SetChipId(unsigned int );

    unsigned int GetqChipId()const;
    void SetqChipId(unsigned int);

    unsigned int GetNCollisions()const;
    void SetNCollisions(unsigned int);

    unsigned int GetPixelAddress()const;
    void SetPixelAddress(unsigned int);

    unsigned int GetHitArbiterAddress()const;
    void SetHitArbiterAddress(unsigned int);

    unsigned int GetPileUpAddress()const;
    void SetPileUpAddress(unsigned int);

    unsigned int GetLeadingSelector()const;
    void SetLeadingSelector(unsigned int);

    unsigned int GetLeadingCoarse()const;
    void SetLeadingCoarse(unsigned int);

    unsigned int GetLeadingFine()const;
    void SetLeadingFine(unsigned int);

    unsigned int GetTotSelector()const;
    void SetTotSelector(unsigned int);

    unsigned int GetTotCoarse()const;
    void SetTotCoarse(unsigned int);

    unsigned int GetTotFine()const;
    void SetTotFine(unsigned int);

    double GetLeadingTime()const;
    double GetTrailingTime()const;

    bool GetSensorUpstream()const;
    void SetSensorUpstream(bool);

    int GetPixelUID()const;

  protected:
    float fDelay;
    unsigned int fSourceId;
    unsigned int fStationId;
    unsigned int fFrameCounter;
    unsigned int fChipId;
    unsigned int fqChipId;
    unsigned int fNCollisions;

    unsigned int fPixelAddress;
    unsigned int fHitArbiterAddress;
    unsigned int fPileUpAddress;

    unsigned int fLeadingSelector;
    unsigned int fLeadingCoarse;
    unsigned int fLeadingFine;

    unsigned int fTotSelector;
    unsigned int fTotCoarse;
    unsigned int fTotFine;

    bool fSensorUpstream;

  };

  std::ostream & operator<<(std::ostream & os,
			    const GTK::GigaTrackerTLHit & Frame);


}//~namespace IImaS

#endif//~GigaTrackerTLHit_HH_

