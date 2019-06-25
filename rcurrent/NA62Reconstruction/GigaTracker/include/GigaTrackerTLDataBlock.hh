#ifndef GigaTrackerTLDataBlock_HH_
#define GigaTrackerTLDataBlock_HH_
//#include "GTKTypeDefs.hh"
#include <iostream>
#include <vector>
#include "GigaTrackerTLHit.hh"
#include "GigaTrackerTLStation.hh"

namespace GTK
{

  class GigaTrackerTLDataBlock
  {
  public:
    GigaTrackerTLDataBlock();
    virtual ~GigaTrackerTLDataBlock();

    unsigned int ReadFromBuffer(unsigned int* Buff, std::vector<GigaTrackerTLHit>& Hits);
    void WriteToBuffer(unsigned int*);

    void SetSourceId(unsigned int);
    unsigned int GetSourceId()const;

    void SetNBytes(unsigned int);
    unsigned int GetNBytes()const;

    void SetNStations(unsigned int);
    unsigned int GetNStations()const;

    void Add( GigaTrackerTLHit &hit);

    //    const GigaTrackerTLStation & operator[](unsigned int idx)const;

    friend std::ostream & operator<<(std::ostream & os, 
				     const GigaTrackerTLDataBlock & block); 

    GigaTrackerTLDataBlock(const GigaTrackerTLDataBlock &);

    GigaTrackerTLDataBlock & operator=(const GigaTrackerTLDataBlock &);

  protected:
    unsigned int fSourceId;
    unsigned int fNBytes;
    unsigned int fNStations;
    std::vector<GigaTrackerTLStation> fStations;


  };


}//~namespace IImaS

#endif//~GigaTrackerTLDataBlock_HH_

