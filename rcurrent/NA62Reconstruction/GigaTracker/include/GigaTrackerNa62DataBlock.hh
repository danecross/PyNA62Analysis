#ifndef GigaTrackerNa62DataBlock_HH_
#define GigaTrackerNa62DataBlock_HH_
#include <vector>
#include "GigaTrackerNa62HeaderBase.hh"
#include "GigaTrackerNa62HeaderL1.hh"
#include "GigaTrackerHitsBlock.hh"
#include <iostream>

namespace GTK
{

  class GigaTrackerNa62DataBlock
  {
  public:
    explicit GigaTrackerNa62DataBlock( int format = 1);
    virtual ~GigaTrackerNa62DataBlock();

    int ReadFromBuffer(u8* buffer, int& newoffset);

    //    GigaTrackerNa62HeaderL0* GetHeader();
    GigaTrackerNa62HeaderBase* GetHeader();
    GigaTrackerHitsBlock* GetGigaTrackerHitsBlock();

    void Clear();

    friend std::ostream& operator<< (std::ostream &out, GigaTrackerNa62DataBlock & dataBlock);

  private:
    //    GigaTrackerNa62DataBlock(const GigaTrackerNa62DataBlock &);
    //    GigaTrackerNa62DataBlock & operator=(const GigaTrackerNa62DataBlock &);

    //GigaTrackerNa62HeaderL0* fEvtHeader;
    GigaTrackerNa62HeaderBase* fEvtHeader;
    GigaTrackerHitsBlock* fGigaTrackerHitsBlock;

    int fErrorIncNHitLength;
    int fDataFormat;
  protected:
  };

}//~namespace IImaS

#endif//~GigaTrackerNa62DataBlock_HH_

