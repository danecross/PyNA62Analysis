#ifndef GigaTrackerHitsBlock_HH_
#define GigaTrackerHitsBlock_HH_
#include <vector>
#include "GigaTrackerDAQBoardTrailerOne.hh"
#include "GigaTrackerDAQBoardTrailerTwo.hh"
#include "GigaTrackerDAQBoardTrailerThree.hh"
#include "GigaTrackerDAQBoardTimeStamp.hh"
#include <iostream>

namespace GTK
{


  class GigaTrackerHitsBlock
  {
  public:
    explicit GigaTrackerHitsBlock(int);
    virtual ~GigaTrackerHitsBlock();

    int ReadSingleEvent(u8* buffer, int & newoffset, bool eob);
    int ReadMultiEvent(u8* buffer, u32 length, bool eob);

    std::vector<GigaTrackerDAQBoardTimeStamp*>    GetTimeStamps();
    std::vector<GigaTrackerDAQBoardTimeStamp*>    GetTimeStamps(u32 chip);
    std::vector<GigaTrackerDAQBoardTrailerOne*>   GetTrailersOne();
    std::vector<GigaTrackerDAQBoardTrailerTwo*>   GetTrailersTwo();
    std::vector<GigaTrackerDAQBoardTrailerThree*> GetTrailersThree();
    int SortByChipID();

    friend std::ostream& operator<< (std::ostream &out, GigaTrackerHitsBlock & dataBlock);

    static bool CompareTSChipID(GigaTrackerDAQBoardTimeStamp* a, GigaTrackerDAQBoardTimeStamp* b);

    int Clear();

  private:
    //    GigaTrackerHitsBlock(const GigaTrackerHitsBlock &);
    //    GigaTrackerHitsBlock & operator=(const GigaTrackerHitsBlock &);

    int fDataFormat;
    bool fSorted;
    std::vector<GigaTrackerDAQBoardTimeStamp*>    fvpTimeStamp;
    std::vector<GigaTrackerDAQBoardTrailerOne*>   fvpTrailerOne;
    std::vector<GigaTrackerDAQBoardTrailerTwo*>   fvpTrailerTwo;
    std::vector<GigaTrackerDAQBoardTrailerThree*> fvpTrailerThree;

  protected:
  };

}//~namespace IImaS

#endif//~GigaTrackerHitsBlock_HH_

