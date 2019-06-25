#ifndef GigaTrackerDAQBoardTrailerOne_HH_
#define GigaTrackerDAQBoardTrailerOne_HH_
#include "GigaTrackerDataChunkBase.hh"
#include "GTKTypeDefs.hh"
#include <vector>


namespace GTK
{

  class GigaTrackerDAQBoardTrailerOne : public GigaTrackerDataChunkBase
  {
  public:
    GigaTrackerDAQBoardTrailerOne();
    virtual ~GigaTrackerDAQBoardTrailerOne();
    
    static const int buffLength = 8; //lenght in BYTE

    // iX: word in which to find X 
    // mX: mask to apply to word
    // sX: bits by which to shift masked value

    static const u32 iTag;
    static const u32 mTag;
    static const u32 sTag;
    static const u32 rTag;
    
    static const u32 iChipID;
    static const u32 mChipID;
    static const u32 sChipID;
    
    static const u32 iL1AEvtCounter;
    static const u32 mL1AEvtCounter;
    static const u32 sL1AEvtCounter;
    
    static const u32 iL1ATimeStamp;
    static const u32 mL1ATimeStamp;
    static const u32 sL1ATimeStamp;

    int SetBuffer(u8*);
    int GetBufferLength();
    

    u32 GetTag();

    u32 GetChipId();

    u32 GetL1AEvtCounter();

    u32 GetL1ATimeStamp();

    friend  std::ostream& operator<< (std::ostream &out, GigaTrackerDAQBoardTrailerOne & t);

  private:
    u8 fBuffer[buffLength];
    int fErrorFlag;
    int fErrorChip;
    
  protected:
  };

}//~namespace IImaS

#endif//~GigaTrackerDAQBoardTrailerOne_HH_

