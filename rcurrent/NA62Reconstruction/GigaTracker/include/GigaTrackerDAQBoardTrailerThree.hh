#ifndef GigaTrackerDAQBoardTrailerThree_HH_
#define GigaTrackerDAQBoardTrailerThree_HH_
#include "GigaTrackerDataChunkBase.hh"
#include "GTKTypeDefs.hh"



namespace GTK
{

  class GigaTrackerDAQBoardTrailerThree : public GigaTrackerDataChunkBase
  {
  public:
    GigaTrackerDAQBoardTrailerThree();
    virtual ~GigaTrackerDAQBoardTrailerThree();

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

    static const u32 iFW;
    static const u32 mFW;
    static const u32 sFW;

    static const u32 iHitNb;
    static const u32 mHitNb;
    static const u32 sHitNb;

    int SetBuffer(u8*);
    int GetBufferLength();
    

    u32 GetTag();
    u32 GetFW();
    u32 GetChipId();
    u32 GetHitNb();


    friend  std::ostream& operator<< (std::ostream &out, GigaTrackerDAQBoardTrailerThree & t);

  private:
    u8 fBuffer[buffLength];
    int fErrorFlag;
    int fErrorChip;
    
  protected:
  };

}//~namespace IImaS

#endif//~GigaTrackerDAQBoardTrailerThree_HH_

