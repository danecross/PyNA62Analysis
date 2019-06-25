#ifndef GigaTrackerDAQBoardTrailerTwo_HH_
#define GigaTrackerDAQBoardTrailerTwo_HH_
#include "GigaTrackerDataChunkBase.hh"
#include "GTKTypeDefs.hh"



namespace GTK
{

  class GigaTrackerDAQBoardTrailerTwo : public GigaTrackerDataChunkBase
  {
  public:
    GigaTrackerDAQBoardTrailerTwo();
    virtual ~GigaTrackerDAQBoardTrailerTwo();

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

    static const u32 iHitNbFromPrev;
    static const u32 mHitNbFromPrev;
    static const u32 sHitNbFromPrev;

    static const u32 iTriggerType;
    static const u32 mTriggerType;
    static const u32 sTriggerType;

    // timestamp and sob specific
    static const u32 EOBTriggerType;

    static const u32 iHitNb;
    static const u32 mHitNb;
    static const u32 sHitNb;

    static const u32 iL1ANb;
    static const u32 mL1ANb;
    static const u32 sL1ANb;

    // eob specific
    static const u32 iErrCounter;
    static const u32 mErrCounter;
    static const u32 sErrCounter;

    static const u32 iChokeCounter;
    static const u32 mChokeCounter;
    static const u32 sChokeCounter;

    static const u32 iHWTriggerTimeStamp;
    static const u32 mHWTriggerTimeStamp;
    static const u32 sHWTriggerTimeStamp;


    int SetBuffer(u8*);
    int GetBufferLength();
    

    u32 GetTag();
    u32 GetChipId();
    u32 GetHitNbFromPrev();
    u32 GetTriggerType();
    bool IsEOB();

    u32 GetHitNb();
    u32 GetL1ANb();

    u32 GetErrCounter();
    u32 GetChokeCounter();
    u32 GetHWTriggerTimeStamp();

    friend  std::ostream& operator<< (std::ostream &out, GigaTrackerDAQBoardTrailerTwo & t);

  private:
    u8 fBuffer[buffLength];
    int fErrorFlag;
    int fErrorChip;

  protected:
  };

}//~namespace IImaS

#endif//~GigaTrackerDAQBoardTrailerTwo_HH_

