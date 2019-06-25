
#ifndef GigaTrackerDAQBoardTimeStamp_HH_
#define GigaTrackerDAQBoardTimeStamp_HH_
#include "GigaTrackerDataChunkBase.hh"
#include "GTKTypeDefs.hh"
#include "GigaTrackerDAQBoardTrailerOne.hh"
#include "GigaTrackerDAQBoardTrailerTwo.hh"
#include <stdio.h>

  

namespace GTK
{

  class GigaTrackerDAQBoardTimeStamp : public GigaTrackerDataChunkBase
  {
  public:
    GigaTrackerDAQBoardTimeStamp();
    virtual ~GigaTrackerDAQBoardTimeStamp();
    
    static const int buffLength = 8; //lenght in BYTE
    
    // iX: word in which to find X 
    // mX: mask to apply to word
    // sX: bits by which to shift masked value

    static const u32 iFlag;
    static const u32 mFlag;
    static const u32 sFlag;
    static const u32 rFlag;

    static const u32 iChipId;
    static const u32 mChipId;
    static const u32 sChipId;

    static const u32 iStationId;
    static const u32 mStationId;
    static const u32 sStationId;

    static const u32 iFrameCountLSB;
    static const u32 mFrameCountLSB;
    static const u32 sFrameCountLSB;

    static const u32 iDataStatusSelector;
    static const u32 mDataStatusSelector;
    static const u32 sDataStatusSelector;

    static const u32 iPixelAddress;
    static const u32 mPixelAddress;
    static const u32 sPixelAddress;

    static const u32 iHitArbiterAddress;
    static const u32 mHitArbiterAddress;
    static const u32 sHitArbiterAddress;

    static const u32 iPileUpAddress;
    static const u32 mPileUpAddress;
    static const u32 sPileUpAddress;

    static const u32 iqChipId;
    static const u32 mqChipId;
    static const u32 sqChipId;

    static const u32 iLeadingSelector;
    static const u32 mLeadingSelector;
    static const u32 sLeadingSelector;

    static const u32 iLeadingCoarse;
    static const u32 mLeadingCoarse;
    static const u32 sLeadingCoarse;

    static const u32 iLeadingFine;
    static const u32 mLeadingFine;
    static const u32 sLeadingFine;

    static const u32 iTotSelector;
    static const u32 mTotSelector;
    static const u32 sTotSelector;

    static const u32 iTotCoarse;
    static const u32 mTotCoarse;
    static const u32 sTotCoarse;

    static const u32 iTotFine;
    static const u32 mTotFine;
    static const u32 sTotFine;

    static u32 NbErrFrameLSB[60];
    static u32 NbErrStationId[60];
    static u32 NbErrFrameLSBAll;
    static u32 NbErrTag;


    static void ResetError();
    static void PrintErrorSummary();

    int SetBuffer(u8*);
    u8* GetBuffer( ){return fBuffer;};
    int GetBufferLength();
    
    //data accessor
    u32 GetFlag();

    u32 GetChipId();

    u32 GetStationId();

    u32 GetFrameCountLSB();

    u32 GetDataStatusSelector();

    u32 GetPixelAddress();

    u32 GetHitArbiterAddress();
    
    int GetNbHitArbiterAddress();
    
    u32 GetPileUpAddress();

    int GetNbPileUpAddress();

    
    u32 GetqChipId();

    u32 GetLeadingSelector();

    u32 GetLeadingCoarse();

    u32 GetLeadingFine();

    u32 GetTotSelector();

    u32 GetTotCoarse();

    u32 GetTotFine();

    u32 GetFrameCounter();

    u32 GetL1ATimeStamp();

    int ConsistencyWithTrailers();

    //refined function
    double GetLeadingTime();
    int    GetPixelUID(int refiring = 0, bool sensorUpstream = false);

    
    double GetTrailingTime();

    int SetTrailerOne( GigaTrackerDAQBoardTrailerOne* t );
    int SetTrailerTwo( GigaTrackerDAQBoardTrailerTwo* t );

    GigaTrackerDAQBoardTrailerOne* GetTrailerOne(){return fTrailerOne;};
    GigaTrackerDAQBoardTrailerTwo* GetTrailerTwo(){return fTrailerTwo;};


    friend  std::ostream& operator<< (std::ostream &out, GigaTrackerDAQBoardTimeStamp & ts);
    friend bool operator == (  GigaTrackerDAQBoardTimeStamp & ts1,  GigaTrackerDAQBoardTimeStamp & ts2);

  private:

    u8 fBuffer[buffLength];          // 64 bits
    GigaTrackerDAQBoardTrailerOne* fTrailerOne; // 64 bits
    GigaTrackerDAQBoardTrailerTwo* fTrailerTwo; // 64 bits
    //Nota: To save RAM the two trailers pointer could be
    //      replaced by the trigger timestamp (u32).
    //      Memory usage would go from 32 to 20 bytes.

    int fErrorFlag;
    int fErrorChip;
    int fErrorqChip;
    int fErrorStation;
    int fErrorHitArbiter;
    int fErrorPixAddress;
    int fErrorPUAddress;
    int fErrorDataStatus;

    int fErrorIncFcL1;
    int fErrorIncChipT1;
    int fErrorIncChipT2;


  protected:
  };

}//~namespace IImaS

#endif//~GigaTrackerDAQBoardTimeStamp_HH_

