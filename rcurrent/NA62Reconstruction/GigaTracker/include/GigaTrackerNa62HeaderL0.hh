
#ifndef GigaTrackerNa62HeaderL0_HH_
#define GigaTrackerNa62HeaderL0_HH_
#include "GigaTrackerDataChunkBase.hh"
#include "GigaTrackerNa62HeaderBase.hh"

namespace GTK
{

  class GigaTrackerNa62HeaderL0: public GigaTrackerNa62HeaderBase
  {
  public:
    GigaTrackerNa62HeaderL0();
    virtual ~GigaTrackerNa62HeaderL0();

    static const int buffLength = 8;// lenght in BYTE

    // iX: word in which to find X 
    // mX: mask to apply to word
    // sX: bits by which to shift masked value

    static const u32 iFlag;
    static const u32 mFlag;
    static const u32 sFlag;
    static const u32 rFlag;

    static const u32 iSourceSubID;
    static const u32 mSourceSubID;
    static const u32 sSourceSubID;
    
    static const u32 iByteNb;
    static const u32 mByteNb;
    static const u32 sByteNb;

    static const u32 iTimeStamp;
    static const u32 mTimeStamp;
    static const u32 sTimeStamp;

    //virtual fct in DataChunk must be implemented
    int SetBuffer(u8*);
    int GetBufferLength();

    u32 GetFlag();
    u32 GetSourceSubID();
    u32 GetByteNb();
    u32 GetTimeStamp();
    int Clear();
    void Print(std::ostream & out);

    //    friend std::ostream& operator<< (std::ostream &out, GigaTrackerNa62HeaderL0 & h);

  private:

    u8 fBuffer[buffLength];
    int fErrorFlag;

  protected:
  };

}//~namespace IImaS

#endif//~GigaTrackerNa62HeaderL0_HH_

