
#ifndef GigaTrackerNa62HeaderL1_HH_
#define GigaTrackerNa62HeaderL1_HH_
#include "GigaTrackerDataChunkBase.hh"
#include "GigaTrackerNa62HeaderBase.hh"

namespace GTK
{

  class GigaTrackerNa62HeaderL1:  public GigaTrackerNa62HeaderBase
  {
  public:
    GigaTrackerNa62HeaderL1();
    virtual ~GigaTrackerNa62HeaderL1();

    static const int buffLength = 16; //lenght in BYTE

    // iX: word in which to find X 
    // mX: mask to apply to word
    // sX: bits by which to shift masked value
    // rX: reference if applicable

    static const u32 iSourceID;
    static const u32 mSourceID;
    static const u32 sSourceID;
    static const u32 rSourceID;

    static const u32 iEvtNb;
    static const u32 mEvtNb;
    static const u32 sEvtNb;

    static const u32 iWordNb;
    static const u32 mWordNb;
    static const u32 sWordNb;

    static const u32 iTimeStamp;
    static const u32 mTimeStamp;
    static const u32 sTimeStamp;

    static const u32 iSourceSubID;
    static const u32 mSourceSubID;
    static const u32 sSourceSubID;

    static const u32 iErrorBits;
    static const u32 mErrorBits;
    static const u32 sErrorBits;

    static const u32 iL0TriggerWord;
    static const u32 mL0TriggerWord;
    static const u32 sL0TriggerWord;

    


    //virtual fct in DataChunk must be implemented

    int SetBuffer(u8*);
    int GetBufferLength();

    u32 GetSourceID();
    u32 GetEvtNb();
    u32 GetWordNb();
    u32 GetTimeStamp();
    u32 GetSourceSubID();
    u32 GetErrorBits();
    u32 GetL0TriggerWord();
    bool IsEoB();
    u32 GetByteNb();
    int Clear();
    void Print(std::ostream & out);
    //    std::ofstream& Print(std::ofstream & out);

    //friend std::ostream& operator<< (std::ostream &out, GigaTrackerNa62HeaderL1 & h);

  private:

    u8 fBuffer[buffLength];
    int fErrorFlag;
    int fErrorSubId;

  protected:
  };

}//~namespace IImaS

#endif//~GigaTrackerNa62HeaderL1_HH_

