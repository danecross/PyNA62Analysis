
#ifndef GigaTrackerNa62HeaderBase_HH_
#define GigaTrackerNa62HeaderBase_HH_
#include "GigaTrackerDataChunkBase.hh"
//#include "GTKTypeDefs.hh"

namespace GTK
{

  class GigaTrackerNa62HeaderBase: public GigaTrackerDataChunkBase
  {
  public:
    GigaTrackerNa62HeaderBase();
    virtual ~GigaTrackerNa62HeaderBase();

    //virtual int SetBuffer(u8*)    =0;
    //virtual int GetBufferLength() =0;
    virtual u32 GetByteNb()=0;
    virtual u32 GetTimeStamp()=0;
    virtual u32 GetSourceSubID()=0;
    virtual int Clear() = 0;
    virtual void Print(std::ostream & out);
    //std::ostream& operator<< (std::ostream &out) ;
    friend std::ostream& operator<< (std::ostream &out, GigaTrackerNa62HeaderBase & h);



  private:

  protected:
  };

}//~namespace IImaS

#endif//~GigaTrackerNa62HeaderBase_HH_

