#ifndef GigaTrackerDataChunkBase_HH_
#define GigaTrackerDataChunkBase_HH_
#include "GTKTypeDefs.hh"


namespace GTK
{

  class GigaTrackerDataChunkBase
  {
  public:
    GigaTrackerDataChunkBase();
    virtual ~GigaTrackerDataChunkBase();

    int GetUInt( u32 buff, u32 mask, u32 shift, u32& val);
    int SetUInt( u32* buff, u32 val, u32 mask, u32 shift);    

    int GetUChar( u8 buff, u8 mask, u8 shift, u8 &val);
    int SetUChar( u8* buff, u8 val, u8 mask, u8 shift);    

    int GetUShort( u16 buff, u16 mask, u16 shift, u16 &val);
    int SetUShort( u16* buff, u16 val, u16 mask, u16 shift);    

    int GetFloat( u32 buff, float &);
    int SetFloat( u32* buff, float fval);
    
    //pure virtual
    virtual int SetBuffer(u8*)    =0;
    virtual int GetBufferLength() =0;    

  private:


  protected:

  };

}//~namespace IImaS

#endif//~GigaTrackerDataChunkBase_HH_

