#include "GigaTrackerDataChunkBase.hh" 
#include "RG_Utilities.hh"

namespace GTK
{

  //==============================================
  GigaTrackerDataChunkBase::GigaTrackerDataChunkBase()
  {
    
  }
  
  //==============================================
  GigaTrackerDataChunkBase::~GigaTrackerDataChunkBase()
  {
    
  }
  
  
  //==============================================
  int GigaTrackerDataChunkBase::GetUInt( u32 buff, u32 mask, u32 shift, u32& val){
    if(shift > 31) {
      RG_REPORT_ERROR_MSG("Shift must be between 0 and 31");
      return 1;
    }
    u32 masked  =  (buff&mask);
    val = (masked >> shift);
    return 0;
  }
  
  //==============================================  
  int GigaTrackerDataChunkBase::SetUInt( u32* buff, u32 val, u32 mask, u32 shift){
    if(shift > 31) {
      RG_REPORT_ERROR_MSG("Shift must be between 0 and 31");
      return 1;
    }
    u32 shifted = (val << shift);
    u32 anti_mask = (~mask);
    (*buff) = ((*buff) & anti_mask); //put zeros where val has to be inserted
    (*buff) =  ((*buff) | shifted);  //insert val
    return 0;
  }
  
  //==============================================
  int GigaTrackerDataChunkBase::GetUChar( u8 buff, u8 mask, u8 shift, u8& val){
    if(shift > 31) {
      RG_REPORT_ERROR_MSG("Shift must be between 0 and 7");
      return 1;
    }
    u8 masked  =  (buff&mask);
    val = (masked >> shift);
    return 0;
  }

  
  //==============================================  
  int GigaTrackerDataChunkBase::SetUChar( u8* buff, u8 val, u8 mask, u8 shift){
    if(shift > 31) {
      RG_REPORT_ERROR_MSG("Shift must be between 0 and 31");
      return 1;
    }
    u8 shifted = (val << shift);
    u8 anti_mask = (~mask);
    (*buff) = ((*buff) & anti_mask); //put zeros where val has to be inserted
    (*buff) =  ((*buff) | shifted);  //insert val
    return 0;
  }



  //==============================================
  int GigaTrackerDataChunkBase::GetUShort( u16 buff, u16 mask, u16 shift, u16& val){
    if(shift > 31) {
      RG_REPORT_ERROR_MSG("Shift must be between 0 and 7");
      return 1;
    }
    u16 masked  =  (buff&mask);
    val = (masked >> shift);
    return 0;
  }

  
  //==============================================  
  int GigaTrackerDataChunkBase::SetUShort( u16* buff, u16 val, u16 mask, u16 shift){
    if(shift > 31) {
      RG_REPORT_ERROR_MSG("Shift must be between 0 and 31");
      return 1;
    }
    u16 shifted = (val << shift);
    u16 anti_mask = (~mask);
    (*buff) = ((*buff) & anti_mask); //put zeros where val has to be inserted
    (*buff) =  ((*buff) | shifted);  //insert val
    return 0;
  }


  //==============================================
  int GigaTrackerDataChunkBase::GetFloat( u32 buff, float& val){
    float* value = reinterpret_cast<float*>(&buff);
    val = *value;
    return 0;
  }
  
  //==============================================  
  int GigaTrackerDataChunkBase::SetFloat( u32* buff, float fval){
    RG_CHECK_PTR_RETURN_INT(buff,1);
    u32* val = reinterpret_cast<u32*>(&fval);
    (*buff) = (*val);
    return 1;
  }

}//~namespace IImaS

