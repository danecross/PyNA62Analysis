#include "GigaTrackerTLMasks.hh"
#include <stdio.h>
#include <bitset>
#include <iostream> //cout
#include "RG_Utilities.hh"

namespace GigaTrackerTLMask
{
   unsigned int GetUInt( unsigned int buff, unsigned int mask, unsigned int shift){
     unsigned int masked  =  (buff&mask);
     unsigned int shifted = (masked >> shift);
    return shifted;
  }

  void SetUInt( unsigned int* buff, unsigned int val, unsigned int mask, unsigned int shift){
    DBG(std::cout<<"  0. value      : "<< std::bitset<32>(val)<<" = "<<std::hex<<val<<" = "<<std::dec<<val<<"\n");
    DBG(std::cout<<"  1. mask       : "<< std::bitset<32>(mask)<<"\n");
    unsigned int shifted = (val << shift);
    DBG(std::cout<<"  2. shifted    : "<< std::bitset<32>(shifted)<<"\n");
    DBG(std::cout<<"  3. ori buff   : "<< std::bitset<32>(*buff)<<"\n");
    unsigned int anti_mask = (~mask);
    (*buff) = ((*buff) & anti_mask); //put zeros where val has to be inserted
    DBG(std::cout<<"  4. clean buff : "<< std::bitset<32>(*buff)<<"\n");
    (*buff) =  ((*buff) | shifted);  //insert val
    DBG(std::cout<<"  5. modi buff  : "<< std::bitset<32>(*buff)<<"\n--\n");
  }
  

   float GetFloat( unsigned int buff){
    float* value = reinterpret_cast<float*>(&buff);
    return (*value);
  }
  
   void SetFloat( unsigned int* buff, float fval){
    unsigned int* val = reinterpret_cast<unsigned int*>(&fval);
    (*buff) = (*val);
  }

}
