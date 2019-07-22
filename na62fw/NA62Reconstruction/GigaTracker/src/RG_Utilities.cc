#include "RG_Utilities.hh"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>

#ifndef DEBUG_INT_STACK
#define DEBUG_INT_STACK 0
#endif
  

#if( DEBUG_INT_STACK )
#define DBG(_x)	((void)(_x))
#else
#define DBG(_x)	((void)0)
#endif



int RG_FileExists(const char * aFileName)
{
  struct stat lbuf;
  int lRet=stat(aFileName, &lbuf);
  if(lRet!=-1){
    return S_ISREG(lbuf.st_mode);//==S_IFREG;
  }
  return -1;
}


unsigned int binary_to_gray(unsigned int num)
{
  return (num>>1)^num;
}
 

unsigned int gray_to_binary(unsigned int num)
{
  unsigned int numBits = 8*sizeof(num);
  unsigned int shift=0;
  for (shift=1; shift<numBits; shift=2*shift)
    {
      num=num^(num>>shift);
    }
  return num;
}


unsigned int ones_count(unsigned int num, unsigned int nbits){
  unsigned int count=0; 
  for(unsigned int i(0); i!=nbits; i++){
    unsigned int bit=1<<i; 
    if(bit&num)count++;
  }
  return count;   
}
unsigned int parity(unsigned int num, unsigned int nbits)
{
  return (ones_count(num, nbits)&1);
}
