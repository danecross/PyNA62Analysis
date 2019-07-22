#include <stdlib.h>
#include <stdio.h>

#include <math.h>
#include <string.h>

#include <stdarg.h>

#include "SLMBuffer.h"
#include "SLMBufferProto.h"

unsigned short isSLMLKrBlock (unsigned int Data) {
  return ( ((Data&0xff000000)>>24)==LKR_ID);
}
unsigned short isSLMToBeDecoded(unsigned int Data) {
  return (!(Data&0x80000000));
}
unsigned short isSLMGoodMarker(unsigned int Data) {
  return ( (Data&0xffff0000)==0xfaf00000);
}


unsigned int SLMTrigNumber(unsigned int Header) {
  return (Header & 0x00ffffff);
}

//unsigned int TrigWord(unsigned int Header) {
//  return ((Header & 0xffff0000)>>16);
//}

unsigned int SLMChannelId(unsigned int Header) {
  return (Header & 0x00003fff);
}

unsigned int SLMTimeStamp(unsigned int Header) {
  return (Header);
}


unsigned short SLMNChannels (unsigned int Header) {

     return (Header&0x0000ffff);

}

unsigned short SLMBlockLength (unsigned int Header) {

     return (Header&0x0000ffff);

}






