#include <stdlib.h>
#include <stdio.h>

#include <math.h>
#include <string.h>

#include <stdarg.h>

#include "CPDBuffer.h"
#include "CPDBufferProto.h"

unsigned short isLKrBlock (unsigned int Data) {
  return ((Data&0xffff0000)==LKR_ID);
}
unsigned short isToBeDecoded(unsigned int Data) {
  return (!(Data&0x80000000));
}


unsigned int TrigNumber(unsigned int Header) {
  return (Header & 0x0000ffff);
}

unsigned int TrigWord(unsigned int Header) {
  return ((Header & 0xffff0000)>>16);
}

unsigned int ChannelId(unsigned int Header) {
  return (Header & 0x00003fff);
}

unsigned int TimeStamp(unsigned int Header) {
  return (Header);
}

unsigned int RioHeader(unsigned int Header, unsigned int nRio) {
  if ((Header&0xffff0000)!= RIO_HEADER + (nRio<<16)) {
    printf ("Error decoding Rio header %x for Rio %d\n",Header,nRio); return(0);}
  
  else return (Header);

}

unsigned int RioTrailer(unsigned int Header, unsigned int nRio) {
  if ((Header&0xffff0000)!= RIO_TRAILER + (nRio<<16)) {
    printf ("Error decoding Rio trailer %x for Rio %d\n",Header,nRio); return(0);}
  
  else      return (Header);
 
}

unsigned short RioNChannels (unsigned int Header) {

     return (Header&0x0000ffff);

}

unsigned short BlockLength (unsigned int Header) {

     return (Header&0x0000ffff);

}

int xcha_scha (int s) {return s/128;}
int ycha_scha (int s) {return (s%128)^0x0000007f;}
int xcpd_scpd (int s) {return s/16;}
int ycpd_scpd (int s) {return (s%16)^0x0000000f;}
int xchacpd_schacpd (int s) {return s/8;}
int ychacpd_schacpd (int s) {return (s%8)^0x00000007;}
int xcha_scpd_schacpd (int s ,int c) {return 8*xcpd_scpd(s)+xchacpd_schacpd(c);}
int ycha_scpd_schacpd (int s ,int c) {return 8*ycpd_scpd(s)+ychacpd_schacpd(c);}
int schacpd_xchacpd_ychacpd (int x, int y) {return 8*x +7 -y;}
int xchacpd_xcha (int xcha) {return xcha%8;}
int ychacpd_ycha (int ycha) {return ycha%8;}
int schacpd_xcha_ycha (int xcha, int ycha) {return schacpd_xchacpd_ychacpd(xchacpd_xcha(xcha),ychacpd_ycha(ycha));}
int schacpd_scha (int scha) {return schacpd_xcha_ycha(xcha_scha(scha),ycha_scha(scha));}
int xcpd_xcha (int xcha) {return xcha/8;}
int ycpd_ycha (int ycha) {return ycha/8;}
int scpd_xcpd_ycpd (int x, int y) {return 16*x+15-y;}
int scpd_xcha_ycha (int x, int y) {return scpd_xcpd_ycpd (xcpd_xcha(x), ycpd_ycha(y));}
int scpd_scha (int scha) {return scpd_xcha_ycha (xcha_scha(scha), ycha_scha(scha));}





