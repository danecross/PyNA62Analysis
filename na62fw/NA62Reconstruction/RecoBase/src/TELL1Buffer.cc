#include <stdlib.h>
#include <stdio.h>
                                                                                                          
#include <math.h>
#include <string.h>
                                                                                                          
#include <stdarg.h>

#include "TELL1Buffer.h"

unsigned short isTELL1Header(unsigned long datum) {
  return ((datum & TELL1_HEADER) == TELL1_HEADER && (datum & TELL1_TYPE_MASK) == 0);
}

unsigned long TELL1EventSize(unsigned long datum) {
  if(!isTELL1Header(datum)) {
    printf("TELL1Data:EventSize - not a header long");
    return 0;
  }
  else {
    return ((datum & TH_WORDCOUNT_MASK) >> TH_WORDCOUNT_RSHIFT);
  }
}

unsigned short isTELL1DummyWord(unsigned long /*datum*/) {
  return 1;
}

unsigned short isTELL1Error(unsigned long datum) {
  return ((datum & TELL1_TYPE_MASK) == TELL1_ERROR);
}

unsigned short isTELL1Leading(unsigned long datum)  {
  return ((datum & TELL1_TYPE_MASK) == TELL1_LEADING);
}

unsigned short isTELL1Trailing(unsigned long datum)  {
  return ((datum & TELL1_TYPE_MASK) == TELL1_TRAILING);
}

unsigned short isTELL1Measurement(unsigned long datum) {
  return (isTELL1Leading(datum)  || isTELL1Trailing(datum));
}

int TELL1ChannelNumber(unsigned long Datum) {
  if(!isTELL1Measurement(Datum)) {
    printf("TELL1Data:ChannelNumber  is not a measurement");
    return 0;
  } 
  else {
    return (((Datum & TELL1_CHANNEL_MASK) >> TELL1_CHANNEL_RSHIFT) | ((Datum & TELL1_TDC_MASK) >> TELL1_TDC_RSHIFT));
  }
}

long TELL1ChannelValue(unsigned long datum) {
  if (!isTELL1Measurement(datum)) {
    printf("TELL1Data:ChannelValue - is not a measurement");
    return 0;
  } else {
    return (datum & TELL1_DATA_MASK);
  }
}

unsigned short isTELL1TimeStamp(unsigned long datum) {
  return ((datum & TELL1_TYPE_MASK) >= TELL1_TIMESTAMP);
}

unsigned long TELL1TimeStamp(unsigned long datum) {
  if(!isTELL1TimeStamp(datum)) {
    printf("TELL1Data:TELL1TimeStamp not a time stamp word");
    return 0;
  } 
  else {
    return (datum & TELL1_TIMESTAMP_MASK);
  }
}

unsigned long TELL1TimeStampBoard(unsigned long datum) {
  if(!isTELL1TimeStamp(datum)) {
    printf("TELL1Data:TELL1TimeStamp not a time stamp word");
    return 0;
  } 
  else {
    return (((datum & TELL1_TYPE_MASK) >> TELL1_TIMESTAMP_RSHIFT) - 8);
  }
}

