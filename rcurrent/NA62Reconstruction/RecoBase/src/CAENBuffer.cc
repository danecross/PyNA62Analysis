#include <stdlib.h>
#include <stdio.h>
                                                                                                          
#include <math.h>
#include <string.h>
                                                                                                          
#include <stdarg.h>
/* #include "CAENVMElib.h" */
/* #include "console.h" */
                                                                                                          
/* #include "VmeIO.h" */
/* #include "ExpReadTdcAdc.h" */
/* #include "Tdc.h" */
#include "CAENBuffer.h"
#include "CAENBufferProto.h"

unsigned short isScalerHeader(unsigned long Data) {
  return (Data == SCALER_HEADER);
}

unsigned short isScalerTrailer(unsigned long datum) {
  return (datum == SCALER_TRAILER);
}

unsigned short isGlobalHeader(unsigned long Data) {
  return ((Data & TYPE_MASK) == GLOBAL_HEADER);
}

unsigned short isGlobalTrailer(unsigned long datum) {
  return ((datum & TYPE_MASK) == GLOBAL_TRAILER);
}

int TriggerNumber(unsigned long Header) {
  if(!isGlobalHeader(Header)) {
    printf("TdcData:TriggerNumber - given non global header");
    return 0;
  }
  else {
    return ((Header & GH_EVENTCOUNT_MASK) >> GH_EVENT_RSHIFT);
  }
}

unsigned int BoardNumber(unsigned long Header) {
  if((!isGlobalHeader(Header)) && (!isGlobalTrailer(Header))) {
    printf("TdcData:BoardNumber - given non global header");
    return 0;
  }
  else {
    return (Header & GH_GEO_MASK);
  }
}

unsigned short isTDCHeader(unsigned long Header) {
  return ((Header & TYPE_MASK) == TDC_HEADER);
}

unsigned short isTDCTrailer(unsigned long header) {
  return ((header & TYPE_MASK) == TDC_TRAILER);
}

unsigned short isTDCError(unsigned long Datum) {
  return ((Datum & TYPE_MASK) == TDC_ERROR);
}

unsigned int TDCChip(unsigned long Header) {
  if(!isTDCHeader(Header) && !isTDCTrailer(Header) && !isTDCError(Header)) {
    printf("TdcData:TDCChip - not data with chip no.");
    return 0;
  } 
  else {
    return ((Header & TDC_MASK) >> TDC_RSHIFT);
  }
}

unsigned int EventId(unsigned long Header) {
  if (!isTDCHeader(Header)  && !isTDCTrailer(Header)) {
    printf("TdcData:EventId - not a tdc header");
    return 0;
  } 
  else {
    return ((Header & EVENTID_MASK) >> EVENTID_RSHIFT);
  }
}

unsigned int BunchId(unsigned long Header) {
  if(!isTDCHeader(Header)) {
    printf("TdcData:Bundhid - not a tdc header");
    return 0;
  }
  else {
    return (Header & BUNCHID_MASK); // Assumes right justified.
  }
}

short TDCWordCount(unsigned long Header) {
  if(!isTDCTrailer(Header)) {
    printf("TdcData:TDCWordCount not a tdc trailer");
    return 0;
  } 
  else {
    return (Header & TDCWORDCOUNT_MASK);
  }

}

unsigned short isMeasurement(unsigned long data) {
  return ((data & TYPE_MASK) == MEASUREMENT);
}

unsigned short isTrailing(unsigned long datum)  {
  if(!isMeasurement(datum)) {
    printf("TdcData:isTrailing not a measurement word");
    return 0;
  } else {
    return ((datum & TRAILING_BIT) != 0);
  }
}

int ChannelNumber(unsigned long Datum) {
  if(!isMeasurement(Datum)) {
    printf("TdcData:ChannelNumber  is not a measurement");
    return 0;
  } 
  else {
    unsigned short is1190 = 1; // 1 for rich, 0 for cedar
    unsigned long mask = V1190CHANNEL_MASK;
    unsigned long shift= V1190CHANNEL_RSHIFT;
    
    // If the module is actually a 1290, correct the
    // assumptions above.
    
    if(!is1190) {
      mask = V1290CHANNEL_MASK;
      shift= V1290CHANNEL_RSHIFT;
    }

    return ((Datum & mask) >> shift);
  }
}

long ChannelValue(unsigned long datum) {
  if (!isMeasurement(datum)) {
    printf("TdcData:ChannelValue - is not a measurement");
    return 0;
  } else {
    unsigned short is1190 = 1; // 1 for rich, 0 for cedar
    unsigned long mask = is1190 ? V1190DATA_MASK : 
                                  V1290DATA_MASK;
    return (datum & mask);
  }
}

unsigned long TDCErrorBits(unsigned long datum) {
  if(!isTDCError(datum)) {
    printf("TdcData: not an error flag word");
    return 0;
  }
  else {
    return datum & ERROR_MASK;
  }
  
}

unsigned short isTriggerTimeTag(unsigned long datum) {
  return ((datum & TYPE_MASK) == TRIGGER_TIME);
}

unsigned long ExtendedTriggerTime(unsigned long datum) {
  if(!isTriggerTimeTag(datum)) {
    printf("TdcData:ExtendedTriggerTime not a trigger time word");
    return 0;
  } 
  else {
    return (datum & TRIGGERTIME_MASK);
  }
}

unsigned short Overflow(unsigned long datum) {
  if(!isGlobalTrailer(datum)) {
    printf("TdcData:Overflow - not a global trailer");
    return 0;
  }
  else {
    return ((datum & GT_OVERFLOW_MASK) == GT_OVERFLOW_MASK);
  }
}

unsigned short Error(unsigned long datum) {
  if(!isGlobalTrailer(datum)) {
    printf("TdcData:Error - not a global trailer");
    return 0;
  } 
  else {
    return ((datum & GT_TDCERROR_MASK) == GT_TDCERROR_MASK);
  }
}

unsigned short Lost(unsigned long datum) {
  if(!isGlobalTrailer(datum)) {
    printf("TdcData:Lost - not a trailer long");
    return 0;
  } 
  else {
    return ((datum & GT_TRIGGERLOST_MASK) & GT_TRIGGERLOST_MASK);
  }
}

unsigned long EventSize(unsigned long datum) {
  if(!isGlobalTrailer(datum)) {
    printf("TdcData:EventSize - not a trailer long");
    return 0;
  }
  else {
    return ((datum & GT_WORDCOUNT_MASK) >> GT_WORDCOUNT_RSHIFT);
  }
}

unsigned short isFiller(unsigned long datum) {
    return ((datum & TYPE_MASK) == FILLER_LONG);
}

