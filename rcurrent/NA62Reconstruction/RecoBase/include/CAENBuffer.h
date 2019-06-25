#ifndef TDC_BUFFER_H
#define TDC_BUFFER_H

// Definitions of bits that appear in all data words:

#define TYPE_MASK  0xf8000000  // Data type field
#define GLOBAL_HEADER 0x40000000  // GBL header type fld
#define TDC_HEADER  0x08000000  // TDC header
#define TDC_TRAILER  0x18000000  // TDC Trailer
#define MEASUREMENT  0x00000000  // TDC Measurement
#define TDC_ERROR  0x20000000  // TDC Error flag.
#define TRIGGER_TIME  0x88000000  // Trigger time tag.
#define GLOBAL_TRAILER 0x80000000 // Global trailer.
#define FILLER_LONG  0xc0000000  // Filler longs.

#define SCALER_HEADER  0x48000000
#define SCALER_TRAILER 0x90000000

// Definitions in the global header only:

#define GH_EVENTCOUNT_MASK  0x07ffffe0 
#define GH_EVENT_RSHIFT  0x5 
#define GH_GEO_MASK  0x1f 

// Definitions in the TDC header longword:

#define TDC_MASK  0x03000000 
#define TDC_RSHIFT  24 
#define EVENTID_MASK  0x007ff800 
#define EVENTID_RSHIFT  12 
#define BUNCHID_MASK  0x000007ff 
#define TDCWORDCOUNT_MASK  0x000007ff 

// Definitions in the TDC measurement word:

#define TRAILING_BIT  0x04000000 
#define V1190CHANNEL_MASK 0x03f80000 
#define V1290CHANNEL_MASK 0x03e00000 
#define V1190CHANNEL_RSHIFT 19 
#define V1290CHANNEL_RSHIFT 21 
#define V1190DATA_MASK  0x0007ffff 
#define V1290DATA_MASK  0x001fffff 

#define HITLOST_0_FIFO  0x0001  // hits lost group 0 FIFO overflow
#define HITLOST_0_L1  0x0002  // hits lost group 0 L1 overflow
#define HITERROR_0  0x0004  // hit error in group 0.'
#define HITLOST_1_FIFO  0x0008  // hits lost group 1 FIFO overflow
#define HITLOST_1_L1  0x0010  // hits lost group 1 L1 overflow
#define HITERROR_1  0x0020  // hit error in group 1.
#define HITLOST_2_FIFO  0x0040  // hits lost group 2 FIFO overflow
#define HITLOST_2_L1  0x0080  // hits lost group 2 L1 overflow
#define HITERROR_2  0x0100  // hit error in group 2.
#define HITLOST_3_FIFO  0x0200  // hits lost group 3 FIFO overflow
#define HITLOST_3_L1  0x0400  // hits lost group 3 L1 overflow
#define HITERROR_3  0x0800  // hit error in group 3.
#define HITS_EXCEEDED  0x1000  // Hits lost, size limit exceeded.
#define EVENTLOST_FIFO  0x2000  // Event lost due to trigger fifo overflow.
#define FATALCHIP_ERROR 0x4000  // fatal chip error detected.
#define ERROR_MASK  0x7fff  // All bits.

// Extended trigger time:

#define TRIGGERTIME_MASK  0x07ffffff 

// Global trailer:

#define GT_TRIGGERLOST_MASK  0x04000000 
#define GT_TDCERROR_MASK  0x02000000 
#define GT_OVERFLOW_MASK  0x01000000 
#define GT_WORDCOUNT_MASK  0x001fffe0 
#define GT_WORDCOUNT_RSHIFT  5 

#endif // TDC_BUFFER_H
