#ifndef TELL1_BUFFER_H
#define TELL1_BUFFER_H

// Definitions of bits that appear in all data words:

#define TELL1_TYPE_MASK  0xf0000000  // Data type field
#define TELL1_HEADER 0x0000CBCB  // GBL header type fld
#define TELL1_ERROR  0x60000000  // TDC Error flag.
#define TELL1_TIMESTAMP  0x80000000  // Timestamp TDC tag.
#define TELL1_TIMESTAMP_RSHIFT  28  // Timestamp TDC tag.

// Definitions in the header only:

#define TH_WORDCOUNT_MASK  0xffff0000 
#define TH_WORDCOUNT_RSHIFT  16

// Definitions in the measurement word:

#define TELL1_LEADING  0x40000000 
#define TELL1_TRAILING  0x50000000 
#define TELL1_CHANNEL_MASK 0x00f80000 
#define TELL1_CHANNEL_RSHIFT 19 
#define TELL1_TDC_MASK 0x0f000000 
#define TELL1_TDC_RSHIFT 19 
#define TELL1_DATA_MASK  0x0007ffff 

// Extended time:

#define TELL1_TIMESTAMP_MASK  0x0fffffff  // Timestamp.

#endif // TELL1_BUFFER_H
