#ifndef CPD_BUFFER_H
#define CPD_BUFFER_H

// Definitions for the old 200x CPD data format

#define LKR_ID  0x14900000  // Id of LKr block
#define HEADER_MARKER 0xbacebace  // Visual marker in the header
#define TRAILER_MARKER  0xfeed0000  // Visual marker in the trailer
#define RIO_HEADER  0xfaf00000  // RIO Header
#define RIO_TRAILER  0xfef00000  // RIO Trailer

// Parameters

#define NRIO   8

#endif // CPD_BUFFER_H
