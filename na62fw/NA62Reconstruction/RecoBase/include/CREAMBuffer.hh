#ifndef _CREAMBuffer_hh
#define _CREAMBuffer_hh

// Offsets inside the CREAM block in 32 bit words

#define O_CREAM_EVENT_NUMBER    0              // Offset of the Event number
#define O_CREAM_EVENT_LENGTH    1              // Offset of the Event length
#define O_CREAM_EVENT_TSTAMP    2              // Offset of the Timestamp
#define O_CREAM_EVENT_L1ERROR   3              // Offset of the L1 error bits
#define O_CREAM_EVENT_TRIGWORD  3              // Offset of the Trigger word
#define O_CREAM_EVENT_CREAMID   3              // Offset of the cream ID (crate and slot)
#define O_CREAM_EVENT_PLOAD_LEN 4              // Offset of the payload length
#define O_CREAM_EVENT_NSAMPLES  4              // Offset of the number of samples
#define O_CREAM_EVENT_FLAGS     4              // Offset of the event flags  (L0/L1 & zero suppression)
#define O_CREAM_EVENT_CH_MASK   5              // Offset of the active channel mask
#define O_CREAM_EVENT_SAMPLES   6              // Offset of the start of ADC samples

// Masks to extract the variables

#define M_CREAM_EVENT_NUMBER    0x00ffffff     // Mask for the Event number
#define M_CREAM_EVENT_DETID     0xff000000     // Mask for the Event number
#define M_CREAM_EVENT_LENGTH    0x0000ffff     // Mask for the Event length
#define M_CREAM_EVENT_TSTAMP    0x7fffffff     // Mask for the Timestamp
#define M_CREAM_EVENT_L1ERROR   0xff000000     // Mask for the L1 error bits
#define M_CREAM_EVENT_TRIGWORD  0x003f0000     // Mask for the Trigger word
#define M_CREAM_EVENT_CRATEID   0x000007e0     // Mask for the crate ID
#define M_CREAM_EVENT_SLOTID    0x0000001f     // Mask for the cream ID
#define M_CREAM_EVENT_PLOAD_LEN 0x0000ffff     // Mask for the payload length
#define M_CREAM_EVENT_NSAMPLES  0x01ff0000     // Mask for the number for samples
#define M_CREAM_EVENT_FLAG_L0   0x80000000     // Mask of the event flags  (L0/L1 & zero suppression)
#define M_CREAM_EVENT_FLAG_ZS   0x40000000     // Mask of the event flags  (L0/L1 & zero suppression)

// Shifts to extract the variables

#define S_CREAM_EVENT_L1ERROR   24             // Shift for the L1 error bits
#define S_CREAM_EVENT_TRIGWORD  16             // Shift for the Trigger word
#define S_CREAM_EVENT_CRATEID    5             // Shift for the crate ID
#define S_CREAM_EVENT_NSAMPLES  16             // Shift for the number of samples

#endif
