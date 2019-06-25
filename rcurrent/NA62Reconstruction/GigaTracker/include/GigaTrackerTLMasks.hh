#ifndef _GigaTrackerTLMasks_HH_
#define _GigaTrackerTLMasks_HH_

namespace GigaTrackerTLMask
{

  const unsigned int l_word = 4;

  // Data Block Header
   const unsigned int l_block = 2;
   const unsigned int m_block_NBytes = 0xffff;
   const unsigned int s_block_NBytes = 0;

   const unsigned int m_block_SourceId = 0xff0000;
   const unsigned int s_block_SourceId = 16;

   const unsigned int m_block_NStations = 0x7;
   const unsigned int s_block_NStations = 0;


  // Station Header
   const unsigned int l_station = 2;
   const unsigned int m_station_NFrames = 0xffff;
   const unsigned int s_station_NFrames = 0;

   const unsigned int m_station_StationId = 0x30000;
   const unsigned int s_station_StationId = 16;

   const unsigned int m_station_SensorUpstream = 0x40000;
   const unsigned int s_station_SensorUpstream = 18;

   const unsigned int m_station_Delay = 0xffffffff;
   const unsigned int s_station_Delay = 0;

  // Frame Header
   const unsigned int l_frame = 2;
   const unsigned int m_frame_FrameCounter = 0xfffffff;
   const unsigned int s_frame_FrameCounter = 0;

   const unsigned int m_frame_NHits = 0xffff;
   const unsigned int s_frame_NHits = 0;


  // Hits
   const unsigned int l_hit = 2;
   const unsigned int m_hit_ChipId =  0x3c000000;
   const unsigned int s_hit_ChipId = 26;

   const unsigned int m_hit_qChipId = 0x03000000;
   const unsigned int s_hit_qChipId = 24;

   const unsigned int m_hit_NCollisions = 0xfc0000;
   const unsigned int s_hit_NCollisions = 18;

   const unsigned int m_hit_PixelAddress = 0x3f800;
   const unsigned int s_hit_PixelAddress = 11;

   const unsigned int m_hit_HitArbiterAddress = 0x3e0;
   const unsigned int s_hit_HitArbiterAddress = 5;

   const unsigned int m_hit_PileUpAddress = 0x1f;
   const unsigned int s_hit_PileUpAddress = 0;

   const unsigned int m_hit_LeadingSelector = 0x20000000;
   const unsigned int s_hit_LeadingSelector = 29;
  
   const unsigned int m_hit_LeadingCoarse = 0x1ffe0000;
   const unsigned int s_hit_LeadingCoarse = 17;

   const unsigned int m_hit_LeadingFine = 0x1f000;
   const unsigned int s_hit_LeadingFine = 12;

   const unsigned int m_hit_TotSelector = 0x800;
   const unsigned int s_hit_TotSelector = 11;

   const unsigned int m_hit_TotCoarse = 0x7e0;
   const unsigned int s_hit_TotCoarse = 5;

   const unsigned int m_hit_TotFine = 0x1f;
   const unsigned int s_hit_TotFine = 0;
  
   unsigned int GetUInt( unsigned int buff, unsigned int mask, unsigned int shift);
   void SetUInt( unsigned int* buff, unsigned int val, unsigned int mask, unsigned int shift);
   float GetFloat( unsigned int buff);
   void SetFloat( unsigned int* buff, float fval);
}

#endif
