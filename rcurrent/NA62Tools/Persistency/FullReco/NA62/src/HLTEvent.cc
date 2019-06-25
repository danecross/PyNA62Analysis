// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-12-05
//
// ---------------------------------------------------------------

#include "HLTEvent.hh"

#include "Riostream.h"

ClassImp(HLTEvent)

HLTEvent::HLTEvent(): TObject(){
  Clear();
}

HLTEvent::HLTEvent(const HLTEvent& c) : TObject(c), fCHODTime(c.fCHODTime),fNewCHODTime(c.fNewCHODTime),
fL0TPRefTime(c.fL0TPRefTime),fL1CHODResponse(c.fL1CHODResponse),fL1KTAGResponse(c.fL1KTAGResponse),fL1LAVResponse(c.fL1LAVResponse),
fL1IRCSACResponse(c.fL1IRCSACResponse),fL1StrawResponse(c.fL1StrawResponse),fL1MUV3Response(c.fL1MUV3Response),fL1NewCHODResponse(c.fL1NewCHODResponse),
fL1CHODProcessed(c.fL1CHODProcessed),fL1KTAGProcessed(c.fL1KTAGProcessed),fL1LAVProcessed(c.fL1LAVProcessed), fL1IRCSACProcessed(c.fL1IRCSACProcessed),
fL1StrawProcessed(c.fL1StrawProcessed), fL1MUV3TriggerMultiProcessed(c.fL1MUV3TriggerMultiProcessed),
fL1MUV3TriggerLeftRightProcessed(c.fL1MUV3TriggerLeftRightProcessed),fL1MUV3TriggerNeighboursProcessed(c.fL1MUV3TriggerNeighboursProcessed),
fL1NewCHODProcessed(c.fL1NewCHODProcessed), fL1CHODEmptyPacket(c.fL1CHODEmptyPacket), fL1KTAGEmptyPacket(c.fL1KTAGEmptyPacket),
fL1LAVEmptyPacket(c.fL1LAVEmptyPacket), fL1IRCSACEmptyPacket(c.fL1IRCSACEmptyPacket), fL1StrawEmptyPacket(c.fL1StrawEmptyPacket),
fL1MUV3EmptyPacket(c.fL1MUV3EmptyPacket), fL1NewCHODEmptyPacket(c.fL1NewCHODEmptyPacket), fL1CHODBadData(c.fL1CHODBadData), fL1KTAGBadData(c.fL1KTAGBadData),
fL1LAVBadData(c.fL1LAVBadData), fL1IRCSACBadData(c.fL1IRCSACBadData), fL1StrawBadData(c.fL1StrawBadData), fL1MUV3BadData(c.fL1MUV3BadData),
fL1NewCHODBadData(c.fL1NewCHODBadData), fL1StrawOverflow(c.fL1StrawOverflow), fNCHODHits(c.fNCHODHits), fNKTAGSectorsL0TP(c.fNKTAGSectorsL0TP),
fNKTAGSectorsCHOD(c.fNKTAGSectorsCHOD), fNLAVHits(c.fNLAVHits), fNIRCSACHits(c.fNIRCSACHits), fNStrawTracks(c.fNStrawTracks), fNMUV3Tiles(c.fNMUV3Tiles),
fNNewCHODHits(c.fNNewCHODHits), fHLTTracks(c.fHLTTracks){}

HLTEvent::~HLTEvent(){
  fHLTTracks.clear();
}

void HLTEvent::Clear(Option_t* /*option*/){
  fCHODTime = 0.;
  fNewCHODTime = 0.;
  fL0TPRefTime = 0;

  fL1CHODResponse = 0;
  fL1KTAGResponse = 0;
  fL1LAVResponse = 0;
  fL1IRCSACResponse = 0;
  fL1StrawResponse = 0;
  fL1MUV3Response = 0;
  fL1NewCHODResponse = 0;

  fL1CHODProcessed = false;
  fL1KTAGProcessed = false;
  fL1LAVProcessed = false;
  fL1IRCSACProcessed = false;
  fL1StrawProcessed = false;
  fL1MUV3TriggerMultiProcessed = false;
  fL1MUV3TriggerLeftRightProcessed = false;
  fL1MUV3TriggerNeighboursProcessed = false;
  fL1NewCHODProcessed = false;

  fL1CHODEmptyPacket = true;
  fL1KTAGEmptyPacket = true;
  fL1LAVEmptyPacket = true;
  fL1IRCSACEmptyPacket = true;
  fL1StrawEmptyPacket = true;
  fL1MUV3EmptyPacket = true;
  fL1NewCHODEmptyPacket = true;

  fL1CHODBadData = false;
  fL1KTAGBadData = false;
  fL1LAVBadData = false;
  fL1IRCSACBadData = false;
  fL1StrawBadData = false;
  fL1MUV3BadData = false;
  fL1NewCHODBadData = false;

  fL1StrawOverflow = false;

  fNCHODHits = 0;
  fNKTAGSectorsL0TP = 0;
  fNKTAGSectorsCHOD = 0;
  fNLAVHits = 0;
  fNIRCSACHits = 0;
  fNStrawTracks = 0;
  fNMUV3Tiles = 0;
  fNNewCHODHits = 0;

  fHLTTracks.clear();
}

void HLTEvent::SetKTAGSectors(UInt_t NSecL0TP, UInt_t NSecCHOD){
  fNKTAGSectorsL0TP = NSecL0TP;
  fNKTAGSectorsCHOD = NSecCHOD;
}

void HLTEvent::SetKTAGProcessInfo(Bool_t is_l1_KTAG_processed, Bool_t is_l1_KTAG_empty_packet, Bool_t is_l1_KTAG_bad_data) {
  fL1KTAGProcessed = is_l1_KTAG_processed;
  fL1KTAGEmptyPacket = is_l1_KTAG_empty_packet;
  fL1KTAGBadData = is_l1_KTAG_bad_data;
}
void HLTEvent::SetKTAGResponse(UChar_t ktag_response) {
  fL1KTAGResponse = ktag_response;
}

void HLTEvent::SetLAVHits(UInt_t l1_LAV_n_hits) {
  fNLAVHits = l1_LAV_n_hits;
}

void HLTEvent::SetLAVProcessInfo(Bool_t is_l1_LAV_processed, Bool_t is_l1_LAV_empty_packet, Bool_t is_l1_LAV_bad_data) {
  fL1LAVProcessed = is_l1_LAV_processed;
  fL1LAVEmptyPacket = is_l1_LAV_empty_packet;
  fL1LAVBadData = is_l1_LAV_bad_data;
}

void HLTEvent::SetLAVResponse(UChar_t lav_response) {
  fL1LAVResponse = lav_response;
}

void HLTEvent::SetSTRAWNTracks(UInt_t l1_straw_n_tracks) {
  fNStrawTracks = l1_straw_n_tracks;
}

void HLTEvent::SetSTRAWProcessInfo(Bool_t is_l1_straw_processed, Bool_t is_l1_straw_empty_packet, Bool_t is_l1_straw_bad_data, Bool_t is_l1_straw_overflow) {
  fL1StrawProcessed = is_l1_straw_processed;
  fL1StrawEmptyPacket = is_l1_straw_empty_packet;
  fL1StrawBadData = is_l1_straw_bad_data;
  fL1StrawOverflow = is_l1_straw_overflow;
}
void HLTEvent::SetSTRAWResponse(UChar_t straw_response) {
  fL1StrawResponse = straw_response;
}

void HLTEvent::PrintInfo(){
  std::cout << "[HLTEvent]  --> Printing L1TP info!" << std::endl;
  for(UInt_t iMask=0;iMask<fHLTTracks.size();iMask++){
    fHLTTracks[iMask].PrintInfo();
  }
}

ClassImp(HLTTrack)

HLTTrack::HLTTrack(): TObject(){
  Clear();
}

HLTTrack::HLTTrack(const HLTTrack& c) : TObject(c), fHLTTrackID(c.fHLTTrackID),fNHoughIntersections(c.fNHoughIntersections),
  fNHoughAdjacents(c.fNHoughIntersections),fdydz(c.fdydz),fQy(c.fQy),fdxdzBeforeMagnet(c.fdxdzBeforeMagnet),fdxdzAfterMagnet(c.fdxdzAfterMagnet),
  fQxBeforeMagnet(c.fQxBeforeMagnet),fQxAfterMagnet(c.fQxAfterMagnet),fZVertex(c.fZVertex),fPz(c.fPz),fCDA(c.fCDA),fTrailing(c.fTrailing),
  fMomentumBeforeMagnet(c.fMomentumBeforeMagnet), fMomentumAfterMagnet(c.fMomentumAfterMagnet), fVertex(c.fVertex){}

void HLTTrack::Clear(Option_t* /*option*/){
  fHLTTrackID = -1;
  fNHoughIntersections = 0; // number of intersections in the Hough transform
  fNHoughAdjacents = 0;     // number of adjacent points in the Hough transform
  fdydz = 0.;
  fQy = 0.;
  fdxdzBeforeMagnet = 0.;
  fdxdzAfterMagnet = 0.;
  fQxBeforeMagnet = 0.;
  fQxAfterMagnet = 0.;
  fZVertex = 0.;
  fPz = 0.;
  fCDA = 0.;
  fTrailing = 0.;

  fMomentumBeforeMagnet.SetXYZ(0.,0.,0.);
  fMomentumAfterMagnet.SetXYZ(0.,0.,0.);
  fVertex.SetXYZ(0.,0.,0.);
}

void HLTTrack::PrintInfo(){
  std::cout << "[HLTEvent]  * TrackID " << (Int_t)fHLTTrackID << " L1CDA:             " << (Int_t)fCDA             << std::endl;
}
