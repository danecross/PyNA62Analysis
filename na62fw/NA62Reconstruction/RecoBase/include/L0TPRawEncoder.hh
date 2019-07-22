// ---------------------------------------------------------------
// History:
//
// Created by LorenzaIacobuzio (lorenza.iacobuzio@cern.ch) June 2016
//
// ---------------------------------------------------------------

#ifndef L0TPRawEncoder_H
#define L0TPRawEncoder_H 1

#include "NA62VRawEncoder.hh"
#include "NA62VReconstruction.hh"
#include "BinaryEvent.hh"
#include "L0TPData.hh"
#include "NA62Global.hh"
 
class L0TPRawEncoder : public NA62VRawEncoder
{

public:
  
  explicit L0TPRawEncoder(NA62VReconstruction*);
  ~L0TPRawEncoder();
  virtual BinaryEvent* EncodeNextEvent(TDetectorVEvent *, Bool_t);
  
  void Init();
  void End();
  
private:
  
  uint32_t * fBuffer;
  L0TPData * fL0TP;
};
#endif
