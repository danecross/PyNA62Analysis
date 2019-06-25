#ifndef LAVStaticGeometry_H
#define LAVStaticGeometry_H

#include <Rtypes.h>

// The following can be used as directly as numbers:
// Blocks per banana = 4
// Number of stations = 12

#define MAX_LAYER 5
#define MAX_COLUMN 1300
#define MAX_BLOCK_ID 130000


class LAVStaticGeometry {
  
public:
  
  LAVStaticGeometry();
  static LAVStaticGeometry* GetInstance();
  Int_t LayersPerStation();
  Int_t LayersPerStation(Int_t Station);
  Int_t BananasPerLayer();
  Int_t BananasPerLayer(Int_t Station);
  Int_t CompactPerStation();
  Int_t CompactPerStation(Int_t Station);
  void SpecifyChannel(Int_t Station, Int_t Layer, Int_t Banana, Int_t Block);
  void SpecifyChannel(Int_t Station, Int_t Layer, Int_t Position);
  void SpecifyChannel(Int_t Station, Int_t Compact);
  void SpecifyChannel(Int_t BlockID);
  void SpecifyColumn(Int_t Station, Int_t Posiiton);
  void SpecifyColumn(Int_t Column);
  Int_t GetStation();
  Int_t GetLayer();
  Int_t GetBlock();
  Int_t GetBanana();
  Int_t GetPosition();
  Int_t GetColumn();
  Int_t GetCompact();
  Int_t GetBlockID();

private:

  static LAVStaticGeometry* fInstance;

  Int_t fNLayersPerStation[12];
  Int_t fNBananasPerLayer[12];
  Int_t fNCompactPerStation[12];
  Int_t fStation, fLayer, fBanana, fBlock;

};


#endif
