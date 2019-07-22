#ifndef LAVDigiBlockHandler_H
#define LAVDigiBlockHandler_H 1

#include "LAVDigiBlock.hh"

#define MAXHITPERBLOCK 10
#define MAXBLOCKS 2500
#define MAXBLOCKMAP 130000

#include <map>

class LAVDigiBlockHandler{

public:
  LAVDigiBlockHandler();
  ~LAVDigiBlockHandler();

  LAVDigiBlock* GetDigiBlock(Int_t, Int_t); // ChannelID, Number of Photons
  void Clear();

  void LoopInit(){fIBlock = fHitBlockMap.begin(); fIBlockCnt = 0;}
  LAVDigiBlock* NextBlock();
  void CompactifyOpticalPhotons();
  void CompactifyLastDynode();
  void ApplyPMT(TRandom3*);
  void Digitize();

  void Print();

private:

  std::map<Int_t,Int_t> fHitBlockMap;  //[MAXBLOCKMAP]; // Block index from index of corresponding low-threshold electronic channel 

  Int_t fHitBlock[MAXBLOCKS];     // BlockID from block index 
  Int_t fNHitBlock[MAXBLOCKS];    // Number of times the block is fired from block index 
  LAVDigiBlock* fDigiBlockList[MAXBLOCKS][MAXHITPERBLOCK]; // Pointers to LAVDigiBlock, per blockIdx and per copy number.

  std::map<Int_t,Int_t>::iterator fIBlock;
  Int_t fIBlockCnt;

};

#endif
