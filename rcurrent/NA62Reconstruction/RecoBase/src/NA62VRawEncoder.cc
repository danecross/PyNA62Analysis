// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-23
//
// ---------------------------------------------------------------

#include "NA62VRawEncoder.hh"

NA62VRawEncoder::NA62VRawEncoder(NA62VReconstruction* Reco, TString Name) :
  NA62VNamedModule(Name),
  fReco(Reco),
  fBinaryEvent(0),
  fSpecialTriggerEvent(0),
  fEncoder(0),
  fDetID(0),
  fNROBoards(0),
  fNROMezzanines(0),
  fTimeStamp(0),
  fFineTime(0),
  fROMezzaninesT0(0),
  fNWords(0),
  fNWordsPerROBoard(0),
  fBinaryFile(0),
  fHeaderFile(0),
  fBinaryFileName(""),
  fHeaderFileName(""),
  fNHitsOutOfSlot(0)
{
}

NA62VRawEncoder::~NA62VRawEncoder(){
  if(fEncoder){ //avoid multi deletion due to fEncoder being of the class NA62VRawEncoder
    std::cout << "Deleting " << fName << " RawEncoder.." << std::endl;
    if(fBinaryEvent){
      delete fBinaryEvent;
      fBinaryEvent=0;
    }
    if(fSpecialTriggerEvent) {
      delete fSpecialTriggerEvent;
      fSpecialTriggerEvent=0;
    }
    delete fEncoder;
    fEncoder=0;
  }
}

void NA62VRawEncoder::Open(){
  fBinaryFile = fopen(fBinaryFileName,"wb");
  if (!fBinaryFile){
    printf("RawEncoder: Unable to open the Binary file!\n");
  }
  fHeaderFile = fopen(fHeaderFileName,"w");
  if (!fHeaderFile){
    printf("RawEncoder: Unable to open the Header file!\n");
  }
}

void NA62VRawEncoder::Close(){
  if(fBinaryFile) fclose(fBinaryFile);
  else printf("RawEncoder: Unable to close the Binary file!\n");

  if(fHeaderFile) fclose(fHeaderFile);
  else printf("RawEncoder: Unable to close the Header file!\n");
}
