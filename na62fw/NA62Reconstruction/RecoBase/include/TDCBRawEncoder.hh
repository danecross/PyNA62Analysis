// ---------------------------------------------------------------
// History:
//
// Created by Angela Romano (axr@hep.ph.bham.ac.uk) 2014-09-24
//
// ---------------------------------------------------------------
/// \class TDCBRawEncoder
/// \Brief
/// Common class to all Sub-Detetectors readout with Tel62s
/// performing the encoding into binary output \n
/// Process Digi (from MC and DATA) to generate special data format for L1 trigger tests -
/// see talk in Software meeting - https://indico.cern.ch/event/368718/contribution/4/material/slides/0.pdf
/// \EndBrief
/// 
/// \Detailed
/// Run by setting OutputStage= Binary in NA62Reconstruction.conf \n
/// Inputs set (by Users) in sub-detector configuration files: \n
/// Header and Binary Filenames (full path) \n
/// Sub-detector ID (DetID) \n
/// Number of TEL62s (NROBoards) \n
/// Number of TDC boards per TEL62 (NFPGAs) \n
/// Number of ~25ns (=1/Clock frequency) long frames in DAQ windows (NSlots) \n
/// Index of Last frame (LastSlotID) \n
/// Index of frame matching the Trigger timestamp is 0 by default \n
/// ROMezzaninesT0 of detector station \n
/// Sub-detector configuration files set for encoding MC and DATA events from run 1423 (October 2014) \n 
/// Outputs produced in NA62Reconstruction directory: \n
/// Header file - "SubDet_header.txt" - containing the recipe needed by the pcfarm to read the binary file "SubDet.dat" \n
/// Binary file - "SubDet.dat" - containing the encoded binary output \n
/// For MC event the Timestamp is hardcoded - starting from 1000 and with 200kHz frequency
/// \EndDetailed

#ifndef TDCBRawEncoder_H
#define TDCBRawEncoder_H 1

#include "NA62VRawEncoder.hh"
#include "NA62VReconstruction.hh"
#include "BinaryEvent.hh"
#include "TSpecialTriggerEvent.hh"
#include "TDCEvent.hh"

class TDCBRawEncoder : public NA62VRawEncoder
{

  public:

    explicit TDCBRawEncoder(NA62VReconstruction*);
    ~TDCBRawEncoder();
    virtual BinaryEvent* EncodeNextEvent(TDetectorVEvent *, Bool_t);

    void Init();
    void End();

  private:

    uint32_t * fBuffer;
    uint32_t ** fBufferPerTel62;
    uint32_t *** fSubBuffer;
    uint32_t *** fSubBuffer_tmp;

    Int_t* fNFPGAs;
    Int_t* fNSlots;
    Int_t* fLastSlotID;

    //Int_t fNHitsOutOfSlot;    //for debug
    Int_t fNLeadsOutOfSlot;    //for debug
    Int_t fNTrailsOutOfSlot;   //for debug
    Int_t fNLeadingsOnly;      //for debug
    Int_t fNTrailingsOnly;     //for debug
};
#endif
