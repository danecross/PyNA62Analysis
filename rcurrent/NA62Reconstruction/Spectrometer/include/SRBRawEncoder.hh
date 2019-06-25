// ---------------------------------------------------------------
// History:
//
// Created by Jacopo Pinzino (jacopo.pinzino@cern.ch) 2015-04-27
// Modified by Maria Brigida Brunetti (maria.brigida.brunetti@cern.ch) 2016-01-21
//
// ---------------------------------------------------------------
/// \class SRBRawEncoder
/// \Brief
/// Class Spectrometer readout with SRB
/// performing the encoding into binary output \n
/// Process Digi to generate special data format for L1 trigger tests
/// Can currently only be run on data (Digi from MC do not match)
/// see talk in TDAQ meeting - https://indico.cern.ch/event/470227/contribution/7/attachments/1206083/1757523/MariaBrigidaBrunetti.pdf
/// \EndBrief
/// 
/// \Detailed
/// Run by setting OutputStage= Binary in NA62Reconstruction.conf \n
/// Inputs set (by Users) in Spectrometer configuration file: \n
/// Header and Binary Filenames (full path) \n
/// Sub-detector ID (DetID) \n
/// Number of SRB (NROBoards) \n
/// Number of Slots per SRB (NSlots) \n
/// Offset common to all SRBs (SRBOffset) \n
/// Latency for each SRB (Latency) \n
/// Global Time Offset of detector station \n
/// Sub-detector configuration files set for encoding DATA events from run 4070 (2015) \n 
/// Outputs produced in NA62Reconstruction directory: \n
/// Header file - "Spectrometer_header.txt" - containing the recipe needed by the pcfarm to read the binary file "Spectrometer.dat" \n
/// Binary file - "Spectrometer.dat" - containing the encoded binary output \n
/// \EndDetailed

#ifndef SRBBRawEncoder_H
#define SRBRawEncoder_H 1

#include "NA62VRawEncoder.hh"
#include "NA62VReconstruction.hh"
#include "SRBEvent.hh"
#include "TSpecialTriggerEvent.hh"
#include "BinaryEvent.hh"
#include "TClonesArray.h"

#include <vector>

class SRBRawEncoder : public NA62VRawEncoder
{

  public:

    explicit SRBRawEncoder(NA62VReconstruction*);
    ~SRBRawEncoder();
    virtual BinaryEvent* EncodeNextEvent(TDetectorVEvent *, Bool_t);

    void Init();
    void End();

    BinaryEvent * GetBinaryEvent() { return fBinaryEvent; };

  private:

    uint32_t * fBuffer;
    uint32_t ** fBufferPerSRB;

    Int_t* fNSlots;
    Int_t* fLastSlotID;
    UInt_t fOffset;
    UInt_t* fLatency;
};
#endif
