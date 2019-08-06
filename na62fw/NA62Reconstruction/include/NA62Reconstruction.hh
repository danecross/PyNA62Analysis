#ifndef NA62Reconstruction_H
#define NA62Reconstruction_H

#include "TFile.h"
#include "TChain.h"
#include "TTree.h"
#include "TObjArray.h"
#include "TGraph.h"

#include "NA62VReconstruction.hh"
#include "NA62VDigitizer.hh"
#include "NA62VRawDecoder.hh"
#include "NA62VRawEncoder.hh"
#include "BinaryEvent.hh"
#include "NA62Buffer.hh"
#include "L0TPRawEncoder.hh"
#include "L1TPData.hh"

#include "NA62Timer.hh"
#include "Event.hh"
#include "Stream.hh"
#include "InfoWriter.h"
#include "HLTLibController.hh"
#include <vector>

#define MAXBUFFERWORDS 100000000
#define MAXNDETECTORS 22

class NA62Reconstruction : public NA62VReconstruction {
  public:

    NA62Reconstruction(TObjArray*, TString, TFile*, Int_t, Int_t, Int_t, UInt_t);
    NA62Reconstruction(TString, TString, TString, Int_t, Int_t, Int_t, UInt_t, Int_t);
    ~NA62Reconstruction();
    NA62VRawDecoder * GetRawDecoder() { return 0; }
    void Init(NA62VReconstruction*)   { return;   }
    void StartOfBurst();
    void EndOfBurst();
    void Init();
    void ReadInputList(TString, Int_t);
    virtual TRecoVEvent * ProcessEvent(TDetectorVEvent* = 0, Event* = 0);
    TRecoVEvent* GetRecoEvent(){return 0;};
    virtual TDetectorVEvent * Trigger(TDetectorVEvent* = 0, Event* = 0);
    virtual void EndProcessing();
    virtual void PrintInfo();
    virtual void FillTimes(Double_t aReferenceTime=0.);
    Bool_t NextEvent();
    void CompleteInit();
    void EvaluateTriggerDriftT0();
    void EvaluateInstantaneousIntensity();
    void SetHLTLib(std::shared_ptr<HLTLibController> lib) {
        fHLTLib = lib;
    }

  private:

    TChain * BuildChain(TString);
    void ResetVariables();
    void ResetFileVariables();
    void ResetTotalVariables();
    void InitHistograms();
    void InitLibraries();
    void InitDetectorsInfo();
    void InitRecoRawTable();
    void InitTimers();
    void CleanUpLibraries();
    void CleanUpSequence();
    void CleanUpTimer(UInt_t iTimer);
    void CleanUpTimers(UInt_t);
    void ParseConfigFile(TString);
    void NextFile(Int_t nMaxEvents);
    void NextChunk(Int_t FileDescriptor);
    void WriteFileName(TString newFile);
    Bool_t HandleFile();
    Bool_t DecodeEvent(UInt_t * &pDataBuffer, std::vector<UInt_t> &DataBuffer);
    void   DecodeBurstHeader();
    void   DecodeDIMBlock(UInt_t *pDataBuffer, UInt_t * NextOffset);
    void   ReadSingleEvent(Int_t);
    void   ReadEOB();

    TString CheckProtocols(TString);
    std::pair<Double_t,Double_t> GetMemoryUsage();
    Double_t GetFileSizeUsage(int option=1);
    void CheckSpecialTriggerTimeStamps();
    void RemoveGuestDetector(Int_t,Int_t);

    std::vector<TString>               fHostsForGuestDetectors;     ///< Names of the host detectors
    std::vector<std::vector<TString> > fGuestDetectorNames;         ///< Names of guest detectors [for each host detector]
    std::vector<std::vector<Int_t> >   fGuestDetectorIndices;       ///< DataSource ID's of guest detectors [for each host detector]

    TString fReferenceDetectorName;                                 ///< Name of the reference detector
    Int_t   fReferenceDetectorNHitsMin;                             ///< NHitsMin for using a reference detector's candidate
    NA62VReconstruction* fReferenceDetectorReco;                    ///< Pointer of the reference detector reconstruction

    typedef std::vector<NA62VReconstruction*> RecoVector;       ///< Reconstruction vector
    typedef std::vector<NA62VDigitizer*> DigitizerVector;       ///< Digitization vector
    typedef std::vector<NA62VRawDecoder*> RawDecoderVector;     ///< RawDecoding vector
    typedef std::vector<NA62VRawEncoder*> RawEncoderVector;     ///< RawEncoding vector
    typedef std::vector<TDetectorVEvent*> EventVector;          ///< Event vector
    typedef std::vector<TRecoVEvent*> RecoEventVector;          ///< Reco output vector
    typedef std::vector<TSlimRecoVEvent*> SlimRecoEventVector;  ///< SlimReco output vector

  public:

    float FindT10(int);

    Event *              GetMCTruthEvent()                                  { return fMCTruthEvent;                    };
    void                 SetMCTruthEvent(Event * value)                     { fMCTruthEvent = value;                   };
    Stream *             GetStream()                                        { return fStream;                          };
    void                 SetStream(Stream * value)                          { fStream = value;                         };

    EventVector *        GetInputEvents()                                   { return &fInputEvents;                    };
    EventVector *        GetEvents()                                        { return &fEvents;                         };
    BinaryEvent *        GetBinaryEvent()                                   { return &fBinaryEvent;                    };
    RecoEventVector *    GetRecoEvents()                                    { return &fRecoEvents;                     };
    std::vector<TString> GetRecoSequence()                                  { return fRecoSequence;                    };
    Int_t                GetNReconstructions()                              { return fNReconstructions;                };

    Int_t                GetNEvt()                                          { return fNEvt;                            };
    void                 SetNEvt(Int_t value)                               { fNEvt = value;                           };
    Int_t                GetDisplayPeriod()                                 { return fDisplayPeriod;                   };
    void                 SetDisplayPeriod(Int_t value)                      { fDisplayPeriod = value;                  };

    Long_t               GetNWordsInChunk()                                 { return fNWordsInChunk;                   };
    void                 SetNWordsInChunk(Int_t value)                      { fNWordsInChunk = value;                  };
    Long_t               GetNWordsInFile()                                  { return fNWordsInFile;                    };
    void                 SetNWordsInFile(Int_t value)                       { fNWordsInFile = value;                   };
    Long_t               GetNReadEventsInFile()                             { return fNReadEventsInFile;               };
    void                 SetNReadEventsInFile(Int_t value)                  { fNReadEventsInFile = value;              };
    Long_t               GetNReadEventsInTotal()                            { return fNReadEventsInTotal;              };
    void                 SetNReadEventsInTotal(Int_t value)                 { fNReadEventsInTotal = value;             };
    Long_t               GetNReadControlTriggerEventsInFile()               { return fNReadControlTriggerEventsInFile; };
    Long_t               GetNReadPeriodicTriggerEventsInFile()              { return fNReadPeriodicTriggerEventsInFile;};
    Long_t               GetNReadCalibrationTriggerEventsInFile()           { return fNReadCalibrationTriggerEventsInFile;};
    UInt_t               GetNReadSOBEventsInFile()                          { return fNReadSOBEventsInFile;            };
    UInt_t               GetNReadSOBEventsInTotal()                         { return fNReadSOBEventsInTotal;           };
    UInt_t               GetNReadEOBEventsInFile()                          { return fNReadEOBEventsInFile;            };
    UInt_t               GetNReadEOBEventsInTotal()                         { return fNReadEOBEventsInTotal;           };
    Long_t               GetNProcessedEventsInFile()                        { return fNProcessedEventsInFile;          };
    void                 SetNProcessedEventsInFile(Int_t value)             { fNProcessedEventsInFile = value;         };
    Long_t               GetNProcessedEventsInTotal()                       { return fNProcessedEventsInTotal;         };
    void                 SetNProcessedEventsInTotal(Int_t value)            { fNProcessedEventsInTotal = value;        };
    Long_t               GetNProcessedControlTriggerEventsInFile()          { return fNProcessedControlTriggerEventsInFile;  };
    Long_t               GetNProcessedPeriodicTriggerEventsInFile()         { return fNProcessedPeriodicTriggerEventsInFile; };
    Long_t               GetNProcessedCalibrationTriggerEventsInFile()      { return fNProcessedCalibrationTriggerEventsInFile; };
    UInt_t               GetNProcessedSOBEventsInFile()                     { return fNProcessedSOBEventsInFile;       };
    UInt_t               GetNProcessedSOBEventsInTotal()                    { return fNProcessedSOBEventsInTotal;      };
    UInt_t               GetNProcessedEOBEventsInFile()                     { return fNProcessedEOBEventsInFile;       };
    UInt_t               GetNProcessedEOBEventsInTotal()                    { return fNProcessedEOBEventsInTotal;      };
    Long_t               GetNSkippedDownscaledEventsInFile()                { return fNSkippedDownscaledEventsInFile;  };
    Int_t                GetDownscalingFactor()                             { return fDownscalingFactor;               };
    void                 SetDownscalingFactor(Int_t value)                  { fDownscalingFactor = value;              };
    Int_t                GetOnlineMonitorMode()                             { return fOnlineMonitorMode;               };
    void                 SetOnlineMonitorMode(Int_t value)                  { fOnlineMonitorMode = value;              };
    Bool_t               GetSaveOnlineMonitorPlots()                        { return fSaveOnlineMonitorPlots;          };
    void                 SetSaveOnlineMonitorPlots(Bool_t value)            { fSaveOnlineMonitorPlots = value;         };
    TString              GetSaveOnlineMonitorPlotsDir()                     { return fSaveOnlineMonitorPlotsDir;       };
    void                 SetSaveOnlineMonitorPlotsDir(TString value)        { fSaveOnlineMonitorPlotsDir = value;      };
    TString              GetOnlineMonitorReferenceFileName()                { return fOnlineMonitorReferenceFileName;  };
    void                 SetOnlineMonitorReferenceFileName(TString value)   { fOnlineMonitorReferenceFileName = value; };
    Bool_t               GetRawFileEOF()                                    { return fRawFileEOF;                      };
    void                 SetRawFileEOF(Bool_t value)                        { fRawFileEOF = value;                     };
    Int_t                GetDataFormat()                                    { return fDataFormat;                      };
    void                 SetDataFormat(Int_t value)                         { fDataFormat = value;                     };
    Int_t                GetL0TPFineTimeBit()                               { return fL0TPFineTimeBit;                 };

    TString              GetCurrentFileName()                               { return fCurrentFileName;                 };
    TString              GetDnsServerName()                                 { return fDnsServerName;                   };
    TString              GetDIMRecoveryFileName()                           { return fDIMRecoveryFileName;             };
    Int_t                GetSkipEventsLevel()                               { return fSkipEventsLevel;                 };
    ULong64_t            GetSkipEventsMask()                                { return fSkipEventsMask;                  };
    Int_t                GetOutputStatus()                                  { return fOutputStatus;                    };
    void                 SetOutputStatus(Int_t value)                       { fOutputStatus = value;                   };
    Bool_t               GetFillTimesEnabled()                              { return fFillTimesEnabled;                };
    void                 SetFillTimesEnabled(Bool_t value)                  { fFillTimesEnabled = value;               };
    Double_t             GetTriggerDriftT0()                                { return fTriggerDriftT0;                  };
    TH1D*                GetHEventTimeStamp()                               { return fHEventTimeStamp;                 };
    TH1D*                GetHSkippedEventTimeStamp()                        { return fHSkippedEventTimeStamp;          };
    TH1D*                GetHNCriticalEventsPerDetector()                   { return fHNCriticalEventsPerDetector;     };
    TH1D*                GetHNEventsWithQualityWarningsPerDetector()        { return fHNEventsWithQualityWarningsPerDetector; };
    TH1D*                GetHEventTimeStampBits()                           { return fHEventTimeStampBits;             };
    TH1D*                GetHEventTimeStamp16()                             { return fHEventTimeStamp16;               };
    TH1D*                GetHEventTimeStamp128()                            { return fHEventTimeStamp128;              };
    TH1D*                GetHEventTimeStamp1024()                           { return fHEventTimeStamp1024;             };

    TH1D*                GetHEventSize()                                    { return fHEventSize;                      };
    TH1D*                GetHDeltaTimeStamp()                               { return fHDeltaTimeStamp;                 };
    TH1D*                GetHDeltaTimeStampStored()                         { return fHDeltaTimeStampStored;           };
    TH2F*                GetHL0TriggerFlags()                               { return fHL0TriggerFlags;                 };
    TH2F*                GetHL1TriggerFlags()                               { return fHL1TriggerFlags;                 };
    TH1F*                GetHL1Counters()                                   { return fHL1Counters;                     };
    Int_t                GetL1Counter(Int_t iCounter) { if(!fHL1Counters) return 0; return fHL1Counters->GetBinContent(iCounter); };

    TString              GetL1TriggerType(UInt_t L1TriggerBit);
    TProfile*            GetHTimingProfile()                                { return fHTimingProfile;                  };
    TH2F*                GetHTiming2D()                                     { return fHTiming2D;                       };
    TGraph*              GetGVirtMem()                                      { return fGVirtMem;                        };
    TGraph*              GetGResMem()                                       { return fGResMem;                         };
    void                 PrintT0Settings(std::ostream &os);
    void                 PrintRecoSummary(std::ostream &os);
    void                 PrintWarning(std::stringstream &ss);
    Bool_t               GetChunkIsFinished()  { return fpDataBuffer>=fDataBuffer.data()+fNWordsInChunk; }; //for OM purposes only

    // Stream additional info
    void                 SetKaonRate(Double_t val)                          { fKaonRate = val;                         };
    void                 SetKaonRateError(Double_t val)                     { fKaonRateError = val;                    };

    Double_t             GetChokeONTimeInFile()                             { return fChokeONTimeInFile;               };

    // these two functions should eventually become private! [currently needed by NA62OnlineMonitor/NA62EventDisplay respectively]
    NA62VReconstruction * FindReco(TString);
    TDetectorVEvent     * FindMCEvent(TString);
    // this function should eventually be removed..
    Bool_t GetIsRawData() { return fIsRawData; }

  private:

    Bool_t fIsRawData;

    TRecoVEvent         * FindRecoEvent(TString);
    TSlimRecoVEvent     * FindSlimRecoEvent(TString);
    NA62VDigitizer      * FindDigi(TString);
    NA62VRawDecoder     * FindRaw(TString);
    NA62VRawEncoder     * FindBinary(TString);
    TTree               * FindOutputTree(TString);
    EventVector         * FindOutputBranchEvent(TString);
    TChain              * FindMCChain(TString);

    std::shared_ptr<HLTLibController> fHLTLib;

    Int_t fInputFileDescriptor;          ///< Input file descriptor
    TObjArray * fInputFileNameList;      ///< Input file names list
    TString     fInputListFileName;      ///< Input list file name
    TString     fCurrentFileName;        ///< Current input file name
    Bool_t      fContinuousReading;      ///< Continuously refreshing file list and waiting if missing
    Int_t       fSkipEventsLevel;        ///< Flag to define the treatment of bad events
    ULong64_t   fSkipEventsMask;         ///< Flag to enable/disable the check for bad events on each subdetector
    Int_t fiFile;                        ///< Current file counter
    Int_t fNFiles;                       ///< Total number of files

    // RecoStages - bitwise definition:
    // bit 0-3: spares
    // bit 4:   histos enabled(1)/disabled(0)
    // bit 5:   reconstruction enabled(1)/disabled(0)
    // bit 6:   reco tree enabled(1)/disabled(0)
    // bit 7:   digi tree enabled(1)/disabled(0)
    // bit >=8: spares
    enum RecoStage {
      kStageNone      = 0x00,             ///< No processing
      kStageRawHistos = 0x10,             ///< Only digitization, no tree saved
      kStageHistos    = 0x30,             ///< Full reconstruction, no tree saved
      kStageReco      = 0x70,             ///< Full reconstruction
      kStageSlimReco  = 0x71,             ///< Slim reconstruction
      kStageBinary    = 0x72,             ///< Binary conversion
      kStageDigis     = 0x90,             ///< Only digitization
      kStageRecoDigis = 0xf0,             ///< Full reconstruction + digi tree
    };
    enum RecoStageBits {
      kHistoEnabled     = 1<<4,
      kRecoEnabled      = 1<<5,
      kRecoTreeEnabled  = 1<<6,
      kDigiTreeEnabled  = 1<<7
    };

    TString fCedarConfigFileName;        ///< Cedar configuration filename
    TString fCHANTIConfigFileName;       ///< CHANTI configuration filename
    TString fCHODConfigFileName;         ///< CHOD configuration filename
    TString fGigaTrackerConfigFileName;  ///< GigaTracker configuration filename
    TString fHACConfigFileName;          ///< HAC configuration filename
    TString fIRCConfigFileName;          ///< IRC configuration filename
    TString fLAVConfigFileName;          ///< LAV configuration filename
    TString fLKrConfigFileName;          ///< LKr configuration filename
    TString fMUV0ConfigFileName;         ///< MUV0 configuration filename
    TString fMUV1ConfigFileName;         ///< MUV1 configuration filename
    TString fMUV2ConfigFileName;         ///< MUV2 configuration filename
    TString fMUV3ConfigFileName;         ///< MUV3 configuration filename
    TString fNewCHODConfigFileName;      ///< NewCHOD configuration filename
    TString fRICHConfigFileName;         ///< RICH configuration filename
    TString fSACConfigFileName;          ///< SAC configuration filename
    TString fSpectrometerConfigFileName; ///< Spectrometer configuration filename
    TString fSAVConfigFileName;          ///< SAV configuration filename

    TString fDnsServerName;              ///< DNS server address for dim communication
    TString fDIMRecoveryFileName;        ///< Name of the file input to retrieve information in case of lost DIM communication
    Bool_t  fDIMRecoveryMode;            ///< Switched true to recover information in case of lost DIM communication

    Int_t fOutputStage;                  ///< Output stage flags
    TString fOutputStageName;            ///< Output stage name read from configuration file
    TString fRequestedInputStage;        ///< InputStage read from configuration file
    std::vector<TString> fRecoSequence;  ///< Vector of Reconstruction names read from configuration file

    RecoVector fRecoLibrary;             ///< STL vector of NA62VReconstruction of all available Reconstructions
    RecoVector fReconstructions;         ///< STL vector of NA62VReconstruction for configurable running sequence
    Int_t fNReconstructions;             ///< Number of active Reconstructions

    DigitizerVector fDigiLibrary;        ///< STL vector of NA62VDigitizer of all available Digitizers
    DigitizerVector fDigitizers;         ///< STL vector of NA62VDigitizer for configurable running sequence
    Int_t fNDigitizers;                  ///< Number of active Digitizers

    RawDecoderVector fRawLibrary;        ///< STL vector of NA62VRawDecoder of all available RawDecoders
    RawDecoderVector fRawDecoders;       ///< STL vector of NA62VRawDecoder for configurable running sequence
    Int_t fNRawDecoders;                 ///< Number of active RawDecoders

    RawEncoderVector fRawEncLibrary;     ///< STL vector of NA62VRawEncoder of all available RawEncoders
    RawEncoderVector fRawEncoders;       ///< STL vector of NA62VRawEncoder for configurable running sequence
    Int_t fNRawEncoders;                 ///< Number of active RawEncoders

    Int_t  fDataFormat;                            ///< To encode MC events, read from configuration file
    Int_t  fL0TPFineTimeBit;                       ///< L0TP fine time bit (0: 25 ns, 1: 12.5ns, 2: 6.25 ns, ...)
    UInt_t fiCurrentEventInFile;                   ///< Index of the current event
    UInt_t fNWordsInChunk;                         ///< Total number of words loaded in fDataBuffer for the current chunk of data
    UInt_t fNWordsInFile;                          ///< Total number of words loaded in fDataBuffer from the beginning of the current file
    UInt_t fNReadEventsInFile;                     ///< Total number of events loaded in fDataBuffer from the beginning of the current file
    UInt_t fNReadEventsInTotal;                    ///< Total number of events loaded in fDataBuffer in total
    UInt_t fNReadPhysicsTriggerEventsInFile;       ///< Total number of physics trigger events from the beginning of the current file
    UInt_t fNReadPhysicsTriggerEventsInTotal;      ///< Total number of physics trigger events in total
    UInt_t fNReadControlTriggerEventsInFile;       ///< Total number of control trigger events from the beginning of the current file
    UInt_t fNReadControlTriggerEventsInTotal;      ///< Total number of control trigger events in total
    UInt_t fNReadPeriodicTriggerEventsInFile;      ///< Total number of periodic trigger events from the beginning of the current file
    UInt_t fNReadPeriodicTriggerEventsInTotal;     ///< Total number of periodic trigger events in total
    UInt_t fNReadCalibrationTriggerEventsInFile;   ///< Total number of calibration trigger events from the beginning of the current file
    UInt_t fNReadCalibrationTriggerEventsInTotal;  ///< Total number of calibration trigger events in total
    UInt_t fNReadSpecialTriggerEventsInFile;       ///< Total number of special trigger events from the beginning of the current file
    UInt_t fNReadSpecialTriggerEventsInTotal;      ///< Total number of special trigger events in total
    UInt_t fNReadSOBEventsInFile;                  ///< Total number of SOB events from the beginning of the current file
    UInt_t fNReadSOBEventsInTotal;                 ///< Total number of SOB events in total
    UInt_t fNReadEOBEventsInFile;                  ///< Total number of EOB events from the beginning of the current file
    UInt_t fNReadEOBEventsInTotal;                 ///< Total number of EOB events in total
    UInt_t fNProcessedEventsInFile;                ///< Total number of events processed from the beginning of the current file
    UInt_t fNProcessedEventsInTotal;               ///< Total number of events processed in total
    UInt_t fNProcessedPhysicsTriggerEventsInFile;  ///< Total number of physics trigger events from the beginning of the current file
    UInt_t fNProcessedPhysicsTriggerEventsInTotal; ///< Total number of physics trigger events in total
    UInt_t fNProcessedControlTriggerEventsInFile;  ///< Total number of control trigger events from the beginning of the current file
    UInt_t fNProcessedControlTriggerEventsInTotal; ///< Total number of control trigger events in total
    UInt_t fNProcessedPeriodicTriggerEventsInFile; ///< Total number of periodic trigger events from the beginning of the current file
    UInt_t fNProcessedPeriodicTriggerEventsInTotal;///< Total number of periodic trigger events in total
    UInt_t fNProcessedCalibrationTriggerEventsInFile; ///< Total number of calibration trigger events from the beginning of the current file
    UInt_t fNProcessedCalibrationTriggerEventsInTotal;///< Total number of calibration trigger events in total
    UInt_t fNProcessedSpecialTriggerEventsInFile;  ///< Total number of special trigger events from the beginning of the current file
    UInt_t fNProcessedSpecialTriggerEventsInTotal; ///< Total number of special trigger events in total
    UInt_t fNProcessedSOBEventsInFile;             ///< Total number of SOB events from the beginning of the current file
    UInt_t fNProcessedSOBEventsInTotal;            ///< Total number of SOB events in total
    UInt_t fNProcessedEOBEventsInFile;             ///< Total number of EOB events from the beginning of the current file
    UInt_t fNProcessedEOBEventsInTotal;            ///< Total number of EOB events in total
    UInt_t fNSkippedEventsInFile;                  ///< Total number of events skipped from the beginning of the current file
    UInt_t fNSkippedEventsInTotal;                 ///< Total number of events skipped in total
    UInt_t fNSkippedCorruptedEventsInFile;         ///< Total number of corrupted events skipped from the beginning of the current file
    UInt_t fNSkippedCorruptedEventsInTotal;        ///< Total number of corrupted events skipped in total
    UInt_t fNSkippedDownscaledEventsInFile;        ///< Total number of downscaled events skipped from the beginning of the current file
    UInt_t fNSkippedDownscaledEventsInTotal;       ///< Total number of downscaled events skipped in total
    UInt_t fNSkippedUnrequestedEventsInFile;       ///< Total number of unrequested events skipped from the beginning of the current file
    UInt_t fNSkippedUnrequestedEventsInTotal;      ///< Total number of unrequested events skipped in total
    UInt_t fNSkippedFilesInTotal;                  ///< Total number of skipped files in total
    UInt_t fNCriticalEventsInFile;                 ///< Total number of events with critical errors from the beginning of the current file
    UInt_t fNCriticalEventsInTotal;                ///< Total number of events with critical errors in total
    Int_t  fDownscalingFactor;                     ///< Downscaling factor for processed events [used only if continuous reading is enabled]
    Int_t  fOnlineMonitorMode;           ///< Flag to select Shifter/Expert OM mode
    Bool_t fSaveOnlineMonitorPlots;      ///< Flag to enable (1) or disable (0) the saving of Online Monitor plots
    TString fSaveOnlineMonitorPlotsDir;  ///< Directory where the Online Monitor plots are stored, if saving of Online Monitor plots is enabled
    TString fOnlineMonitorReferenceFileName; ///< Name of the Online Monitor reference file
    Bool_t fCheckErrno;                  ///< Flag to enable (1) or disable (0) the checking of errno
    Int_t  fWarningsLevel;               ///< Handle the warning verbosity

    std::vector<UInt_t> fDataBuffer;     ///< Data buffer for binary decoding
    UInt_t fTmpBuffer[2];                ///< Temporary data buffer required by chunk-style reading
    UInt_t * fpDataBuffer;               ///< Pointer to the current position being read in fDataBuffer
    Bool_t fRawFileEOF;                  ///< End of file flag for raw files
    Bool_t fErrorInReading;              ///< Flag set to true if an error during the reading of the input file occur
    ULong64_t fCurrentOffsetInFile;      ///< Current Offset (in bytes) of the data read from the input file

    EventVector fInputEvents;            ///< Vector of subdetector events coming from TTrees or raw decoding
    EventVector fEvents;                 ///< Vector of subdetector events intermediate during processing
    BinaryEvent fBinaryEvent;            ///< Vector of subdetector events coming from encoding and used to produce binary data-like output
    RecoEventVector fRecoEvents;         ///< Vector of subdetector RecoEvents which hold the reconstructed information
    SlimRecoEventVector fSlimRecoEvents; ///< Vector of subdetector SlimRecoEvents which hold the reconstructed information

    std::vector<TTree*>  fOutputTrees;             ///< Vector of TTrees for output storage
    std::vector<EventVector*> fOutputBranchEvents; ///< Vector of subdetector events which will be stored on disk
    TTree * fStreamsOutputTree;                    ///< Streams TTree for output storage
    Stream *  fStream;                             ///< Container of settings for reco files
    std::vector<TChain *> fMCChains;               ///< Dedicated TChains for MC information reading
    Event *   fMCTruthEvent;                       ///< MCTruth event read by MC file
    Int_t     fNInputBranches;                     ///< Number of branches in the TChain
    Bool_t    fFillTimesEnabled;                   ///< Flag set to true if T0 evaluation is enabled (via config file)
    std::vector<TString> fT0ReferenceDetectors;    ///< Name of the detector used as a reference for the T0 evaluation
    std::vector<Int_t>   fT0NHitsMinValues;        ///< Minimum number of hits required for reference detector candidates
    Bool_t   fEvaluateTriggerDriftT0;              ///< Flag to enable/disable the TriggerDriftT0 evaluation
    Double_t fTriggerDriftT0;                      ///< Trigger drift T0 to be added to Stations T0s (ns)
    TString  fTriggerDriftT0FileInput;             ///< Trigger drift T0 file name
    Long_t   fBurstTime;                           ///< Burst time (read from RunTimes.dat, for MC )

    Int_t   fNEvt;                                 ///< Number of events to be processed (from -n option)
    Int_t   fNEvtPerFile;                          ///< Number of events to be processed for each file (from -N option)
    Int_t   fJumpNEvt;                             ///< Number of events to be jumped before starting the processing (from -j option)
    UInt_t  fGlobalSeed;                           ///< Global seed for digitization (from -s option)
    Int_t   fDisplayPeriod;                        ///< Interval between progress print out
    TGraph* fGVirtMem;                             ///< Virtual Memory usage monitor
    TGraph* fGResMem;                              ///< Resident Memory usage monitor
    TGraph* fGFileSize;                            ///< Memory usage monitor
    TGraph* fGSystemFileSize;                      ///< Memory usage monitor

    // Stream additional info
    Double_t fKaonRate;                            ///< Kaon rate [MHz] estimated from Cedar
    Double_t fKaonRateError;                       ///< Error of the Kaon rate [MHz] estimated from Cedar
    Double_t fChokeONTimeInFile;                   ///< Time [ns] of with Choke ON
    Double_t fChokeONTimeInTotal;                  ///< Time [ns] of with Choke ON

    Bool_t   fChokeON;                             ///< Choke is ON
    Double_t fChokeONStartTime;                    ///< Start of a Choke ON period, needed for the ChokeON time evaluation
    Double_t fChokeONEndTime;                      ///< End of a Choke ON period, needed for the ChokeON time evaluation

    // Online Monitor histos
    TH1D *fHNEventsProcessedPerBurst;
    TH1D *fHEventSize;
    TH1D *fHEventTimeStamp;
    TH1D *fHSkippedEventTimeStamp;
    TH1D *fHEventTimeStampBits;
    TH1D *fHNCriticalEventsPerDetector;
    TH1D *fHNEventsWithQualityWarningsPerDetector;
    TH1D *fHEventTimeStamp16;
    TH1D *fHEventTimeStamp128;
    TH1D *fHEventTimeStamp1024;
    TH1D *fHDeltaTimeStamp;
    TH1D *fHDeltaTimeStampStored;
    TH2F *fHL0TriggerFlags;
    TH2F *fHL1TriggerFlags;
    TH1F *fHL1Counters;
    TProfile * fHTimingProfile;                      ///< Timing profile histogram, bins are algorithms y axis is event time in ms
    TH2F * fHTiming2D;                               ///< Timing 2D histogram, x bins are algorithms and y bins are log10(time/ms)

    TString fRecoRawTable[MAXNDETECTORS];
    Int_t * fNCriticalErrors[MAXNDETECTORS];         ///< Number of critical errors for each raw decoder [for each ROmezzanine] (for debug)
    Int_t * fNQualityWarnings[MAXNDETECTORS];        ///< Number of quality warnings for each raw decoder [for each ROmezzanine] (for debug)
    Int_t * fNWrongSlots[MAXNDETECTORS];             ///< Number of wrong slots for each raw decoder [for each ROmezzanine] (for debug)
    Int_t * fNTotalSlots[MAXNDETECTORS];             ///< Number of total slots for each raw decoder [for each ROmezzanine] (for debug)
    Int_t * fNHitsFromMaskedChannels[MAXNDETECTORS]; ///< Number of wrong slots for each raw decoder [for each ROmezzanine] (for debug)
    std::vector<Int_t> fRawRecoIndex;                ///< Index table used in event loops, derived from the above
    Bool_t fEOBFileEnabled;                          ///< Flag to enable/disable the writing of the EOB file
    Bool_t fLogFileEnabled;                          ///< Flag to enable/disable the writing of the log file
    UInt_t fRequestedTriggersFlag;                   ///< Flag to enable/disable the processing of a certain trigger mask
    FILE *fEOBFileDescriptor;
    FILE *fLogFileDescriptor;
    FILE *fSkippedFileDescriptor;
    TString fSkippedFileName;
    TString fSvcClass;

    std::vector<unsigned int> fRawDecTimers; ///< Timers for algs
    std::vector<unsigned int> fDigiTimers;   ///< Timers for algs
    std::vector<unsigned int> fRecoTimers;   ///< Timers for algs
    std::vector<unsigned int> fGlobalTimers; ///< global timers index
    unsigned int fOpenInputTimer;            ///< Raw file opening
    unsigned int fInputReadTimer;            ///< Read chunk
    NA62Timer fTimer;                        ///< Timer for whole processing

    /// Events with OutputStatus <0 are never processed [fatally corrupted events or downscaled]
    /// if the OutputStatus value is >0, it corresponds to the SkipEventsLevel required to skip that category
    /// Example: Events with kStatusCriticalError are rejected by SkipEventsLevel >= 1;
    ///          Events with kStatusMaskedChannel are rejected by SkipEventsLevel >= 2;
    enum OutputStatus {
      kStatusBadHeader         = -9,  ///< Corrupted event [Bad Header]
      kStatusBadRawID,                ///< Corrupted event [Bad Raw DetectorID]
      kStatusEventTypeMismatch,       ///< At least one detector with a different event type
      kStatusAlreadyProcessed  = -3,  ///< Event rejected because was already processed
      kStatusUnrequested       = -2,  ///< Event rejected because unrequested (unrequested trigger or -j option)
      kStatusDownscaling       = -1,  ///< Event rejected because of downscaling
      kStatusGood              =  0,  ///< Event to be processed
      kStatusCriticalError     =  1,  ///< At least one critical error detected
      kStatusInconsistentTS    =  1,  ///< At least one slot inconsisten with the Trigger TS
      kStatusMaskedChannel     =  2   ///< At least one hit from a masked channel
    };

    Int_t  fOutputStatus;    ///< Check the event status: the event is unconditionally skipped (<0), always processed (==0) or skipped depending on the SkipLevel (>0) [See enum of OutputStatus]

    Int_t  fPreviousEventNumber;      ///< Previous Event Number from the EventHeader (for debug)

    // Burst Header variables
    UInt_t    fBurstHeaderFormat;     ///< Burst Header Format version
    ULong_t   fNEventsInFile;         ///< Number of events in the current file (for data >= 2015 only)
    ULong_t * fEventNumbers;          ///< IDs of all the events in the file
    ULong_t * fTriggerTypes;          ///< Trigger Types of all the events in the file
    ULong_t * fEventOffsets;          ///< Offsets of all the events in the file
    Bool_t  * fEventAlreadyProcessed; ///< Flag to distinguish events which have already been processed

    // L0TPRawEncoder variables
    L0TPRawEncoder *fL0TPRawEncoder;
    Event * fEvent;

    const Int_t fBasketSize = 10000000;
};
#endif
