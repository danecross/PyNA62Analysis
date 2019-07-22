#ifndef NA62VReconstruction_H
#define NA62VReconstruction_H

#include "TFile.h"

#include "Event.hh"
#include "TDetectorVEvent.hh"
#include "TRecoVEvent.hh"
#include "TSlimRecoVEvent.hh"
#include "TTimeCluster.hh"

#include "NA62VNamedModule.hh"
#include "NA62VChannel.hh"
#include "NA62Global.hh"

#include "TH1D.h"
#include "TH2F.h"

class NA62VRawDecoder;

class NA62VReconstruction : public NA62VNamedModule
{
  public:

    NA62VReconstruction(TFile*, TString, TString);
    explicit NA62VReconstruction(TString);
    virtual ~NA62VReconstruction();
    virtual TRecoVEvent * ProcessEvent(TDetectorVEvent* = 0, Event* = 0) = 0;
    virtual TDetectorVEvent * Trigger(TDetectorVEvent* = 0, Event* = 0) = 0;
    virtual void Init(NA62VReconstruction*) = 0;
    virtual void StartOfBurst() = 0;
    virtual void EndOfBurst() = 0;
    virtual void EndProcessing() = 0; ///< Call from derived classes
    virtual void FillTimes(Double_t) = 0;
    virtual void ParseConfFile(TString);
    static Double_t StrawDrift(Double_t * x, Double_t * par);
    void ReadStationT0s();
    void ReadT0s();
    void PrintT0s();
    void ResetT0s();
    Double_t GetT0Correction(Int_t ChannelID, Int_t iStation);
    Double_t GetT0Correction(TVDigi * Digi);
    TTimeCluster* TimeClustering(TDetectorVEvent*, Double_t);
    void AddHisto(TObject*);

    static void Exception(TString,Int_t = kGenericError);

  public:

    TRecoVEvent *          GetRecoEvent()                                   { return fRecoEvent;                                  };
    void                   SetRecoEvent(TRecoVEvent * value)                { fRecoEvent = value;                                 };
    TSlimRecoVEvent *      GetSlimRecoEvent()                               { return fSlimRecoEvent;                              };
    void                   SetSlimRecoEvent(TSlimRecoVEvent * value)        { fSlimRecoEvent = value;                             };
    NA62VReconstruction *  GetMainReco()                                    { return fMainReco;                                   };
    NA62VRawDecoder *      GetRawDecoder()                                  { return fRawDecoder;                                 };
    void                   SetRawDecoder(NA62VRawDecoder* value)            { fRawDecoder = value;                                };
    NA62VChannel**         GetChannels()                                    { return fChannels;                                   };
    void                   SetChannels(NA62VChannel** val)                  { fChannels = val;                                    };

    Double_t *             GetStationsMCToF()                               { return fStationsMCToF;                              };
    Double_t *             GetStationsT0()                                  { return fStationsT0;                                 };
    Double_t               GetStationT0(Int_t i)                            { if(fStationsT0 && i<fNStations) return fStationsT0[i]; else return 0;};
    Double_t               GetStationMCToF(Int_t i)                         { if(fStationsMCToF && i<fNStations) return fStationsMCToF[i]; else return 0;};

    TFile *                GetHistoFile()                                   { return fHistoFile;                                  };

    Int_t                  GetNStations()                                   { return fNStations;                                  };
    void                   SetNStations(Int_t value)                        { fNStations = value;                                 };
    Int_t                  GetNChannels()                                   { return fNChannels;                                  };
    void                   SetNChannels(Int_t value)                        { fNChannels = value;                                 };

    Bool_t                 GetEnableT0()                                    { return fEnableT0;                                   };
    TString                GetConfigFileName()                              { return fConfigFileName;                             };
    void                   SetConfigFileName(TString val)                   { fConfigFileName = val;                              };
    TString                GetHeaderFileName()                              { return fHeaderFileName;                             };
    void                   SetHeaderFileName(TString val)                   { fHeaderFileName = val;                              };
    TString                GetBinaryFileName()                              { return fBinaryFileName;                             };
    void                   SetBinaryFileName(TString val)                   { fBinaryFileName = val;                              };
    TString                GetT0ReferenceDetector()                         { return fT0ReferenceDetector;                        };
    void                   SetT0ReferenceDetector(TString val)              { fT0ReferenceDetector = val;                         };
    Int_t                  GetT0NHitsMin()                                  { return fT0NHitsMin;                                 };
    void                   SetT0NHitsMin(Int_t val)                         { fT0NHitsMin = val;                                  };
    TString                GetT0FileName()                                  { return fT0FileName;                                 };
    void                   SetT0FileName(TString val)                       { fT0FileName = val;                                  };
    TString                GetCoarseT0FileName()                            { return fCoarseT0FileName;                           };
    void                   SetCoarseT0FileName(TString val)                 { fCoarseT0FileName = val;                            };
    TString                GetRawDecoderSettingsFileName()                  { return fRawDecoderSettingsFileName;                 };
    void                   SetRawDecoderSettingsFileName(TString val)       { fRawDecoderSettingsFileName = val;                  };
    Int_t                  GetDetID()                                       { return fDetID;                                      };
    void                   SetDetID(Int_t value)                            { fDetID=value;                                       };
    TH2F*                  GetHRecoHitTimeWrtReferenceVsROChannel()         { return fHRecoHitTimeWrtReferenceVsROChannel;        };
    TH2F*                  GetHRecoHitTimeWrtReferenceVsROChannelNoT0()     { return fHRecoHitTimeWrtReferenceVsROChannelNoT0;    };
    TH2F*                  GetHRecoHitTimeWrtReferenceVsROChannelNoT0Prim() { return fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim;};

    Int_t                  GetOnlineMonitorAccumulation()                   { return fOnlineMonitorAccumulation;                  };
    void                   SetOnlineMonitorAccumulation(Int_t value)        { fOnlineMonitorAccumulation = value;                 };

    Double_t               GetTimeClusteringWindow()                        { return fTimeClusteringWindow;                       };
    void                   SetTimeClusteringWindow(Double_t value)          { fTimeClusteringWindow = value;                      };

    Int_t                  GetHistosLevel()                                 { return fHistosLevel;                                };
    void                   SetHistosLevel(Int_t value)                      { fHistosLevel = value;                               };

    /// use to get an existing director or create if not already made
    TDirectory * GetOrMakeDir(TDirectory *inDir,TString dirName);

    // histo handling
    TObjArray* GetHistos() { return fHistoArray; }

  protected:

    TFile * fHistoFile;
    NA62VReconstruction * fMainReco;
    NA62VRawDecoder *     fRawDecoder;
    Double_t * fStationsMCToF;
    Double_t * fStationsT0;

    Int_t fNTimeClusters;
    Double_t fTimeClusteringWindow;
    Int_t fiCurrentTimeCluster;

    TRecoVEvent * fRecoEvent;
    TSlimRecoVEvent * fSlimRecoEvent;
    NA62VChannel** fChannels;

    TString   fConfigFileName;
    TString   fRawDecoderSettingsFileName;
    TString   fT0FileName, fCoarseT0FileName;

    TString   fT0ReferenceDetector;     ///< Name of the detector used as a reference for the T0 evaluation
    Int_t     fT0NHitsMin;              ///< Minimum number of hits required for reference detector candidates

    TString   fHeaderFileName;
    TString   fBinaryFileName;

    Int_t     fNStations;
    Int_t     fNChannels;
    Int_t     fDetID;
    Bool_t    fEnableT0;
    Bool_t    fEnableROMezzaninesT0;
    Bool_t    fEnableStationsT0;
    Bool_t    fEnableTriggerDriftT0;
    Bool_t    fChannelHistograms;
    Int_t     fOnlineMonitorAccumulation;   ///< Set the number of bursts after which the OM histos are reset

    TObjArray * fHistoArray;

    Int_t fHistosLevel;
    Int_t fT0HistoMask;                                 ///<Bitwise mask to enable/disable the FineT0 histos
    TH2F *fHRecoHitTimeWrtReferenceVsROChannel,         ///<Histogram to monitor T0 corrections
         *fHRecoHitTimeWrtReferenceVsROChannelNoT0,     ///<Histogram to evaluate T0 corrections
         *fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim; ///<Histogram to evaluate T0 corrections
};
#endif
