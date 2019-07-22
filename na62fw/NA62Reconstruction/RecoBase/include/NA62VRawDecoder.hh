#ifndef NA62VRawDecoder_H
#define NA62VRawDecoder_H

#include "Rtypes.h"
#include "NA62VNamedModule.hh"
#include "TDetectorVEvent.hh"
#include "TDigiVEvent.hh"
#include "TSpecialTriggerEvent.hh"
#include "EventHeader.hh"
#include "NA62VReconstruction.hh"

class NA62VRawDecoder : public NA62VNamedModule
{
  public:

    NA62VRawDecoder(NA62VReconstruction*, TString, Int_t NMezzPerFullBoard=0);
    virtual ~NA62VRawDecoder();
    void CreateObjects();
    void Init();
    virtual void Reset();
    virtual void StartOfBurst() = 0;
    virtual void EndOfBurst() = 0;
    virtual TDetectorVEvent * DecodeNextEvent(UInt_t*, EventHeader*, UInt_t*) = 0;
    virtual void EndProcessing();

    NA62VRawDecoder * GetDecoder()                                                  { return fDecoder;                         };
    void SetDecoder(NA62VRawDecoder * value)                                        { fDecoder=value;                          };
    void ParseRawDecoderSettingsFile(TString);
    virtual void FillDigiTimes(Double_t);
    virtual Int_t GetNCriticalErrorTypes() { return 0; };

  public:

    NA62VReconstruction *  GetReco()                                                { return fReco;                            };
    void                   SetReco(NA62VReconstruction * value)                     { fReco = value;                           };
    TDigiVEvent *          GetDigiEvent()                                           { return fDigiEvent;                       };
    void                   SetDigiEvent(TDigiVEvent * value)                        { fDigiEvent = value;                      };
    TSpecialTriggerEvent * GetSpecialTriggerEvent()                                 { return fSpecialTriggerEvent;             };
    void                   SetSpecialTriggerEvent(TSpecialTriggerEvent * value)     { fSpecialTriggerEvent = value;            };
    Int_t                  GetChannelRemap(Int_t chID)                              { if(fChannelRemap && 0<=chID && chID<fNROChannels) return fChannelRemap[chID]; else return -1; };
    Int_t                  GetChannelRO(Int_t chID)                                 { if(fChannelRO && 0<=chID && chID<fChannelROSize) return fChannelRO[chID]; else return -1; };
    Int_t                  GetNROChannels()                                         { return fNROChannels;                     };
    UInt_t                 GetNROBoards()                                           { return fNROBoards;                       };
    void                   SetNROBoards(UInt_t value)                               { fNROBoards=value;                        };
    Int_t                  GetROBoard(Int_t chID)                                   { if(fNROChannels) return (chID*fNROBoards)/fNROChannels; else return 0; };
    Int_t                  GetNROBoardsPerStation(UInt_t i) { if(i<fNROBoardsPerStation.size()) return fNROBoardsPerStation[i]; else return 0;};
    UInt_t                 GetNROMezzanines()                                       { return fNROMezzanines;                   };
    void                   SetNROMezzanines(UInt_t value)                           { fNROMezzanines=value;                    };
    Int_t                  GetROMezzanine(Int_t chID)                               { if(fNROChannels) return chID/(fNROChannels/fNROMezzanines); else return 0; }; //warning: int division!
    UInt_t                 GetNROMezzaninesPerFullBoard()                           { return fNROMezzaninesPerFullBoard;       };
    UInt_t *               GetROMezzanineMasksPerBoard()                            { return fROMezzanineMasksPerBoard;        };
    void                   SetROMezzanineMasksPerBoard(UInt_t * value)              { fROMezzanineMasksPerBoard=value;         };
    UInt_t                 GetROMezzanineMasksPerBoard(UInt_t i) { if(fROMezzanineMasksPerBoard && i<(UInt_t)fNROBoards) return fROMezzanineMasksPerBoard[i]; else return 0;};
    Int_t *                GetNSlots()                                              { return fNSlots;                          };
    void                   SetNSlots(Int_t * value)                                 { fNSlots=value;                           };
    Int_t                  GetNSlots(Int_t i)                                       { if(fNSlots && i<fNROMezzanines) return fNSlots[i]; else return 0;  };
    void                   SetNSlots(Int_t i, Int_t value)                          { if(fNSlots && i<fNROMezzanines) fNSlots[i]=value;                  };
    Int_t *                GetLastSlotID()                                          { return fLastSlotID;                      };
    void                   SetLastSlotID(Int_t * value)                             { fLastSlotID=value;                       };
    Int_t                  GetLastSlotID(Int_t i)                                   { if(fLastSlotID && i<fNROMezzanines) return fLastSlotID[i]; else return 0; };
    void                   SetLastSlotID(Int_t i, Int_t value)                      { if(fLastSlotID && i<fNROMezzanines) fLastSlotID[i]=value;              };
    TH2F *                 GetHDigiTimeRaw()                                        { return fHDigiTimeRaw;                        };
    TH2F *                 GetHDigiTimeRawFine()                                    { return fHDigiTimeRawFine;                    };
    TH2F *                 GetHDigiTimeRawFineVsROChannel()                         { return fHDigiTimeRawFineVsROChannel;         };
    TH2F *                 GetHDecoderErrors()                                      { return fHDecoderErrors;                      };
    void                   SetHDecoderErrors(TH2F *h)         { if(fHDecoderErrors){ delete fHDecoderErrors; }; fHDecoderErrors=h; };
    Double_t *             GetROMezzaninesT0()                                      { return fROMezzaninesT0;                        };
    Double_t               GetROMezzanineT0(Int_t i)                                { if(fROMezzaninesT0 && i<fNROMezzanines) return fROMezzaninesT0[i]; else return 0;};
    void                   SetROMezzaninesT0(Double_t* buffer)                      { fROMezzaninesT0=buffer;                        };
    void                   SetROMezzaninesT0(Int_t i, Double_t value)               { if(fROMezzaninesT0 && i<fNROMezzanines) fROMezzaninesT0[i]=value;};
    Int_t                  GetWarningsLevel()                                       { return fWarningsLevel;                       };
    void                   SetWarningsLevel(Int_t value)                            { fWarningsLevel = value;                      };
    Int_t                  GetNCriticalErrors(Int_t iMezzanine)                     { return fNCriticalErrors[iMezzanine];         }
    void                   SetNCriticalErrors(Int_t iMezzanine,Int_t value)         { fNCriticalErrors[iMezzanine] = value;        }
    Int_t                  GetNQualityWarnings(Int_t iMezzanine)                    { return fNQualityWarnings[iMezzanine];        }
    void                   SetNQualityWarnings(Int_t iMezzanine,Int_t value)        { fNQualityWarnings[iMezzanine] = value;       }
    Int_t                  GetNWrongSlots(Int_t iMezzanine)                         { return fNWrongSlots[iMezzanine];             }
    void                   SetNWrongSlots(Int_t iMezzanine,Int_t value)             { fNWrongSlots[iMezzanine] = value;            }
    Int_t                  GetNTotalSlots(Int_t iMezzanine)                         { return fNTotalSlots[iMezzanine];             }
    void                   SetNTotalSlots(Int_t iMezzanine,Int_t value)             { fNTotalSlots[iMezzanine] = value;            }
    Int_t                  GetNHitsFromMaskedChannels(Int_t iMezzanine)             { return fNHitsFromMaskedChannels[iMezzanine]; }
    void                   SetNHitsFromMaskedChannels(Int_t iMezzanine,Int_t value) { fNHitsFromMaskedChannels[iMezzanine] = value;}
    Bool_t                 GetIsAGuestOrHostDetector()                              { return fIsAGuestOrHostDetector;              }
    void                   SetIsAGuestOrHostDetector(Bool_t value)                  { fIsAGuestOrHostDetector = value;             }
  
  protected:

    NA62VReconstruction * fReco;

    TDigiVEvent * fDigiEvent;
    TSpecialTriggerEvent * fSpecialTriggerEvent;

    NA62VRawDecoder * fDecoder;

    Int_t   fNROBoards;
    Int_t   fNROMezzanines;             ///< Total number of mezzanines (fNROBoards*fNROMezzaninesPerFullBoard)
    Int_t   fNROMezzaninesPerFullBoard; ///< Number of mezzanines of a full board
    UInt_t* fROMezzanineMasksPerBoard;  ///< Mezzanine masks to indicate the enabled mezzanines in each board [one value for each ROBoard]
    std::vector<Int_t> fNROBoardsPerStation;
    Int_t * fChannelRemap;
    Int_t * fChannelRO;
    Int_t   fNROChannels;
    Int_t   fChannelROSize;
    Int_t * fNSlots;                    ///< Number of slots for each mezzanine (array of length fNROMezzanines)
    Int_t   fNSlotsMax;                 ///< Max number of slots
    Int_t * fLastSlotID;                ///< LastSlotID for each mezzanine (array of length fNROMezzanines)
    Int_t   fLastSlotIDMax;             ///< Max LastSlotID
    Bool_t  fIsAGuestOrHostDetector;    ///< Flag to distinguish guest/host detectors from the "standard" ones

    Double_t* fROMezzaninesT0;          ///< Mezzanine T0s (array of length fNROMezzanines)
    Int_t   fWarningsLevel;             ///< Flag to define the treatment of warning printouts
    Int_t * fNCriticalErrors;           ///< Number of critical errors for each mezzanine (array of length fNROMezzanines)
    Int_t * fNQualityWarnings;          ///< Number of quality warnings for each mezzanine (array of length fNROMezzanines)
    Int_t * fNWrongSlots;               ///< Number of wrong slots for each mezzanine (array of length fNROMezzanines)
    Int_t * fNTotalSlots;               ///< Total number of slots for each mezzanine (array of length fNROMezzanines)
    Int_t * fNHitsFromMaskedChannels;   ///< Number of hits from masked channels for each mezzanine (array of length fNROMezzanines)

    TH2F * fHDecoderErrors,   ///< Histogram with decoder errors in [one histo for each detector (MezzanineID on the x)]
         * fHDigiTimeRaw,     ///< Histograms with times for StationsT0 evaluation [one histo for each detector (MezzanineID on the x)]
         * fHDigiTimeRawFine; ///< Histograms with times (wrt trigger fine times) for StationsT0 evaluation [one histo for each detector (MezzanineID on the x)]
    TH2F * fHDigiTimeRawFineVsROChannel; ///< Histograms with leading times (wrt trigger fine times) for each RO channel [one histo for each detector (MezzanineID on the x)]

};
#endif
