// ---------------------------------------------------------------
// History:
//
// A major update: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2013-02-14
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-26
//
// ---------------------------------------------------------------

#ifndef CedarReconstruction_H
#define CedarReconstruction_H 1

#include "NA62VReconstruction.hh"
#include "TDCEvent.hh"

#include "TFile.h"
#include "TH1D.h"
#include "TH2F.h"
#include "TPaveText.h"

#include "CedarAlignment.hh"
#include "CedarGeometry.hh"
#include "TDCBRawDecoder.hh"
#include "TCedarSpecialTriggerEvent.hh"

class TCedarDigi;

class CedarReconstruction : public NA62VReconstruction {

  public:

    CedarReconstruction(TFile*, TString);
    CedarReconstruction();
    ~CedarReconstruction();
    void ParseConfFile(TString);
    void ReadSlewingCorrections();

    virtual void Init(NA62VReconstruction*);
    virtual void StartOfBurst();
    virtual void EndOfBurst();

    virtual TRecoVEvent* ProcessEvent(TDetectorVEvent*, Event*);
    virtual TDetectorVEvent* Trigger(TDetectorVEvent*, Event*);
    virtual void EndProcessing();
    virtual void FillTimes(Double_t);

    void InitHistograms();
    void SaveHistograms();
    void ResetHistograms();
    void DeleteHistograms();

    void EvaluateSlewingCorrections();

    void ReconstructCandidates(TRecoVEvent*);
    void TDCEventMonitor(TDCEvent*);

    Int_t    GetMinSectors()                                { return fMinSectors;                      }
    void     SetMinSectors(Int_t value)                     { fMinSectors = value;                     }
    Int_t    GetMinPMTs()                                   { return fMinPMTs;                         }
    void     SetMinPMTs(Int_t value)                        { fMinPMTs = value;                        }
    Double_t GetTimeWindow()                                { return fTimeWindow;                      }
    void     SetTimeWindow(Double_t value)                  { fTimeWindow = value;                     }
 
    void     SetNCandidateClusteringIterations(Int_t value) { fNCandidateClusteringIterations = value; }
    // for digitisation                                                                    
    /// get time response function as text                                                 
    TString  GetPMTTime_Response()                       { return fPMTTime_Response;       }
    /// min time validity for PMTTime_Response                                             
    Double_t GetPMTTime_min()                            { return fPMTTime_min;            }
    /// max time validity for PMTTime_Response                                             
    Double_t GetPMTTime_max()                            { return fPMTTime_max;            }
    /// PMT response Width mean (ns)                                                       
    Double_t GetPMTWidth_mean()                          { return fPMTWidth_mean;          }
    /// PMT response Width sigma (ns)                                                      
    Double_t GetPMTWidth_sigma()                         { return fPMTWidth_sigma;         }
    /// global PMT efficiency                                                              
    Double_t GetPMT_Efficiency()                         { return fPMT_Efficiency;         }
    /// hit merge threshold in ns                                                          
    Double_t GetPMT_MergeThreshold()                     { return fPMT_MergeThreshold;     }

    CedarDIMInfo GetDIMInfo()                            { return fDIMInfo;                }

    Int_t GetNRecoHitsIntegrated(Int_t i)            { return fNRecoHitsIntegrated[i];     }
    Int_t GetNCoincidencesIntegrated(Int_t i)        { return fNCoincidencesIntegrated[i]; }

    //Online Monitor functions
    Int_t GetResetOMAlignmentNEvents()                 { return fResetOMAlignmentNEvents;  }
    void  SetResetOMAlignmentNEvents(Int_t  value)     { fResetOMAlignmentNEvents = value; }

  public:

    TH1D* GetHHitStatus()                                        { return fHHitStatus;                                 }
    TH1D* GetHRecoHitTime()                                      { return fHRecoHitTime;                               }
    TH1D* GetHRecoHitTimeWrtCandidate()                          { return fHRecoHitTimeWrtCandidate;                   }
    TH2F* GetHRecoHitTimeWrtCandidateVsWidth()                   { return fHRecoHitTimeWrtCandidateVsWidth;            }
    TH1D** GetHRecoHitTimeWrtCandidateInSector()                 { return fHRecoHitTimeWrtCandidateInSector;           }
    TH1D* GetHRecoHitTimeWrtCandidateInSector(Int_t i)           { return fHRecoHitTimeWrtCandidateInSector[i-1];      }
    TH1D* GetHTimeWidth()                                        { return fHTimeWidth;                                 }
    TH1D* GetHNRecoHits()                                        { return fHNRecoHits;                                 }
    TH1D* GetHNSectors()                                         { return fHNSectors;                                  }
    TH1D* GetHNCandidates()                                      { return fHNCandidates;                               }
    TH1D* GetHNRecoHitsInCandidate()                             { return fHNRecoHitsInCandidate;                      }
    TH1D* GetHNSectorsInCandidate()                              { return fHNSectorsInCandidate;                       }
    TH1D* GetHNSelectedCandidates()                              { return fHNSelectedCandidates;                       }
    TH1D* GetHNRecoHitsInSelectedCandidate()                     { return fHNRecoHitsInSelectedCandidate;              }
    TH1D* GetHNSectorsInSelectedCandidate()                      { return fHNSectorsInSelectedCandidate;               }
    TH1D* GetHChannelProfileInSector(Int_t iSect,Int_t iTrig)    { return fHChannelProfileInSector[iSect-1][iTrig];    }
    TH2I*** GetHChannelProfileInSectorVis()                      { return fHChannelProfileInSectorVis;                 }
    TH2I* GetHChannelProfileInSectorVis(Int_t iSect,Int_t iTrig) { return fHChannelProfileInSectorVis[iSect-1][iTrig]; }
    TH1D** GetHNRecoHitsInCandidateInSector()                    { return fHNRecoHitsInCandidateInSector;              }
    TH1D* GetHNRecoHitsInCandidateInSector(Int_t iSect)          { return fHNRecoHitsInCandidateInSector[iSect-1];     }
    TH1D** GetHNRecoHitsInSector()                               { return fHNRecoHitsInSector;                         }
    TH1D* GetHNRecoHitsInSector(Int_t iTrig)                     { return fHNRecoHitsInSector[iTrig];                  }
    TH2I* GetHNRecoHitsInSectorVis(Int_t iTrig)                  { return fHNRecoHitsInSectorVis[iTrig];               }
    TH1D* GetHNRecoHitsChannelProfile(Int_t iTrig)               { return fHNRecoHitsChannelProfile[iTrig];            }
    TH1D* GetHAsymUpDown()                                       { return fHAsymUpDown;                                }
    TH1D* GetHAsymSalvJura()                                     { return fHAsymSalvJura;                              }
    TH1D* GetHRecoHitTimeWrtCandidateInFPGA(UInt_t iROBoard, Int_t iFPGA){
      if(iROBoard<=fRawDecoder->GetDecoder()->GetNROBoards() &&
	 static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetNFPGAs() &&
	 iFPGA<=static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetNFPGAs(iROBoard))
	return fHRecoHitTimeWrtCandidateInFPGA[iROBoard][iFPGA];
      return 0;
    }

    Bool_t GetAlignEnabled()                             { return fAlignEnabled;                            }
    TH1  * GetHAlignmentSectorsVis()                     { if(fAlignEnabled) return fAlign->GetOctantHitVis();  else return 0; }
    TH1  * GetHAlignmentPMTsVis()                        { if(fAlignEnabled) return fAlign->GetPMTVis();        else return 0; }
    TH1  * GetHAlignmentSixthsVis()                      { if(fAlignEnabled) return fAlign->GetSixthsVis();     else return 0; }
    TH1  * GetHAlignmentSixthsFitVis()                   { if(fAlignEnabled) return fAlign->GetSixthsFitVis();  else return 0; }
    TH1  * GetHAlignmentOctantsVis()                     { if(fAlignEnabled) return fAlign->GetOctantsVis();    else return 0; }
    TH1  * GetHAlignmentOctantsFitVis()                  { if(fAlignEnabled) return fAlign->GetOctantsFitVis(); else return 0; }
    TH1  * GetHAlignmentCoarseVis()                      { if(fAlignEnabled) return fAlign->GetCoarseVis();     else return 0; }
    TH1  * GetHAlignmentAsymUD()                         { if(fAlignEnabled) return fAlign->GetHAsymUD();       else return 0; }
    TH1  * GetHAlignmentAsymSJ()                         { if(fAlignEnabled) return fAlign->GetHAsymSJ();       else return 0; }
    TGraph * GetGAlignmentSugXYTrend()                   { if(fAlignEnabled) return fAlign->GetGSugXYTrend();   else return 0; }

    void ComputeAlignment()                              { if(fAlignEnabled) fAlign->ComputeAlignment(); }
    void ResetAlignmentHits()                            { if(fAlignEnabled) fAlign->ResetHits();        }
    Long64_t GetAlignmentNEvents()                       { if(fAlignEnabled) return fAlign->GetNEvents();       else return 0; }
    Float_t  GetAlignmentBestX()                         { if(fAlignEnabled) return fAlign->GetBestX();         else return 0; }
    Float_t  GetAlignmentBestY()                         { if(fAlignEnabled) return fAlign->GetBestY();         else return 0; }

    void PrintAlignment( TPaveText * Pave )              { if(fAlignEnabled) fAlign->PrintStats( Pave ); }
    void PrintAsym( TPaveText * Pave )                   { if(fAlignEnabled) fAlign->PrintAsym( Pave );  }

  protected:

    Double_t GetRecoTime(TCedarDigi*);

  private:

    CedarGeometry * fGeometry;

    TString fSlewingCorrFileName;

    Int_t fNCandidateClusteringIterations;

    Int_t          fMinSectors, fMinPMTs;
    Double_t       fTimeWindow;

    Int_t          fNSectors;
    Int_t          fNSectorsEnabled;
    Int_t          fNSectorsOccupancy;
    Int_t*         fSectorOccupancy;
    Bool_t*        fSectorIsEnabled;
    Bool_t         fEnableSlewingCorr;
    Bool_t         fEvaluateSlewingCorr;
    Double_t       fNewMinWidth; //new MinWidth for SlewingCorrection
    Double_t       fNewMaxWidth; //new MaxWidth for SlewingCorrection
    Int_t          fEdgeRequirement;

    //Cedar DIM info
    CedarDIMInfo   fDIMInfo;

    Int_t *        fNRecoHitsIntegrated;
    Int_t *        fNCoincidencesIntegrated;

    void PrintCedarDigitizerParameters();

    // parameters for digitisation
    TString  fPMTTime_Response;    ///< time response function as text
    Double_t fPMTTime_min;         ///< min time validity
    Double_t fPMTTime_max;         ///< max time validity
    Double_t fPMTWidth_mean;       ///< Average width of PMT response (ns)
    Double_t fPMTWidth_sigma;      ///< Smear of width of PMT response (ns)
    Double_t fPMT_Efficiency;      ///< global PMT efficiency: the QE is multiplied by it
    Double_t fPMT_MergeThreshold;  ///< hit merge threshold in ns

    //Online Monitor parameters
    Int_t fResetOMAlignmentNEvents;   //Minimum number of entries required to reset alignment histos

    //CedarAlign class
    Bool_t       fAlignEnabled;
    CedarAlign * fAlign;

    // Kaon rate evaluation
    std::vector<Double_t> fNSelectedCandidatesInTimeSlots;

    // Histograms

    TH1D *fHHitStatus,
         *fHRecoHitTime,
         *fHRecoHitTimeWrtCandidate,
         **fHRecoHitTimeWrtCandidateInSector,
         *fHTimeWidth,
         *fHAsymUpDown,
         *fHAsymSalvJura,
         *fHNRecoHits,
         *fHNSectors,
         *fHNCandidates,
         *fHNSelectedCandidates,
         *fHNRecoHitsInCandidate,
         *fHNRecoHitsInSelectedCandidate,
         *fHNSectorsInCandidate,
         *fHNSectorsInSelectedCandidate,
         **fHNRecoHitsInCandidateInSector,
         ***fHRecoHitTimeWrtCandidateInFPGA;

    TH2F *fHRecoHitTimeWrtCandidateVsWidth,
         *fHRecoHitTimeWrtCandidateVsNHitsPerCandidate,
         *fHRecoHitTimeWrtTriggerVsNHitsPerCandidate,
         *fHCandidateTimeWrtTriggerVsNHitsPerCandidate;

    // Histos for different trigger conditions
    TH1D ***fHChannelProfileInSector,
         ***fHRowProfileInSector,
         **fHNRecoHitsInRow,
         **fHNRecoHitsInSector,
         **fHNRecoHitsChannelProfile;

    TH2I ***fHChannelProfileInSectorVis,
         **fHNRecoHitsInSectorVis;
};

#endif
