#ifndef RICHReconstruction_H
#define RICHReconstruction_H 1

#include <map>

#include "NA62VReconstruction.hh"
#include "TRecoRICHCandidate.hh"
#include "RICHChannel.hh"

#include "TDCEvent.hh"
#include "TRecoRICHEvent.hh"

#include "TGraph.h"
#include "TMinuit.h"
#include "TString.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"
#include "TArc.h"
#include "TLegend.h"
#include "TCanvas.h"
#include "TLatex.h"
#include "TStyle.h"

class TRICHDigi;

class RICHReconstruction : public NA62VReconstruction {

  public:

  RICHReconstruction(TFile*, TString);
  ~RICHReconstruction();
  void ParseConfFile(TString);
  void ReadSlewingCorrections();
  
  virtual void Init(NA62VReconstruction*);
  virtual void StartOfBurst();
  virtual void EndOfBurst();
  
  virtual TRecoVEvent * ProcessEvent(TDetectorVEvent*, Event*);
  virtual TDetectorVEvent * Trigger(TDetectorVEvent*, Event*);
  virtual void EndProcessing();
  virtual void FillTimes(Double_t);
  void TDCEventMonitor(TDCEvent*);
  
  void ReconstructCandidates(TRecoVEvent*);
  void ReconstructPMCandidates(TRecoVEvent*);
  void SetEnableSC(Bool_t value )                                         { fEnableSuperCells = value;              };
  void SetCandClusteringIteretions(Int_t value)                           { fNCandidateClusteringIterations = value;};
  void SetTimeWindow(Int_t value )                                        { fTimeWindow = value;                    };
  void SetMinPMsForEvent(Int_t value)                                     { fMinPMsForEvent = value;                };

  TRecoRICHCandidate * GetCandidate()                                     { return fCandidate;                      };
  void                 SetCandidate(TRecoRICHCandidate * value)           { fCandidate = value;                     };
  
  Int_t                GetNSCChannels()                                   { return fNSCChannels;                    };
  void                 SetNSCChannels(Int_t value)                        { fNSCChannels = value;                   };
  Int_t                GetNPhotoDetectors()                               { return fNPhotoDetectors;                };
  void                 SetNPhotoDetectors(Int_t value)                    { fNPhotoDetectors = value;               };
  Double_t             GetChargeThreshold()                               { return fChargeThreshold;                };
  void                 SetChargeThreshold(Double_t value)                 { fChargeThreshold = value;               };
  Double_t             GetTimeWidthSigma()                                { return fTimeWidthSigma;                 };
  void                 SetTimeWidthSigma(Double_t value)                  { fTimeWidthSigma = value;                };
  Double_t             GetWidthConstant()                                 { return fWidthConstant;                  };
  void                 SetWidthConstant(Double_t value)                   { fWidthConstant = value;                 };
  Int_t                GetEdgeRequirement()                               { return fEdgeRequirement;                };
  void                 SetEdgeRequirement(Int_t value)                    { fEdgeRequirement = value;               };
  Double_t             GetPreviousEventCandidateTime()                    { return fPreviousEventCandidateTime;     };
  void                 SetPreviousEventCandidateTime(Double_t value)      { fPreviousEventCandidateTime = value;    };
  
  void InitHistograms();
  void SaveHistograms();
  
  Int_t FromGeoIDtoSeqID(Int_t);
  
protected:
  
  Double_t GetRecoTime(TRICHDigi*);
  Double_t GetRecoTimeWidth(TRICHDigi*);
  
public:
  
  Bool_t RingFit(TRecoRICHCandidate*);
  TVector2 GetChPosAngCorr(Int_t);
  Bool_t Chi2Fit(TRecoRICHCandidate*);
  Bool_t Chi2FitSpecial();
  static void RingChi2FCN(Int_t &, Double_t *, Double_t &, Double_t *, Int_t);
  static Double_t RingChi2(Double_t *);
  Bool_t MultiRingReco(TRecoRICHCandidate *); 
  Bool_t StartingTriplet(Int_t,TRecoRICHCandidate *);
  Bool_t TripDistance(Int_t,Int_t);
  TRecoRICHCandidate * HitAssociation(TRecoRICHCandidate *);
  void HitReAssociation(TRecoRICHCandidate *,TRecoRICHCandidate *);
  Double_t PtolemyValue(); 
  void Mapping(TRecoRICHCandidate *);
  void FitMultiRing(); 
  void LoadAlignmentParameters(Int_t);
  //void CalculateNewT0s(TString);
  
public:
  
  Int_t                GetMinHitForMulti()                                { return fMinHitForMulti;               };
  void                 SetMinHitForMulti(Int_t value)                     { fMinHitForMulti = value;              };
  Int_t                GetMinTripDist()                                   { return fMinTripDist;                  };
  void                 SetMinTripDist(Int_t value)                        { fMinTripDist = value;                 };
  Int_t                GetMinHitAssociated()                              { return fMinHitAssociated;             };
  void                 SetMinHitAssociated(Int_t value)                   { fMinHitAssociated = value;            };
  Double_t             GetPtolemyCondition()                              { return fPTolCondition;                };
  void                 SetPtolemyCondition(Double_t value)                { fPTolCondition = value;               };

private:


  Int_t fnRICHEvent;
  TRecoRICHCandidate* fCandidate;
  
  TString fSlewingFileName;
  
  Int_t fNCandidateClusteringIterations;
  
  Float_t fTimeWindow;
  
  Int_t fNSCChannels;
  Int_t fNPhotoDetectors;
  Bool_t fEnableSuperCells;
  Bool_t fEnableSlewingCorr;
  Bool_t fEvaluateSlewingCorr;
  Double_t fChargeThreshold;
  Double_t fTimeWidthSigma;
  Double_t fWidthConstant;
  Double_t fPreviousEventCandidateTime;
  Int_t fEdgeRequirement;
  Double_t fMinWidth;
  Double_t fMaxWidth;
  Int_t fNHitsForT0;
  TVector2 fSpotCenterSaleve, fSpotCenterJura;
  TVector2 fAngleRotationSaleve, fAngleRotationJura;
  Float_t fFocalLength;
  TVector2 *fPMsPositions, *fPMsPositionsShifted;
  Bool_t fMultiRingRecoFlag;
  std::vector<TVector2> fHitPos;
  Bool_t fFlagChi2;
  Int_t fNPars;
  TMinuit *fFitter;
  
  Float_t fPTolCondition;
  Int_t fMinHitForMulti;
  Int_t fMinTripDist;
  Int_t fMinHitAssociated;
  Int_t fMinPMsForEvent;
  typedef std::pair<Int_t,TVector3> SortPair;
  SortPair fPosPair;
  std::vector< std::pair<Int_t,TVector3> > fSortMapX;
  std::vector< std::pair<Int_t,TVector3> > fSortMapY;
  std::vector< std::pair<Int_t,TVector3> > fSortMapXY;
  std::vector< std::pair<Int_t,TVector3> > fSortMapYX;
  
  Int_t fEnableRingFit;
  TVector2 *fPtolHitPos; 
  std::vector<Int_t> fPtolHitChID;
  Int_t fnHitRes; 
  Int_t fCandSideAss[8];
  
  TVector2 fSpotCenter;
  TString  fMirrorAlignmentDBName;
  Bool_t fLoadAlignmentFlag;
  
public:
  
  // Histos for monitoring
  TH2F *fHRecoHitTimeWrtReferenceVsSeqChannel;
  TH2F *fHRecoHitTimeWrtReferenceVsSeqChannelNoT0;
  
  TH1F *fHTimeResolution;
  TH2F *fHLeadingTimeVsSlot;
  TH2F *fHTrailingTimeVsSlot;
  TH1D *fHTrailingTime;
  TH1I *fHNTotHits;
  TH1I *fHNHitsPMJura;
  TH1I *fHNHitsPMSaleve;
  TH1I *fHNHitsSCJura;
  TH1I *fHNHitsSCSaleve;
  TH1I *fHNRecoHits;
  TH1I *fHNRecoHitsPMJ;
  TH1I *fHNRecoHitsPMS;
  TH1I *fHNRecoHitsSCJ;
  TH1I *fHNRecoHitsSCS;
  TH1I *fHHitStatus;
  TH1F *fHRingRadius;
  TH1F *fHRingChi2;
  TH1F *fHRingXCenter;
  TH1F *fHRingYCenter;
  TH1F *fHSingleRingTime;
  TH1F *fHRingTime;
  // TH1F *fHOccupancy;
  //  TH1F *fHOccupancyJura;
  //  TH1F *fHOccupancySaleve;
  //  TH1F *fHSuperCellOccupancyJura;
  //  TH1F *fHSuperCellOccupancySaleve;
  TH1F *fHROOccupancy;
  TH1F *fHROOccupancyJura;
  TH1F *fHROOccupancySaleve;
  TH1F *fHSuperCellROOccupancyJura;
  TH1F *fHSuperCellROOccupancySaleve;
  TH1F *fHWidth;
  TH1F *fHPtolemy;
  TH1I *fHNHitperTimeCandidate;
  TH1I *fHNHitperRingCandidate;
  TH1F *fHHitCandidateTimeDiff;
  TH2F *fHPMTIlluminationSaleve;
  TH2F *fHPMTIlluminationJura;
  TH2F *fHPMTFitIlluminationSaleve;
  TH2F *fHPMTFitIlluminationJura;
  TH2F *fHPMTIllumination;
  TH2F *fHSuperCellIlluminationSaleve;
  TH2F *fHSuperCellIlluminationJura;
  TH2F *fHSuperCellIllumination;
  TH1I *fHNRingCandidates;
  TH1I *fHNTimeCandidates;
  TH2F *fHRingChi2VsRingRadius;
  TH2F *fHRingCenter;
  TH2F *fHHitCandidateTimeDiffvsChannel;
  TH2F *fHHitCandidateTimeDiffvsWidth;
  TH2F *fHHitCandidateTimeDiffvsROChannel;
  // TH2F *fHHitLeadingTimevsChannel;
    TH2F *fHHitLeadingTimevsROChannel;
  TH2F *fHHitWidthvsChannel;
  TH2F *fHHitWidthvsROChannel;
  //  TH2F *fHSCLeadingTimevsChannel;
  TH2F *fHSCLeadingTimevsROChannel;
  TH2F *fHSCWidthvsChannel;
  TH2F *fHSCWidthvsROChannel;
  
  
  // Get Histos for monitoring

  TH1F*  GetHTimeResolution()                  {return fHTimeResolution;                 }
  TH2F*  GetHLeadingTimeVsSlot()               {return fHLeadingTimeVsSlot;              }
  TH2F*  GetHTrailingTimeVsSlot()              {return fHTrailingTimeVsSlot;             }
  TH1D*  GetHTrailingTime()                    {return fHTrailingTime;                   }
  TH1I*  GetHNTotHits()                        {return fHNTotHits;                       }
  TH1I*  GetHNHitsPMJura()                     {return fHNHitsPMJura;                    }
  TH1I*  GetHNHitsPMSaleve()                   {return fHNHitsPMSaleve;                  }
  TH1I*  GetHNHitsSCJura()                     {return fHNHitsSCJura;                    }
  TH1I*  GetHNHitsSCSaleve()                   {return fHNHitsSCSaleve;                  }
  TH1I*  GetHNRecoHits()                       {return fHNRecoHits;                      }
  TH1I*  GetHNRecoHitsPMJ()                    {return fHNRecoHitsPMJ;                   }
  TH1I*  GetHNRecoHitsPMS()                    {return fHNRecoHitsPMS;                   }
  TH1I*  GetHNRecoHitsSCJ()                    {return fHNRecoHitsSCJ;                   }
  TH1I*  GetHNRecoHitsSCS()                    {return fHNRecoHitsSCS;                   }
  TH1I*  GetHNRingCandidates()                 {return fHNRingCandidates;                }
  TH1I*  GetHNTimeCandidates()                 {return fHNTimeCandidates;                }
  
  TH1I*  GetHNHitperTimeCandidate()            {return fHNHitperTimeCandidate;           }
  TH1I*  GetHNHitperRingCandidate()            {return fHNHitperRingCandidate;           }
  
  TH1I*  GetHHitStatus()                       {return fHHitStatus;                      }
  TH1F*  GetHRingRadius()                      {return fHRingRadius;                     }
  TH1F*  GetHRingChi2()                        {return fHRingChi2;                       }
  TH1F*  GetHRingXCenter()                     {return fHRingXCenter;                    }
  TH1F*  GetHRingYCenter()                     {return fHRingYCenter;                    }
  TH1F*  GetHSingleRingTime()                  {return fHSingleRingTime;                 }
  TH1F*  GetHRingTime()                        {return fHRingTime;                       }
  // TH1F*  GetHOccupancy()                       {return fHOccupancy;                      }
  //   TH1F*  GetHOccupancyJura()                   {return fHOccupancyJura;                  }
  //  TH1F*  GetHOccupancySaleve()                 {return fHOccupancySaleve;                }
  TH1F*  GetHROOccupancy()                     {return fHROOccupancy;                    }
  TH1F*  GetHROOccupancyJura()                 {return fHROOccupancyJura;                }
  TH1F*  GetHROOccupancySaleve()               {return fHROOccupancySaleve;              }
  TH1F*  GetHSuperCellROOccupancyJura()        {return fHSuperCellROOccupancyJura;       }
  TH1F*  GetHSuperCellROOccupancySaleve()      {return fHSuperCellROOccupancySaleve;     }
  
  TH1F*  GetHWidth()                           {return fHWidth;                          }
  TH1F*  GetHPtolemy()                         {return fHPtolemy;                        }
  TH1F*  GetHHitCandidateTimeDiff()            {return fHHitCandidateTimeDiff;           }
  TH2F*  GetHPMTIlluminationSaleve()           {return fHPMTIlluminationSaleve;          }
  TH2F*  GetHPMTIlluminationJura()             {return fHPMTIlluminationJura;            }
  TH2F*  GetHPMTFitIlluminationSaleve()        {return fHPMTFitIlluminationSaleve;       }
  TH2F*  GetHPMTFitIlluminationJura()          {return fHPMTFitIlluminationJura;         }
  TH2F*  GetHPMTIllumination()                 {return fHPMTIllumination;                }
  TH2F*  GetHSuperCellIlluminationSaleve()     {return fHSuperCellIlluminationSaleve;    }
  TH2F*  GetHSuperCellIlluminationJura()       {return fHSuperCellIlluminationJura;      }
  TH2F*  GetHSuperCellIllumination()           {return fHSuperCellIllumination;          }
  TH2F*  GetHRingChi2VsRingRadius()            {return fHRingChi2VsRingRadius;           }
  TH2F*  GetHRingCenter()                      {return fHRingCenter;                     }
  TH2F*  GetHHitCandidateTimeDiffvsChannel()   {return fHHitCandidateTimeDiffvsChannel;  }
  TH2F*  GetHHitCandidateTimeDiffvsROChannel() {return fHHitCandidateTimeDiffvsROChannel;}
  TH2F*  GetHHitCandidateTimeDiffvsWidth()     {return fHHitCandidateTimeDiffvsWidth;    }
  // TH2F*  GetHHitLeadingTimevsChannel()         {return fHHitLeadingTimevsChannel;        }
  TH2F*  GetHHitLeadingTimevsROChannel()       {return fHHitLeadingTimevsROChannel;      }
  TH2F*  GetHHitWidthvsChannel()               {return fHHitWidthvsChannel;              }
  TH2F*  GetHHitWidthvsROChannel()             {return fHHitWidthvsROChannel;            }
  //  TH2F*  GetHSCLeadingTimevsChannel()          {return fHSCLeadingTimevsChannel;         }
  TH2F*  GetHSCLeadingTimevsROChannel()        {return fHSCLeadingTimevsROChannel;       }
  TH2F*  GetHSCWidthvsChannel()                {return fHSCWidthvsChannel;               }
  TH2F*  GetHSCWidthvsROChannel()              {return fHSCWidthvsROChannel;             }
  
};

#endif
