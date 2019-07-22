#ifndef CHODReconstruction_H
#define CHODReconstruction_H 1


#include "NA62VReconstruction.hh"
#include "TDCEvent.hh"
#include "TCHODDigi.hh"

#include "TFile.h"
#include "TString.h"
#include "TH1D.h"
#include "TH2F.h"
#include "TH3D.h"

#include <vector>

class CHODReconstruction : public NA62VReconstruction
{

  public:

    CHODReconstruction(TFile*, TString);
    ~CHODReconstruction();
    void ParseConfFile(TString);

    virtual void Init(NA62VReconstruction*);
    virtual void StartOfBurst();
    virtual void EndOfBurst();

    void InitHistograms();
    void SaveHistograms();
    virtual TRecoVEvent * ProcessEvent(TDetectorVEvent*, Event*);
    virtual TDetectorVEvent * Trigger(TDetectorVEvent*, Event*);
    virtual void EndProcessing();
    virtual void FillTimes(Double_t);
    Double_t TimeCorrections(TCHODDigi*,TDCEvent*);
    void QuadrantHitAssociation(Int_t, Int_t);
    void ReadSlewCorr();
    void ReadT0();
    void ReadLightVelocities();
    void ReadT0AtPM();
    void ReadTOT();
    void ReadTOTpdf();
    Double_t GetHitEnergyThreshold();
    Double_t GetTOFBetweenCHODPlanes();
    Double_t GetSlewCorrSlope(Int_t, Int_t);
    Double_t GetSlewCorrConst(Int_t, Int_t);
    Double_t GetLowThreshold();
    Double_t GetHighThreshold();
    Int_t GetQuadrant(Int_t);
    Int_t GetPlane(Int_t);
    Double_t GetLightVelocity(Int_t);
    Double_t GetT0AtPM(Int_t);
    Double_t GetTOTAtPM(Int_t);
    Double_t GetTOTSlope(Int_t);
    Double_t GetTOTpdf(Int_t);
    Double_t LightVelocitiesTimeCorrection(Int_t, Int_t);

  private:

    Double_t fNRecoHitsPerBurst;
    Double_t fNCandidatesPerBurst;
    Int_t fNMaxSlabs;
    Double_t fHighTh;
    Double_t fLowTh;
    Double_t fTimeWindowForSlew;
    Double_t fTimeIntervalAroundTrigger;
    Double_t fSlewCorrSlope[128][16], fSlewCorrConstant[128][16];
    Double_t fNHitsCHodLow;
    Int_t fEnableSlewCorr;
    Double_t fHitEnergyThreshold;
    Double_t fTOFBetweenCHODPlanes;
    Double_t fLightVelocities[128];
    Double_t fAverageLightVelocity;
    Double_t fT0AtPM[128];
    Double_t fTOTAtPM[128];
    Double_t fTOTSlope[128];
    Double_t fTOTpdf[12];

    TString fCHODAllT0FileName;
    TString fCHODLightVelocitiesFileName;
    TString fCHODSlewCorrFileName;
    TString fCHODT0AtPMFileName;
    TString fCHODTOTFileName;
    TString fCHODTOTpdfFileName;

  public:

    Double_t            GetNCandidatesPerBurst()                            { return fNCandidatesPerBurst;          };

  public:

    TH2F* GetHCandidateTimeWrtReferenceVsBurst()                        { return fHCandidateTimeWrtReferenceVsBurst;                };
    TH1D *GetHNHitsPerEventLow()                                        { return fHNHitsPerEventLow;                                };
    TH1D *GetHChannelOccupancyLow()                                     { return fHChannelOccupancyLow;                             };
    TH1D *GetHHitLeadingTimeLow()                                       { return fHHitLeadingTimeLow;                               };
    TH1D *GetHHitWidthLow()                                             { return fHHitWidthLow;                                     };
    TH1D *GetHNErrorWord()                                              { return fHNErrorWord;                                      };
    TH1D *GetHNDetectedEdge()                                           { return fHNDetectedEdge;                                   };
    TH1D *GetHNHitsPerEventInTime()                                     { return fHNHitsPerEventInTime;                             };
    TH2F *GetHChannelIllumination()                                     { return fHChannelIllumination;                             };
    TH2F *GetHRecoHitTimeWrtReferenceVsIntersectionIDNoT0()             { return fHRecoHitTimeWrtReferenceVsIntersectionIDNoT0;     };
    TH2F *GetHRecoHitTimeWrtReferenceVsIntersectionID()                 { return fHRecoHitTimeWrtReferenceVsIntersectionID;         };
    TH2F *GetHRecoHitTimeWrtReferenceVsL0IDNoT0()                       { return fHRecoHitTimeWrtReferenceVsL0IDNoT0;               };
    TH1D *GetHCandidateTimeWrtReference()                               { return fHCandidateTimeWrtReference;                       };
    TH1D *GetHDtUncorr()                                                { return fHDtUncorr;                                        };
    TH1D *GetHDtT0Corr()                                                { return fHDtT0Corr;                                        };
    TH1D *GetHDtAllCorr()                                               { return fHDtAllCorr;                                       };

  private:

    TH2F *fHCandidateTimeWrtReferenceVsBurst;
    TH2F *fHNhodTimeVsSlot;
    TH2F *fHNhodTimeVsIDLow;
    TH2F *fHNhodTimeVsIDHigh;
    TH2F *fHTimeVsSlot;
    TH2F *fHTimeVsID;
    TH2F *fHDeltaLeadingEdgesVsChannelsLow;

    TH2F *fHMultPerSlab;
    TH2F *fHRecoHitTimeWrtReferenceVsIntersectionIDNoT0;
    TH2F *fHRecoHitTimeWrtReferenceVsIntersectionID;
    TH2F *fHRecoHitTimeWrtReferenceVsL0IDNoT0;
    TH1D *fHCandidateTimeWrtReference;

    TH1D *fHNHitsPerEventLow;
    TH1D *fHChannelOccupancyLow;
    TH1D *fHHitLeadingTimeLow;
    TH1D *fHHitWidthLow;

    TH1D *fHNErrorWord;
    TH1D *fHNDetectedEdge;
    TH1D *fHNHitsPerEventInTime;
    TH2F *fHChannelIllumination;

    TH1D *fHDtUncorr;
    TH1D *fHDtUncorr2Hits;
    TH1D *fHDtAllCorr;
    TH1D *fHDtAllCorr2Hits;
    TH1D *fHDtT0Corr;
    TH1D *fHDtT0Corr2Hits;
    TH2F *fHTOTvsID;
    TH2F *fHTOTvsID2Hits;
    TH3D *fHTOTvsIDvsIP;

    TH2F *fHEnergyVvsDt;
    TH2F *fHEnergyHvsDt;

  Double_t fPMCoordinate[128] = {1210., 1210., 1210., 1210., 1210., 1210., 1210., 1210.,
				 1191., 1126., 1061., 996., 897., 798., 699., 600.,
				 -600., -699., -798., -897., -996., -1061., -1126., -1191.,
				 -1210., -1210., -1210., -1210., -1210., -1210., -1210., -1210.,
				 -1210., -1210., -1210., -1210., -1210., -1210., -1210., -1210.,
				 -1191., -1126., -1061., -996., -897., -798., -699., -600.,
				 600., 699., 798., 897., 996., 1061., 1126., 1191.,
				 1210., 1210., 1210., 1210., 1210., 1210., 1210., 1210.,

				 -600., -699., -798., -897., -996., -1061., -1126., -1191.,
				 -1210., -1210., -1210., -1210., -1210., -1210., -1210., -1210.,
				 -1210., -1210., -1210., -1210., -1210., -1210., -1210., -1210.,
				 -1191., -1126., -1061., -996., -897., -798., -699., -600.,
				 600., 699., 798., 897., 996., 1061., 1126., 1191.,
				 1210., 1210., 1210., 1210., 1210., 1210., 1210., 1210.,
				 1210., 1210., 1210., 1210., 1210., 1210., 1210., 1210.,
				 1191., 1126., 1061., 996., 897., 798., 699., 600.};
  
  // x coordinate for 0<=i<64, y coordinate for 64<=i<128
  Double_t fSlabCenter[128] = {-32.5, -97.53, -162.56, -227.59, -292.62, -357.65, -422.68, -487.71,
                               -552.74, -617.77, -682.80, -764.83, -863.86, -962.89, -1061.92, -1160.95,
                               -1160.95, -1061.92, -962.89, -863.86, -764.83, -682.80, -617.77, -552.74,
                               -487.71, -422.68, -357.65, -292.62, -227.59, -162.56, -97.53, -32.5,
                               32.5, 97.53, 162.56, 227.59, 292.62, 357.65, 422.68, 487.71,
                               552.74, 617.77, 682.80, 764.83, 863.86, 962.89, 1061.92, 1160.95,
                               1160.95, 1061.92, 962.89, 863.86, 764.83, 682.80, 617.77, 552.74,
                               487.71, 422.68, 357.65, 292.62, 227.59, 162.56, 97.53, 32.5,

                               1160.95, 1061.92, 962.89, 863.86, 764.83, 682.80, 617.77, 552.74,
                               487.71, 422.68, 357.65, 292.62, 227.59, 162.56, 97.53, 32.5,
                               -32.5, -97.53, -162.56, -227.59, -292.62, -357.65, -422.68, -487.71,
                               -552.74, -617.77, -682.80, -764.83, -863.86, -962.89, -1061.92, -1160.95,
                               -1160.95, -1061.92, -962.89, -863.86, -764.83, -682.80, -617.77,-552.74,
                               -487.71,-422.68, -357.65, -292.62, -227.59, -162.56, -97.53, -32.5,
                               32.5, 97.53, 162.56, 227.59, 292.62, 357.65, 422.68, 487.71,
                               552.74, 617.77, 682.80, 764.83, 863.86, 962.89, 1061.92, 1160.95};
 
};
#endif
