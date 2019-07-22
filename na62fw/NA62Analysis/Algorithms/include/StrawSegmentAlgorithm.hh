#ifndef STRAWSEGMENTALGORITHM_HH
#define STRAWSEGMENTALGORITHM_HH

#include <memory>
#include <string>

#include "Algorithm.hh"
#include "TRecoSpectrometerEvent.hh"
#include "SpectrometerReconstruction.hh"
#include "SpectrometerGeometry.hh"
#include "SpectrometerParameters.hh"

// temporarily store Pnn structs in a separate header
#include "StrawClasses.hh"

class StrawSegmentAlgorithm : public Algorithm {
public:
    StrawSegmentAlgorithm(BaseAnalysis *ba, Analyzer* ana,
                          const std::string &name = "StrawSegmentAlgorithm");
    virtual ~StrawSegmentAlgorithm() = default;
    void Init();
    void Process(TRecoSpectrometerEvent*, Int_t track_ID);
    void SetConfFileName(TString value) {fConfFileName = value;}
    TString GetConfFileName() const {return fConfFileName;}
    void SetDebugMode(Bool_t value) {fDebugMode = value;};
    Bool_t GetDebugMode() const {return fDebugMode;};

    void Clear();
    Bool_t SelectTrack(Int_t,TRecoSpectrometerEvent*);
    PnnStrawHit *GetStrawCandidateHit() {return fStrawCandidateHit;};
    PnnStrawHit *GetStrawNoCandidateHit() {return fStrawNoCandidateHit;};
    int GetStrawClustersNoMatched(int jc, int jv) {return fStrawCluster[jc][jv]->GetNClusters();};
    bool ReconstructSegments(TVector3 *,double);
    void AnalyzeHits(int trackID);
    void SetFlag(int val){fFlag = val;};

private:
    TString fConfFileName;        ///< Path to Spectrometer config file
    SpectrometerParameters &fPar;
    SpectrometerGeometry &fGeo;
    Bool_t fIsInitialized;
    Int_t fRunID;
    Int_t fBurstID;
    Bool_t fIsMC;

    // variables for debugging
    Bool_t fDebugMode;

    TRecoSpectrometerEvent *fSpectrometerEvent = nullptr;
    Int_t fFlag;
    Double_t fZStation[4];
    Double_t fXCenter[4];
    void CreateStrawGeometry();
    bool FakeTrack(int,int);
    Bool_t MultiTrack(int);
    void Test2TrackVertex(int,int,TVector3*);
    Int_t Acceptance(TRecoSpectrometerCandidate*);
    Bool_t InRICHAcceptance(TRecoSpectrometerCandidate* track);
    Int_t GetTotalAcceptance();
    void SolveLR();
    ////  void OneViewSegment(Int_t,Int_t,Int_t);
    void OneViewSegment(int);
    void ComputeSegment(Int_t,Int_t,Int_t);
    void MultipleViewSegment(Int_t chamber,Int_t,Int_t,Double_t,Double_t,Double_t,Double_t);
    void ComputeSegment(Double_t ,Double_t,Double_t,Double_t,Double_t,Double_t,Double_t,Double_t);
    void FillTrackHitsArray();
    void FillTrackHitsArray(TRecoSpectrometerCandidate*);
    void FillNoTrackHitsArray();
    Int_t GetTrackHitIndex(Int_t);
    Double_t SigmaRadius(Double_t);
    double GetLocalCoordinate(TRecoSpectrometerCandidate*,int,double);
    double ChooseTheCombination(double*,double*,double*,double*,double*,double*);
    double Chi2LinearFit(double*,double*,double*,double*,double*);
    bool Pairing(double*,int*);
    void BuildChamberHit(int);
    int IntType1(int);
    int IntType2(int,int);
    int IntType3(int,int);
    void ComputeCoordinate(const int &, const double &, double*);
    void ComputeCoordinate(const int &, const double &, const double &, double*);
    int IntersectionQuality(int,double*,double,double*);
    void UpdateCoordinate(int,double*,double);
    std::vector<PnnStrawIntersection>::iterator GetHit(int cH, int j) { return fChamberHit[cH].begin()+j; };
    bool AcceptanceTwoView(int,double*);
    int StoreHit(int,double*,int*,double*);
    int StrawAcceptance(int,double*,int);
    void InitSegmentVariables();
    void InitGeometryVariables();
    Double_t fMinIB;
    double fMinChi2[2];
    double fMinDist[2];
    double fMinTime[2];
    double fMinSlopeX[2];
    double fMinSlopeY[2];
    double fPExp;
    double fMinXCoor[2][2];
    double fMinYCoor[2][2];
    double fMinYCh1;
    double fMinLKrDT;
    double fMinLKrDX;
    double fMinLKrDY;
    double fMinLKrD;
    double fSaveTime;
    Double_t fVertex[6];

    Int_t fNNoTrackHits;
    TRecoSpectrometerCandidate* fThisTrack = nullptr;
    PnnStrawHit *fStrawCandidateHit;
    PnnStrawHit *fStrawNoCandidateHit;
    PnnStrawCluster *fStrawCluster[4][4];
    std::vector<PnnStrawIntersection> fChamberHit[4]; //< Vector of reconstructed chamber-hits.
    double fStrawHitLocalX[4][4][4][122];
    double fStrawHitGlobalZ[4][4][4][122];
    double fHoleChamberMax[4][4];
    double fHoleChamberMin[4][4];
    double fViewPlaneTransverseSize;
    double fChamberZPosition[4];
};

#endif /* STRAWSEGMENTALGORITHM_H */
