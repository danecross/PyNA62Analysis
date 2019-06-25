#ifndef SEGMENTREJECTION_HH
#define SEGMENTREJECTION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include "TRecoSpectrometerEvent.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "TRecoSpectrometerHit.hh"

class TRecoLKrEvent;

class SegmentRejection : public NA62Analysis::Analyzer
{
public:
  explicit SegmentRejection(NA62Analysis::Core::BaseAnalysis *ba);
  ~SegmentRejection();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
  void Process(int iEvent);
  void PostProcess();
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void DrawPlot();
  void PrepareOutputs();
  void ValidateOutputs();
  void AnalyzeHits(TRecoSpectrometerEvent *,int);
  void AnalyzeHits(TRecoSpectrometerEvent *,int,int);
  void LoadEvent(TRecoLKrEvent*);

protected:
  struct PnnStrawCandidate{
    Int_t fTrackID;
    Bool_t fGoodTrack;
    Int_t fCharge;
    Bool_t fMultiVertex;
    double fMultiCDA;
    Int_t fAcceptance;
    Int_t fAcceptanceStraw;
    Int_t fAcceptanceCHOD;
    Int_t fAcceptanceNewCHOD;
    Int_t fAcceptanceRICH;
    Int_t fAcceptanceLKr;
    Int_t fAcceptanceMUV1;
    Int_t fAcceptanceMUV2;
    Int_t fAcceptanceMUV3;

    PnnStrawCandidate(){};
    ~PnnStrawCandidate(){};
    void Clear(){
      fTrackID = -1;
      fGoodTrack = 0;
      fCharge = 0;
      fMultiVertex = 0;
      fMultiCDA = 99999.;
      fAcceptance = 0;
      fAcceptanceStraw= 0;
      fAcceptanceCHOD = 0;
      fAcceptanceNewCHOD = 0;
      fAcceptanceRICH = 0;
      fAcceptanceLKr  = 0;
      fAcceptanceMUV1 = 0;
      fAcceptanceMUV2 = 0;
      fAcceptanceMUV3 = 0;
    };
    void SetTrackID(Int_t val)      {fTrackID=val;};
    void SetGoodTrack(Bool_t val)   {fGoodTrack=val;};
    void SetCharge(Int_t val)   {fCharge=val;};
    void SetMultiVertex(Bool_t val) {fMultiVertex=val;};
    void SetMultiCDA(double val) {fMultiCDA=val;};
    void SetAcceptance(Int_t val)   {fAcceptance=val;};
    void SetAcceptanceStraw(Int_t val)   {fAcceptanceStraw=val;};
    void SetAcceptanceCHOD(Int_t val)   {fAcceptanceCHOD=val;};
    void SetAcceptanceNewCHOD(Int_t val)   {fAcceptanceNewCHOD=val;};
    void SetAcceptanceLKr(Int_t val)   {fAcceptanceLKr=val;};
    void SetAcceptanceRICH(Int_t val)   {fAcceptanceRICH= val;};
    void SetAcceptanceMUV1(Int_t val)   {fAcceptanceMUV1= val;};
    void SetAcceptanceMUV2(Int_t val)   {fAcceptanceMUV2= val;};
    void SetAcceptanceMUV3(Int_t val)   {fAcceptanceMUV3= val;};
    Int_t GetTrackID()      {return fTrackID;};
    Bool_t GetGoodTrack()   {return fGoodTrack;};
    Int_t GetCharge()   {return fCharge;};
    Bool_t GetMultiVertex() {return fMultiVertex;};
    double GetMultiCDA() {return fMultiCDA;};
    Int_t GetAcceptance()   {return fAcceptance;};
    Int_t GetAcceptanceStraw()  {return fAcceptanceStraw;};
    Int_t GetAcceptanceCHOD()   {return fAcceptanceCHOD;};
    Int_t GetAcceptanceNewCHOD()   {return fAcceptanceNewCHOD;};
    Int_t GetAcceptanceLKr()    {return fAcceptanceLKr;};
    Int_t GetAcceptanceRICH()   {return fAcceptanceRICH;};
    Int_t GetAcceptanceMUV1()   {return fAcceptanceMUV1;};
    Int_t GetAcceptanceMUV2()   {return fAcceptanceMUV2;};
    Int_t GetAcceptanceMUV3()   {return fAcceptanceMUV3;};
  };

  struct PnnStrawHit{
    int fIndex[500];
    int fPaired[500];

    void Clear(){
      for(Int_t k=0; k<500; k++){
	fIndex[k] = -1;
	fPaired[k] = 0;
      };
    };
    Int_t GetIndex(Int_t j) { return fIndex[j]; };
    void SetIndex(Int_t j, Int_t val) { fIndex[j] = val; };
    int GetPaired(int j) { return fPaired[j]; };
    void SetPaired(int j, int val) { fPaired[j]=val; };
    PnnStrawHit(){};
    ~PnnStrawHit(){};
  };

  struct PnnStrawIntersection{
    double fTrailingTime;
    double fQuality;
    int fSubType;
    double fX;
    double fY;
    double fU;
    double fV;
    double fXcoor;
    double fYcoor;
    std::vector<int> fClusterId; //< Vector of the Id of the clusters forming the intersection.
    std::vector<int> fViewId;

    PnnStrawIntersection(){};
    ~PnnStrawIntersection(){};
    void Clear(){
      fTrailingTime = -99999.;
      fQuality = 9999.;
      fSubType = -1;
      fX=fY=fU=fV=fXcoor=fYcoor=-9999.;
      fClusterId.clear();
      fViewId.clear();
    };
    double GetX() { return fX; };
    double GetY() { return fY; };
    double GetU() { return fU; };
    double GetV() { return fV; };
    double GetXcoor() { return fXcoor; };
    double GetYcoor() { return fYcoor; };
    int GetType() { return fClusterId.size(); };
    double GetTrailingTime() { return fTrailingTime; };
    double GetQuality() { return fQuality; };
    int GetClusterId(Int_t jCluster) { return fClusterId[jCluster]; };
    int GetViewId(Int_t jCluster) { return fViewId[jCluster]; };
    void SetTrailingTime(double val) {fTrailingTime=val;} ;
    void SetQuality(double val) {fQuality=val;};
    void SetSubType(int val) {fSubType=val;};
    void SetCoordinate(double *xycoor){
      double sq2 = sqrt(2.);
      fXcoor = xycoor[0];
      fYcoor = xycoor[1];
      fX = xycoor[0];
      fY = xycoor[1];
      fU = xycoor[2];
      fV = xycoor[3];

      switch (fSubType) {
      case 20:
	fV = -9999;
	fX = -9999;
	fY = -9999;
	fXcoor = -9999;
	fYcoor = -9999;
	break;

      case 21:
	fU = -9999;
	fX = -9999;
	fY = -9999;
	fXcoor = -9999;
	fYcoor = -9999;
	break;

      case 22:
	fV = -9999;
	fU = -9999;
	fY = -9999;
	fYcoor = -9999;
	break;

      case 23:
	fU = -9999;
	fV = -9999;
	fX = -9999;
	fXcoor = -9999;
	break;

      case 14:
	fV = -9999;
	break;

      case 13:
	fU = -9999;
	break;

      case 11:
	fY = -9999;
	fYcoor = (-fU+fV)/sq2;
	break;

      case 7:
	fX = -9999;
	fXcoor = (fU+fV)/sq2;
	break;

      case 5:
	fX = -9999;
	fY = -9999;
	fXcoor = (fU+fV)/sq2;
	fYcoor = (-fU+fV)/sq2;
	break;

      case 4:
	fX = -9999;
	fU = -9999;
	fXcoor = -fY+fV*sq2;
	break;

      case 3:
	fX = -9999;
	fV = -9999;
	fXcoor = fY+fU*sq2;
	break;

      case 2:
	fY = -9999;
	fU = -9999;
	fYcoor = -fX+fV*sq2;
	break;

      case 1:
	fY = -9999;
	fV = -9999;
	fYcoor = fX-fU*sq2;
	break;

      case 0:
	fU = -9999;
	fV = -9999;
	break;

      default:
	break;
      }
    };
    void SetClusterId(int val){fClusterId.push_back(val);};
    void SetViewId(int val){fViewId.push_back(val);};
  };

  struct PnnStrawCluster{
    int fChamber;
    int fView;
    int fNClusters;
    int fNHits[50];
    int fIndex[3][50];
    double fX[50];
    double fZ[50];
    double fT[50];
    bool fEdge[50];
    double fQuality[50];

    PnnStrawCluster(int jc, int jv){
      fChamber = jc;
      fView = jv;
    };
    ~PnnStrawCluster(){};
    void Clear(){
      fChamber = -1;
      fView = -1;
      fNClusters = 0;
      for (int k=0; k<50; k++) {
	fNHits[k] = 0;
	fIndex[0][k] = -1;
	fIndex[1][k] = -1;
	fIndex[2][k] = -1;
	fX[k] = -99999.;
	fZ[k] = -99999.;
	fT[k] = -99999.;
	fEdge[k] = 0;
	fQuality[k] = 99999.;
      };
    };
    int GetChamber() {return fChamber;};
    int GetView() {return fView;};
    int GetNClusters() {return fNClusters;};
    int GetNHits(int j) {return fNHits[j];};
    int GetIndex(int i, int j) {return fIndex[i][j];};
    double GetX(int j) {return fX[j];};
    double GetZ(int j) {return fZ[j];};
    double GetT(int j) {return fT[j];};
    bool GetEdge(int j) {return fEdge[j];};
    double GetQuality(int j) {return fQuality[j];};
    void SetNClusters(int val) {fNClusters=val;};
    void SetNHits(int j, int val) {fNHits[j]=val;};
    void SetIndex(int i, int j, int val) {fIndex[i][j]=val;};
    void SetX(int j, double val) {fX[j]=val;};
    void SetZ(int j, double val) {fZ[j]=val;};
    void SetT(int j, double val) {fT[j]=val;};
    void SetEdge(int j, bool val) {fEdge[j]=val;};
    void SetQuality(int j, double val) {fQuality[j]=val;};
  };

  struct HitCommon{
    TRecoSpectrometerEvent *fev;
    int fidtr;
    HitCommon(TRecoSpectrometerEvent *e,int id) : fev(e), fidtr(id) {};
    bool operator() (int i){
      if (i==fidtr) return false;
      int nhcomm(0);
      TRecoSpectrometerCandidate *cj = (TRecoSpectrometerCandidate *)fev->GetCandidate(fidtr);
      int *hcj = cj->GetHitsIndexes();
      TRecoSpectrometerCandidate *ci = (TRecoSpectrometerCandidate *)fev->GetCandidate(i);
      int *hci = (int *)ci->GetHitsIndexes();
      for (int jh(0); jh<cj->GetNHits(); jh++) {
	for (int ih(0); ih<ci->GetNHits(); ih++) {
	  if (hci[ih]==hcj[jh]) nhcomm++;
	}
      }
      return nhcomm>1?true:false;
    }
  };

  struct PlaneChamberCondition{
    int fc;
    int fv;
    int fp;
    TRecoSpectrometerEvent* fevent;
    PnnStrawHit* fhit;
    PlaneChamberCondition(int c, int v, int p, TRecoSpectrometerEvent* e, PnnStrawHit* h) : fc(c),fv(v),fp(p),fevent(e),fhit(h) {};
    bool operator() ( int i ){
      int idHit = fhit->GetIndex(i);
      TRecoSpectrometerHit *hit = (TRecoSpectrometerHit *)fevent->GetHit(idHit);
      int jPlane = 2*hit->GetHalfViewID()+hit->GetPlaneID();
      if (hit->GetChamberID()==fc && hit->GetViewID()==fv && jPlane==fp) return false;
      return true;
    }
  };

  void Clear();
  Bool_t SelectTrack(Int_t,TRecoSpectrometerEvent*);
  PnnStrawCandidate *GetStrawCandidate() {return fStrawCandidate;};
  PnnStrawHit *GetStrawCandidateHit() {return fStrawCandidateHit;};
  PnnStrawHit *GetStrawNoCandidateHit() {return fStrawNoCandidateHit;};
  int GetStrawClustersNoMatched(int jc, int jv) {return fStrawCluster[jc][jv]->GetNClusters();};
  bool ReconstructSegments(TVector3 *,double);

private:
  TRecoSpectrometerEvent *fSpectrometerEvent;
  PnnStrawCandidate *fStrawCandidate;
  TRecoLKrEvent *fLKr;
  Int_t fFlag;
  Double_t fZStation[4];
  Double_t fXCenter[4];
  void CreateStrawGeometry();
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
  void CountTrackHits();
  void FillTrackHitsArray();
  void FillTrackHitsArray(TRecoSpectrometerCandidate*);
  void FillNoTrackHitsArray();
  Int_t GetTrackHitIndex(Int_t);
  Double_t SigmaRadius(Double_t);
  Double_t GetYCoordinate(Double_t,Int_t,Double_t,Int_t);
  Double_t GetXCoordinate(Double_t,Int_t,Double_t,Int_t);
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
  Double_t fSIB;
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

  Int_t fNHits;
  Int_t fNTrackHits;
  Int_t fNNoTrackHits;
  Int_t fmv1;
  Int_t fmv2;
  Double_t fDeltaZ1;
  Double_t fDeltaZ2;
  Double_t fDeltaR1;
  Double_t fDeltaR2;
  Int_t fCountMultipleView;
  TRecoSpectrometerCandidate* fThisTrack;
  TRecoSpectrometerCandidate* fThisTrack2;
  PnnStrawHit *fStrawCandidateHit;
  PnnStrawHit *fStrawNoCandidateHit;
  PnnStrawCluster *fStrawCluster[4][4];
  double fStrawHitLocalX[4][4][4][122];
  double fStrawHitGlobalZ[4][4][4][122];
  double fHoleChamberMax[4][4];
  double fHoleChamberMin[4][4];
  double fViewPlaneTransverseSize;
  double fChamberZPosition[4];
  int fIDTrack;
  std::vector<PnnStrawIntersection> fChamberHit[4]; //< Vector of reconstructed chamber-hits.

  //output
  bool fSegments;

  //parameters
  bool verb;
  bool UseGTK;

};
#endif
