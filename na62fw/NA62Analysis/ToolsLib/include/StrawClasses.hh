#ifndef STRAWCLASSES_HH
#define STRAWCLASSES_HH

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
    PnnStrawHit(){Clear();};
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

    PnnStrawIntersection(){Clear();};
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
        Clear();
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
        TRecoSpectrometerCandidate *cj = static_cast<TRecoSpectrometerCandidate*>(fev->GetCandidate(fidtr));
        int *hcj = cj->GetHitsIndexes();
        TRecoSpectrometerCandidate *ci = static_cast<TRecoSpectrometerCandidate*>(fev->GetCandidate(i));
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
        TRecoSpectrometerHit *hit = static_cast<TRecoSpectrometerHit*>(fevent->GetHit(idHit));
        int jPlane = 2*hit->GetHalfViewID()+hit->GetPlaneID();
        if (hit->GetChamberID()==fc && hit->GetViewID()==fv && jPlane==fp) return false;
        return true;
    }
};


#endif /* STRAWCLASSES_HH */
