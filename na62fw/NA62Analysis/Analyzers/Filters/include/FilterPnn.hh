#ifndef FILTERPNN_HH
#define FILTERPNN_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "EventHeader.hh"
#include "TRecoSpectrometerEvent.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "TRecoCHODEvent.hh"
#include "TRecoCHODHit.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class TRecoCedarEvent;
class TRecoGigaTrackerEvent;
class TRecoLKrEvent;
class TRecoLAVEvent;
class TRecoMUV3Event;

class TRecoLKrCandidate;

class GeometricAcceptance;
class SpectrometerMUV3AssociationOutput;

class FilterPnn : public NA62Analysis::Analyzer
{
    public:
        explicit FilterPnn(NA62Analysis::Core::BaseAnalysis *ba);
        ~FilterPnn();
        void DefineMCSimple() {}
        void InitHist();
        void InitOutput();
        void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
        void Process(int iEvent);
        void PostProcess();
        void StartOfBurstUser();
        void EndOfBurstUser();
        void StartOfRunUser();
        void EndOfRunUser();
        void EndOfJobUser();
        void DrawPlot();

        protected:
        TRecoCedarEvent *fCedarEvent;
        TRecoGigaTrackerEvent *fGigaTrackerEvent;
        TRecoSpectrometerEvent *fSpectrometerEvent;
        TRecoCHODEvent *fCHODEvent;
        TRecoLKrEvent *fLKrEvent;
        TRecoLAVEvent *fLAVEvent;
        TRecoMUV3Event *fMUV3Event;
        EventHeader *fRawHeader;
        L0TPData *fL0Data;
        Int_t fFilterControl;
        Int_t fCDW;
        GeometricAcceptance *fGeo;
        std::vector<SpectrometerMUV3AssociationOutput> fMUV3Candidate;
        Double_t fCHODPosV[64];
        Double_t fCHODPosH[64];
        Double_t fCHODAllSlewSlope[128][16];
        Double_t fCHODAllSlewConst[128][16];
        Double_t fCHODLightVelocitiesCorr[128][16];
        Double_t fCHODAllFineT0[128][16];

	protected:
        Bool_t MultiTrack(Int_t,Double_t);
        TLorentzVector Get4Momentum(Double_t,Double_t,Double_t,Double_t);
        TVector3 MultiTrackVertex(Int_t,TLorentzVector*,TVector3*,Double_t*);
        Bool_t CHODQ1(std::vector<Int_t> *);
        std::vector<Double_t> CHODTrackMatching(Double_t,Double_t,Double_t,std::vector<Int_t>*);
        Double_t CHODTimeCorrection(Int_t,Int_t,Double_t,Bool_t);
        Bool_t KTAGTrackMatching(Double_t,Double_t*);
        Bool_t MUV3TrackMatching(Int_t,Double_t,Double_t,Double_t);
        Bool_t LKrPhotons(Double_t,Double_t,Double_t,Double_t*);
        Double_t LKrCorrectedEnergy(TRecoLKrCandidate*);
        Bool_t LAVPhotons(Double_t);
        Bool_t FilteringCondition();
        Bool_t FilterControl();

        protected:
        struct TrackQualityCondition{
          TRecoSpectrometerEvent* fevent;
          Int_t fjT;
          TrackQualityCondition(TRecoSpectrometerEvent* a, Int_t k) : fevent(a),fjT(k) {};
          Bool_t operator() ( Int_t i ){
            TRecoSpectrometerCandidate *pT = static_cast<TRecoSpectrometerCandidate *>(fevent->GetCandidate(i));
            Bool_t goodT = true;
            if (i==fjT) goodT = false;
            if (pT->GetNChambers()<4) goodT = false;
            if (pT->GetChi2()>30) goodT = false;
            return goodT;
          }
        };
        struct ChannelOrder{
          TRecoCHODEvent* fevent;
          explicit ChannelOrder(TRecoCHODEvent* a) : fevent(a) {};
          Bool_t operator() ( Int_t i, Int_t j ){
            TRecoCHODHit* hi = static_cast<TRecoCHODHit*>(fevent->GetHit(i));
            TRecoCHODHit* hj = static_cast<TRecoCHODHit*>(fevent->GetHit(j));
            return hi->GetChannelID()<hj->GetChannelID();
          }
        };
        struct PlaneCondition{
          TRecoCHODEvent* fevent;
          explicit PlaneCondition(TRecoCHODEvent* a) : fevent(a) {};
          Bool_t operator() ( Int_t i ){
            TRecoCHODHit* hi = static_cast<TRecoCHODHit*>(fevent->GetHit(i));
            return hi->GetChannelID()<=63;
          }
        };
        struct QuadrantCondition{
          TRecoCHODEvent* fevent;
          Int_t fQ;
          Int_t fP;
          QuadrantCondition(TRecoCHODEvent* a, Int_t q, Int_t p) : fevent(a),fQ(q),fP(p) {};
          Bool_t operator() ( Int_t i ){
            TRecoCHODHit* hi = static_cast<TRecoCHODHit*>(fevent->GetHit(i));
            return ((hi->GetChannelID()>=16*fQ+64*fP)&&(hi->GetChannelID()<16*(fQ+1)+64*fP));
          }
        };

        private:
        Double_t fCHODLightVelocities[128];
        Double_t fPMCoordinate[128] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
				       1.9, 8.4, 14.9, 21.4, 31.3, 41.2, 51.1, 61.0,
				       60.0, 69.9, 79.8, 89.7, 99.6, 106.1, 112.6, 119.1,
				       121.0, 121.0, 121.0, 121.0, 121.0, 121.0, 121.0, 121.0,
				       0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
				       1.9, 8.4, 14.9, 21.4, 31.3, 41.2, 51.1, 61.0,
				       60.0, 69.9, 79.8, 89.7, 99.6, 106.1, 112.6, 119.1,
				       121.0, 121.0, 121.0, 121.0, 121.0, 121.0, 121.0, 121.0,
				       60.0, 69.9, 79.8, 89.7, 99.6, 106.1, 112.6, 119.1,
				       121.0, 121.0, 121.0, 121.0, 121.0, 121.0, 121.0, 121.0,
				       0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
				       1.9, 8.4, 14.9, 21.4, 31.3, 41.2, 51.1, 61.0,
				       60.0, 69.9, 79.8, 89.7, 99.6, 106.1, 112.6, 119.1,
				       121.0, 121.0, 121.0, 121.0, 121.0, 121.0, 121.0, 121.0,
				       0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
				       1.9, 8.4, 14.9, 21.4, 31.3, 41.2, 51.1, 61.0};

        Double_t fSlabLimitsX[33] = {-1210.,-1110.,-1010.,-910.,-810.,-710.,-650.,-580.5,-520.,-450.5,-390.,
				     -320.5,-260.,-190.5,-130.,-60.5,0.,60.5,130.,190.5,260.,320.5,
				     390.,450.5,520.,580.5,650.,710.,810.,910.,1010.,1110.,1210.};
        Double_t fSlabLimitsY[33] = {-1210.,-1110.,-1010.,-910.,-810.,-710.,-650.,-580.5,-520.,-450.5,-390.,
				     -320.5,-260.,-190.5,-130.,-60.5,0.,60.5,130.,190.5,260.,320.5,
				     390.,450.5,520.,580.5,650.,710.,810.,910.,1010.,1110.,1210.};
        Double_t fSlabCenter[128]; // x coordinate for 0<=i<64, y coordinate for 64<=i<128
};

//template <typename T>
//class make_vector {
//public:
//  typedef make_vector<T> my_type;
//  my_type& operator<< (const T& val) {
//    data_.push_back(val);
//    return *this;
//  }
//  operator std::vector<T>() const {
//    return data_;
//  }
//private:
//  std::vector<T> data_;
//};

#endif
