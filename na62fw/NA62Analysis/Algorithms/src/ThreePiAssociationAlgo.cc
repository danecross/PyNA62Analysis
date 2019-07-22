/// \class ThreePiAssociationAlgo
/// \Brief
/// Example of an Algorithm. Algorithm are created in Analyzer as:
/// fThreePiAlgo = new ThreePiAssociationAlgo(ba,this,"ThreePiAlgo");
/// You can then used the  algorith as:
/// std::vector<DownstreamTrack> dTracks = *(std::vector<DownstreamTrack>*) GetOutput("DownstreamTrackBuilder.Output");
/// \code
/// std::vector<Particle> parts;
///  for (auto it = dTracks.begin(); it!=dTracks.end(); it++){
///    Particle part;
///    part.SetMomentum( (it)->GetMomentumBeforeMagnet());
///    part.SetPosition((it)->GetPositionBeforeMagnet());
///    part.SetTime((it)->GetCHODTime());
///    part.SetMass(139.57018);
///    part.SetProto(&(*it));
///    parts.push_back(part)
///}
/// std::vector<Particle> vK3Pi = fThreePiAlgo->Associate(parts);
/// \endcode
/// This would return a vector or Particle representing a K->3pi.
/// Other function are avaiables, see documentation.
/// Histograms booked in the Algorithm can be saved by calling:
/// fThreePiAlgo->SaveAllPlots();
/// in the EndOfJobUser().
/// \EndBrief
/// \author Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch)

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "ThreePiAssociationAlgo.hh"
#include "DownstreamTrack.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "Utils.hh"

bool MomentumOrder(Particle p1, Particle p2) { return p1.GetP() < p2.GetP(); }

namespace NA62Analysis
{
  //  ==============================================  
  ThreePiAssociationAlgo::ThreePiAssociationAlgo(BaseAnalysis *ba, Analyzer* ana, const std::string &name):
    Algorithm(ba,ana,name)
  {
	/// \MemberDescr
	/// \param ba : Parent BaseAnalysis instance
	/// \param ana : Analyser Holding the Algorithm
	/// \param name : Name of the instance of the algo
	/// Constructor with name. Usual AddParam and Histo and Counter Booking function can be called
	/// \EndMemberDescr

    AddParam("Chi2max" , &fChi2max, 30.);
    AddParam("Zmin"   , &fZmin, 105000.);
    AddParam("Zmax"   , &fZmax, 180000.);
    AddParam("Pmin"   , &fPmin, 70000.);
    AddParam("Pmax"   , &fPmax, 80000.);
    AddParam("InvMmin", &fInvMmin, 483.677);
    AddParam("InvMmax", &fInvMmax, 503.677);
    
    BookHisto("hMatchRate", new TH1F("hMatchRate",";NotMatched(0) Matched(1);count",2,0,2)) ;
    BookHisto("hUnMatchedPos", new TH2F("hUnMatchedPos","Extrapolated Position at GTK3;x;y",200,-80,120,40,-20,20)) ;
    BookHisto("hMatchedPos", new TH2F("hMatchedPos","Extrapolated Position at GTK3;x;y",200,-80,120,40,-20,20)) ;


    fHmK = new TH1F ("fHmK","; m_{K} [MeV]; count", 600,494-30, 494+30);
    BookHisto(fHmK);

    fHpK = new TH1F ("fHpK","; p_{K} [GeV/c]; count", 600,75-10,75+10);
    BookHisto(fHpK);

    fHP3PiThPi = new TH2F ("fHP3PiThPi","; P_{#pi} [GeV]; #theta(#pi K) [mrad]", 400,0,90, 400,0,10);
    BookHisto(fHP3PiThPi);

    fHThX3Pi = new TH1F ("fHThX3Pi","; #theta_{K}^{X} - 1.2 [mrad]; count", 400,-1.5,1.5);
    BookHisto(fHThX3Pi);
    
    fHThY3Pi = new TH1F ("fHThY3Pi","; #theta_{K}^{Y} [mrad]; count", 400,-1.5,1.5);
    BookHisto(fHThY3Pi);
    
    fHZChi2 = new TH2F ("fHZCHI2","; Z_{vtx} [m]; chi2_{vtx} ", 280,60,200, 200,0,75);
    BookHisto(fHZChi2);
    
    fHTVtx = new TH1F ("fHTVtx","; t_{vtx} [ns]; count", 400,-50,50);
    BookHisto(fHTVtx);

    fHDtVtx = new TH1F ("fHDtVtx","; #sum (t_{i} - t_{vtx})^{2} / 3 [ns]; count", 400,-50,50);
    BookHisto(fHDtVtx);
    
    fHnPi = new TH1F ("fHnPi","; Nb #pi; count", 30,0, 30);
    BookHisto(fHnPi);

    fHPRichMass = new TH2F("fHPRichMass",";m_{RICH};Momentum [Gev/x]",300,0,600, 200,0,90);
    BookHisto(fHPRichMass);


    //-------------
    BookCounter("K3Pi");
    BookCounter("K3Pi-KTAG");
    BookHisto("hDt_3Pi-KTAG",new TH1F("hDt_3Pi-KTAG",";t_{3#pi} - t_{KTAG}",500,-5,5),0,"KTAG");
    BookHisto("hDt_KTAG",new TH1F("hDt_KTAG",";t_{KTAG} - t_{trigger}",1000,-100,100),0,"KTAG");
    BookHisto("hDt_KTAGHits_N",new TH2F("hDt_KTAGHits_N",";t^{hits}_{KTAG} - t^{cand}_{KTAG}",1400,-3.5,3.5,70,0,70),0,"KTAG");
    BookHisto("hDt_K3Pi",new TH1F("hDt_K3Pi",";t_{K3Pi} - t_{trigger}",1000,-100,100),0,"KTAG");
    BookHisto("hDtKTAG-3Pi",new TH1F("hDt_KTAG-3Pi",";t_{KTAG} - t_{3Pi}",1400,-3.5,3.5),0,"KTAG");

  
    //-------------
    BookHisto("hChi2",new TH1F("hChi2",";#chi^{2}",200,0,50),0,"GTK_All");
    //BookHisto("hDt"  ,new TH1F("hDt",";t_{KTAG} - t_{GTK} [ns]",1400,-3.5,3.5),0,"GTK_All");
    BookHisto("hDx"  ,new TH1F("hDx",";x_{GTK} - x_{3Pi} [mm]" ,320,-20,20),0,"GTK_All");
    BookHisto("hDy"  ,new TH1F("hDy",";y_{GTK} - y_{3Pi} [mm]" ,320,-20,20),0,"GTK_All");
    BookHisto("hDpx" ,new TH1F("hDpx",";Px_{GTK} - Px_{3Pi} [MeV]" ,320,-20,20),0,"GTK_All");
    BookHisto("hDpy" ,new TH1F("hDpy",";Py_{GTK} - Py_{3Pi} [MeV]" ,320,-20,20),0,"GTK_All");
    BookHisto("hDp"  ,new TH1F("hDp",";P_{GTK} - P_{3Pi} [MeV]" ,320,-3000,3000),0,"GTK_All");

    BookHisto("hDx_s"  ,new TH1F("hDx_s",";x_{GTK} - x_{3Pi} [mm]" ,320,-20,20),0,"GTK_Sel");
    BookHisto("hDy_s"  ,new TH1F("hDy_s",";y_{GTK} - y_{3Pi} [mm]" ,320,-20,20),0,"GTK_Sel");
    BookHisto("hDpx_s" ,new TH1F("hDpx_s",";Px_{GTK} - Px_{3Pi} [MeV]" ,320,-20,20),0,"GTK_Sel");
    BookHisto("hDpy_s" ,new TH1F("hDpy_s",";Py_{GTK} - Py_{3Pi} [MeV]" ,320,-20,20),0,"GTK_Sel");
    BookHisto("hDp_s"  ,new TH1F("hDp_s",";P_{GTK} - P_{3Pi} [MeV]" ,320,-3000,3000),0,"GTK_Sel");


    BookHisto("hClusterSize1"  ,new TH1F("hClusterSize1",";Nb Hits in Cluster" ,15,0,15),0,"GTK_All");
    BookHisto("hClusterSize2"  ,new TH1F("hClusterSize2",";Nb Hits in Cluster" ,15,0,15),0,"GTK_All");
    BookHisto("hClusterSize3"  ,new TH1F("hClusterSize3",";Nb Hits in Cluster" ,15,0,15),0,"GTK_All");
    

  }

  //==============================================
  std::vector<Particle>  ThreePiAssociationAlgo::Associate(std::vector<Particle> vPi ){

    /// \MemberDescr
    /// \param vPi : Vector of Particle containing pions
    /// \return : Vector of Particle representing K->3Pi decay
    /// Loop over the pi tracks and find recursively the  K3pi Candiate with the best vertex chi2
    /// The candidates are kept is bestChi2 < 30 && bestZ>105000 && bestZ<180000 && abs(bestPk.Mag()-75e3)<5e3 && abs(bestM-493.677)<10
    /// \EndMemberDescr


    EventHeader* rawHeader = GetEventHeader();
    CheckPtr(rawHeader);
    double finetime =  rawHeader->GetFineTime()*ClockPeriod/256.;

    std::vector<Particle> vK3Pi;
    fHnPi->Fill(double(vPi.size()));

    double bestM=0.,bestTh=0.,bestT=0.,bestZ=0;
    int best1=-1,best2=-1,best3=-1;
    int totCharge(0);
    bool keepOn(1);
    TVector3 bestPk;
    TVector3 bestPos;

    //sort the pion vector by momentum
    std::sort(vPi.begin(), vPi.end(), MomentumOrder);

    while(keepOn){

      double bestChi2 = 99999;
      unsigned int nPis = vPi.size();
      if(nPis<3) return vK3Pi;

      for(unsigned int i1(0); i1<nPis-2; i1++){


	DownstreamTrack* tr1 = static_cast<DownstreamTrack*>(vPi[i1].GetProto());
	CheckPtr(tr1);
	TRecoSpectrometerCandidate*  sTr1 = tr1->GetSpectrometerCandidate();
	CheckPtr(sTr1);


	for(unsigned int i2 = i1+1; i2<nPis-1; i2++){

	  DownstreamTrack* tr2 = static_cast<DownstreamTrack*>(vPi[i2].GetProto());
	  CheckPtr(tr2);
	  TRecoSpectrometerCandidate*  sTr2 = tr2->GetSpectrometerCandidate();
	  CheckPtr(sTr2);


	  for(unsigned int i3 = i2+1; i3<nPis; i3++){

	    DownstreamTrack* tr3 = static_cast<DownstreamTrack*>(vPi[i3].GetProto());
	    CheckPtr(tr3);
	    TRecoSpectrometerCandidate*  sTr3 = tr3->GetSpectrometerCandidate();
	    CheckPtr(sTr3);
	  
	    fVertexLSF.Reset();
	    fVertexLSF.AddTrack(sTr1);
	    fVertexLSF.AddTrack(sTr2);
	    fVertexLSF.AddTrack(sTr3);

	    if(!fVertexLSF.FitVertex(kTRUE)){
	      // cout<<"fit failed"<<endl;
	      // cout<<sTr1<<" "<<sTr2<<"  "<<sTr3<<endl;
	      continue;
	    }

	    double  chi2 = fVertexLSF.GetChi2();
	    double     z =  fVertexLSF.GetVertexPosition().z();
	    double     m = (vPi[i1].Get4Momentum() + vPi[i2].Get4Momentum() + vPi[i3].Get4Momentum()).M();
	    TVector3  pK = fVertexLSF.GetTrackThreeMomentum(0) + fVertexLSF.GetTrackThreeMomentum(1) + fVertexLSF.GetTrackThreeMomentum(2);
	    double    th = pK.Angle(vPi[2].GetMomentum());
	    double     t = (vPi[i1].GetTime() + vPi[i2].GetTime() + vPi[i3].GetTime()) / 3.;
	    //double    dt =  sqrt((pow(vPi[i1].GetTime()-t,2) +pow(vPi[i2].GetTime()-t,2) +pow(vPi[i3].GetTime()-t,2))/3);
	    totCharge = sTr1->GetCharge() + sTr2->GetCharge() + sTr3->GetCharge();

	    if(chi2<bestChi2 && totCharge == 1){
	      bestChi2 = chi2;
	      bestM = m;
	      bestPk = pK;
	      bestPos = fVertexLSF.GetVertexPosition ();
	      bestTh = th;
	      bestZ = z;
	      bestT = t;
	      //bestDt = dt;
	      best1 = i1;
	      best2 = i2;
	      best3 = i3;
	    }
	  }
	}
      }


      //decide to keep or not
      if(bestChi2 < 30 && bestZ>105000 && bestZ<180000 && abs(bestPk.Mag()-75e3)<5e3 && abs(bestM-493.677)<10  &&
	 ( bestTh*1e3 - (9.4 - 9.4/58. * vPi[best3].GetP()*1e-3) ) < 0 ) {
	Particle part;
	part.SetCharge(1);
	part.SetMass(0.493677);
	part.SetMeasMass(bestM);
	part.SetTime(bestT);
	part.SetPosition(bestPos);
	part.SetEndVertex(bestPos);
	part.SetMomentum(bestPk);
	part.AddDaughter(vPi[best1]);
	part.AddDaughter(vPi[best2]);
	part.AddDaughter(vPi[best3]);

	fHmK->Fill(bestM);
	fHpK->Fill(bestPk.Mag()*1e-3);
	fHThX3Pi->Fill(1e3*bestPk.X()/bestPk.Z() - 1.2 );
	fHThY3Pi->Fill(1e3*bestPk.Y()/bestPk.Z() );
	fHTVtx->Fill(bestT - finetime);
	fHP3PiThPi->Fill(vPi[best3].GetP()*1e-3 , bestTh*1e3 );

	fHZChi2->Fill(bestZ*1e-3 , bestChi2 );
	fHPRichMass->Fill(vPi[best1].GetMeasMass(), vPi[best1].GetP()*1e-3);
	fHPRichMass->Fill(vPi[best2].GetMeasMass(), vPi[best2].GetP()*1e-3);
	fHPRichMass->Fill(vPi[best3].GetMeasMass(), vPi[best3].GetP()*1e-3);

	//	fHP3PiThPi->Fill(bestPk.Mag()*1e-3 , bestTh*1e3 );

	vPi.erase(vPi.begin() + best3);
	vPi.erase(vPi.begin() + best2);
	vPi.erase(vPi.begin() + best1);
	vK3Pi.push_back(part);

      }
      //it means that the best candidate we could make with the remainig track is bad, so stop
      else keepOn = false; 
    }
    return vK3Pi;
  }



  void ThreePiAssociationAlgo::Associate(std::vector<Particle>& vK3Pi, TRecoCedarEvent* cedarEvt ){  

    /// \MemberDescr
    /// \param vK3Pi : Vector of K3Pi particle
    /// \param cedarEvt : Cedar event
    /// Look for a cedar candidate in time (+/- 3ns) with the K3pi
    /// \EndMemberDescr


    //match K3Pi with a KTAG
    IncrementCounter("K3Pi",int(vK3Pi.size()));
    TRecoCedarCandidate* cedarCand=nullptr;
    int bestCand3Pi(0);
    int bestCandKTAG(0);

    EventHeader* rawHeader = static_cast<EventHeader*>( GetEventHeader());
    CheckPtr(rawHeader);
    double t0 =  rawHeader->GetFineTime()*ClockPeriod/256.;

    //plotting
    int nbCedarCand(0);
    for (int iM(0); iM<cedarEvt->GetNCandidates(); iM++){
      cedarCand = static_cast<TRecoCedarCandidate*>(cedarEvt->GetCandidate(iM));
      if (cedarCand->GetNSectors()>=4) {
	FillHisto("hDt_KTAG",cedarCand->GetTime()-t0);
	nbCedarCand++;
      }
      for ( std::vector<Particle>::iterator K3Pi = vK3Pi.begin();K3Pi!=vK3Pi.end();++K3Pi){
	if(iM==0) FillHisto("hDt_K3Pi",K3Pi->GetTime()-t0);
	if (cedarCand->GetNSectors()<4) continue;
	double dt = K3Pi->GetTime() - cedarCand->GetTime();
	FillHisto("hDt_3Pi-KTAG",dt);
      }
    }

    bool keepon(1);
    vector<bool> mK3Pi(int(vK3Pi.size()),0);
    vector<bool> mKTAG(cedarEvt->GetNCandidates(),0);
    while(keepon){
      int jM(0);
      double bestDt = 9999;
      for ( std::vector<Particle>::iterator K3Pi = vK3Pi.begin();K3Pi!=vK3Pi.end();++K3Pi,++jM){
	if(mK3Pi[jM]==1) continue; //already matched
	for (int iM(0); iM<cedarEvt->GetNCandidates(); iM++){
	  if(mKTAG[iM]==1) continue; //already matched
	  cedarCand = static_cast<TRecoCedarCandidate*>(cedarEvt->GetCandidate(iM));
	  if (cedarCand->GetNSectors()<4) continue;

	  // check that RMS is good
	  double rms(0);
	  int* KTAGHits = cedarCand->GetHitsIndexes();
	  for (int iHKTAG(0); iHKTAG<cedarCand->GetNHits();iHKTAG++){
	    TRecoCedarHit* hit = static_cast<TRecoCedarHit*>(cedarEvt->GetHit(KTAGHits[iHKTAG]));
	    rms += pow(cedarCand->GetTime()-hit->GetTime(),2);
	  }
	  if( sqrt(rms / cedarCand->GetNHits()) > 0.6) continue;

	  double dt = abs(cedarCand->GetTime()-K3Pi->GetTime());
	  if (dt < 3 && dt<bestDt) {
	    bestDt=dt;
	    bestCandKTAG = iM;
	    bestCand3Pi = jM;
	  }
	}
      }
      if(bestDt<3){
	//check that there is no other candidate within 3ns
	bool ambigousCand(0);
	TRecoCedarCandidate* bestCedarCand = static_cast<TRecoCedarCandidate*>(cedarEvt->GetCandidate(bestCandKTAG));
	for (int iM(0); iM<cedarEvt->GetNCandidates(); iM++){
	  if(iM==bestCandKTAG) continue;
	  if( fabs(bestCedarCand->GetTime()-cedarEvt->GetCandidate(iM)->GetTime())<3) ambigousCand=1;
	}
	if(ambigousCand==0){
	  IncrementCounter("K3Pi-KTAG");
	  
	  ProtoParticle* protoK =  vK3Pi[bestCand3Pi].GetProto();
	  if(protoK==NULL) {
	    protoK = new ProtoParticle;
	    vK3Pi[bestCand3Pi].SetProto(protoK);
	  }
	  protoK->AddCandidate(static_cast<TRecoCedarCandidate*>(cedarEvt->GetCandidate(bestCandKTAG)), ProtoParticle::kTRecoCedarCandidate);

	  FillHisto("hDtKTAG-3Pi",cedarEvt->GetCandidate(bestCandKTAG)->GetTime() - vK3Pi[bestCand3Pi].GetTime());
	  int* KTAGHits = cedarCand->GetHitsIndexes();
	  for (int iHKTAG(0); iHKTAG<cedarCand->GetNHits();iHKTAG++){
	    TRecoCedarHit* hit = static_cast<TRecoCedarHit*>(cedarEvt->GetHit(KTAGHits[iHKTAG]));
	    FillHisto("hDt_KTAGHits_N",hit->GetTime()-cedarCand->GetTime(),cedarCand->GetNHits());
	  }

	}
	mK3Pi[bestCand3Pi]=1;  //tag as matched
	mKTAG[bestCandKTAG]=1;  //tag as matched
      }
      else keepon = 0;
    }
    return;
  }



  void ThreePiAssociationAlgo::Associate(std::vector<Particle>& vK3Pi, TRecoGigaTrackerEvent* gigatrackerEvt ){


    /// \MemberDescr
    /// \param vK3Pi : Vector of K3Pi particle
    /// \param  gigatrackerEvt: GigaTracker Eevent
    /// Look for a GigaTracker candidate matching the K3pi
    /// The match is done based on a chi2 defined as:
    /// pow(dPos.X()/2.5,2) + pow(dPos.Y()/2.5,2) + pow(dMom.X()/5,2) + pow(dMom.Y()/5,2)  + pow(dP/550,2);
    /// Note that the time is not used.
    /// \EndMemberDescr

    if(gigatrackerEvt->GetErrorMask()!=0) {
      //cout<<"Error Mask ON"<<endl;
      return;
    }

    TRecoGigaTrackerCandidate* gigatrackerCand;
    if(gigatrackerEvt->GetNCandidates() > 15){
      return;
    }

    double bestdP=-999.;
    TVector3 bestdPos, bestdMom;

    int    bestCandGTK(0);
    int    bestCand3Pi2(0);
    vector<bool> mK3Pi2(int(vK3Pi.size()),0);
    vector<bool> mGTK(gigatrackerEvt->GetNCandidates(),0);
    bool keepon = 1;
    int pass(0);
    while(keepon){
      pass ++;
      int jM(0);
      double bestChi2 = 9999;
      for (std::vector<Particle>::iterator K3Pi = vK3Pi.begin();K3Pi!=vK3Pi.end();++K3Pi,++jM ){
	if(mK3Pi2[jM]==1) continue; //already matched

	for (int iM(0); iM<gigatrackerEvt->GetNCandidates(); iM++){
	  if(mGTK[iM]==1) continue; //already matched
	  gigatrackerCand = static_cast<TRecoGigaTrackerCandidate*>(gigatrackerEvt->GetCandidate(iM));
	  if(gigatrackerCand==NULL) continue;
	  if(gigatrackerCand->GetNHits()!=3) continue;
	  if(gigatrackerCand->GetChi2()>50) continue;
	  if(abs(gigatrackerCand->GetTime())<1e-6) {
	    cout<<"Suspicious Candidate "<<gigatrackerCand->GetTime()<<endl;
	    continue;
	  }

	  //propagate to GTK3
	  BlueTubeTracker::GetInstance()->SetInitialPosition(K3Pi->GetPosition());
	  BlueTubeTracker::GetInstance()->SetInitialMomentum(K3Pi->GetMomentum());
	  BlueTubeTracker::GetInstance()->SetCharge(1);
	  BlueTubeTracker::GetInstance()->SetZFinal(102400); //GTK3
	  BlueTubeTracker::GetInstance()->TrackParticle();
	  K3Pi->SetPosition(BlueTubeTracker::GetInstance()->GetFinalPosition());
	  K3Pi->SetMomentum(BlueTubeTracker::GetInstance()->GetFinalMomentum());
	  TVector3 dPos = K3Pi->GetPosition() - gigatrackerCand->GetPosition(2);
	  TVector3 dMom = K3Pi->GetMomentum() - gigatrackerCand->GetMomentum();
	  double dP = K3Pi->GetMomentum().Mag() - gigatrackerCand->GetMomentum().Mag();
	  double chi2 = pow(dPos.X()/2.5,2) + pow(dPos.Y()/2.5,2) + pow(dMom.X()/5,2) + pow(dMom.Y()/5,2)  + pow(dP/550,2);

	  //some histo filling
	  if(pass == 1){ //only at the first pass
	    FillHisto("hChi2", chi2);
	    FillHisto("hDx"  , dPos.X());
	    FillHisto("hDy"  , dPos.Y());
	    FillHisto("hDpx" , dMom.X());
	    FillHisto("hDpy" , dMom.Y());
	    FillHisto("hDp"  , dP);

	    int nHits[3] = {0,0,0};
	    int* hits = gigatrackerCand->GetHitsIndexes();
	    for(int iH(0);iH<gigatrackerCand->GetNHits();iH++){
	      TRecoGigaTrackerHit* hit = static_cast<TRecoGigaTrackerHit*>(gigatrackerEvt->GetHit(hits[iH]));
	      int s = hit->GetStationNo();
	      nHits[s]++;
	    }
	    FillHisto("hClusterSize1",nHits[0]);
	    FillHisto("hClusterSize2",nHits[1]);
	    FillHisto("hClusterSize3",nHits[2]);
	  }


	  //best
	  if(chi2<=5 && chi2<bestChi2){
	    bestChi2 = chi2;
	    bestCandGTK = iM;
	    bestCand3Pi2 = jM;
	    bestdP = dP;
	    bestdPos = dPos;
	    bestdMom = dMom;
	  }
	}
      }
      if(bestChi2<5){
	mK3Pi2[bestCand3Pi2]=1;
	mGTK[bestCandGTK]=1; 
	ProtoParticle* protoK = vK3Pi[bestCand3Pi2].GetProto();
	if(protoK==NULL) {
	  protoK = new ProtoParticle;
	  vK3Pi[bestCand3Pi2].SetProto(protoK);
	}
	protoK->AddCandidate(gigatrackerEvt->GetCandidate(bestCandGTK), ProtoParticle::kTRecoGigaTrackerCandidate);
	FillHisto("hDx_s"  , bestdPos.X());
	FillHisto("hDy_s"  , bestdPos.Y());
	FillHisto("hDpx_s" , bestdMom.X());
	FillHisto("hDpy_s" , bestdMom.Y());
	FillHisto("hDp_s"  , bestdP);


      }
      else{
	keepon = 0;
	//plot the stat of the match/not matched
	for(int i(0);i<int(vK3Pi.size());i++) {
	  if(mK3Pi2[i]==0) {
	    FillHisto("hMatchRate",0);
	    FillHisto("hUnMatchedPos",vK3Pi[i].GetPosition().X(),vK3Pi[i].GetPosition().Y());
	  }
	  else{
	    FillHisto("hMatchRate",1);
	    FillHisto("hMatchedPos",vK3Pi[i].GetPosition().X(),vK3Pi[i].GetPosition().Y());
	  }
	}
      }
    }
    return;
  }

  
}//~namespace IImaS


