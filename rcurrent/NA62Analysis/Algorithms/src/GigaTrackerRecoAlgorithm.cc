/// \class GigaTrackerRecoAlgorithm
/// \Brief
/// GigaTracker reconstruction at the NA62Analysis stage
/// \EndBrief
/// \Detailed
/// Rebuilds GigaTracker candidates (i.e. the GigaTracker tracks).
/// It is recommended to use the UpstreamPileupGenerator pre-analyzer,
/// otherwise MC will not agree with the data in terms of GTK reconstruction outputs.
/// This algorithm can be called from an analyzer. The time window in terms of the
/// middle ("reference time") and half-width should be specified; GTK hits within
/// this time window only are considered by the algorithm to reconstruct he tracks.
/// The algorithm should be initialized in the constructor of user's analyser as follows:
/// \code
/// fGTKReco = new GigaTrackerRecoAlgorithm(ba, this, "GTKRecoAlgo");
/// // optional settings (further settings are available, see the class description)
/// fGTKReco->SetRedoTimeCorr(false);
/// fGTKReco->SetFillHistograms(true);
/// fGTKReco->SetTimeWindowWrtReference(2.5); // this is half-width of the time window [ns]
/// \endcode
/// It should be run in the Process() method of user's analyser as follows.
/// Note that the algorithm runs on a cloned event GTKEvent2 in the example below.
/// Cloning the event (and then deleting the clone) is highly recommended because
/// the original event, which might be used by other analyzers, is rewritten otherwise.
/// \code
/// TRecoGigaTrackerEvent* GTKEvent = (TRecoGigaTrackerEvent*)GetEvent("GigaTracker");
/// TRecoGigaTrackerEvent* GTKEvent2 = new TRecoGigaTrackerEvent(*GTKEvent);
/// fGTKReco->Process(GTKEvent2, RefTime);
/// ...
/// delete GTKEvent2;
/// \endcode
/// To use the trigger time as the reference time,
/// \code
/// RefTime = GetL0Data->GetReferenceTime()*TdcCalib;
/// \endcode
/// To save the monitoring histograms produced by the algorithm,
/// do the following in EndOfJobUser() method of your analyser:
/// \code
/// if (fGTKReco->GetFillHistograms()) fGTKReco->SaveAllPlots();
/// \endcode
/// \author Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch)
/// \author Alina Kleimenova (alina.kleimenova@cern.ch)
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "GigaTrackerRecoAlgorithm.hh"
#include "NA62ConditionsService.hh"
#include "TRecoGigaTrackerCandidate.hh"

GigaTrackerRecoAlgorithm::GigaTrackerRecoAlgorithm
(BaseAnalysis *ba, Analyzer* ana, const std::string &name) :
  Algorithm(ba, ana, name), fRunID(-1), fHistogramsBooked(false) {

  AddParam("RedoTimeCorr",           &fRedoTimeCorr,          false);
  AddParam("FineTimeCorr",           &fFineTimeCorr,          false); // track time smearing: to be phased out?
  AddParam("RedoXYCorr",             &fRedoXYCorr,                1); // 1=redo alignment corrections
  AddParam("FineCorr",               &fFineCorr,               true); // Apply fine corrections (MomScale, rotation)?
  AddParam("TimeWindowWrtReference", &fTimeWindowWrtReference, 10.0); // [ns]
  AddParam("NHitsMax",               &fNHitsMax,                100); // Max number of in-time hits to process the event
  AddParam("NTracksMax",             &fNTracksMax,              100); // Max number of tracks to be built
  AddParam("FillHistograms",         &fFillHistograms,        false); // Fill monitoring histograms?

  // Algorithm parameters
  fClight     = TMath::C();
  fTimeWindow = 0.7;   // ns
  fXWindow    = 10;    // mm
  fYWindow    = 10;    // mm
  fChi2X      = 20;    // chi2 cut
  fChi2Y      = 20;    // chi2 cut
  fChi2T      = 13.82; // chi2 cut => prob(chi2Time>13.82,ndof=2)=10^-3
  fRunID = -1;
  fTZeroFile    = "GigaTracker-T0.dat";
  fTWalkFile    = "GigaTracker-TimeWalk.dat";

  fParameters   = nullptr;
  // initialize fT0 to 0. to avoid warning messages when reading the T0 corrections
  for (int iS(0); iS<3; iS++){
    for (int iP(0); iP<18000; iP++){
      fT0[iS][iP]=0.;
    }
    for (int iP(0); iP<10; iP++) fTW[iS][iP]=nullptr;
  }
}

// Read the parameters of the correction from CDB at the start of new run
void GigaTrackerRecoAlgorithm::StartOfRunUser() {
  cout << user_normal() << "Station alignment: RedoXYCorr=" << fRedoXYCorr;
  if (!fRedoXYCorr)        cout << " (alignment by NA62Reconstruction)" << endl;
  else if (fRedoXYCorr==1) cout << " (alignment by GigaTrackerRecoAlgorithm)" << endl;
  else if (fRedoXYCorr==2) cout << " (raw hit positions used)" << endl;
  cout << user_normal() << "Fine corrections (MomScale, rotation) " <<
    (fFineCorr ? "enabled" : "disabled") << endl;
  InitCorrections();
  if (fRedoTimeCorr) InitTimeCorrections();
  if (!fHistogramsBooked) InitHistograms();
}

void GigaTrackerRecoAlgorithm::InitHistograms() {
  fHistogramsBooked = true;
  if (!fFillHistograms) return;

  BookHisto("hNGTKCandidates", new TH1F("hNGTKCandidates", ";Number of GTK candidates;count",100,-0.5,99.5));
  BookHisto("hMomentum", new TH1F("hMomentum", ";p [GeV/c];count", 200,60,90));
  BookHisto("hThX",      new TH1F("hThX",";#theta_{X} - 1.2 [mrad];count",100,-1,1));
  BookHisto("hThY",      new TH1F("hThY",";#theta_{Y} [mrad];count",100,-1,1));
  BookHisto("hChi2",     new TH1F("hChi2",";#chi^{2};count",200,0,500));
  BookHisto("hChi2X",    new TH1F("hChi2X",";#chi^{2} X;count",200,0,500));
  BookHisto("hChi2Y",    new TH1F("hChi2Y",";#chi^{2} Y;count",200,0,500)); // not filled yet
  BookHisto("hChi2T",    new TH1F("hChi2T",";#chi^{2} T;count",200,0,500));
  BookHisto("hProbChi2X",new TH1F("hProbChi2X",";Prob #chi^{2} X;count",1000,0,1));
  BookHisto("hProbChi2Y",new TH1F("hProbChi2Y",";Prob #chi^{2} Y;count",1000,0,1)); // not filled yet
  BookHisto("hProbChi2T",new TH1F("hProbChi2T",";Prob #chi^{2} T;count",1000,0,1));
  BookHisto("hDX_12",    new TH1F("hDX_12",";X_{1}-X_{2};count",200,-25,25));
  BookHisto("hDX_13",    new TH1F("hDX_13",";X_{1}-X_{3};count",200,-25,25));
  BookHisto("hDX_23",    new TH1F("hDX_23",";X_{2}-X_{3};count",200,-25,25));
  BookHisto("hDY_12",    new TH1F("hDY_12",";Y_{1}-Y_{2};count",200,-25,25));
  BookHisto("hDY_13",    new TH1F("hDY_13",";Y_{1}-Y_{3};count",200,-25,25));
  BookHisto("hDY_23",    new TH1F("hDY_23",";Y_{2}-Y_{3};count",200,-25,25));
  BookHisto("hDt_12",    new TH1F("hDt_12",";t_{1}-t_{2};count",200,-5,+5));
  BookHisto("hDt_13",    new TH1F("hDt_13",";t_{1}-t_{3};count",200,-5,+5));
  BookHisto("hDt_23",    new TH1F("hDt_23",";t_{2}-t_{3};count",200,-5,+5));
  BookHisto("hDX_X_12",  new TH2F("hDX_X_12",";X_{1};X_{1}-X_{2}",200,-40,40,200,-25,25));
  BookHisto("hDX_X_13",  new TH2F("hDX_X_13",";X_{1};X_{1}-X_{3}",200,-40,40,200,-25,25));
  BookHisto("hDX_X_23",  new TH2F("hDX_X_23",";X_{2};X_{2}-X_{3}",200,-40,40,200,-25,25));
  BookHisto("hDY_Y_12",  new TH2F("hDY_Y_12",";Y_{1};Y_{1}-Y_{2}",200,-25,25,200,-25,25));
  BookHisto("hDY_Y_13",  new TH2F("hDY_Y_13",";Y_{1};Y_{1}-Y_{3}",200,-25,25,200,-25,25));
  BookHisto("hDY_Y_23",  new TH2F("hDY_Y_23",";Y_{2};Y_{2}-Y_{3}",200,-25,25,200,-25,25));

  for (Int_t iS=0; iS<3; iS++) {
    BookHisto(Form("hResX_Sta%d",iS), new TH1F
	      (Form("hResX_Sta%d", iS), Form("Station %d;res X [mm];count",iS),200,-5.,5.));
    BookHisto(Form("hNormResX_Sta%d",iS), new TH1F
	      (Form("hNormResX_Sta%d", iS),Form("Station %d;norm res X;count",iS),200,-5.,5.));
    BookHisto(Form("hResY_Sta%d",iS), new TH1F
	      (Form("hResY_Sta%d", iS), Form("Station %d;res Y [mm];count",iS),200,-5.,5.));
    BookHisto(Form("hNormResY_Sta%d",iS), new TH1F
	      (Form("hNormResY_Sta%d", iS),Form("Station %d;norm res Y;count",iS),200,-5.,5.));
  }
}

void GigaTrackerRecoAlgorithm::Process(TRecoGigaTrackerEvent* event, Double_t RefTime) {
  if (!GetIsTree()) return;
  if (!GetEventHeader()) return; // do not run on NA62MC (i.e. not NA62Reco) output

  if (GetRunID()!=fRunID) {
    fRunID = GetRunID();
    fParameters = GigaTrackerParameterTools::GetInstance(fRunID,GetWithMC());
    StartOfRunUser();
  }

  Int_t nCand = event->GetNCandidates();
  for (Int_t iC=0; iC<nCand; iC++) event->RemoveCandidate(iC);
  TRecoGigaTrackerHit *Hit;
  Int_t iH=0;
  while (iH<event->GetNHits()) {
    Hit = static_cast<TRecoGigaTrackerHit*>(event->GetHit(iH));
    if (Hit==nullptr) {
      cout << user_normal() << "Error: null pointer to TRecoGigaTrackerHit" << endl;
      return;
    }

    Int_t pixelID   = Hit->GetPixelID();
    Int_t stationNo = Hit->GetStationNo();
    Int_t chip      = Hit->GetChipID();
    Double_t ToT    = Hit->GetToT();

    if (stationNo<0 || stationNo>=3 || pixelID >=18000) {
      cout << user_normal() << "Warning: invalid stationNo or pixelID (" <<
	stationNo <<", "<<pixelID<<"), hit removed" << endl;
      event->RemoveHit(iH);
      continue;
    }

    //////////////////////////////////////
    // Hit time correction (for data only)

    if (!GetWithMC() && fRedoTimeCorr) {
      Hit->SetTime(Hit->GetRawTime() - fT0[stationNo][pixelID] - fTW[stationNo][chip]->Eval(ToT));
    }

    ////////////////////////////////////////////////////////////
    // Station (x,y) alignment depending on fRedoXYCorr:
    // 0: alignment applied at NA62Reconstruction level is used;
    // 1: alignment is re-done;
    // 2: raw hit positions used, no alignment applied.

    if (fRedoXYCorr==1)
      Hit->SetPosition(Hit->GetRawPosition() - fPosOff[stationNo]);
    else if (fRedoXYCorr==2)
      Hit->SetPosition(Hit->GetRawPosition());

    iH++;
  }

  // BUILD CANDIDATES
  vector<int> vT;
  for (Int_t i=0; i<event->GetNHits(); i++) {
    Hit = static_cast<TRecoGigaTrackerHit*>(event->GetHit(i));
    if (fabs(Hit->GetTime()-RefTime)<=fTimeWindowWrtReference || Hit->GetIsPileUpHit() ) vT.push_back(i);
  }
  if (vT.size()>UInt_t(fNHitsMax)) {
    cout << user_normal() << "Error: more than " << fNHitsMax << " in-time hits, skipping event" << endl;
    return;
  }

  TimeOrder    to(event);
  XOrder       xo(event);
  YOrder       yo(event);
  StationOrder so(event);

  // Regroup hit per Block
  // i.e. hits that could come from the same particle
  vector<vector<int>> vBt, vBtx, vBtxy;
  vector<vector<int>>::iterator iBt, iBtx, iBtxy;
  Clusterize(vT, vBt, fTimeWindow, to, 1); //make [time] block

  // Building Candidate from Block
  multimap<int, Cluster> mC;
  vector<vector<int>> aB, vCx, vCxy;
  vector<vector<int>>::iterator iS, iCx, iCxy;
  vector<int>::iterator iHit;
  for (iBtxy = vBt.begin(); iBtxy!=vBt.end(); ++iBtxy) { // { h1,h0,h0,h2... }
    //for(iBtxy = vBtxy.begin(); iBtxy!=vBtxy.end(); iBtxy++) { // { h1,h0,h0,h2... }
    mC.clear();
    // Clustering Hits
    aB.clear();
    // Split block in station{ (h0,h0..), (h1,h1..), (h2,h2..) }
    Clusterize(*iBtxy, aB, 0, so, 0);
    for (iS = aB.begin(); iS!=aB.end(); ++iS) { //iS contains hit from one station (h0,h0...)

      // Split iS in space (x then y) clusters
      vCx.clear(); vCxy.clear();
      Clusterize(*iS, vCx, 0.410, xo, 0);
      for(iCx = vCx.begin(); iCx!=vCx.end(); ++iCx)  Clusterize(*iCx, vCxy,0.410, yo, 0);

      // Merge hits in Cluster
      for (iCxy = vCxy.begin(); iCxy!=vCxy.end(); ++iCxy) {
	Cluster cluster(event);
	for (iHit=iCxy->begin(); iHit!=iCxy->end(); ++iHit) {
	  cluster.add(*iHit);
	}
	mC.insert(pair<const int,Cluster>(cluster.S, cluster));
      }
    }

    //Building Candidate
    // Four Cases: GTK123, GTK13, GTK12, GTK23
    int NC0 = mC.count(0);
    int NC1 = mC.count(1);
    int NC2 = mC.count(2);
    int cType = -1;
    if (NC0>0  && NC1>0  && NC2>0)  cType = 123;
    if (NC0>0  && NC1>0  && NC2==0) cType =  12;
    if (NC0>0  && NC1==0 && NC2>0)  cType =  13;
    if (NC0==0 && NC1>0  && NC2>0)  cType =  23;

    TRecoGigaTrackerCandidate* cand;
    pair <multimap<int, Cluster>::iterator, multimap<int, Cluster>::iterator> ii0,ii1,ii2;
    multimap<int,Cluster>::iterator i0,i1,i2; 
    switch (cType) {  
    case 123: //############## GTK 123 ##############
      ii0=mC.equal_range(0); ii1=mC.equal_range(1); ii2=mC.equal_range(2);
      for(i0=ii0.first; i0!=ii0.second; ++i0){
	for(i1=ii1.first; i1!=ii1.second; ++i1){
	  for(i2=ii2.first; i2!=ii2.second; ++i2){
	    Cluster cs[3] = {i0->second, i1->second, i2->second};
	    cand = static_cast<TRecoGigaTrackerCandidate*>(event->AddCandidate());
	    for(int s(0); s<3; s++){
	      TVector3 pos(cs[s].X,cs[s].Y,cs[s].Z);
	      cand->SetType(cType);
	      cand->SetPosition(s,pos);
	      cand->SetTimeStation(s,cs[s].T);
	      for(iHit = cs[s].hits.begin();iHit != cs[s].hits.end(); ++iHit) cand->AddHit(*iHit);
	    }

	    BuildCandidate(cand);

	    // Fill monitoring histos
	    if (fFillHistograms) {
	      FillHisto("hMomentum", 1e-3*cand->GetMomentum().Mag());
	      FillHisto("hThX", 1e3*cand->GetMomentum().X()/cand->GetMomentum().Z()-1.2);
	      FillHisto("hThY", 1e3*cand->GetMomentum().Y()/cand->GetMomentum().Z());

	      FillHisto("hChi2",  cand->GetChi2());
	      FillHisto("hChi2X", cand->GetChi2X());
	      FillHisto("hChi2Y", cand->GetChi2Y());
	      FillHisto("hChi2T", cand->GetChi2Time());
	      FillHisto("hProbChi2X", TMath::Prob(cand->GetChi2X(),1));
	      FillHisto("hProbChi2Y", TMath::Prob(cand->GetChi2Y(),1));
	      FillHisto("hProbChi2T", TMath::Prob(cand->GetChi2Time(),2));

	      FillHisto("hDX_12", cand->GetPosition(0).X()-cand->GetPosition(1).X());
	      FillHisto("hDX_13", cand->GetPosition(0).X()-cand->GetPosition(2).X());
	      FillHisto("hDX_23", cand->GetPosition(1).X()-cand->GetPosition(2).X());

	      FillHisto("hDY_12", cand->GetPosition(0).Y()-cand->GetPosition(1).Y());
	      FillHisto("hDY_13", cand->GetPosition(0).Y()-cand->GetPosition(2).Y());
	      FillHisto("hDY_23", cand->GetPosition(1).Y()-cand->GetPosition(2).Y());

	      FillHisto("hDt_12", cand->GetTimeStation(0)-cand->GetTimeStation(1));
	      FillHisto("hDt_13", cand->GetTimeStation(0)-cand->GetTimeStation(2));
	      FillHisto("hDt_23", cand->GetTimeStation(1)-cand->GetTimeStation(2));
  
	      FillHisto("hDX_X_12", cand->GetPosition(0).X(), cand->GetPosition(0).X()-cand->GetPosition(1).X());
	      FillHisto("hDX_X_13", cand->GetPosition(0).X(), cand->GetPosition(0).X()-cand->GetPosition(2).X());
	      FillHisto("hDX_X_23", cand->GetPosition(1).X(), cand->GetPosition(1).X()-cand->GetPosition(2).X());

	      FillHisto("hDY_Y_12", cand->GetPosition(0).Y(), cand->GetPosition(0).Y()-cand->GetPosition(1).Y());
	      FillHisto("hDY_Y_13", cand->GetPosition(0).Y(), cand->GetPosition(0).Y()-cand->GetPosition(2).Y());
	      FillHisto("hDY_Y_23", cand->GetPosition(1).Y(), cand->GetPosition(1).Y()-cand->GetPosition(2).Y());
	    }

	    //tests a la giuseppe
	    double pkaon = cand->GetMomentum().Mag();
	    double dxdz = cand->GetMomentum().X()/cand->GetMomentum().Z();
	    double dydz = cand->GetMomentum().Y()/cand->GetMomentum().Z();
	    double chi2Time = cand->GetChi2Time();

	    if (pkaon<72000 || pkaon>78000  ||
		dxdz>0.0016 || dxdz<+0.0009 ||
		dydz>0.0004 || dydz<-0.0003 || chi2Time>fChi2T) {
	      event->RemoveCandidate(event->GetNCandidates()-1);
	    }

	    if (event->GetNCandidates()==fNTracksMax) {
	      cout << user_normal() << "Warning: " << fNTracksMax << " tracks reached" << endl;
	      goto too_many_tracks;
	    }
	  }
	}
      }
    }
    mC.clear();
  }

  if (fFillHistograms) FillHisto("hNGTKCandidates", event->GetNCandidates());

 too_many_tracks:

  if (fRedoXYCorr!=2) { // alignment is performed (i.e. not raw hit positions used)

    /////////////////////////////////////////////////////
    // GTK fine alignment correction to track directions:
    // d(x') = A+Bx; d(y') = C+Dy.
    // E Goudzovski, January 2018

    if (fFineCorr) {
      for (Int_t i=0; i<event->GetNCandidates(); i++) {
        TRecoGigaTrackerCandidate *Gcand = static_cast<TRecoGigaTrackerCandidate*>(event->GetCandidate(i));
        Double_t d_dxdz = fA + fB * Gcand->GetPosition(2).X();
        Double_t d_dydz = fC + fD * Gcand->GetPosition(2).Y();
        Double_t dPx    = Gcand->GetMomentum().Z() * d_dxdz * 1e-6;
        Double_t dPy    = Gcand->GetMomentum().Z() * d_dydz * 1e-6;
        TVector3 Corr   = TVector3(dPx, dPy, 0.0);
        TVector3 Mom    = Gcand->GetMomentum() + Corr;
        Gcand->SetMomentum(Mom);
      }
    }

    /////////////////////////////////////////////////////
    // Momentum scale correction (E Goudzovski, Feb 2018)

    if (fabs(fMomentumScale-1.0)>1e-4) {
      for (Int_t i=0; i<event->GetNCandidates(); i++) {
        TRecoGigaTrackerCandidate *Gcand = static_cast<TRecoGigaTrackerCandidate*>(event->GetCandidate(i));
        TVector3 Mom = (1.0/fMomentumScale) * Gcand->GetMomentum();
        Gcand->SetMomentum(Mom);
      }
    }
  }
}

void GigaTrackerRecoAlgorithm::InitCorrections() {
  fPosOff[0].SetXYZ(0.0, 0.0, 0.0); // station offsets [mm]
  fPosOff[1].SetXYZ(0.0, 0.0, 0.0);
  fPosOff[2].SetXYZ(0.0, 0.0, 0.0);
  fMomentumScale = 1.0;
  fA = fB = fC = fD = 0.0; // rotation parameters

  TString Line;
  if (fRedoXYCorr==1) {
    for (int iS(0); iS<3; iS++) fPosOff[iS] = fParameters->GetGigaTrackerStationMisalignment(iS); // [mm]
  }

  if (fFineCorr) {
    // Read the momentum scale parameter from the CDB
    Bool_t InputFound = false;
    TString MomScaleFile = GetWithMC() ? "GigaTracker-MomentumScaleMC.dat" : "GigaTracker-MomentumScale.dat";
    NA62ConditionsService::GetInstance()->Open(MomScaleFile);
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(MomScaleFile))) {
      if (Line.BeginsWith("#")) continue;
      TObjArray *l = Line.Tokenize(" ");
      Int_t    Run   = ((TObjString*)(l->At(0)))->GetString().Atoi();
      Double_t Scale = ((TObjString*)(l->At(1)))->GetString().Atof();
      delete l;
      if (Run!=GetRunID()) continue; // wrong run number
      fMomentumScale = Scale;
      InputFound = true;
    }
    NA62ConditionsService::GetInstance()->Close(MomScaleFile);
    if (InputFound) {
      cout << user_normal() << "Run " << GetRunID() <<": momentum scale = " << fMomentumScale << endl;
    }
    else {
      cout << user_normal() << "Warning: momentum scale for run " << GetRunID() << " not found" << endl;
    }

    // Read the rotation parameters from the CDB
    InputFound = false;
    TString RotationFile = GetWithMC() ? "GigaTracker-RotationMC.dat" : "GigaTracker-Rotation.dat";
    NA62ConditionsService::GetInstance()->Open(RotationFile);
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(RotationFile))) {
      if (Line.BeginsWith("#")) continue;
      TObjArray *l = Line.Tokenize(" ");
      Int_t Run = ((TObjString*)(l->At(0)))->GetString().Atoi();
      if (Run!=GetRunID()) { // wrong run number
	delete l;
	continue;
      }
      fA = ((TObjString*)(l->At(1)))->GetString().Atof();
      fB = ((TObjString*)(l->At(2)))->GetString().Atof();
      fC = ((TObjString*)(l->At(3)))->GetString().Atof();
      fD = ((TObjString*)(l->At(4)))->GetString().Atof();
      InputFound = true;
      delete l;
    }
    NA62ConditionsService::GetInstance()->Close(RotationFile);
    if (InputFound) {
      cout << user_normal() << "Run " << GetRunID() << ": rotation corrections A,B,C,D [x10^6] = " <<
	fA << " " << fB << " " << fC << " " << fD <<  endl;
    }
    else {
      cout << user_normal() << "Warning: rotaton constants for run " << GetRunID() << " not found" << endl;
    }
  }
}

void GigaTrackerRecoAlgorithm::InitTimeCorrections() {
  if (GetWithMC()) return;
  TString Line;
  //T0
  NA62ConditionsService::GetInstance()->Open(fTZeroFile);
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fTZeroFile))){
    if(Line.BeginsWith("#")) continue; //COMMENT LINE
    TObjArray *tok = Line.Tokenize(" ");
    if(tok->GetEntries()>1){
      Int_t    ChID = ((TObjString*)(tok->At(0)))->GetString().Atoi();
      Double_t t0   = ((TObjString*)(tok->At(1)))->GetString().Atof();
      Int_t iS  = ChID/100000;
      Int_t uid = ChID%100000;
      fT0[iS][uid] = t0;
    }
    delete tok;
  }
  NA62ConditionsService::GetInstance()->Close(fTZeroFile); 
  // TWALK BINS
  Line.Clear();
  //TW Binning
  NA62ConditionsService::GetInstance()->Open(fTWalkFile);
  double twBins[412];
  int nBins(0);
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fTWalkFile))){
    if(Line.BeginsWith("#")) continue; //COMMENT LINE
    else if (Line.BeginsWith("TimeWalkBinning")){
      TObjArray *tok = Line.Tokenize(" ");
      twBins[nBins] = ((TObjString*)(tok->At(1)))->GetString().Atof();
      nBins++;
      delete tok;
    }
  }
  NA62ConditionsService::GetInstance()->Close(fTWalkFile);
  if(NA62ConditionsService::GetInstance()->Open(fTWalkFile)!=kSuccess) return;
  for (int iS(0); iS < 3; iS++) {
    for (int iC(0); iC < 10; iC++) {
      fTW[iS][iC] = new TGraph(nBins);
    }
  }
  int iLine(0);
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fTWalkFile))){
    if(Line.BeginsWith("#")) continue; //COMMENT LINE
    else if (Line.BeginsWith("TimeWalk") && !Line.BeginsWith("TimeWalkBinning")){
      TObjArray *tok = Line.Tokenize(" ");
      Int_t StationID = iLine/(10*(nBins-1));
      Int_t ChipID    = (iLine/(nBins-1))%10;
      Int_t iBins = iLine%(nBins-1);
      fTW[StationID][ChipID]->SetPoint(iBins,twBins[iBins],((TObjString*)(tok->At(1)))->GetString().Atof());
      iLine++;
      delete tok;
    }
  }
  NA62ConditionsService::GetInstance()->Close(fTWalkFile);
}

void GigaTrackerRecoAlgorithm::BuildCandidate(TRecoGigaTrackerCandidate* cand) {
  Double_t BLbend    = -1.0 * fParameters->GetGigaTrackerMCBMagnetFieldStrength(0) * 
                        fParameters->GetGigaTrackerMCBMagnetLength().Z(); // to mm
  Double_t DeltaBend =  fParameters->GetGigaTrackerMCBMagnetPosition(1).Z() - 
                        fParameters->GetGigaTrackerMCBMagnetPosition(0).Z();
  Double_t Beta      =  1e-3 * fClight * BLbend * DeltaBend;
  Double_t BLtrim    =  fParameters->GetGigaTrackerMDXMagnetFieldStrength() * 
                        fParameters->GetGigaTrackerMDXMagnetLength().Z();
  Double_t Delta12   =  fParameters->GetGigaTrackerStationPositionRaw(1).Z() - 
                        fParameters->GetGigaTrackerStationPositionRaw(0).Z();
  Double_t Delta13   =  fParameters->GetGigaTrackerStationPositionRaw(2).Z() - 
                        fParameters->GetGigaTrackerStationPositionRaw(0).Z();
  Double_t Delta23   =  fParameters->GetGigaTrackerStationPositionRaw(2).Z() - 
                        fParameters->GetGigaTrackerStationPositionRaw(1).Z();
  Double_t alpha     = (fParameters->GetGigaTrackerStationPositionRaw(1).Z() - 
                        fParameters->GetGigaTrackerStationPositionRaw(0).Z()) / 
                       (fParameters->GetGigaTrackerStationPositionRaw(2).Z() - 
                        fParameters->GetGigaTrackerStationPositionRaw(0).Z());
  Double_t X[3], Xshift[3], Y[3], T[3];
  for (Int_t iStation=0; iStation<3; iStation++) {
    X[iStation] = cand->GetPosition(iStation).X();
    Xshift[iStation] = X[iStation];
    Y[iStation] = cand->GetPosition(iStation).Y();
    T[iStation] = cand->GetTimeStation(iStation); // ns
  }

  Double_t p = Beta / (Y[0] * (1.0-alpha) - Y[1] - fParameters->GetGigaTrackerStationPositionRaw(1).Y() + (alpha*Y[2]));
  Double_t ShiftTrim[3] = {0.,0.,0.};
  for (Int_t i=0; i<3; i++) {
    Double_t DeltaTrim = fParameters->GetGigaTrackerStationPositionCorrected(i).Z() -              
                         fParameters->GetGigaTrackerMDXMagnetPosition().Z();
    ShiftTrim[i] = -((1e-3*fClight * BLtrim) / p) * DeltaTrim;
    if (i<2) Xshift[i] += ShiftTrim[i]; 
  }

  // ---  Candidate Time
  // Double_t Time = (T[0] + T[1] + T[2]) / 3.0; //ns (old implemenation)
  // Weighted average to deal with double pixel hits 
  // 1/ count pixels per hit
  Int_t nPixels[3]={0,0,0};
  Bool_t IsPileUpHit[3] = {false, false, false};
  std::vector<Int_t> HitIndex[3];
  for (int iH=0; iH<cand->GetNHits(); iH++) {
    TRecoGigaTrackerHit * fGTKHit = static_cast<TRecoGigaTrackerHit*>(cand->GetHit(iH));
    Int_t iS = fGTKHit->GetStationNo();
    nPixels[iS]++;
    HitIndex[iS].push_back(iH);
    if ( fGTKHit->GetIsPileUpHit() ) IsPileUpHit[iS] = true; 
  }	
  // 2/ assign to the time measurement in each station an uncertainty based on the number of pixel/hit
  // DATA (2017)
  // 1-pixel hit: 0.135 ns (default)
  // 2-pixel hit: 0.180 ns (+35% degradation) 
  // pile-up hit: 0.600 ns (educated guess...)
  Double_t errT[3]={0.125, 0.125, 0.125}; // default
  for (Int_t iS=0; iS<3; iS++) {
    if (GetWithMC()){
      errT[iS] = fParameters->GetSigmaT(iS); // in order to match the smearing in digitizer 
      continue;
    }
    errT[iS] = nPixels[iS]==1 ? 0.135 : 0.180;
    if ( IsPileUpHit[iS] ) errT[iS] = 0.600;
  }
  // 3/ weighted average
  Double_t wSum(0.), tSum(0.);
  for (Int_t iS=0; iS<3; iS++) {
    wSum += 1./(errT[iS]*errT[iS]);
    tSum += T[iS]/(errT[iS]*errT[iS]);
  }  
  Double_t Time = tSum/wSum; // ns
  Double_t sigmaT = 1./TMath::Sqrt(wSum);

  // Constraints on relative cluster times
  Double_t chi2Time =
    TMath::Power((T[0]-Time)/errT[0], 2.) +
    TMath::Power((T[1]-Time)/errT[1], 2.) +
    TMath::Power((T[2]-Time)/errT[2], 2.);

  // --- Candidate ZX view
  // Track fitting to straight lines for horizontal view (X), with specific offset for GTK1 and GTK2 (trim effect corrected)
  Double_t a,b,rho,chi2X;
  Double_t sigmaX[3];
  for (int iS=0; iS<3; iS++) { // begin of loop on iS
    // 1-pixel hit
    if ( nPixels[iS]==1 ) {
      sigmaX[iS] = 0.0866; // 300um pixel
      // 400um pixel
      Int_t itH = HitIndex[iS][0];
      TRecoGigaTrackerHit * fGTKHit = static_cast<TRecoGigaTrackerHit*>(cand->GetHit(itH));
      Int_t iCol = fGTKHit->GetColumn();
      if ( iCol>0 && iCol<199 && (iCol%40==0 || iCol%40==39) ) {
        sigmaX[iS]=0.1155;
      } 
    }
    else sigmaX[iS] = 0.0866;
  } // end of loop on iS

  // Add Multiple Scattering
  Double_t GTKthickness[2] = {0.,0.}; // thickness for GTK2 and GTK3 = Z_chip + Z_sensor + Z_cooling_plate
  Double_t theta[2];
  for (Int_t i=0; i<2; i++){
    GTKthickness[i] = fParameters->GetGigaTrackerSensorLength(i+1).Z() + 
                      fParameters->GetGigaTrackerChipLength(i+1).Z() + 
                      fParameters->GetCoolingPlateLength(i+1).Z() - 
                      fParameters->GetCoolingPlateBottomDepth(i+1) - 
                      fParameters->GetCoolingPlateTopDepth(i+1);
    theta[i]        = 13.6e6/p * TMath::Sqrt(GTKthickness[i]/93.7) *
                      (1 + 0.038 * TMath::Log(GTKthickness[i]/93.7));
  }

  sigmaX[1] = TMath::Sqrt( sigmaX[1]*sigmaX[1]
  			   + 0.002*ShiftTrim[1]*0.002*ShiftTrim[1]
  			   + (theta[1] * Delta23)*(theta[1] * Delta23));
  sigmaX[0] = TMath::Sqrt( sigmaX[0]*sigmaX[0]
  			   + 0.002*ShiftTrim[0]*0.002*ShiftTrim[0]
  			   + (theta[1] * Delta13)*(theta[1] * Delta13)
  			   + (theta[0] * Delta12)*(theta[0] * Delta12));

  Double_t z[3] = {0, Delta12, Delta13};
  LinearLeastSquareFit(z,Xshift,3,sigmaX,a,b,rho,chi2X);
  //Double_t dxdz = (X[2] - X[0]) / fDelta13 - (((1e-3*fClight * fBLtrim) / p) * (1. - (fDeltaTrim[2] / fDelta13)));
  Double_t dxdz = b;

  if (fFillHistograms) {
    for (UInt_t iS=0; iS<3; iS++){
      FillHisto(Form("hResX_Sta%d",iS),     Xshift[iS]-(a+b*z[iS]));
      FillHisto(Form("hNormResX_Sta%d",iS),(Xshift[iS]-(a+b*z[iS]))/sigmaX[iS]);
    }
  }

  // --- Candidate YZ view
  Double_t dydz = (Y[2] - Y[0]) / Delta13;

  // --- Candidate kinematics
  Double_t pz = p / TMath::Sqrt(1. + dxdz * dxdz + dydz * dydz);
  TVector3 Momentum;
  Momentum.SetXYZ(1e-6*pz * dxdz, 1e-6*pz * dydz, 1e-6*pz);

  // Constraints on vertical view (Y)
  Double_t sigmaY12 = 1.42;
  Double_t sigmaY23 = 1.20;
  Double_t chi2Y = TMath::Power((Y[1] - Y[0])/sigmaY12, 2.) + TMath::Power((Y[2] - Y[1])/sigmaY23, 2.);

  // Global Chi2
  Double_t chi2 = chi2X + chi2Y + chi2Time;

  ////////////////////////////////////////////////////////////////////
  // Timing corrections, disabled by default (E Goudzovski, June 2018)

  if (fFineTimeCorr) {
    // Apply time smearing to match MC to data v0.11.2:
    // the resolutions are 125ps (MC) + 85ps (smearing) = 150ps (data)
    if (GetWithMC()) Time += gRandom->Gaus(0.0, 0.085);
  }

  // Candidate
  cand->SetMomentum(Momentum);
  cand->SetTime(Time);
  cand->SetChi2X(chi2X);
  cand->SetChi2Y(chi2Y);
  cand->SetChi2Time(chi2Time);
  cand->SetChi2(chi2);
  cand->SetTimeError(sigmaT);
}

void GigaTrackerRecoAlgorithm::LinearLeastSquareFit(Double_t *x, Double_t *y, Int_t Nsample, Double_t *sigma, Double_t &a, Double_t &b, Double_t &rho, Double_t &chi2){
// least square method applied to straight line (Y = a + b*X) weighted by the errors
    Double_t sumx = 0.;
    Double_t sumy = 0.;
    Double_t sumxy= 0.;
    Double_t sumxx= 0.;
    Double_t sums = 0.;

    // accumulate
    for(Int_t i=0; i < Nsample ; i++){
      Double_t wgt = 1./(sigma[i]*sigma[i]);
      sumx  += x[i]*wgt;
      sumxx += x[i]*x[i]*wgt; 
      sumy  += y[i]*wgt; 
      sumxy += x[i]*y[i]*wgt; 
      sums  += wgt;
    }
    a = (sumxx*sumy  - sumx*sumxy) / (sums*sumxx - sumx*sumx);
    b = (sums *sumxy - sumx*sumy ) / (sums*sumxx - sumx*sumx);
    rho = - sumx / TMath::Sqrt(sums*sumxx);

    chi2 = 0.0;
    for(Int_t i=0; i < Nsample ; i++){
      chi2 += ((y[i] - a - b*x[i])/sigma[i]) * ((y[i] - a - b*x[i])/sigma[i]) ;
    }
}

template<typename Order> void GigaTrackerRecoAlgorithm::Clusterize
(vector<int> v, vector<vector<int>>& clusters, double minDist, Order order, int minCont){
  if (v.size()==0) return;
  std::sort(v.begin(),v.end(),order); //sort first mpt 13/12/2016
  vector<int> aCluster;
  vector<int>::iterator iV = v.begin(); //sort first mpt 13/12/2016

  while(iV!=v.end()){
    //seed
    aCluster.clear();
    int seed = (*iV);
    //clusterize
    while(iV!=v.end() && order.dist(seed,(*iV))<=minDist ) {
      aCluster.push_back(*iV);
      seed = (*iV); // mpt 13/12/2016
      ++iV;
    }
    //store clusters
    if(aCluster.size()>(UInt_t)minCont) clusters.push_back(aCluster);
    aCluster.clear();
  }
  return;
}
