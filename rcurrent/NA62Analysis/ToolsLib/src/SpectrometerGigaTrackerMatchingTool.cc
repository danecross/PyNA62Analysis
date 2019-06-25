// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-05-28
// Based on the code developed by Giuseppe Ruggiero
//
// ---------------------------------------------------------------

/// \class SpectrometerGigaTrackerMatchingTool
/// \Brief
/// Matching of Spectrometer to GigaTracker tracks
/// \EndBrief
/// \Detailed
/// Matching of a single Spectrometer track to each reconstructed GigaTracker track
/// based on blue field corrected distance of approach (CDA) and timing.
/// For the matching to a given GTK track to be successful, the GTK track should be consistent
/// with the beam profile (in terms of momentum and direction), have a good quality
/// (as determnined by its chi2), and consistent to the reference time within the
/// matching window specified by the user.
/// Inputs for the matching algorithm Match() for each spectrometer track:
/// GTK event, spectrometer track, reference time and reference detector ID (kCedar, kCHOD or kRICH).
/// Outputs: vectors of vertex coordinates, CDA, raw and blue tube corrected beam track momenta,
/// blue tube corrected spectrometer track momenta and discriminants for each GTK track;
/// the "best" GTK candidate (with the largest discriminant value) and the "best" discriminant.
/// Example of use:
/// \code
/// // Code in the constructor of user analyzer
/// fSG = new SpectrometerGigaTrackerMatchingTool();
/// fSG->SetMatchingTimingCuts(-0.5, 0.5); // [ns]
/// ...
/// // Code in the Process() method of user analyzer
/// // a) perform the matching for a given spectrometer track
/// fSG->Match(GTKevent, Tracks[itr].GetSpectrometerCandidate(), CedarTime, kCedar);
/// // b) retrieve the outcome of the best matched GTK track (i.e. the largest discriminant)
/// Double_t BestTimeDifference = fSG->GetBestDeltaTime();
/// Double_t BestCDA = fSG->GetBestCDA();
/// TVector3 BeamMomentum  = fSG->GetBestCorrectedBeamMomentum(); // beam track, corrected for blue field
/// TVector3 TrackMomentum = fSG->GetBestCorrectedTrackMomentum(); // spectrometer track, corrected for blue field
/// // c) if needed, retrieve the outcome for any GTK track with index iTrack
/// Double_t TimeDifference = fSG->GetDeltaTime(iTrack);
/// Double_t CDA = fSG->GetCDA(iTrack);
/// ...
/// \endcode
/// \author Giuseppe Ruggiero (Giuseppe.Ruggiero@cern.ch)
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "SpectrometerGigaTrackerMatchingTool.hh"
#include "BeamParameters.hh"
#include "BlueTubeTracker.hh"

using namespace std;

SpectrometerGigaTrackerMatchingTool::SpectrometerGigaTrackerMatchingTool() {
  // Initialize the PDFs
  DiscriminantNormalization(kCedar); // the same discriminant is used for Cedar and RICH reference
  DiscriminantNormalization(kCHOD);
  Reset();
  fMinTimingCut = -1.0; // [ns]
  fMaxTimingCut = +1.0;
  fCDAcomp = new TwoLinesCDA();
}

SpectrometerGigaTrackerMatchingTool::~SpectrometerGigaTrackerMatchingTool() {
  if (fCDAcomp) delete fCDAcomp;
}

void SpectrometerGigaTrackerMatchingTool::Reset() {
  fBestIndex = -1; // -1 means no association, i.e. MatchingGTKTrackFound() returns false
  fSecondBestIndex = -1;
  fBestDiscriminant = 0.0; // this is the smallest (i.e. worst) possible discriminant
  fSecondBestDiscriminant = 0.0;
  fCandidateIsMatched.clear();
  fVertex.clear();
  fBeamMomentum.clear();
  fCorrectedBeamMomentum.clear();
  fCorrectedTrackMomentum.clear();
  fCDA.clear();
  fDeltaTime.clear();
  fDiscriminant.clear();
  fDiscriminantPileup.clear();
  fNGTKCandidates        = 0;
  fNInTimeGTKCandidates  = 0;
  fNMatchedGTKCandidates = 0;
  fClosestDeltaTime      = 999.0;
}

void SpectrometerGigaTrackerMatchingTool::Match
(TRecoGigaTrackerEvent *event, TRecoSpectrometerCandidate *Scand, Double_t RefTime, Int_t RefDet) {

  if (RefDet!=kCedar && RefDet!=kCHOD && RefDet!=kRICH) {
    cout << "[SpectrometerGigaTrackerMatchingTool] Warning: invalid reference detector specified" << endl;
  }

  Reset();
  fNGTKCandidates = event->GetNCandidates();
  fNInTimeGTKCandidates = 0;
  fNMatchedGTKCandidates = 0;
  for (Int_t iGTK=0; iGTK<fNGTKCandidates; iGTK++) { // loop over GTK tracks
    TRecoGigaTrackerCandidate *Gcand = static_cast<TRecoGigaTrackerCandidate*>(event->GetCandidate(iGTK));

    Double_t chi2beam = Chi2CandidateBeam(Gcand); // GTK track compatibility with beam
    Double_t chi2     = Gcand->GetChi2();
    Double_t dt       = Gcand->GetTime()-RefTime;

    if (fabs(dt)<fabs(fClosestDeltaTime)) fClosestDeltaTime = dt;
    Bool_t GoodCandidate = (chi2beam<20.0 && chi2<50.0 && dt>fMinTimingCut && dt<fMaxTimingCut);

    if (dt>fMinTimingCut && dt<fMaxTimingCut) fNInTimeGTKCandidates++;
    if (GoodCandidate) fNMatchedGTKCandidates++;

    fCDATemp = -999.0;
    fVertexTemp = TVector3(0.0, 0.0, 0.0);
    fBeamMomentumTemp = TVector3(0.0, 0.0, 0.0);
    fTrackMomentumTemp = TVector3(0.0, 0.0, 0.0);
    fDiscr[0] = fDiscr[1] = 0.0; // this is the worst possible discriminant value
    if (GoodCandidate) {
      ComputeVertex(Gcand, Scand);
      ComputeDiscriminant(fCDATemp, dt, RefDet);
    }
    fCandidateIsMatched.push_back(GoodCandidate); // "IsMatched" means GTK track consistent with beam profile and in time
    fVertex.push_back(fVertexTemp);
    fBeamMomentum.push_back(Gcand->GetMomentum());
    fCorrectedBeamMomentum.push_back(fBeamMomentumTemp);
    fCorrectedTrackMomentum.push_back(fTrackMomentumTemp);
    fCDA.push_back(fCDATemp);
    fDeltaTime.push_back(dt);
    fDiscriminant.push_back(fDiscr[0]);
    fDiscriminantPileup.push_back(fDiscr[1]);
  }

  // Find the best and second best GTK tracks
  fBestIndex = -1;
  fBestDiscriminant = 0.0;
  for (Int_t i=0; i<fNGTKCandidates; i++) {
    if (!fCandidateIsMatched[i]) continue; // "IsMatched" means GTK track consistent with beam profile and in time
    if (fBestIndex<0 || fDiscriminant[i]>fBestDiscriminant) {
      fBestIndex = i;
      fBestDiscriminant = fDiscriminant[i];
      fBestDiscriminantPileup = fDiscriminantPileup[i];
    }
  }
  fSecondBestIndex = -1;
  fSecondBestDiscriminant = 0.0;
  for (Int_t i=0; i<fNGTKCandidates; i++) {
    if (i==fBestIndex) continue;
    if (!fCandidateIsMatched[i]) continue;
    if (fSecondBestIndex<0 || fDiscriminant[i]>fSecondBestDiscriminant) {
      fSecondBestIndex = i;
      fSecondBestDiscriminant = fDiscriminant[i];
      fSecondBestDiscriminantPileup = fDiscriminantPileup[i];
    }
  }
}

//////////////////////////////////////////////////////////////////////
// GTK-Spectrometer track vertex computation accounting for blue field

void SpectrometerGigaTrackerMatchingTool::ComputeVertex
(TRecoGigaTrackerCandidate *Gcand, TRecoSpectrometerCandidate *Scand) {

  fBeamMomentumTemp  = Gcand->GetMomentum();
  fTrackMomentumTemp = Scand->GetThreeMomentumBeforeMagnet();

  // Vertex: first iteration without blue field correction
  fCDAcomp->SetLine1Point1(Gcand->GetPosition(2)); // position at GTK3
  fCDAcomp->SetDir1(fBeamMomentumTemp);
  fCDAcomp->SetLine2Point1(Scand->GetPositionBeforeMagnet());
  fCDAcomp->SetDir2(fTrackMomentumTemp);
  fCDAcomp->ComputeVertexCDA();
  fVertexTemp = fCDAcomp->GetVertex();
  fCDATemp = fCDAcomp->GetCDA();

  // No blue tube corrections for unphysical vertices
  if (fVertexTemp.z()>180000.0) return;

  // Correct GTK track momentum for blue field
  BlueTubeTracker::GetInstance()->SetCharge(1);
  BlueTubeTracker::GetInstance()->SetInitialPosition(Gcand->GetPosition(2));
  BlueTubeTracker::GetInstance()->SetInitialMomentum(fBeamMomentumTemp);
  BlueTubeTracker::GetInstance()->SetZFinal(fVertexTemp.Z());
  BlueTubeTracker::GetInstance()->TrackParticle();
  TVector3 pos1 = BlueTubeTracker::GetInstance()->GetFinalPosition();
  fBeamMomentumTemp = BlueTubeTracker::GetInstance()->GetFinalMomentum();

  // Correct spectrometer track momentum for blue field
  BlueTubeTracker::GetInstance()->SetCharge(Scand->GetCharge());
  BlueTubeTracker::GetInstance()->SetInitialPosition(Scand->GetPositionBeforeMagnet());
  BlueTubeTracker::GetInstance()->SetInitialMomentum(fTrackMomentumTemp);
  BlueTubeTracker::GetInstance()->SetZFinal(fVertexTemp.Z());
  BlueTubeTracker::GetInstance()->TrackParticle();
  TVector3 pos2 = BlueTubeTracker::GetInstance()->GetFinalPosition();
  fTrackMomentumTemp = BlueTubeTracker::GetInstance()->GetFinalMomentum();

  // Vertex: second iteration with momentum corrected for blue field
  fCDAcomp->SetLine1Point1(pos1);
  fCDAcomp->SetDir1(fBeamMomentumTemp);
  fCDAcomp->SetLine2Point1(pos2);
  fCDAcomp->SetDir2(fTrackMomentumTemp);
  fCDAcomp->ComputeVertexCDA();
  fVertexTemp = fCDAcomp->GetVertex();
  fCDATemp = fCDAcomp->GetCDA();
}

///////////////////////////////////////////////////////////
// GTK track compatibility with beam momentum and direction

Double_t SpectrometerGigaTrackerMatchingTool::Chi2CandidateBeam(TRecoGigaTrackerCandidate *cand) {

  Double_t p    = cand->GetMomentum().Mag(); // [GeV/c]
  Double_t dxdz = cand->GetMomentum().X()/cand->GetMomentum().Z();
  Double_t dydz = cand->GetMomentum().Y()/cand->GetMomentum().Z();

  Double_t pbeam     = BeamParameters::GetInstance()->GetBeamMomentum();
  Double_t dxdz_beam = BeamParameters::GetInstance()->GetBeamXSlope();
  Double_t dydz_beam = BeamParameters::GetInstance()->GetBeamYSlope();

  return (p-pbeam)*(p-pbeam)/(900.0*900.0)+
    (dxdz-dxdz_beam)*(dxdz-dxdz_beam)/(1.2e-4*1.2e-4)+
    (dydz-dydz_beam)*(dydz-dydz_beam)/(1.0e-4*1.0e-4);
}

//////////////////////////////////////////
// Initialization of the discriminant PDFs

void SpectrometerGigaTrackerMatchingTool::DiscriminantNormalization(Int_t RefDet) {
  double pass_cda = 0.01; // [mm]
  double pass_dt = 0.001; // [ns]

  Int_t jD = (RefDet==kCedar || RefDet==kRICH) ? 0 : 1;

  fPDFKaonCDA[jD].clear();
  fIntPDFKaonCDA[jD] = 0;
  fPDFPileCDA[jD].clear();
  fIntPDFPileCDA[jD] = 0;
  for (Int_t jbincda=0; jbincda<6000; jbincda++) { // < 60 mm
    Double_t cda = pass_cda*jbincda+0.5*pass_cda;
    Double_t pdfkc = PDFKaonCDA(cda);
    if (pdfkc<0) pdfkc=0.0;
    fIntPDFKaonCDA[jD] += pdfkc*pass_cda/0.25;
    // if (fIntPDFKaonCDA[jD]>=1) fIntPDFKaonCDA[jD] = 1;
    fPDFKaonCDA[jD].push_back(fIntPDFKaonCDA[jD]);
    Double_t pdfpc = PDFPileCDA(cda);
    if (pdfpc<0) pdfpc=0.0;
    fIntPDFPileCDA[jD] += pdfpc*pass_cda/0.25;
    // if (fIntPDFPileCDA[jD]>=1) fIntPDFPileCDA[jD] = 1;
    fPDFPileCDA[jD].push_back(fIntPDFPileCDA[jD]);
  }
  fPDFKaonDT[jD].clear();
  fIntPDFKaonDT[jD] = 0;
  fPDFPileDT[jD].clear();
  fIntPDFPileDT[jD] = 0;
  for (Int_t jbindt=0; jbindt<1000; jbindt++) { // +-1 ns
    Double_t dt = pass_dt*jbindt+0.5*pass_dt;
    fIntPDFKaonDT[jD] += (PDFKaonDT(dt,RefDet)+PDFKaonDT(-dt,RefDet))*pass_dt/0.01;
    fPDFKaonDT[jD].push_back(fIntPDFKaonDT[jD]);
    fIntPDFPileDT[jD] += (PDFPileDT(dt,RefDet)+PDFPileDT(-dt,RefDet))*pass_dt/0.01;
    fPDFPileDT[jD].push_back(fIntPDFPileDT[jD]);
  }
}

//////////////////////////////////////////////////////////
// Computation of the discriminant based on CDA and timing

void SpectrometerGigaTrackerMatchingTool::ComputeDiscriminant
(Double_t cda_val, Double_t dt_val, Int_t RefDet) {

  Int_t jD = (RefDet==kCedar || RefDet==kRICH) ? 0 : 1;

  fDiscr[0] = 0.0;
  fDiscr[1] = 0.0;
  if (cda_val>59.9 || fabs(dt_val)>0.95) return;

  double pass_cda = 0.01; // mm
  double pass_dt = 0.001; //0.05; // ns
  double pkcda = 0.;
  vector<double>::iterator ikcda = fPDFKaonCDA[jD].begin();
  while(ikcda!=fPDFKaonCDA[jD].end()) {
    double cda = (double)(distance(fPDFKaonCDA[jD].begin(),ikcda)-1)*pass_cda;
    if ((pkcda=EvaluateCondition(cda,cda_val,*ikcda))>=0) break;
    ++ikcda;
  }
  pkcda /= fIntPDFKaonCDA[jD];
  double pkdt = 0.;
  vector<double>::iterator ikdt = fPDFKaonDT[jD].begin();
  while(ikdt!=fPDFKaonDT[jD].end()) {
    double dt = (double)(distance(fPDFKaonDT[jD].begin(),ikdt)-1)*pass_dt;
    if ((pkdt=EvaluateCondition(fabs(dt),fabs(dt_val),*ikdt))>=0) break;
    ++ikdt;
  }
  pkdt /= fIntPDFKaonDT[jD];
  double ppcda = 0.;
  vector<double>::iterator ipcda = fPDFPileCDA[jD].begin();
  while(ipcda!=fPDFPileCDA[jD].end()) {
    double cda = (double)(distance(fPDFPileCDA[jD].begin(),ipcda)-1)*pass_cda;
    if ((ppcda=EvaluateCondition(cda,cda_val,*ipcda))>=0) break;
    ++ipcda;
  }
  ppcda /= fIntPDFPileCDA[jD];
  double ppdt = 0.;
  vector<double>::iterator ipdt = fPDFPileDT[jD].begin();
  while(ipdt!=fPDFPileDT[jD].end()) {
    double dt = (double)(distance(fPDFPileDT[jD].begin(),ipdt)-1)*pass_dt;
    if ((ppdt=EvaluateCondition(fabs(dt),fabs(dt_val),*ipdt))>=0) break;
    ++ipdt;
  }
  ppdt /= fIntPDFPileDT[jD];

  fDiscr[0] = (1.0-pkcda)*(1.0-pkdt); // kaon
  fDiscr[1] = (1.0-ppcda)*(1.0-ppdt); // pileup
}

Double_t SpectrometerGigaTrackerMatchingTool::GetDiscriminantValue(Double_t cda, Double_t dt, Int_t RefDet) {
  ComputeDiscriminant(cda, dt, RefDet);
  return fDiscr[0];
}

// PDF projections
Double_t SpectrometerGigaTrackerMatchingTool::PDFKaonCDA(Double_t cda) {
  Double_t cdapar[8];
  cdapar[0] = 7.02e-02;
  cdapar[1] = 1.47e+00;
  cdapar[2] = 2.29e-02;
  cdapar[3] = 2.60e+00;
  cdapar[4] = 1.45e-02;
  cdapar[5] = 3.19e-01;
  cdapar[6] = 3.3e-05;
  cdapar[7] =-1.6e-06;
  Double_t cdag1 = cdapar[0]*exp(-0.5*(cda/cdapar[1])*(cda/cdapar[1]));
  Double_t cdag2 = cdapar[2]*exp(-0.5*(cda/cdapar[3])*(cda/cdapar[3]));
  Double_t cdaf1 = cdapar[4]*exp(-cdapar[5]*cda)+cdapar[6]+cdapar[7]*cda;
  if (cda>25.) return 0.0;
  return cdag1+cdag2+cdaf1;
}

Double_t SpectrometerGigaTrackerMatchingTool::PDFKaonDT(Double_t dt, Int_t RefDet) {
  Double_t dtpar[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
  if (RefDet==kCedar || RefDet==kRICH) { // KTAG - GTK association
    dtpar[0] = 2.4e-02;
    dtpar[1] = 0.;
    dtpar[2] = 1.3e-01;
    dtpar[3] = 3.2e-03;
    dtpar[4] = 0.;
    dtpar[5] = 2.1e-01;
  }
  else if (RefDet==kCHOD) { // CHOD - GTK association, r1336
    dtpar[0] = 1.5e-02;
    dtpar[1] = 0.;
    dtpar[2] = 1.9e-01;
    dtpar[3] = 2.6e-03;
    dtpar[4] = 0.;
    dtpar[5] = 4.0e-01;
  }
  Double_t dtg1 = dtpar[0]*exp(-0.5*((dt-dtpar[1])/dtpar[2])*((dt-dtpar[1])/dtpar[2]));
  Double_t dtg2 = dtpar[3]*exp(-0.5*((dt-dtpar[4])/dtpar[5])*((dt-dtpar[4])/dtpar[5]));
  return dtg1+dtg2;
}

Double_t SpectrometerGigaTrackerMatchingTool::PDFPileCDA(Double_t cda) {
  Double_t cdapar[6]; // "Universal" pdf
  cdapar[0] = 0.;
  cdapar[1] = 1.5e-03;
  cdapar[2] = 1.97e+01;
  cdapar[3] = 9.8e+00;
  cdapar[4] = 1.629e-02;
  cdapar[5] = 1.04e+01;
  Double_t cdap0 = cdapar[0];
  Double_t cdag1 = cdapar[1]*exp(-0.5*((cda-cdapar[2])/cdapar[3])*((cda-cdapar[2])/cdapar[3]));
  Double_t cdag2 = cdapar[4]*exp(-0.5*((cda)/cdapar[5])*((cda)/cdapar[5]));
  return cdap0+cdag1+cdag2;
}

Double_t SpectrometerGigaTrackerMatchingTool::PDFPileDT(Double_t dt, Int_t RefDet) {
  Double_t dtpar[4] = {0.0, 0.0, 0.0, 0.0};
  if (RefDet==kCedar || RefDet==kRICH) { // KTAG - GTK association
    dtpar[0] = 4.6e-03;
    dtpar[1] = 0.;
    dtpar[2] = 0.;
    dtpar[3] = 0.;
  }
  else if (RefDet==kCHOD) { // CHOD - GTK association
    dtpar[0] = 3.65e-03;
    dtpar[1] = 0.;
    dtpar[2] = 0.;
    dtpar[3] = 0.;
  }
  return dtpar[0]+dtpar[1]*dt+dtpar[2]*dt*dt+dtpar[3]*dt*dt*dt*dt;
}

void SpectrometerGigaTrackerMatchingTool::Print() {
  cout << "[SpectrometerGigaTrackerMatchingTool] Matched GTK tracks: " << fNMatchedGTKCandidates << endl;
  cout << "Best GTK track: " << fBestIndex << "; second-best GTK track: " << fSecondBestIndex << endl;
  for (Int_t i=0; i<fNGTKCandidates; i++) {
    cout << "GTK track " << Form("%2d",i) << ": " <<
      (fCandidateIsMatched[i] ? "matched    ":"not matched") <<
      ", CDA=" << (fCandidateIsMatched[i] ? Form("%5.2f", fCDA[i]) : "n/a  ") <<
      ", dT="  << Form("%5.2f", fDeltaTime[i]) <<
      ", D="   << (fCandidateIsMatched[i] ? Form("%5.3f", fDiscriminant[i]) : "n/a  ") <<
      ", D(pileup) = " << (fCandidateIsMatched[i] ? Form("%5.3f", fDiscriminantPileup[i]) : "n/a") << endl;
  }
}
