// ---------------------------------------------------------------------------------
// History:
//
// Created by Daniel Moise (razvan.daniel.moise@cern.ch)  07 june 2017
//
// ---------------------------------------------------------------------------------

/// \class SpectrometerGigaTrackerMVAMatchingTool
/// \Brief
/// Matches downstream track with the best upstream track 
/// \EndBrief
/// \Detailed
/// A neural network is used to determine the best upstream match to the given downstream track.
/// Information from upstream and downstream is fed to the network, which gives a response usually in [0,1).
/// The higher the response, the more compatible the two pairs of tracks are.
/// Performance  of default network: mismatch probability ~ 1.3%, no-match probability ~ 19% (assuming 11% GTK reconstruction inefficiency)
/// The variables used are: upstream & downstream chi2, time, number of hits in the GTK, and 
/// the (x,y) coordinates of the two tracks extrapolated to the vertical plane containing the CDA vertex.
/// The time is the difference between GTK candidate time and a reference time, which should be set by the user.
/// The extrapolation is done by BlueTubeTracker, whilst the vertex is found by TwoLinesCDA.
/// This tool can be used to study any decay channel; it has been tested on K3pi and optimised for Kpinunu.
/// The matching can either be performed on individual pairs of tracks using the method MatchIndividually(), which outputs the response for the given pair,
/// or across all upstream tracks for a given downstream track using the method PerformMatching(), which finds the index of the GTK candidate that gives 
/// the maximum response in association to the given downstream track.
/// The former method is more straightforward, as the user only needs to set the downstream track, upstream tracks and reference time.
/// The latter method is more flexible and allows the user more freedom to optimise the matching, especially for multitrack events.
/// Example 1: determining the best match with no adjustments from the user:
/// \code 
/// SpectrometerGigaTrackerMVAMatchingTool *matching = new SpectrometerGigaTrackerMVAMatchingTool();
/// matching->SetReferenceTime(myTime);
/// TRecoGigaTrackerEvent *evt = (TRecoGigaTrackerEvent*)GetEvent("GigaTracker");
/// matching->SetUpstreamTracks((TRecoGigaTrackerEvent*)evt);
/// std::vector<DownstreamTrack> Tracks = *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
/// for(int i = 0; i < NDownstreamTracks; i++){
/// 	matching->SetDownstreamTrack (&Tracks[i]);
/// 	matching->PerformMatching();
///	Int_t MatchIndex = matching->GetMatchIndex();
///	TRecoGigaTrackerCandidate *BestMatch = evt->GetCandidate(MatchIndex);
///	//Additional outputs also available
///	TVector3 MatchVertex = matching->GetMatchVertex();
///	TVector3 MatchUpstreamMomentum = matching->GetMatchUpstreamMomentum();
///	TVector3 MatchDownstreamMomentum = matching->GetMatchDownstreamMomentum();
///	Double_t MatchCDA = matching->GetMatchCDA();
///	Double_t MatchTime = matching->GetMatchTime();
///	...}
/// \endcode
/// Example 2: the user wants to adjust downstream information:
/// \code 
/// SpectrometerGigaTrackerMVAMatchingTool *matching = new SpectrometerGigaTrackerMVAMatchingTool();
/// matching->SetReferenceTime(myTime);
/// TRecoGigaTrackerEvent *evt = (TRecoGigaTrackerEvent*)GetEvent("GigaTracker");
/// matching->SetUpstreamTracks(evt);
/// std::vector<DownstreamTrack> Tracks = *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
/// for(int i = 0; i < NDownstreamTracks; i++){
/// 	matching->SetDownstreamTrack (Tracks[i]);
///	matching->SetDownstreamMomentum (myMomentum);
///	for(int j = 0; j < NUpstreamTracks; j++){
/// 		Double_t PairResponse = matching->MatchIndividually(j, 1, myVertexZ);
///		//algorithm to pick best match from the set of responses goes here - max response recommended
///	}
///	Int_t MatchIndex = matching->GetMatchIndex();
///	TRecoGigaTrackerCandidate *BestMatch = evt->GetCandidate(MatchIndex);
///	...}
/// \endcode
/// This has uses e.g. for multitrack events, if the user wants to use the VertexBuilder to get the vertex, as well as to use adjusted momenta
/// The performance of the tool is improved in this case (0.9% mismatch probability, 17% no-match probability)
/// \author Daniel Moise (razvan.daniel.moise@cern.ch)
/// \EndDetailed

#include "SpectrometerGigaTrackerMVAMatchingTool.hh"

using namespace std;

SpectrometerGigaTrackerMVAMatchingTool::SpectrometerGigaTrackerMVAMatchingTool():
	fUpstreamChi2(0.),
	fDownstreamChi2(0.),
	fUpstreamX(0.),
	fUpstreamY(0.),
	fDownstreamX(0.),
	fDownstreamY(0.),
	fTime(0.),
	fNGTKHits(0.),
	fUpstreamTracks(nullptr),
	fDownstreamTrack(nullptr)
{
	reader = new TMVA::Reader();
	reader->AddVariable("DownstreamChi2", &fDownstreamChi2);
	reader->AddVariable("UpstreamChi2", &fUpstreamChi2);
	reader->AddVariable("VertexX", &fDownstreamX);
	reader->AddVariable("VertexY", &fDownstreamY);
	reader->AddVariable("CandidateX", &fUpstreamX);
	reader->AddVariable("CandidateY", &fUpstreamY);
	reader->AddVariable("Time", &fTime);
	reader->AddVariable("nHits", &fNGTKHits);
	reader->BookMVA("MLP method", fWeights);
}

void SpectrometerGigaTrackerMVAMatchingTool::SetUpstreamTracks(TRecoGigaTrackerEvent* value){
	fUpstreamTracks = value;
	SetNGTKHits(value->GetNHits());
}

void SpectrometerGigaTrackerMVAMatchingTool::SetDownstreamTrack(DownstreamTrack* value){
	fDownstreamTrack = value;
	SetDownstreamChi2(value->GetChi2());
	SetDownstreamMomentum(value->GetMomentumBeforeMagnet());
	SetDownstreamPosition(value->GetPositionBeforeMagnet());
}

Double_t SpectrometerGigaTrackerMVAMatchingTool::MatchIndividually(Int_t UpstreamTrackIndex, bool VertexFlag, Double_t Z){
	TRecoGigaTrackerCandidate* cand = static_cast<TRecoGigaTrackerCandidate*>(fUpstreamTracks->GetCandidate(UpstreamTrackIndex));
	if (cand==nullptr && fVerbose){
	  cout<<"[SpectrometerGigaTrackerMVAMatchingTool] Warning: cannot locate upstream candidate with index "<<UpstreamTrackIndex;
	  cout<<". Response set to default value of -999. Aborting."<<endl;
	  return -999.;
	}

	SetTime(cand->GetTime()-fReferenceTime);
	SetUpstreamChi2(cand->GetChi2());
	SetUpstreamMomentum(cand->GetMomentum());
	TVector3 GTK3Position(cand->GetPosition(2).X(),cand->GetPosition(2).Y(),102400.);
	//run TwoLinesCDA to get vertex
	TwoLinesCDA *cdacomp = new TwoLinesCDA();
	//downstream track, then upstream track
	cdacomp->SetLine1PointDir(fDownstreamPosition, fDownstreamMomentum);
	cdacomp->SetLine2PointDir(GTK3Position, fUpstreamMomentum);
	cdacomp->ComputeVertexCDA();
	SetVertex(cdacomp->GetVertex());
	SetCDA(cdacomp->GetCDA());
	Double_t VertexZ = cdacomp->GetVertex().Z();
	//the user can override the CDA vertex by calling MatchIndividually with VertexFlag = 1
	if(VertexFlag) VertexZ = Z;

	//propagate the upstream track to the plane of the vertex
	BlueTubeTracker::GetInstance()->SetCharge(+1);
	BlueTubeTracker::GetInstance()->SetInitialPosition(GTK3Position);
	BlueTubeTracker::GetInstance()->SetInitialMomentum(fUpstreamMomentum);
	BlueTubeTracker::GetInstance()->SetZFinal(VertexZ);
	BlueTubeTracker::GetInstance()->TrackParticle();
	SetUpstreamMomentum(BlueTubeTracker::GetInstance()->GetFinalMomentum());
	SetUpstreamX(BlueTubeTracker::GetInstance()->GetFinalPosition().X());
	SetUpstreamY(BlueTubeTracker::GetInstance()->GetFinalPosition().Y());

	//propagate the downstream track to the plane of the vertex
	BlueTubeTracker::GetInstance()->SetCharge(fDownstreamTrack->GetCharge());
	BlueTubeTracker::GetInstance()->SetInitialPosition(fDownstreamPosition);
	BlueTubeTracker::GetInstance()->SetInitialMomentum(fDownstreamMomentum);
	BlueTubeTracker::GetInstance()->SetZFinal(VertexZ);
	BlueTubeTracker::GetInstance()->TrackParticle();
	SetDownstreamMomentum(BlueTubeTracker::GetInstance()->GetFinalMomentum());
	SetDownstreamX(BlueTubeTracker::GetInstance()->GetFinalPosition().X());
	SetDownstreamY(BlueTubeTracker::GetInstance()->GetFinalPosition().Y());
	return reader->EvaluateMVA("MLP method");
}

void SpectrometerGigaTrackerMVAMatchingTool::PerformMatching(){
	Int_t NCandidates = fUpstreamTracks->GetNCandidates();
	if (!NCandidates && fVerbose) cout<<"[SpectrometerGigaTrackerMVAMatchingTool] Warning: no tracks found upstream."<<endl;
	Double_t MaxResponse = -999.;
	for(int i = 0; i < NCandidates; i++){
		Double_t Response = MatchIndividually(i);
		if(Response > MaxResponse){
			MaxResponse = Response; 
			fMatchIndex = i;
			fMatchVertex = fVertex;
			fMatchUpstreamMomentum = fUpstreamMomentum;
			fMatchDownstreamMomentum = fDownstreamMomentum;
			fMatchCDA = fCDA;
			fMatchTime = fTime;
		}
	}
	fResponse = MaxResponse;
	if(fResponse < fResponseCut) fMatchIndex = -1;
}
	
Double_t SpectrometerGigaTrackerMVAMatchingTool::MatchIndividually(Float_t UpstreamX, Float_t UpstreamY,
	 Float_t DownstreamX, Float_t DownstreamY, Float_t UpstreamChi2, Float_t DownstreamChi2,
	 Float_t NGTKHits, Float_t Time){
	
	fUpstreamChi2 = UpstreamChi2;
	fDownstreamChi2 = DownstreamChi2;
	fUpstreamX = UpstreamX;
	fUpstreamY = UpstreamY;
	fDownstreamX = DownstreamX;
	fDownstreamY = DownstreamY;
	fTime = Time;
	fNGTKHits = NGTKHits;
	
	return reader->EvaluateMVA("MLP method");
}
	
	
	

	
	
	
	
	
	
	
	
	
	
	
