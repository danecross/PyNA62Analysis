// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-09
//
// ---------------------------------------------------------------

/// \class SpectrometerVertexBuilder
/// \Brief
/// Builds spectrometer N-track vertices (calling the VertexLSF algorithm)
/// with "good" chi2 and within a "good" Z position range.
/// \EndBrief
/// \Detailed
/// The vertices are built using Spectrometer candidates; DownstreamTracks are not used.
/// Default cuts defining the "good" chi2 and Z ranges: chi2<100 and 50m < z < 180m).
/// The output is contained in a vector of SpectrometerTrackVertex objects.
/// Only spectrometer tracks with fit chi2<100 are considered for vertex building.
/// Track timing is not checked.
/// Only three-track vertices are built by default; however 2 to 5-track vertices can be built.
/// An example of use (a loop over all vertices):
/// \code
/// std::vector<SpectrometerTrackVertex> Vertices =
///   *(std::vector<SpectrometerTrackVertex>*)GetOutput("SpectrometerVertexBuilder.Output");
/// for (UInt_t i=0; i<Vertices.size(); i++) {
///   Vertices[i].Print();
///   Int_t NTracks = Vertices[i].GetNTracks(); // this can vary among the vertices and must be checked
///   // ...
/// }
/// \endcode
/// The number of tracks in a vertex and the pre-selection criteria can be specified
/// via the ReconfigureAnalyzer mechanism (see an example in the K3piSelection class) or
/// from a command line. For example of command line operation:
/// \code
/// ./MyApp -l <list> -p "SpectrometerVertexBuilder:Build2TrackVtx=1;Build3TrackVtx=0;MaxChi2=500" ...
/// \endcode
/// This will switch from 3-track (standard) to 2-track vertices and loosen the upper chi2
/// cut in the vertex pre-selection.<br>
/// Important: when using the vertices, always check the number of tracks in a vertex and discard
/// the vertices with numbers of tracks not of interest for your analysis (see the example above).
/// \todo Add straw timing consistency for the tracks when building vertices?
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include "SpectrometerVertexBuilder.hh"
#include "Event.hh"
#include "TRecoSpectrometerEvent.hh"
//#include "BlueTubeTracker.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

SpectrometerVertexBuilder::SpectrometerVertexBuilder(Core::BaseAnalysis *ba) :
  Analyzer(ba, "SpectrometerVertexBuilder") {
  if (!GetIsTree()) return;

  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");

  fMaxNTracks   = 50; // if more tracks in event, give up, no vertices are built
  fMaxNVertices = 50; // maximum number of vertices to build, give up afterwards

  // Which types of vertices to build?
  // All types are off by default (in this case, 3-track vertices only are built).
  // User analyzers can ask for other numbers of tracks via the ReconfigureAnalyzer feature.
  // These parameters are also controllable from the command line but this is not recommended.

  AddParam("Build2TrackVtx", &fBuild2TrackVertices, false);
  AddParam("Build3TrackVtx", &fBuild3TrackVertices, false);
  AddParam("Build4TrackVtx", &fBuild4TrackVertices, false);
  AddParam("Build5TrackVtx", &fBuild5TrackVertices, false);

  // Other parameters controllable from command line
  AddParam("BlueFieldCorrection", &fBlueFieldCorrection, true);
  AddParam("MaxChi2",      &fMaxChi2,       100.0);
  AddParam("MinZvertex",   &fMinZVertex,  50000.0); // [mm]
  AddParam("MaxZvertex",   &fMaxZVertex, 180000.0); // [mm]
  AddParam("VertexCharge", &fCharge,         -999); // selection of vertex charge
}

SpectrometerVertexBuilder::~SpectrometerVertexBuilder() {
  if (!GetIsTree()) return;
}

void SpectrometerVertexBuilder::InitHist() {
  if (!GetIsTree()) return;
  BookHisto("hNtracks", new TH1F
	    ("Ntracks", "Number of tracks;Number of tracks", 20, -0.5, 19.5));
  BookHisto("hVertexChi2", new TH1F
	    ("VertexChi2", "Vertex #chi^{2};#chi^{2}", 100, 0.0, fMaxChi2));
  BookHisto("hZVertex", new TH1F
	    ("ZVertex", "Vertex Z coordinate;Z [m]",
	     100, 1e-3*fMinZVertex, 1e-3*fMaxZVertex)); // [m]
}

void SpectrometerVertexBuilder::InitOutput() {
  RegisterOutput("Output", &fContainer);
}

void SpectrometerVertexBuilder::Process(Int_t) {
  if (!GetIsTree()) return;
  SetOutputState("Output", kOValid);
  fContainer.clear();

  TRecoSpectrometerEvent* STRAWevent = GetEvent<TRecoSpectrometerEvent>();
  if (!STRAWevent) return;

  FillHisto("hNtracks", STRAWevent->GetNCandidates());
  if ((UInt_t)STRAWevent->GetNCandidates()>fMaxNTracks) return; // too many tracks, give up
  if ((UInt_t)STRAWevent->GetNCandidates()<2)           return; // too few tracks

  // Build three-track vertices only if there are no special user requests.
  // Had to put this in Process() as the ReconfigureAnalyzer feature triggers late.

  if (!fBuild2TrackVertices && !fBuild3TrackVertices &&
      !fBuild4TrackVertices && !fBuild5TrackVertices)
    fBuild3TrackVertices = true;

  // Two-track vertices
  Int_t ind[5];
  if (fBuild2TrackVertices) {
    for (ind[0]=0; ind[0]<STRAWevent->GetNCandidates(); ind[0]++) {
      for (ind[1]=ind[0]+1; ind[1]<STRAWevent->GetNCandidates(); ind[1]++) {
	BuildVertex(ind, 2);
      }
    }
  }

  // Three-track vertices
  if ((UInt_t)STRAWevent->GetNCandidates()<3) return;
  if (fBuild3TrackVertices) {
    for (ind[0]=0; ind[0]<STRAWevent->GetNCandidates(); ind[0]++) {
      for (ind[1]=ind[0]+1; ind[1]<STRAWevent->GetNCandidates(); ind[1]++) {
	for (ind[2]=ind[1]+1; ind[2]<STRAWevent->GetNCandidates(); ind[2]++) {
	  BuildVertex(ind, 3);
	}
      }
    }
  }

  // Four-track vertices
  if ((UInt_t)STRAWevent->GetNCandidates()<4) return;
  if (fBuild4TrackVertices) {
    for (ind[0]=0; ind[0]<STRAWevent->GetNCandidates(); ind[0]++) {
      for (ind[1]=ind[0]+1; ind[1]<STRAWevent->GetNCandidates(); ind[1]++) {
	for (ind[2]=ind[1]+1; ind[2]<STRAWevent->GetNCandidates(); ind[2]++) {
	  for (ind[3]=ind[2]+1; ind[3]<STRAWevent->GetNCandidates(); ind[3]++) {
	    BuildVertex(ind, 4);
	  }
	}
      }
    }
  }

  // Five-track vertices
  if ((UInt_t)STRAWevent->GetNCandidates()<5) return;
  if (fBuild5TrackVertices) {
    for (ind[0]=0; ind[0]<STRAWevent->GetNCandidates(); ind[0]++) {
      for (ind[1]=ind[0]+1; ind[1]<STRAWevent->GetNCandidates(); ind[1]++) {
	for (ind[2]=ind[1]+1; ind[2]<STRAWevent->GetNCandidates(); ind[2]++) {
	  for (ind[3]=ind[2]+1; ind[3]<STRAWevent->GetNCandidates(); ind[3]++) {
	    for (ind[4]=ind[3]+1; ind[4]<STRAWevent->GetNCandidates(); ind[4]++) {
	      BuildVertex(ind, 5);
	    }
	  }
	}
      }
    }
  }
}

void SpectrometerVertexBuilder::BuildVertex(Int_t ind[], Int_t NTracks) {
  if (fContainer.size()==fMaxNVertices) return;

  TRecoSpectrometerEvent* event = GetEvent<TRecoSpectrometerEvent>();
  fVertexLSF.Reset();

  Int_t Charge = 0;
  for (Int_t iTrack=0; iTrack<NTracks; iTrack++) {
    TRecoSpectrometerCandidate *cand =
      static_cast<TRecoSpectrometerCandidate*>(event->GetCandidate(ind[iTrack]));
    if (cand->GetChi2()>100.0) return; // Vertex is not built if at least one track has poor chi2
    if (cand->GetMomentum()<1.0) return; // Protection against corrupted tracks
    Charge += cand->GetCharge();
    fVertexLSF.AddTrack(cand);
  }

  // Vertex charge selection
  if (fCharge!=-999 && Charge!=fCharge) return;

  // Vertex fit (with or without blue field correction)
  fVertexLSF.FitVertex(fBlueFieldCorrection);

  // Loose selection of vertices returned by VertexLSF
  if (fVertexLSF.GetChi2()>fMaxChi2) return; // tested for 3-track vertices only
  if (fVertexLSF.GetVertexPosition().z()<fMinZVertex) return;
  if (fVertexLSF.GetVertexPosition().z()>fMaxZVertex) return;

  // Save the vertex into a common structure
  SpectrometerTrackVertex Vertex;
  Vertex.SetNTracks (fVertexLSF.GetNTracks());
  Vertex.SetCharge  (Charge);
  Vertex.SetPosition(fVertexLSF.GetVertexPosition());
  Vertex.SetChi2    (fVertexLSF.GetChi2());

  TVector3 TotalThreeMomentum  = TVector3(0.0, 0.0, 0.0); // Total momentum after vertex fit
  TVector3 TotalThreeMomentum0 = TVector3(0.0, 0.0, 0.0); // Total momentum before vertex fit
  Double_t Time = 0.0;
  for (Int_t iTrack=0; iTrack<fVertexLSF.GetNTracks(); iTrack++) {
    TRecoSpectrometerCandidate *Scand =
      static_cast<TRecoSpectrometerCandidate*>(event->GetCandidate(ind[iTrack]));
    TVector3 Momentum = fVertexLSF.GetTrackThreeMomentum(iTrack); // momentum after vertex fit

    // An "ad hoc" blue tube correction can be applied in case FitStraightTracksNoBlueBueld() is used
    /*
    TVector3 InitPos = TVector3
      (Scand->xAtBeforeMagnet(183311.0), Scand->yAtBeforeMagnet(183311.0), 183311.0);
    Double_t FinalZ = fVertexLSF.GetVertexPosition().z();
    if (FinalZ>183311.0) FinalZ = 183311.0; // BlueFieldTracker upper z limit
    BlueTubeTracker::GetInstance()->SetCharge(Scand->GetCharge());
    BlueTubeTracker::GetInstance()->SetInitialPosition(InitPos);
    BlueTubeTracker::GetInstance()->SetInitialMomentum(Momentum);
    BlueTubeTracker::GetInstance()->SetZFinal(FinalZ);
    BlueTubeTracker::GetInstance()->TrackParticle();
    Momentum = fTracker->GetFinalMomentum();
    */

    Vertex.AddTrack(Scand, ind[iTrack], Scand->GetCharge(), Momentum, Scand->GetThreeMomentumBeforeMagnet());
    TotalThreeMomentum  += Momentum; // after vertex fit
    TotalThreeMomentum0 += Scand->GetThreeMomentumBeforeMagnet(); // before vertex fit
    Time += Scand->GetLeadingTime(); // 1.2ns resolution, as opposed to 5ns for GetTime()
  }
  Vertex.SetTotalThreeMomentum(TotalThreeMomentum);   // after vertex fit
  Vertex.SetTotalThreeMomentum0(TotalThreeMomentum0); // before vertex fit
  Vertex.SetTime(Time/(1.0*fVertexLSF.GetNTracks()));
  fContainer.push_back(Vertex);

  // Monitoring of the vertices in the output
  FillHisto("hVertexChi2", fVertexLSF.GetChi2());
  FillHisto("hZVertex", 1.0e-3*fVertexLSF.GetVertexPosition().z()); // [m]
}

void SpectrometerVertexBuilder::EndOfJobUser() {
  if (!GetIsTree()) return;
  SaveAllPlots();
}
