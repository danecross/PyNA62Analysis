// -------------------------------------------------------------------
// History:
//
// Created by Joel Swallow (joelchristopherswallow@cern.ch) 2017-06-09
// (Based on code writted by Evgueni Goudzovski)
// modified by Joel Swallow 2017-07-06
// Modified by Joel Swallow 14/09/17
// -------------------------------------------------------------------
#include <iostream>
#include "SpectrometerGigaTrackerMatchingOutput.hh"

using namespace std;

/// \class SpectrometerGigaTrackerMatchingOutput
/// \Brief
/// Results of Spectrometer to GTK track matching: one record per DownstreamTrack.
/// \EndBrief

SpectrometerGigaTrackerMatchingOutput::SpectrometerGigaTrackerMatchingOutput() {
  Clear();
}

SpectrometerGigaTrackerMatchingOutput::SpectrometerGigaTrackerMatchingOutput (Int_t TrackID) {
  Clear();
  fTrackID = TrackID;
}

void SpectrometerGigaTrackerMatchingOutput::Clear() {
  fTrackID = -1;
  fBestIndex = -1;
  fBestDiscriminant = 100000.0; //For MD matching procedure the best matched candidate has the LOWEST discriminant value.
  fGTKCandidateIndex.clear();
  fMatchMade.clear();
  fVertex.clear();
  fBeamParticleMomentum.clear();
  fCorrectedBeamParticleMomentum.clear();
  fTrackMomentum.clear();
  fCorrectedTrackMomentum.clear();
  fCDA.clear();
  fDiscriminant.clear();
}

void SpectrometerGigaTrackerMatchingOutput::AddRecord
(Int_t GTKCandidateIndex, Bool_t CandidateIsGood, TVector3 Vertex, TVector3 BeamParticleMomentum,
 TVector3 CorrectedBeamParticleMomentum, TVector3 TrackMomentum, TVector3 CorrectedTrackMomentum,
 Double_t CDA, Double_t Discriminant) {
  fGTKCandidateIndex.push_back(GTKCandidateIndex); 
  fMatchMade.push_back(CandidateIsGood);	
  fVertex.push_back(Vertex);
  fBeamParticleMomentum.push_back(BeamParticleMomentum);
  fCorrectedBeamParticleMomentum.push_back(CorrectedBeamParticleMomentum);
  fTrackMomentum.push_back(TrackMomentum);
  fCorrectedTrackMomentum.push_back(CorrectedTrackMomentum);
  fCDA.push_back(CDA);
  fDiscriminant.push_back(Discriminant);
}

void SpectrometerGigaTrackerMatchingOutput::SetBestMatch(Int_t i, Double_t Disc){ //set outputs for best match
  SetBestIndex(i);
  SetBestDiscriminant(Disc);
}

void SpectrometerGigaTrackerMatchingOutput::Print(Int_t i) { //print results for ith GTK candidate
//Print results

  cout<<"DownstreamTrack index = "<<fTrackID<<endl;
  cout<<"GTK Candidate Index = "<<fGTKCandidateIndex[i]<<endl;
  cout<<"Match Made = "<<fMatchMade[i]<<endl;
  cout<<"Vertex : x ="<<fVertex[i].X()<<" , y = "<<fVertex[i].Y()<<" , z = "<<fVertex[i].Z()<<endl;
  cout<<"BeamParticleMomentum : p_x = "<<fBeamParticleMomentum[i].X()<<" , p_y = "<<fBeamParticleMomentum[i].Y()<<" , p_z = "<<fBeamParticleMomentum[i].Z()<<endl; 
  cout<<"CorrectedBeamParticleMomentum : p_x = "<<fCorrectedBeamParticleMomentum[i].X()<<" , p_y = "<<fCorrectedBeamParticleMomentum[i].Y()<<" , p_z = "<<fCorrectedBeamParticleMomentum[i].Z()<<endl;
  cout<<"TrackMomentum : p_x = "<<fTrackMomentum[i].X()<<" , p_y = "<<fTrackMomentum[i].Y()<<" , p_z = "<<fTrackMomentum[i].Z()<<endl;
  cout<<"CorrectedTrackMomentum : p_x = "<<fCorrectedTrackMomentum[i].X()<<" , p_y = "<<fCorrectedTrackMomentum[i].Y()<<" , p_z = "<<fCorrectedTrackMomentum[i].Z()<<endl;
  cout<<"CDA = "<<fCDA[i]<<endl;
  cout<<"Discriminant = "<<fDiscriminant[i]<<endl;

}
