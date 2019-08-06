#include "StrawsTracksFilterAlgo.hh"

namespace NA62Analysis
{
  //  ==============================================
  StrawsTracksFilterAlgo::StrawsTracksFilterAlgo(BaseAnalysis *ba, Analyzer* ana, const std::string &name):
    Algorithm(ba,ana,name)
  {

    fSpectrometerEvent = new TRecoSpectrometerEvent;
    RequestTree("Spectrometer", fSpectrometerEvent, "Reco");

    AddParam("DeltaP" 	                , &fDeltaP                            ,  20000.	);
    AddParam("Chi2" 	                , &fChi2                              ,  20.	);
    AddParam("Chi2Fake" 	        , &fChi2Fake                          ,  30.	);
    //AddParam("IsoDeltaT" 	                , &fIsoDeltaT                         ,  50.	);
    //AddParam("IsoChi2" 	                , &fIsoChi2                            ,  15.	);

    BookHisto(new TH1F("BadTrackRate", ";Bad Track Fraction;count", 10,0.,1.));
    BookHisto(new TH1F("NbGoodTracks", ";Nb Good Tracks;count", 10,0.,10.));

  }

  //==============================================
  vector<TRecoSpectrometerCandidate*> StrawsTracksFilterAlgo::GetGoodTracks(){

    fSpectrometerEvent = GetEvent<TRecoSpectrometerEvent>();

    //copy tracks in a vector
    vector<TRecoSpectrometerCandidate*> tracks;
    tracks.reserve(fSpectrometerEvent->GetNCandidates());
    for (int i(0);i<fSpectrometerEvent->GetNCandidates();++i)tracks.push_back( static_cast<TRecoSpectrometerCandidate*>(fSpectrometerEvent->GetCandidate(i)));

    //remove fake tracks
    //I believe this could be replaced with
    //tracks.erase(std::remove_if(tracks.begin(), tracks.end(), IsFake), tracks.end());
    auto it = tracks.begin();
    while(it!=tracks.end()){
      if(IsFake(*it)) it = tracks.erase(it);
      else ++it;
    }

    //remove bad tracks
    //I believe this could be replaced with
    //tracks.erase(std::remove_if(tracks.begin(), tracks.end(), [] (TRecoSpectrometerCandidate* c){return !IsGood(c);}), tracks.end());
    it = tracks.begin();
    while(it!=tracks.end()){
      if(!IsGood(*it)) it = tracks.erase(it);
      else ++it;
    }

    return tracks;
  }

  //==============================================
  void StrawsTracksFilterAlgo::FilterGoodTrack(){

    fSpectrometerEvent = GetEvent<TRecoSpectrometerEvent>();

    double nInit = fSpectrometerEvent->GetNCandidates();
    vector<int> toRemove;
    //remove fake tracks
    for (int i(0);i<fSpectrometerEvent->GetNCandidates();){
      if(IsFake( static_cast<TRecoSpectrometerCandidate*>(fSpectrometerEvent->GetCandidate(i)) )) fSpectrometerEvent->RemoveCandidate(i);
      else i++;
    }

    //remove bad tracks
    for (int i(0);i<fSpectrometerEvent->GetNCandidates();){
      if(!IsGood( static_cast<TRecoSpectrometerCandidate*>(fSpectrometerEvent->GetCandidate(i)) )) fSpectrometerEvent->RemoveCandidate(i);
      else i++;
    }
    double nFinal = fSpectrometerEvent->GetNCandidates();
    FillHisto("NbGoodTracks",nFinal);
    if(nInit>0)   FillHisto("BadTrackRate",nFinal/nInit);
    return;
  }

  /*
  //  ==============================================
  vector<TRecoSpectrometerCandidate*> StrawsTracksFilterAlgo::GetIsolatedTracks(TRecoSpectrometerEvent*){
    //copy tracks in a vector
    vector<TRecoSpectrometerCandidate*> tracks;
    tracks.reserve(evt.GetNCandidates());
    for (int i(0);i<evt.GetNCandidates();i++)tracks.push_back(evt.GetCandidate(i));

    //remove fake tracks
    auto it = tracks.begin();
    while(it!=tracks.end()){
      if(IsFake(*it,evt)) erase(it);
      else it++;
    }

    //copy good tracks
    vector<TRecoSpectrometerCandidate*> iTracks;
    it = tracks.begin();
    for (;it!=tracks.end();it++){
      if(IsGood(*it,tracks)) iTracks.push_back(*it);
    }
    return iTracks;
  }
  */

  //==============================================
  bool StrawsTracksFilterAlgo::IsGood(TRecoSpectrometerCandidate* track){
    // forward track, good chi2, 4 chambers, small delta P, not fake, isolated track
    if (track->GetThreeMomentumBeforeMagnet().Z() < 1000)  return 0;
    if (track->GetChi2()>fChi2 ) return 0; //chi
    if (track->GetNChambers()<4) return 0;  //Nchambers
    Double_t quality_momentum = fabs(track->GetMomentumBeforeFit()-track->GetMomentum());
    if (fabs(quality_momentum)>fDeltaP   ) return 0;  //deltaP
    return 1;

  }

  //==============================================
  bool StrawsTracksFilterAlgo::IsFake(TRecoSpectrometerCandidate* track){

    int nCh = track->GetNChambers();
    if(nCh == 4 ) return 0; //4Ch Tracks are good

    double chi2 = track->GetChi2(); // bad chi2 tracks are fake
    if(chi2 > fChi2Fake ) return 1;

    // tracks sharing 2 hits with another track
    int *trackHitIndex = (int *)track->GetHitsIndexes();
    for (int jtrack=0; jtrack<fSpectrometerEvent->GetNCandidates(); jtrack++) {
      TRecoSpectrometerCandidate *track2 = static_cast<TRecoSpectrometerCandidate *>(fSpectrometerEvent->GetCandidate(jtrack));
      if(track==track2) continue;
      Int_t *track2HitIndex = (Int_t *)track2->GetHitsIndexes();
      Int_t nHitCommon = 0;
      for (int jHit1=0; jHit1<track->GetNHits(); jHit1++) {
        for (int jHit2=0; jHit2<track2->GetNHits(); jHit2++) {
          if (trackHitIndex[jHit1]==track2HitIndex[jHit2])  {
	    nHitCommon ++;
	  }
        }
      }
      if(nHitCommon > 1) return 1;
    }
    return 0;
  }

}//~namespace IImaS


