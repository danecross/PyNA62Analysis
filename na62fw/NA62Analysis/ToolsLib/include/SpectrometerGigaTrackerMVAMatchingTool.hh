// ---------------------------------------------------------------------------------
// History:
//
// Created by Daniel Moise (razvan.daniel.moise@cern.ch) 07 june 2017
//
// ---------------------------------------------------------------------------------

#ifndef SPECTROMETERGIGATRACKERMVAMATCHINGTOOL_HH
#define SPECTROMETERGIGATRACKERMVAMATCHINGTOOL_HH

#include <iostream>

#include "TVector3.h"
#include "TMVA/Tools.h"
#include "TMVA/Reader.h"
#include "DownstreamTrack.hh"
#include "TRecoGigaTrackerEvent.hh"
#include "TwoLinesCDA.hh"
#include "BlueTubeTracker.hh"
#include "TSystem.h"

class SpectrometerGigaTrackerMVAMatchingTool {
	
public:
	
  SpectrometerGigaTrackerMVAMatchingTool();
  ~SpectrometerGigaTrackerMVAMatchingTool() {}
	
	void SetWeightsFile(TString value) 				{ fWeightsFile = value; }
	void SetDirectory(TString value) 				{ fDirectory = value; }
	
	void SetUpstreamChi2(Double_t value) 		{ fUpstreamChi2 = (Float_t)value; }
	void SetDownstreamChi2(Double_t value) 	{ fDownstreamChi2 = (Float_t)value; }
	void SetUpstreamX(Double_t value) 			{ fUpstreamX = (Float_t)value; }
	void SetUpstreamY(Double_t value) 			{ fUpstreamY = (Float_t)value; }
	void SetDownstreamX(Double_t value) 		{ fDownstreamX = (Float_t)value; }
	void SetDownstreamY(Double_t value) 		{ fDownstreamY = (Float_t)value; }
	void SetTime(Double_t value)						{ fTime = (Float_t)value; }
	void SetNGTKHits(Double_t value)				{ fNGTKHits = (Float_t)value; }
	
	void SetVertex(TVector3 value)					{ fVertex = value; }
	void SetCDA(Double_t value)						{ fCDA = value; }
	void SetReferenceTime(Double_t value)		{ fReferenceTime = value; }
	
	void SetVerbose(Bool_t value)					{ fVerbose = value; }
	
	void SetUpstreamTracks(TRecoGigaTrackerEvent *value);
	void SetDownstreamTrack(DownstreamTrack* value);
	void SetUpstreamMomentum(TVector3 value)		{ fUpstreamMomentum = value; }
	void SetDownstreamMomentum(TVector3 value)	{ fDownstreamMomentum = value; }
	void SetDownstreamPosition(TVector3 value)		{ fDownstreamPosition = value; }
	
	void SetMatchIndex(Int_t value)					{ fMatchIndex = value; }
	void SetMatchVertex(TVector3 value)			{ fMatchVertex = value; }
	void SetMatchUpstreamMomentum(TVector3 value)			{ fMatchUpstreamMomentum= value; }
	void SetMatchDownstreamMomentum(TVector3 value)		{ fMatchDownstreamMomentum= value; }
	void SetMatchCDA(Double_t value)				{ fMatchCDA = value; }
	void SetMatchTime(Double_t value)				{ fMatchTime = value; }
	void SetResponse(Int_t value)					{ fResponse = value; }
	void SetResponseCut(Int_t value)				{ fResponseCut = value; }
	
	
	TString GetWeightsFile() 	{ return fWeightsFile; }
	TString GetDirectory() 		{ return fDirectory; }
	
	Float_t GetUpstreamChi2() 			{ return fUpstreamChi2; }
	Float_t GetDownstreamChi2()		{ return fDownstreamChi2; }
	Float_t GetUpstreamX() 				{ return fUpstreamX; }
	Float_t GetUpstreamY() 				{ return fUpstreamY; }
	Float_t GetDownstreamX() 			{ return fDownstreamX; }
	Float_t GetDownstreamY() 			{ return fDownstreamY; }
	Float_t GetTime()						{ return fTime; }
	Float_t GetNGTKHits()					{ return fNGTKHits; }
	
	TVector3 GetVertex()					{return fVertex; }
	Double_t GetCDA()					{return fCDA; }
	Double_t GetReferenceTime()		{return fReferenceTime; }

	TRecoGigaTrackerEvent* GetUpstreamTracks()
		{ return fUpstreamTracks; }
	DownstreamTrack* GetDownstreamTrack()
		{ return fDownstreamTrack; }
	TVector3 GetUpstreamMomentum()			{ return fUpstreamMomentum; }
	TVector3 GetDownstreamMomentum()		{ return fDownstreamMomentum; }
	TVector3 GetDownstreamPosition()			{ return fDownstreamPosition; }
	
	//output
	Double_t GetMatchIndex()			{ return fMatchIndex; }
	TVector3 GetMatchVertex()			{ return fMatchVertex; }
	TVector3 GetMatchUpstreamMomentum()			{ return fMatchUpstreamMomentum; }
	TVector3 GetMatchDownstreamMomentum()		{ return fMatchDownstreamMomentum; }
	Double_t GetMatchCDA()			{ return fMatchCDA; }
	Double_t GetMatchTime()			{ return fMatchTime; }
	Double_t GetMatchResponse()		{ return fResponse; }
	Double_t GetResponseCut()		{ return fResponseCut; }

private:
	
	//MVA weights file, reader and input variables
	TMVA::Reader *reader;
	TString fDirectory = TString(gSystem->Getenv("ANALYSISFW_PATH"));
	TString fWeightsFile = "config/SpectrometerGigatrackerMVAMatchingTool_TMVA_weights";
	TString fFooter = ".xml";
	TString fWeights = fDirectory + fWeightsFile + fFooter;

	Float_t fUpstreamChi2;
	Float_t fDownstreamChi2;
	Float_t fUpstreamX;
	Float_t fUpstreamY;
	Float_t fDownstreamX;
	Float_t fDownstreamY;
	Float_t fTime;
	Float_t fNGTKHits;
	TVector3 fVertex;
	Double_t fCDA;
	Double_t fReferenceTime;
	Bool_t fVerbose = 1; //for output

	//Upstream and downstream cadidates
	TRecoGigaTrackerEvent *fUpstreamTracks; //to be potentially replaced with UpstreamTrack vector 
	DownstreamTrack *fDownstreamTrack; //do matching one downstream track at a time
	TVector3 fDownstreamMomentum;
	TVector3 fDownstreamPosition;
	TVector3 fUpstreamMomentum;

	Int_t fMatchIndex = -1;
	TVector3 fMatchVertex;
	TVector3 fMatchUpstreamMomentum;
	TVector3 fMatchDownstreamMomentum;
	Double_t fMatchCDA = -999.;
	Double_t fMatchTime = -999.;
	Double_t fResponse = -999;
	Double_t fResponseCut = .4;

public:

	void PerformMatching();
	Double_t MatchIndividually(Int_t UpstreamTrackIndex, bool VertexFlag = 0, Double_t Z = 0.);
	Double_t MatchIndividually(Float_t UpstreamX = 0., Float_t UpstreamY = 0.,
		   Float_t DownstreamX = 0., Float_t DownstreamY = 0.,
		   Float_t UpstreamChi2 = 0., Float_t DownstreamChi2 = 0.,
		   Float_t NGTKHits = 0., Float_t Time = 0.);	
};
#endif
