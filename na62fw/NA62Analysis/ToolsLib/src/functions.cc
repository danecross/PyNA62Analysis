#include <iostream>
#include <TVector3.h>
#include <signal.h>
#include <sstream>
#include "functions.hh"
#include "BlueTubeTracker.hh"
#include "TwoLinesCDA.hh"
#include "GeometricAcceptance.hh"
#include "VertexLSF.hh"

/// \file functions.cc
/// DISCLAIMER: This file needs reviewing. Most of the functions in this file are here by legacy
/// from early development stages of the framework. None of them should be considered validated
/// at this point.

TVector3 propagate(TVector3 pos, TVector3 p, double z){
  /// \MemberDescr
  /// \param pos : position
  /// \param p : momentum
  /// \param z : projection position
  ///
  /// Propagate the particle to z position
  /// \return TVector3 representing the propagated position
  /// \EndMemberDescr

  TVector3 result;
  result = pos + p*((z-pos.Z())/p.Z());
  return result;
}

TString printVector2( TVector2 v )
{
  /// \MemberDescr
  /// \param v : Vector to format
  ///
  /// Output correct format for printing TVector2 coordinates
  /// \return Formatted string representation of TVector2
  /// \EndMemberDescr

  std::ostringstream ss;
  ss << "( " << v.X() << ", " << v.Y() << " )";
  return TString( ss.str().c_str() );
}

TString printVector3( TVector3 v )
{
  /// \MemberDescr
  /// \param v : Vector to format
  ///
  /// Output correct format for printing TVector3 coordinates
  /// \return Formatted string representation of TVector3
  /// \EndMemberDescr

  std::ostringstream ss;
  ss << "( " << v.X() << ", " << v.Y() << ", " << v.Z() << " )";
  return TString( ss.str().c_str() );
}

TString printVector4( TLorentzVector v )
{
  /// \MemberDescr
  /// \param v : Vector to format
  ///
  /// Output correct format for printing TLorentzVector coordinates
  /// \return Formatted string representation of TLorentzVector
  /// \EndMemberDescr

  std::ostringstream ss;
  ss << "( " << v.T() << ", " << v.X() << ", " << v.Y() << ", " << v.Z() << " )";
  return TString( ss.str().c_str() );
}

double distance3D(TVector3 p1, TVector3 p2){
  /// \MemberDescr
  /// \param p1 : point 1
  /// \param p2 : point 2
  ///
  /// \return Distance between two points in 3D space
  /// \EndMemberDescr

  return sqrt(pow(p1.X()-p2.X(), 2) + pow(p1.Y()-p2.Y(), 2) + pow(p1.Z()-p2.Z(), 2));
}

double distance2D(TVector3 p1, TVector3 p2, TString plane){
  /// \MemberDescr
  /// \param p1 : point 1
  /// \param p2 : point 2
  /// \param plane : plane on which the distance is computed
  ///
  /// \return Distance between two points in 2D space
  /// \EndMemberDescr

  plane.ToUpper();
  if( (plane.CompareTo("XY")==0) || (plane.CompareTo("YX")==0) ) return sqrt(pow(p1.X()-p2.X(), 2) + pow(p1.Y()-p2.Y(), 2));
  else if( (plane.CompareTo("XZ")==0) || (plane.CompareTo("ZX")==0) ) return sqrt(pow(p1.X()-p2.X(), 2) + pow(p1.Z()-p2.Z(), 2));
  else if( (plane.CompareTo("YZ")==0) || (plane.CompareTo("ZY")==0) ) return sqrt(pow(p1.Y()-p2.Y(), 2) + pow(p1.Z()-p2.Z(), 2));
  else return -1;
}

void ApplyBlueTube(int charge, TVector3 oldPos, TVector3 oldMom, double finalZ, TVector3 *newPos, TVector3 *newMom){
  BlueTubeTracker *tracker = BlueTubeTracker::GetInstance();
  tracker->SetCharge(charge);
  tracker->SetInitialPosition(oldPos);
  tracker->SetInitialMomentum(oldMom);
  tracker->SetZFinal(finalZ);
  tracker->TrackParticle();
  TVector3 mom = tracker->GetFinalMomentum();
  TVector3 pos = tracker->GetFinalPosition();
  newPos->SetXYZ(pos.X(), pos.Y(), pos.Z());
  newMom->SetXYZ(mom.X(), mom.Y(), mom.Z());
}

TVector3 GetPositionAtZ(TVector3 mom, TVector3 oldPos, double newZ){
  double MNP33kick = 270.;
  if(((newZ<=196995.)&&(oldPos.Z()<=196995.)) || ((newZ>=196995.)&&(oldPos.Z()>=196995.))){
    double slopeX = (mom.X())/(mom.Z());
    double slopeY = (mom.Y())/(mom.Z());
    double xAtZ = oldPos.X() - slopeX*(oldPos.Z() - newZ);
    double yAtZ = oldPos.Y() - slopeY*(oldPos.Z() - newZ);
    TVector3 newPos(xAtZ, yAtZ, newZ);
    return newPos;
  }else if((newZ>196995.)&&(oldPos.Z()<196995.)){
    double slopeX = (mom.X())/(mom.Z());
    double slopeY = (mom.Y())/(mom.Z());
    double xAtZ = oldPos.X() - slopeX*(oldPos.Z() - 196995.);
    double yAtZ = oldPos.Y() - slopeY*(oldPos.Z() - 196995.);
    TVector3 posAtMagnet(xAtZ, yAtZ, 196995.);
    TVector3 newMom = MomAfterKick(mom, MNP33kick);
    double slopeXa = (newMom.X())/(newMom.Z());
    double slopeYa = (newMom.Y())/(newMom.Z());
    double xAtZa = posAtMagnet.X() - slopeXa*(196995. - newZ);
    double yAtZa = posAtMagnet.Y() - slopeYa*(196995. - newZ);
    TVector3 newPos(xAtZa, yAtZa, newZ);
    return newPos;
  }else if((newZ<196995.)&&(oldPos.Z()>196995.)){
    double slopeX = (mom.X())/(mom.Z());
    double slopeY = (mom.Y())/(mom.Z());
    double xAtZ = oldPos.X() - slopeX*(oldPos.Z() - 196995.);
    double yAtZ = oldPos.Y() - slopeY*(oldPos.Z() - 196995.);
    TVector3 posAtMagnet(xAtZ, yAtZ, 196995.);
    TVector3 newMom = MomAfterKick(mom, -MNP33kick);
    double slopeXa = (newMom.X())/(newMom.Z());
    double slopeYa = (newMom.Y())/(newMom.Z());
    double xAtZa = posAtMagnet.X() - slopeXa*(196995. - newZ);
    double yAtZa = posAtMagnet.Y() - slopeYa*(196995. - newZ);
    TVector3 newPos(xAtZa, yAtZa, newZ);
    return newPos;
  }else{
    std::cout<<"cannot calculate the position"<<std::endl;
    return oldPos;
  };
}

TVector3 MomAfterKick(TVector3 oldMom, double kick){
  TVector3 mom = oldMom;
  TVector2 pTvec(mom.X(), mom.Z());
  double pT = pTvec.Mod();
  double pxNew = mom.X() - kick;
  double beta = acos(mom.X()/pT);
  double theta = acos(pxNew/pT);
  double alpha = theta - beta;
  mom.RotateY(-alpha);
  TVector3 newMom = mom;

  return newMom;
}

TVector3 GetVertexCDA(TVector3 trackMom, TVector3 trackPos, TVector3 kaonMom, TVector3 kaonPos){
  TwoLinesCDA twoLinesCDA;
  twoLinesCDA.SetLine1PointDir(trackPos, trackMom);
  twoLinesCDA.SetLine2PointDir(kaonPos, kaonMom);
  twoLinesCDA.ComputeVertexCDA();
  TVector3 vertex = twoLinesCDA.GetVertex();
  return vertex;
}

TVector3 GetSimpleVertex(double trackCharge, TVector3 trackMom, TVector3 trackPos, double kaonCharge, TVector3 kaonMom, TVector3 kaonPos, TVector3 *newKaonMom, TVector3 *newKaonPos, TVector3 *newTrackMom, TVector3 *newTrackPos){
  double GTK3Z = GeometricAcceptance::GetInstance()->GetZGTK3();
  double STRAW0Z_front = 183311.;
  TVector3 nul(0., 0., 0.);
  TVector3 simpleVert = GetVertexCDA(trackMom, trackPos, kaonMom, kaonPos);
  if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)) return nul;
  newKaonMom->SetXYZ(kaonMom.X(), kaonMom.Y(), kaonMom.Z());
  newKaonPos->SetXYZ(kaonPos.X(), kaonPos.Y(), kaonPos.Z());
  newTrackMom->SetXYZ(trackMom.X(), trackMom.Y(), trackMom.Z());
  newTrackPos->SetXYZ(trackPos.X(), trackPos.Y(), trackPos.Z());
  ApplyBlueTube(kaonCharge, kaonPos, kaonMom, simpleVert.Z(), newKaonPos, newKaonMom);
  ApplyBlueTube(trackCharge, trackPos, trackMom, simpleVert.Z(), newTrackPos, newTrackMom);

  return simpleVert;
}

TVector3 GetIterativeVertex(double trackCharge, TVector3 trackMom, TVector3 trackPos, double kaonCharge, TVector3 kaonMom, TVector3 kaonPos, TVector3 *newKaonMom, TVector3 *newKaonPos, TVector3 *newTrackMom, TVector3 *newTrackPos, double dist){

  int count = 0;
  double GTK3Z = GeometricAcceptance::GetInstance()->GetZGTK3();
  double STRAW0Z_front = 183311.;

  TVector3 simpleVert;
  TVector3 nul(0., 0., 0.);
  newKaonMom->SetXYZ(kaonMom.X(), kaonMom.Y(), kaonMom.Z());
  newKaonPos->SetXYZ(kaonPos.X(), kaonPos.Y(), kaonPos.Z());
  newTrackMom->SetXYZ(trackMom.X(), trackMom.Y(), trackMom.Z());
  newTrackPos->SetXYZ(trackPos.X(), trackPos.Y(), trackPos.Z());
  while(fabs((*newKaonPos).Z() - (*newTrackPos).Z())>dist){
    simpleVert = GetVertexCDA(*newTrackMom, *newTrackPos, *newKaonMom, *newKaonPos);
    if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)) return nul;
    double step = ((simpleVert.Z() - (*newKaonPos).Z())/2.);
    ApplyBlueTube(kaonCharge, *newKaonPos, *newKaonMom, step + (*newKaonPos).Z(), newKaonPos, newKaonMom);
    step = ((-simpleVert.Z() + (*newTrackPos).Z())/2.);
    ApplyBlueTube(trackCharge, *newTrackPos, *newTrackMom, (*newTrackPos).Z() - step, newTrackPos, newTrackMom);
    count++;
    if(count>100) break;
  };
  simpleVert = GetVertexCDA(*newTrackMom, *newTrackPos, *newKaonMom, *newKaonPos);
  if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)) return nul;
  ApplyBlueTube(kaonCharge, kaonPos, kaonMom, simpleVert.Z(), newKaonPos, newKaonMom);
  ApplyBlueTube(trackCharge, trackPos, trackMom, simpleVert.Z(), newTrackPos, newTrackMom);
  return simpleVert;
}

TVector3 GetRadoVertex(double trackCharge, TVector3 trackMom, TVector3 trackPos, double kaonCharge, TVector3 kaonMom, TVector3 kaonPos, TVector3 *newKaonMom, TVector3 *newKaonPos, TVector3 *newTrackMom, TVector3 *newTrackPos){
  double GTK3Z = GeometricAcceptance::GetInstance()->GetZGTK3();
  double STRAW0Z_front = 183311.;
  TVector3 simpleVert;
  TVector3 somePos;
  TVector3 nul(0., 0., 0.);
  newKaonMom->SetXYZ(kaonMom.X(), kaonMom.Y(), kaonMom.Z());
  newKaonPos->SetXYZ(kaonPos.X(), kaonPos.Y(), kaonPos.Z());
  newTrackMom->SetXYZ(trackMom.X(), trackMom.Y(), trackMom.Z());
  newTrackPos->SetXYZ(trackPos.X(), trackPos.Y(), trackPos.Z());

  simpleVert = GetVertexCDA(trackMom, trackPos, kaonMom, kaonPos);
  if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)) return nul;
  ApplyBlueTube(kaonCharge, kaonPos, kaonMom, simpleVert.Z(), &somePos, newKaonMom);
  ApplyBlueTube(trackCharge, trackPos, trackMom, simpleVert.Z(), &somePos, newTrackMom);
  simpleVert = GetVertexCDA(*newTrackMom, trackPos, *newKaonMom, kaonPos);
  if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)) return nul;
  ApplyBlueTube(kaonCharge, kaonPos, kaonMom, simpleVert.Z(), newKaonPos, newKaonMom);
  ApplyBlueTube(trackCharge, trackPos, trackMom, simpleVert.Z(), newTrackPos, newTrackMom);

  return simpleVert;
}

TVector3 GetLSFVertex(double trackCharge, TRecoSpectrometerCandidate *STRAWCand, double kaonCharge, TRecoGigaTrackerCandidate *GTKCand, TVector3 *newKaonMom, TVector3 *newKaonPos, TVector3 *newTrackMom, TVector3 *newTrackPos, double &chi2){
  double GTK3Z = GeometricAcceptance::GetInstance()->GetZGTK3();
  double STRAW0Z_front = 183311.;
  VertexLSF vertexLSF;
  vertexLSF.Reset();

  vertexLSF.AddTrack(STRAWCand);
  vertexLSF.AddGTKTrack(GTKCand);

  TVector3 nul(0., 0., 0.);
  bool goodvertex = vertexLSF.FitVertex(true);
  if(!goodvertex) return nul;

  TVector3 kaonMom = GTKCand->GetMomentum();
  TVector3 kaonPos = GTKCand->GetPosition(2);
  TVector3 trackMom = STRAWCand->GetThreeMomentumBeforeMagnet();
  TVector3 trackPos = STRAWCand->GetPositionBeforeMagnet();
  newKaonMom->SetXYZ(kaonMom.X(), kaonMom.Y(), kaonMom.Z());
  newKaonPos->SetXYZ(kaonPos.X(), kaonPos.Y(), kaonPos.Z());
  newTrackMom->SetXYZ(trackMom.X(), trackMom.Y(), trackMom.Z());
  newTrackPos->SetXYZ(trackPos.X(), trackPos.Y(), trackPos.Z());

  TVector3 simpleVert = vertexLSF.GetVertexPosition();
  if((simpleVert.Z()<GTK3Z) || (simpleVert.Z()>STRAW0Z_front)) return nul;
  chi2 = vertexLSF.GetChi2();
  ApplyBlueTube(kaonCharge, kaonPos, kaonMom, simpleVert.Z(), newKaonPos, newKaonMom);
  ApplyBlueTube(trackCharge, trackPos, trackMom, simpleVert.Z(), newTrackPos, newTrackMom);

  return simpleVert;
}

double GetCDA(TRecoSpectrometerCandidate* STRAWCand1, TRecoSpectrometerCandidate* STRAWCand2){
  TwoLinesCDA twoLinesCDA;
  twoLinesCDA = TwoLinesCDA();
  twoLinesCDA.SetLine1PointDir(STRAWCand1->GetPositionBeforeMagnet(), STRAWCand1->GetThreeMomentumBeforeMagnet());
  twoLinesCDA.SetLine2PointDir(STRAWCand2->GetPositionBeforeMagnet(), STRAWCand2->GetThreeMomentumBeforeMagnet());
  twoLinesCDA.ComputeVertexCDA();
  double CDA = twoLinesCDA.GetCDA();
  return CDA;
}

double GetCDA(TVector3 trackMom, TVector3 trackPos, TVector3 kaonMom, TVector3 kaonPos){
  TwoLinesCDA twoLinesCDA;
  twoLinesCDA.SetLine1PointDir(trackPos, trackMom);
  twoLinesCDA.SetLine2PointDir(kaonPos, kaonMom);
  twoLinesCDA.ComputeVertexCDA();
  return twoLinesCDA.GetCDA();
}

int WhichCHODQuadrant(TVector2 pos){
  int qID = -1;
  if(pos.X()>0. && pos.Y()>0.){
    qID = 3;
  }else if(pos.X()>0. && pos.Y()<0.){
    qID = 2;
  }else if(pos.X()<0. && pos.Y()>0.){
    qID = 0;
  }else if(pos.X()<0. && pos.Y()<0.){
    qID = 1;
  };
  return qID;
}
