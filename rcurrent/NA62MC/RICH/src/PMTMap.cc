/*
 * PMTMap.cpp
 *
 *  Created on: 21.07.2010
 *      Author: imp
 */

#include "PMTMap.hh"

#include "RICHGeometryParameters.hh"
#include <iostream>



PMTMap::PMTMap() :
    fNChannels(0),
    fNPhotoDetectors(0),
    fT0(nullptr)
{
	// TODO Auto-generated constructor stub
	
	//this has to be used inside G4
	RICHGeometryParameters* gp = RICHGeometryParameters::GetInstance();
	TVector2* PMsPositions= gp->GetPMsPositions();//This gives us the one spot of PMTs but it's in the center

	//m_RadiusWinstonCone=0.009;
	m_RadiusWinstonCone= gp->GetConeInputRadius()/m;

	// so we have to move it to the left and the right side
	fNPhotoDetectors = gp->GetNPMs();//this is the amount of PMTs left + right

	
	//so let's double it and move to left , right side
	
	fPMsPositions = new TVector2[fNPhotoDetectors];

	//G4cout<<" RICH PMT Map for  Fast Simulation "<<G4endl;	

	for(int i=0;i<fNPhotoDetectors/2;i++){
	  fPMsPositions[i].Set(PMsPositions[i].X()+1600,PMsPositions[i].Y());
	  fPMsPositions[1237+i].Set(PMsPositions[i].X()-1600,PMsPositions[i].Y());
	  //G4cout<<" ipm X  Y   "<< fPMsPositions[i].X() << "\t" << fPMsPositions[i].Y() <<G4endl;
	}

	
}

PMTMap::~PMTMap() {
	// TODO Auto-generated destructor stub
}

bool PMTMap::checkAcceptance(double Px, double Py){
	m_PMTPosition=TVector2(0,0);
	bool isAccepted = false;
	for(Int_t i = 0; i < fNPhotoDetectors; i++){
	       // if(i==207 && Side < 1000) continue; // ToDo for what is this???
	        Double_t x= fPMsPositions[i].X()/1000.;//we need this in Meter
	        Double_t y= fPMsPositions[i].Y()/1000.;
	        //std::cout<<x<<"\t"<<y<<std::endl;

	        if(isInsideCircle(m_RadiusWinstonCone,x,y,Px,Py)){
	        	m_PMTPosition=TVector2(x,y);
	        	isAccepted=true;
	        	//std::cout<<"Is accepted = true"<<std::endl;
	        	break;
	        }
	   }
	if(isAccepted){
		return true;
	}
	return false;
}

bool PMTMap::isInsideCircle(double CircleR, double CircleCenterX, double CircleCenterY, double PointX, double PointY){
	double x=PointX-CircleCenterX;
	double y=PointY-CircleCenterY;

	if(std::abs(y)<CircleR && std::abs(x)<std::sqrt(std::pow(CircleR,2)-std::pow(y,2))){
		return true;
	}
	else{
		return false;
	}

}

void PMTMap::setRadiusWinstonCone(double R){
	m_RadiusWinstonCone=R;
}

TVector2 PMTMap::getHitPMTPos(){

	return m_PMTPosition;
}


//This function is stolen from somewhere
//reads in the PMT pos.
TVector2* PMTMap::GetPMPositions(std::string FileName) {
    int nItems;

    //ifstream confFile(((G4String)std::getenv("NA62MCSOURCE"))+"/config/RICH.conf");
    std::ifstream confFile(FileName.c_str());
    TString Line;
    Int_t NSpots = 1;

    nItems=0;
    while(Line.ReadLine(confFile)) {
        if(Line.BeginsWith("#")) continue; /* comment line */
        if(Line.BeginsWith("NChannels=")) {
            fNChannels = TString(Line(TRegexp("[0-9]+"))).Atoi();
            fT0 = new Float_t[fNChannels];
            fPMsPositions = new TVector2[fNChannels];
            nItems++;
        } else if(Line.BeginsWith("NPhotoDetectors=")) {
            fNPhotoDetectors = TString(Line(TRegexp("-*[0-9]+"))).Atoi();
            nItems++;
        } else if(Line.BeginsWith("SpotCenter=")) {
            TObjArray * l = Line.Tokenize(" ");
            fSpotCenter = TVector2(((TObjString*)(l->At(1)))->GetString().Atof(),((TObjString*)(l->At(2)))->GetString().Atof());
            if(fSpotCenter.Mod()!=0)
                NSpots = 2;
            nItems++;
        } else if(Line.BeginsWith("T0_")){
            for(int i = 0; i < (Int_t)(fNChannels/8); i++) {
                if(Line.BeginsWith(Form("T0_%d= ",i+1))) {
                    TObjArray * l = Line.Tokenize(" ");
                    for(int j = 0; j < 8; j++)
                        fT0[8*i+j] = ((TObjString*)(l->At(j+1)))->GetString().Atof();
                    nItems++;
                }
            }
        } else if(Line.BeginsWith("PMIndexes_")){
            for(int i = 0; i < (Int_t)(fNChannels/8); i++) {
                if(Line.BeginsWith(Form("PMIndexes_%d= ",i+1))) {
                    TObjArray * l = Line.Tokenize(" ");
                    for(int j = 0; j < 8; j++){
                        Int_t n = ((TObjString*)(l->At(2*j+1)))->GetString().Atoi();
                        Int_t m = ((TObjString*)(l->At(2*j+2)))->GetString().Atoi();
                        fPMsPositions[8*i+j] = (8*i+j >= fNPhotoDetectors/NSpots ? -1 : 1) * (TVector2((n-m)*9,(n+m)*9*TMath::Sqrt(3))+fSpotCenter);
                        nItems++;
                    }
                }
            }
        }
    }
    //Double_t SlewChi2=1.0;
    //for(Int_t iCh = 0; iCh < fNChannels; iCh++){
    //    SlewPars[iCh][0] = 13; SlewPars[iCh][1] = 0; SlewPars[iCh][2] = 15; SlewPars[iCh][3] = 0; SlewPars[iCh][4] = 0;
    //}

    //Int_t ch=0;
    //ifstream SlewParsFile(((G4String)std::getenv("NA62MCSOURCE"))+"/config/RICHSlewPars.txt");
    //if(SlewParsFile){
    //    while(SlewParsFile>>ch){
    //        SlewParsFile>>SlewPars[ch][0]>>SlewPars[ch][1]>>SlewPars[ch][2]>>SlewPars[ch][3]>>SlewPars[ch][4]>>SlewChi2;
    //    }
    //    SlewParsFile.close();
    //}
    return fPMsPositions;
}

//another stolen function
void PMTMap::DrawFlange(Double_t Side){
/*  gStyle->SetOptStat(0);
  TCanvas * c1 = new TCanvas("RR","Ring Reference",600,600);
  c1->SetFillColor(kWhite);
  c1->SetBorderMode(kFALSE);
  c1->SetFrameFillColor(kWhite);
  c1->SetFrameBorderMode(kFALSE);
*/
  //TH2F * null = new TH2F("null", "Mirror Reference Frame", 100, -300., 300.,100, -300., 300.);
  TH2F * null = new TH2F("null", "", 100, -2, 2,100, -2, 2);
  null->SetFillColor(kWhite);
  null->GetXaxis()->SetTitle("x [m]");
  null->GetYaxis()->SetTitle("y [m]");
  null->GetYaxis()->SetTitleOffset(1.3);
  null->Draw();

  for(Int_t i = 0; i < fNPhotoDetectors; i++){
        if(i==207 && Side < 1) continue;
        Double_t x= fPMsPositions[i].X()/1000;
        Double_t y= fPMsPositions[i].Y()/1000;
        TArc * arc = new TArc(x,y,0.009);
        arc->SetLineWidth(1);
        arc->SetFillStyle(0);
        arc->Draw("l");
        //cout << Form("{% 6.3f,  % 6.3f}, //--->%d",x,y,i) << endl;
	//cout << "{" << setprecision(3) << setiosflags(ios::fixed) << setw(8) << x << ", " << setw(8) << y << "}, //--->" << i << endl;
    }
}
