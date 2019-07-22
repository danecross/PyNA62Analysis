// --------------------------------------------------------------------
// History:
//
// Createdfor LAV by Domenico Di Filippo (difilippo@na.infn.it) 2010-03-15
//
// Modified for RICH
//
// --------------------------------------------------------------------
#include "RICHOptTrack.hh"
#include "RICHGeometryParameters.hh"
#include "G4PVPlacement.hh"


#include "G4ParticleTable.hh"
#include "G4AffineTransform.hh"
#include "G4NavigationHistory.hh"
#include "TMath.h"

const G4ThreeVector Z = G4ThreeVector(0, 0, 1);

RICHOptTrack::RICHOptTrack(){

  RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();
  G4double MirrorFocalLength = GeoPars->GetMirrorFocalLength()/m;
  G4double MirrorFlangeDistance = GeoPars->GetMirrorFlangeDistance()/m;
  G4double MirrorZPosition = GeoPars->GetMirrorZPosition()/m;
  G4double MirrorWindowZLength = GeoPars->GetMirrorWindowZLength()/m;
  G4double RICHDetectorZPosition = GeoPars->GetRICHDetectorZPosition()/m;  
  G4double RICHDetectorZLength = GeoPars->GetRICHDetectorZLength()/m;
  G4double RICHDetectorRadius = GeoPars->GetRICHDetectorRadius()/m;
  G4double VesselZLength= GeoPars->GetVesselZLength()/m;


  fOpticalPhoton =    G4ParticleTable::GetParticleTable()->FindParticle("opticalphoton");
	 
  fPMTMap = new PMTMap();


	 fPhotonsNumber = 0;
     
      //initialization of the the objects

      //	fRichAcceptance.setMirrorPos(17.01+219.625); // rich beginning + focal length
	 // better: vessel centre + Mirror Z length
	 G4double VesselCentreZ = (0.5*RICHDetectorZLength + RICHDetectorZPosition - MirrorWindowZLength) -0.5*VesselZLength;
	 G4double MirrorZPos = VesselCentreZ + MirrorZPosition;
	 fRichAcceptance.setMirrorPos(MirrorZPos);
	 
	 fRichAcceptance.setRichRadius(RICHDetectorRadius);//2m RICH Radius   was 3m!!!

	//fTwoMirror.setMirrorCenterZ(-17.01+219.625);  // rich beginning - focal length = zmirror - 2 focal length
	fTwoMirror.setMirrorCenterZ(MirrorZPos-2.*MirrorFocalLength);
	
	//	fTwoMirror.setPMTPos(219.625);  // rich beginning
	// better mirror position - mirrorflange distance
	fTwoMirror.setPMTPos(MirrorZPos-MirrorFlangeDistance);

	//		G4cout<< " OptTrack - MirrorZPos  "  << MirrorZPos << " MirrorCenterZ  " <<
	//	MirrorZPos-2.*MirrorFocalLength << " PMTPos  " 	<< MirrorZPos-MirrorFlangeDistance  << G4endl;

	//	G4cout<< " MirrorFocalLength  "  << MirrorFocalLength << " MirrorFlangeDistance  " <<
	//MirrorFlangeDistance  << G4endl;

	//	G4cout<< " VesselCentreZ "  << VesselCentreZ << " RICHDetectorRadius  " <<
	//RICHDetectorRadius  << G4endl;

}

RICHOptTrack::~RICHOptTrack(){
;}

void RICHOptTrack::ProduceOpticalPhoton(G4Step *aStep){

  RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();
  G4double AngleWRTXaxis = GeoPars->GetAngleWRTXaxis();

 
   fPhotonsNumber = 0;


// Get track which generated this hit
  G4Track* ThisTrack = aStep->GetTrack();

// All geometrical and kinematical quantities are taken at the track (equivalent to post step point)

  G4ThreeVector PartMom = ThisTrack->GetMomentum(); //PartMom.x() PartMom.z() PartMom.z() PartMom.mag() in ev
  G4ThreeVector PartPos = ThisTrack->GetPosition(); // we are at the RICH entrance boundary
  G4double PartEne = ThisTrack->GetTotalEnergy();
   
  /* G4cout<<"RICHOptTrack before rotation "<< G4endl;
  G4cout<<"Mom: X "<<PartMom.x()<<" Y "<<PartMom.y()<<" Z "<<PartMom.z()<<" E "<<PartEne<<G4endl;
  G4cout<<"Pos: X "<<PartPos.x()<<" Y "<<PartPos.y()<<" Z "<<PartPos.z()<<G4endl;
  */  

  PartMom.rotateY(-AngleWRTXaxis);

  /* G4cout<<"RICHOptTrack after rotation "<< G4endl; 
  G4cout<<"Mom rot: X "<<PartMom.x()<<" Y "<<PartMom.y()<<" Z "<<PartMom.z()<<" E "<<PartEne<<G4endl;
  G4cout<<"Pos: X "<<PartPos.x()<<" Y "<<PartPos.y()<<" Z "<<PartPos.z()<<G4endl; 
  */ 
  
  
 // G4cout<<"X "<<PartMom.x()<<G4endl;
  
  TVector3 ParticleMomentum(PartMom.x(),PartMom.y(),PartMom.z());
  TVector3 ParticlePos(PartPos.x()/1000.,PartPos.y()/1000.,PartPos.z()/1000.);
  
  //ParticleMomentum = 100000*ParticleMomentum.Unit();
  //G4cout<<"Old E" << PartEne << " Part Momentum " << ParticleMomentum.Mag() << G4endl;  
  //  TVector3 ParticleMomentum(0.,0.,50000);
  //TVector3 ParticlePos(0.5,0.,220.625);
  //PartEne=TMath::Sqrt(ParticleMomentum.Mag()*ParticleMomentum.Mag()+139.570*139.570);
  //G4cout<<"New E" <<PartEne<<G4endl;
  //G4cout<<"Mom: X "<<ParticleMomentum.x()<<" Y "<<ParticleMomentum.y()<<" Z "<<ParticleMomentum.z()<<" E "<<PartEne<<G4endl;
  //G4cout<<"Pos: X "<<PartPos.x()/1000.<<" Y "<<PartPos.y()/1000.<<" Z "<<PartPos.z()/1000.<<G4endl;
  //G4cout<<"Pos: X "<<ParticlePos.x()<<" Y "<<ParticlePos.y()<<" Z "<<ParticlePos.z()<<G4endl;
  
 
  TVector3 Pos;
  
  TVector2 PMT;
  
  std::vector<Photon> PhotVec;

   // Daniel Cherenkov calcuation:
   
  // G4cout<<"My calculations"<<G4endl;
  // G4cout<<"MomX "<<ParticleMomentum.X()<<" Y" <<ParticleMomentum.Y()<<" Z" <<ParticleMomentum.Z()<<G4endl;
 //  G4cout<<"PosX "<<ParticlePos.X()<<" Y" <<ParticlePos.Y()<<" Z" <<ParticlePos.Z()<<G4endl;
 //  G4cout<<"length "<<fRichAcceptance.calculatePathLength(ParticlePos,ParticleMomentum)<<G4endl;
 //  G4cout<<"Energy "<<PartEne<<G4endl;
   
     //clear the vector!!!
  
    fPhotonEnergy.clear();
    fPMTPhotons.clear();
   
   	fCHP.produceCherenkovPhoton(ParticleMomentum,ParticlePos,fRichAcceptance.calculatePathLength(ParticlePos,ParticleMomentum),PartEne);
	PhotVec=fCHP.getPhotonVector();
	//G4cout<<"N Photons produced"<<PhotVec.size()<<G4endl;
	for(unsigned int j=0;j<PhotVec.size();j++){//run over all produced Photons
		//position in observation plane
		
		Pos = fTwoMirror.getPhotonPosAtPMT(PhotVec[j].X(),PhotVec[j].Y(),PhotVec[j].Z(),PhotVec[j].Theta(),PhotVec[j].Phi(),PhotVec[j].getEnergy());


		//MirrorPos = fTwoMirror.getPhotonPosOnMirror(); //this would be the position of the photon on the mirror
	//	if(!(MirrorPos.X()==0 && MirrorPos.Y()==0)){//lost photons are marked with 0 0
	//		//without lost photons.
			//lost photons are marked with 0;0
	//	}
		if(!(Pos.X()==0 && Pos.Y()==0)){//lost photons are marked with 0 0
			//here we have now phontons on the observation plane wich are not lost
			//but without the acceptance of the PMT area
			//here comes the part which checks the PMT stuff/acceptance
			//G4cout<<"Passed"<<G4endl;
			//G4cout<<Pos.X()<<" "<<Pos.Y()<<G4endl;
			
			//fPMTPhotons.push_back(TVector3(Pos.X(),Pos.Y(),Pos.Z()));
			//fPhotonEnergy.push_back(PhotVec[j].getEnergy());
			
			if(fPMTMap->checkAcceptance(Pos.X(),Pos.Y())){
			  //G4cout<<" pos.X  pos.Y  in acceptance   " << Pos.X()<<" "<<Pos.Y()<<G4endl;
				//here we have only photons which are in the acceptance area of the PMTs
				//G4cout<<"Passed PMT"<<G4endl;	
				PMT=fPMTMap->getHitPMTPos();//this returns the position of the PMT which was hit
				//fPMTPhotons.push_back(Pos);
				
				//G4cout<<" PMT.X  PMT.Y  from  map   " <<PMT.X()<<" "<<PMT.Y()<<G4endl;
				//calculate the time the pi travels through the detectur until it emitts the photon 
				// + the time the emitted photon travels through mirror system

				fTravelTime.push_back(PhotVec[j].getTime()+fTwoMirror.getTravelTime());
				fPMTPhotons.push_back(TVector3(PMT.X(),PMT.Y(),Pos.Z()));
	
				fPhotonEnergy.push_back(PhotVec[j].getEnergy());
					
			}
		}
	}
		
        fCHP.clearPhotonVector();
	

   
	
   

        fPhotonsNumber = fPMTPhotons.size();  // will be the result of Daniel calculation
	
	//G4cout<<"fPhotonsNumber "<<fPhotonsNumber<<G4endl;

   // here loop to fill photon objects:
   //.....................
}


void RICHOptTrack::ProcessOpticalPhoton(G4int index){
   
  RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();
  G4double InputDisplacementWRTXaxis = GeoPars->GetInputDisplacementWRTXaxis();   
  G4double MirrorFlangeDistance = GeoPars->GetMirrorFlangeDistance()/m; 

  G4double MirrorZPos = fRichAcceptance.getMirrorPos();

 // retrieve from photon objects at position = index

  fPhotonsPosition[0] = fPMTPhotons[index].X()*1000 + InputDisplacementWRTXaxis; //convert m->mm
  fPhotonsPosition[1] = fPMTPhotons[index].Y()*1000; 
  //fPhotonsPosition[2] = fPMTPhotons[index].Z()*1000+219.625; //add rich beginning, i.e. PM position
  fPhotonsPosition[2] = fPMTPhotons[index].Z()*1000 + MirrorZPos - MirrorFlangeDistance; 

  fPhotonsEnergy = fPhotonEnergy[index];
 
 
  // G4cout<<"X pos: "<<fPhotonsPosition[0]<<" Y pos: "<<fPhotonsPosition[1]<<" Z pos: "<<fPhotonsPosition[2]<<G4endl;
  /*G4cout<<"Energy: "<<fPhotonsEnergy<<G4endl;*/
 //G4cout<<"TravelTime: "<<fTravelTime[index]*1e9<<G4endl;
  fPhotonsDelay = fTravelTime[index]*1e9;//convert from s in ns;
  
  
  //clear the vector!!!
/*  
  fPhotonEnergy.clear();
  fPMTPhotons.clear();
  */

}


 


