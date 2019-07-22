#include "Riostream.h"

#include "ChamberHitCollector.hh"
#include "TMath.h"

ChamberHitCollector::ChamberHitCollector(Int_t iChamber) :
  fPar(SpectrometerParameters::GetInstance()),
  fGeo(SpectrometerGeometry::GetInstance()),
  fChamber(iChamber)
{
  for (Int_t iView = 0; iView < fPar.GetNViews(); iView++)
    fViewHitCollector.push_back(new ViewHitCollector(iChamber, iView));
}

ChamberHitCollector::~ChamberHitCollector()
{
  for (ViewHitCollector *vhc : fViewHitCollector) delete vhc;
}

void ChamberHitCollector::Init() {
  fDATA = fPar.GetIsRawData();
  fDistCut2 = fPar.GetInterDistanceCut()*fPar.GetInterDistanceCut();
}

void ChamberHitCollector::Reset()
{
/// \MemberDescr
/// Clear the variable ChamberHitCollector::fChamberHit.
/// \EndMemberDescr

  fChamberHit.clear();
  for (Int_t j=0; j<4; j++) fNHit[j] = 0;
}

void ChamberHitCollector::ReconstructHit()
{
  CreateIntersection();
////  StoreSingleHit(); // << COMMENTED
}

void ChamberHitCollector::CreateIntersection() {

  Bool_t trailing_pair = 0;
  Bool_t trailing_triplet = 0;
  Bool_t trailing_quadruplet = 0;

  // Views: uvxy = 0123
  Int_t jView = 0;
  while(jView<=3){ 
    for (Int_t j=0; j<GetView(jView)->GetNcluster(); j++){
      Intersection inters;
      Cluster *clusterj = GetView(jView)->Getcluster(j);
      if (clusterj->GetNHit()<2) continue; // only paired hits in building chamber-hits (it discriminates out-of-time events) 
      inters.Reset();
      Int_t nextView = jView+1;
      while(nextView<=3){ // Look for 2-view intersections
        for (Int_t k=0; k<GetView(nextView)->GetNcluster(); k++){
          Int_t intType2 = IntType2(jView,nextView);
          Cluster *clusterk = GetView(nextView)->Getcluster(k);
          if (clusterk->GetNHit()<2) continue; // only paired hits in building chamber-hits (it discriminates out-of-time events)
          trailing_pair = (clusterj->GetEdge() && clusterk->GetEdge()) ? 1 : 0;
          if (fDATA && trailing_pair && fabs(clusterj->GetTrailingTime()-clusterk->GetTrailingTime())>=fPar.GetInterTtrailingCut()) continue; // 2014 reco
          Double_t xyinter[4] = {-9999., -9999., -9999., -9999.};
          ComputeCoordinate(intType2,clusterj->GetLocalPosition().X(),clusterk->GetLocalPosition().X(),xyinter); // transforms the coordinate in xyuv
          Double_t tinter2 = trailing_pair ? clusterj->GetTrailingTime()+clusterk->GetTrailingTime() : -2*999999.;
          inters.SetTrailingTime(tinter2/2.);
          inters.SetQuality(9999);
          inters.SetSubType(intType2);
//          inters.SetCoordinate(fDATA,xyinter);
          inters.SetCoordinate(1,xyinter);
          Int_t next2nextView = nextView+1;
          while(next2nextView<=3){ // Look for 3-view intersections (only 1 per 3-view intersection)
            Double_t mindist3(99999.);
            Int_t mini(-1);
            Int_t intType3(-1);
            for (Int_t i=0; i<GetView(next2nextView)->GetNcluster(); i++){
              Cluster *clusteri = GetView(next2nextView)->Getcluster(i);
              if (clusteri->GetNHit()<2) continue; // only paired hits in building chamber-hits (it discriminates out-of-time events)
              trailing_triplet = (trailing_pair && clusteri->GetEdge()) ? 1 : 0;
              if (fDATA && trailing_triplet && fabs(clusteri->GetTrailingTime()-clusterj->GetTrailingTime())>=fPar.GetInterTtrailingCut()) continue; // 2014 reco
              if (fDATA && trailing_triplet && fabs(clusteri->GetTrailingTime()-clusterk->GetTrailingTime())>=fPar.GetInterTtrailingCut()) continue; // 2014 reco
              if (fDATA && trailing_triplet && fabs(tinter2/2-clusteri->GetTrailingTime())>=fPar.GetInterTtrailingCut()) continue; // 2014 reco
              Double_t distance3(99999.);
              intType3 = IntType3(intType2,next2nextView); 
              if (!IntersectionQuality(intType3,xyinter,clusteri->GetLocalPosition().X(),&distance3)) continue; // distance3 from the 2-view intersection
              Double_t tave3 = trailing_triplet ? (tinter2+clusteri->GetTrailingTime())/3 : -999999.;
              Double_t stave3 = (clusteri->GetTrailingTime()-tave3)*(clusteri->GetTrailingTime()-tave3)+
                                (clusterj->GetTrailingTime()-tave3)*(clusterj->GetTrailingTime()-tave3)+
                                (clusterk->GetTrailingTime()-tave3)*(clusterk->GetTrailingTime()-tave3);
              stave3 /= 3;
              Double_t clusterq = (clusteri->GetQuality()+clusterj->GetQuality()+clusterk->GetQuality())/3;
              Double_t quality3 = distance3/2.+clusterq; // 2015 reco TO BE TUNED
              if (trailing_triplet) quality3 = distance3/2.+sqrt(stave3)/fPar.GetInter3ViewsTtrailingSigma()+clusterq; // 2014 reco
              if (quality3<mindist3) { mindist3=quality3; mini=i; } // Look for the best quality 3-view cluste (ADD CONDITIONS ?)r 
            } // end cluster loop in 3-view search 
            if (mini>-1) { // Search for a 4-view intersection only if a 3-view intersection exists
              Cluster *clustermini = GetView(next2nextView)->Getcluster(mini);
              trailing_triplet = (trailing_pair && clustermini->GetEdge()) ? 1 : 0;
              Double_t tinter3 = trailing_triplet ? tinter2+clustermini->GetTrailingTime() : -3*999999.;
              UpdateCoordinate(intType3,xyinter,clustermini->GetLocalPosition().X());
              Int_t next2next2nextView = next2nextView+1;
              Int_t intType4(15);
              while(next2next2nextView<=3){ // Look for 4-view intersections (only 1 per 4-view intersection)
                Double_t mindist4(99999.);
                Int_t minl(-1);
                for (Int_t l=0; l<GetView(next2next2nextView)->GetNcluster(); l++){
                  Double_t distance4(99999.);
                  Cluster *clusterl = GetView(next2next2nextView)->Getcluster(l);
                  if (clusterl->GetNHit()<2) continue; // only paired hits in building chamber-hits (it discriminates out-of-time events)
                  trailing_quadruplet = (trailing_triplet && clusterl->GetEdge()) ? 1 : 0;
                  if (fDATA && trailing_quadruplet && fabs(clusterl->GetTrailingTime()-clusterj->GetTrailingTime())>=fPar.GetInterTtrailingCut()) continue; // 2014 reco
                  if (fDATA && trailing_quadruplet && fabs(clusterl->GetTrailingTime()-clusterk->GetTrailingTime())>=fPar.GetInterTtrailingCut()) continue; // 2014 reco
                  if (fDATA && trailing_quadruplet && fabs(clusterl->GetTrailingTime()-clustermini->GetTrailingTime())>=fPar.GetInterTtrailingCut()) continue; // 2014 reco
                  if (fDATA && trailing_quadruplet && fabs(tinter3/3-clusterl->GetTrailingTime())>=fPar.GetInterTtrailingCut()) continue; // 2014 reco
                  if (!IntersectionQuality(intType4,xyinter,clusterl->GetLocalPosition().X(),&distance4)) continue;
                  Double_t tave4 = trailing_quadruplet ? (tinter3+clusterl->GetTrailingTime())/4 : -999999.;
                  Double_t stave4 = (clustermini->GetTrailingTime()-tave4)*(clustermini->GetTrailingTime()-tave4)+
                                    (clusterj->GetTrailingTime()-tave4)*(clusterj->GetTrailingTime()-tave4)+
                                    (clusterk->GetTrailingTime()-tave4)*(clusterk->GetTrailingTime()-tave4)+
                                    (clusterl->GetTrailingTime()-tave4)*(clusterl->GetTrailingTime()-tave4);
                  stave4 /= 4;
                  Double_t clusterq = (clustermini->GetQuality()+clusterj->GetQuality()+clusterk->GetQuality()+clusterl->GetQuality())/4;
                  Double_t quality4 = distance4/2.+clusterq; // 2015 reco TO BE TUNED
                  if (trailing_quadruplet) quality4 = distance4/2.+sqrt(stave4)/fPar.GetInter4ViewsTtrailingSigma()+clusterq; // 2014 reco
                  if (quality4<mindist4) { mindist4=quality4; minl=l; } // Look for the best quality 4-view cluster (ADD CONDITIONS ?) 
                } // end cluster loop in 4-view search
                if (minl>-1) { // 4-view cluster found
                  Cluster *clusterminl = GetView(next2next2nextView)->Getcluster(minl);
                  trailing_quadruplet = (trailing_triplet && clusterminl->GetEdge()) ? 1 : 0;
                  Double_t tinter4 = trailing_quadruplet ? tinter3+clusterminl->GetTrailingTime() : -4*999999.;
                  UpdateCoordinate(15,xyinter,clusterminl->GetLocalPosition().X());
                  Int_t arrayId[10] = {j,k,mini,minl,jView,nextView,next2nextView,next2next2nextView,intType4};
                  Double_t variable[2] = {mindist4,tinter4/4};
                  if (StoreHit(variable,arrayId,xyinter)){
                    inters.SetSubType(arrayId[8]); 
//                    inters.SetCoordinate(fDATA,xyinter);
                    inters.SetCoordinate(1,xyinter);
                    for (Int_t jvar=0; jvar<4; jvar++){
                      inters.SetClusterId(arrayId[jvar]);
                      inters.SetViewId(arrayId[jvar+4]);
                    }
                    inters.SetQuality(variable[0]);
                    inters.SetTrailingTime(variable[1]);
                    clusterminl->SetUsedForHit(1); 
                    clustermini->SetUsedForHit(1);
                    clusterj->SetUsedForHit(1);
                    clusterk->SetUsedForHit(1);
                    fChamberHit.push_back(inters);
                    fNHit[3]++;
                    inters.Reset();
                    intType3 = -1;
                  }
                }
                next2next2nextView++;
              } // End 4-view search
              if (intType3>-1) { // Store 3-view hit only if the corresponding 4 view hit does not exist
                Int_t arrayId[10] = {j,k,mini,-1,jView,nextView,next2nextView,-1,intType3};
                Double_t variable[2] = {mindist3,tinter3/3};
                if (StoreHit(variable,arrayId,xyinter)){
                  inters.SetSubType(arrayId[8]);
//                  inters.SetCoordinate(fDATA,xyinter);
                  inters.SetCoordinate(1,xyinter);
                  for (Int_t jvar=0; jvar<3; jvar++){
                    inters.SetClusterId(arrayId[jvar]);
                    inters.SetViewId(arrayId[jvar+4]);
                  }
                  inters.SetQuality(variable[0]);
                  inters.SetTrailingTime(variable[1]);
                  clustermini->SetUsedForHit(1);
                  clusterj->SetUsedForHit(1);
                  clusterk->SetUsedForHit(1);
                  fChamberHit.push_back(inters);
                  fNHit[2]++;
                  inters.Reset();
                  intType2 = -1;
                }
              }  
            } // end if (mini>-1)
            next2nextView++; //
          } // End 3-view search   
          if (intType2>-1) { // Store 2-view hit only if the corresponding 3 view hit does not exist
              if (AcceptanceTwoView(xyinter)) {
              Int_t arrayId[10] = {j,k,-1,-1,jView,nextView,-1,-1,intType2};
              Double_t variable[2] = {9999.,tinter2/2};
              //Bool_t storehit = 0;
              if (StoreHit(variable,arrayId,xyinter)){
                for (Int_t jvar=0; jvar<2; jvar++){
                  inters.SetClusterId(arrayId[jvar]);
                  inters.SetViewId(arrayId[jvar+4]);
                }
                clusterj->SetUsedForHit(1);
                clusterk->SetUsedForHit(1);
                inters.SetTrailingTime(variable[1]);
                fChamberHit.push_back(inters);
                fNHit[1]++;
                inters.Reset();
              } 
            } else intType2 = -1; 
          }  
          inters.Reset(); //
        } // end cluster loop in 2-view search 
        nextView++; //
      } // End 2-view search  
    } // end cluster loop 
    jView++; //
  } // End single view search

//////  std::cout << "total " << fChamberHit.size() << std::endl;
}

void ChamberHitCollector::StoreSingleHit()
{
  Int_t jView = 0; //
  while(jView<=3){ // 
    for (Int_t j=0; j<GetView(jView)->GetNcluster(); j++){ //
      Cluster *clusterj = GetView(jView)->Getcluster(j); //
      if (clusterj->GetUsedForHit()) continue;
      Intersection inters;
      inters.Reset();
      Int_t intType1 = IntType1(jView); 
      Double_t xyinter[4] = {-9999., -9999., -9999., -9999.};
      ComputeCoordinate(intType1,clusterj->GetLocalPosition().X(),xyinter); //
      inters.SetTrailingTime(clusterj->GetTrailingTime()); //
      inters.SetQuality(9999); //
      inters.SetSubType(intType1); //
////      inters.SetCoordinate(fDATA,xyinter); //
      inters.SetCoordinate(1,xyinter); //
      inters.SetClusterId(j);
      inters.SetViewId(jView);
      fChamberHit.push_back(inters); //
      fNHit[0]++; //
    } 
    jView++;
  }
}

Int_t ChamberHitCollector::IntType1(Int_t v1)
{
  if (v1==0) return 20; 
  if (v1==1) return 21; 
  if (v1==2) return 22; 
  if (v1==3) return 23; 
  return -1;
}

Int_t ChamberHitCollector::IntType2(Int_t v1, Int_t v2)
{
  if (v1==0 && v2==2) return 1; // ux
  if (v1==0 && v2==3) return 3; // uy
  if (v1==1 && v2==2) return 2; // vx
  if (v1==1 && v2==3) return 4; // vy
  if (v1==2 && v2==3) return 0; // xy
  return 5; // uv
}

Int_t ChamberHitCollector::IntType3(Int_t intType2, Int_t view)
{
  if (intType2==5 && view==2) return 11; // uv+x  
  if (intType2==5 && view==3) return 7;  // uv+y
  if (intType2==2 && view==3) return 13; // vx+y
  if (intType2==1 && view==3) return 14; // ux+y
  return -1;
}

void ChamberHitCollector::ComputeCoordinate(const Int_t &intType1, const Double_t &fX, Double_t *p)
{
  switch (intType1){
    case 20: // only u
    *(p+2) = fX;
    break;

    case 21: // only v
    *(p+3) = fX;
    break;

    case 22: // only x
    *p = fX;
    break;

    case 23: // only y
    *(p+1) = fX;
    break;
  }
}

void ChamberHitCollector::ComputeCoordinate(const Int_t &intType2, const Double_t &fX1, const Double_t &fX2, Double_t *p)
{
  Double_t p1 = intType2==0 || intType2==5 ? fX1 : fX2;
  Double_t p2 = intType2==0 || intType2==5 ? fX2 : fX1;
  Double_t fSQ2 = TMath::Sqrt2();

//  if (fDATA) {
    switch (intType2){
      case 0: *p=p1;           *(p+1)=p2;           *(p+2)=(p1-p2)/fSQ2; *(p+3)=(p1+p2)/fSQ2; break; // xy --
      case 1: *p=p1;           *(p+1)=p1-p2*fSQ2;   *(p+2)=p2;           *(p+3)=p1*fSQ2-p2;   break; // ux --
      case 2: *p=p1;           *(p+1)=-p1+p2*fSQ2;  *(p+2)=p1*fSQ2-p2;   *(p+3)=p2;           break; // vx --
      case 3: *p=p1+p2*fSQ2;   *(p+1)=p1;           *(p+2)=p2;           *(p+3)=p1*fSQ2+p2;   break; // uy --
      case 4: *p=-p1+p2*fSQ2;  *(p+1)=p1;           *(p+2)=-p1*fSQ2+p2;  *(p+3)=p2;           break; // vy
      case 5: *p=(p1+p2)/fSQ2; *(p+1)=(-p1+p2)/fSQ2; *(p+2)=p1;          *(p+3)=p2;           break; // uv
    }
//  } else {
//    switch (intType2){
//      case 0: *p=p1;           *(p+1)=p2;           *(p+2)=(p1+p2)/fSQ2; *(p+3)=(p1-p2)/fSQ2; break; // xy
//      case 1: *p=p1;           *(p+1)=-p1+p2*fSQ2;  *(p+2)=p2;           *(p+3)=p1*fSQ2-p2;   break; // ux
//      case 2: *p=p1;           *(p+1)=p1-p2*fSQ2;   *(p+2)=p1*fSQ2-p2;   *(p+3)=p2;           break; // vx
//      case 3: *p=-p1+p2*fSQ2;  *(p+1)=p1;           *(p+2)=p2;           *(p+3)=-p1*fSQ2+p2;  break; // uy
//      case 4: *p=p1+p2*fSQ2;   *(p+1)=p1;           *(p+2)=p1*fSQ2+p2;   *(p+3)=p2;           break; // vy
//      case 5: *p=(p1+p2)/fSQ2; *(p+1)=(p1-p2)/fSQ2; *(p+2)=p1;           *(p+3)=p2;           break; // uv
//    }
//  }
}

Int_t ChamberHitCollector::IntersectionQuality(Int_t type, Double_t *xyinter, Double_t xcluster, Double_t *qual)
{
  switch (type){

    case 15: // xyuv: uvx + y -> check on y 
    {
    Double_t dcoor[4]  = {-9999., -9999., -9999., -9999.};
    if ((*qual=fabs(xyinter[1]-xcluster))>fPar.GetInterDistanceCut()) return 0;
    Double_t fSQ2 = TMath::Sqrt2();
//    if (fDATA) {
      dcoor[0] = fabs(xyinter[0]-(xyinter[2]+xyinter[3])/fSQ2);
      dcoor[1] = fabs(xyinter[1]-(-xyinter[2]+xyinter[3])/fSQ2);
      dcoor[2] = fabs(xyinter[2]-(xyinter[0]-xcluster)/fSQ2);
      dcoor[3] = fabs(xyinter[3]-(xyinter[0]+xcluster)/fSQ2);
//    } else {
//      dcoor[0] = fabs(xyinter[0]-(xyinter[2]+xyinter[3])/fSQ2);
//      dcoor[1] = fabs(xyinter[1]-(xyinter[2]-xyinter[3])/fSQ2);
//      dcoor[2] = fabs(xyinter[2]-(xyinter[0]+xcluster)/fSQ2);
//      dcoor[3] = fabs(xyinter[3]-(xyinter[0]-xcluster)/fSQ2);
//    }
    Double_t mean(dcoor[0]);
    Double_t dist(dcoor[0]);
    for (Int_t j=1; j<4; j++) {
      mean += dcoor[j];
      if (dcoor[j]>dist) dist = dcoor[j];
    }
    mean /= 4.;
    Double_t sigm2(0.);
    for (Int_t j=0; j<4; j++) sigm2 += (dcoor[j]-mean)*(dcoor[j]-mean);
    if (dist>fPar.GetInterQuality4ViewsCut1() && sigm2/(mean*mean)>(fPar.GetInterQuality4ViewsCut2()*fPar.GetInterQuality4ViewsCut2())) return 0;
    if (dist>fPar.GetInterQuality4ViewsCut3()) return 0;
    if (mean>fPar.GetInterQuality4ViewsCut4()) return 0;

    }  
    break;

    case 14: // xyu: ux + y -> check on y (1 cm distance) 
    if ((*qual=fabs(xyinter[1]-xcluster))>fPar.GetInterQuality3ViewsCutXYU()) return 0;
    break;
     
    case 13: // xyv: vx + y -> check on y
    if ((*qual=fabs(xyinter[1]-xcluster))>fPar.GetInterQuality3ViewsCutXYV()) return 0;
    break;

    case 7:  // yuv: uv + y -> check on y
    if ((*qual=fabs(xyinter[1]-xcluster))>fPar.GetInterQuality3ViewsCutYUV()) return 0;
    break;
 
    case 11: // xuv: uv + x -> check on x
    if ((*qual=fabs(xyinter[0]-xcluster))>fPar.GetInterQuality3ViewsCutXUV()) return 0;
    break;

    default:
    return 0;
  }

  return 1;
}

void ChamberHitCollector::UpdateCoordinate(Int_t type, Double_t *xyinter, Double_t xcluster)
{
  switch (type){

    case 15: case 14: case 13: case 7: // xyuv, xyu, xyv, yuv
    xyinter[1]=xcluster;
    break;

    case 11:
    xyinter[0]=xcluster;
    break;

    default:
    break;
  }
}

Bool_t ChamberHitCollector::AcceptanceTwoView(Double_t *xyinter)
{
  TVector2 xy(xyinter);
  if (xy.Mod2() >= 1000*1000) return false;
  // two view region
  if (fGeo.StrawAcceptance(fChamber, xy, 12)) return true;
  // three view region
  if (fGeo.StrawAcceptance(fChamber, xy, 13)) return true;
  // four view region
  if (fGeo.StrawAcceptance(fChamber, xy, 4)) return true;
  // if not found, then false
  return false;
}

Int_t ChamberHitCollector::StoreHit(Double_t *var, Int_t *arrayId, Double_t *xyinter)
{
  // Intersection type
  Int_t thistype(0);
  for (Int_t j=0; j<4; j++) {
    if (arrayId[j]>-1) thistype++;
  }

  for (size_t jHit=0; jHit<fChamberHit.size(); jHit++) {

    // Cluster in common with existing hits
    Int_t ncommon(0);
    for (Int_t jCluster=0; jCluster<GetHit(jHit)->GetType(); jCluster++){
      for (Int_t jCluster1=0; jCluster1<4; jCluster1++){
        if (arrayId[jCluster1]==-1) continue;
        if (GetHit(jHit)->GetViewId(jCluster)==arrayId[4+jCluster1] && 
            GetHit(jHit)->GetClusterId(jCluster)==arrayId[jCluster1]) ncommon++;
      }
    }

    // Distance between existing hits
    Double_t oldxyinter[2] = {GetHit(jHit)->GetXcoor(),GetHit(jHit)->GetYcoor()};
    Double_t dist(0);
    for (Int_t j=0; j<2; j++) dist += (xyinter[j]-oldxyinter[j])*(xyinter[j]-oldxyinter[j]);

    // Conditions 
    Int_t replace(0);
    if (dist>fDistCut2) {
      if (ncommon==0) continue;  

      if (ncommon==1) {
        if (thistype==2) {
//          if (GetHit(jHit)->GetType()==2) continue; // keep this hit and the already existing one
//          if (GetHit(jHit)->GetType()>2) return 0; // reject this hit and keep the already existing one 
          if (GetHit(jHit)->GetType()==2) {
            if (fGeo.StrawAcceptance(fChamber,xyinter,12)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,12)==0) {
              fChamberHit.erase(fChamberHit.begin()+jHit);
              jHit--;
              continue; 
            } 
            if (fGeo.StrawAcceptance(fChamber,xyinter,12)==0 && fGeo.StrawAcceptance(fChamber,oldxyinter,12)==1) return 0;
            if (fGeo.StrawAcceptance(fChamber,xyinter,12)==0 && fGeo.StrawAcceptance(fChamber,oldxyinter,12)==0) continue;
//            if (fGeo.StrawAcceptance(fChamber,xyinter,12)==0 && fGeo.StrawAcceptance(fChamber,oldxyinter,12)==0) {
//              if (fGeo.StrawAcceptance(fChamber,xyinter,4)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,4)==1) continue;
//              if (fGeo.StrawAcceptance(fChamber,xyinter,4)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,13)==1) return 0;
//              if (fGeo.StrawAcceptance(fChamber,xyinter,13)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,4)==1) replace = 1;
//              if (fGeo.StrawAcceptance(fChamber,xyinter,13)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,13)==1) continue;
//            } 
            if (fGeo.StrawAcceptance(fChamber,xyinter,12)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,12)==1) continue;
          } 
          if (GetHit(jHit)->GetType()>2) {
            if (fGeo.StrawAcceptance(fChamber,xyinter,4)==1) return 0; // if the 2 view hit is in regions where 4 are expected reject the hit if has a straw in common with a > 2 view hit
            if (fGeo.StrawAcceptance(fChamber,xyinter,4)==0) { // if the 2 view hit is not in regions where 4 are expected
              if (GetHit(jHit)->GetType()==3) { // if the other hit is 3 view
                if (fGeo.StrawAcceptance(fChamber,oldxyinter,13)==1) return 0; // reject the 2 view hit if the other hit is in a 3 view hit region
                else if (fGeo.StrawAcceptance(fChamber,oldxyinter,13)==0) {
                  if (fGeo.StrawAcceptance(fChamber,xyinter,12)==1) replace = 1; // keep the 2 view and reject the 3 view only if the 2 view is in a 2 view region and the 3 view is not in a 3 view region
                  else return 0;
                } else return 0; 
              }
              if (GetHit(jHit)->GetType()==4) { // if the other hit is 4 view
                if (fGeo.StrawAcceptance(fChamber,oldxyinter,4)==1) return 0; // reject the 2 view hit if the other hit is in a 4 view hit region
                else if (fGeo.StrawAcceptance(fChamber,oldxyinter,4)==0) {
                  if (fGeo.StrawAcceptance(fChamber,xyinter,12)==1) replace = 1; // keep the 2 view and reject the 4 view only if the 2 view is in a 2 view region and the 3 view is not in a 3 view region
                  else return 0;
                } else return 0; 
              }
            }
          }
        }
        if (thistype>2) {
//          if (GetHit(jHit)->GetType()>2) continue; 
          if (GetHit(jHit)->GetType()>2) {
            if (thistype==3 && GetHit(jHit)->GetType()==3) {
              if (fGeo.StrawAcceptance(fChamber,xyinter,13)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,13)==0) replace = 1;
              if (fGeo.StrawAcceptance(fChamber,xyinter,13)==0 && fGeo.StrawAcceptance(fChamber,oldxyinter,13)==1) return 0;
              if (fGeo.StrawAcceptance(fChamber,xyinter,13)==0 && fGeo.StrawAcceptance(fChamber,oldxyinter,13)==0) continue;
              if (fGeo.StrawAcceptance(fChamber,xyinter,13)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,13)==1) continue;
            }
            if (thistype==3 && GetHit(jHit)->GetType()==4) {
              if (fGeo.StrawAcceptance(fChamber,xyinter,13)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,4)==0) replace = 1;
              if (fGeo.StrawAcceptance(fChamber,xyinter,13)==0 && fGeo.StrawAcceptance(fChamber,oldxyinter,4)==1) return 0;
              if (fGeo.StrawAcceptance(fChamber,xyinter,13)==0 && fGeo.StrawAcceptance(fChamber,oldxyinter,4)==0) continue;
              if (fGeo.StrawAcceptance(fChamber,xyinter,13)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,4)==1) continue;
            }
            if (thistype==4 && GetHit(jHit)->GetType()==3) {
              if (fGeo.StrawAcceptance(fChamber,xyinter,4)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,13)==0) replace = 1;
              if (fGeo.StrawAcceptance(fChamber,xyinter,4)==0 && fGeo.StrawAcceptance(fChamber,oldxyinter,13)==1) return 0;
              if (fGeo.StrawAcceptance(fChamber,xyinter,4)==0 && fGeo.StrawAcceptance(fChamber,oldxyinter,13)==0) continue;
              if (fGeo.StrawAcceptance(fChamber,xyinter,4)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,13)==1) continue;
            }
            if (thistype==4 && GetHit(jHit)->GetType()==4) continue; 
          } 
          if (GetHit(jHit)->GetType()==2) {
            if (fGeo.StrawAcceptance(fChamber,oldxyinter,4)==1) replace = 1;
            if (fGeo.StrawAcceptance(fChamber,oldxyinter,4)==0) {
              if (thistype==3) { // if the other hit is 3 view
                if (fGeo.StrawAcceptance(fChamber,xyinter,13)==1) replace = 1; // reject the 2 view hit is the other hit is in a 3 view hit region
                else if (fGeo.StrawAcceptance(fChamber,xyinter,13)==0) {
                  if (fGeo.StrawAcceptance(fChamber,oldxyinter,12)==1) return 0; // keep the 2 view and reject the 3 view only if the 2 view is in a 2 view region and the 3 view is not in a 3 view region
                  else replace = 1;
                } else replace = 1; 
              }
              if (thistype==4) { // if the other hit is 4 view
                if (fGeo.StrawAcceptance(fChamber,xyinter,4)==1) replace = 1; // reject the 2 view hit is the other hit is in a 4 view hit region
                else if (fGeo.StrawAcceptance(fChamber,xyinter,4)==0) {
                  if (fGeo.StrawAcceptance(fChamber,oldxyinter,12)==1) return 0; // keep the 2 view and reject the 4 view only if the 2 view is in a 2 view region and the 3 view is not in a 3 view region
                  else replace = 1;
                } else replace = 1; 
              }
            }
          }
        } 
      }
 
    }
    if (replace) {
      fChamberHit.erase(fChamberHit.begin()+jHit);
      jHit--;
      continue;
    } 
    if (dist>fDistCut2) continue; // keep only those few cases with hits at 3 or 4 views with 2 views in commons 

    // Here only if dist < fDistCut2 
//    if (thistype>2) {
//      if (thistype==GetHit(jHit)->GetType()) {
//        if (var[0]<GetHit(jHit)->GetQuality()) replace = 1;
//        else return 0;
//      }
//      if (thistype>GetHit(jHit)->GetType()) replace = 1;
//      if (thistype<GetHit(jHit)->GetType()) return 0;
//    }
// 
//    if (thistype==2) {
//      if (dist>fDistCut2) replace = 1;
//      else {
//        if (GetHit(jHit)->GetType()>thistype) return 0; 
//        if (GetHit(jHit)->GetType()==thistype) return 0; 
//      } 
//    }
    if (thistype>2 && GetHit(jHit)->GetType()>2) {
      if (thistype==GetHit(jHit)->GetType()) { // Same quality hits close each other
        if (var[0]<GetHit(jHit)->GetQuality()) replace = 1;
        else return 0;
      } else if (thistype<GetHit(jHit)->GetType()) return 0; // a bit naive. Use geometrical constrains to improve ?
      else replace = 1;
    } 
    if (thistype>2 &&  GetHit(jHit)->GetType()==2) {
      replace = 1;
    }
    if (thistype==2 &&  GetHit(jHit)->GetType()>2) {
      return 0;
    }
    if (thistype==2 &&  GetHit(jHit)->GetType()==2) {
      if (fGeo.StrawAcceptance(fChamber,xyinter,12)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,12)==0) replace = 1;
      if (fGeo.StrawAcceptance(fChamber,xyinter,12)==0 && fGeo.StrawAcceptance(fChamber,oldxyinter,12)==1) return 0;
      if (fGeo.StrawAcceptance(fChamber,xyinter,12)==1 && fGeo.StrawAcceptance(fChamber,oldxyinter,12)==1) return 0;
      if (fGeo.StrawAcceptance(fChamber,xyinter,12)==0 && fGeo.StrawAcceptance(fChamber,oldxyinter,12)==0) return 0;
    }
 
    if (replace) {
      fChamberHit.erase(fChamberHit.begin()+jHit);
      jHit--;
    } 

  } 

  return 1; // Store as a new hit
}

