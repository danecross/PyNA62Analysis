// ---------------------------------------------------------------
// History:
//
// Created by Riccardo Fantechi (fantechi@cern.ch) 2016-07-05
// from a previous set of methods by G. Ruggiero
// Adaptation for NA62Analysis: E. Goudzovski, October 2016
//
// ---------------------------------------------------------------

/// \class LKrAuxClusterReco
/// \Brief
/// An alternative LKr cluster reconstruction algorithm with higher efficiency
/// \EndBrief
/// \Detailed
/// Search for LKr clusters based on TRecoLKrEvent, within
/// a tunable time window (default: +-40ns) of the reference time provided.
/// The efficiency of this algorithm
/// is higher than that of the standard LKr reconstruction, especially for low-energy clusters.
/// The clusters reconstructed by the standard reconstruction are generally reconstructed
/// also by this algorithm.
/// Example of use:
/// \code
/// // Init part of an Analyzer
/// fLKrAuxClusterReco = new LKrAuxClusterReco();
/// ...
/// // Event processing part of an Analyzer
/// fLKrAuxClusterReco->FindClusters(ReferenceTime, LKrEvent);
/// fLKrAuxClusterReco->PrintClusters();
/// for (Int_t i=0; i<fLKrAuxClusterReco->GetNClusters(); i++) {
///   Double_t Energy = fLKrAuxClusterReco->GetCandidate(i)->GetClusterEnergy();
///   Double_t Time = fLKrAuxClusterReco->GetCandidate(i)->GetTime();
///   Double_t x = fLKrAuxClusterReco->GetCandidate(i)->GetClusterX();
///   Double_t y = fLKrAuxClusterReco->GetCandidate(i)->GetClusterY();
///   ...
/// }
/// \endcode
/// This tool is useful for LKr veto. For one-track decays, the event can be vetoed e.g. if in-time clusters with
/// energy above 2 GeV and more than 60mm away from the track impact point are found.
/// \author Riccardo Fantechi (riccardo.fantechi@cern.ch)
/// \author Giuseppe Ruggiero (giuseppe.ruggiero@cern.ch)
/// \author Evgueni Goudzovski (evgueni.goudzovski@cern.ch)
/// \EndDetailed

#include "LKrAuxClusterReco.hh"
#include "NA62ConditionsService.hh"
#include "NA62Exceptions.hh"

using namespace std;
using namespace NA62Analysis;

Bool_t CompareHits (hit hi, hit hj) {
  return (hi.y<hj.y) ?
    true : ((hj.y<hi.y) ?
	    false : (hi.x<hj.x ?
		     true : ((hj.x<hi.x ?
		          // cppcheck-suppress duplicateValueTernary
			      false : false))));
}

LKrAuxClusterReco::LKrAuxClusterReco() :
  Ncell(0),
  newCluster(0),
  totalCluster(0)
{

  fNewHits.clear();
  for (Int_t i=0; i<MAXCLUSTERNO; i++) fNewCandidates[i] = new TRecoLKrCandidate();

  // Default parameters (could be overridden using the appropriate methods)
  maxCluster              = 20;
  fCutClustTrackDist      = 150.;    // in mm
  fCutClustMatchClustDist = 100.;    // in mm
  fCutMinHitEnergy        = 40.;     // in MeV
  fCutMaxHitEnergy        = 100000.; // in MeV
  fCutDeltaTime           = 40.;     // in ns
  fCutCellDistance        = 10;      // in cell units
  fHitTimeCorrection      = 0.0;     // in ns
}

LKrAuxClusterReco::~LKrAuxClusterReco() {
  for (Int_t i=0; i<MAXCLUSTERNO; i++) delete fNewCandidates[i];
}

Bool_t LKrAuxClusterReco::Nearby
(std::vector<hit>::iterator i, std::vector<hit>::iterator j) {
/*  for (Int_t kk=0; kk<fCutCellDistance; kk++)
  {
    if (j->x==i->x + kk)
    {
      for (Int_t k=0; k<fCutCellDistance; k++)
      {
        if ((j->y==i->y+k) ||
            (j->y==i->y-k)) return 1;
      }
    }
    if (j->x==i->x-kk)
    {
      for (Int_t k=0; k<fCutCellDistance; k++)
      {
        if ((j->y==i->y+k) ||
            (j->y==i->y-k)) return 1;
      }
    }
  }
  return 0;
*/
  Int_t dx = j->x - i->x;
  Int_t dy = j->y - i->y;
  if (dx*dx+dy*dy <= fCutCellDistance*fCutCellDistance) return true;
  return false;
}

Int_t LKrAuxClusterReco::FindClusters(Double_t timeref, TRecoLKrEvent *event) {

  std::vector<hit>::iterator it;
  std::vector<std::vector<hit>::iterator> id;
  // Init
  Clear();
  // Prepare data for the clustering procedure
  ReadCell(timeref,event);
  // Sort the hits by column and row
  std::sort(fNewHits.begin(), fNewHits.end(), CompareHits);
  auto last = std::end(fNewHits); last--;  // last is the iterator to the last element
  //for (it = std::begin(fNewHits); it!=std::end(fNewHits); ++it) cout <<  " New 2 x y " << it->x << " " << it->y <<  " E " << it->Ecells << std::endl;
  //Clear "already used" variable
  for (auto it1 = std::begin(fNewHits); it1!=std::end(fNewHits); ++it1) it1->iread=0;
  // Cascade loop over the cells
  Int_t m=0;
  for (it = std::begin(fNewHits); it!=std::end(fNewHits); ++it) {
    if (it->iread) continue;
    Int_t l=0;
    auto kt=it;
    Double_t energy = 0.;
    Double_t xpos = 0.;
    Double_t ypos = 0.;
    Double_t time = 0.;
    Double_t energyprev = 0.;
    for (auto jt = std::begin(fNewHits); jt!=std::end(fNewHits); ++jt) {
      if(Nearby(it,jt)) {
        if (!jt->iread) {
	  //cout <<  " x y " << it->x << " " << it->y <<  " x y " << jt->x << " " << jt->y << " l " << l << endl;
          jt->iread=1;
          id.push_back(jt);
          it=jt;
          energy+=it->Ecells;
          if (it->Ecells>energyprev) {
	    time = it->Tcells;
	    energyprev = it->Ecells;
	  }
          xpos+=it->Xcells*it->Ecells;
          ypos+=it->Ycells*it->Ecells;
          jt=std::begin(fNewHits);
          l++;
        }
      }
      if (jt==last&&m<l) {
	//cout << "Ncell-1 " <<  " l " << l << " m " << m << " energy " << energy << endl;
        jt=std::begin(fNewHits);
        it = id.at(m);
        m++;
	//cout << "Ncell * " << " i " << i << " j " << j << " l " << l << " m " << m << endl;
      }
    }

    // Store cluster
    if (l>1 && newCluster<maxCluster) {
      if (energy>0.0) {
	//time=time/energy;
        xpos=xpos/energy;
        ypos=ypos/energy;
      }
      else {
        energy=0.0;
        time=999999.;
        xpos=999999.;
        ypos=999999.;
      }
      //cout << "xpos, ypos " << xpos << " " << ypos << endl;
      //cout << "Energy found " << energy << " i " << i <<  " k " << k << " l " << l << " m " << m <<endl;
      if (energy>0.0) {
        fNewCandidates[newCluster]->SetClusterEnergy(energy);
	fNewCandidates[newCluster]->SetTime(time);
        fNewCandidates[newCluster]->SetTime(time);
        fNewCandidates[newCluster]->SetClusterX(xpos);
        fNewCandidates[newCluster]->SetClusterY(ypos);
        fNewCandidates[newCluster]->SetNCells(l);
        newCluster++;
	totalCluster++;
      }
    }
    // Reset
    it=kt;
    m=0;
    id.clear();
    //cout << " reset " << " i " << i << " k " << k << endl;
  }
  return newCluster;
}

void LKrAuxClusterReco::ReadCell(Double_t timeref, TRecoLKrEvent *event) {
  hit temphit;
  fNewHits.clear();
  TClonesArray& Hits = *(event->GetHits());
  //cout << "Aux " << (Int_t) event->GetNHits() << " timeref " << timeref <<endl;
  for (Int_t j=0; j<event->GetNHits(); j++) {
    TRecoLKrHit *hit = static_cast<TRecoLKrHit*>(Hits[j]);
    if (hit->GetEnergy() <= fCutMinHitEnergy) continue;
    Double_t xcell = hit->GetPosition().X();
    Double_t ycell = hit->GetPosition().Y();
    Int_t ixcell = (Int_t)hit->GetXCellID();
    Int_t iycell = (Int_t)hit->GetYCellID();
    Double_t tcell = hit->GetTime() + fHitTimeCorrection;
    //cout << "Aux ix iy " << ixcell << " " << iycell << " tcell " << tcell << endl;
    if (fabs(tcell-timeref)>fCutDeltaTime) continue; // in time with the reference
    if (fabs(hit->GetEnergy()) > fCutMaxHitEnergy) continue;
    temphit.x = ixcell;
    temphit.y = iycell;
    temphit.Tcells = hit->GetTime() + fHitTimeCorrection;
    temphit.Ecells = hit->GetEnergy();
    temphit.Xcells = xcell;
    temphit.Ycells = ycell;
    fNewHits.push_back(temphit);
  }
  Ncell = fNewHits.size();
  //cout << "Aux " << Ncell <<endl;
}

void LKrAuxClusterReco::Clear() {
  newCluster = 0;
  fNewHits.clear();
  //fNewCandidates.clear();
}

void LKrAuxClusterReco::PrintClusters() {
  cout << "[LKrAuxClusterReco] Found " << GetNClusters() << " cluster";
  if (GetNClusters()!=1) cout << "s";
  cout << endl;
  for (Int_t j=0; j<GetNClusters(); j++) {
    cout << "[LKrAuxClusterReco] " << "E= " << GetCandidate(j)->GetClusterEnergy() << " t= " << GetCandidate(j)->GetTime() << " x,y= " << GetCandidate(j)->GetClusterX() << " " << GetCandidate(j)->GetClusterY() << endl;
  }
}

void LKrAuxClusterReco::PrintSummary() {
  cout << "[LKrAuxClusterReco] " << GetNTotalClusters() << " clusters found" << endl;
}

void LKrAuxClusterReco::PrintParameters() {
  cout << "[LKrAuxClusterReco] Parameter settings" << endl << endl;
  cout << "[LKrAuxClusterReco] Cluster-track distance cut             " << fCutClustTrackDist << " mm" << endl;
  cout << "[LKrAuxClusterReco] Cluster-matched cluster distance cut   " << fCutClustMatchClustDist << " mm" << endl;
  cout << "[LKrAuxClusterReco] Hit minimum energy                     " << fCutMinHitEnergy   << " MeV" << endl;
  cout << "[LKrAuxClusterReco] Hit maximum energy                     " << fCutMaxHitEnergy   << " MeV" << endl;
  cout << "[LKrAuxClusterReco] Delta time cut                         " << fCutDeltaTime      << " ns" << endl;
  cout << "[LKrAuxClusterReco] Max distance between hits (in x and y) " << fCutCellDistance   << " cells" << endl;
  cout << "[LKrAuxClusterReco] Hit time correction                    " << fHitTimeCorrection << " ns" << endl;
}
