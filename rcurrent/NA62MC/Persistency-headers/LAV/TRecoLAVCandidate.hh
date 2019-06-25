// --------------------------------------------------------------
// History:
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - Added methods to set and retrieve cluster properties
// - Doxygen-compliant documentation added
// 2015-01-22 Totally modified and revised by T. Spadaro and E. Leonardi
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoLAVCandidate_H
#define TRecoLAVCandidate_H

#include "TRecoVCandidate.hh"

using namespace std;

class TRecoLAVCandidate : public TRecoVCandidate {

    public:

       TRecoLAVCandidate();
       ~TRecoLAVCandidate(){};
       void Clear(Option_t* = "");

       void SetEnergy(Double_t val){fEnergy = val;}
       Double_t GetEnergy(){return fEnergy;}

// Number of Hits per layer

       void SetNHitsPerLayer(Int_t ilay, Int_t val){if (ilay<0 || ilay>=5) return; fNHitsPerLayer[ilay] = val;}
       Int_t GetNHitsPerLayer(Int_t ilay){if (ilay<0 || ilay>=5) return 0; return fNHitsPerLayer[ilay];}

// Algorithm Type

       void  SetAlgorithm(Int_t val) {fAlgorithm = val;}
       Int_t GetAlgorithm() {return fAlgorithm;}

// Classification of the cluster: MIP, Shower, Unknown

       void  SetClusterType(Int_t val) {fClusterType = val;}
       Int_t GetClusterType() {return fClusterType;}

// Centroid position: unwegithed and energy-weighted (at the moment, in fact, charge-weighted) average

       void SetPosition(TVector3 val){fPosition = val;}
       TVector3 GetPosition(){return fPosition;}
       void SetWeightedPosition(TVector3 val){fWeightedPosition = val;}
       TVector3 GetWeightedPosition(){return fWeightedPosition;}

// Methods to set/retrieve errors on the averages 

       void SetZUnweightedError(Double_t val){fZUnweightedError = val;}     
       Double_t GetZUnweightedError(){return fZUnweightedError;}            
     
       void SetPhiUnweightedError(Double_t val){fPhiUnweightedError = val;} 
       Double_t GetPhiUnweightedError(){return fPhiUnweightedError;}        
     
       void SetZWeightedError(Double_t val){fZWeightedError = val;}         
       Double_t GetZWeightedError(){return fZWeightedError;}                
     
       void SetPhiWeightedError(Double_t val){fPhiWeightedError = val;}     
       Double_t GetPhiWeightedError(){return fPhiWeightedError;}            

  // Method to retrieve the error matrix of x,y,z centroid coordinate estimates

  static Double_t GetCentroidErrorMatrix(TVector3, Double_t, Double_t, Double_t, Int_t, Int_t);
  
    private:

       Double_t fEnergy;             ///< Total cluster energy (at the moment, in fact, it is the total charge)
       TVector3 fPosition;           ///< Cluster centroid position: no weight is used while averaging fired block positions
       TVector3 fWeightedPosition;   ///< Cluster centroid position: the block energy (in fact, at the moment, the charge) is used while averaging fired block positions
       Double_t fZUnweightedError;   ///< Error on the unweighted average of the z coordinate
       Double_t fPhiUnweightedError; ///< Error on the unweighted average of the azimuthal angle
       Double_t fZWeightedError;     ///< Error on the energy-weighted average of the z coordinate   
       Double_t fPhiWeightedError;   ///< Error on the energy-weighted average of the azimuthal angle

       Int_t fNHitsPerLayer[5];   ///< Number of hits per layer
       Int_t fAlgorithm;  ///< =0 if cluster is done by grouping adjacent blocks; =1 if cluster is done by grouping blocks close in phi and time (so-called "tracking"); =2 if no algo is passed
       Int_t fClusterType;  ///<  =1 for MIP cluster (>=2 rows hit with 1 hit per row); =2 for showerLike (>=2 rows hit with > 1 hit per row in at least one row); =0 else
       ClassDef(TRecoLAVCandidate,1);
};
#endif
