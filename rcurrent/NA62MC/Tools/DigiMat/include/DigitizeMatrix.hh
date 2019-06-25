// --------------------------------------------------------------------
// History:
//
// Created by Domenico Di Filippo (difilippo@na.infn.it) 2010-04-13
//
//  2010-04-19 Domenico Di Filippo: new independent variables definition
// --------------------------------------------------------------------

#ifndef DIGITIZEMATRIX_HH
#define DIGITIZEMATRIX_HH

#include "Utils.hh"

#include <vector>
#include "TTree.h"
#include "TFile.h"
#include "TRint.h"
#include "TH1I.h"
#include "TCanvas.h"
#include "TMath.h"
#include "TROOT.h"
#include "TF1.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TProfile2D.h"
#include "TLeaf.h"
#include "TBranch.h"
#include "TClonesArray.h"
#include "TROOT.h"
#include "CLHEP/Vector/ThreeVector.h"
#include <math.h>
#include "Event.hh"
#include "LAVSampleMatrix.hh"
#include "TLAVEvent.hh"
#include "KinePart.hh"
#include "TLAVHit.hh"
#include "LAVGeometryParameters.hh"

using namespace std;

#define __MATRIX_FILE__ "MatrixHeader.txt"

class Application {

public:
   Application(int argc=0, char **argv=0);
   void Run();

  void Prepare();
  void Process();
  void Save();

  void HitReset();
  void CalculatePrimary();
  void CalculateHits();

private:
   vector<string> args;

  // DATA
  TFile *currentfile;
  Event *aRUNEvent;
  TLAVEvent *aLAVEvent;
  KinePart *kinepart;
  TLAVHit *lavhit;
  vector<MatrixFiller*> WorkMatrix;
  LAVSampleMatrix Header;
  Double_t px,py,pz,dx,dy,dz,en,gen,pr,pf,dr,df,phs,timsum,timsum2;
  Float_t *tim;
  Int_t phsi;
  //CLHEP::Hep3Vector CerenkovCenter, BlockAxis, OrthoFace, ThirdVersor;
  CLHEP::Hep3Vector xver,yver,zver,center;
  Int_t chid;

};

#endif

