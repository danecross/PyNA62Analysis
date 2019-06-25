// --------------------------------------------------------------------
// History:
//
// Created by Domenico Di Filippo (difilippo@na.infn.it) 2010-04-13
//
//  2010-04-19 Domenico Di Filippo: new independent variables definition
// --------------------------------------------------------------------

#ifndef UTILS_HH
#define UTILS_HH

#include "LAVSampleMatrix.hh"
#include <sstream>
#include <iostream>
#include <string>
#include <fstream>
#include "TFile.h"

using namespace std;

#define __MAX_PH_PER_EV__ 10000

class Variable {
  public:
    Variable(string n,Double_t*f)
      {name=n;value=f;}

   string name;
   Double_t *value;
};

class BinnedVariable: public Variable {
public:
   BinnedVariable(string n,Double_t*f,Int_t s,Double_t*d):Variable(n,f)
      {min=0;max=0;divnum=s;div=d;}
   BinnedVariable(string n,Double_t*f,Int_t s,Double_t m,Double_t M):Variable(n,f)
      {min=m;max=M;divnum=s;div=0;}

   Double_t min, max;
   Int_t divnum;
   Double_t *div;
};

class MatrixFiller {
  public:
    MatrixFiller(Variable* val = 0,
	      BinnedVariable* dim1 = 0,
	      BinnedVariable* dim2 = 0,
	      BinnedVariable* dim3 = 0,
	      BinnedVariable* dim4 = 0,
	      BinnedVariable* dim5 = 0,
	      BinnedVariable* dim6 = 0);

    void SetTagName(string tag=""){nametag=tag;}
    string GetName();

    void Prepare();
    void Load();
    void Save();

    void Update(){Query();Fill();}
    void Query();
    void Fill();

    DataType GetMatrixValue()
       {if(matrix==0)return 0;return matrix->GetValue();}

    string nametag;
    LAVSampleMatrix *matrix;
    vector<BinnedVariable*> Dimension;
    Variable *Value;
    Double_t sum;

};

#endif

