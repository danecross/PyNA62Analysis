// --------------------------------------------------------------------
// History:
//
// Created by Domenico Di Filippo (difilippo@na.infn.it) 2010-04-13
//
//  2010-04-19 Domenico Di Filippo: new independent variables definition
// --------------------------------------------------------------------

#include "Utils.hh"

MatrixFiller::MatrixFiller(Variable* val,
	             BinnedVariable* dim1,
	             BinnedVariable* dim2,
	             BinnedVariable* dim3,
	             BinnedVariable* dim4,
	             BinnedVariable* dim5,
	             BinnedVariable* dim6) {

   Value=val;

   if (dim1 != 0) Dimension.push_back(dim1);
   if (dim2 != 0) Dimension.push_back(dim2);
   if (dim3 != 0) Dimension.push_back(dim3);
   if (dim4 != 0) Dimension.push_back(dim4);
   if (dim5 != 0) Dimension.push_back(dim5);
   if (dim6 != 0) Dimension.push_back(dim6);
   
   matrix = 0;
   sum = 0;
   
}

void MatrixFiller::Prepare(){

      matrix = new LAVSampleMatrix();
      Load();
      if (matrix->GetDimensionsNumber() != 0) return; // Do better error recognition

      matrix->SetDimensionsNumber(Dimension.size());
      for (unsigned int d=0; d<Dimension.size(); d++){
         for (int i=0; i<Dimension[d]->divnum; i++){
	    if (Dimension[d]->div != 0) {
	       matrix->GetDimension(d)->AddDivision((Dimension[d]->div)[i]);
	    } else if (Dimension[d]->divnum > 1) {
	       const Double_t step = (Dimension[d]->max - Dimension[d]->min)/((Double_t)Dimension[d]->divnum);
	       for (Double_t v=Dimension[d]->min;v<Dimension[d]->max;v+=step)
	          matrix->GetDimension(d)->AddDivision(v);   
	    }
	 }
      }
      matrix->Prepare();
      matrix->Reset(0);

}

void MatrixFiller::Query(){

    for (unsigned int j=0; j<Dimension.size(); j++){
       if (Dimension[j]->value == 0) return;
       matrix->GetDimension(j)->Query(*(Dimension[j]->value));
    }
    
}

void MatrixFiller::Fill(){
  
    if (Value != 0) if (Value->value != 0) {
       if (matrix!=0)
	  matrix->SetValue(matrix->GetValue()+*(Value->value));
       sum += *(Value->value);
    }

}

string MatrixFiller::GetName(){

  if (Value == 0) return "";
  string name = Value->name;
  for (unsigned int j=0; j<Dimension.size(); j++)
     name.append("_"+Dimension[j]->name);
  return nametag+name;

}

void MatrixFiller::Save(){

   if (matrix != 0)
      matrix->Save((GetName()+".txt").c_str());

}

void MatrixFiller::Load(){

   if (matrix != 0)
      matrix->Load((GetName()+".txt").c_str());

}
