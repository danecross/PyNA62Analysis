// --------------------------------------------------------------------
// History:
//
// Created by Domenico Di Filippo (difilippo@na.infn.it) 2012-01-26
//
// --------------------------------------------------------------------

#include <iostream>
using namespace std;

#include "LAVSampleMatrix.hh"

int main(int argc, char *argv[]){

   if (argc<3) {
      cout << "USAGE: " << argv[0] << " FileIn.txt FileInOut" << endl;
      return -1;
   }

   LAVSampleMatrix In,InOut;
   In.Load(argv[1]);
   InOut.Load(argv[2]);

   DataType *in = In.GetArray();
   DataType *inout = InOut.GetArray();

   for (unsigned int i=0; i<In.GetSize(); i++)
      inout[i] /= in[i];

   InOut.Save(argv[2]);

   return 0;

}
