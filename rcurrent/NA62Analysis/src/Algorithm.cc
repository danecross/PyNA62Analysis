/// \class Algorithm
/// \Brief
/// Base class for a standard piece of code used in analysers
/// \EndBrief
/// \Detailed
/// Algorithm is designed to contain a standard piece of code used in analysers.
/// It goes beyond a simple ToolsLib object as it gives access to all features of
/// Analysers (histogram booking, output registering, etc).
/// Several instances of a given Algorithm can be used in the same analyser
/// (with different names) in order to perform optimisation.
/// An example for Algorithm is provided as ThreePiAssociationAlgo.
/// Algorithm are created in Analyzer as:
/// fThreePiAlgo = new ThreePiAssociationAlgo(ba,this,"ThreePiAlgo");
/// Histograms booked in the Algorithm can be saved by calling:
/// fThreePiAlgo->SaveAllPlots();
/// in the EndOfJobUser().
/// \author Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch)
/// \EndDetailed

#include "Algorithm.hh" 
#include "TPRegexp.h"

namespace NA62Analysis {

Algorithm::Algorithm(BaseAnalysis *ba, Analyzer* ana, const std::string &name):
  UserMethods(ba, (ana->GetAnalyzerName()+"/"+name).Data()),
  fAlgoName(name),
  fCallingAnaName(ana->GetAnalyzerName()),
  fCallingAna(ana)
{
  //FIXME add a mechanism to detect if two algo have the same name (static map?)
}

void Algorithm::SaveAllPlots() {
  //cout<<"Save All Plots: Calling="<<fCallingAnaName<<" Algo="<<fAlgoName<<endl;
  // TString CallingAnaName(fCallingAnaName);
  //TPRegexp("\\.").Substitute(CallingAnaName,"/","g");
  //cout<<"Current Dir :";
  //gDirectory->pwd();

  if(gFile->GetDirectory(TString(fCallingAnaName)+"/"+TString(fAlgoName))==NULL){
    gFile->mkdir(TString(fCallingAnaName)+"/"+TString(fAlgoName));
    gDirectory->Cd(fAlgoName.c_str());
    //gDirectory->pwd();
  }
  UserMethods::SaveAllPlots();
  gDirectory->Cd("..");
}
}  /* namespace NA62Analysis */
