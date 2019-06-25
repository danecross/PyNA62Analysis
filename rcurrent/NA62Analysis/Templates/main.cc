#include <getopt.h>
#include <iostream>
#include <signal.h>
#include <stdlib.h>

#include <TString.h>
#include <TApplication.h>

#include "BaseAnalysis.hh"
#include "Verbose.hh"
#include "ConfigSettings.hh"
#include "Misc.hh"
#include "NA62ConditionsService.hh"
#include "NA62Exceptions.hh"
#include "NA62Global.hh"

$$ANALYZERSINCLUDE$$

NA62Analysis::Core::BaseAnalysis *ban = 0;
TApplication *theApp = 0;
using namespace std;

void PrintUserParameters(){
/*$$ANALYZERSHELP$$*/
/*$$ANALYZERSDELETE$$*/
}

void terminate(int exitCode){
	if(ban){
		ban->WriteOutput();
		ban->Terminate();
	}

	if(theApp) theApp->Terminate();

	if(ban)
		delete ban;

	exit(exitCode);
}

void sighandler(int sig)
{
	cerr << endl << "********************************************************************************" << endl;
	cerr << "Killed with Signal " << sig << endl;
	cerr << endl << "********************************************************************************" << endl;
	cerr << "Bye!" << endl;

	terminate(128+sig);
}

int doRun(bool graphicMode, bool flContinuousReading){
	using namespace Configuration;


	bool retCode = false;

	ban = new NA62Analysis::Core::BaseAnalysis();
	ban->SetGlobalVerbosity(ConfigSettings::global::fCoreVerbosity, ConfigSettings::global::fAnVerbosity);
	if(ConfigSettings::fUseLogFile) ban->SetLogToFile(ConfigSettings::CLI::fLogFile);
	ban->SetGraphicMode(graphicMode);
	ban->SetDownscaling(ConfigSettings::fUseDownscaling);
	if(ConfigSettings::CLI::fHistoMode) ban->SetReadType(NA62Analysis::Core::IOHandlerType::kHISTO);
	else ban->SetReadType(NA62Analysis::Core::IOHandlerType::kTREE);
	if(ConfigSettings::fUsePrimitiveFile) ban->InitPrimitives();
	if(ConfigSettings::CLI::fFastStart) ban->SetFastStart(true);
	if(ConfigSettings::CLI::fSkipIsFatal) ban->SetSkipIsFatal(true);
	if(flContinuousReading) ban->SetContinuousReading(true);
	if(ConfigSettings::CLI::fFilter) {
		ban->SetFiltering(true);
		ConfigSettings::CLI::fNoSkipBadBurst = true;
		ConfigSettings::CLI::fNoCheckEvents = true;
	}
	if(ConfigSettings::CLI::fSpecialOnly) ban->SetSpecialOnly(true);

/*$$FORCENOCHECKDETECTORS$$*/
/*$$FORCENOBADBURSTDETECTORS$$*/
/*$$FORCEPARAMETERS$$*/

	ban->AddInputFiles(ConfigSettings::CLI::fInputFile, ConfigSettings::CLI::fNProcessFiles);

	//DEF_ANALYZER is the ClassName of the analyzer. Defined by Makefile target
/*$$ANALYZERSNEW$$*/

	if(ConfigSettings::fUsePrimitiveFile) ban->SetPrimitiveFile(ConfigSettings::CLI::fPrimitiveFile);
	ban->Init(ConfigSettings::CLI::fOutputFile,
			ConfigSettings::CLI::fParameters,
			ConfigSettings::CLI::fConfigFile,
			ConfigSettings::CLI::fReferenceFile,
			ConfigSettings::CLI::fIgnoreNonExistingTrees);
	if(flContinuousReading) ban->StartContinuous(ConfigSettings::CLI::fInputFile);
	else retCode = ban->Process(ConfigSettings::CLI::fStartEvent,
			ConfigSettings::CLI::fNEvents,
			ConfigSettings::CLI::fNBursts);

	if(graphicMode) theApp->Run();

/*$$ANALYZERSDELETE$$*/
	delete ban;
	return retCode;
}

int main(int argc, char** argv){
	using namespace Configuration;

	// Setting signals
	signal(SIGXCPU, sighandler);
	signal(SIGTERM, sighandler);
	signal(SIGINT, sighandler);
	signal(SIGABRT, sighandler);


	//Reading and setting configuration
	ConfigSettings configurationReader;
	configurationReader.ParseFile(TString(GetUserAnalysisPath()) + TString("/.settingsna62"));
	configurationReader.ParseEnv("NA62ANALYSIS");
	if(!configurationReader.ParseCLI(argc, argv))
		return EXIT_SUCCESS;
	
	if(ConfigSettings::CLI::fMoreHelp){
		PrintUserParameters();
		return EXIT_SUCCESS;
	}

	if(!ConfigSettings::fUseFileList && ConfigSettings::CLI::fNProcessFiles>0){
		cerr << "Option -f/--nfiles can only be used with the -l/--list parameter" << endl;
		return kWrongConfiguration;
	}
	if(TString(ConfigSettings::CLI::fOutputFile).CompareTo(ConfigSettings::CLI::fInputFile)==0){
		cerr << "Input and output files are the same" << endl;
		return kWrongConfiguration;
	}
	if(ConfigSettings::CLI::fInputFile.length()==0){
		std::cerr << "Either of option -i/--input-file or -l/--list must be specified" << std::endl;
		return kWrongConfiguration;
	}

	if(ConfigSettings::CLI::fExternalCDBDirectoryPath.length()>0)
		NA62ConditionsService::GetInstance()->SetExternalCDBDirectoryPath(ConfigSettings::CLI::fExternalCDBDirectoryPath);
	if(ConfigSettings::CLI::fConditionsServiceExitLevel>=0)
		NA62ConditionsService::GetInstance()->SetExitLevel(ConfigSettings::CLI::fConditionsServiceExitLevel);

	bool flContinuousReading = ConfigSettings::CLI::fContinuousReading;
	bool graphicMode = ConfigSettings::CLI::fUseGUI;

	if(flContinuousReading) graphicMode = true;

	if(graphicMode) theApp = new TApplication("NA62Analysis", &argc, argv);

	int retCode = 0;
	try{
		retCode = doRun(graphicMode, flContinuousReading);
	}
	catch (NA62Analysis::Core::LogicException &){
		terminate(kGenericError);
	}
	catch (NA62Analysis::Core::ReadException &){
		terminate(kReadError);
	}
	catch (NA62Analysis::Core::WriteException &){
		terminate(kWriteError);
	}

	return retCode ? 0 : kGenericError;
}
