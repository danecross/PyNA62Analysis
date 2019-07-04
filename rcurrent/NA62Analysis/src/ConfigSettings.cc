/*
 * ConfigSettings.cc
 *
 *  Created on: Mar 19, 2015
 *      Author: ncl
 */

#include <ConfigSettings.hh>

#include <iostream>
#include <boost/program_options.hpp>
#include <TObjArray.h>
#include "NA62Exceptions.hh"
#include "Misc.hh"
#include "Verbose.hh"

namespace NA62Analysis {
namespace Configuration {

ConfigSettings::ConfigSettings() {
	/// \MemberDescr
	/// Default constructor
	/// \EndMemberDescr
}

ConfigSettings::~ConfigSettings() {
	/// \MemberDescr
	/// Default destructor
	/// \EndMemberDescr
}

void NotifyNoCheck(std::string const& system) {
	if(system.compare("all")==0)
		ConfigSettings::CLI::fNoCheckEvents = true;
	ConfigSettings::CLI::fNoCheckSystems.insert(system);
}

void NotifyNoBadBurst(std::string const& system) {
	if(system.compare("all")==0)
		ConfigSettings::CLI::fNoSkipBadBurst = true;
	ConfigSettings::CLI::fNoBadBurstSystems.insert(system);
}

void ConfigSettings::ParseFile(TString const& fileName) {
	/// \MemberDescr
	/// \param fileName : Path to the settings file to parse
	///
	/// Parse a settings file and fill the settings structure.
	/// \EndMemberDescr

	ConfigParser::ParseFile(fileName);

	if(NamespaceExists("global")){
		ConfigNamespace ns = GetNamespace("global");
		ns.SetValue("usecolors",                 global::fUseColors);
		ns.SetValue("processoutputnewline",      global::fProcessOutputNewLine);
		ns.SetValue("skippedname",               global::fSkippedName);
		ns.SetValue("svcclass",                  global::fSvcClass);
		ns.SetValue("memoryreporteventinterval", global::fMemoryReportEventInterval);
		ns.SetValue("memoryreporttimeinterval",  global::fMemoryReportTimeInterval);
		ns.SetValue("metadatapath",              global::fMetadataPath);
		ns.SetValue("defaultautoupdaterate",     global::fDefaultAutoUpdateRate);
		ns.SetValue("datetimeformat",            global::fDateTimeFormat);
		ns.SetValue("outputprefixformat",        global::fOutputPrefixFormat);
		ns.SetValue("basketsize",                global::fBasketSize);
		ns.SetValue("autoflush",                 global::fAutoFlushValue);
		ns.SetValue("nonexistingwarning",        global::fNonExistingWarning);
		int verb = -1;
		ns.SetValue("verbosity",                 verb);

		if(verb!=-1){
		    global::fCoreVerbosity = Verbose::GetCoreVerbosityLevelFromInt(verb);
		    global::fAnVerbosity = Verbose::GetAnalyzerVerbosityLevelFromInt(verb);
		}
	}
}

void ConfigSettings::ParseEnv(TString const& prefix) {
	/// \MemberDescr
	/// \param prefix: Prefix of the environment variables to parse
	///
	/// Parse the environment variables for the GLOBAL namespace
	/// \EndMemberDescr
	///
	ConfigParser::ParseEnv(prefix);

	if(NamespaceExists("global")){
		ConfigNamespace ns = GetNamespace("global");
		ns.SetValue("usecolors",                 global::fUseColors);
		ns.SetValue("processoutputnewline",      global::fProcessOutputNewLine);
		ns.SetValue("skippedname",               global::fSkippedName);
		ns.SetValue("svcclass",                  global::fSvcClass);
		ns.SetValue("memoryreporteventinterval", global::fMemoryReportEventInterval);
		ns.SetValue("memoryreporttimeinterval",  global::fMemoryReportTimeInterval);
		ns.SetValue("metadatapath",              global::fMetadataPath);
		ns.SetValue("defaultautoupdaterate",     global::fDefaultAutoUpdateRate);
		ns.SetValue("datetimeformat",            global::fDateTimeFormat);
		ns.SetValue("outputprefixformat",        global::fOutputPrefixFormat);
		ns.SetValue("basketsize",                global::fBasketSize);
		ns.SetValue("autoflush",                 global::fAutoFlushValue);
		ns.SetValue("nonexistingwarning",        global::fNonExistingWarning);
		int verb = -1;
		ns.SetValue("verbosity",                 verb);

		if(verb!=-1){
		    global::fCoreVerbosity = Verbose::GetCoreVerbosityLevelFromInt(verb);
		    global::fAnVerbosity = Verbose::GetAnalyzerVerbosityLevelFromInt(verb);
		}
	}
}

bool ConfigSettings::ParseCLI(int argc, char**argv){
	/// \MemberDescr
	/// \param argc: Number of arguments
	/// \param argv: Array of arguments
	///
	/// Parse the command line arguments and fill the configuration variables
	/// \EndMemberDescr

	namespace po = boost::program_options;

	//General options
	po::options_description globalOptions("Processing control options");

	//Add CLI parameters
	globalOptions.add_options()
		// Help
		("help,h",
			"Display this help")
		("more-help,m",
			po::bool_switch(&CLI::fMoreHelp),
			"Display this help and the list of parameters for loaded analyzers")
		// Input/Output files
		("prim",
			po::value(      &CLI::fPrimitiveFile),
			"Path to a primitive ROOT file")
		("output,o",
			po::value(      &CLI::fOutputFile)->default_value("outFile.root"),
			"Path to output ROOT file. Will be overwritten if already exists")
		("config",
			po::value(      &CLI::fConfigFile),
			"Path to a configuration file containing analyzers parameters")
		("reffile",
			po::value(      &CLI::fReferenceFile),
			"Path to a ROOT file containing reference plots")
		("logtofile",
			po::value(      &CLI::fLogFile),
			"Write the log output to the specified file instead of standard output")
		("conditions,C",
			po::value(      &CLI::fExternalCDBDirectoryPath),
			"Custom path for the Conditions DB (CDB)")
		("exitlevel,e",
			po::value(      &CLI::fConditionsServiceExitLevel),
			"ConditionsService exit level")

		// Control number of input
		("nevt,n",
			po::value(      &CLI::fNEvents)->default_value(-1),
			"Maximum number of events to process (-1= All)")
		("nburst,B",
			po::value(      &CLI::fNBursts)->default_value(-1),
			"Maximum number of bursts to process (-1= All)")
		("start",
			po::value(      &CLI::fStartEvent)->default_value(0),
			"Index of the first event to process.\nEvent index starts at 0")

		// Parameters
		("params,p",
			po::value(      &CLI::fParameters),
			"List of parameters to pass to analyzers.\n"
			"The format of the string is \"analyzerName:param=val;param=val&analyzerName:param=val&...\"")

		// Running mode
		("histo",
			po::bool_switch(&CLI::fHistoMode),
			"Read histograms only and bypass TTree reading")
		("special-only",
			po::bool_switch(&CLI::fSpecialOnly),
			"Process only special trigger tree")
		("gui,g",
			po::bool_switch(&CLI::fUseGUI),
			"Graphical mode. Starts a ROOT application for display.\n"
			"Do not automatically exit at the end of the processing, Ctrl-C to exit")
        ("filter",
            po::bool_switch(&CLI::fFilter),
            "Enable the filtering of events (disables bad burst skipping and bad event checking)")
        ("filterslim",
            po::bool_switch(&CLI::fFilterToSlim),
            "Enable the filtering of events and save output in Slim persistency (disables bad burst skipping and bad event checking)")
        ("filterfull",
            po::bool_switch(&CLI::fFilterToFull),
            "Enable the filtering of events and save output in Full persistency (disables bad burst skipping and bad event checking)")

		// Processing control
		("fast-start",
			po::bool_switch(&CLI::fFastStart),
			"Start processing immediately without reading input files headers.\n"
			"Can be useful on CASTOR but total number of events is not known a priori")
		("stop-on-skip",
			po::bool_switch(&CLI::fSkipIsFatal),
			"Stop processing if an input file root cannot be read.")
		("ignore",
			po::bool_switch(&CLI::fIgnoreNonExistingTrees),
			"Ignore non-existing trees and continue processing")
		("no-use-badburst",
			po::bool_switch(&CLI::fNoUseBadBurst),
			"Do not read the bad burst database, do not skip bad bursts")
	        ("invert-badburst",
			po::bool_switch(&CLI::fInvertBadBurst),
			"Process only bad bursts. Skip good bursts")
		("invert-badevent",
			po::bool_switch(&CLI::fInvertBadEvent),
			"Process only bad events. Skip good events")
	;

	// Detector check disable
	std::vector<std::string> systems{"all", "Cedar", "CHANTI", "CHOD", "GTK", "HAC", "IRC", "L0", "L1", "LAV", "LKr", "MUV0", "MUV1", "MUV2", "MUV3", "NewCHOD", "RICH", "SAC", "SAV", "Straw"};
	for(auto s : systems){
		globalOptions.add_options()( ("no-check-badevent-" + s).c_str(),
			po::value<std::string>()->implicit_value(s)->zero_tokens()->notifier(&NotifyNoCheck),
			("Do not run bad event checks for " + s).c_str());
	}

	// BadBurst check disable
	std::vector<std::string> systemsBB{"all", "Cedar", "CHANTI", "CHOD", "DIM", "GTK", "HAC", "IRC", "L0Calo", "L0CHOD", "L0LAV", "L0MUV3", "L0NewCHOD", "L0RICH", "L0TALK", "L0TP", "L1TP", "L2EB", "LAV", "LKr", "MUV0", "MUV1", "MUV2", "MUV3", "NewCHOD", "Processing", "RICH", "SAC", "SAV", "Straw"};
	for(auto s : systemsBB){
		globalOptions.add_options()( ("no-check-badburst-" + s).c_str(),
			po::value<std::string>()->implicit_value(s)->zero_tokens()->notifier(&NotifyNoBadBurst),
			("Do not run bad burst checks for " + s).c_str());
	}

	//Add global parameters
	int verbosity = 11;
	globalOptions.add_options()
		("verbose,v",
			po::value(&verbosity)->default_value(11)->implicit_value(22),
			"Verbosity level mask.\n"
			"Input has two possible formats:\n"
			"ac where a is the analyzer verbosity level (0-2) and c is the core verbosity level (0-4).\n"
			"a|c where a is the analyzer verbosity level (kUserAlways, kUserNormal, kUser) and \n"
			"c is the core verbosity level (kNormal, kExtended, kDebug, kTrace)"
			"Default=kUserNormal|kNormal (11); if level not specified: kUser|kExtended")
		("downscaling,d",
			po::value(&global::fEventsDownscaling)->implicit_value(global::fEventsDownscaling),
			"Activate downscaling")
		("autoflush",
			po::value(&global::fAutoFlushValue)->default_value(-10000),
			"Autoflush value for ROOT")
		("basketsize",
			po::value(&global::fBasketSize)->default_value(0),
			"TTree basket size for ROOT")
	;

	//Group 1 options (file processing)
	po::options_description group1("Single file processing");
	group1.add_options()
		("input-file,i",
			po::value(      &CLI::fInputFile),
			"Path to an input ROOT file")
	;

	//Group 2 options (list processing)
	po::options_description group2("File list processing");
	group2.add_options()
		("list,l",
			po::value(      &CLI::fInputFile),
			"Path to a text file containing a list of paths to input ROOT files.\nOne file per line")
		("continuous",
			po::bool_switch(&CLI::fContinuousReading),
			"Use continuous reading (automatically enables -g)")
		("nfiles,f",
			po::value(      &CLI::fNProcessFiles)->default_value(-1),
			"Maximum number of files to process from the list. (-1= All)")
	;

	po::options_description all("Allowed options");
	all.add(globalOptions).add(group1).add(group2);

	// Process arguments
	po::variables_map vm;
	po::store(po::command_line_parser(argc, argv).options(all).style(po::command_line_style::unix_style|po::command_line_style::case_insensitive).run(), vm);
	po::notify(vm);

	// Print help
	if (vm.count("help") || vm["more-help"].as<bool>()) {
	    std::cout << globalOptions << std::endl;
	    std::cout << "Mutually exclusive option groups:" << std::endl << group1 << std::endl << group2 << "\n";
	    return vm["more-help"].as<bool>();
	}

	if (vm.count("list"))
		fUseFileList = true;
	if (vm.count("logtofile"))
		fUseLogFile = true;
	if (vm.count("downscaling"))
		fUseDownscaling = true;
	if (vm.count("input-file"))
		CLI::fNProcessFiles = 0;
	if (vm.count("prim"))
		fUsePrimitiveFile = true;

	if (CLI::fFilterToSlim)
	    CLI::fFilter = true;
	if (CLI::fFilterToFull)
	    CLI::fFilter = true;

	if((verbosity/10)>(Verbosity::kUDisable-1) || (verbosity%10)>(Verbosity::kCDisable-1)){
		std::cerr << "Verbosity level " << verbosity << " is not allowed." << std::endl;
		throw NA62Analysis::Core::LogicException();
	}
	global::fCoreVerbosity = Verbose::GetCoreVerbosityLevelFromInt(verbosity);
	global::fAnVerbosity = Verbose::GetAnalyzerVerbosityLevelFromInt(verbosity);


	if(vm.count("input-file") && vm.count("list")){
		std::cerr << "Option -i/--input-file and -l/--list cannot be used together" << std::endl;
		throw NA62Analysis::Core::LogicException();
	}

	return true;
}

// Initialize all static members

// Functionality flags
bool ConfigSettings::fUseFileList      = false;
bool ConfigSettings::fUseLogFile       = false;
bool ConfigSettings::fUseDownscaling   = false;
bool ConfigSettings::fUsePrimitiveFile = false;

// The following members can be set through config file or environment variable or command line for some
// Visual look
bool        ConfigSettings::global::fUseColors             = true;
bool        ConfigSettings::global::fProcessOutputNewLine  = true;
std::string ConfigSettings::global::fDateTimeFormat = "%d/%m/%y %H:%M:%S";
std::string ConfigSettings::global::fOutputPrefixFormat = "[%n]";

// Processing control
int                               ConfigSettings::global::fEventsDownscaling         = 1000;
int                               ConfigSettings::global::fMemoryReportEventInterval = -1;
int                               ConfigSettings::global::fMemoryReportTimeInterval  = 0;
int                               ConfigSettings::global::fDefaultAutoUpdateRate     = 1000;
Verbosity::CoreVerbosityLevel     ConfigSettings::global::fCoreVerbosity             = Verbosity::kNormal;
Verbosity::AnalyzerVerbosityLevel ConfigSettings::global::fAnVerbosity               = Verbosity::kUserNormal;
bool                              ConfigSettings::global::fNonExistingWarning        = true;

// Input/Output control
std::string ConfigSettings::global::fSkippedName    = "NA62Analysis";
int         ConfigSettings::global::fBasketSize     = 0;
int         ConfigSettings::global::fAutoFlushValue = -10000;

// Path/Names configuration
std::string ConfigSettings::global::fSvcClass         = "na62";
std::string ConfigSettings::global::fMetadataPath     = "/afs/cern.ch/na62/offline/metadata/";

// The following members can be set only through command line
// Processing control
bool ConfigSettings::CLI::fContinuousReading      = false;
bool ConfigSettings::CLI::fMoreHelp               = false;
int  ConfigSettings::CLI::fNProcessFiles          = 0;
bool ConfigSettings::CLI::fHistoMode              = false;
bool ConfigSettings::CLI::fNoUseBadBurst          = false;
bool ConfigSettings::CLI::fNoSkipBadBurst         = false;
bool ConfigSettings::CLI::fInvertBadBurst         = false;
bool ConfigSettings::CLI::fFastStart              = false;
bool ConfigSettings::CLI::fSkipIsFatal            = false;
bool ConfigSettings::CLI::fFilter                 = false;
bool ConfigSettings::CLI::fFilterToSlim           = false;
bool ConfigSettings::CLI::fFilterToFull           = false;
bool ConfigSettings::CLI::fNoCheckEvents          = false;
bool ConfigSettings::CLI::fInvertBadEvent         = false;
bool ConfigSettings::CLI::fSpecialOnly            = false;
bool ConfigSettings::CLI::fIgnoreNonExistingTrees = false;
int  ConfigSettings::CLI::fStartEvent             = -1;
int  ConfigSettings::CLI::fNEvents                = 0;
int  ConfigSettings::CLI::fNBursts                = -1;
bool ConfigSettings::CLI::fUseGUI                 = false;
std::string              ConfigSettings::CLI::fParameters;
std::set<std::string> ConfigSettings::CLI::fNoCheckSystems;
std::set<std::string> ConfigSettings::CLI::fNoBadBurstSystems;

// Input/Output control
std::string ConfigSettings::CLI::fInputFile;
std::string ConfigSettings::CLI::fLogFile;
std::string ConfigSettings::CLI::fPrimitiveFile;
std::string ConfigSettings::CLI::fOutputFile;
std::string ConfigSettings::CLI::fConfigFile;
std::string ConfigSettings::CLI::fReferenceFile;
std::string ConfigSettings::CLI::fExternalCDBDirectoryPath;
int         ConfigSettings::CLI::fConditionsServiceExitLevel=-1;

//SAVMatching tool
std::string ConfigSettings::SAVMatchingConfig::fSAVMatchingIODirectoryPath = GetNA62AnalysisPath() + "/config/SAV_time-corrections/";

} /* namespace Configuration */
} /* namespace NA62Analysis */
