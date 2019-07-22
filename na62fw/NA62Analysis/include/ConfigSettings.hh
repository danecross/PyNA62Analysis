/*
 * SettingsReader.hh
 *
 *  Created on: Mar 19, 2015
 *      Author: ncl
 */

#ifndef SETTINGSREADER_HH_
#define SETTINGSREADER_HH_

#include <set>

#include "ConfigParser.hh"
#include "FWEnums.hh"

namespace NA62Analysis {
namespace Configuration {

/// \class ConfigSettings
/// \Brief
/// Read settings file for the framework
/// \EndBrief
/// \Detailed
/// The settings file must be in the format
/// \code
/// [Global]
/// UseColors = true|false
/// ProcessOutputNewLine = true|false
/// SkippedName = NA62Analysis
/// EventsDownscaling = integer
/// MemoryReportEventInterval = integer
/// MemoryReportTimeInterval = integer
/// MetadataPath = /path/to/metadata/directory
/// DefaultAutoUpdateRate = 1000
/// DateTimeFormat = %d/%m/%y %H:%M:%S
/// OutputPrefixFormat = [%n] # Old default: [%d] %l - [%n]
/// BasketSize = integer
/// AutoFlush = integer
/// Verbosity = integer
/// NonExistingWarning = true|false
/// \endcode
/// The currently available settings for NA62Analysis are<br>
/// UseColors: If true the terminal output can use colors.<br>
/// ProcessOutputNewLine : If true the "Processing event xxx/xxx => xx.xx% ... " lines end with
/// an end line. Else a carriage return is used and the line is erased by the next terminal output.<br>
/// SkippedName: Name for the output file containing the list of skipped input files. The file is named
/// SkippedName.skipped<br>
/// EventsDownscaling: Process only 1 events out of EventsDownscaling. This option is activated
/// with the -d/--downscaling option<br>
/// MemoryReportEventInterval: Number of events between memory usage checks<br>
/// MemoryReportTimeInterval: Number of seconds between memory usage checks<br>
/// DefaultAutoUpdateRate: AutoUpdate rate are for histogram with autoupdate activated, or
/// in OnlineMonitor mode. It is the (default) frequency of update of the histogram display. It can
/// be overwritten by analyzer parameter or directly within the analyzer.<br>
/// DateTimeFormat: Date and time display format in the terminal output. The output of the date<br>
/// OutputPrefixFormat: Format of the verbosity output message. %d: date (according to datatimeformat), %l verbosity level, %n module name<br>
/// can be disabled by setting it as - (minus sign)<br>
/// BasketSize: Size of the basket for filter trees<br>
/// AutoFlush: Value for the autoflush of filter trees<br>
/// Verbosity: Verbosity level<br>
/// NonExistingWarning = Display warning when trying to use a non existing object
/// \EndDetailed

class ConfigSettings : public ConfigParser{
public:
	ConfigSettings();
	virtual ~ConfigSettings();

	void ParseFile(TString const& fileName);
	void ParseEnv(TString const& prefix);
	bool ParseCLI(int argc, char** argv);

	static void SetNoSkipBadBurst(bool val) { CLI::fNoSkipBadBurst = val; }

	/// \struct global
	/// \Brief
	/// Structure for the global settings. These can be set by config file and environment variables.
	/// \EndBrief
	///
	/// \Detailed
	/// Structure containing the settings from the "Global" section in the settings file.
	/// \EndDetailed
	struct global {
		// Visual look
		static bool        fUseColors;            ///< Can use colors in the output
		static bool        fProcessOutputNewLine; ///< Use \\n or \\r when printing the  "Processing ..." line
		static std::string fDateTimeFormat;       ///< Format to display date and time (- to disable display)
		static std::string fOutputPrefixFormat;   ///< Format of the output prefix

		// Processing control
		static int                               fEventsDownscaling;         ///< Events downscaling. Process only 1 out of fEventsDownscaling
		static int                               fMemoryReportEventInterval; ///< Interval between event based memory usage checks.
		static int                               fMemoryReportTimeInterval;  ///< Interval between time based memory usage checks.static
		static int                               fDefaultAutoUpdateRate;     ///< Default AutoUpdate rate
		static Verbosity::CoreVerbosityLevel     fCoreVerbosity;             ///< Verbosity level
		static Verbosity::AnalyzerVerbosityLevel fAnVerbosity;               ///< Verbosity level
		static bool                              fNonExistingWarning;        ///< Warning for requesting non-existing objects

		// Input/Output control
		static std::string fSkippedName;    ///< Name of the .skipped file
		static int         fBasketSize;     ///< Size of the filter trees baskets (0 means disabled)
		static int         fAutoFlushValue; ///< Value for the autoflush of filter trees (0 means disabled)

		// Path/Names configuration
		static std::string fSvcClass;       ///< Name of the svcClass
		static std::string fMetadataPath;   ///< Path to the metadata dir
	};

	/// \struct CLI
	/// \Brief
	/// Structure for the settings that can be set through command line only.
	/// \EndBrief
	struct CLI {
		// Processing control
		static bool fContinuousReading;                  ///< Continuous reading mode
		static bool fMoreHelp;                           ///< Print help and the parameters of all loaded analyzers
		static int  fNProcessFiles;                      ///< Maximum number of processed files
		static bool fHistoMode;                          ///< Activate histo mode
		static bool fNoUseBadBurst;                      ///< Do not read bad burst database, do not skip bad bursts
		static bool fNoSkipBadBurst;                     ///< Do not skip bad bursts
		static bool fInvertBadBurst;                     ///< Invert skip bad burst logic
		static bool fFastStart;                          ///< Fast start: skip reading file headers, number of events to process is not known
		static bool fSkipIsFatal;                        ///< Skip is fatal: If an input root file cannot be read, interrupt the processing
		static bool fFilter;                             ///< Activate filtering
		static bool fFilterToSlim;                       ///< When activated, filtered output is saved as Slim persistency
		static bool fFilterToFull;                       ///< When activated, filtered output is saved as Full persistency
		static bool fNoCheckEvents;                      ///< Do not activate event quality flag check
		static bool fInvertBadEvent;                     ///< Invert skip bad events logic
		static bool fSpecialOnly;                        ///< Run over special events only
		static bool fIgnoreNonExistingTrees;             ///< Ignore non existing tree
		static int  fStartEvent;                         ///< First event to process
		static int  fNEvents;                            ///< Number of events to process
		static int  fNBursts;                            ///< Number of bursts to process
		static bool fUseGUI;                             ///< Use graphical interface
		static std::string              fParameters;     ///< String with parameters for the analyzers
		static std::set<std::string> fNoBadBurstSystems; ///< List of systems not to check for bad burst
		static std::set<std::string> fNoCheckSystems;    ///< List of systems not to check for bad events

		// Input/Output control
		static std::string fInputFile;                   ///< Path to input file (ROOT file or list of ROOT files)
		static std::string fLogFile;                     ///< Name of log file to redirect output
		static std::string fPrimitiveFile;               ///< Path to primitive file
		static std::string fOutputFile;                  ///< Name of output ROOT file
		static std::string fConfigFile;                  ///< Path to a config file to read with parameters for the analyzers
		static std::string fReferenceFile;               ///< Path to a reference file
		static std::string fExternalCDBDirectoryPath;    ///< Path to an external CDB directory
		static int fConditionsServiceExitLevel;          ///< ConditionsService exit level
	};

	//Flags to activate functionalities
	static bool fUseFileList;      ///< Input file is a list
	static bool fUseLogFile;       ///< Redirect output to log file
	static bool fUseDownscaling;   ///< Use downscaling (read one event every fEventsDownscaling)
	static bool fUsePrimitiveFile; ///< Use a primitive file

	/// \struct SAVMAtchingConfig
	/// \Brief
	/// Structure for the settings of the SAVMatching class.
	/// \EndBrief
	///
	/// \Detailed
	/// Structure containing the settings from the "SAVMatching section" in the settings file.
	/// \EndDetailed
	struct SAVMatchingConfig{
		static std::string fSAVMatchingIODirectoryPath; ///< Path to the IO directory for SAVMatching
	};
};

} /* namespace Configuration */
} /* namespace NA62Analysis */

#endif /* SETTINGSREADER_HH_ */
