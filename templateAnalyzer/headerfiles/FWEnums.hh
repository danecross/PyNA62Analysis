#ifndef FWENUMS_HH
#define FWENUMS_HH

namespace NA62Analysis{
namespace Verbosity{
// Always is always printed, whatever the verbosity level
// Normal/UserNormal is the default output (same as standard, but standard suppresses the prefix with time and module name):
//   Should be printing everything rhe framework does rarely (such as initialization), and non-recurrent error/warnings
//   messages (arising typically when user does something wrong).
// Extended/User:
//   Print all the other occasional messages that users might want to see, but not by default. Definitely no information
//   that will be printed for all event
// Debug:
//   Print detailed debugging information that user do not usually want to see when running normally. Will be used
//   when it is needed to understand roughly what the framework is doing (typically debug but exclude messages from within loop).
//   Also include messages printed for all events
// Trace:
//   Highest level of printing. Print everything, especially content of loops and things that gets printed many times/event
//
// Core levels should be used in the framework core.
// Analyzer levels should be used in all analyzers.
enum CoreVerbosityLevel {kAlways, kNormal, kExtended, kDebug, kTrace, kCDisable};
enum AnalyzerVerbosityLevel {kUserAlways, kUserNormal, kUser, kUDisable};
} /* namespace Verbosity */
} /* namespace NA62Analysis */

#endif
