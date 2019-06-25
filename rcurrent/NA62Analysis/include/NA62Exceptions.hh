/*
 * NA62Exceptions.hh
 *
 *  Created on: Nov 23, 2015
 *      Author: nlurkin
 */

#ifndef INCLUDE_NA62EXCEPTIONS_HH_
#define INCLUDE_NA62EXCEPTIONS_HH_

#include <exception>

namespace NA62Analysis {
namespace Core {

/// \class LogicException
/// \Brief
/// C++ Exception used to abort program.
/// \EndBrief
class LogicException: public std::exception {
public:
	virtual const char* what() const throw () {
		/// \MemberDescr
		/// \return Display text
		///
		/// Exception what
		/// \EndMemberDescr
		return "Aborting execution...\n\t   Run again with -v5 for detailed output";
	}
};

/// \class ReadException
/// \Brief
/// C++ Exception used to abort program on Read issues.
/// \EndBrief
class ReadException: public std::exception {
public:
	virtual const char* what() const throw () {
		/// \MemberDescr
		/// \return Display text
		///
		/// Exception what
		/// \EndMemberDescr
		return "Aborting execution...\n\t   Unable to read input";
	}
};

/// \class WriteException
/// \Brief
/// C++ Exception used to abort program on Write issues.
/// \EndBrief
class WriteException: public std::exception {
public:
	virtual const char* what() const throw () {
		/// \MemberDescr
		/// \return Display text
		///
		/// Exception what
		/// \EndMemberDescr
		return "Aborting execution...\n\t   Unable to write output";
	}
};

} /* namespace Core */
} /* namespace NA62Analysis */

#endif /* INCLUDE_NA62EXCEPTIONS_HH_ */
