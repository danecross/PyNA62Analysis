/*
 * containers.hh
 *
 *  Created on: 2 May 2014
 *      Author: ncl
 */

#ifndef CONTAINERS_HH_
#define CONTAINERS_HH_
#include <iostream>
#include <TString.h>
#include <TObjString.h>

/// \file containers.hh
/// \Brief
/// Feature switch
/// \EndBrief
///
/// \Detailed
/// This header switches features used in the code depending if C++11 is enabled or not.\n
/// Currently:\n
/// If C++11 enabled:
/// - Uses std::unordered_map: map providing O(1) random access to elements (implemented with hashtable).\n
///   Provides a ~40% speed improvement on maps (accessing histograms, counters, events, analyzers, ...)
///   compared to std::map.
/// .
/// If C++11 disabled:
/// - Uses std::map: Fallback solution
/// \EndDetailed

#ifdef NA62_C11
#include <unordered_map>

namespace std
{
template<> struct hash<TString>
{
	size_t operator()(const TString& v) const
	{
		/// \MemberDescr
		/// \param v : TString to hash
		///
		/// Hash operation proxy for TString to use it in hashtable.
		/// \EndMemberDescr
		return v.Hash();
	}
};
template<> struct hash<TObjString>
{
	size_t operator()(const TObjString& v) const
	{
		/// \MemberDescr
		/// \param v : TString to hash
		///
		/// Hash operation proxy for TString to use it in hashtable.
		/// \EndMemberDescr
		return v.Hash();
	}
};

template<> struct less<TObjString>
{
	bool operator()(const TObjString& lhs,const TObjString& rhs) const
	{
		return lhs.Compare(&rhs) < 0;
	}
};
}

namespace NA62Analysis {
template <class K, class V>
	struct NA62Map{ typedef std::unordered_map<K,V> type; ///< C++ trick for template typedef.
	};	///< Defines the type of map used in the framework (C++11 enabled)
	template <class K, class V>
	struct NA62MultiMap{ typedef std::unordered_multimap<K,V> type; ///< C++ trick for template typedef.
	};	///< Defines the type of multimap used in the framework (C++11 enabled)
} /* namespace NA62Analysis */
#else


#include <map>
namespace NA62Analysis {
	template <class K, class V>
	struct NA62Map{ typedef std::map<K,V> type; ///< C++ trick for template typedef.
	}; ///< Defines the type of map used in the framework (C++11 disabled)
	template <class K, class V>
	struct NA62MultiMap{ typedef std::multimap<K,V> type; ///< C++ trick for template typedef.
	}; ///< Defines the type of multimap used in the framework (C++11 disabled)
} /* namespace NA62Analysis */

#endif

/// \struct NA62CaseIgnorecomp
/// \Brief
/// Structure for case insensitive string comparison in stl containers
/// \EndBrief
struct NA62CaseIgnorecomp {
    bool operator() (const TString& lhs, const TString& rhs) const {
    	/// \MemberDescr
    	/// \param lhs : Left-hand-side of the comparison operator
    	/// \param rhs : Right-hand-side of the comparison operator
    	/// \return True of lhs is smaller than rhs.
    	///
    	/// Case insensitive smaller than compare of TString.
    	/// \EndMemberDescr
        return lhs.CompareTo(rhs, TString::ECaseCompare::kIgnoreCase) < 0;
    }
};

namespace NA62Analysis {
template <class T>
using Array1D = std::vector<T>;
template <class T>
using Array2D = std::vector<std::vector<T>>;
template <class T>
using Array3D = std::vector<std::vector<std::vector<T>>>;

template <class T>
Array1D<T> createArray1D(size_t size, T defValue){
	return Array1D<T>(size, defValue);
}

template <class T>
Array2D<T> CreateArray2D(Int_t size1, Int_t size2, T defValue){
	return Array2D<T>(size1, Array1D<T>(size2, defValue));
}

template <class T>
Array3D<T> CreateArray3D(Int_t size1, Int_t size2, Int_t size3, T defValue){
	return Array3D<T>(size1, Array2D<T>(size2, Array1D<T>(size3,defValue)));
}
} /* namespace NA62Analysis */

#endif
