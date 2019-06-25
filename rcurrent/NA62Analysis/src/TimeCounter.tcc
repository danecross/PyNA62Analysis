/*
 * TimeCounter.cc
 *
 *  Created on: Apr 15, 2015
 *      Author: ncl
 */

#include <TimeCounter.hh>
#include <iostream>

namespace NA62Analysis {

template <> TimeOfDay TimeCounter_us::fGetTime = TimeOfDay();
template <> ClockGetTime<CLOCK_MONOTONIC_RAW> TimeCounter_ns::fGetTime = ClockGetTime<CLOCK_MONOTONIC_RAW>();

template <typename clockOp, typename TimeStruct, typename SubSecond>
TimeCounter<clockOp, TimeStruct, SubSecond>::TimeCounter() :
	fIsRunning(0),
	fTotalTime(0),
	fStartTime()
{
	/// \MemberDescr
	/// Default constructor
	/// \EndMemberDescr
}

template <typename clockOp, typename TimeStruct, typename SubSecond>
TimeCounter<clockOp, TimeStruct, SubSecond>::TimeCounter(bool startNow) :
	fIsRunning(1),
	fTotalTime(0)
{
	/// \MemberDescr
	/// \param startNow : if true, start the counter immediately
	///
	/// Start constructor.
	/// \EndMemberDescr

	if(startNow) fGetTime(&fStartTime.s);
}

template <typename clockOp, typename TimeStruct, typename SubSecond>
TimeCounter<clockOp, TimeStruct, SubSecond>::~TimeCounter() {
	/// \MemberDescr
	/// Default destructor
	/// \EndMemberDescr
}

template <typename clockOp, typename TimeStruct, typename SubSecond>
void TimeCounter<clockOp, TimeStruct, SubSecond>::Start() {
	/// \MemberDescr
	/// Start the counter if not already started
	/// \EndMemberDescr
	if(IncrementStart()){
		fGetTime(&fStartTime.s);
	}
}

template <typename clockOp, typename TimeStruct, typename SubSecond>
void TimeCounter<clockOp, TimeStruct, SubSecond>::Stop() {
	/// \MemberDescr
	/// Stop the counter if this Stop() correspond to the first Start()
	/// \EndMemberDescr
	if(DecrementStart()) fTotalTime += GetTime().g - fStartTime.g;
}

template <typename clockOp, typename TimeStruct, typename SubSecond>
void TimeCounter<clockOp, TimeStruct, SubSecond>::Reset() {
	/// \MemberDescr
	/// Reset the counter to 0
	/// \EndMemberDescr
	fTotalTime = 0;
	fIsRunning = 0;
}

template <typename clockOp, typename TimeStruct, typename SubSecond>
bool TimeCounter<clockOp, TimeStruct, SubSecond>::IncrementStart() {
	/// \MemberDescr
	/// Increment the count of Start()
	///
	/// \return true if the counter was not running
	/// \EndMemberDescr
	return 0==fIsRunning++;
}

template <typename clockOp, typename TimeStruct, typename SubSecond>
bool TimeCounter<clockOp, TimeStruct, SubSecond>::DecrementStart() {
	/// \MemberDescr
	/// Increment the count of Stop()
	///
	/// \return true if the number of Stop() is equal to the number of Start()
	/// \EndMemberDescr
	fIsRunning--;
	if(fIsRunning<0) {
		fIsRunning=0;
		return false;
	}
	return fIsRunning==0;
}

template <typename clockOp, typename TimeStruct, typename SubSecond>
void TimeCounter<clockOp, TimeStruct, SubSecond>::Print() const {
	/// \MemberDescr
	/// Print of the internal values of the counter
	/// \EndMemberDescr
	std::cout << "Started at: " << fStartTime.g.tv_sec + fStartTime.g.tv_sub/1000000. << std::endl;
	std::cout << "Total time is: " << fTotalTime << std::endl;
	std::cout << "IsRunning is: " << fIsRunning << std::endl;
}

inline float operator-(struct timeval t1, struct timeval t2){
	return (float)(t1.tv_sec-t2.tv_sec) + (t1.tv_usec-t2.tv_usec)/1000000.;
}
inline float operator-(struct TimeCounter<TimeOfDay, timeval, suseconds_t>::TC_Time_t t1, struct TimeCounter<TimeOfDay, timeval, suseconds_t>::TC_Time_t t2){
	return (float)(t1.tv_sec-t2.tv_sec) + (t1.tv_sub-t2.tv_sub)/1000000.;
}

#ifndef __MACH__
inline float operator-(struct TimeCounter<ClockGetTime<CLOCK_MONOTONIC_RAW>, timespec, long>::TC_Time_t t1, struct TimeCounter<ClockGetTime<CLOCK_MONOTONIC_RAW>, timespec, long>::TC_Time_t t2){
	return (float)(t1.tv_sec-t2.tv_sec) + (t1.tv_sub-t2.tv_sub)/1000000000.;
}
#endif
} /* namespace NA62Analysis */
