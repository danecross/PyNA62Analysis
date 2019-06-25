/*
 * TimeCounter.h
 *
 *  Created on: Apr 15, 2015
 *      Author: ncl
 */

#ifndef TIMECOUNTER_H_
#define TIMECOUNTER_H_

#include <sys/time.h>
#include <time.h>
namespace NA62Analysis {

/*inline float operator-(struct timeval t1, struct timeval t2){
	return (float)(t1.tv_sec-t2.tv_sec) + (t1.tv_usec-t2.tv_usec)/1000000.;
}*/

/// \class TimeCounter
/// \Brief
/// Time counter (stop watch)
/// \EndBrief
///
/// \Detailed
/// This class provides a basic time counter. It starts running when Start()
/// or the constructor TimeCounter(clock_t s) is called and stops when as many
/// Stop() have been called.\n
/// Example:\n
/// The following code will count the time elapsed between lines 2 and 7. Lines 3,4,5,6 and 8 have no effect.
/// \code
/// 1: TimeCounter tc;
/// 2: tc.Start();
/// 3: tc.Start();
/// 4: tc.Start();
/// 5: tc.Stop();
/// 6: tc.Stop();
/// 7: tc.Stop();
/// 8: tc.Stop();
/// \endcode
/// \EndDetailed


template <typename clockOp, typename TimeStruct, typename SubSecond>
class TimeCounter {
public:
	/// \struct TC_Time_t
	/// \Brief
	/// Contains time in sec and sub-second
	/// \EndBrief
	struct TC_Time_t{
		time_t tv_sec;
		SubSecond tv_sub;
	};
	/// \union TC_Time
	/// \Brief
	/// Union for time structure (depending on which precision is required)
	/// \EndBrief
	union TC_Time{
		TimeStruct s;
		TC_Time_t g;
	};
public:
	TimeCounter();
	explicit TimeCounter(bool startNow);
	virtual ~TimeCounter();

	void Start();
	void Stop();
	void Reset();

	void AddTime(float v) {
		/// \MemberDescr
		/// \param v: Amount of time to add
		///
		/// Add time to the total time.
		/// \EndMemberDescr
		fTotalTime+=v;
	};

	float GetTotalTime() const {
		/// \MemberDescr
		/// \return Total accumulated time up to now if still running, else total accumulated time up to last Stop()
		/// \EndMemberDescr
		if(fIsRunning>0) return fTotalTime + (GetTime().g-fStartTime.g);
		else return fTotalTime;
	}

	TC_Time GetStartTime() const {
		/// \MemberDescr
		/// \return Timestamp when the counter started running
		/// \EndMemberDescr
		return fStartTime;
	}

	void Print() const;

	static TC_Time GetTime(){
		/// \MemberDescr
		/// \return timeval struct representing the current time
		/// \EndMemberDescr

		TC_Time s_time;
		fGetTime(&s_time.s);
		return s_time;
	}
private:
	bool IncrementStart();
	bool DecrementStart();

	int fIsRunning; ///< Indicate how many Start() were requested without Stop() (# Start() - # Stop())
	float fTotalTime; ///< Total accumulated time between all Start() and Stop()
	TC_Time fStartTime; ///< Timestamp when the counter started running
	static clockOp fGetTime;
};



/* This section declares counters for gettimeofday function
 * - We need a functor to gettimeofday
 * - We need the difference operator for both the generic union member and the
 *   specialized union member
 * - We need declaration of the specialized class
 * - We need typedef to the specialized class (TimeCounter_us)
 */
/// \struct TimeOfDay
/// \Brief
/// Functor for gettimeofday
/// \EndBrief
struct TimeOfDay{
	int operator()(timeval* time_struct) { return gettimeofday(time_struct, nullptr);};
};

float operator-(struct TimeCounter<TimeOfDay, timeval, suseconds_t>::TC_Time_t t1, struct TimeCounter<TimeOfDay, timeval, suseconds_t>::TC_Time_t t2);
inline float operator-(TimeCounter<TimeOfDay, timeval, suseconds_t>::TC_Time t1, TimeCounter<TimeOfDay, timeval, suseconds_t>::TC_Time t2){
	return (t1.g-t2.g);
}

template class TimeCounter<TimeOfDay, timeval, suseconds_t>;
typedef TimeCounter<TimeOfDay, timeval, suseconds_t> TimeCounter_us;

//#define _POSIX_TIMERS 1
#ifndef __MACH__
/* This section declares counters for clock_gettime functions
 * These functions are available only on linux, not on MAC
 * - We need a functor to clock_gettime
 * - We need the difference operator for both the generic union member and the
 *   specialized union member
 * - We need declaration of the specialized class
 * - We need typedef to the specialized class (TimeCounter_us, TimeCounter_ns)
 */
/// \struct ClockGetTime
/// \Brief
/// Functor for clock_gettime
/// \EndBrief
template <clockid_t CLOCK_ID>
struct ClockGetTime{
	int operator()(timespec *time_struct) { return clock_gettime(CLOCK_ID, time_struct);}
};

float operator-(struct TimeCounter<ClockGetTime<CLOCK_MONOTONIC_RAW>, timespec, long>::TC_Time_t t1, struct TimeCounter<ClockGetTime<CLOCK_MONOTONIC_RAW>, timespec, long>::TC_Time_t t2);
inline float operator-(TimeCounter<ClockGetTime<CLOCK_MONOTONIC_RAW>, timespec, long>::TC_Time t1, TimeCounter<ClockGetTime<CLOCK_MONOTONIC_RAW>, timespec, long>::TC_Time t2){
	return (t1.g-t2.g);
}

template class TimeCounter<ClockGetTime<CLOCK_MONOTONIC_RAW>, timespec, long>;
typedef TimeCounter<ClockGetTime<CLOCK_MONOTONIC_RAW>, timespec, long> TimeCounter_ns;

#endif

} /* namespace NA62Analysis */

#include "../src/TimeCounter.tcc"


#endif /* TIMECOUNTER_H_ */
