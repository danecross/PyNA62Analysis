/*
 * TemplateFunctions.hh
 *
 *  Created on: 22 Nov 2016
 *      Author: ncl
 */

#ifndef INCLUDE_TEMPLATEFUNCTIONS_HH_
#define INCLUDE_TEMPLATEFUNCTIONS_HH_



template <typename T, typename U>
bool all_equal(const T &t, const U &u) {
    return t == u;
}

template <typename T, typename U, typename... Others>
bool all_equal(const T &t, const U &u, Others const &... args) {
    return (t == u) && all_equal(u, args...);
}


#endif /* INCLUDE_TEMPLATEFUNCTIONS_HH_ */
