#include <iterator>

#ifndef INCLUDE_INDEXER_HH_
#define INCLUDE_INDEXER_HH_

/// \class Indexer
/// \Brief
/// For any container with iterator, creates a new iterator containing the pair (index,element).
/// \EndBrief
/// \Detailed
/// This is usefull when you want to do a range based loop over the elements of a container, while having the index of the element available.
/// Do not use the class directly, but use the factory function Indexer<T> index(T& t) instead.
/// \n
/// Example:\n
/// \code
/// std::vector<int> myVector = {0,2,4,6,8};
///	for(auto it : index(myVector)){
///		std::cout << it.first << " -> " << it.second << std::endl;
///	}
/// \endcode
/// Output:\n
/// \code
/// 0 -> 0
/// 1 -> 2
/// 2 -> 4
/// 3 -> 6
/// 4 -> 8
/// /// \endcode
/// \EndDetailed


template <typename T>
struct iterator_extractor { typedef typename T::iterator type; };

template <typename T>
struct iterator_extractor<T const> { typedef typename T::const_iterator type; };

template <typename T>
class Indexer {
    public:
        class iterator {
            typedef typename iterator_extractor<T>::type inner_iterator;

            typedef typename std::iterator_traits<inner_iterator>::reference inner_reference;
            public:
            typedef std::pair<size_t, inner_reference> reference;

            explicit iterator(inner_iterator it): _pos(0), _it(it) {}

            reference operator*() const { return reference(_pos, *_it); }

            iterator& operator++() { ++_pos; ++_it; return *this; }
            iterator operator++(int) { iterator tmp(*this); ++*this; return tmp; }

            bool operator==(iterator const& it) const { return _it == it._it; }
            bool operator!=(iterator const& it) const { return !(*this == it); }

            private:
            size_t _pos;
            inner_iterator _it;
        };

        explicit Indexer(T& t): _container(t) {}

        iterator begin() const { return iterator(_container.begin()); }
        iterator end() const { return iterator(_container.end()); }

    private:
        T& _container;
};

// Factory function for Indexer
template <typename T>
Indexer<T> indexer(T& t) { return Indexer<T>(t); }


/// \class skip
/// \Brief
/// Use for range based loop on a container, skipping the first n elements of the container.
/// \EndBrief
/// \Detailed
/// Use for range based loop on a container, skipping the first n elements of the container.
/// \n
/// Example:\n
/// \code
/// std::vector<int> myVector = {1,2,3,4,5};
///	for(auto it : skip<decltype(myVector)>(myVector, 2)){
///		std::cout << it << ", ";
///	}
/// \endcode
/// Output:\n
/// \code
/// 3, 4, 5,
/// \endcode
/// \EndDetailed
template <typename T>
struct skip
{
    T& t;
    std::size_t n;
    skip(T& v, std::size_t s) : t(v), n(s) {}
    auto begin() -> decltype(std::begin(t))
    {
        return std::next(std::begin(t), n);
    }
    auto end() -> decltype(std::end(t))
    {
        return std::end(t);
    }
};

#endif /* INCLUDE_INDEXER_HH_ */
