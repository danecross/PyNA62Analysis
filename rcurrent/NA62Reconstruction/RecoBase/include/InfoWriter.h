#ifndef WARN_EVT
#define WARN_EVT 1
#endif
#ifndef WARN_DET
#define WARN_DET 2
#endif
#ifndef WARN_MAX
#define WARN_MAX 3
#endif

#define cout_en(x,y) if((x>=y)) std::cout
#define cerr_en(x,y) if((x>=y)) std::cerr
