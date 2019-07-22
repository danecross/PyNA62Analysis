// Basic Check functions
#define CheckPtr(ptr) do{if(NULL==(ptr)){fprintf(stderr, "ERROR: NULL pointer encountered in function %s (file %s : %d).\n",__func__,__FILE__,__LINE__);}}while(0)
#define CheckPtrRVone(ptr) do{if(NULL==(ptr)){fprintf(stderr, "ERROR: NULL pointer encountered in function %s (file %s : %d).\n",__func__,__FILE__,__LINE__);return int(1);}}while(0)
#define CheckPtrVoid(ptr)  do{if(NULL==(ptr)){fprintf(stderr, "ERROR: NULL pointer encountered in function %s (file %s : %d).\n",__func__,__FILE__,__LINE__);return;}}while(0)
#define CheckRVNULL(rv)    do{if(0!=(rv)){fprintf(stderr, "ERROR: non null return value in function %s (file %s : %d).\n",__func__,__FILE__,__LINE__);return int(rv);}}while(0)
#define CheckRVVoid(rv)    do{if(0!=(rv)){fprintf(stderr, "ERROR: non null return value in function %s (file %s : %d).\n",__func__,__FILE__,__LINE__);return   ;}}while(0)


