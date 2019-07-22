#ifndef __RG_UTILITIES_H_
#define __RG_UTILITIES_H_

#include <stdio.h>





#ifdef __cplusplus
extern "C" {
#endif

/*
 * general function wrappers: *
 *
 *
 */

#ifndef DEBUG_INT_STACK
#define DEBUG_INT_STACK 0
#endif
  

#if( DEBUG_INT_STACK )
#define DBG(_x)	((void)(_x))
#else
#define DBG(_x)	((void)0)
#endif



  int RG_FileExists(const char * aFileName);

  unsigned int binary_to_gray(unsigned int num);
  unsigned int gray_to_binary(unsigned int num);
  unsigned int ones_count(unsigned int num, unsigned int nbits);
  
  unsigned int parity(unsigned int num, unsigned int nbits);

/*
 * error checking and reporting mechanisms. 
 *
 *
 */
#define RG_FORMAT_ERROR_MSG(msg) do{sprintf(msg,"Function: %s at line %d of file %s: %s",__func__, __LINE__,__FILE__, (msg));}while(0)
#define RG_REPORT_ERROR_MSG(str) do{fprintf(stderr, "RG_ERROR: File = %s, function = %s, line %d: Message = %s \n", __FILE__,__func__,__LINE__,(str));}while(0)


#define RG_CHECK_PTR_RETURN_NULL(ptr) do{if(NULL==(ptr)){fprintf(stderr, "RG_ERROR: NULL pointer encounter in function %s (file %s : %d). This could have been passed ass an argument, or the result of some operation.\n",__func__,__FILE__,__LINE__);return NULL;}}while(0)

#define RG_CHECK_PTR_RETURN_INT(ptr, retval) do{if(NULL==(ptr)){fprintf(stderr, "RG_ERROR: NULL pointer encountered in function %s (file %s : %d). This could have been passed ass an argument, or the result of some operation.\n",__func__,__FILE__,__LINE__);return(retval);}}while(0)

#define RG_CHECK_PTR_RETURN_VOID(ptr) do{if(NULL==(ptr)){fprintf(stderr, "RG_ERROR: NULL pointer encountered in function %s (file %s : %d). This could have been passed ass an argument, or the result of some operation\n",__func__,__FILE__,__LINE__);return;}}while(0)




#define RG_CHECK_ZERO_RETURN(retval, usr_msg) do{if(0!=(retval)){fprintf(stderr, "RG_ERROR: Non-zero return value received (%d). Message = %s.(func %s file %s : %d)\n", retval, usr_msg, __func__,__FILE__,__LINE__);return(retval);}}while(0)




#define RG_REPORT_ERROR_MSG(str) do{fprintf(stderr, "RG_ERROR: File = %s, function = %s, line %d: Message = %s \n", __FILE__,__func__,__LINE__,(str));}while(0)


#define RG_REPORT_ERROR_MSG2(str1, str2) do{fprintf(stderr, "RG_ERROR: File = %s, function = %s, line %d: Message = %s %s \n", __FILE__,__func__,__LINE__,(str1),(str2));}while(0)


#define RG_REPORT_WARNING_MSG(str) do{fprintf(stderr, "RG_WARN: File = %s, function = %s, line %d: Message = %s \n", __FILE__,__func__,__LINE__,(str));}while(0)
#define RG_REPORT_WARNING_MSG2(str1, str2) do{fprintf(stderr, "RG_WARN: File = %s, function = %s, line %d: Message = %s %s \n", __FILE__,__func__,__LINE__,(str1),(str2));}while(0)



#ifdef  __cplusplus
}
#endif




#endif 



