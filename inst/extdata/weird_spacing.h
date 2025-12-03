#ifndef WEIRD_SPACING_H
#define WEIRD_SPACING_H

/* Test header with weird spacing and formatting */

// Defines with weird spacing
#define    MAX_VALUE    9999
#define MIN_VALUE     1  

/* Multi-line comment
   with various stuff
   that should be ignored */
   
// Struct with weird formatting
struct   WeirdStruct  {
    int     field1  ;
    double      field2   ;
    char   *   name   ;
};

// Function with weird spacing
int    process   (  int   a  ,   double   b  )  ;

// Pointer function
void*   allocate  ( size_t    size )  ;

struct  NormalStruct  {
  float value;
  int flag;
};

// Multi-param with weird spacing
long   calculate  (  int  x,  int  y  ,  int  z  );

#endif
