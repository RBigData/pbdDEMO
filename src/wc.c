#include <R.h>
#include <Rdefines.h>
#include <stdlib.h>
#include <stdio.h>

#define BUFLEN 1024
#define CHARPT(x,i)     ((char*)CHAR(STRING_ELT(x,i)))

// wc -l basically
SEXP pbddemo_linecount(SEXP file)
{
  SEXP ret;
  int i;
  int nlines = 0;
  size_t readsize;
  char *buf = malloc(BUFLEN);
  FILE *fp = fopen(CHARPT(file, 0), "r");
  
  while (1)
  {
    readsize = fread(buf, 1, BUFLEN, fp);
    
    for (i=0; i<readsize; i++)
    {
      if (buf[i] == '\n')
        nlines++;
    }
    
    if (readsize < BUFLEN)
      break;
  }
  
  fclose(fp);
  free(buf);
  
  PROTECT(ret = allocVector(INTSXP, 1));
  INTEGER(ret)[0] = nlines;
  UNPROTECT(1);
  
  return ret;
}

