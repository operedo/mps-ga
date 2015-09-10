#include <stdlib.h>
#include <stdio.h>
#include <smmintrin.h>


int dotsquaresimd_ ( int *len, int *A);

int dotsquaresimd_ ( int *len, int *A)
{
	int i,res;
	__m128i acum = _mm_set1_epi32(0), *a;
  	
	//printf ( "len=%d\n", *len);
	for (i=0; i<*len; i+=4) {
		a = (__m128i*) &A[i];
		//b = (__m128i*) &B[i];
		acum = _mm_add_epi32(acum, _mm_mullo_epi32(*a, *a)); 
	}
	res = _mm_extract_epi32(acum,0) +_mm_extract_epi32(acum,1) + _mm_extract_epi32(acum,2) +_mm_extract_epi32(acum,3);
	
  	//printf ( "res=%d\n", res );
	return res;
}

