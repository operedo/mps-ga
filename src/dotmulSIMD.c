#include <stdlib.h>
#include <stdio.h>
#include <smmintrin.h>


int dotmulsimd_ ( int *len, int *A, int *B);

int dotmulsimd_ ( int *len, int *A, int *B)
{
        //printf("%d %d %d\n",*len,A[0],B[0]);
        
	int i,ac1,ac2,ac3,ac4;
	__m128i acum1 = _mm_set1_epi32(0);
	//__m128i acum2 = _mm_set1_epi32(0);
	//__m128i acum3 = _mm_set1_epi32(0);
	//__m128i acum4 = _mm_set1_epi32(0);
	__m128i *a, *b, res;
  	ac1=0;
  	ac2=0;
  	ac3=0;
  	ac4=0;
	//printf ( "len=%d\n", *len);
	for (i=0; i<*len; i+=4) {
		a = (__m128i*) &A[i];
		b = (__m128i*) &B[i];
		/*res = _mm_mullo_epi32(*a, *b);
		ac1=ac1+_mm_extract_epi32(res,0);
		ac2=ac2+_mm_extract_epi32(res,1);
		ac3=ac3+_mm_extract_epi32(res,2);
		ac4=ac4+_mm_extract_epi32(res,3);*/
		acum1 = _mm_add_epi32(acum1, _mm_mullo_epi32(*a, *b)); 
		//acum2 = _mm_add_epi32(acum2, _mm_mullo_epi32(*a, *b)); 
		//acum3 = _mm_add_epi32(acum3, _mm_mullo_epi32(*a, *b)); 
		//acum4 = _mm_add_epi32(acum4, _mm_mullo_epi32(*a, *b)); 
	}
	//return _mm_extract_epi32(acum,0) +_mm_extract_epi32(acum,1) + _mm_extract_epi32(acum,2) +_mm_extract_epi32(acum,3);
	//return acum1.m128i_i32[0] + acum2.m128i_i32[1] + acum3.m128i_i32[2] + acum4.m128i_i32[3];
	//printf("%d %d %d %d\n",_mm_extract_epi32(acum1,0) ,_mm_extract_epi32(acum1,1) , _mm_extract_epi32(acum1,2) ,_mm_extract_epi32(acum1,3));
        return _mm_extract_epi32(acum1,0) +_mm_extract_epi32(acum1,1) + _mm_extract_epi32(acum1,2) +_mm_extract_epi32(acum1,3);
	//return ac1+ac2+ac3+ac4;
	
  	//printf ( "res=%d\n", res );
	//return res;
}

