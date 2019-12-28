/*
 * trans.c - Matrix transpose B = A^T
 *
 * Each transpose function must have a prototype of the form:
 * void trans(int M, int N, int A[N][M], int B[M][N]);
 *
 * A transpose function is evaluated by counting the number of misses
 * on a 1KB direct mapped cache with a block size of 32 bytes.
 */
#include <stdio.h>
#include "cachelab.h"


int min(int one, int two)
{
  return (one < two) ? one : two;
}

int is_transpose(int M, int N, int A[N][M], int B[M][N]);

/*
 * transpose_submit - This is the solution transpose function that you
 *     will be graded on for Part B of the assignment. Do not change
 *     the description string "Transpose submission", as the driver
 *     searches for that string to identify the transpose function to
 *     be graded.
 */
char transpose_submit_desc[] = "Transpose submission";
void transpose_submit(int M, int N, int A[N][M], int B[M][N])
{
  
  int i,j,k,l,tmp1,tmp2;//,tmp3,tmp4;//,tmp5,tmp6,tmp7,tmp8;
  if (N == 32 && M == 32){
    for (i = 0; i < N; i += 8){
      for (j = 0; j < M; j += 8){
	if (i != j){ // CHECK IF ON DIAGONAL.
	  for (k = j; k < min(j+8,M); ++k){
	    for (l = i; l < min(i+8,N); ++l){
	      tmp1 = A[l][k];
	      B[k][l] = tmp1;
	    }
	  }
	} else {
	  for (k = j; k < min(j+8,M); ++k){
	    for (l = i; l < min(i+8,N); ++l){
	      if (k != l){
		tmp1 = A[k][l];
		B[l][k] = tmp1;
	      } else {
		tmp2 = k;
	      }
	      
	    }
	    B[tmp2][tmp2] = A[tmp2][tmp2];
	  }
	  /*
	  B[j][j] = A[j][j];
	  B[j+1][j+1] = A[j+1][j+1];
	  B[j+2][j+2] = A[j+2][j+2];
	  B[j+3][j+3] = A[j+3][j+3];
	  B[j+4][j+4] = A[j+4][j+4];
	  B[j+5][j+5] = A[j+5][j+5];
	  B[j+6][j+6] = A[j+6][j+6];
	  B[j+7][j+7] = A[j+7][j+7];
	  */
	  	  	  
	  /*
	  tmp1 = A[j][j];
	  tmp2 = A[j+1][j+1];
	  tmp3 = A[j+2][j+2];
	  tmp4 = A[j+3][j+3];
	  tmp5 = A[j+4][j+4];
	  tmp6 = A[j+5][j+5];
	  tmp7 = A[j+6][j+6];
	  tmp8 = A[j+7][j+7];

	  B[j][j] = tmp1;
	  B[j+1][j+1] = tmp2;
	  B[j+2][j+2] = tmp3;
	  B[j+3][j+3] = tmp4;
	  B[j+4][j+4] = tmp5;
	  B[j+5][j+5] = tmp6;
	  B[j+6][j+6] = tmp7;
	  B[j+7][j+7] = tmp8;
	  */
	}
      }
    }
  }
      
  if (N == 64 && M == 32){
    for (i = 0; i < N; i += 8){
      for (j = 0; j < M; j += 8){
	for (k = j; k < min(j+8,M); ++k){
	  for (l = i; l < min(i+8,N); ++l){
	    B[k][l] = A[l][k];
	  }
	}
      }
    }
  }


  int tmp3, tmp4;
  int tmp5, tmp6;
  if (N == 64 && M == 64){
    for (tmp5 = 0; tmp5 < N; tmp5 += 32){
      for (tmp6 = 0; tmp6 < M; tmp6 += 32){

	for (i = tmp5; i < min(tmp5+32,N); i += 4){
	  for (j = tmp6; j < min(tmp6+32,M); j += 4){
	    //  if (i != j){
	      for (k = j; k < min(j+4,M); ++k){
		tmp1 = A[k][i];
		tmp2 = A[k][i+1];
		tmp3 = A[k][i+2];
		tmp4 = A[k][i+3];
		B[i][k] = tmp1;
		B[i+1][k] = tmp2;
		B[i+2][k] = tmp3;
		B[i+3][k] = tmp4;
	      } /*
	      } else {
	      ///
	      tmp1 = A[i][i+1];
	      tmp2 = A[i][i+2];
	      tmp3 = A[i][i+3];

	      B[i+1][i] = tmp1;
	      B[i+2][i] = tmp2;
	      B[i+3][i] = tmp3;

	      tmp1 = A[i+1][i];
	      tmp2 = A[i+1][i+2];
	      tmp3 = A[i+1][i+3];

	      B[i][i+1] = tmp1;
	      B[i+2][i+1] = tmp2;
	      B[i+3][i+1] = tmp3;

	      tmp1 = A[i+2][i];
	      tmp2 = A[i+2][i+1];
	      tmp3 = A[i+2][i+3];

	      B[i][i+2] = tmp1;
	      B[i+1][i+2] = tmp2;
	      B[i+3][i+2] = tmp3;

	      tmp1 = A[i+3][i];
	      tmp2 = A[i+3][i+1];
	      tmp3 = A[i+3][i+2];

	      B[i][i+3] = tmp1;
	      B[i+1][i+3] = tmp2;
	      B[i+2][i+3] = tmp3;


	      B[i][i] = A[i][i];
	      B[i+1][i+1] = A[i+1][i+1];
	      B[i+2][i+2] = A[i+2][i+2];
	      B[i+3][i+3] = A[i+3][i+3];
	      ///
		*/
	      /*
	      for (k = j; k < min(j+4,M); ++k){
		for (l = i; l < min(i+4,N); ++l){
		  if (l != k){
		    B[l][k] = A[k][l];
		  } else {
		    tmp2 = k;
		  }  
		}
		B[tmp2][tmp2] = A[tmp2][tmp2];
		    
	      }
	    }*/
	  }
	}
      }
    }
  }
}












char my_trans[] = "My test trans function";
void my_transpose(int M, int N, int A[N][M], int B[M][N])
{

  int i,j,k,l;
  //  int var0,var1,var2,var3,var4,var5,var6,var7;

  if (N == 32 && M == 32){
    for (i = 0; i < N; i += 8){
      for (j = 0; j < M; j += 8){
	/*
	var0 = A[i][i];
	var1 = A[i+1][j];
	var2 = A[i+2][j];
	var3 = A[i+3][j];
	var4 = A[i+4][j];
	var5 = A[i+5][j];
	var6 = A[i+6][j];
	var7 = A[i+7][j];
	*/
	for (k = j; k < min(j+8,M); ++k){
	  for (l = i; l < min(i+8,N); ++l){
	    B[k][l] = A[l][k];
	  }
	}
      }
      
    }
  }
}



/*
 * You can define additional transpose functions below. We've defined
 * a simple one below to help you get started.
 */

/*
 * trans - A simple baseline transpose function, not optimized for the cache.
 */

char trans_desc[] = "Simple row-wise scan transpose";
void trans(int M, int N, int A[N][M], int B[M][N])
{
  int i, j, tmp;
  
  for (i = 0; i < N; i++) {
    for (j = 0; j < M; j++) {
      tmp = A[i][j];
      B[j][i] = tmp;
    }
  }

}

/*
 * registerFunctions - This function registers your transpose
 *     functions with the driver.  At runtime, the driver will
 *     evaluate each of the registered functions and summarize their
 *     performance. This is a handy way to experiment with different
 *     transpose strategies.
 */
void registerFunctions()
{
    /* Register your solution function */
    registerTransFunction(transpose_submit, transpose_submit_desc);

    
    /* Register any additional transpose functions */
    registerTransFunction(my_transpose,my_trans); 
    registerTransFunction(trans, trans_desc);
    
}

/*
 * is_transpose - This helper function checks if B is the transpose of
 *     A. You can check the correctness of your transpose by calling
 *     it before returning from the transpose function.
 */
int is_transpose(int M, int N, int A[N][M], int B[M][N])
{
    int i, j;

    for (i = 0; i < N; i++) {
        for (j = 0; j < M; ++j) {
            if (A[i][j] != B[j][i]) {
                return 0;
            }
        }
    }
    return 1;
}

