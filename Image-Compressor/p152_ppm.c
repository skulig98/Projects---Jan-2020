#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ppm_p152.h"



void header()
{
  char *hstring = malloc(7);
  for (int i = 0; i < 6; ++i)
    {
      hstring[i] = getchar();
    }
  hstring[7] = '\0';
  if (strcmp(hstring, "P152v1") != 0){
    fprintf(stderr, "Target is not P152v1.\n");
    exit(1);
  }
  free(hstring);
  printf("P3\n");
}


void skip_space(int n)
{
  for (int i = 0; i < n; ++i)
    getchar();
}

int bigE_four_int(int byte1, int byte2,
                  int byte3, int byte4)
{
  return (byte1<<24)+(byte2<<16)
    + (byte3<<8) + byte4;
}


int bigE_two_int()
{
  int byte1 = getchar();
  int byte2 = getchar();
  return (byte1<<8) + byte2;
}

void get_string()
{
  int r = getchar();
  while (r != '\0')
    r = getchar();
}



int main()
{

  // ** HEADER ** //
  header();
  // ** /// ** //

  // ** BYTES 6-7 ** //
  skip_space(2);
  // ** /// ** //

  // ** DATE ** //
  skip_space(4);
  // ** /// ** //

  // ** TIME ** //
  skip_space(2);
  // ** /// ** //
  
  // ** WIDTH AND HEIGHT ** //
  int w = bigE_two_int();
  int h = bigE_two_int();
  int size = w*h;
  printf("%d %d\n", w,h);
  printf("255\n");
  // ** /// ** //

  // ** BYTES 18-21 (COLOR TABLE SIZE) ** //
  int b1 = getchar();
  int b2 = getchar();
  int b3 = getchar();
  int b4 = getchar();
  int ct_size = bigE_four_int(b1,b2,b3,b4);
  // ** /// ** //

  // ** BYTE 22 ** //
  int byte22 = getchar();
  int grayscale = 0, has_rl = 0, has_ct = 0;
  if ( ((byte22)&64) != 0)
    grayscale = 1;
  if ( ((byte22)&128) != 0)
    has_rl = 1;
  if ( ((byte22)&32) != 0)
    has_ct = 1;
  // ** /// ** //
  
  
  // ** BYTES 23-25 ** //
  skip_space(3);
  // ** /// ** //

  // ** BYTE 26 ** //
  get_string();
  // ** /// ** //

  // ** COLOR TABLE ** //
  struct rgb *col_tbl = malloc(ct_size*sizeof(struct rgb));
  
  if (!has_ct)
    skip_space(4);
  else {
    for (int i = 0; i < ct_size; ++i){
      col_tbl[i].r = getchar();
      col_tbl[i].g = getchar();
      col_tbl[i].b = getchar();
    }
  }
  // ** /// ** //

  // If RL exclusive,
  // IF RL AND NO COLOR TABLE, THEN USE RL.
  
  // ** THE RUNS ** //
  if (has_rl){
    int byte1 = getchar();
    int byte2 = getchar();
    int byte3 = getchar();
    int byte4 = getchar(); 
    int num_runs = bigE_four_int(byte1,byte2,
				 byte3,byte4);
    //  if (!has_ct){
    struct rgb *colors = malloc(num_runs*sizeof(struct rgb));
    int *lengths = malloc(num_runs*sizeof(struct rgb));
    
    for (int i = 0; i < num_runs; ++i){
      int b1 = getchar();
      int b2 = getchar();
      int b3 = getchar();
      int b4 = getchar();
      
      lengths[i] = bigE_four_int(b1,b2,b3,b4);
    }
    
    
    // ** ACCESSING THE COLORS ** //
    if (has_ct){
      for (int i = 0; i < num_runs; ++i){
	int ind = getchar();
	colors[i].r = col_tbl[ind].r;
	colors[i].g = col_tbl[ind].g;
	colors[i].b = col_tbl[ind].b;
      }
    } else if (!grayscale){
      for (int i = 0; i < num_runs; ++i){
	int r = getchar();
	int g = getchar();
	int b = getchar();
	colors[i].r = r;
	colors[i].g = g;
	colors[i].b = b;
      }
    } else {
      for (int i = 0; i < num_runs; ++i){
	int r = getchar();
	colors[i].r = r;
	colors[i].g = r;
	colors[i].b = r;
      }
    }
    
    // ** PRINTING THE COLORS ** //
    for (int i = 0; i < num_runs; ++i){
      for (int j = 0; j < lengths[i]; ++j)
	printf("%d %d %d\n", colors[i].r,
	       colors[i].g, colors[i].b);
    } 
    free(colors);
    free(lengths);
  }
  


  if (has_ct && !has_rl) {
    for (int i = 0; i < size; ++i){
      int ind = getchar();
      printf("%d %d %d\n", col_tbl[ind].r,
	     col_tbl[ind].g, col_tbl[ind].b);
    }
  } 
  
  if (!has_ct && !has_rl){
    for (int i = 0; i < size; ++i){
      int r = getchar();
      int g = getchar();
      int b = getchar();
      
      printf("%d %d %d\n", r,g,b);
    }
  }
  
  
  if (col_tbl)
    free(col_tbl);
}
