#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "negative.h"

void header(int *is_p3, int *w, int *h, int *cd)
{
  /*
  int b1 = getchar();
  int b2 = getchar();
  */
  char hdr[3];
  
  int f1 = scanf("%s", hdr);
  int f2 = scanf("%d %d", w,h);
  int f3 = scanf("%d",cd);
  if (f1 != 1 || f2 != 2 || f3 != 1){
    fprintf(stderr, "Not quite right.\n");
    exit(1);
  }
  
  if (strcmp("P3", hdr) == 0)
    *is_p3 = 1;
  else if (strcmp("P6",hdr) == 0)
    *is_p3 = 0;
  else {
    fprintf(stderr, "Neiter P3 nor P6.\n");
    exit(1);
  }

  
  /*
  if (b1 != 'P' ){
    fprintf(stderr, "Very bad.\n");
    exit(1);
  }
  
  if (b2 == '3')
    *is_p3 = 1;
  if (b2 == '6')
    *is_p3 = 0;
  else {
    fprintf(stderr, "Not good.\n");
    exit(1);
  }
  */
  // DOES P6 have P6\n w h\n cd\n ...?
  
  
}


struct rgb *p3(int size, struct rgb *my_rgb)
{
  //  struct rgb *my_rgb = malloc(size*sizeof(struct rgb));
  int r, g, b;
  for (int i = 0; i < size; ++i){
    scanf("%d %d %d", &r, &g, &b);
    my_rgb[i].r = 255-r;
    my_rgb[i].g = 255-g;
    my_rgb[i].b = 255-b;
  }
  return my_rgb;
}


struct rgb *p6(int size, struct rgb *my_rgb)
{
  //  struct rgb *my_rgb = malloc(size*sizeof(struct rgb));
  for (int i = 0; i < size; ++i){
    
    int r = getchar(); 
    int g = getchar();
    int b = getchar();    
    my_rgb[i].r = 255-r;
    my_rgb[i].g = 255-g;
    my_rgb[i].b = 255-b;
  }
  return my_rgb;
}



int main()
{
  int w,h,cd, is_p3;

  header(&is_p3, &w, &h, &cd);
  
  int size = w*h;
  struct rgb *my_rgb = malloc(size*sizeof(struct rgb));  
  
  if (is_p3){
    my_rgb = p3(size, my_rgb);
    printf("P3\n%d %d\n%d\n", w,h,cd);
    for (int i = 0; i < size; ++i){
      printf("%d %d %d\n", my_rgb[i].r,
	     my_rgb[i].g, my_rgb[i].b);
    }
  }
  else {
    my_rgb = p6(size, my_rgb);
    printf("P6\n%d %d\n%d\n", w,h,cd);
    for (int i = 0; i < size; ++i){
      putchar(my_rgb[i].r);
      putchar(my_rgb[i].g);
      putchar(my_rgb[i].b);   
    }
  }
  return 0;
}



