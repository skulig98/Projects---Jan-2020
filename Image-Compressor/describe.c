#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "describe.h"



int bigEnd4()
{
  int byte1 = getchar();
  int byte2 = getchar();
  int byte3 = getchar();
  int byte4 = getchar();
  
  return (byte1<<24) + (byte2<<16)
    + (byte3<<8) + byte4;
}



int bigEnd2()
{
  int byte1 = getchar();
  int byte2 = getchar();
 
  return (byte1<<8) + byte2;
}




int main() {
  char *header = malloc(7);
  for (int i = 0; i < 6; ++i){
    header[i] = getchar();
  }
  header[6] = '\0';
  if (strcmp("P152v1",header)){
    fprintf(stderr, "Not a P152.\n");
    exit(1);
  }
  
  free(header);

  // BYTES 6-7 //
  getchar();
  getchar();
  // ** /// ** //

  
  // DATE, TIME, WIDTH and HEIGHT //
  int date = bigEnd4();
  char *d_str = malloc(9);
  sprintf(d_str, "%d", date);
  
  int time = bigEnd2();
  //char *t_str = malloc(5);
  char t_str[5] = {0};
  sprintf(t_str, "%d", time);
  
  int width = bigEnd2();

  int height = bigEnd2();
  // ** /// ** //

  
  // COLOR TABLE SIZE //
  int ct_size = bigEnd4();
  // ** /// ** //
  
  
  // BYTE 22 //
  int byte22 = getchar();
  int is_grayscale = 0, has_ct = 0, has_rl = 0;
  
  if (byte22&128)
    has_rl = 1;
  if (byte22&64)
    is_grayscale = 1;
  if (byte22&32)
    has_ct = 1;
  // ** /// ** //
  
  
  // BYTES 23-25 //
  getchar();
  getchar();
  getchar();
  // ** /// ** //
  
  
  printf("The P152 was made at date: %c%c/%c%c/%c%c%c%c\n", d_str[4], d_str[5], d_str[6], d_str[7],
	 d_str[0],d_str[1],
	 d_str[2],d_str[3]);
  int t_len = strlen(t_str);
  if (t_len == 2) {
    printf("It was made at time 00:%c%c\n", t_str[0], t_str[1]);
  } else if (t_len == 3) {
    printf("It was made at time 0%c:%c%c\n", t_str[0], t_str[1],
	   t_str[2]);
  } else if (t_len == 1) {
    printf("It was made at time 00:0%c\n", t_str[0]);
  } else {
  printf("It was made at time %c%c:%c%c\n", t_str[0], t_str[1],
	 t_str[2], t_str[3]);
  }
  printf("The width of the image is %d\n", width);
  printf("The height of the image is %d\n", height);
  
  
  if (has_rl)
    printf("The image has run-length encoding.\n");
  else 
    printf("The image does not have run-length encoding.\n");
  
  if (is_grayscale)
    printf("The image is grayscale.\n");
  else
    printf("The image is not grayscale.\n");
  
  if (has_ct)
    printf("The image has a color table of size %d\n", ct_size);
  else
    printf("The image does not have a color table.\n");
  
  
  int b = getchar();
  if (!b)
    printf("The file has no embedded string.\n");
  else {
    printf("The string embedded in the file is: ");
    while (b) {
      putchar(b);
      b = getchar();
    }
    putchar('\n');
  }
  return 0;
}

