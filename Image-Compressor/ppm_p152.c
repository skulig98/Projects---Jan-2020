#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "ppm_p152.h"
#include <time.h>


int big_endian_help(int num) //num or n_out??                                                                                                
{
  int sum = 0;
  int n = 1;
  for (int i = 0; i < 8; ++i){
    sum += ((num)&(n<<i));
  }
  return sum;
}


int char_to_int(char c)
{
  if (c >= '0' && c <= '9')
    return (int) c-'0';
  fprintf(stderr, "Wrong char value.\n");
  exit(1);
}



int time_or_date(const char *string)
{
  time_t rawtime;
  struct tm * timeinfo;
  time ( &rawtime );
  timeinfo = localtime ( &rawtime );

  if (strcmp(string,"date") == 0){
    return 10000*(timeinfo->tm_year+1900) +
      100*(timeinfo->tm_mon+1) + timeinfo->tm_mday;
  }
  if (strcmp(string,"time") == 0){
  return 100*timeinfo->tm_hour +
    + timeinfo->tm_min;
  } else {
    fprintf(stderr, "Wrong string. "
	    "Expected 'date' or 'time'.\n");
    exit(1);
  }
}


void big_endian_four(int num, FILE *f)
{
  fputc(big_endian_help(num>>24), f);
  fputc(big_endian_help(num>>16), f);
  fputc(big_endian_help(num>>8), f);
  fputc(big_endian_help(num), f);

}


void big_endian_two(int num, FILE *f)
{
  fputc(big_endian_help(num>>8), f);
  fputc(big_endian_help(num), f);
}





void color_size()
{
  for (int i = 0; i < 4; ++i)
    putchar(0);
}






int is_gray (struct rgb my_rgb)
{
  if (my_rgb.r == my_rgb.g && my_rgb.r == my_rgb.b)
    return 1;
  else
    return 0;
}



int is_grayscale (struct rgb *my_rgb, int size)
{
  int sum = 0;
  for (int i = 0; i < size; ++i){
    sum += is_gray(my_rgb[i]);
  }
  if (sum == size)
    return 1;
  return 0;
}


// rgb_eq is 0 when the rgbs ARE equal.                                                                                                      
int rgb_eq (struct rgb rgb1, struct rgb rgb2)
{
  if (rgb1.r == rgb2.r &&
      rgb1.g == rgb2.g &&
      rgb1.b == rgb2.b)
    return 0;
  return 1;
}


int num_runs(struct rgb *my_rgb, int size)
{
  int counter = 1;
  for (int i = 0; i < size-1; ++i){
    if (rgb_eq(my_rgb[i],my_rgb[i+1]) != 0)
      counter += 1;
  } return counter;
}


struct run_len *runs_lengths(struct rgb *my_rgb, int size)
{
  int num_of_runs = num_runs(my_rgb, size);
  struct run_len *my_rl = malloc(num_of_runs*sizeof(struct run_len));

  int counter = 0;
  int placehold = 0;
  for (int i = 0; i < size-1; ++i){
    if ( rgb_eq(my_rgb[i],my_rgb[i+1]) != 0){


      my_rl[counter].len = (i+1)-placehold;
      placehold = i+1;
      my_rl[counter].color = &my_rgb[i];
      counter += 1;
    }
  }
  my_rl[num_of_runs-1].len = size - placehold ;
  my_rl[num_of_runs-1].color = &my_rgb[size-1];

  return my_rl;

}



// A function that takes in an array of rgbs, their length
// and returns the average rgb value.
struct rgb *average (struct rgb *rgb_array, int len)
{
  if (len <= 0){
    fprintf(stderr, "Bad length.\n");
    exit(1);
  }
  struct rgb *avg_rgb = malloc(sizeof(struct rgb));;
  int r = 0, g = 0, b = 0;
  for (int i = 0; i < len; ++i){
    r += rgb_array[i].r;
    g += rgb_array[i].g;
    b += rgb_array[i].b;
  }
  avg_rgb->r = r/len;
  avg_rgb->g = g/len;
  avg_rgb->b = b/len;
  return avg_rgb;
}




// A function that determines if char *str is contained in argv[].
// Saves the index as an out parameter.
int contains(int argc, char *argv[], char *str, int *i_out)
{
  for (int i = 0; i < argc; ++i){
    if (!strcmp(str, argv[i])){
      *i_out = i;
      return 1;
    }
  }
  return 0;
}


/*
// If argv ...
int index(int argc, char *argv[], char *str)
{
  return 0;
}
*/


int str_to_num(char *t_num)
{
  int len = strlen(t_num);
  
  if (len == 0){
    fprintf(stderr, "str_to_num: Unexpected error.\n");
    exit(1);
  }
  if (len > 3){
    fprintf(stderr, "str_to_num: String too large.\n");
    exit(1);
  }
  /*
  for (int i = 0; i < len; ++i){
    if (t_num[i] < 49 || t_num[i] > 57){
      fprintf(stderr, "str_to_num: Not a number.\n");
      exit(1);
    }
  }
  */
  int sum;
  if (len == 1)
    sum = t_num[0] - '0';
  // return t_num[0] - '0';
  else if (len == 2)
    sum = (t_num[1]-'0') + (t_num[0]-'0')*10; 
    //return (t_num[1]-'0') + (t_num[0]-'0')*10;
  else
    sum = (t_num[2]-'0') + (t_num[1]-'0')*10                                            
      + (t_num[0]-'0')*100;  
    //    return (t_num[2]-'0') + (t_num[1]-'0')*10
    // + (t_num[0]-'0')*100;
  if (sum > 256 || sum <= 0){
    fprintf(stderr, "str_to_num: Number not in correct range.\n");
    exit(1);
  }
  return sum;
}





// Distance function for rgbs.
int dist_rgb(struct rgb rgb1, struct rgb rgb2)
{
  return sqrt( (rgb1.r - rgb2.r)*(rgb1.r - rgb2.r)
	       + (rgb1.g - rgb2.g)*(rgb1.g - rgb2.g)
	       + (rgb1.b - rgb2.b)*(rgb1.b - rgb2.b));
}


int my_index(struct rgb one_rgb, struct rgb *ct_array, int ct_size)
{
  if (ct_size <= 0){
    fprintf(stderr, "Bad.\n");
    exit(1);
  }
  int ind = 0;
  int min_dist = dist_rgb(one_rgb, ct_array[0]);
  
  for (int i = 0; i < ct_size; ++i){
    if (dist_rgb(one_rgb, ct_array[i]) < min_dist){
      min_dist = dist_rgb(one_rgb, ct_array[i]);
      ind = i;
    }
  }
  return ind;
}


// Takes in two arrays of rgbs, runs through one of them, and
// returns the index of the color in the second that is closest to
// the color under inspection.
int *indeces(struct rgb *my_rgb, int size,
	  struct rgb *ct_array, int ct_size)
{
  if (size <= 0 || ct_size <= 0){
    fprintf(stderr, "Bad 1.\n");
    exit(1);
  }
  
  int *index_array = malloc(size*sizeof(int));
  for (int i = 0; i < size; ++i){
    index_array[i] = my_index(my_rgb[i], ct_array, ct_size);
  }
    
  return index_array;
}







int main(int argc, char *argv[])
{ 
  
  // ** DETERMINING if RUN-LEN, COLOR TABLE, INFILE AND OUTFILE ** //
  int has_str = 0, has_rl = 0, has_ct = 0, ct_size = 0;
  FILE *in_file = stdin;
  FILE *out_file = stdout;
  int n_out;
  
  // ** IF has string ** //
  if (argc >= 2){
    if (argv[1][0] != '-')
      has_str = 1;
  } 
  
  // ** IF -i ** //
  if (contains(argc, argv, "-i", &n_out)){  
    int i_index = n_out + 1;
    if (i_index >= argc){
      fprintf(stderr, "Index out of range.\n");
      exit(1);
    }
    in_file = fopen(argv[i_index], "r");
  }

  // ** IF -o ** //
  if (contains(argc, argv, "-o", &n_out)){
    int o_index = n_out + 1;
    if (o_index >= argc){
      fprintf(stderr, "Index out of range.\n");
      exit(1);
    }
    out_file = fopen(argv[o_index], "w");
  }
  
  // ** IF -r ** //
  if (contains(argc, argv, "-r", &n_out)){
    has_rl = 1;
  }
  
  // ** IF -t ** //
  if (contains(argc, argv, "-t", &n_out)){
    int t_index = n_out + 1;
    if (t_index >= argc){
      fprintf(stderr, "Index out of range.\n");
      exit(1);
    }
    has_ct = 1;
    ct_size = str_to_num(argv[t_index]);
    if (ct_size > 256 || ct_size <= 0){
      fprintf(stderr, "Unacceptable size of color table.\n");
      exit(1);
    }
  }
  // ** /// ** //
  
  
  // ** HEADER ** //                                                                                                     
  char string[3];
  int w,h,cd; 
  int is_p3 = 0, is_p6 = 0;
  
  fscanf(in_file, "%s", string);
  fscanf(in_file, "%d %d", &w,&h);
  fscanf(in_file, "%d",&cd);
  
  if (!strcmp(string, "P3")){
    is_p3 = 1;
  } else if (!strcmp(string, "P6")) {
    is_p6 = 1;
  } else {
    printf("Neither P6 nor P3.\n");
    exit(1);
  }
  int size = w*h;
  fprintf(out_file,"P152v1");
  // **  /// **  //                                                                                               
  
  struct rgb *my_rgb = malloc(size*sizeof(struct rgb));
  
  if (is_p3){
    int r,g,b;
    for (int i = 0; i < size; ++i){
      fscanf(in_file, "%d %d %d", &r,&g,&b);
      my_rgb[i].r = r;
      my_rgb[i].g = g;
      my_rgb[i].b = b;
    }
  } else {
    for (int i = 0; i < size; ++i){
      my_rgb[i].r = fgetc(in_file);
      my_rgb[i].g = fgetc(in_file);
      my_rgb[i].b = fgetc(in_file);
    }
  }


  // ** BYTES 6-7 ** //                                                                                           
  fputc(0, out_file);
  fputc(0, out_file);
  // **  /// **  //
  
  // ** DATE ** //
  int my_date = time_or_date("date");
  big_endian_four(my_date, out_file);
  // ** /// ** //

  // ** TIME ** //
  int my_time = time_or_date("time");
  big_endian_two(my_time, out_file);
  // ** /// ** // 

  // ** WIDTH ** //
  big_endian_two(w, out_file);
  // ** /// ** //

  // ** HEIGHT ** //
  big_endian_two(h, out_file);
  // ** /// ** //

  // ** BYTES 18-21 (COLOR TABLE SIZE) ** //
  if (has_ct)
    big_endian_four(ct_size, out_file);
  else {
    fputc(0, out_file);
    fputc(0, out_file);
    fputc(0, out_file);
    fputc(0, out_file);
  }
  // ** /// ** //
  
  // ** BYTE 22 ** //
  int is_gs = is_grayscale(my_rgb, size);
  if (is_gs)
    has_ct = 0;
  int byte22 = 128*has_rl + 64*is_gs
    + 32*has_ct;
  fputc(byte22, out_file);
  // ** /// ** //

  // ** BYTES 23-25 ** //
  fputc(0, out_file);
  fputc(0, out_file);
  fputc(0, out_file);
  // ** /// ** // 

  // ** BYTE 26 (string) ** //
  if (has_str)
    fprintf(out_file,"%s", argv[1]);
  fputc('\0', out_file);  
  // ** /// ** //

  

  // ** COLOR TABLE ** //
  struct rgb *ct_array = malloc(sizeof(struct rgb)*ct_size);
  if (has_ct) {
    if (ct_size > size) {        
      for (int i = 0; i < ct_size; ++i){
	ct_array[0].r = i;
	ct_array[0].g = 255-i;
	ct_array[0].b = i;

	fputc(i, out_file);
	fputc(255-i, out_file);
	fputc(i, out_file);
      }
    } else {
      int len = size/ct_size;
      for (int i = 0; i < ct_size; ++i){
	struct rgb *temp = malloc(len*sizeof(struct rgb));
	for (int j = i*len; j < (i+1)*len; ++j) {
	  temp[j - i*len] = my_rgb[j];
	}
	ct_array[i].r = average(temp, len)->r;
	ct_array[i].g = average(temp, len)->g;
	ct_array[i].b = average(temp, len)->b;
	
	fputc(ct_array[i].r, out_file);
	fputc(ct_array[i].g, out_file);
	fputc(ct_array[i].b, out_file);

	free(temp);
      }        
    }
    int *index_array = indeces(my_rgb, size, ct_array, ct_size);
    
    // If (has_rl==1), make array of "rgb's" out of index array.
    // Then call run_lengths on that, make run_len struct out of it.
    // Then put all lengths as big_endians, and then simply use fputc()
    // on the first coordinate of each color of the "rgb" array.
    
    if (has_rl) {
      struct rgb *index_rgb = malloc(size*sizeof(struct rgb));
      for (int i = 0; i < size; ++i) { // FILLING index_rgb with index values.
	index_rgb[i].r = index_array[i];
	index_rgb[i].g = index_array[i];
	index_rgb[i].b = index_array[i];
      }
      int oth_no_runs = num_runs(index_rgb,size);
      big_endian_four(oth_no_runs, out_file); // WRITING NO. OF RUNS AS BIG ENDIAN.
      struct run_len *index_rl = 
	malloc(oth_no_runs*sizeof(struct run_len));      
      
      index_rl = runs_lengths(index_rgb, size); // Making a run_len of the index array.
      for (int i = 0; i < oth_no_runs; ++i){ // Printing the run lengths.
	big_endian_four(index_rl[i].len, out_file);
      }
      for (int i = 0; i < oth_no_runs; ++i){ // Printing the indeces.
	fputc(index_rl[i].color->r, out_file);
      }
      free(index_rgb); // Freeing arrays.
      free(index_rl);
    
    } else {
      for (int i = 0; i < size; ++i){
	fputc(index_array[i], out_file);
      }
      free(index_array);
    }
  
  } else {
    fputc(0, out_file);
    fputc(0, out_file);
    fputc(0, out_file);
    fputc(0, out_file);
  }
  // ** /// ** //      




  // ** RUN-LENGTH ENCODING (W/O COLOR TABLE) ** //
  if (has_rl && !has_ct) {
    int the_numruns = num_runs(my_rgb,size);
    big_endian_four(the_numruns, out_file); // STORING NUMBER OF RUNS
  
    struct run_len *my_rl = runs_lengths(my_rgb, size);
    for (int i = 0; i < the_numruns; ++i){
      big_endian_four(my_rl[i].len, out_file);
    }
    //if (!has_ct){ // I THINK THIS PART IS WRONG.
      if (is_gs){
	for (int i = 0; i < the_numruns; ++i){
	  fputc(my_rl[i].color->r, out_file);
	}
      
      } else {
	for (int i = 0; i < the_numruns; ++i){
	  fputc(my_rl[i].color->r, out_file);
	  fputc(my_rl[i].color->g, out_file);
	  fputc(my_rl[i].color->b, out_file);
	}
      }
      //}
    free(my_rl);
  }
  

  /* MOVED TO EARLIER if(has_ct && !has_rl) CLAUSE.
     
  if (has_ct) {
    int *index_array = indeces(my_rgb, size, ct_array, ct_size);
    for (int i = 0; i < size; ++i){
      fputc(index_array[i], out_file);
    }
    free(index_array);
  }
  */
  
  if (!has_ct && !has_rl){
    for (int i = 0; i < size; ++i){
      fputc(my_rgb[i].r, out_file);
      fputc(my_rgb[i].g, out_file);
      fputc(my_rgb[i].b, out_file);
    }  
  }
  // ** /// ** //                                        
  


  // ** FREE ** //
  if (ct_array)
    free(ct_array);
  free(my_rgb);
  fclose(in_file); 
  fclose(out_file);
  // ** /// ** //

  return 0;
}
