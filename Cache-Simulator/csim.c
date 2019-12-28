#include <stdio.h>
#include <math.h>
#include <string.h>
#include "cachelab.h"
#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>


typedef struct {
  int valid;
  unsigned long tag;
  unsigned long addr;
  int ord;
} block_t;

typedef struct {
  int S;
  int E;
  int B;
  block_t *sets;
} cache_t;

typedef struct {
  char opt;
  unsigned long addr;
} trace_t;


typedef unsigned long ulong;



ulong expt(int x, int n)
{
  ulong y = 1;
  for (int i = 0; i < n; ++i){
    y *= (ulong) x;
  }
  return y;
}

ulong addr_to_num(char* address)
{
  int len = strlen(address);
  if (len <= 0){
    fprintf(stderr, "addr_to_num: Incorrect length of address.\n");
    exit(1);
  }
  int i, sum = 0;
  
  for (i = len-1; i >= 0; --i){
    char c = address[i];
    printf("%c\n", c);
    if (c < 58 && c > 47){
      sum += (c - 48)*expt(16,len-1-i);
    } else {
      sum += (c - 87)*expt(16,len-1-i);
    }
  }
  return sum;
}


ulong dec_to_num(char* num)
{
  int len = strlen(num);
  if (len <= 0){
    fprintf(stderr, "dec_to_num: Incorrect length of address.\n");
    exit(1);
  }
  int i, sum = 0;

  for (i = len-1; i >= 0; --i){
    char c = num[i];
    if (c < 58 && c > 47){
      sum += (c - 48)*expt(10,len-1-i);
    } else {
      fprintf(stderr, "This is not a decimal number.\n");
      exit(1);
    }
  }
  return sum;

}

/*
// creates a number denoting the exact time
// e.g. 0512131606 is May 12th at 13:16:06
// ignores the year.
int gettime()
{
  time_t rawtime;
  struct tm * timeinfo;
  
  time(&rawtime);
  timeinfo = localtime (&rawtime);
  
  int m = timeinfo->tm_mon*expt(10,8);
  int d = timeinfo->tm_day*expt(10,6);
  int 
}
*/


ulong set_index(int b, int s, ulong c_addr)
{
  ulong res = c_addr<<(64-b-s); // remove tag
  res = res>>(64-s); // remove bit shift
  ulong mask = 0;
  for (int i = 0; i < s; ++i){
    mask += (1<<i);
  }
  
  //printf("%lu\n", mask);
  return res&mask;
}


ulong addr_tag(int b, int s, ulong c_addr)
{
  ulong u = c_addr>>(b+s);
  ulong mask = 0;
  for (int i = 0; i < 64-b-s; ++i){
    mask += (1<<i);
  }
  mask += 1; // WHY IS THIS NECESSARY??
  // printf("%lu\n", mask);
  return u&mask;
}


int is_Hit(cache_t my_cache, ulong my_tag, ulong set_ind)
{
  int E = my_cache.E;
  for (int i = set_ind*E; i < (set_ind+1)*E; ++i){
    if ( (my_cache.sets[i]).tag == my_tag
	 && (my_cache.sets[i]).valid == 1)
      return 1;
  }
  return 0;
}


int is_Miss(cache_t my_cache, ulong set_ind)
{
  int E = my_cache.E;
  for (int i = set_ind*E; i < (set_ind + 1)*E; ++i){
    if ((my_cache.sets[i]).valid == 0){
      return 1;
    }
  }
  return 0;
}


int is_Evict(cache_t my_cache, ulong c_addr, ulong set_ind) //not is_Miss basically.
{
  int E = my_cache.E;
  for (int i = set_ind*E; i < (set_ind + 1)*E; ++i){
    if ((my_cache.sets[i]).valid == 0){
      return 0;
    }
  }
  return 1;
}


ulong find_empty(cache_t my_cache, ulong set_ind)
{
  int E = my_cache.E;
  ulong res;
  for (int i = set_ind*E; i < (set_ind+1)*E; ++i){
    if ((my_cache.sets[i]).valid == 0){
      res = i;
    }
  }
  return res;
}



ulong find_LRU(cache_t my_cache, ulong set_ind)
{
  int E = my_cache.E;
  ulong res;
  for (int i = set_ind*E; i < (set_ind+1)*E; ++i){
    if ((my_cache.sets[i]).ord == 0){
      res = i;
    }
  }
  return res;
}


ulong find_hit (cache_t my_cache, ulong set_ind, ulong my_tag)
{
  int E = my_cache.E;
  ulong res;
  for (int i = set_ind*E; i < (set_ind+1)*E; ++i){
    if ((my_cache.sets[i]).tag == my_tag){
      res = i;
    }
  }
  return res;
}




int main(int argc, char *argv[])
{
  /// *** PART 1 - READ IN VALUES *** ///
  int E = 0,s = -1,b = -1,i;
  FILE *file = stdout;
  int is_v = 0, is_h = 0;
  int c;
  char file_name[100] = {0};
  
  while ((c = getopt(argc, argv, "vhs:E:b:t:")) != -1){
    
    switch (c) {
    case 's':
      s = dec_to_num(optarg);
      break;
      
    case 'E':
      E = dec_to_num(optarg);
      break;
      
    case 'b':
      b = dec_to_num(optarg);
      break;
      
    case 't':
      sprintf(file_name, "%s", optarg); // Need to free thi at the end.
      file = fopen(optarg, "r");
      break;
      
    case 'v':
      is_v = 1;
      break;
      
    case 'h':
      is_h = 1;
      break;
      
    case '?':
      if (optopt == 's' ||
	  optopt == 'E' ||
	  optopt == 'b' ||
	  optopt == 't'){
	fprintf(stderr, "The option -%c requires an argument.\n", optopt);
	exit(1);
      } else {
	fprintf(stderr, "An invalid option was given.\n");
	exit(1);
      }
      break;
      
    default:
      fprintf(stderr, "Unexpected error in command line argument.\n");
      exit(1);
      
      // What does this do [ abort() ]?
      // Print a message to stderr instead??
    }
  }
  
  if (s <= -1 || b <= -1 || E <= 0 || file == stdout){
    fprintf(stderr, "At least one of the options -s, -E, -b or -t "
	    "was not included, or an invalid value "
	    "was given.\n");
    exit(1);
  }
  /// *** // *** ///

  printf("E = %d, s = %d, b = %d\n", E,s,b);

  /*
  char *str = "ffeffead6";
  char *str2 = "562";
  char *str3 = "a1b2c";
  
  
  printf("%lu\n", expt(16,3));
  printf("%lu\n", expt(10,3));
  printf("%lu\n", expt(2,5));
  printf("%lu\n", addr_to_num(str));
  printf("%lu\n", dec_to_num(str2));

  printf("%lu\n", addr_to_num(str2));
  printf("%lu\n", addr_to_num(str3));
  */






  /// *** PART 2 - READING FROM FILE *** ///
  char op;
  int size;
  ulong address;
  char my_buffer[1000];
  // char address[9];
  int ctr = 0;
  while (fgets(my_buffer, 1000, file) != NULL){
  
    if (my_buffer[0] == ' '){
      sscanf(my_buffer, " %c %lx,%d", &op,&address, &size);  
      ++ctr;
    }
  }
  fclose(file);
  file = fopen(file_name, "r");
  
  // Would it work to read to end of file, close,
  // then open again to read into array?
  int ctr2 = 0;
  trace_t *trace = malloc(ctr*sizeof(trace_t));
  while (fgets(my_buffer, 1000,file) != NULL){  
    
    if (my_buffer[0] == ' '){     
      sscanf(my_buffer, " %c %lx,%d", &op,&address, &size);  
      trace[ctr2].opt = op;
      trace[ctr2].addr = address;
      
      //printf("%c %lu\n", trace[ctr2].opt, trace[ctr2].addr);
      ++ctr2;
    }
  }
  /// *** // *** ///
  
  
  


  /// *** PART 3 - CONSTRUCT CACHE *** ///
  int S = 1, B = 1;
  
  for (i = 0; i < s; ++i){
    S <<= 2;
  }
  for (i = 0; i < b; ++i){
    B <<= b;
  }
  
  block_t *my_sets = malloc(S*E*sizeof(block_t));
  for (i = 0; i < S; ++i){
    for (int j = i*E; j < (i+1)*E; ++j){
      my_sets[j].valid = 0;
      my_sets[j].tag = 0;
      my_sets[j].addr = 0;
      my_sets[j].ord = j - i*E;
      /*
      printf("Set %d - Block %d\n", i, my_sets[j].ord);
      printf("Valid - %d\n", my_sets[j].valid);
      printf("Tag - %lu\n", my_sets[j].tag);
      printf("Order - %d\n", my_sets[j].ord);
      printf("Index is %d\n", j);
      putchar('\n');
      */
      }
  }
  
  cache_t my_cache;
  my_cache.sets = my_sets;
  my_cache.S = S;
  my_cache.B = B;
  my_cache.E = E;
  /// *** // *** ///     



  printf("The set index of 111010111011 for b = 5, s = 5 is 21 "
	 ", predicted to be %lu\n", set_index(5,5,3771));

  printf("The tag of 111010111011 for b = 1, s = 1 is 942 "
	 ", predicted value is %lu\n", addr_tag(1,1,3771));
  printf("The tag of 111010111011 for b = 0, s = 0 is 3771 "
	 ", predicted value is %lu\n", addr_tag(0,0,3771));
  
  printf("The tag of 111010111011 for b = 2, s = 5 is 29 "
	 ", predicted value is %lu\n", addr_tag(2,5,3771));
  
  
  // Assumptions: We know which flags are active, we
  // have an array of length ctr2 that contains the 
  // relevant trace, and we have constructed a cache
  // that contains sets of correct dimensions (given from
  // command line).



  
  /// *** PART 4 - GETTING THE INFO *** ///
  int hits = 0, misses = 0, evicts = 0;
  int trace_size = ctr2;

  for (i = 0; i < trace_size; ++i){
    ulong c_addr = trace[i].addr; // current address
    char c_opt = trace[i].opt; // current instruction option
    
    ulong set_ind = set_index(b,s,c_addr); // find set index of the address
    ulong my_tag = addr_tag(b,s,c_addr); // find tag of address
    if (my_tag)
     my_tag = my_tag + 1 - 1;
    
    int is_miss = is_Miss(my_cache, set_ind); 
    int is_hit = is_Hit(my_cache, my_tag, set_ind);    
    //printf("trace_size =  %d --- i = %d\n", trace_size, i);

    
    
    if (is_hit){  // OPTION 1 -- IF HIT
      ++hits;
      ulong ind1 = find_hit(my_cache, set_ind, my_tag);
      int ord1 = my_sets[ind1].ord;
	
      for (int j = set_ind*E; j < (set_ind+1)*E; ++j){
        if (my_sets[j].ord > ord1){
	  my_sets[j].ord -= 1;
	}
      }
      my_sets[ind1].ord = E-1;
    
      if (is_v){
	printf("%c, %lu, 1 hit ",c_opt, c_addr);
      }
    
    } else if (is_miss){   // OPTION 2 -- IF MISS
      //ulong ind2 = find_LRU(my_cache, set_ind);
      ulong ind2 = find_empty(my_cache, set_ind);
      my_sets[ind2].valid = 1;
      my_sets[ind2].addr = c_addr;
      my_sets[ind2].tag = my_tag;
      int ord2 = my_sets[ind2].ord; 
	
      for (int j = set_ind*E; j < (set_ind+1)*E; ++j){
	if (my_sets[j].ord > ord2){
	  my_sets[j].ord -= 1;
	}
      }
      my_sets[ind2].ord = E-1;
      ++misses;
      
      if (is_v){
        printf("%c, %lu, 1 miss ",c_opt, c_addr);
      }
      
    } else {      // OPTION 3 -- IF MISS EVICTION
      ulong ind3 = find_LRU(my_cache, set_ind);
      int ord3 = my_sets[ind3].ord;
      
      my_sets[ind3].addr = c_addr;
      my_sets[ind3].tag = my_tag;
      
      for (int k = set_ind*E; k < (set_ind+1)*E; ++k){
	if (my_sets[k].ord > ord3){
	  my_sets[k].ord -= 1;
	}
      } 
      my_sets[ind3].ord = E-1;
      ++evicts;
      ++misses;
      
      if (is_v){
        printf("%c, %lu, 1 miss eviction ",c_opt, c_addr);
      }
      
    }
    if (c_opt == 'M'){
      if (is_v){
      printf("hit\n");
      }
      ++hits;
    } else {
      if (is_v){
	putchar('\n');
      }
    }
  }
  /// *** // *** ///



  // The hits should decrement LRU.
  // Evicts are misses too.

  
  // Forget about cache_t? Just use sets and pass E in as arg?

  if (is_h)
    is_h = 1;
  if (is_v)
    is_v = 1;

  fclose(file);
  free(trace);
  free(my_sets);
  printSummary(hits, misses, evicts);
  return 0;
}



