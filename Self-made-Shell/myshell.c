#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <sys/wait.h>
#include <ctype.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>



void myPrint(char *msg)
{
    write(STDOUT_FILENO, msg, strlen(msg));
}


int if_ls(char *arg)
{
  return 0;
}

int if_pwd(char *arg)
{
  return 0;
}

int if_cd(char *arg)
{
  return 0;
}

int if_exit(char *arg) 
{
  return 0;
}


// Appends second string to the first.
char *string_append (char *s1, char *s2)
{
  int i;
  int len1 = strlen(s1);
  int len2 = strlen(s2);

  int new_len = len1 + len2 + 1;
  char *new_str = malloc(new_len+1);

  for (i = 0; i < len1; ++i){
    new_str[i] = s1[i];
  }

  new_str[len1] = '/';

  for (i = len1+1; i < new_len; ++i){
    new_str[i] = s2[i-(len1+1)];
  }

  new_str[new_len] = '\0';

  return new_str;
  
}


char *get_path (char *cwd)
{
  int i;
  int ctr = 0;
  int len = strlen(cwd);

  int stop = 0;
  
  for (i = (len-1); i >= stop; ++i){
    if (cwd[i] != '/')
      ++ctr;
    else {
      stop = i;
    }
  }
  char *new_str = malloc(ctr+1);
  for (i = 0; i < ctr; ++i){
    new_str[i] = cwd[i+(len-ctr)];
  }
  new_str[ctr] = '\0';
  return new_str;
}


int str_eq (char *s1, char *s2)
{
  int len1 = strlen(s1);
  int len2 = strlen(s2);
  
  if (len1 != len2 && (len1+1) != len2)
    return 0;

  for (int i = 0; i < len1; ++i){
    if (s1[i] != s2[i]){
      return 0;
    }
  }
  if ((len1 + 1) == len2){
    if (s2[len1] != '\t' && s2[len1] != '\n')
      return 0;
  }
  
  return 1;
}





void writeError()//char* cmdline)
{
  // write(STDOUT_FILENO, cmdline, strlen(cmdline));
  char error_message[30] = "An error has occurred\n";
  write(STDOUT_FILENO, error_message, strlen(error_message));

}

void errorLine(char *cmdline)
{
  write(STDOUT_FILENO, cmdline, strlen(cmdline));
  char error_message[30] = "An error has occurred\n";
  write(STDOUT_FILENO, error_message, strlen(error_message));

}


int main(int argc, char *argv[]) 
{
    char cmd_line[1026];
    char *cmd_buff;
    char *cmd_buff_pre;
    
    char *pinput;
    FILE *in_file = stdin;
    int is_batch = 0;
    int is_blank_line = 1;
    //int cmd_len;
    char *space_token;
    char *space_token2;
    char my_path[100];
    int chDir;
    char bigExit[5] = "exit";
    char cd[3] = "cd";
    char pwd[4] = "pwd";
    char dotdot[3] = "..";

    int is_cd;
    int is_pwd;
    int is_exit;
    int ctr;
    
    if (argc == 2){
      is_batch = 1;
      in_file = fopen(argv[1], "r");
      if (!in_file){
	writeError();
	exit(0);
      }
    } else if (argc > 2) {
      writeError();
      exit(0);
    }
    
    while (1) {
      is_blank_line = 1;
      if (!is_batch){
        myPrint("myshell> ");
      }
      pinput = fgets(cmd_line, 1026, in_file); // Change stdin to the file pointer in order to implement batch mode

      int cmd_len = strlen(cmd_line);
      if (cmd_len > 512){
	myPrint(cmd_line);
	writeError();
	continue;
      }

      
      for (int i = 0; i < cmd_len; ++i){
	if (!isspace(cmd_line[i]))//((cmd_line[i] != ' ') && (cmd_line[i] != '\t')
	  //(cmd_line[i] != '\n')){
	  {
	    //printf("%d\n", is_blank_line);
	    is_blank_line = 0;
	    //break;
	  }
      }
      
      if (!pinput) {
	exit(0);
      }

      //printf("%d\n", is_blank_line);

      if (is_blank_line){
	continue;
      }
       
      if (!is_blank_line && is_batch){
	myPrint(cmd_line);
      }
      





      
      /// ** PARSE pinput[] BY ";" *** ///
      char jello[514];
      char *cmd_line_copy = strcpy(jello, cmd_line);
      int cmd_no = 0;
      cmd_buff_pre = strtok(cmd_line, ";"); // Also filter out newline?

      while (cmd_buff_pre){
	++cmd_no;
	cmd_buff_pre = strtok(NULL, ";");
      }
      char *all_commands[cmd_no+1];

      

      cmd_no = 0;
      cmd_buff = strtok(cmd_line_copy, ";");
      
      while (cmd_buff){
	all_commands[cmd_no] = cmd_buff;
	++cmd_no;
	cmd_buff = strtok(NULL, ";");
      }
      all_commands[cmd_no] = NULL; // IS THIS NECESSARY??
      /// ** // ** ///

      //printf("%d\n", cmd_no);
      //for (int i = 0; i < cmd_no; ++i){    
      //	printf("all_commands[%d] = %s\n", i,all_commands[i]);
      //}
      
      /// ** WHAT IS CURRENT COMMAND? ** ///
      int i; // replace cmd_buff with all_commands[i] (for multipleCommand)
      // replace all_commands[] with something else??
      
      for (i = 0; i < cmd_no; ++i){ // The ALL_COMMANDS[i] for LOOP !!!!!!!!!
	int is_Redr = 0,is_adRedr = 0; 
	
	//printf("all_commands[%d] = %s\n", i, all_commands[i]);
	char redr_buff[514];
	char *cmd_buff_copy2 = strcpy(redr_buff, all_commands[i]);

	
       
      char hello[514];
      char *cmd_buff_copy = strcpy(hello, all_commands[i]);
      space_token = strtok(all_commands[i], " \n\t"); // HOW DO I DO THE SAME FOR TAB? \t

      if (space_token == NULL){
	if (!is_batch){
	writeError();
	}
	continue;
      }
      ctr = 0;
      while (space_token != NULL){
	++ctr;
	space_token = strtok(NULL, " \n\t");
      }

      // printf("ctr = %d\n",ctr);

      char *cmd_array[ctr+1]; // set this to [ctr + 1]
      int ctr2 = 0;  
      space_token2 = strtok(cmd_buff_copy, " \n\t");

      //printf("cmd_buff_copy = %s\n", cmd_buff_copy);
      //printf("space_token2 = %s\n", space_token2);

      while (space_token2){
	cmd_array[ctr2] = space_token2;
	++ctr2;
	space_token2 = strtok(NULL, " \n\t");
	//printf("space_token2 is now = %s\n", space_token2);
      }

      cmd_array[ctr+1] = NULL;
      //printf("ctr2 = %d\n", ctr2);
	     
      // REPLACE space_token with cmd_array[0];  
      is_exit = str_eq(bigExit, cmd_array[0]);
      is_cd = str_eq(cd, cmd_array[0]);
      is_pwd = str_eq(pwd, cmd_array[0]);      
      /// ** // ** ///

      
      // myPrint(cmd_buff);
      /*
       printf("strlen(cmd_array[0]) = %d\n", (int) strlen(cmd_array[0]));
      printf("is_exit: %d\n",  is_exit);
      printf("is_cd: %d\n",  is_cd); 
      printf("is_pwd: %d\n",  is_pwd);
      int st_len = strlen(cmd_array[0]);
      printf("Is the last character the newline? %s\n",
        (('\n' == cmd_array[0][st_len-1])? "Yes" : "No"));
      */

      /*
      if (ctr == 0){ // If the command line has only whitespaces.
	writeError(cmd_buff);
	continue;
      }
      */
      
      if (is_exit){ // IF EXIT
	if (ctr != 1){
	  writeError();
	  continue;
	}
	exit(0);
      }
      else if (is_cd) {
	// Is the number of arguments (i.e. ctr) greater than one?

	//printf("is ctr > 1? %s\n", ((ctr > 1)?  "Yes" : "No"));

	// The last space_token cannot be just the newline (or tab) character!!
	// if (space_token != NULL && (space_token[0] != '\n')
	//   && (space_token[0] != '\t'))
	if (ctr > 1 && (cmd_array[1][0] != '\n') && (cmd_array[1][0] != '\t')) {
	  if (ctr > 2){
	    writeError();
	    continue;
	  }
	  if (str_eq(dotdot, cmd_array[1])){ // SEE IF space_token == ".." or not
	    chdir("..");
	  } else {	   
	    char *new_path = string_append("",cmd_array[1]);
	    int go_to_file = chdir(cmd_array[1]);
	    if (go_to_file){
	      writeError();
	    }
	    free(new_path);
	  }
	  
	} else {
	  // Go to #HOME
	  chDir = chdir((getenv("HOME")));
	  if (chDir){
	    writeError();
	    continue;
	  }
	}

      } else if (is_pwd) {
	// my_path = getcwd(my_path, 100);  // WHY DOES THIS ALWAYS EVALUATE TO TRUE??
	if (getcwd(my_path,100) && (ctr == 1)){
	  myPrint(my_path);
	  myPrint("\n");
	}
	else {
	  writeError();
	  continue;
	}
      } else {

	
	if (strstr(cmd_buff_copy2,">+")){
	  is_adRedr = 1;
	} else if (strstr(cmd_buff_copy2,">")) {
	  is_Redr = 1;
	}
	
	//printf("cmd_buff_copy2 = %s\n", cmd_buff_copy2);
	
	
	//printf("cmd_buff_copy2 = %s\n", cmd_buff_copy2);
	//printf("is_adRedr = %d\n", is_adRedr);
	//printf("is_Redr = %d\n", is_Redr);
	
	
	if (is_adRedr){  // ** IF ADVANCED REDIRECTION ** //
	  char *adRedr_token = strtok(redr_buff,">+");
	  char *adRedr_cmd[3];
	 
	  for (int k = 0; k < 3; ++k){
	    adRedr_cmd[k] = adRedr_token;
	    adRedr_token = strtok(NULL, ">+"); // EVEN IF OVERBOARD, KEEPS BEING NULL, RIGHT?
	  }
	  
	  if ((adRedr_cmd[0] == NULL)||
	      (adRedr_cmd[1] == NULL) ||
	      (adRedr_cmd[2] != NULL)){
	    writeError();
	    continue;
	  }
	  
	  // Need before[] and after[] arrays.
	  
	  // before[]
	  char before_buff[514];
	  char *before_copy = strcpy(before_buff, adRedr_cmd[0]);

	  int count = 0;	  
	  char *before_token1 = strtok(adRedr_cmd[0], " \t\n");
	  if (before_token1 == NULL){
	    writeError();
	    continue;
	  }
	  
	  while (before_token1) {
	    ++count;
	    before_token1 = strtok(NULL, " \t\n");
	  }

	  char *before[count+1];
	  char *before_token2 = strtok(before_copy, " \t\n");
	  count = 0;
	  while (before_token2){
	    before[count] = before_token2;
	    before_token2 = strtok(NULL, " \t\n");
	    ++count;
	  }
	  //int before_len = count; // BEFORE_LEN!!!!!!!!
	  before[count] = NULL;
	  //

	  // after[]
	  char after_buff[514];
	  char *after_copy = strcpy(after_buff, adRedr_cmd[1]);

	  count = 0;
	  char *after_token1 = strtok(adRedr_cmd[1], " \t\n");
	  while(after_token1){
	    ++count;
	    after_token1 = strtok(NULL, " \t\n");
	  }
	  if (count != 1){
	    writeError();
	    continue;
	  }
	  
	  char *after[2];
	  after[0] = strtok(after_copy, " \t\n");
	  after[1] = NULL;

	  if (strstr(after[0],">")){ // If there is a ">" after ">+"
	    writeError();
	    continue;
	  }

	  if (strcmp(before[0],"exit") == 0 ||
	      strcmp(before[0], "cd") == 0 ||
	      strcmp(before[0], "pwd") == 0){
	    writeError();
	    continue;
	  }
	  
	  // ** Look at before[] and after[] ** //
	  
	  //for (int k = 0; k < before_len; ++k){
	  //  printf("before[%d] = %s\n", k, before[k]);
	  // }
	  //printf("after[0] = %s\n", after[0]);
	  
	  // ** / ** //
	  
	  

	  // File time!!
	  //
	  int fd_TEST = open(after[0],O_RDWR);
	  //printf("%d\n", fd_TEST);
	  if (fd_TEST != -1){
	    // Write into the beginning of the file.
	    // How the hell do I do that?
	    close(fd_TEST);
	    int fd_new = creat("new_file_tmp",S_IWUSR|S_IRUSR);
	    int original = dup(1);
	    dup2(fd_new,1);
	    close(fd_new);

	    int pid = fork();
	    if (pid == 0){
	      int t = execvp(before[0],before);
	      if (t){
		writeError();
		exit(0);
	      }
	      exit(0);
	    } else {
	      int stat;
	      waitpid(pid,&stat,0);
	      FILE *old_file = fopen(after[0], "r");
	      char final_buff[1026];
	      char *file_input = fgets(final_buff,1026, old_file);

	      while (file_input){
		myPrint(file_input);
		file_input = fgets(final_buff,1026, old_file);
	      }
	      fclose(old_file);
	      //close(fd_new);
	      // myPrint(form fd_TEST to fd_new)
	      // rename(fd_new to fd_TEST)
	      
	      rename("new_file_tmp" ,after[0]);
	      dup2(original,1);
	      close(original);
	    }

	  } else {
	    int fd = creat(after[0], S_IRUSR|S_IWUSR); // Advanced Redirection.
	    if (fd == -1){
	      writeError();
	      continue;
	    }
	      
	    int original = dup(1);
	    dup2(fd,1);

	    int pid = fork();
	    if (pid == 0){
	      int t = execvp(before[0],before);
	      if (t){
		writeError(); // won't the error be written to the file now?
		exit(0);
	      }
	      exit(0);
	    } else {
	      int stat;
	      waitpid(pid,&stat,0);
	      close(fd);
	      dup2(original,1);
	    }  
	  }	  
	  //
	  
	} else if (is_Redr){  // ** IF SIMPLE REDIRECTION ** //

	  char *redr_token = strtok(redr_buff,">");
	  char *redr_cmd[3];

	  for (int k = 0; k < 3; ++k){
	    redr_cmd[k] = redr_token;
	    redr_token = strtok(NULL, ">"); 
	  }

	  if ((redr_cmd[0] == NULL)||
	      (redr_cmd[1] == NULL) ||
	      (redr_cmd[2] != NULL)){
	    writeError();
	    continue;
	  }

	  // Need before[] and after[] arrays.

	  // before[]
	  char before_buff[514];
	  char *before_copy = strcpy(before_buff, redr_cmd[0]);

	  int count = 0;
	  char *before_token1 = strtok(redr_cmd[0], " \t\n");
	  if (before_token1 == NULL){
	    writeError();
	    continue;
	  }
	  
	  while (before_token1){
	    ++count;
	    before_token1 = strtok(NULL, " \t\n");
	  }

	  char *before[count+1];
	  char *before_token2 = strtok(before_copy, " \t\n");
	  count = 0;
	  while (before_token2){
	    before[count] = before_token2;
	    before_token2 = strtok(NULL, " \t\n");
	    ++count;
	  }
	  // int before_len = count; // Do I actually need this? BEFORE_LEN!!!!!
	  before[count] = NULL;
	  //

	  // after[]
	  char after_buff[514];
	  char *after_copy = strcpy(after_buff, redr_cmd[1]);

	  count = 0;
	  char *after_token1 = strtok(redr_cmd[1], " \t\n");
	  while (after_token1){
	    ++count;
	    after_token1 = strtok(NULL, " \t\n");
	  }
	  if (count != 1){
	    writeError();
	    continue;
	  }
	  
	  char *after[2];
	  after[0] = strtok(after_copy, " \t\n");
	  after[1] = NULL;

	  if (strcmp(before[0],"exit") == 0 ||
	      strcmp(before[0], "cd") == 0 ||
	      strcmp(before[0], "pwd") == 0){
	    writeError();
	    continue;
	  }
	  
	  // ** Look at before[] and after[] ** //
	  
	  //for (i = 0; i < before_len; ++i){
	  //  printf("before[%d] = %s\n", i, before[i]);
	  //}
	  //printf("after[0] = %s\n", after[0]);
	  
	  // ** / ** //
	  

	  
	  //
	  int fd_TEST = open(after[0], O_RDWR);
	  //printf("fd_TEST = %d\n", fd_TEST);
	  if (fd_TEST != -1){
	    close(fd_TEST);
	    writeError();
	    continue;
	  } else {
	    int fd = creat(after[0], S_IWUSR|S_IRUSR); // ASK ABOUT MODE.
	    if (fd == -1){
	      writeError();
	      continue;
	    }
	    int original = dup(1);
	    dup2(fd,1);

	    //printf("Testing\n");
	    int pid = fork();
	    if (pid == 0){
	      int t = execvp(before[0],before);
	      //printf("execvp(before[0],before) = %d\n",t);
	      if (t){			
		writeError(); // won't the error be written to the file now?
		exit(0);
	      }
	      exit(0);
	    } else {
	      int stat;
	      waitpid(pid,&stat,0);
	      close(fd);
	      dup2(original,1);
	      
	    }
	  }

	  } else {  // IF NO REDIRECTION -- TESTING REST ****
	int pid = fork();
	if (pid == 0){
	  int test = execvp(cmd_array[0], cmd_array); 
	  if (test){
	   writeError();
	   exit(0);
	  }
	  exit(0);	  
	  
	} else {
	  int status;
	  waitpid(pid,&status,0);	  
	}

	} //TESTING REST ******

      }
      //printf("End of loop %d\n", i);
      } // END OF for (all_commands)

      
      
      
      
      
      
      
      /// ** // ** ///


      
    }
}



