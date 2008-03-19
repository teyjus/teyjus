#include <stdlib.h>
#include <stdio.h>
#include "module.h"
#include "file.h"
#include "message.h"

int verbosity = 0;
char* top_modname = NULL;

const char *version_string = "tjlink 1.0";

const char *help_string =
"Usage: tjlink [OPTION...] MODULENAME\n"
"The teyjus linker - combines compiled lambda-prolog modules into a single\n"
"program\n"
"\n"
"  -q, --quiet                Don't produce any output\n"
"  -v, --verbose              Produce verbose output\n"
"  -h, --help                 Give this help list\n"
"  -V, --version              Print program version\n"
"\n"
"Mandatory or optional arguments to long options are also mandatory or optional\n"
"for any corresponding short options.\n"
"\n"
  "Report bugs to <SHolte@gmail.com>.";

const char *usage_string = 
"Usage: tjlink [OPTION...] MODULENAME\n"
  "Try `tjlink --help' for more information.";

void help()
{
  printf("%s", help_string);
  exit(0);
}

void usage(int code)
{
  printf("%s", usage_string);
  exit(code);
}

void version()
{
  printf("%s", version_string);
  exit(0);
}
  
void parse_args(int argc, char *argv[])
{
  int i;

  for(i = 1; i < argc; i++)
  {
    if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--verbose"))
      verbosity++;
    else if (!strcmp(argv[i], "-q") || !strcmp(argv[i], "--quiet"))
      verbosity--;
    else if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help"))
      help();
   else if (!strcmp(argv[i], "-V") || !strcmp(argv[i], "--version"))
      version();
    else if (argv[i][0] == '\0' || argv[i][0] == '-')
      usage(0);
    else
    {
      if (top_modname == NULL)
        top_modname = argv[i];
      else
      {
        printf("Error: More than one module file given.\n");
        usage(1);
      }
    }
  }

  if (top_modname == NULL)
  {
    printf("Error: No module file given.\n");
    usage(1);
  }
}
    

int main(int argc, char* argv[])
{
  parse_args(argc, argv);
  
  EM_TRY
  {
    InitAll();
    LoadTopModule(top_modname);
    WriteAll(top_modname);
  }
  EM_CATCH
  {
    bad("Linking aborted due to an exception.\n");
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
