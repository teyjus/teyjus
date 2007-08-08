#include <stdlib.h>
#include <stdio.h>
#include <argp.h>
#include "module.h"
#include "file.h"
#include "message.h"

int verbosity = 0;
char* top_modname=NULL;

const char *argp_program_version = "tjlink 1.0";
const char *argp_program_bug_address = "<SHolte@gmail.com>";

static char doc[] =
    "The teyjus linker - combines compiled lambda-prolog modules into a single program";

static char args_doc[] = "MODULENAME";

static struct argp_option options[] = {
  {"verbose",  'v', 0,      0,  "Produce verbose output" },
  {"quiet",    'q', 0,      0,  "Don't produce any output" },
  {"silent",   's', 0,      OPTION_ALIAS },
  {"input",    'i', ".ext", 0,  "Extensions on bytecode files"},
  {"output",   'o', ".ext", 0,  "Extensions on linkcode files" },
  { 0 }
};

static error_t parse_opt (int key, char *arg, struct argp_state *state)
{
       /* Get the input argument from argp_parse, which we
  know is a pointer to our arguments structure. */
  //struct arguments *arguments = state->input;
     
  switch (key)
  {
    case 'q': case 's':
      verbosity--;
      break;
    case 'v':
      verbosity++;
      break;
    case 'i':
      LK_FILE_ByteCodeExt = arg;
      break;
    case 'o':
      LK_FILE_LinkCodeExt = arg;
      break;
     
    case ARGP_KEY_ARG:
      if (state->arg_num >= 1)
        argp_usage (state);
      top_modname = arg;
      break;
     
    case ARGP_KEY_END:
      if (state->arg_num < 1)
        argp_usage (state);
      break;
     
    default:
      return ARGP_ERR_UNKNOWN;
  }
  return 0;
}
     
static struct argp argp = { options, parse_opt, args_doc, doc };

int main(int argc, char* argv[])
{
  argp_parse (&argp, argc, argv, 0, 0, 0);
  
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
