#ifndef __EEP0018_H__
#define __EEP0018_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <erl_driver.h>
#include <ei.h>

#include "yajl_gen.h"
#include "yajl_parse.h"

#define ERROR 1
#define OK 0

#define MAX_DEPTH   1024

int json_to_term(char* buf, int len, char** rbuf, int rlen);
int term_to_json(char* buf, int len, char** rbuf, int rlen);

#endif
