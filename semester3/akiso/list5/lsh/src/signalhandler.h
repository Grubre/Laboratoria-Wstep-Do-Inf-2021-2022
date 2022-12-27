#ifndef SIGNAL_HANDLER
#define SIGNAL_HANDLER
#include <setjmp.h>
#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>

#define LOOP_BEGIN_JMP_POINT 96

void signal_handler(int sig);
void fork_sig_handler();
void bg_process_sig_handler();


#endif // !SIGNAL_HANDLER
