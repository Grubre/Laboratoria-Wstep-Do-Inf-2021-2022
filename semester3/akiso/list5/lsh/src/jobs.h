#ifndef JOBS_H
#define JOBS_H

#include <stddef.h>

typedef enum JobState {
    RUNNING,
    SUSPENED
} JobState;

typedef struct Job
{
    int pgid;
    JobState jobState;
} Job;

extern Job* jobs;
extern Job* last_job;
extern size_t jobs_size;

Job* add_new_job(int pgid);
int remove_job(int pgid);
int change_job_state(int pgid, JobState jobState);

#endif // !JOBS_H
