#include "jobs.h"
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

Job* jobs;
Job* last_job;
size_t jobs_size = 0;

Job* add_new_job(int pgid)
{
    printf("New job!\n");
    Job new_job;
    new_job.jobState = SUSPENED;
    new_job.pgid = pgid;
    jobs_size++;
    jobs = (Job*)realloc(jobs, jobs_size * sizeof(Job));
    jobs[jobs_size - 1] = new_job;
    return &jobs[jobs_size - 1];
}

int remove_job(int pgid)
{
    Job* new_jobs = (Job*)malloc(sizeof(Job) * (jobs_size - 1));
    int j = 0;
    for(size_t i = 0; i < jobs_size; i++)
    {
        if(jobs[i].pgid != pgid)
        {
            new_jobs[j] = jobs[i];
            j++;
        }
    }
    free(jobs);
    jobs = new_jobs;
    jobs_size--;
    return 0;
}

int change_job_state(int pgid, JobState jobState)
{
    for(size_t i = 0; i < jobs_size; i++)
    {
        if(jobs[i].pgid == pgid)
        {
            jobs[i].jobState = jobState;
            if(jobState == RUNNING)
            {
                killpg(pgid, SIGCONT);
            }
            else if(jobState == SUSPENED)
            {
                killpg(pgid, SIGSTOP);
            }
            return 0;
        }
    }
    return -1;
}
