#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/time.h> 
#include <stdlib.h>


typedef struct matrix {
    int n;
    long long** val;
} matrix;


matrix create_matrix(int n) {
    matrix m;
    m.n = n;
    m.val = malloc(n * sizeof(long long*));
    for(int i = 0; i < n; i++)
        m.val[i] = malloc(n * sizeof(long long));
    return m;
}


void fill_rand(matrix* m) {
    for(int i = 0; i < m->n; i++) {
        for(int j = 0; j < m->n; j++) {
            m->val[i][j] = rand() % 10;
        }
    }
}


int multiply_matrices(matrix* res, matrix* m1, matrix* m2) {
    if(m1->n != m2->n) {
        return -1;
    }

    int n = m1->n;

    *res = create_matrix(n);

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            res->val[i][j] = 0;
            for (int k = 0; k < n; k++) {
                res->val[i][j] += m1->val[i][k] * m2->val[k][j];
            }
        }
    }

    return 0;
}



int multiply_by_transposed(matrix* res, matrix* m1, matrix* m2) {
    if(m1->n != m2->n) {
        return -1;
    }

    int n = m1->n;

    *res = create_matrix(n);

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            res->val[i][j] = 0;
            for (int k = 0; k < n; k++) {
                res->val[i][j] += m1->val[i][k] * m2->val[j][k];
            }
        }
    }

    return 0;
}




int transpose_matrix(matrix* m) {
    int n = m->n;
    long long** copy = malloc(n * sizeof(long long*));
    for(int i = 0; i < n; i++) {
        copy[i] = malloc(n * sizeof(long long));
        memcpy(copy[i], m->val[i], sizeof(long long*) * n);
    }


    for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
            m->val[j][i] = copy[i][j];
        }
    }
    return 0;
}


void print_matrix(matrix* m) {
    for(int i = 0; i < m->n; i++) {
        for(int j = 0; j < m->n; j++) {
            printf("%lld ", m->val[j][i]);
        }
        printf("\n");
    }

}


int main() {
    srand(time(NULL));

    matrix m1 = create_matrix(2000), m2 = create_matrix(2000), m3;
    fill_rand(&m1);
    fill_rand(&m2);

    struct timeval t1, t2;
    double elapsedTime;

    gettimeofday(&t1, NULL);
    multiply_matrices(&m3, &m1, &m2);
    gettimeofday(&t2, NULL);
    elapsedTime = (t2.tv_sec - t1.tv_sec) * 1000.0;      // sec to ms
    elapsedTime += (t2.tv_usec - t1.tv_usec) / 1000.0;   // us to ms
    printf("normal: %f ms.\n", elapsedTime);

    transpose_matrix(&m2);

    gettimeofday(&t1, NULL);
    multiply_by_transposed(&m3, &m1, &m2);
    gettimeofday(&t2, NULL);
    elapsedTime = (t2.tv_sec - t1.tv_sec) * 1000.0;      // sec to ms
    elapsedTime += (t2.tv_usec - t1.tv_usec) / 1000.0;   // us to ms
    printf("transposed: %f ms.\n", elapsedTime);

    return 0;
}
