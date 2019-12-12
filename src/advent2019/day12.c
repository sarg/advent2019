#include <stdio.h>

#define CMP(a,b) (a>b?-1:a==b?0:1)
#define MCMP(x,a,b,c,d) (CMP(x[a],x[b])+CMP(x[a],x[c])+CMP(x[a],x[d]))

__attribute__((always_inline))
void gravity(long x[], long v[]) {
    v[0] += MCMP(x, 0, 1, 2, 3);
    v[1] += MCMP(x, 1, 0, 2, 3);
    v[2] += MCMP(x, 2, 1, 0, 3);
    v[3] += MCMP(x, 3, 1, 2, 0);
}

__attribute__((always_inline))
void velocity(long x[], long v[]) {
    x[0] += v[0];
    x[1] += v[1];
    x[2] += v[2];
    x[3] += v[3];
}

#define GV(t) gravity(m ## t, v ## t);velocity(m ## t, v ## t);
#define STEP(i) GV(x ## i) GV(y ## i) GV(z ## i)
#define EQ(a,b) a[0]==b[0] && a[1] == b[1] && a[2] == b[2] && a[3] == b[3]
#define TEQ(t) EQ(t ## x ## 1, t ## x ## 2) && EQ(t ## y ## 1, t ## y ## 2) && EQ(t ## z ## 1, t ## z ## 2)
#define ALLEQ TEQ(m) && TEQ(v)

int main() {
    long mx1[4] = { -8, -5, 11, 1};
    long my1[4] = { -9, 2, 8, -4};
    long mz1[4] = { -7, -1, -14, -11};

    /* long mx1[4] = { -1, 2, 4, 3 }; */
    /* long my1[4] = { 0, -10, -8, 5 }; */
    /* long mz1[4] = { 2, -7, 8, -1 }; */

    long vx1[4] = { 0, 0, 0};
    long vy1[4] = { 0, 0, 0};
    long vz1[4] = { 0, 0, 0};

    /* long mx2[4] = { -1, 2, 4, 3 }; */
    /* long my2[4] = { 0, -10, -8, 5 }; */
    /* long mz2[4] = { 2, -7, 8, -1 }; */

    long mx2[4] = { -8, -5, 11, 1};
    long my2[4] = { -9, 2, 8, -4};
    long mz2[4] = { -7, -1, -14, -11};
    long vx2[4] = { 0, 0, 0};
    long vy2[4] = { 0, 0, 0};
    long vz2[4] = { 0, 0, 0};

    unsigned long i = 0;

    do {
        STEP(1);
        STEP(2);
        STEP(2);
        i++;
        if (i % 100000 == 0) {
           printf("%lu\n", i);
        }
    } while (!(ALLEQ));

    /* for(int i=0; i<10; i++) { */
    /*     printf("Step %d\n", i); */
    /*     for(int x=0; x<4; x++) { */
    /*         printf("pos=% 3ld % 3ld % 3ld vel=% 3ld % 3ld % 3ld\n", mx1[x], my1[x], mz1[x], vx1[x], vy1[x], vz1[x]); */
    /*     } */
    /*     printf("\n"); */

    /*     STEP(1); */

    /* } */

    printf("%lu\n", i);
}
