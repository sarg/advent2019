#include <string.h>
#include <stdio.h>

#define ulong unsigned long
#define CMP(a,b) (a>b?-1:a==b?0:1)
#define MCMP(x,a,b,c,d) CMP(x[a],x[b])+CMP(x[a],x[c])+CMP(x[a],x[d])
#define gravity(x, v) \
    v[0] += MCMP(x, 0, 1, 2, 3); \
    v[1] += MCMP(x, 1, 0, 2, 3); \
    v[2] += MCMP(x, 2, 1, 0, 3); \
    v[3] += MCMP(x, 3, 1, 2, 0); \

#define velocity(x, v) \
    x[0] += v[0]; \
    x[1] += v[1]; \
    x[2] += v[2]; \
    x[3] += v[3];

#define GV(x,v) gravity(x,v);velocity(x,v);
#define EQ(a,b) a[0]==b[0] && a[1] == b[1] && a[2] == b[2] && a[3] == b[3]
#define PP(x) for (int p = 0; p < 4; p++) { printf("% 3ld", x[p]); } printf("\n");

ulong cycle(long x[]) {
    long v[4] = { 0, 0, 0 };
    long v2[4] = { 0, 0, 0 };

    long x2[4]; memcpy(x2, x, 4 * sizeof(ulong));

    ulong i = 0;
    do {
        GV(x, v);
        GV(x2, v2);GV(x2, v2);

        i++;
    } while(!(EQ(x, x2) && EQ(v, v2)));

    return i;
}

ulong gcd(ulong a, ulong b){ 
    if (a == 0) 
        return b;  
    return gcd(b % a, a);  
 } 
  
   
// Function to return LCM of two numbers  
ulong lcm(ulong a, ulong b) {  
    return (a*b)/gcd(a, b);  
}

int main() {
    long mx[4] = { -8, -5, 11, 1};
    long my[4] = { -9, 2, 8, -4};
    long mz[4] = { -7, -1, -14, -11};

    ulong cx = cycle(mx);
    ulong cy = cycle(my);
    ulong cz = cycle(mz);

    printf("lcm(%lu, %lu, %lu) = %lu\n", cx, cy, cz, lcm(lcm(cx, cy), cz));
}
