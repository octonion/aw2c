#include <stdio.h>

/* Note that we're ignoring alw_loc in here, we're C programmers and we just don't care. */

void Test (int v, int *r, int *vr, int *n(void *), int *a(void *,int))
{
    int j;

    printf("v = %d\n", v);
    printf("vr = %d\n", *vr);
    printf("n = %d\n", *n(NULL));
    for (j = 1; j <= 3; ++j) printf("a(%d) = %d\n", j, *a(NULL,j));

    v++;
    ++(*vr);
    ++(*n(NULL));
    *r = 41;
    for (j = 1; j <= 3; ++j) ++(*a(NULL,j));

    printf("v = %d\n", v);
    printf("vr = %d\n", *vr);
    printf("n = %d\n", *n(NULL));
    for (j = 1; j <= 3; ++j) printf("a(%d) = %d\n", j, *a(NULL,j));
}
