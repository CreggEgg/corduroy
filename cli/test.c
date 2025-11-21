#include "stdio.h"
extern int printint(int);

int printint(int x) {
	printf("%d", x);
}

int println(char *str) {
	printf("%s", str);
}

