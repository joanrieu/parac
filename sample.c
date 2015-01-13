int* k;
int b;

void printint(int x);
void printfloat(float x);

void foo(float a) {
	int i;
	i = 1234;
	printint(i);
	printint(i);
	printint(i);
	printint(i);
	printint(i);
#pragma omp parallel for
	for (i = 0; i < 10; i++)
		printint(i);
	if (k[1] > 1000)
		printfloat(a);
	printint(k[1]);
}

void foo(float b);

float bar(float x) {
	printint(!x);
	return -x;
}

int main() {
	int l[2];
	l[1] = 1336+1;
	k = l;
	foo(l[1]);
	printfloat(bar(42));
	if (!!1234)
		printint(1234);
	if (0.)
		printfloat(1234);
	else
		printfloat(5678);
	return 0;
}
