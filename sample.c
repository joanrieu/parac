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

float bar() {
	return 42;
}

int main() {
	int l[2];
	l[1] = 1336+1;
	k = l;
	foo(l[1]);
	printfloat(bar());
	return 0;
}
