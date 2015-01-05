int* k;
int b;

void printint(int x);
void printfloat(float x);

void foo(float a) {
	int i;
#pragma omp parallel for
	for (i = 0; i < 10; i++)
		printint(i);
	if (k > 1000)
		printfloat(a);
	printint(k++);
	printint(k);
}

int main() {
	int l[1];
	l[0] = 1337;
	k = l;
	foo(42.);
	return 0;
}
