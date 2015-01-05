int k;

void printint(int x);

void foo(int a) {
	int i;
#pragma omp parallel for
	for (i = 0; i < 10; i++)
		printint(i);
	if (k > 1000)
		printint(42);
	printint(k++);
	printint(k);
}

int main() {
	k = 1337;
	foo(42);
	return 0;
}
