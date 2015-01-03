#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <sys/sysinfo.h>

void printint(int x) {
	printf("%d\n", x);
}

void printfloat(float x) {
	printf("%f", x);
}

void (*__parac_parallel_for_thread)(int* i) = NULL;
void* __parac_parallel_for_ebp = NULL;
void* __parac_parallel_for_esp = NULL;

struct Thread {
	int position;
	int limit;
	pthread_t ref;
}* threads = NULL;

int __parac_get_thread_count() {
	return get_nprocs();
}

// http://bisqwit.iki.fi/story/howto/openmp/#LoopDirectiveFor
void* __parac_run_parallel_for_thread(void *data) {
	struct Thread* thread = data;
	for (; thread->position < thread->limit; ++thread->position) {
		__parac_parallel_for_thread(&thread->position);
	}
	return NULL;
}

void __parac_run_parallel_for(int start, int end) {
	const int count = __parac_get_thread_count();
	const int length = end - start;
	struct Thread* threads = malloc(count * sizeof(struct Thread));
	int i;
	for (i = 0; i < count; ++i) {
		threads[i].position = start + i * length / count;
		threads[i].limit = start + (i + 1) * length / count;
		pthread_create(&threads[i].ref, NULL, __parac_run_parallel_for_thread,
				&threads[i]);
	}
	for (i = 0; i < count; ++i)
		pthread_join(threads[i].ref, NULL);
	free(threads);
}
