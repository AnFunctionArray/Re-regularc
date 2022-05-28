#include <limits.h>
#include <stdlib.h>
#include <pthread.h>

pthread_t thread;

#ifdef _WIN32

HANDLE hCurrModule;

void docall(const char *name, size_t szname, void *phashmap) {
	char cProcName[USHRT_MAX];
	sprintf(cProcName, "%.*s", szname, name);
	if (!hCurrModule) hCurrModule = GetModuleHandle(0);

	FARPROC pfunc = GetProcAddress(hCurrModule, cProcName);

	if (!pfunc) return;

	extern void global_han();

	global_han(cProcName, phashmap);

	//EXCEPTION_POINTERS* pexc;

	//__try {

		((void (*)(void* phashmap))pfunc)(phashmap);
	//}
	//__except (pexc=GetExceptionInformation(), EXCEPTION_EXECUTE_HANDLER) {
	//	__debugbreak();
	//}
}
#else
#include <dlfcn.h>
void docall(const char *name, size_t szname, void *phashmap) {
	char cProcName[USHRT_MAX];
	sprintf(cProcName, "%.*s", (int)szname, name);
	//if (!hCurrModule) hCurrModule = GetModuleHandle(0);

	void *dlhndl = dlopen(0, RTLD_LAZY);

	//FARPROC pfunc = GetProcAddress(hCurrModule, cProcName);

	void *pfunc = dlsym(dlhndl, cProcName);

	if (!pfunc) return;

	extern void global_han();

	global_han(cProcName, phashmap);

	((void (*)(void* phashmap))pfunc)(phashmap);
}

#endif