#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/stat.h>

#ifdef _WIN32
#  define _WIN32_WINNT 0x0501
#  include <windows.h>
#  include <direct.h>
#else
#  include <unistd.h>
#  include <dlfcn.h>
#endif


#include "julia.h"
#include "uv.h"

#define PATHBUF 512

extern char *julia_home;

uv_lib_t *jl_dlopen_null()
{
    uv_lib_t *lib = malloc(sizeof(uv_lib_t));
    lib->errmsg = NULL;
#ifdef _WIN32
    GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS
        | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
        (LPCSTR)(&jl_load_dynamic_library),
        &lib->handle);
#else
    lib->handle = dlopen(NULL, RTLD_NOW);
#endif
    return lib;
}

uv_lib_t *jl_load_dynamic_library(const char *libname, const char *libversion)
{
    uv_lib_t *lib = malloc(sizeof(uv_lib_t));
    char path[PATHBUF];
    const char *ext =
#if defined(__APPLE__)
        ".dylib";
#elif defined(_WIN32)
        ".dll";
#else
        ".so";
#endif

    if (libversion) {
#ifdef __APPLE__
        snprintf(path, PATHBUF, "%s.%s%s", libname, libversion, ext);
#else
        snprintf(path, PATHBUF, "%s%s.%s", libname, ext, libversion);
#endif
    } else {
        snprintf(path, PATHBUF, "%s%s", libname, ext);
    }

    if (!uv_dlopen(path, lib))
        return lib;

    if (!uv_dlopen(libname, lib))
        return lib;

    JL_PRINTF(JL_STDERR, "could not load module %s: %s\n", libname, uv_dlerror(lib));
    jl_errorf("could not load module %s.%s: %s", libname, libversion, uv_dlerror(lib));
    uv_dlclose(lib);
    free(lib);
    return NULL;
}

void *jl_dlsym_e(uv_lib_t *handle, char *symbol) {
    void *ptr;
    int  error=uv_dlsym(handle, symbol, &ptr);
    if(error) ptr=NULL;
    return ptr;
}

void *jl_dlsym(uv_lib_t *handle, char *symbol)
{
    void *ptr;
    int  error = uv_dlsym(handle, symbol, &ptr);
    if (error != 0) {
        JL_PRINTF(JL_STDERR, "symbol could not be found %s (%d): %s\n", symbol, error, uv_dlerror(handle));
    }
    return ptr;
}
