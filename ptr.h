#ifndef PTR_H
#define PTR_H

//#define _XOPEN_SOURCE 700

// Needed for intrinsics
//#//include <malloc.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/custom.h>

#if defined(__WIN32__) || defined(__WIN32)
#ifndef _WIN32
#define _WIN32
#endif
#endif

#ifndef _WIN32
#include <unistd.h>
#include <sys/mman.h>
#include <string.h>
#include <errno.h>
#endif

// Microsoft typing
#ifdef _WIN32
typedef __int16 int16_t;
typedef unsigned __int16 uint16_t;
#else
#include <stdint.h>
#define FALSE 0
#define TRUE 1
#endif

enum ptr_type {
	PTR_MALLOC,
	PTR_MMAP,
	PTR_VIRTUALALLOC,
	PTR_NULL // For ignoring
};

struct ptr_struct {
	char *begin;
	char *alloc_begin;
	intnat length;
	intnat align;
	enum ptr_type type;
};

#define Struct_val(x) ((struct ptr_struct *)Data_custom_val(x))
#define Begin_val(x) Struct_val(x)->begin
#define Alloc_begin_val(x) Struct_val(x)->alloc_begin
#define Length_val(x) Struct_val(x)->length
#define Align_val(x) Struct_val(x)->align
#define Type_val(x) Struct_val(x)->type


static void ptr_finalize(value v) {
	struct ptr_struct *p = Struct_val(v);
	switch (p->type) {
		case PTR_MALLOC:
			free(p->alloc_begin);
			break;
		case PTR_MMAP:
#ifdef _WIN32
			UnmapViewOfFile(p->alloc_begin);
			break;
#else
			munmap(p->alloc_begin, p->length);
			break;
#endif
		case PTR_VIRTUALALLOC:
#ifdef _WIN32
			VirtualFree(p->alloc_begin, 0, MEM_RELEASE);
			break;
#else
			break;
#endif
		case PTR_NULL:
			break;
	}
}


/* COMPARE */
static int ptr_compare(value a_val, value b_val) {
	int len_a = Length_val(a_val);
	int len_b = Length_val(b_val);

	if(len_a != len_b) {
		return((len_a < len_b) - (len_b < len_a));
	} else {
		return(memcmp(Begin_val(a_val), Begin_val(b_val), len_a));
	}
}

static struct custom_operations generic_ptr_opts = {
	"c_ptr",
	ptr_finalize,
	ptr_compare/*custom_compare_default*/,
	custom_hash_default,
	custom_serialize_default,
	custom_deserialize_default
};

#endif /* PTR_H */
