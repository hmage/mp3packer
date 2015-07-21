#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

// DELETEME
#include <caml/custom.h>

//#include "ptr.h"
#include <stdio.h>

// ONLY IF IT'S WINDOWS!
#if defined(__WIN32) || defined(_WIN32) || defined(__WIN32__)
#ifndef WINVER
#define WINVER 0x0501
#define _WIN32_WINNT 0x0501
#endif
#include <Windows.h>
#include <WinIoCtl.h>
#include <KtmW32.h>
#include <shellapi.h>
#include <fcntl.h>



#define win_bad(out,bad) {\
	(out) = caml_alloc(1, 1);\
	Store_field((out), 0, Val_int(bad));\
}

#define win_good(out,good) {\
	(out) = caml_alloc(1, 0);\
	Store_field((out), 0, (good));\
}

#define win_error(out,good,bad) {\
	if((bad) == 0) win_good(out,good)\
	else win_bad(out,bad)\
}




CAMLprim value uni_is_win() {
	return(Val_int(1));
}

/***********/
/* UNICODE */
/***********/

CAMLprim void uni_set_utf8_output() {
	SetConsoleOutputCP(CP_UTF8);
}

CAMLprim void uni_silly_print(value s) {
	CAMLparam1(s);
	wprintf(L"%S", String_val(s));
	fflush(stdout);
	CAMLreturn0;
}

// Convert a UTF8 string to a UTF16 string
// Optionally save the trailing null
CAMLprim value uni_utf16_of_utf8(value utf8_val, value include_null) {
	CAMLparam2(utf8_val, include_null);
	int err = 0;
	// Every OCaml string is null-terminated, so it's safe to add 1 to the length
	long utf8_len = Bool_val(include_null) ? string_length(utf8_val) + 1 : string_length(utf8_val);
	CAMLlocal2(unicode_val, out_val);

	if(utf8_len == 0) {
		unicode_val = caml_alloc_string(0);
	} else {
		int needed = MultiByteToWideChar(CP_UTF8, 0, String_val(utf8_val), utf8_len, NULL, 0);
		if(needed == 0) {
			err = GetLastError();
		} else {
			int n2;
			unicode_val = caml_alloc_string(sizeof(WCHAR) * needed);
			n2 = MultiByteToWideChar(CP_UTF8, 0, String_val(utf8_val), utf8_len, (LPWSTR)String_val(unicode_val), needed);
			if(n2 == 0) {
				err = GetLastError();
			}
		}
	}
	win_error(out_val, unicode_val, err);
	CAMLreturn(out_val);
}

CAMLprim value uni_utf8_of_utf16(value byte_len_val, value unicode_val) {
	CAMLparam2(byte_len_val, unicode_val);
	int err = 0;
	int len = Int_val(byte_len_val) / sizeof(WCHAR);
	CAMLlocal2(utf8_val, out_val);

	if(string_length(unicode_val) < sizeof(WCHAR)) {
		utf8_val = caml_alloc_string(0);
	} else {
		int needed = WideCharToMultiByte(CP_UTF8, 0, (LPCWSTR)String_val(unicode_val), len, NULL, 0, NULL, NULL);
		if(needed == 0) {
			err = GetLastError();
		} else {
			int n2;
			utf8_val = caml_alloc_string(needed);
			n2 = WideCharToMultiByte(CP_UTF8, 0, (LPCWSTR)String_val(unicode_val), len, String_val(utf8_val), needed, NULL, NULL);
			if(n2 == 0) {
				err = GetLastError();
			}
		}
	}
	win_error(out_val, utf8_val, err);
	CAMLreturn(out_val);
}

CAMLprim value uni_active_of_utf16(value byte_len_val, value unicode_val) {
	CAMLparam2(byte_len_val, unicode_val);
	int err = 0;
	int len = Int_val(byte_len_val) / sizeof(WCHAR);
	CAMLlocal2(utf8_val, out_val);

	printf("ACP: %d\n", GetACP());

	if(string_length(unicode_val) < sizeof(WCHAR)) {
		utf8_val = caml_alloc_string(0);
	} else {
		int needed = WideCharToMultiByte(GetACP(), 0, (LPCWSTR)String_val(unicode_val), len, NULL, 0, NULL, NULL);
		if(needed == 0) {
			err = GetLastError();
		} else {
			int n2;
			utf8_val = caml_alloc_string(needed);
			n2 = WideCharToMultiByte(GetACP(), 0, (LPCWSTR)String_val(unicode_val), len, String_val(utf8_val), needed, NULL, NULL);
			if(n2 == 0) {
				err = GetLastError();
			}
		}
	}
	win_error(out_val, utf8_val, err);
	CAMLreturn(out_val);
}


CAMLprim value uni_utf8_of_utf16_and_length(value unicode_val, value len_val) {
	CAMLparam2(unicode_val, len_val);
	int err = 0;
	int len_in_wchars = Int_val(len_val);
	CAMLlocal2(utf8_val, out_val);

	if(len_in_wchars < 1) {
		utf8_val = caml_alloc_string(0);
	} else {
		int needed = WideCharToMultiByte(CP_UTF8, 0, (LPCWSTR)String_val(unicode_val), len_in_wchars, NULL, 0, NULL, NULL);
		if(needed == 0) {
			err = GetLastError();
		} else {
			int n2;
			utf8_val = caml_alloc_string(needed);
			n2 = WideCharToMultiByte(CP_UTF8, 0, (LPCWSTR)String_val(unicode_val), len_in_wchars, String_val(utf8_val), needed, NULL, NULL);
			if(n2 == 0) {
				err = GetLastError();
			}
		}
	}
	win_error(out_val, utf8_val, err);
	CAMLreturn(out_val);
}

CAMLprim value uni_get_utf16_command_line() {
	CAMLparam0();
	CAMLlocal2(str_val, out_val);
	int err = 0;
	LPWSTR com_str = GetCommandLineW();
	int needed = sizeof(WCHAR) * wcslen(com_str);
	if(needed == 0) {
		err = GetLastError();
	} else {
		str_val = caml_alloc_string(needed);
		memcpy(String_val(str_val), com_str, needed);
	}
	win_error(out_val, str_val, err);
	CAMLreturn(out_val);
}

CAMLprim value uni_get_utf8_argv() {
	CAMLparam0();
	CAMLlocal3(str_val, array_val, out_val);

	int num_args;
	int needed;
	int err = 0;
	LPWSTR com_str = GetCommandLineW();

	LPWSTR *argv = CommandLineToArgvW(com_str, &num_args);
	if(argv == NULL) {
		err = GetLastError();
	} else {
		int i;
		int needed;
		LPWSTR str;
		array_val = caml_alloc_tuple(num_args);
//		printf("NUM ARGS: %d\n", num_args);
		for(i = 0; i < num_args; i++) {
//			printf("i = %d\n", i);
			str = argv[i];
			needed = WideCharToMultiByte(CP_UTF8, 0, str, -1, NULL, 0, NULL, NULL);
//			printf("needed = %d\n", needed);
			if(needed == 0) {
				err = GetLastError();
				break;
			} else {
				// WideCharToMultiByte with -1 returns the terminating null char as well
				// Luckily OCaml allocates a terminating null after any string
				str_val = caml_alloc_string(needed - 1);
				needed = WideCharToMultiByte(CP_UTF8, 0, str, -1, String_val(str_val), needed, NULL, NULL);
//				printf("needed2 = %d\n", needed);
				if(needed == 0) {
					err = GetLastError();
					break;
				} else {
					Store_field(array_val, i, str_val);
				}
			}
		}
	}

	win_error(out_val, array_val, err);
	CAMLreturn(out_val);
}


// OPENFILE
// Basically copied from otherlibs\win32unix\open.c

static int open_access_flags[12] = {
  GENERIC_READ, GENERIC_WRITE, GENERIC_READ|GENERIC_WRITE,
  0, 0, 0, 0, 0, 0, 0, 0, 0
};

static int open_create_flags[12] = {
  0, 0, 0, 0, 0, O_CREAT, O_TRUNC, O_EXCL, 0, 0, 0, 0
};

CAMLprim value uni_openfile_utf16(value path, value flags, value perm)
{
  int fileaccess, createflags, fileattrib, filecreate;
  SECURITY_ATTRIBUTES attr;
  HANDLE h;

  fileaccess = convert_flag_list(flags, open_access_flags);

  createflags = convert_flag_list(flags, open_create_flags);
  if ((createflags & (O_CREAT | O_EXCL)) == (O_CREAT | O_EXCL))
    filecreate = CREATE_NEW;
  else if ((createflags & (O_CREAT | O_TRUNC)) == (O_CREAT | O_TRUNC))
    filecreate = CREATE_ALWAYS;
  else if (createflags & O_TRUNC)
    filecreate = TRUNCATE_EXISTING;
  else if (createflags & O_CREAT)
    filecreate = OPEN_ALWAYS;
  else
    filecreate = OPEN_EXISTING;

  if ((createflags & O_CREAT) && (Int_val(perm) & 0200) == 0)
    fileattrib = FILE_ATTRIBUTE_READONLY;
  else
    fileattrib = FILE_ATTRIBUTE_NORMAL;

  attr.nLength = sizeof(attr);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle = TRUE;

  h = CreateFileW((LPCWSTR)String_val(path), fileaccess,
                 FILE_SHARE_READ | FILE_SHARE_WRITE, &attr,
                 filecreate, fileattrib, NULL);
  if (h == INVALID_HANDLE_VALUE) {
    win32_maperr(GetLastError());
    uerror("Unicode.openfile", Nothing); // The path here will be in UTF-16, so OCaml won't be able to print it right
  }
  return win_alloc_handle(h);
}




CAMLprim value uni_readdir_find_first_file_utf16(value name_val) {
	CAMLparam1(name_val);
	HANDLE handle;
	DWORD err = 0;
	WIN32_FIND_DATAW f;
	CAMLlocal3(out_val, tuple_val, out_str_val);

	handle = FindFirstFileW((LPCWSTR)String_val(name_val), &f);
	if(handle == INVALID_HANDLE_VALUE) {
		err = GetLastError();
		win_bad(out_val, err);
	} else {
		size_t str_len = wcslen(f.cFileName) + 1; // include the null
		tuple_val = caml_alloc_tuple(2);
		out_str_val = caml_alloc_string(str_len * sizeof(WCHAR));
		memcpy(String_val(out_str_val), f.cFileName, str_len * sizeof(WCHAR));
		Store_field(tuple_val, 0, win_alloc_handle(handle));
		Store_field(tuple_val, 1, out_str_val);
		win_good(out_val, tuple_val);
	}
	CAMLreturn(out_val);
}

CAMLprim value uni_readdir_find_next_file_utf16(value handle_val) {
	CAMLparam1(handle_val);
	HANDLE handle = Handle_val(handle_val);
	WIN32_FIND_DATAW f;
	DWORD err = 0;
	CAMLlocal2(out_val, out_str_val);

	if(FindNextFileW(handle, &f)) {
		size_t str_len = wcslen(f.cFileName) + 1;
		out_str_val = caml_alloc_string(str_len * sizeof(WCHAR));
		memcpy(String_val(out_str_val), f.cFileName, str_len * sizeof(WCHAR));
		win_good(out_val, out_str_val);
	} else {
		err = GetLastError();
		win_bad(out_val, err);
	}
	CAMLreturn(out_val);
}

CAMLprim value uni_readdir_find_close(value handle_val) {
	CAMLparam1(handle_val);
	HANDLE handle = Handle_val(handle_val);
	CAMLlocal1(out_val);

	if(FindClose(handle)) {
		win_good(out_val, Val_int(0));
	} else {
		win_bad(out_val, GetLastError());
	}
	CAMLreturn(out_val);
}



// Taken from stat.c
#include <sys/types.h>
#include <sys/stat.h>
#ifndef S_IFLNK
#define S_IFLNK 0
#endif
#ifndef S_IFIFO
#define S_IFIFO 0
#endif
#ifndef S_IFSOCK
#define S_IFSOCK 0
#endif
#ifndef S_IFBLK
#define S_IFBLK 0
#endif

static int file_kind_table[] = {
  S_IFREG, S_IFDIR, S_IFCHR, S_IFBLK, S_IFLNK, S_IFIFO, S_IFSOCK
};

CAMLprim value uni_stat_utf16(value name_val) {
	CAMLparam1(name_val);
	struct _stati64 buf;
	int ret;
	wchar_t *name = (wchar_t *)String_val(name_val);
	CAMLlocal2(out_val, tuple_val);

	ret = _wstati64(name, &buf);
	if(ret == -1) {
		win_bad(out_val, GetLastError());
	} else if(buf.st_size > Max_long) {
		// Too big to fit into an int
		win_bad(out_val, ERROR_ARITHMETIC_OVERFLOW);
	} else {
		tuple_val = caml_alloc_tuple(12);
		Store_field(tuple_val,  0, Val_int(buf.st_dev));
		Store_field(tuple_val,  1, Val_int(buf.st_ino));
		Store_field(tuple_val,  2, cst_to_constr(buf.st_mode & S_IFMT, file_kind_table, sizeof(file_kind_table) / sizeof(int), 0));
		Store_field(tuple_val,  3, Val_int(buf.st_mode & 07777));
		Store_field(tuple_val,  4, Val_int(buf.st_nlink));
		Store_field(tuple_val,  5, Val_int(buf.st_uid));
		Store_field(tuple_val,  6, Val_int(buf.st_gid));
		Store_field(tuple_val,  7, Val_int(buf.st_rdev));
		Store_field(tuple_val,  8, Val_int(buf.st_size)); // no int64 version
		Store_field(tuple_val,  9, copy_double((double)buf.st_atime));
		Store_field(tuple_val, 10, copy_double((double)buf.st_mtime));
		Store_field(tuple_val, 11, copy_double((double)buf.st_ctime));
		win_good(out_val, tuple_val);
	}
	CAMLreturn(out_val);
}

CAMLprim value uni_file_exists_utf16(value name_val) {
	CAMLparam1(name_val);
	struct _stati64 buf;
	int ret;
	wchar_t *name = (wchar_t *)String_val(name_val);
	CAMLlocal1(out_val);
	ret = _wstati64(name, &buf);
	CAMLreturn(Val_bool(ret != -1));
}
CAMLprim value uni_rename_utf16(value path1, value path2)
{
	CAMLparam2(path1, path2);
  static int supports_MoveFileEx = -1; /* don't know yet */
	wchar_t *wpath1;
	wchar_t *wpath2;
  BOOL ok;
	CAMLlocal1(out_val);

	wpath1 = (wchar_t *)String_val(path1);
	wpath2 = (wchar_t *)String_val(path2);

  if (supports_MoveFileEx < 0) {
    OSVERSIONINFO VersionInfo;
    VersionInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    supports_MoveFileEx =
      (GetVersionEx(&VersionInfo) != 0)
      && (VersionInfo.dwPlatformId == VER_PLATFORM_WIN32_NT);
  }
  if (supports_MoveFileEx > 0) {
    ok = MoveFileExW(wpath1, wpath2,
                    MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH |
                    MOVEFILE_COPY_ALLOWED);
	} else {
    ok = MoveFileW(wpath1, wpath2);
	}
	if(ok) {
		win_good(out_val, Val_int(0));
	} else {
		win_bad(out_val, GetLastError());
	}
  CAMLreturn(out_val);
  }
CAMLprim value uni_remove_utf16(value name_val) {
	CAMLparam1(name_val);
	CAMLlocal1(out_val);
	int err = 0;
	if(!DeleteFileW((wchar_t *)String_val(name_val))) {
		win_bad(out_val, GetLastError());
	} else {
		win_good(out_val, Val_int(0));
	}
	CAMLreturn(out_val);
}

#else

#define RETURN_INVALID {\
	CAMLlocal1(out_val);\
	out_val = caml_alloc(1, 1);\
	Store_field(out_val, 0, Val_int(1));\
	CAMLreturn(out_val);\
}

CAMLprim value uni_is_win() {
	return(Val_int(0));
}

CAMLprim void uni_set_utf8_output() {}

CAMLprim void uni_silly_print(value s) {
	CAMLparam1(s);
	printf("%s", String_val(s));
	fflush(stdout);
	CAMLreturn0;
}

// None of these functions need to exist in Linux
CAMLprim value uni_utf16_of_utf8(value utf8_val, value include_null) {
	CAMLparam2(utf8_val, include_null);
	RETURN_INVALID;
}

CAMLprim value uni_utf8_of_utf16(value unicode_val) {
	CAMLparam1(unicode_val);
	RETURN_INVALID;
}

CAMLprim value uni_active_of_utf16(value byte_len_val, value unicode_val) {
	CAMLparam2(byte_len_val, unicode_val);
	RETURN_INVALID;
}

CAMLprim value uni_utf8_of_utf16_and_length(value unicode_val, value len_val) {
	CAMLparam2(unicode_val, len_val);
	RETURN_INVALID;
}

CAMLprim value uni_get_utf16_command_line() {
	CAMLparam0();
	RETURN_INVALID;
}

CAMLprim value uni_get_utf8_argv() {
	CAMLparam0();
	RETURN_INVALID;
}

CAMLprim value uni_openfile_utf16(value path, value flags, value perm) {
	CAMLparam3(path, flags, perm);
	RETURN_INVALID;
}

CAMLprim value uni_readdir_find_first_file_utf16(value name_val) {
	CAMLparam1(name_val);
	RETURN_INVALID;
}
CAMLprim value uni_readdir_find_next_file_utf16(value handle_val) {
	CAMLparam1(handle_val);
	RETURN_INVALID;
}
CAMLprim value uni_readdir_find_close(value handle_val) {
	CAMLparam1(handle_val);
	RETURN_INVALID;
}

CAMLprim value uni_stat_utf16(value name_val) {
	CAMLparam1(name_val);
	RETURN_INVALID;
}

CAMLprim value uni_file_exists_utf16(value name_val) {
	CAMLparam1(name_val);
	RETURN_INVALID;
}
CAMLprim value uni_rename_utf16(value path1, value path2) {
	CAMLparam2(path1, path2);
	RETURN_INVALID;
}

CAMLprim value uni_remove_utf16(value name_val) {
	CAMLparam1(name_val);
	RETURN_INVALID;
}
#endif

