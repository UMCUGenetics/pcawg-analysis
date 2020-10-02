AUTOMAKE_OPTIONS               = subdir-objects

extensiondir = $(EXTDIR)
extension_LTLIBRARIES          = libunmapped_filter.la

libunmapped_filter_la_CFLAGS   = $(guile_CFLAGS) $(htslib_CFLAGS)
libunmapped_filter_la_LIBADD   = $(guile_LIBS) $(htslib_LIBS)
libunmapped_filter_la_SOURCES  = unmapped_filter.c
