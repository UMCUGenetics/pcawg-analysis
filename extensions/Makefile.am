AUTOMAKE_OPTIONS               = subdir-objects

extensiondir                   = $(EXTDIR)
extension_LTLIBRARIES          = libunmapped_filter.la libunmapped_verifier.la libregion_filter.la

libunmapped_filter_la_CFLAGS   = $(guile_CFLAGS) $(htslib_CFLAGS)
libunmapped_filter_la_LIBADD   = $(guile_LIBS) $(htslib_LIBS)
libunmapped_filter_la_SOURCES  = unmapped_filter.c

libunmapped_verifier_la_CFLAGS   = $(guile_CFLAGS) $(htslib_CFLAGS)
libunmapped_verifier_la_LIBADD   = $(guile_LIBS) $(htslib_LIBS)
libunmapped_verifier_la_SOURCES  = unmapped_verifier.c

libregion_filter_la_CFLAGS   = $(guile_CFLAGS) $(htslib_CFLAGS)
libregion_filter_la_LIBADD   = $(guile_LIBS) $(htslib_LIBS)
libregion_filter_la_SOURCES  = region_filter.c
