CC = gcc
CFLAGS = -O2

GNAT_RTL_OBJS = \
 ada.o ada-io_exceptions.ads ada-tags.o ada-text_io.o ada-text_io-aux.o \
 io_exceptions.ads system-img_b.o system-img_c.o system-img_f.o  \
 system-img_i.o system-img_lf.o system-img_llf.o system-img_lli.o \
 system-img_sf.o system-storage_elements.o system-xp.o system-xp_f.o \
 system-xp_i.o system-xp_lf.o system-xp_li.o system-xp_llf.o \
 system-xp_lli.o system-xp_sf.o system-xp_si.o system-xp_ssi.o system.o \
 text_io.ads ttypes.o

.SUFFIXES: .adb .ads
.adb.o:
	$(CC)  -c $(CFLAGS) $<
.ads.o:
	$(CC)  -c $(CFLAGS) $<

gnatrtl: $(GNAT_RTL_OBJS)
