#ifndef ISTKsds_mdep_h     
#define ISTKsds_mdep_h     

/* $Header$ */

/**************************************************************************
 *                 ****** ISTK Release 1.2 *****                          *
 *                                                                        *
 *                                                                        *
 * This code has been produced by numerous authors at the CERN centre for *
 * high energy physics, Geneve, Switzerland, at the SSC laboratory in     *
 * Dallas, Texas, USA and at the Lawrence Berekeley Laboratory in         *
 * California, USA.                                                       *
 * The latter two institutions perform work under US Government contract. *
 * The intent of the work is to provide useful code for people who need   *
 * it, with an emphasis on free and collaborative exchange of ideas,      *
 * techniques and implementations.                                        *
 * Please read the disclaimer and copyright notices contained in the ISTK *
 * distribution and in distributed applications.                          *
 *                                                                        *
 **************************************************************************/


/************************************************************************
 *            Copyright (C) Frogsoft Corporation,  1987                 *
 *
 *  Modification history:
 *
 *
 ***********************************************************************/
     

     
/*  PADDING BYTE INFORMATION  */
/************************************************************************
   These numbers tell you what broad architecture type you are using; 
   one of these is chosen at compile time to indicate how to pad data
   what sizes data primitives are and what floating point representations
   are used. The choice is made by testing for the standard #defines.
   Adding a new machine may be simple if the correct architecture is 
   already here. Two defined blocks must be added below: one tests for
   the machine type and defines SDS_ARC; in this block you also note if
   shared memory and/or file mapping is available on the system by
   defining SHMEM and/or MEMMAP. In the second block you define
   some of the more generic primitives - for instance whether an int
   is two or four bytes. Byte ordering is picked up automatically.

   If the architecture is not yet available, several things must be done
   before the above stuff can be entered:

   1. NARCS (number of architecture types) increases.
   2. A new architecture is defined as SDS_SUN3ARC,SDS_SPARC etc have
 been below.
   3. A new series of entries need to be made in the static array sds_arcs,
 which appears in the globals file sds_glob.c. An array of
 NPRIMITIVES integers represent the byte boundary of each primitive
 type as defined in this file (SDS_WORD, SDS_LONG etc) and named in
 sds_glob.c. Also, the most restrictive boundary (eg 1 for Vax, 2 for
 680x0, 8 for sparc risc) is put in the sds_arcs array in sds_glob.c.
   4. System-dependant header differences can be ironed out in the file
 sdsgen.h
   5. Proceed as above when the architecture was already in place.

 Be careful - in some cases differences happen on the same machine - eg
 VMS and ultrix vaxen - and even with different compilers.

   If you need to add a new primitive type, increase the value of NTYPES
     defined in this file and add entries to the type_name array ,
     the sds_sizes array and the sds_arcs array (sds_glob.c) for
     ALL CURRENT ARCHITECTURES.Add the define for the new type, and
     recompile everything.

****************************************************************************/
#define SDS_SUN3ARC   0
#define SDS_SPARC     1
#define SDS_VAXARC    2
#define SDS_ALPHAARC  3
#define SDS_HCUBESARC 4

#define  NARCS  5

#if defined(mv147) || defined(__mv147__) || defined(VXWORKS)
#define SDS_ARC SDS_SUN3ARC
#endif

#if defined(__ultrix__)
#define SHMEM 1
#define SDS_ARC SDS_HCUBESARC
#endif

#if defined(__i486__) || defined(__i386__)
#define SHMEM 1
#define SDS_ARC SDS_HCUBESARC
#endif

#if defined(mips) || defined(__mips__)
#if !defined(sgi) && !defined(__sgi__)
#define MIPS_SWAPPED
#endif 
#define SHMEM 1
#define MEMMAP 1
#define SDS_ARC SDS_SPARC
#endif 

#if defined(__alpha__)
#define SDS_BIGADDR 1
#define SHMEM 1
#define MEMMAP 1
#define SDS_ARC SDS_ALPHAARC
#endif 

#if defined(sparc) || defined(__sparc__)
#define SHMEM 1
#define MEMMAP 1
#define SDS_ARC SDS_SPARC
#endif 

#if defined(sun3) || defined(__sun3__)
#define SHMEM 1
#define MEMMAP 1
#define SDS_ARC SDS_SUN3ARC
#endif

#if defined(mac) || defined(__mac__)
#define SDS_ARC SDS_SUN3ARC
#endif

#if defined(masscomp) || defined(__masscomp__)
#define SHMEM 1
#define SDS_ARC SDS_SUN3ARC 
#endif 

#if defined(vax) || defined(__vax__)
#define SDS_ARC SDS_VAXARC
#endif

#ifdef atari
#define SDS_ARC SDS_SUN3ARC 
#endif 

#if defined(_AMIGA)
#define SDS_ARC SDS_SUN3ARC
#endif
 
#if defined(hp9000s800) || defined(__hp9000s800__)
#define SHMEM 1
#define SDS_ARC SDS_SPARC
#endif

#if defined(hp9000s300) || defined(__hp9000s300__)
#define SHMEM 1
#define SDS_ARC SDS_SUN3ARC
#endif

#if defined(NeXT) || defined(__NeXT__)
#define SDS_ARC SDS_SUN3ARC
#endif

#ifdef __DGUX__
#define SHMEM 1
#define SDS_ARC SDS_SPARC
#endif

#define SDS_SIGNED 0
#define SDS_UNSIGNED 1

/*    element type codes */
#define  NTYPES  39
#define  NPRIMITIVES NTYPES

#define SDS_WEIRD  (unsigned long)0x00
#define SDS_PADB  (unsigned long)0x01
#define SDS_BYTE  (unsigned long)0x02
#define SDS_UNS_BYTE  (unsigned long)0x03
#define SDS_WORD  (unsigned long)0x04
#define SDS_UNS_WORD  (unsigned long)0x05
#define SDS_LONG  (unsigned long)0x06
#define SDS_UNS_LONG  (unsigned long)0x07
#define SDS_IFLOAT  (unsigned long)0x08
#define SDS_IDOUBLE  (unsigned long)0x09
#define SDS_VFLOAT  (unsigned long)0x0a

#define SDS_DVDOUBLE             (unsigned long)0x0b
#define SDS_GVDOUBLE             (unsigned long)0x0c
#define SDS_STRING               (unsigned long)0x0d
#define SDS_DIRECTORY_STRUCTURE  (unsigned long)0x0e
#define SDS_ICOMPLEX             (unsigned long)0x0f
#define SDS_IDOUBLE_COMPLEX      (unsigned long)0x10
#define SDS_VCOMPLEX             (unsigned long)0x11
#define SDS_DVDOUBLE_COMPLEX     (unsigned long)0x12
#define SDS_GVDOUBLE_COMPLEX     (unsigned long)0x13
#define SDS_LOGICAL_1            (unsigned long)0x14
#define SDS_LOGICAL_2            (unsigned long)0x15
#define SDS_LOGICAL_4            (unsigned long)0x16
#define SDS_POINTER              (unsigned long)0x17
#define SDS_TIME                 (unsigned long)0x18
#define SDS_SDS                  (unsigned long)0x19
#define SDS_FSTRING              (unsigned long)0x1a
#define SDS_SIZE_MODIFIER        (unsigned long)0x1b
#define SDS_GLISH_VALUE          (unsigned long)0x1c
#define SDS_UNIX_TIME            (unsigned long)0x1d
#define SDS_BITFIELD             (unsigned long)0x1e
#define SDS_INTERNAL_POINTER     (unsigned long)0x1f
#define SDS_DOUBLE_LONG          (unsigned long)0x20
#define SDS_END_BITFIELDS        (unsigned long)0x21
#define SDS_CHAR_BITFIELD        (unsigned long)0x22
#define SDS_SHORT_BITFIELD       (unsigned long)0x23
#define SDS_LONG_BITFIELD        (unsigned long)0x24
#define SDS_DOUBLE_LONG_BITFIELD (unsigned long)0x25
#define SDS_UNS_DOUBLE_LONG      (unsigned long)0x26

#define SDS_LAST_BITFIELD        (unsigned long)0x8000
#define SDS_BITFIELD_NMASK       (unsigned long)0x7fff


#if defined(vms) || defined(__ultrix__)
#define VAXFP
#define SDS_CHAR SDS_BYTE
#define SDS_INT SDS_LONG
#define SDS_SHORT SDS_WORD
#define SDS_UNS_CHAR SDS_UNS_BYTE
#define SDS_UNS_INT SDS_UNS_LONG
#define SDS_UNS_SHORT SDS_UNS_WORD
#define SDS_FLOAT SDS_VFLOAT
#define SDS_DOUBLE SDS_DVDOUBLE
#define SDS_COMPLEX SDS_VCOMPLEX
#define SDS_DOUBLE_COMPLEX SDS_DVDOUBLE_COMPLEX
#endif
     
#ifdef atari
#define VAXFP
#define SDS_CHAR SDS_BYTE
#define SDS_INT SDS_WORD
#define SDS_SHORT SDS_WORD
#define SDS_UNS_CHAR SDS_UNS_BYTE
#define SDS_UNS_INT SDS_UNS_WORD
#define SDS_UNS_SHORT SDS_UNS_WORD
#define SDS_FLOAT SDS_VFLOAT
#define SDS_DOUBLE SDS_DVDOUBLE
#define SDS_COMPLEX SDS_VCOMPLEX
#define SDS_DOUBLE_COMPLEX SDS_DVDOUBLE_COMPLEX
#endif

#if defined(hpux) || defined(__hpux__)
#define IEEEFP
#define SDS_FLOAT SDS_IFLOAT
#define SDS_DOUBLE SDS_IDOUBLE
#define SDS_COMPLEX SDS_ICOMPLEX
#define SDS_DOUBLE_COMPLEX SDS_IDOUBLE_COMPLEX
#define SDS_CHAR SDS_BYTE
#define SDS_INT SDS_LONG
#define SDS_SHORT SDS_WORD
#define SDS_UNS_CHAR SDS_UNS_BYTE
#define SDS_UNS_INT SDS_UNS_LONG
#define SDS_UNS_SHORT SDS_UNS_WORD
#endif

#if defined(NeXT) || defined(__NeXT__)
#define IEEEFP
#define SDS_FLOAT SDS_IFLOAT
#define SDS_DOUBLE SDS_IDOUBLE
#define SDS_COMPLEX SDS_ICOMPLEX
#define SDS_DOUBLE_COMPLEX SDS_IDOUBLE_COMPLEX
#define SDS_CHAR SDS_BYTE
#define SDS_INT SDS_LONG
#define SDS_SHORT SDS_WORD
#define SDS_UNS_CHAR SDS_UNS_BYTE
#define SDS_UNS_INT SDS_UNS_LONG
#define SDS_UNS_SHORT SDS_UNS_WORD
#endif

#if defined(sun) || defined(__sun__) || defined (__alpha__)
#define IEEEFP
#define SDS_FLOAT SDS_IFLOAT
#define SDS_DOUBLE SDS_IDOUBLE
#define SDS_COMPLEX SDS_ICOMPLEX
#define SDS_DOUBLE_COMPLEX SDS_IDOUBLE_COMPLEX
#define SDS_CHAR SDS_BYTE
#define SDS_INT SDS_LONG
#define SDS_SHORT SDS_WORD
#define SDS_UNS_CHAR SDS_UNS_BYTE
#define SDS_UNS_INT SDS_UNS_LONG
#define SDS_UNS_SHORT SDS_UNS_WORD
#endif

#if defined(_AMIGA) || defined(THINK_C)
#define IEEEFP
#define SDS_FLOAT SDS_IFLOAT
#define SDS_DOUBLE SDS_IDOUBLE
#define SDS_COMPLEX SDS_ICOMPLEX
#define SDS_DOUBLE_COMPLEX SDS_IDOUBLE_COMPLEX
#define SDS_CHAR SDS_BYTE
#define SDS_INT SDS_LONG
#define SDS_SHORT SDS_WORD
#define SDS_UNS_CHAR SDS_UNS_BYTE
#define SDS_UNS_INT SDS_UNS_LONG
#define SDS_UNS_SHORT SDS_UNS_WORD
#endif
 
#if defined(mac) || defined(__mac__)
#define IEEEFP
#define SDS_FLOAT SDS_IFLOAT
#define SDS_DOUBLE SDS_IDOUBLE
#define SDS_COMPLEX SDS_ICOMPLEX
#define SDS_DOUBLE_COMPLEX SDS_IDOUBLE_COMPLEX
#define SDS_CHAR SDS_BYTE
#define SDS_INT SDS_LONG
#define SDS_SHORT SDS_WORD
#define SDS_UNS_CHAR SDS_UNS_BYTE
#define SDS_UNS_INT SDS_UNS_LONG
#define SDS_UNS_SHORT SDS_UNS_WORD
#endif

#ifdef __DGUX__
#define IEEEFP
#define SDS_FLOAT SDS_IFLOAT
#define SDS_DOUBLE SDS_IDOUBLE
#define SDS_COMPLEX SDS_ICOMPLEX
#define SDS_DOUBLE_COMPLEX SDS_IDOUBLE_COMPLEX
#define SDS_CHAR SDS_BYTE
#define SDS_INT SDS_LONG
#define SDS_SHORT SDS_WORD
#define SDS_UNS_CHAR SDS_UNS_BYTE
#define SDS_UNS_INT SDS_UNS_LONG
#define SDS_UNS_SHORT SDS_UNS_WORD
#endif

#if defined(mv147) || defined(__mv147__) || defined(VXWORKS)
#define IEEEFP
#define SDS_FLOAT SDS_IFLOAT
#define SDS_DOUBLE SDS_IDOUBLE
#define SDS_COMPLEX SDS_ICOMPLEX
#define SDS_DOUBLE_COMPLEX SDS_IDOUBLE_COMPLEX
#define SDS_CHAR SDS_BYTE
#define SDS_INT SDS_LONG
#define SDS_SHORT SDS_WORD
#define SDS_UNS_CHAR SDS_UNS_BYTE
#define SDS_UNS_INT SDS_UNS_LONG
#define SDS_UNS_SHORT SDS_UNS_WORD
#endif

#if defined(__i486__) || defined(__i386__)
#define IEEEFP
#define SDS_FLOAT SDS_IFLOAT
#define SDS_DOUBLE SDS_IDOUBLE
#define SDS_COMPLEX SDS_ICOMPLEX
#define SDS_DOUBLE_COMPLEX SDS_IDOUBLE_COMPLEX
#define SDS_CHAR SDS_BYTE
#define SDS_INT SDS_LONG
#define SDS_SHORT SDS_WORD
#define SDS_UNS_CHAR SDS_UNS_BYTE
#define SDS_UNS_INT SDS_UNS_LONG
#define SDS_UNS_SHORT SDS_UNS_WORD
#endif

#if defined(mips) || defined(__mips__)
#define IEEEFP
#define SDS_FLOAT SDS_IFLOAT
#define SDS_DOUBLE SDS_IDOUBLE
#define SDS_COMPLEX SDS_ICOMPLEX
#define SDS_DOUBLE_COMPLEX SDS_IDOUBLE_COMPLEX
#define SDS_CHAR SDS_BYTE
#define SDS_INT SDS_LONG
#define SDS_SHORT SDS_WORD
#define SDS_UNS_CHAR SDS_UNS_BYTE
#define SDS_UNS_INT SDS_UNS_LONG
#define SDS_UNS_SHORT SDS_UNS_WORD
#endif

#if defined(masscomp) || defined(__masscomp__)
#define IEEEFP
#define SDS_FLOAT SDS_IFLOAT
#define SDS_DOUBLE SDS_IDOUBLE
#define SDS_COMPLEX SDS_ICOMPLEX
#define SDS_DOUBLE_COMPLEX SDS_IDOUBLE_COMPLEX
#define SDS_CHAR SDS_BYTE
#define SDS_INT SDS_LONG
#define SDS_SHORT SDS_WORD
#define SDS_UNS_CHAR SDS_UNS_BYTE
#define SDS_UNS_INT SDS_UNS_LONG
#define SDS_UNS_SHORT SDS_UNS_WORD
#endif

#ifdef MSDOS
#define IEEEFP
#define SDS_CHAR SDS_BYTE
#define SDS_INT SDS_WORD
#define SDS_SHORT SDS_WORD
#define SDS_UNS_CHAR SDS_UNS_BYTE
#define SDS_UNS_INT SDS_UNS_WORD
#define SDS_UNS_SHORT SDS_UNS_WORD
#define SDS_FLOAT SDS_IFLOAT
#define SDS_DOUBLE SDS_IDOUBLE
#define SDS_COMPLEX SDS_COMPLEX
#define SDS_DOUBLE_COMPLEX SDS_IDOUBLE_COMPLEX
#endif

#if !defined(SDS_BIGADDR)
#define SDS_BIGADDR 0
#endif

#endif
