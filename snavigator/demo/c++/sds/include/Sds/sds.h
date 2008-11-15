#ifndef ISTKsds_h
#define ISTKsds_h

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

/* Reference release  Aug 10 1991 - C G Saltmarsh */
/* Has the basics used at CDG & SSC 1988-1991, plus vxworks
   support
*/

/*    type stuff to standard out as hex/ascii dump....    */
#define    SDS_LIST_RAW          (sds_code)0x01
/*    ...or as per the element code                */
#define    SDS_LIST_FORMATTED    (sds_code)0x00
/*    do you want description headers before the data?    */
#define SDS_LIST_HEADERS         (sds_code)0x04
/*    do you want columnar output?                */
#define SDS_LIST_COLUMNS         (sds_code)0x08
/*    do you want headers only?                   */
#define SDS_HEADER_ONLY          (sds_code)0x10


#define SDS_PROC_MEM        (sds_code )0x0001
#define SDS_SHARED_MEM      (sds_code )0x0002
#define SDS_FILE            (sds_code )0x0004
#define SDS_SEQUENTIAL      (sds_code )0x0008
#define SDS_READ            (sds_code )0x0010
#define SDS_APPEND          (sds_code )0x0020
#define SDS_WRITE           (sds_code )0x0040
#define SDS_SYBASE          (sds_code )0x0080
#define SDS_TRUNC           (sds_code )0x0100
#define SDS_CREATE          (sds_code )0x0200
#define SDS_DB_STRUCTURE    (sds_code )0x0400
#define SDS_DB_FLAT_ARRAYS  (sds_code )0x0800
#define SDS_DIREC_ONLY      (sds_code )0x1000
#define SDS_LISTDATA_ARRAYS (sds_code )0x2000
#define SDS_MAPPED_MEM      (sds_code )0x8000
#define SDS_SOCKET          (sds_code )0x10000

#define    MAX_SDS    64

/*    put timestamp on dataset and all objects        */
#define    SDS_TIMESTAMP_ALL (sds_code )0xfffffffe

#define    SDS_CODE      1
#define    SDS_OBJECT    2

#define LINEAR        1
#define EXP           0x0002
#define EXP2          0x0004
#define EXP10         0x0008
#define LOG           0x0010
#define LOG2          0x0020
#define LOG10         0x0040
#define POWER         0x0080
#define RECIP         0x0100
#define POLAR         0x0200

#endif
