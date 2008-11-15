#ifndef ISTKsds_res_h
#define ISTKsds_res_h

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
#define		STACK_INC	16
#define		SDS_READY	-1

struct	sds_res_control {
	int			stack_size;
	int			addr_inc;
	int			thiso;
	struct	sds_odesc	*start_stack;
	struct	sds_odesc	*this;
	struct	sds_odesc	*parent;
	int			sds_used;
	struct	type_list	*tptr;
	struct	type_list	tdum[2];
	char			*nheap;
	char			*sds_al;
	};

#endif
