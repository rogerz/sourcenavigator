
/* terrible hacks for Sun's Cfront compiler */
#define true XTRUE
#define false XFALSE
#define __attribute__(x)

/*
**  Copyright 1995 Darrell Schiebel (drs@nrao.edu).  All rights reserved.
**
** This software is not subject to any license of the American Telephone
** and Telegraph Company or of the Regents of the University of California.
**
** Permission is granted to anyone to use this software for any purpose on
** any computer system, and to alter it and redistribute it freely, subject
** to the following restrictions:
** 1. The author is not responsible for the consequences of use of this
**    software, no matter how awful, even if they arise from flaws in it.
** 2. The origin of this software must not be misrepresented, either by
**    explicit claim or by omission.  Since few users ever read sources,
**    credits must appear in the documentation.
** 3. Altered versions must be plainly marked as such, and must not be
**    misrepresented as being the original software.  Since few users
**    ever read sources, credits must appear in the documentation.
** 4. This notice may not be removed or altered.
*/

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include <Glish/Value.h>
#include <Glish/Client.h>
#include <string.h>

/* Internal Global Data */
static Client *client = 0;

extern "C" void  boot_Glish (CV*);

static Value* pv2gv(SV *, glish_type = TYPE_ERROR);
static SV* gv2pv(const Value *, glish_type&);
static SV* gv2pv_hash(const recordptr);
static SV* gv2pv_hash(const recordptr, glish_type &);

#define BITS_PER_BYTE	8

// Error field for reporting errors to Perl
#define ERRORs		"error"

// Complex fields for returning Glish complex
// to Perl.
#define REALs		"real"
#define IMAGs		"imag"

// Type prefix for adding Glish types as fields
// in Perl hashes
#define ATTRs		"attr*"
#define TYPEs		"type*"
#define TY_AT_len	5

#define INITIAL_STR_len		256
#define INITIAL_ARRAY_len	128

#define COMMA(x) ,x

#/***************************************************
#* Provide the hook for auto creation of the        *
#* glish_type functions, e.g. TYPE_INT, TYPE_BOOL,  *
#* etc.                                             *
#***************************************************/
static int
type(char *name)
	{
	errno = 0;
	if ( strlen(name) >= 6 )
		switch (name[5])
			{
			case 'I':
				if (strEQ(name, "TYPE_INT"))
					return TYPE_INT;
				break;
			case 'C':
				if (strEQ(name, "TYPE_COMPLEX"))
					return TYPE_COMPLEX;
				break;
			case 'E':
				if (strEQ(name, "TYPE_ERROR"))
					return TYPE_ERROR;
				break;
			case 'R':
				if (strEQ(name, "TYPE_RECORD"))
					return TYPE_RECORD;
				break;
			case 'F':
				if (strEQ(name, "TYPE_FLOAT"))
					return TYPE_FLOAT;
				break;
			case 'B':
				if (strEQ(name, "TYPE_BOOL"))
					return TYPE_BOOL;
				if (strEQ(name, "TYPE_BYTE"))
					return TYPE_BYTE;
				break;
			case 'D':
				if (strEQ(name, "TYPE_DOUBLE"))
					return TYPE_DOUBLE;
				if (strEQ(name, "TYPE_DCOMPLEX"))
					return TYPE_DCOMPLEX;
				break;
			case 'S':
				if (strEQ(name, "TYPE_STRING"))
					return TYPE_STRING;
				if (strEQ(name, "TYPE_SHORT"))
					return TYPE_SHORT;
				break;
			}

	errno = EINVAL;
	return 0;
	}

#/***************************************************
#* Create an empty Glish vector                     *
#***************************************************/
static Value*
empty_glish_value()
	{
	int i = 0;
	Value *gv = new Value( &i, 0, COPY_ARRAY );
	return gv;
	}

#/***************************************************
#* get glish_type given an SV*                      *
#***************************************************/
static glish_type
get_type(const SV *val)
	{
	if ( ! val )
		return TYPE_ERROR;
	if (SvROK(val)) return TYPE_REF;
	if (SvIOKp(val)) return TYPE_INT;
	if (SvNOKp(val)) return TYPE_DOUBLE;
	if (SvPOKp(val)) return TYPE_STRING;
	return TYPE_ERROR;
	}

#/***************************************************
#* get glish_type given an IV                       *
#***************************************************/
static glish_type
get_type(IV type)
	{
	switch( type )
		{
		case TYPE_BOOL: return TYPE_BOOL;
		case TYPE_BYTE: return TYPE_BYTE;
		case TYPE_SHORT: return TYPE_SHORT;
		case TYPE_INT: return TYPE_INT;
		case TYPE_FLOAT: return TYPE_FLOAT;
		case TYPE_DOUBLE: return TYPE_DOUBLE;
		case TYPE_STRING: return TYPE_STRING;
		case TYPE_RECORD: return TYPE_RECORD;
		case TYPE_COMPLEX: return TYPE_COMPLEX;
		case TYPE_DCOMPLEX: return TYPE_DCOMPLEX;
		default: return TYPE_ERROR;
		}
	return TYPE_ERROR;
	}

#/***************************************************
#* Glish vector -> Perl scalar, array or hash       *
#*                                                  *
#* The hash is used for complex numbers             *
#***************************************************/
static SV*
gv2pv_array(const Value *array, glish_type &type )
	{
	SV *r;

#define gv2pv_scalar_array_action(ID,GTYPE,PTYPE,typename,accessor,pctor,extra)\
	case GTYPE:							\
		{							\
		type = ID;						\
		typename val = array->accessor();			\
		r = pctor((PTYPE) val extra);				\
		}							\
		break;
		
#define gv2pv_array_action(ID,GTYPE,PTYPE,typename,accessor,pctor,extra)\
	case GTYPE:							\
		{							\
		AV *ret = newAV();					\
		type = ID;						\
		typename *ary = array->accessor();			\
		for (I32 i = 0; i < array->Length(); i++)		\
			{						\
			av_store(ret,i,pctor((PTYPE) ary[i] extra ));	\
			}						\
		r = newRV((SV*) ret);					\
		}							\
		break;

#define gv2pv_complex_array_action(ID,SUBID,GTYPE,PTYPE,typename,accessor)\
	case GTYPE:							\
		{							\
		HV *ret = newHV();					\
		AV *real = newAV();					\
		AV *imag = newAV();					\
		type = ID;						\
		typename *cmpx = array->accessor();			\
		for (I32 i = 0; i < array->Length(); i++)		\
			{						\
			av_store(real,i,newSVnv((PTYPE) cmpx[i].r));	\
			av_store(imag,i,newSVnv((PTYPE) cmpx[i].i ));	\
			}						\
		hv_store(ret,REALs,strlen(REALs),newRV((SV*)real),0);	\
		hv_store(ret,TYPEs REALs,strlen(TYPEs REALs),newSViv((IV)SUBID),0);\
		hv_store(ret,IMAGs,strlen(IMAGs),newRV((SV*)imag),0);	\
		hv_store(ret,TYPEs IMAGs,strlen(TYPEs IMAGs),newSViv((IV)SUBID),0);\
		r = newRV((SV*) ret);					\
		}							\
		break;

#define gv2pv_scalar_array_action(ID,GTYPE,PTYPE,typename,accessor,pctor,extra)\
	case GTYPE:							\
		{							\
		type = ID;						\
		typename val = array->accessor();			\
		r = pctor((PTYPE) val extra);				\
		}							\
		break;

#define gv2pv_scalar_complex_array_action(ID,SUBID,GTYPE,PTYPE,typename,accessor)\
	case GTYPE:							\
		{							\
		HV *ret = newHV();					\
		type = ID;						\
		typename cmpx = array->accessor();			\
		/*sv_2mortal((SV*)ret);*/				\
		hv_store(ret,REALs,strlen(REALs),newSVnv(cmpx.r),0);	\
		hv_store(ret,TYPEs REALs,strlen(TYPEs REALs),newSViv(SUBID),0);\
		hv_store(ret,IMAGs,strlen(IMAGs),newSVnv(cmpx.i),0);	\
		hv_store(ret,TYPEs IMAGs,strlen(TYPEs IMAGs),newSViv(SUBID),0);\
		r = newRV((SV*) ret);					\
		}							\
		break;
		
	int length = array->Length();
	if ( length > 1 )
		{
		switch (array->Type())
			{
			gv2pv_array_action(TYPE_BOOL,TYPE_BOOL,IV,glish_bool,BoolPtr,newSViv,)
			gv2pv_array_action(TYPE_BYTE,TYPE_BYTE,IV,byte,BytePtr,newSViv,)
			gv2pv_array_action(TYPE_SHORT,TYPE_SHORT,IV,short,ShortPtr,newSViv,)
			gv2pv_array_action(TYPE_INT,TYPE_INT,IV,int,IntPtr,newSViv,)
			gv2pv_array_action(TYPE_FLOAT,TYPE_FLOAT,double,float,FloatPtr,newSVnv,)
			gv2pv_array_action(TYPE_DOUBLE,TYPE_DOUBLE,double,double,DoublePtr,newSVnv,)
			gv2pv_array_action(TYPE_STRING,TYPE_STRING,char*,charptr,StringPtr,
					newSVpv,COMMA(strlen(ary[i])))
			gv2pv_complex_array_action(TYPE_COMPLEX,TYPE_FLOAT,TYPE_COMPLEX,double,complex,ComplexPtr)
			gv2pv_complex_array_action(TYPE_DCOMPLEX,TYPE_DOUBLE,TYPE_DCOMPLEX,double,dcomplex,DcomplexPtr)
			default:
				{
				AV *ret = newAV();
				type=TYPE_ERROR;
				char *err_str = "Bad type in Glish.xs/gv2pv_array";
				av_store(ret,0,newSVpv(err_str,strlen(err_str)));
				r = newRV((SV*) ret);
				}
			}
		}
	else if ( length == 1 )
		{
		switch (array->Type())
			{
			gv2pv_scalar_array_action(TYPE_BOOL,TYPE_BOOL,IV,glish_bool,BoolVal,newSViv,)
			gv2pv_scalar_array_action(TYPE_BYTE,TYPE_BYTE,IV,byte,ByteVal,newSViv,)
			gv2pv_scalar_array_action(TYPE_SHORT,TYPE_SHORT,IV,short,ShortVal,newSViv,)
			gv2pv_scalar_array_action(TYPE_INT,TYPE_INT,IV,int,IntVal,newSViv,)
			gv2pv_scalar_array_action(TYPE_FLOAT,TYPE_FLOAT,double,float,FloatVal,newSVnv,)
			gv2pv_scalar_array_action(TYPE_DOUBLE,TYPE_DOUBLE,double,double,DoubleVal,newSVnv,)
			gv2pv_scalar_array_action(TYPE_STRING,TYPE_STRING,char*,charptr*,StringPtr,
					newSVpv,[0] COMMA(strlen(val[0])))
			gv2pv_scalar_complex_array_action(TYPE_COMPLEX,TYPE_FLOAT,TYPE_COMPLEX,double,complex,ComplexVal)
			gv2pv_scalar_complex_array_action(TYPE_DCOMPLEX,TYPE_DOUBLE,TYPE_DCOMPLEX,double,dcomplex,DcomplexVal)
			default:
				{
				AV *ret = newAV();
				type=TYPE_ERROR;
				char *err_str = "Bad type in Glish.xs/gv2pv_array";
				av_store(ret,0,newSVpv(err_str,strlen(err_str)));
				r = newRV((SV*) ret);
				}
			}

		}
	else
		{
		AV *ret = newAV();
		r = newRV((SV*) ret);
		type = array->Type();
		}

	return r;
	}

#/***************************************************
#* Glish record -> Perl hash                        *
#***************************************************/
static SV*
gv2pv_hash(const recordptr rptr, glish_type &type)
	{
	HV *ret = newHV();
	IterCookie *c = rptr->InitForIteration();
	const Value *member;
	glish_type member_type;
	const char *key;

	type = TYPE_RECORD;

	static int str_len = INITIAL_STR_len;
	static char *type_str = (char*)malloc(str_len*sizeof(char));

	while ( member = rptr->NextEntry( key, c) )
		{
		const attributeptr attr = member->AttributePtr();
		SV *val = gv2pv(member,member_type);
		int key_len = strlen(key);
		hv_store(ret,(char *)key,key_len,val,0);
		if ( key_len + TY_AT_len+1 > str_len )
			{
			while (key_len + TY_AT_len+1 > str_len) str_len *= 2;
			type_str = (char*)realloc((void*)type_str,str_len*sizeof(char));
			}
		strcpy(type_str,TYPEs);
		strcat(type_str,key);
		hv_store(ret,type_str,key_len+TY_AT_len,
			newSViv(member_type),0);
		if ( attr )
			{
			SV *svattr = gv2pv_hash(attr);
			strcpy(type_str,ATTRs);
			strcat(type_str,key);
			hv_store(ret,type_str,key_len+TY_AT_len,svattr,0);
			}
		}
	SV *r = newRV((SV*) ret);
	return r;
	}

#/***************************************************
#* Glish record -> Perl hash                        *
#***************************************************/
static SV*
gv2pv_hash(const recordptr rptr)
	{
	glish_type tmp_type;
	return gv2pv_hash(rptr,tmp_type);
	}

#/***************************************************
#* Glish record -> Perl hash                        *
#***************************************************/
static SV*
gv2pv_hash(const Value *hash, glish_type &type)
	{
	if ( hash->Type() == TYPE_RECORD )
		{
		const recordptr rptr = hash->RecordPtr();
		return gv2pv_hash(rptr,type);
		}
	else
		{
		HV *ret = newHV();
		type=TYPE_ERROR;
		char *err_str = "Bad type in Glish.xs/gv2pv_hash";
		hv_store(ret,ERRORs,strlen(ERRORs),
			newSVpv(err_str,strlen(err_str)),0);
		SV *r = newRV((SV*) ret);
		return r;
		}
	}

#/***************************************************
#* Convert a Glish value to Perl value              *
#***************************************************/
static SV*
gv2pv(const Value *val,glish_type &type)
	{
	if ( val->IsRef() )
		return gv2pv( val->Deref(),type);

	if ( val->Type() == TYPE_RECORD )
		return gv2pv_hash(val,type);
	else
		return gv2pv_array(val,type);
	}

#/***************************************************
#* Perl hash -> Glish record or vector.             *
#***************************************************/
static Value*
pv2gv_hash(HV *hash, glish_type type)
	{
	switch (type)
		{

#define pv2gv_hash_real_action(TYPE,BUILTINs)			\
	case TYPE:						\
		{						\
		SV **rp = hv_fetch(hash,REALs,strlen(REALs),0);	\
		if ( rp )					\
			return pv2gv(*rp,BUILTINs);		\
		else						\
			return empty_glish_value();		\
		}						\
		break;

#define pv2gv_hash_complex_action(TYPE,BUILTINs,builtin,subtype,accessor)\
	case TYPE:							\
		{							\
		SV **rp = hv_fetch(hash,REALs,strlen(REALs),0);		\
		SV **ip = hv_fetch(hash,IMAGs,strlen(IMAGs),0);		\
		if ( rp || ip )						\
			{						\
			Value *realv = 0;				\
			Value *imagv = 0;				\
			if (rp)	realv = pv2gv(*rp,BUILTINs);		\
			if (ip)	imagv = pv2gv(*ip,BUILTINs);		\
			int len = realv ? realv->Length() : 0;		\
			len = imagv ? imagv->Length() > len ? len : 	\
					imagv->Length() : len;		\
			builtin *ret = new builtin[len];		\
									\
			subtype *real = realv ? realv->accessor() : 0;	\
			subtype *imag = imagv ? imagv->accessor() : 0;	\
									\
			for (int cnt = 0; cnt < len; cnt++)		\
				{					\
				ret[cnt].r = real ? real[cnt] : 0;	\
				ret[cnt].i = imag ? imag[cnt] : 0;	\
				}					\
									\
			Value *ret_val = new Value(ret,len);		\
									\
			if (realv)					\
				Unref(realv);				\
			if (imagv)					\
				Unref(imagv);				\
									\
			return ret_val;					\
			}						\
		else							\
			return empty_glish_value();			\
		}							\
		break;

pv2gv_hash_complex_action(TYPE_COMPLEX,TYPE_FLOAT,complex,float,FloatPtr)
pv2gv_hash_complex_action(TYPE_DCOMPLEX,TYPE_DOUBLE,dcomplex,double,DoublePtr)

pv2gv_hash_real_action(TYPE_INT,TYPE_INT)
pv2gv_hash_real_action(TYPE_SHORT,TYPE_SHORT)
pv2gv_hash_real_action(TYPE_BYTE,TYPE_BYTE)
pv2gv_hash_real_action(TYPE_FLOAT,TYPE_FLOAT)
pv2gv_hash_real_action(TYPE_DOUBLE,TYPE_DOUBLE)
pv2gv_hash_real_action(TYPE_BOOL,TYPE_BOOL)

		default:
			{
			hv_iterinit(hash);

			// Used !across! recursive calls to this function
			static int offset = 0;
			static int hash_len = INITIAL_ARRAY_len;
			static const char **keys = (const char**)malloc(hash_len*sizeof(char*));
			static I32 *key_lens = (I32*)malloc(hash_len*sizeof(I32));

			// !Not! used across recursive calls to this function
			static int str_len = INITIAL_STR_len;
			static char *type_str = (char*)malloc(str_len*sizeof(char));

			HE *cur;

			// must maintain offset into the static list of keys becausee
			// this function is called recursively...
			int myoff = offset;
			for (int X = 0; cur = hv_iternext(hash);X++)
				{
				if ( X >= hash_len - myoff )
					{
					hash_len *= 2;
					keys = (const char**)realloc((void*)keys,hash_len*sizeof(char*));
					key_lens = (I32*)realloc((void*)key_lens,hash_len*sizeof(I32));
					}
				keys[myoff+X] = hv_iterkey(cur,&key_lens[myoff+X]);
				if ( key_lens[myoff+X] + TY_AT_len+1 > str_len )
					{
					while (key_lens[myoff+X] + TY_AT_len+1 > str_len) str_len *= 2;
					type_str = (char*)realloc((void*)type_str,str_len*sizeof(char));
					}
				}

			offset += X;			// Advance stack pointer
			Value *ret = create_record();
			for (int XX = 0; XX < X; XX++)
				{
				if ( strncmp(keys[myoff+XX],TYPEs,TY_AT_len) &&
				     strncmp(keys[myoff+XX],ATTRs,TY_AT_len) )
					{
					SV **vp = hv_fetch(hash,(char*)keys[myoff+XX],key_lens[myoff+XX],0);
					if ( vp )
						{
						/** Get type **/
						strcpy(type_str,TYPEs);
						strcat(type_str,keys[myoff+XX]);
						SV **tp = hv_fetch(hash,type_str,key_lens[myoff+XX]+TY_AT_len,0);
						glish_type type_s = TYPE_ERROR;
						if ( tp && SvIOK(*tp) )
							type_s = get_type(SvIV(*tp));

						/** Create Glish Value **/
						Value *newval = pv2gv(*vp,get_type(type_s));
						ret->SetField(keys[myoff+XX],newval);

						/** Set attributes **/
						strcpy(type_str,ATTRs);
						strcat(type_str,keys[myoff+XX]);
						SV **ap = hv_fetch(hash,type_str,key_lens[myoff+XX]+TY_AT_len,0);
						if ( ap )
							{
							Value *newattr = pv2gv(*ap);
							if ( newattr && newattr->Type() == TYPE_RECORD )
								newval->AssignAttributes(newattr);
							else
								Unref(newattr);
							}
						}
					}
				}
			offset -= X;			// Decrement stack pointer
			return ret;
			}
		}
	}

#/***************************************************
#* Perl array -> Glish vector.                      *
#***************************************************/
static Value*
pv2gv_array(AV *array, glish_type type)
	{
	if ( av_len(array) == -1 )
		return empty_glish_value();

	if ( type == TYPE_ERROR )
		{
		SV **vp = av_fetch(array,0,0);
		SV *v = *vp;
		type = get_type(v);
		}
	switch (type)
		{
#define pv2gv_array_action(TYPE,builtin,check,accessor,setup,rest,COPY,cleanup)	\
	case TYPE:							\
		{							\
		builtin *ret_ary = new builtin[av_len(array)+1];	\
		for ( I32 cnt=0; cnt <= av_len(array); cnt++ )		\
			{						\
			SV **vp = av_fetch(array,cnt,0);		\
			SV *v = *vp;					\
			if ( check(v) )					\
				{					\
				setup					\
				ret_ary[cnt] = (builtin) COPY(accessor(v rest));\
				}					\
			else						\
				{					\
				cleanup					\
				delete ret_ary;				\
				return empty_glish_value();		\
				}					\
			}						\
			return new Value(ret_ary,(int)av_len(array)+1);	\
		}							\
		break;

pv2gv_array_action(TYPE_INT,int,SvIOK,SvIV,,,,)
pv2gv_array_action(TYPE_SHORT,short,SvIOK,SvIV,,,,)
pv2gv_array_action(TYPE_BYTE,byte,SvIOK,SvIV,,,,)
pv2gv_array_action(TYPE_FLOAT,float,SvNOK,SvNV,,,,)
pv2gv_array_action(TYPE_DOUBLE,double,SvNOK,SvNV,,,,)
pv2gv_array_action(TYPE_STRING,charptr,SvPOK,SvPV,size_t len;,COMMA(len),
			strdup,for(int XX=0;XX<cnt;XX++) delete (char*)ret_ary[XX];)
		default:
			return empty_glish_value();
		}
	}

#/***************************************************
#* Perl scalar -> Glish vector.                     *
#***************************************************/
static Value*
pv2gv_scalar(SV *val,glish_type type)
	{

	if ( type == TYPE_ERROR )
		type = get_type(val);

	switch (type)
		{
#define pv2gv_scalar_action(TYPE,type,check,accessor,setup,extra)\
	case TYPE:						\
		{						\
		if ( check(val) )				\
			{					\
			setup					\
			type t = (type) accessor(val extra );	\
			return new Value( t );			\
			}					\
		else						\
			return empty_glish_value();		\
		}						\
		break;

pv2gv_scalar_action(TYPE_INT,int,SvIOK,SvIV,,)
pv2gv_scalar_action(TYPE_SHORT,short,SvIOK,SvIV,,)
pv2gv_scalar_action(TYPE_BYTE,byte,SvIOK,SvIV,,)
pv2gv_scalar_action(TYPE_FLOAT,float,SvNOK,SvNV,,)
pv2gv_scalar_action(TYPE_DOUBLE,double,SvNOK,SvNV,,)
pv2gv_scalar_action(TYPE_STRING,charptr,SvPOK,SvPV,STRLEN len;,COMMA(len))
		}
	}


#/***************************************************
#* Convert a Perl value to a Glish value.           *
#***************************************************/
static Value*
pv2gv(SV *val, glish_type type)
	{
	if ( !val )
		return 0;

	if ( SvROK(val) )
		return pv2gv(SvRV(val),type);

	switch( SvTYPE(val) )
		{
		case SVt_PVAV:
			{
			AV *array = (AV*) val;
			return pv2gv_array(array,type);
			}
		case SVt_PVHV:
			{
			HV *hash = (HV*) val;
			return pv2gv_hash(hash,type);
			}
		case SVt_PVCV:
			return empty_glish_value();
		default:
			return pv2gv_scalar(val,type);
		}
	return empty_glish_value();
	}

MODULE = Glish PACKAGE = Glish

BOOT:
#
# Added to the bootstrap code
#
AV* argv_av = perl_get_av("ARGV",FALSE);
I32 argv_cnt = 0;
I32 argv_len = av_len(argv_av);
argv_len += 1;				/* PERL length -1 <-> 0  */
char **argv_real = new char*[argv_len+1];
SV** argv_val;
SV* prog_name = perl_get_sv("0", FALSE);
argv_real[0] = SvPV(prog_name, na);
for ( argv_cnt = 0; argv_cnt < argv_len; argv_cnt++ )
	{
	argv_val = av_fetch(argv_av, argv_cnt, 0);
	argv_real[argv_cnt+1] = SvPV(*argv_val,na);
	}
argv_len += 1;				/* Program name included  */
int al_tmp = (int) argv_len;		/* Temporary arg_len so   */
client = new Client(al_tmp,argv_real);	/*   Client can modify it */
delete argv_real;
if ( ! client )
	{
	croak("Couldn't initialize Glish Client");
	}

#/*****************************************************************
#* Get the next event. Used as follows:                           *
#*                                                                *
#*      ($key,$val,$attr) = nextevent();                          *
#*      ($key,$val,$attr) = nextevent($val_type);                 *
#*      ($key,$val,$attr) = nextevent($val_type,$isrequest);      *
#*                                                                *
#*****************************************************************/
void
nextevent(...)
	PPCODE:
	{
	SV *svtype = 0;
	SV *request = 0;
	GlishEvent* event;
	glish_type type;

	if (items >= 1)
		svtype = ST(0);
	if (items >= 2)
		request = ST(1);

	EXTEND(sp,3);
	event = client->NextEvent();

	if ( event )
		{
		Value *val = event->value;
		PUSHs(sv_2mortal(newSVpv((char*)event->name,strlen(event->name))));
		SV *pv = gv2pv(val,type);
		if ( svtype )
			sv_setiv(svtype,type);
		if ( request )
			sv_setiv(request,(IV)event->IsRequest());
		PUSHs(sv_2mortal(pv));
		const attributeptr aptr = val->AttributePtr();
		if ( aptr )
			{
			SV *svattr = gv2pv_hash(aptr);
			PUSHs(sv_2mortal(svattr));
			}
		else
			{
			PUSHs(&sv_undef);
			}
		}
	else
		{
		type = TYPE_ERROR;
		char *err_str = "Connection closed";
		PUSHs(&sv_undef);
		if ( svtype )
			sv_setiv(svtype,type);
		if ( request )
			sv_setiv(request,0);
		PUSHs(sv_2mortal(newSVpv((char*)err_str,strlen(err_str))));
		PUSHs(&sv_undef);
		}
	}

#/***************************************************
#* Returns the next event; blocks until an event is *
#* available. Invocation format:                    *
#*                                                  *
#*      # Figures out the type from the value       *
#*      postevent(name,value)                       *
#*      # Force a particular Glish type             *
#*      postevent(name,value,type)                  *
#*      # Specify an attribute for the value        *
#*      postevent(name,value,type,$attr)            *
#*                                                  *
#***************************************************/
void
postevent(...)
	PPCODE:
	{
	EXTEND(sp,1);
	if ( items < 2 || !SvPOK(ST(0)) || !SvOK(ST(1)) )
		{
		PUSHs(sv_2mortal(newSViv((IV)0)));
		}
	else if ( items == 2 )
		{
		Value *v = pv2gv(ST(1));
		STRLEN len;
	        char *name = SvPV(ST(0),len);
		client->PostEvent(name,v);
		Unref(v);
		PUSHs(sv_2mortal(newSViv((IV)1)));
		}
	else if ( SvIOK(ST(2)) ) {
	        IV type = SvIV(ST(2));
		Value *v = pv2gv(ST(1),get_type(type));
		STRLEN len;
	        char *name = SvPV(ST(0),len);
		/* handle attribute*/
		if ( items >= 4 && ST(3) != &sv_undef && SvOK(ST(3)) )
			{
			Value *attr = pv2gv(ST(3));
			if (attr->Type() == TYPE_RECORD)
				v->AssignAttributes(attr);
			else
				Unref(attr);
			}
		client->PostEvent(name,v);
		Unref(v);
		PUSHs(sv_2mortal(newSViv((IV)1)));
		}
	else 
		{
		PUSHs(sv_2mortal(newSViv((IV)0)));
		}
	}

#/***************************************************
#* Sends the reply (Client::Reply) to the last      *
#* request that was recieved. Invocation format:    *
#*                                                  *
#*      # Figures out the type from the value       *
#*      reply(value)                                *
#*      # Force a particular Glish type             *
#*	reply(value,type)                           *
#*      # Specify attributes for value              *
#*	reply(value,type,attr)                      *
#*                                                  *
#***************************************************/
void
reply(...)
	PPCODE:
	{
	EXTEND(sp,1);
	if ( items < 1 || !SvOK(ST(0)) )
		{
		PUSHs(sv_2mortal(newSViv((IV)0)));
		}
	else if ( items == 1 )
		{
		Value *v = pv2gv(ST(0));
		client->Reply(v);
		Unref(v);
		PUSHs(sv_2mortal(newSViv((IV)1)));
		}
	else if ( SvIOK(ST(1)) ) {
	        IV type = SvIV(ST(1));
		Value *v = pv2gv(ST(0),get_type(type));
		/* handle attribute*/
		if ( items >= 3 && ST(2) != &sv_undef && SvOK(ST(2)) )
			{
			Value *attr = pv2gv(ST(2));
			if (attr->Type() == TYPE_RECORD)
				v->AssignAttributes(attr);
			else
				Unref(attr);
			}
		client->Reply(v);
		Unref(v);
		PUSHs(sv_2mortal(newSViv((IV)1)));
		}
	else 
		{
		PUSHs(sv_2mortal(newSViv((IV)0)));
		}
	}

#/***************************************************
#* Returns a non-zero value when an event is        *
#* available. Invocation format:                    *
#*                                                  *
#*      waitingevent()                              *
#*                                                  *
#***************************************************/
void
waitingevent()
	PPCODE:
	{
	EXTEND(sp,1);
	fd_set selectSet;
	timeval tv;
	tv.tv_sec = 0;
	tv.tv_usec = 0;
	FD_ZERO(&selectSet);
	client->AddInputMask(&selectSet);
	if ( select(FD_SETSIZE, &selectSet, 0, 0, &tv) > 0 &&
			client->HasClientInput(&selectSet) )
		{
		PUSHs(sv_2mortal(newSViv((IV)1)));
		}
	else
		{	
		PUSHs(sv_2mortal(newSViv((IV)0)));
		}
	}

#/***************************************************
#* Returns a non-zero value when the client is      *
#* running stand-alone. Invocation format:          *
#*                                                  *
#*      standalone()                                *
#*                                                  *
#***************************************************/
void
standalone()
	PPCODE:
	{
	EXTEND(sp,1);
	if ( client->HasInterpreterConnection() )
		{
		PUSHs(sv_2mortal(newSViv((IV)0)));
		}
	else
		{
		PUSHs(sv_2mortal(newSViv((IV)1)));
		}
	}

#/***************************************************
#* Adds the file descriptors used by the client to  *
#* a string which is treated as a bit vector, as    *
#* with Perl's "vec()". The result should be in a   *
#* form usable by Perl's "select()". Invocation     *
#* format:                                          *
#*                                                  *
#*	addfds(vec)                                 *
#*	addfds(vec,offset)                          *
#*                                                  *
#***************************************************/
void
addfds(...)
	PPCODE:
	{
	EXTEND(sp,1);
	register IV offset = 0;
	if ( items < 1)
		{
		PUSHs(sv_2mortal(newSViv((IV)0)));
		}
	else if ( SvPOK(ST(0)) )
		{
		fd_set selectSet;
		FD_ZERO(&selectSet);
		int num_fd = client->AddInputMask(&selectSet);
		if ( num_fd )
			{
			if ( items > 1 && SvIOK(ST(1)) )
				{
				offset = SvIV(ST(1));
				}

			SV *src = ST(0);

			/* len <- the physical allocated length */
			STRLEN len = SvLEN(src);
			/* srclen <- the length of the pre-existing string */
			STRLEN srclen;
			unsigned char *s = (unsigned char*)SvPV(src, srclen);

			STRLEN req_size = (offset + FD_SETSIZE + BITS_PER_BYTE - 1) / BITS_PER_BYTE;

			int failed = 0;

			if ( srclen < req_size )
				{
				/* added one because I believe Perl likes having an extra character */
				if (len < req_size+1)
					{
					SvGROW(src,req_size+1);
					len = SvLEN(src);
					}
				if (len >= req_size+1)
					{
					STRLEN origlen = srclen;
					SvCUR_set(src, req_size); 	/* recognize new size */
					s = (unsigned char*)SvPV(src, srclen);
					memset(&s[origlen],0,len-origlen);
					}
				else
					failed = 1;
				}
			if (! failed )
				{
				/***************************************************************
				* The Perl "vec()" function operates on bytes from low bit to  *
				* high bit. As a result, here we must do the same.             *
				***************************************************************/
				int fd_cnt = 0;
				unsigned char mask = 1;
				int mask_off = 0;

				for (int cur=0; cur < FD_SETSIZE && fd_cnt < num_fd; cur++)
					{
					if ( FD_ISSET(cur,&selectSet) )
						{
						s[mask_off+offset] |= mask;
						fd_cnt++;
						}
					if ( ! (mask <<= 1) )
						{
						mask = 1;
						mask_off++;
						}
					}
				PUSHs(sv_2mortal(newSViv((IV)num_fd)));
				}
			else
				{
				PUSHs(sv_2mortal(newSViv((IV)0)));
				}
			}
		else
			{
			PUSHs(sv_2mortal(newSViv((IV)0)));
			}
		}
	else
		{
		PUSHs(sv_2mortal(newSViv((IV)0)));
		}
	}

int
type(name)
	char *	name
