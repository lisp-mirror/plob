/* -------------------------------------------------------------------------
| Module	plobpxdr.c
| Author	Heiko Kirschke
|		mailto:Heiko.Kirschke@acm.org
| Date		1998/11/09
| Description	Patched XDR functions to handle dynamic typed marshalling.
|
| Copyright	PLOB! Copyright 1994--2001 Heiko Kirschke.
|		All rights reserved.
|
| Unlimited use, reproduction, modification and distribution of this
| software is permitted.  Any copy or modified version of this
| software must include both the above copyright notice of
| Heiko Kirschke and this paragraph; for each modified version, an
| additional statement must be added telling the year of modification
| and quoting the author of the modification.  Any distribution of
| this software must comply with all applicable German export control
| laws.  This software is made available AS IS, and HEIKO KIRSCHKE
| DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT
| LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
| A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION
| CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM THE
| SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
| CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
| HEIKO KIRSCHKE IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
|
| Please note that these license terms adhere only to the code of
| PLOB!  itself. PLOB! uses POSTORE (Persistent Object Store) as a
| low-level persistent memory; it is provided in binary form within
| PLOB! with the permission of the University of St. Andrews
| (http://www-ppg.dcs.st-andrews.ac.uk/Default.html).  Contact the
| University of St. Andrews for getting their license terms on
| POSTORE.
|
| $Header$
|
 ------------------------------------------------------------------------- */

#include	<ctype.h>
#include	<errno.h>
#include	<stdarg.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#if	!WIN32
#include	<unistd.h>
#endif

#define		NOEXCEPTION
#include	"global.h"
#include	"trmalloc.h"
#include	"hash.h"
#include	"generic.h"

#include	"postore.h"

#include	"plob.h"
#include	"plobintern.h"
#include	"plobmisc.h"
#include	"plobff.h"
#include	"plobtype.h"
#include	"plobnumber.h"
#include	"plobsequ.h"
#include	"plobstruct.h"
#include	"plobclos.h"
#include	"plobroot.h"
#include	"ploblock.h"
#include	"plobheap.h"
#include	"plobbtree.h"
#include	"plobadmin.h"

#define		RPCNOTYPES
#include	"plobd.h"
#include	"plobpxdr.h"

/* ----------------------------------------------------------------------- */
MODULE ( __FILE__ );

/* ----------------------------------------------------------------------- */
/* NATIVERPC == 0: Use the `native' xdr functions for transfer of value
                   vectors.
   NATIVERPC != 0: Use the patched xdr functions for transfer of
                   dynamic-typed value vectors. */
#define	NATIVERPC	0

/* ----------------------------------------------------------------------- */
/* LOGGING & 0x01 != 0: Show calls to malloc & free.
   LOGGING & 0x02 != 0: Show marshalled value vectors. */
#define	LOGGING 0x00

/* ----------------------------------------------------------------------- */

#if defined(xdr_fnServerObjectReadValues_rets)
#undef xdr_fnServerObjectReadValues_rets
#endif

#if defined(xdr_fnServerObjectWriteValues_args)
#undef xdr_fnServerObjectWriteValues_args
#endif

#if defined(xdr_fnServerObjectPeekValues_rets)
#undef xdr_fnServerObjectPeekValues_rets
#endif

#if defined(xdr_fnServerObjectPoke_args)
#undef xdr_fnServerObjectPoke_args
#endif

/* ----------------------------------------------------------------------- */
static const char	szErrorEmbeddedVector []	=
  "Passed NULL pointer on embedded value vector at\n"
  "       %sserializing with type tag %s (0x%X).";
static const char	szDeserializing []		= "de";
static const char	szNoClassInfoFound []		=
  "No class info found for type tag 0x%X.";

/* ----------------------------------------------------------------------- */
static bool_t	xdr_values_vector_allocate	( XDR		* pXdr,
						  LPVOID	* ppVal,
						  u_int		nSizeInBytes )
{
  bool_t	bDone	= FALSE;

  PROCEDURE	( xdr_values_vector_allocate );
  
  if ( pXdr->x_op == XDR_DECODE && *ppVal == NULL ) {
    *ppVal	= malloc ( nSizeInBytes );
    ASSERT ( *ppVal != NULL );
    bDone	= ( *ppVal != NULL );
  }

  RETURN ( bDone );
 
} /* xdr_values_vector_allocate */

/* ----------------------------------------------------------------------- */
static bool_t	xdr_values_vector_elements	( XDR		* pXdr,
						  SHTYPETAG	eTyp,
						  u_int		* pLen,
						  LPVOID	* ppVal,
						  bool_t	bIsEmbedded,
						  u_int		nSizeInWords,
						  u_int		nSizeOfElement,
						  xdrproc_t	pfnElement )
{
  bool_t	bDone	= FALSE;
  LPBYTE	pVal	= NULL;
  u_int		i;

  PROCEDURE	( xdr_values_vector_elements );

  nSizeOfElement	/= nBitsPerByte;

  if ( pXdr->x_op == XDR_FREE ) {
#if (LOGGING+0) & 0x01
    INFO (( "Freeing value vector for type %s (0x%X),\n"
	    "       %d elements, %d bytes, address 0x%X.",
	    fnTypeTagName ( eTyp ), eTyp,
	    *pLen, nSizeInWords * nSizeOfPostoreWord, *ppVal ));
#endif
    bDone	= ( bIsEmbedded ) ?
      (bool_t) TRUE :
      xdr_array ( pXdr, (char **) ppVal, pLen, ~0,
		  nSizeOfElement, pfnElement );
  } else {
    bDone	= TRUE;
    if ( nSizeInWords > 0 ) {
      if ( pXdr->x_op == XDR_DECODE && *ppVal == NULL ) {
	if ( bIsEmbedded ) {
	  ERROR (( szErrorEmbeddedVector,
		   ( ( pXdr->x_op == XDR_DECODE ) ?
		     szDeserializing : szEmpty ),
		   fnTypeTagName ( eTyp ), eTyp ));
	  bDone	= FALSE;
	} else {
	  bDone	=
	    xdr_values_vector_allocate ( pXdr, ppVal,
					 nSizeInWords * nSizeOfPostoreWord );
#if (LOGGING+0) & 0x01
	  INFO (( "Allocated value vector for type %s (0x%X),\n"
		  "       %d elements, %d bytes, address 0x%X.",
		  fnTypeTagName ( eTyp ), eTyp,
		  *pLen, nSizeInWords * nSizeOfPostoreWord, *ppVal ));
#endif
	}
      }
      for ( i = 0, pVal = (LPBYTE) *ppVal; i < *pLen && bDone;
	    i++, pVal += nSizeOfElement ) {
	bDone	= ( *pfnElement ) ( pXdr, (char *) pVal );
      }
#if (LOGGING+0) & 0x02
      {
	char	szData [ 256 ];
	switch ( eTyp ) {
	case eshCharacterTag:
	  sprintf ( szData, "`%.*s'", sizeof ( szData ) - 3, *ppVal );
	  szData [ sizeof ( szData ) - 1 ]	= '\0';
	  break;
	case eshShortFloatTag:
	case eshSingleFloatTag:
	  sprintf ( szData, "%g", (double) * (float *) *ppVal );
	  break;
	case eshDoubleFloatTag:
	  sprintf ( szData, "%g", * (double *) *ppVal );
	  break;
	default:
	  szData [ 0 ]	= '\0';
	  break;
	}
	INFO (( "Value vector for type %s (0x%X),\n"
		"       %d elements, %d bytes, address 0x%X\n"
		"%s%s%s"
		"       has been %sserialized.",
		fnTypeTagName ( eTyp ), eTyp,
		*pLen, nSizeInWords * nSizeOfPostoreWord, *ppVal,
		( ( szData [ 0 ] != '\0' ) ? "       data " : szEmpty ),
		( ( szData [ 0 ] != '\0' ) ? szData : szEmpty ),
		( ( szData [ 0 ] != '\0' ) ? "\n" : szEmpty ),
		( ( pXdr->x_op == XDR_DECODE ) ?
		  szDeserializing : szEmpty ) ));
      }
#endif
    }
  }

  RETURN ( bDone );
 
} /* xdr_values_vector_elements */

/* ----------------------------------------------------------------------- */
static bool_t	xdr_values_vector_typed		( XDR		* pXdr,
						  SHTYPETAG	eTyp,
						  u_int		* pLen,
						  LPVOID	* ppVal,
						  bool_t	bIsEmbedded,
						  u_int		nSizeInWords,
						  u_int	nSizeOfElement )
{
  bool_t	bDone	= FALSE;
  u_int		w = 0;

  PROCEDURE	( xdr_values_vector_typed );

  switch ( eTyp ) {

  case eshSingleFloatTag:
    bDone	=
      xdr_values_vector_elements ( pXdr, eTyp, pLen, ppVal, bIsEmbedded,
				   nSizeInWords, nSizeOfElement, xdr_float );
    break;

  case eshDoubleFloatTag:
    bDone	=
      xdr_values_vector_elements ( pXdr, eTyp, pLen, ppVal, bIsEmbedded,
				   nSizeInWords, nSizeOfElement, xdr_double );
    break;

  case eshCharacterTag:
    bDone	=
      xdr_values_vector_opaque ( pXdr, eshCharacterTag, *pLen,
				 ppVal, bIsEmbedded );
    break;

  case eshSignedByte32Tag:
    w		= nSizeInWords;
    bDone	=
      xdr_values_vector_elements ( pXdr, eshSignedByte32Tag, &w, ppVal,
				   bIsEmbedded, nSizeInWords,
				   sizeof ( int ) * nBitsPerByte, xdr_int );
    break;

  case eshSignedByte2Tag:
  case eshSignedByte4Tag:
  case eshSignedByte8Tag:
  case eshUnsignedByte1Tag:
  case eshUnsignedByte2Tag:
  case eshUnsignedByte4Tag:
  case eshUnsignedByte8Tag:
    w		= nSizeInWords * nSizeOfPostoreWord;
    bDone	=
      xdr_values_vector_opaque ( pXdr, eTyp, w, ppVal, bIsEmbedded );
    break;

  case eshSignedByte16Tag:
  case eshUnsignedByte16Tag:

  case eshUnsignedByte32Tag:
    w		= nSizeInWords;
    bDone	=
      xdr_values_vector_elements ( pXdr, eshUnsignedByte32Tag, &w, ppVal,
				   bIsEmbedded, nSizeInWords,
				   sizeof ( u_int ) * nBitsPerByte,
				   xdr_u_int );
    break;

  default:
    if ( nSizeInWords > 0 ) {
      WARN (( "Encountered unknown type tag %s (0x%X) at\n"
	      "       %sserializing a value vector with %d element(s).",
	      fnTypeTagName ( eTyp ), eTyp,
	      ( ( pXdr->x_op == XDR_DECODE ) ? szEmpty : "de" ),
	      *pLen ));
      bDone	= FALSE;
    } else {
      bDone	= TRUE;
    }
    break;
  }


  RETURN ( bDone );
} /* xdr_values_vector_typed */

/* ----------------------------------------------------------------------- */
static bool_t	xdr_values_vector_opaque	( XDR		* pXdr,
						  SHTYPETAG	eTyp,
						  u_int		nLen,
						  LPVOID	* ppVal,
						  bool_t	bIsEmbedded )
{
  bool_t		bDone	= FALSE;
  LPSTR			* pVal	= NULL;

  PROCEDURE	( xdr_values_vector_opaque );

  if ( pXdr->x_op == XDR_FREE ) {
#if (LOGGING+0) & 0x01
    INFO (( "Freeing opaque value vector for type %s (0x%X),\n"
	    "       %d elements, %d bytes, address 0x%X.",
	    fnTypeTagName ( eTyp ), eTyp,
	    nLen, nLen, *ppVal ));
#endif
    bDone	= TRUE;
    if ( ! bIsEmbedded && *ppVal != NULL ) {
      free ( *ppVal );
      *ppVal	= NULL;
    }
  } else {
    bDone	= TRUE;
    if ( nLen > 0 ) {
      if ( pXdr->x_op == XDR_DECODE && *ppVal == NULL ) {
	if ( bIsEmbedded ) {
	  ERROR (( szErrorEmbeddedVector,
		   ( ( pXdr->x_op == XDR_DECODE ) ?
		     szDeserializing : szEmpty ),
		   fnTypeTagName ( eTyp ), eTyp ));
	  bDone	= FALSE;
	} else {
	  bDone	= xdr_values_vector_allocate ( pXdr, ppVal, nLen );
#if (LOGGING+0) & 0x01
	  INFO (( "Allocated opaque value vector for type %s (0x%X),\n"
		  "       %d elements, %d bytes, address 0x%X.",
		  fnTypeTagName ( eTyp ), eTyp,
		  nLen, nLen, *ppVal ));
#endif
	}
      }
      if ( bDone ) {
	bDone		= xdr_opaque ( pXdr, *ppVal, nLen );
#if (LOGGING+0) & 0x02
	{
	  char	szData [ 256 ];
	  switch ( eTyp ) {
	  case eshCharacterTag:
	    sprintf ( szData, "`%.*s'", sizeof ( szData ) - 3, *ppVal );
	    szData [ sizeof ( szData ) - 1 ]	= '\0';
	    break;
	  default:
	    szData [ 0 ]	= '\0';
	    break;
	  }
	  INFO (( "Opaque value vector for type %s (0x%X),\n"
		  "       %d elements, %d bytes, address 0x%X\n"
		  "%s%s%s"
		  "       has been %sserialized.",
		  fnTypeTagName ( eTyp ), eTyp,
		  nLen, nLen, *ppVal,
		  ( ( szData [ 0 ] != '\0' ) ? "       data " : szEmpty ),
		  ( ( szData [ 0 ] != '\0' ) ? szData : szEmpty ),
		  ( ( szData [ 0 ] != '\0' ) ? "\n" : szEmpty ),
		  ( ( pXdr->x_op == XDR_DECODE ) ?
		    szDeserializing : szEmpty ) ));
	}
#endif
      }
    }
  }

  RETURN ( bDone );
 
} /* xdr_values_vector_opaque */

/* ----------------------------------------------------------------------- */
static bool_t	xdr_values_vector_t	( XDR		* pXdr,
					  SHTYPETAG	* pTyp,
					  u_int		* pLen,
					  LPVOID	* ppVal,
					  bool_t	bIsEmbedded,
					  u_int		* pnSizeInBytes )
{
  bool_t	bDone	= FALSE;
  LPCLASSINFO	pClassInfo;
  u_int		nSizeInWords = 0, nSizeOfElement = 0;

  PROCEDURE	( xdr_values_vector_t );

  if ( pnSizeInBytes != NULL ) {
    *pnSizeInBytes	= 0;
  }

  if ( ! xdr_enum ( pXdr, (enum_t*) pTyp ) ) {
    RETURN ( FALSE );
  }

  if ( ! xdr_u_int ( pXdr, pLen ) ) {
    RETURN ( FALSE );
  }

  if ( *pTyp == NULLTYPETAG || *pLen == 0 ) {
    bDone	= TRUE;
    RETURN ( bDone );
  }

  pClassInfo		= (LPCLASSINFO) FindClassInfo ( *pTyp );
  if ( pClassInfo == NULL ) {
    ERROR (( szNoClassInfoFound, *pTyp ));
    bDone	= FALSE;
    RETURN ( bDone );
  }

  nSizeOfElement	= pClassInfo->nFixSizeValue;
  ASSERT ( nSizeOfElement > 0 );
  nSizeInWords		= ( (*pTyp) != NULLTYPETAG && (*pLen) > 0 ) ?
    AlignBitsToWords ( nSizeOfElement * (*pLen) ) : 0;

  bDone	= xdr_values_vector_typed ( pXdr, *pTyp, pLen, ppVal, bIsEmbedded,
				    nSizeInWords, nSizeOfElement );

  if ( pnSizeInBytes != NULL ) {
    *pnSizeInBytes	= ( bDone ) ? nSizeInWords * nSizeOfPostoreWord : 0;
  }

  RETURN ( bDone );
} /* xdr_values_vector_t */

/* ----------------------------------------------------------------------- */
bool_t	xdr_fnPatchedServerObjectReadValues_rets
  ( XDR					* pXdr,
    struct fnServerObjectReadValues_rets* pArguments )
{
  PROCEDURE ( xdr_fnPatchedServerObjectReadValues_rets );

  /* Slot `int ReturnValue': */
  if ( ! xdr_int ( pXdr, &pArguments->ReturnValue ) ) {
    RETURN ( FALSE );
  }

#if (NATIVERPC+0)

  /* Slot `SHTYPETAG pnElementTypeTag': */
  if ( ! xdr_SHTYPETAG ( pXdr, &pArguments->pnElementTypeTagOut ) ) {
    RETURN ( FALSE );
  }

  /* Slot `int pnSizeInElements': */
  if ( ! xdr_int ( pXdr, &pArguments->pnSizeInElementsOut ) ) {
    RETURN ( FALSE );
  }

  /* Slot `void_vector_t pBuffer': */
  if ( ! xdr_void_vector_t ( pXdr, &pArguments->pBuffer ) ) {
    RETURN ( FALSE );
  }

#else

  if ( ! xdr_values_vector_t ( pXdr, &pArguments->pnElementTypeTagOut,
			       &pArguments->pnSizeInElementsOut,
			       &pArguments->pBuffer.void_vector_t_val,
			       FALSE,
			       &pArguments->pBuffer.void_vector_t_len ) ) {
    RETURN ( FALSE );
  }

#endif

  /* Slot `int nErrorLvl': */
  if ( ! xdr_int ( pXdr, &pArguments->nErrorLvl ) ) {
    RETURN ( FALSE );
  }

  /* Slot `string_t pszErrorMsg': */
  if ( ! xdr_string_t ( pXdr, &pArguments->pszErrorMsg ) ) {
    RETURN ( FALSE );
  }

  RETURN ( TRUE );

} /* xdr_fnPatchedServerObjectReadValues_rets */

/* ----------------------------------------------------------------------- */
bool_t	xdr_fnPatchedServerObjectWriteValues_args
  ( XDR					* pXdr,
    struct fnServerObjectWriteValues_args* pArguments )
{
  PROCEDURE ( xdr_fnPatchedServerObjectWriteValues_args );

  /* Slot `SHORTOBJID oShortObjIdHeap': */
  if ( ! xdr_SHORTOBJID ( pXdr, &pArguments->oShortObjIdHeap ) ) {
    RETURN ( FALSE );
  }

  /* Slot `SHORTOBJID oShortObjId': */
  if ( ! xdr_SHORTOBJID ( pXdr, &pArguments->oShortObjId ) ) {
    RETURN ( FALSE );
  }

  /* Slot `SHORTOBJID oExpectingClass': */
  if ( ! xdr_SHORTOBJID ( pXdr, &pArguments->oExpectingClass ) ) {
    RETURN ( FALSE );
  }

  /* Slot `SHTYPETAG nExpectingTypeTag': */
  if ( ! xdr_SHTYPETAG ( pXdr, &pArguments->nExpectingTypeTag ) ) {
    RETURN ( FALSE );
  }

  /* Slot `int nIndex': */
  if ( ! xdr_int ( pXdr, &pArguments->nIndex ) ) {
    RETURN ( FALSE );
  }

#if (NATIVERPC+0)

  /* Slot `SHTYPETAG nElementTypeTag': */
  if ( ! xdr_SHTYPETAG ( pXdr, &pArguments->nElementTypeTag ) ) {
    RETURN ( FALSE );
  }

  /* Slot `int nSizeInElements': */
  if ( ! xdr_int ( pXdr, &pArguments->nSizeInElements ) ) {
    RETURN ( FALSE );
  }

  /* Slot `void_vector_t pBuffer': */
  if ( ! xdr_void_vector_t ( pXdr, &pArguments->pBuffer ) ) {
    RETURN ( FALSE );
  }

#else

  if ( ! xdr_values_vector_t ( pXdr, &pArguments->nElementTypeTag,
			       &pArguments->nSizeInElements,
			       &pArguments->pBuffer.void_vector_t_val,
			       FALSE,
			       &pArguments->pBuffer.void_vector_t_len ) ) {
    RETURN ( FALSE );
  }

#endif

  RETURN ( TRUE );

} /* xdr_fnPatchedServerObjectWriteValues_args */

/* ----------------------------------------------------------------------- */
bool_t	xdr_fnPatchedServerObjectPeekValues_rets
  ( XDR					* pXdr,
    struct fnServerObjectPeekValues_rets* pArguments )
{
  PROCEDURE ( xdr_fnPatchedServerObjectPeekValues_rets );

  /* Slot `SHLOCK ReturnValue': */
  if ( ! xdr_SHLOCK ( pXdr, &pArguments->ReturnValue ) ) {
    RETURN ( FALSE );
  }

  /* Slot `u_int_vector_t pnElementTypeTagsOut': */
  if ( ! xdr_u_int_vector_t ( pXdr, &pArguments->pnElementTypeTagsOut ) ) {
    return ( FALSE );
  }

  /* Slot `u_int_vector_t pnSizesInElementsOut': */
  if ( ! xdr_u_int_vector_t ( pXdr, &pArguments->pnSizesInElementsOut ) ) {
    return ( FALSE );
  }

#if (NATIVERPC+0)

  /* Slot `void_vector_t pBuffer': */
  if ( ! xdr_void_vector_t ( pXdr, &pArguments->pBuffer ) ) {
    RETURN ( FALSE );
  }

#else

  /* Slot `void_vector_t pBuffer': */
  {
    bool_t	bDone = FALSE;

    ASSERT ( pArguments->pnElementTypeTagsOut.u_int_vector_t_len ==
	     pArguments->pnSizesInElementsOut.u_int_vector_t_len );
    if ( pXdr->x_op == XDR_FREE ) {
#if (LOGGING+0) & 0x01
      INFO (( "Freeing value vector %d bytes, address 0x%X.",
	      pArguments->pBuffer.void_vector_t_len,
	      pArguments->pBuffer.void_vector_t_val ));
#endif
      if ( pArguments->pBuffer.void_vector_t_val != NULL ) {
	free ( pArguments->pBuffer.void_vector_t_val );
	pArguments->pBuffer.void_vector_t_val	= NULL;
      }
      pArguments->pBuffer.void_vector_t_len	= 0;
      bDone					= TRUE;
    } else {
      u_int	i, j, n;
      FIXNUM	nTotalSizeInWords = 0;
      n	= pArguments->pnElementTypeTagsOut.u_int_vector_t_len;
      if ( pXdr->x_op == XDR_DECODE &&
	   pArguments->pBuffer.void_vector_t_val == NULL ) {
	FIXNUM	nSizeInBytes;
	nTotalSizeInWords	= 
	  fnTypeTagSizeValue ( n,
			       pArguments->
			       pnElementTypeTagsOut.u_int_vector_t_val,
			       pArguments->
			       pnSizesInElementsOut.u_int_vector_t_val );
	nSizeInBytes	= nTotalSizeInWords * nSizeOfPostoreWord;
	bDone	=
	  xdr_values_vector_allocate ( pXdr,
				       &pArguments->pBuffer.void_vector_t_val,
				       nSizeInBytes );
	if ( ! bDone ) {
	  RETURN ( bDone );
	}
	pArguments->pBuffer.void_vector_t_len	= nSizeInBytes;
#if (LOGGING+0) & 0x01
	INFO (( "Allocated value vector %d bytes, address 0x%X.",
		nSizeInBytes, pArguments->pBuffer.void_vector_t_val ));
#endif
      }
      bDone		= TRUE;
      ASSERT ( pArguments->pBuffer.void_vector_t_len %
	       nSizeOfPostoreWord == 0 );
      nTotalSizeInWords	=
	pArguments->pBuffer.void_vector_t_len / nSizeOfPostoreWord;
      for ( i = 0, j = 0; bDone && i < n; i++ ) {

	SHTYPETAG	nTypeTagValues	=
	  pArguments->pnElementTypeTagsOut.u_int_vector_t_val [ i ];

	u_int		nValues		=
	  pArguments->pnSizesInElementsOut.u_int_vector_t_val [ i ];

	LPCLASSINFO	pClassInfo	= (LPCLASSINFO)
	  FindClassInfo (  nTypeTagValues );

	u_int	nSizeInWords = 0, nSizeOfElement = 0;
	psint	* pVal = NULL;

	if ( pClassInfo == NULL ) {
	  ERROR (( szNoClassInfoFound, nTypeTagValues ));
	  bDone	= FALSE;
	  RETURN ( bDone );
	}
	nSizeOfElement		= pClassInfo->nFixSizeValue;
	ASSERT ( nSizeOfElement > 0 );

	nSizeInWords		=
	  ( nTypeTagValues != NULLTYPETAG && nValues > 0 ) ?
	  AlignBitsToWords ( nSizeOfElement * nValues ) : 0;
	ASSERT ( nSizeInWords > 0 );

	ASSERT ( j < nTotalSizeInWords );
	pVal	= & ( (psint *) pArguments->pBuffer.void_vector_t_val ) [ j ];
	bDone	=
	  xdr_values_vector_typed ( pXdr, nTypeTagValues, &nValues,
				    (LPVOID *) &pVal,
				    TRUE, nSizeInWords, nSizeOfElement );
	j	+= nSizeInWords;
      }
    }
    if ( ! bDone ) {
      RETURN ( bDone );
    }
  }

#endif

  /* Slot `int nErrorLvl': */
  if ( ! xdr_int ( pXdr, &pArguments->nErrorLvl ) ) {
    RETURN ( FALSE );
  }

  /* Slot `string_t pszErrorMsg': */
  if ( ! xdr_string_t ( pXdr, &pArguments->pszErrorMsg ) ) {
    RETURN ( FALSE );
  }

  RETURN ( TRUE );
} /* xdr_fnPatchedServerObjectPeekValues_rets */

/* ----------------------------------------------------------------------- */
bool_t	xdr_fnPatchedServerObjectPoke_args
  ( XDR					* pXdr,
    struct fnServerObjectPoke_args	* pArguments )
{
  PROCEDURE ( xdr_fnPatchedServerObjectPoke_args );

  /* Slot `SHORTOBJID oShortObjIdHeap': */
  if ( ! xdr_SHORTOBJID ( pXdr, &pArguments->oShortObjIdHeap ) ) {
    RETURN ( FALSE );
  }

  /* Slot `SHORTOBJID oShortObjId': */
  if ( ! xdr_SHORTOBJID ( pXdr, &pArguments->oShortObjId ) ) {
    RETURN ( FALSE );
  }

  /* Slot `int nSlots': */
  if ( ! xdr_int ( pXdr, &pArguments->nSlots ) ) {
    RETURN ( FALSE );
  }

  /* Slot `u_int_vector_t pSlots': */
  if ( ! xdr_u_int_vector_t ( pXdr, &pArguments->pSlots ) ) {
    RETURN ( FALSE );
  }

#if (NATIVERPC+0)

  /* Slot `SHTYPETAG nElementTypeTag': */
  if ( ! xdr_SHTYPETAG ( pXdr, &pArguments->nElementTypeTag ) ) {
    RETURN ( FALSE );
  }

  /* Slot `int nSizeInElements': */
  if ( ! xdr_int ( pXdr, &pArguments->nSizeInElements ) ) {
    RETURN ( FALSE );
  }

  /* Slot `void_vector_t pValues': */
  if ( ! xdr_void_vector_t ( pXdr, &pArguments->pValues ) ) {
    RETURN ( FALSE );
  }

#else

  if ( ! xdr_values_vector_t ( pXdr, &pArguments->nElementTypeTag,
			       &pArguments->nSizeInElements,
			       &pArguments->pValues.void_vector_t_val,
			       FALSE,
			       &pArguments->pValues.void_vector_t_len ) ) {
    RETURN ( FALSE );
  }

#endif

  RETURN ( TRUE );
} /* xdr_fnPatchedServerObjectPoke_args */

/*
  Local variables:
  buffer-file-coding-system: iso-latin-1-unix
  End:
*/
