/* -------------------------------------------------------------------------
| Module	pobs.h
| Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
|		kirschke@kogs26.informatik.uni-hamburg.de
| Date		09.11.93
| Description	Persistent OBject Store
|		~          ~~     ~
|		for LISP objects.
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Types
 ------------------------------------------------------------------------- */
typedef	DWORD	OBJID;

/* -------------------------------------------------------------------------
| Extern function prototypes
 ------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
| Function	fn
| Arguments	  
|		
| Return	The objid of the stored object
| Description	
 ------------------------------------------------------------------------- */
void		fnPOBSinit		( LWOBJECT lwNil );

/* -------------------------------------------------------------------------
| Function	fn
| Arguments	  
|		
| Return	The objid of the stored object
| Description	
 ------------------------------------------------------------------------- */
LWOBJECT	fnPOBSstoreObject	( LWOBJECT lwObjectToStore );

/* -------------------------------------------------------------------------
| Function	fn
| Arguments	  
|		
| Return	The loaded object
| Description	
 ------------------------------------------------------------------------- */
LWOBJECT	fnPOBSloadObject	( LWOBJECT lwnObjId );
