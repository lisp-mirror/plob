
BEGIN {
  if( szAwk ~ /nawk/ ) {
    # Solaris' nawk:
    FS				= /[\t ]+/;
  } else {
    # Linux' gawk:
    FS				= " ";
  }
  FALSE				= 0;
  TRUE				= 1;
  szProgram			= toupper( szOutputFile );
  sub( /[.][^.]*$/, "", szProgram );
  szCapitalized			= capitalize( szProgram );
  nGlobalLineCounter		= 0;
  nGlobalConstCounter		= 0;
  nGlobalTypedefCounter		= 0;
  nGlobalTypedefArgsCounter	= 0;
  nGlobalEnumCounter		= 0;
  nGlobalFunctionCounter	= 1;
}

function capitalize( szString ) {
  return toupper( substr( szString, 1, 1 ) ) \
    tolower( substr( szString, 2 ) );
} # capitalize

function replace( szSource, szSearch, szReplace,
		  # Local variables: 
		  nLength, nIndex ) {
  nLength	= length( szSearch );
  nIndex	= index( szSource, szSearch );
  if ( nIndex > 0 ) {
    return substr( szSource, 1, nIndex - 1 ) \
		   szReplace substr( szSource, nIndex + nLength );
  } else {
    return szSource;
  }
} # replace

function greplace( szSource, szSearch, szReplace,
		   # Local variables: 
		   nLength, nIndex ) {
  nLength	= length( szSearch );
  while( TRUE ) {
    nIndex	= index( szSource, szSearch );
    if ( nIndex > 0 ) {
      szSource	= substr( szSource, 1, nIndex - 1 ) \
		  szReplace substr( szSource, nIndex + nLength );
    } else {
      return szSource;
    }
  }
} # greplace

function fnEval( szExpression,
                 # Local variables:
		 szEvalCmd, szEvaluated ) {
  szEvalCmd="evalexpr -a " szAwk " '" szExpression "'";
  szEvalCmd | getline szEvaluated;
  close( szEvalCmd );
  return szEvaluated;
} # fnEval

function fnScanSimpleBlock( szLabel,
			    # Local variables:
			    szBlock, szExpression, szEval, szEvaluated,
			    nIndex ) {
  sub( szLabel, "" );
  szBlock	= $0;
  while( getline > 0 && $1 !~ "@end@" ) {
    # Solaris' nawk:
    # szBlock	= szBlock "\n" $0;
    # Linux' gawk:
    szBlock	= szBlock " " $0;
  }
  if( $2 !~ szLabel ) {
    print "#error Block " szLabel " ended by label " $2;
  }
  while( szBlock ~ /\(/ ) {
    # Evaluate the "(" ... ")" expression:
    szExpression	= szBlock;
    sub( /^[^(]*\(/, "(", szExpression );
    sub( /[\t\n ]*[;,}].*$/, "", szExpression );
    szEval		= szExpression;
    gsub( /[|&]/, ",", szEval );
    gsub( /\n/, " ", szEval );
    szEvaluated		= fnEval( szEval );
    nIndex		= index( szBlock, szExpression );
    szBlock		= greplace( szBlock, szExpression, szEvaluated );
  }
  return szBlock;
} # fnScanSimpleBlock

function fnCommentSize( szExpression ) {
  # Comment out <> size specifications:
  gsub( /<[^>]+>/, " /* & */", szExpression );
  return szExpression;
} # fnCommentSize

function fnScanArgsBlock( szLabel,
			  # Local variables:
			  szBlock, szFunctionType, szFunctionName,
			  szFunctionArgs, nArguments, szArguments,
			  i, szSlot,
			  szArgument, nArgumentsIn, nArgumentsOut,
			  szFunctionArgsIn, szFunctionArgsOut ) {
  sub( szLabel, "" );
  szBlock	= $0;
  while( getline > 0 && $1 !~ "@end@" ) {
    # Solaris' nawk:
    # szBlock	= szBlock "\n" $0;
    # Linux' gawk:
    szBlock	= szBlock " " $0;
  }
  if( $2 !~ szLabel ) {
    print "#error Block " szLabel " ended by label " $2;
  }
  # 1998/11/16: HK: This removed all `,' from argument list:
  # gsub( /[\t ]*,[\t ]*\n*/, "", szBlock );
  gsub( /[\t ]*;[\t\n ]*,[\t ]*\n*/, ";", szBlock );
  $0					= szBlock;
  szFunctionType			= $1;
  szFunctionName			= $2;
  $1					= "";
  $2					= "";
  szFunctionArgs			= $0;
  sub( /^[^a-zA-Z_]*/, "" );
  sub( /[^a-zA-Z_0-9;,]*$/, "" );
  gsub( /;/, "&\n" );
  # Split the function arguments into in and out arguments:
  gsub( /;[\t ]*\)/, "", szFunctionArgs );
  # 1998/11/16: HK: Split only by ';':
  # nArguments	= split( szFunctionArgs, szArguments, /[\t ]*[,;][\t ]*/ );
  nArguments	= split( szFunctionArgs, szArguments, /[\t ]*[;][\t ]*/ );
  szFunctionArgs	= FALSE;
  nArgumentsIn		= 0;
  szFunctionArgsIn	= FALSE;
  nArgumentsOut		= 0;
  szFunctionArgsOut	= FALSE;
  for( i = 1; i <= nArguments; i++ ) {
    szArgument	= szArguments[ i ];
    sub( /^[^a-zA-Z_]*/, "", szArgument );
    sub( /[^a-zA-Z_0-9]*$/, "", szArgument );
    if( szArgument ~ /@out@/ ) {
      $0		= replace( $0, szArgument, "" );
      szFunctionArgsOut	= ( szFunctionArgsOut ) ? \
	szFunctionArgsOut " @and@ " szArgument : szArgument;
      nArgumentsOut++;
      if( szArgument ~/</ ) {
	# Arguments containing <>-expressions are always passed
	# by reference:
	sub( /@out@/, "", szArgument );
	szSlot        = szArgument;
	sub( /[a-zA-Z_][a-zA-Z0-9_]*$/, "@arg@&", szSlot );
	sub( /^.*@arg@/, "", szSlot );
	$0    = $0 "\nint " szSlot "Size;";
        szFunctionArgsIn	= ( szFunctionArgsIn ) ? \
          szFunctionArgsIn " @and@ " szArgument : szArgument;
	nArgumentsIn++;
      } else {
	sub( /@out@/, "*", szArgument );
      }
    } else {
      szFunctionArgsIn        = ( szFunctionArgsIn ) ? \
        szFunctionArgsIn " @and@ " szArgument : szArgument;
      nArgumentsIn++;
    }
    szFunctionArgs    = ( szFunctionArgs ) ? \
      szFunctionArgs " @and@ " szArgument : szArgument;
  }
  if( nArgumentsOut > 0 || szFunctionType !~ /^void$/ ) {
    GlobalFunctionBatch[ szFunctionName ]	= "";
    nArgumentsOut++;
    szArgument		= "int nErrorLvl";
    szFunctionArgsOut	= ( szFunctionArgsOut ) ? \
      szFunctionArgsOut " @and@ " szArgument : szArgument;
    nArgumentsOut++;
    szArgument		= "string_t pszErrorMsg";
    szFunctionArgsOut	= ( szFunctionArgsOut ) ? \
      szFunctionArgsOut " @and@ " szArgument : szArgument;
  } else {
    GlobalFunctionBatch[ szFunctionName ]	= "_batch";
  }
  GlobalFunctionArgs[ szFunctionName ]        = ( szFunctionArgs ) ? \
    "(" szFunctionArgs ")" : "( void )";
  if( nArgumentsOut ) {
    gsub( /@out@/, "", szFunctionArgsOut );
    GlobalFunctionArgsOut[ szFunctionName ]   = "(" szFunctionArgsOut ")";
    gsub( /@and@/, ";\n  ", szFunctionArgsOut );
    if( szFunctionType !~ "void" ) {
      szFunctionArgsOut       = \
        szFunctionType " ReturnValue;\n  " szFunctionArgsOut;
      nArgumentsOut++;
    }
    szFunctionArgsOut = fnCommentSize( szFunctionArgsOut );
    if( nArgumentsOut > 1 ) {
      GlobalTypedefArgs[ nGlobalTypedefArgsCounter ]  = \
        "struct " szFunctionName "_rets {\n  " \
        szFunctionArgsOut ";\n};";
      nGlobalTypedefArgsCounter++;
    }
  } else {
    GlobalFunctionArgsOut[ szFunctionName ]   = FALSE;
  }
  if( nArgumentsIn ) {
    GlobalFunctionArgsIn[ szFunctionName ]    = "(" szFunctionArgsIn ")";
    sub( /^[^a-zA-Z0-9_]*;/, "" );
    gsub( /;[^a-zA-Z0-9_]*;/, ";" );
  } else {
    GlobalFunctionArgsIn[ szFunctionName ]    = FALSE;
  }
  szBlock     = ( nArgumentsIn > 1 ) ? $0 : FALSE;
  if( szBlock ) {
    szBlock     = "struct " szFunctionName "_args {\n  " \
      fnCommentSize( szBlock ) "\n};";
  }
  return szBlock;
} # fnScanArgsBlock

function fnScanFunctionBlock( szLabel,
			      # Local variables:
			      szBlock ) {
  sub( szLabel, "" );
  szBlock	= $0;
  while( getline > 0 && $1 !~ "@end@" ) {
    sub( "@counter@", nGlobalFunctionCounter );
    # Solaris' nawk:
    # szBlock	= szBlock "\n" $0;
    # Linux' gawk:
    szBlock	= szBlock " " $0;
  }
  if( $2 !~ szLabel ) {
    print "#error Block " szLabel " ended by label " $2;
  }
  return szBlock;
} # fnScanFunctionBlock

function fnSplitArguments( szFunctionArgs,
			   # Output variables:
			   szTypes, szArguments, szAccessors, szSizes,
			   # Local variables:
			   n, szSplitted, nArguments, i,
			   szType, szArgument, szAccessor, szSize ) {
  # 1998/11/16 HK: Arguments are now delimted by @and@:
  # n	= split( szFunctionArgs, szSplitted, /[\n\t %]*[,;][\n\t %]*/ );
  n	= split( szFunctionArgs, szSplitted, /[\n\t %]*@and@[\n\t %]*/ );
  nArguments	= 0;
  for( i = 1; i <= n; i++ ) {
    szArgument	= szSplitted[ i ];
    sub( /^[^a-zA-Z_]*/, "", szArgument );
    sub( /[^a-zA-Z_0-9]*$/, "", szArgument );
    if( szArgument ) {
      if( szArgument ~ /</ ) {
	szAccessor	= szArgument;
        sub( /<.*$/, "", szAccessor );
	szSize	= szArgument;
        sub( /^[^<]*</, "", szSize );
        sub( />[^>]*$/, "", szSize );
      } else {
	szAccessor	= FALSE;
	szSize		= FALSE;
      }
      szType	= szArgument;
      # The last word from szArgument is the arguments name:
      sub( /[a-zA-Z_][a-zA-Z0-9_]*$/, "@arg@&", szArgument );
      sub( /^.*@arg@/, "", szArgument );
      if( szArgument !~ "void" ) {
        sub( szArgument, "", szType );
	sub( /[\t ]+$/, "", szType );
        szTypes[ ++nArguments ]		= szType;
        szArguments[ nArguments ]	= szArgument;
        szAccessors[ nArguments ]	= szAccessor;
        szSizes[ nArguments ]		= szSize;
      }
    }
  }
  return nArguments;
} # fnSplitArguments

function fnIn( szElement, szArray, n, i ) {
  for( i = 1; i <= n; i++ )
    if( szArray[ i ] ~ "^" szElement "$" )
      return i;
  return 0;
} # fnIn

function fnPrintFunction( szDeclaration,
			  # Local variables:
			  szResultType, szFunctionName,
			  szFunctionArgs, szFunctionArgsIn, szFunctionArgsOut,
			  i,
			  # szReturn,
			  szType, szArgument, szAccessor, szSize, szDeref,
			  szRpcType, szPackedArgs,
			  # nArguments,
			  szTypes, szArguments,
			  szAccessors, szSizes,
			  # nArgumentsIn,
			  szTypesIn, szArgumentsIn,
			  szAccessorsIn, szSizesIn,
			  # nArgumentsOut,
			  szTypesOut, szArgumentsOut,
			  szAccessorsOut, szSizesOut ) {
  gsub( /<[^>]+>/, "", szDeclaration );
  $0			= szDeclaration;
  szResultType		= $1;
  szFunctionName	= $2;
  szFunctionArgs	= GlobalFunctionArgs[ szFunctionName ];
  szFunctionBatch	= GlobalFunctionBatch[ szFunctionName ];
  nArguments		= \
   fnSplitArguments( szFunctionArgs, szTypes, szArguments,
		     szAccessors, szSizes );
  gsub( /_vector_t<[^>]+>/, " * /* & */", szFunctionArgs );
  gsub( /string_t<[^>]+>/, "string_t /* & */", szFunctionArgs );
  gsub( /@and@/, ",\n%\t ", szFunctionArgs );
  nArgumentsIn		= \
   fnSplitArguments( GlobalFunctionArgsIn[ szFunctionName ],
		     szTypesIn, szArgumentsIn,
                     szAccessorsIn, szSizesIn );
  nArgumentsOut		= \
   fnSplitArguments( GlobalFunctionArgsOut[ szFunctionName ],
		     szTypesOut, szArgumentsOut,
		     szAccessorsOut, szSizesOut );
  sub( szFunctionName, "fnRpc_&" szFunctionBatch, szDeclaration );
  if( nArgumentsIn == 0 ) {
    szPackedArgs	= "void";
  } else if( nArgumentsIn == 1 ) {
    szPackedArgs	= szTypesIn[ 1 ];
    gsub( /<[^>]+>/, " /* & */", szPackedArgs );
  } else {
    szPackedArgs	= szFunctionName "_args";
  }
  sub( "@arguments@", szPackedArgs, szDeclaration );
  if( nArgumentsOut > 1 ||
      ( nArgumentsOut == 1 && szResultType !~ "^void$" ) ) {
    szDeref	= TRUE;
    szRpcType	= szFunctionName "_rets";
    sub( szResultType, szRpcType, szDeclaration );
  } else if( nArgumentsOut == 1 ) {
    szDeref	= "";
    szRpcType	= szTypesOut[ 1 ];
    sub( szResultType, szRpcType, szDeclaration );
  } else {
    szDeref	= "";
    szRpcType	= "void";
  }
  if( szResultType ~ "^void$" ) {
    szReturn	= "return"; 
  } else if( szDeref ) {
    szReturn	= "return Result.ReturnValue";
  } else {
    szReturn	= "return Result";
  }
  print szDeclaration;
  # Generate the function body for the server code:
  print "%";
  print "#if RPC_SVC";
  # Generate a function prototype for parsing in the server function:
  print "%";
  print "%extern " szResultType " " szFunctionName;
  print "%\t" szFunctionArgs ";";
  print "%";
  print "%#ifdef MULTITHREAD";
  print "%bool_t";
  print "%#else";
  print "%" ( ( szFunctionBatch ) ? "void" : szRpcType ) " *";
  print "%#endif";
  print "%\tPASTE2(fnrpc_" tolower( szFunctionName ) szFunctionBatch \
    "_" nVersion ",_SVCSUFFIX)";
  print "%\t\t( " szPackedArgs " * pArguments,";
  print "%#ifdef MULTITHREAD";
  print "%\t\t  " szRpcType " * pResult,";
  print "%#endif";
  print "%\t\t  struct svc_req * pRequest )";
  print "%{";
  print "%  static const char\t__procedure__[]\t= \"server:" \
    "fnrpc_" tolower ( szFunctionName ) szFunctionBatch "_" nVersion "\";";
  print "%#ifdef MULTITHREAD";
  print "%  typedef bool_t RESULTTYPE;";
  print "%#else";
  print "%  typedef " szRpcType " * RESULTTYPE;";
  print "%#endif";
  if( ! szFunctionBatch ) {
    print "%#ifndef MULTITHREAD";
    print "%  static " szRpcType "\tResult;";
    print "%  RESULTTYPE\t\t\tpResult = &Result;";
    print "%#endif";
    print "%  LPCSTR\tpszErrorMsg;";
  }
  print "%";
  print "%  __pRequest__\t= pRequest;";
  print "%";
  print "%  RPC_SERVER_ENTRY();";
  print "%";
  if( szFunctionBatch ) {
    print "%  Reply.bAnswered\t\t= TRUE;";
  } else {
    print "%#ifndef MULTITHREAD";
    print "%  if ( pResult->pszErrorMsg == szEmpty ) {";
    print "%    pResult->pszErrorMsg\t= NULL;";
    print "%  }";
    print "%  xdr_free ( (xdrproc_t) xdr_" szRpcType ", (char *) pResult );";
    print "%#endif";
    print "%  pResult->nErrorLvl\t\t= 0;";
    print "%";
    # Allocate memory for the output values:
    for( i = 1; i <= nArgumentsOut; i++ ) {
      szSize	= szSizesOut[ i ];
      if ( szSize ) {
        szArgument	= szArgumentsOut[ i ];
        szAccessor	= szAccessorsOut[ i ];
        szType		= szTypesOut[ i ];
        if ( szType ~ /^string_t/ ) {
          szSize		= ( szDeref || nArguments > 1 ) ? \
	    "pArguments->" szArgument "Size" : "*pArguments";
	  print "%  if ( " szSize " == 0 ) {";
          print "%    pResult->" szArgument "\t= NULL;";
	  print "%  } else {";
          print "%    pResult->" szArgument "\t=";
          print "%    (string_t) malloc ( " szSize " );";
          print "%    if ( pResult->" szArgument " == NULL ) {";
	  print "%      char szErrorMsg [ 256 ];";
          print "%      sprintf ( szErrorMsg, szFormatMallocFailed,";
          print "%                __procedure__, " szSize " );";
          print "%      RPC_SERVER_ERROR ( szErrorMsg );";
          print "%      return (RESULTTYPE) NULL;";
          print "%    }";
          print "%  }";
        } else {
          szSize		= ( szDeref || nArguments > 1 ) ? \
	    "pArguments->" szArgument "Size" : "*pArguments";
	  sub( /_vector_t.*$/, "", szType );
	  if( szType ~ /as_is/ || szType ~ /void/ ) {
	    szType	= "char";
	  }
          print "%  pResult->" szArgument "." szAccessor "_len\t= " szSize ";";
	  print "%  if ( " szSize " == 0 ) {";
          print "%    pResult->" szArgument "." szAccessor "_val\t= NULL;";
	  print "%  } else {";
          print "%    pResult->" szArgument "." szAccessor "_val\t=";
          print "%      (" szType "*) malloc ( " szSize " *";
          print "%               SIZEOF ( *(pResult->" szArgument \
	    "." szAccessor "_val) ) );";
          print "%    if ( pResult->" szArgument "." szAccessor \
	    "_val == NULL ) {";
          print "%      char\tszErrorMsg [ 256 ];";
          print "%      sprintf ( szErrorMsg, szFormatMallocFailed,";
          print "%                __procedure__, " szSize ",";
          print "%                \"" szArgument "\" );";
          print "%      RPC_SERVER_ERROR ( szErrorMsg );";
          print "%      return (RESULTTYPE) NULL;";
          print "%    }";
          print "%  }";
        }
      }
    }
    print "%  Reply.bAnswered\t\t= FALSE;";
  }
  print "%  Reply.pfnCalled\t\t= (void(*)()) " szFunctionName ";";
  print "%  Reply.pfnXdrResult\t\t= (bool_t(*)()) xdr_" szRpcType ";";
  if( szFunctionBatch ) {
    print "%  Reply.pnErrorLvl\t\t= NULL;";
    print "%  Reply.ppszErrorMsg\t\t= NULL;";
    print "%  Reply.pResult\t\t\t= NULL;";
    print "%  Reply.pReturnValue\t\t= NULL;";
    print "%  Reply.nReturnValueSize\t= 0;";
  } else {
    print "%  Reply.pnErrorLvl\t\t= &pResult->nErrorLvl;";
    print "%  Reply.ppszErrorMsg\t\t= (LPCSTR *) &pResult->pszErrorMsg;";
    print "%  Reply.pResult\t\t= pResult;";
    if( szResultType ~ "void" ) {
      print "%  Reply.pReturnValue\t\t= NULL;";
      print "%  Reply.nReturnValueSize\t= 0;";
    } else {
      if( szDeref ) {
        print "%  Reply.pReturnValue\t\t= &pResult->ReturnValue;";
        print "%  Reply.nReturnValueSize\t= sizeof ( pResult->ReturnValue );";
	print "%";
        print "%  pResult->ReturnValue\t\t=";
      } else {
        print "%  Reply.pReturnValue\t\t= pResult;";
        print "%  Reply.nReturnValueSize\t= sizeof ( *pResult );";
	print "%";
        print "%  *pResult\t\t=";
      }
    }
  }
  print "%  " szFunctionName " ("
  if( nArguments ) {
    for( i = 1; i <= nArguments; i++ ) {
      szArgument	= szArguments[ i ];
      szAccessor	= szAccessors[ i ];
      szAccessor	= ( szAccessor && szTypes[ i ] !~ /^string_t/ ) ? \
        "." szAccessor "_val" : "";
      if( fnIn( szArgument, szArgumentsOut, nArgumentsOut ) ) {
	szType	= ( szAccessor || szTypes[ i ] ~ /^string_t/ ) ? "" : "&";
        szType	= ( szDeref ) ? \
	  szType "pResult->" szArgument szAccessor : \
	  szType "(*pResult)" szAccessor;
      } else {
        szType	= ( nArgumentsIn > 1 ) ? \
	 "pArguments->" szArgument szAccessor : "(*pArguments)" szAccessor;
      }
      if( i < nArguments ) {
        szType	= szType ",";
      } else {
        szType	= szType " );";
      }
      print "%\t" szType;
    }
  } else {
    print "%\t);"
  }
  if( szFunctionBatch ) {
    print "%  __pRequest__\t= NULL;";
    print "%  return (RESULTTYPE) NULL;";
  } else {
    print "%";
    print "%  if ( Reply.bAnswered ) {";
    print "%    __pRequest__\t= NULL;";
    print "%    return (RESULTTYPE) NULL;";
    print "%  }";
    print "%";
    # Check for a user runtime error:
    print "%  pResult->nErrorLvl\t= RPC_SERVER_ERRLVL();";
    print "%  if ( pResult->nErrorLvl > 0 ) {";
    print "%    pszErrorMsg\t\t= RPC_SERVER_ERRMSG();";
    print "%    if ( pszErrorMsg && *pszErrorMsg ) {";
    print "%      pResult->pszErrorMsg\t= strdup ( pszErrorMsg );";
    print "%    }";
    print "%  }";
    print "%  if ( pResult->pszErrorMsg == NULL ) {";
    print "%#ifdef MULTITHREAD";
    print "%    pResult->pszErrorMsg\t= strdup ( szEmpty );";
    print "%#else";
    print "%    pResult->pszErrorMsg\t= (LPSTR) szEmpty;";
    print "%#endif";
    print "%  }";
    print "%  __pRequest__\t= NULL;";
    print "%  return (RESULTTYPE) pResult;"
  }
  print "%} /* fnrpc_" tolower ( szFunctionName ) \
    szFunctionBatch "_" nVersion " */";
  print "#endif\t/* RPC_SVC */";
  # Generate the function body for the client code:
  print "%";
  print "#if RPC_CLNT";
  print "%" szResultType " DLLEXPORT " szFunctionName;
  print "%\t" szFunctionArgs;
  print "%{";
  print "%  static const char\t__procedure__[]\t= \"client:" \
    szFunctionName "\";";
  print "%  static const " \
    ( ( szRpcType ~ /^void$/ ) ? "char" : szRpcType ) "\tZeroResult;";
  print "%  " szRpcType "\t\t* pResult = NULL;";
  if( szResultType ~ "^void$" ) {
    print "%#ifdef MULTITHREAD";
    print "%  char\t\tResult = ZeroResult;";
    print "%#endif";
  } else {
    print "%  " szRpcType "\t\tResult = ZeroResult;";
    print "%  int\tnTries = 0;";
  }
  print "%  char\tszErrorMsg [ 1024 ];";
  if( nArgumentsIn > 1 ) {
    print "%  " szFunctionName "_args\t\tInArguments;";
  }
  print "%";
  print "%  /* Client initialization: */";
  print "%  if ( __bClientInitialize__ ) {";
  print "%    __bClientInitialize__\t= FALSE;";
  print "%    RPC_CLIENT_INITIALIZE();";
  print "%  }";
  print "%";
  print "%#if\tCLIENT_CREATE_BY_USER";
  print "%  if ( __pClient__ == NULL ) {"
  print "%    __pClient__\t= " \
    "RPC_CLIENT_CREATE ( __szHost__, __szTransport__ );";
  print "%    if ( __pClient__ == NULL ) {"
  print "%      /* Error handling for a not connected client: */";
  print "%      sprintf ( szErrorMsg, szFormatNoServer, __procedure__ );";
  print "%      RPC_CLIENT_ERROR ( szErrorMsg );";
  print "%      " szReturn ";";
  print "%    }";
  print "%  }";
  print "%#else";
  print "%  while ( __pClient__ == NULL ) {"
  print "%    __pClient__\t= " \
    "RPC_CLIENT_CREATE ( __szHost__, __szTransport__ );";
  print "%    if ( __pClient__ == NULL ) {"
  print "%      /* Error handling for a not connected client: */";
  print "%      sprintf ( szErrorMsg, szFormatNoServer, __procedure__ );";
  print "%      RPC_CLIENT_CERROR ( szContNoServer, szErrorMsg );";
  print "%    }";
  print "%  }";
  print "%#endif\t/* CLIENT_CREATE_BY_USER */";
  if( nArgumentsIn > 1 ) {
    print "%";
    for( i = 1; i <= nArgumentsIn; i++ ) {
      szArgument	= szArgumentsIn[ i ];
      if( ! fnIn( szArgument, szArgumentsOut, nArgumentsOut ) ) {
	print "%  /* Copy in the " szArgument " argument: */";
        szAccessor	= szAccessorsIn[ i ];
        szSize		= szSizesIn[ i ];
        szType		= szTypesIn[ i ];
        if( szType ~ /^string_t/ ) {
          print "%  InArguments." szArgument "\t= " szArgument ";";
          # Code to check if passed pointer is != NULL:
          print "%  if ( " szArgument " == NULL ) {";
	  print "%    /* Error handling for passing a NULL string pointer: */";
          print "%    InArguments." szArgument "\t= (LPSTR) szEmpty;";
	  print "%    sprintf ( szErrorMsg, szFormatNullString,";
          print "%              __procedure__, \"" szArgument "\" );";
	  print "%    RPC_CLIENT_CERROR ( szContNullString, szErrorMsg );";
          print "%  }";
        } else if ( szSize ) {
          print "%  InArguments." szArgument "." szAccessor "_len\t= " \
	    szSize ";";
          print "%  InArguments." szArgument "." szAccessor "_val\t= " \
	    szArgument ";";
          # Code to check if passed pointer is != NULL:
          print "%  if ( InArguments." szArgument "." szAccessor \
	    "_len != 0 && " szArgument " == NULL ) {";
	  print "%    /* Error handling for passing a NULL pointer on */";
	  print "%    /* VECTOR declared in argument: */";
          print "%    InArguments." szArgument "." szAccessor "_len\t= 0;";
	  print "%    sprintf ( szErrorMsg, szFormatNullPassed,";
          print "%              __procedure__, \"" szArgument "\" );";
	  print "%    RPC_CLIENT_CERROR ( szContNullPassed, szErrorMsg );";
          print "%  }";
        } else {
          print "%  InArguments." szArgument "\t= " szArgument ";";
        }
      }
    }
  } else if( nArgumentsIn == 1 && szTypesIn[ 1 ] ~ /^string_t/ ) {
    print "%";
    print "%  if ( " szArgumentsIn[ 1 ] " == NULL ) {";
    print "%    /* Error handling for passing a NULL string pointer: */";
    print "%    " szArgumentsIn[ 1 ] "\t= (LPSTR) szEmpty;";
    print "%    sprintf ( szErrorMsg, szFormatNullString,";
    print "%              __procedure__, \"" szArgumentsIn[ 1 ] "\" );";
    print "%    RPC_CLIENT_CERROR ( szContNullString, szErrorMsg );";
    print "%  }";
  }
  for( i = 1; i <= nArgumentsOut; i++ ) {
    szSize	= szSizesOut[ i ];
    if ( szSize ) {
      szArgument	= szArgumentsOut[ i ];
      print "%  InArguments." szArgument "Size\t= " szSize ";";
    }
  }
  print "%";
  if( szResultType !~ "^void$" ) {
    print "%  nTries = 0;";
    print "%  Result.nErrorLvl = errLvlSuspended;";
    print "%  while ( Result.nErrorLvl == errLvlSuspended ) {";
  }
  print "%    pResult\t=";
  print "%#ifdef MULTITHREAD";
  print "%      (";
  print "%#else";
  print "%      (" szRpcType " *)";
  print "%#endif";
  print "%      fnrpc_" tolower ( szFunctionName ) szFunctionBatch "_" nVersion " (";
  if( nArgumentsIn == 0 ) {
    szClientArgument	= "NULL";
    szClientType	= "void";
  } else if ( nArgumentsIn == 1 ) {
    szClientArgument	= "&" szArgumentsIn[ 1 ];
    szClientType	= szTypesIn[ 1 ];
  } else {
    szClientArgument	= "&InArguments";
    szClientType	= szFunctionName "_args";
  }
  sub( /<[^>]+>/, "", szClientType );
  print "%\t" szClientArgument ",";
  print "%#ifdef MULTITHREAD";
  print "%\t&Result,";
  print "%#endif";
  print "%\t__pClient__ )"
  print "%#ifdef MULTITHREAD";
  print "%\t== RPC_SUCCESS ) ? &Result : NULL";
  print "%#endif";
  print "%\t;";
  print "%";
  print "%    if ( pResult == NULL ) {"
  print "%      /* Error handling for a failed RPC call, */";
  print "%      /* i.e. the server did not respond: */";
  print "%      sprintf ( szErrorMsg, szFormatRpcFailed, __procedure__,";
  print "%                clnt_sperror ( __pClient__, __szHost__ ) );";
  print "%      __pClient__\t= RPC_CLIENT_DESTROY ( __pClient__ );";
  print "%      RPC_CLIENT_CERROR ( szContRpcFailed, szErrorMsg );";
  if( szResultType !~ "^void$" ) {
    print "%      Result\t= ZeroResult;";
  }
  print "%      " szReturn ";";
  print "%    }";
  if( szResultType !~ "^void$" ) {
    print "%#ifndef MULTITHREAD";
    print "%    Result\t= * pResult;";
    print "%#endif";
  }
  if( ! szFunctionBatch ) {
    #  Generate code for copying the out arguments back:
    if( nArgumentsOut ) {
      print "%";
      for( i = 1; i <= nArgumentsOut; i++ ) {
        szArgument	= szArgumentsOut[ i ];
        if( fnIn( szArgument, szArguments, nArguments ) ) {
          print "%    /* Copy out the " szArgument " argument: */";
          szAccessor	= szAccessorsOut[ i ];
          szSize	= szSizesOut[ i ];
          szType	= szTypesOut[ i ];
          if( szDeref ) {
	    szDeref	= szArgument ".";
          }
          # Generate code to check if the output buffer pointer
          # is != NULL:
          print "%  if ( ! ( ";
	  if( szType ~ /^string_t/ ) {
	    print "%         pResult" ( ( szDeref ) ? "->" szArgument : "" );
	    print "%         && *(pResult" \
	      ( ( szDeref ) ? "->" szArgument : "" ) ") &&";
	  } else if ( szSize ) {
	    # Generate code to check if the output buffer size is
	    # big enough:
            print "%         pResult->" szDeref szAccessor "_len" \
	      " != 0 &&";
	  }
          print "%         " szArgument " == NULL ) ) {";
	  if( szType ~ /^string_t/ ) {
	    print "%      if ( " szArgument " != NULL ) {";
	    print "%        if ( pResult" \
	      ( ( szDeref ) ? "->" szArgument : "" ) " != NULL ) {";
	    print "%          strncpy ( " szArgument ", pResult" \
	      ( ( szDeref ) ? "->" szArgument : "" ) ", " szSize " );";
	    print "%        } else {";
	    print "%          *" szArgument "\t= (char) 0;";
	    print "%        }";
	    print "%      }";
	  } else if ( szSize ) {
	    # Generate code to check if the output buffer size is
	    # big enough:
            print "%      memcpy ( " szArgument ",";
            print "%               pResult->" szDeref szAccessor "_val,";
	    print "%               MIN ( pResult->" szDeref szAccessor "_len,";
	    print "%                     " szSize " ) *";
	    print "%               SIZEOF ( *(pResult->" \
	      szDeref szAccessor "_val) ) );";
	    # print "%#ifndef MULTITHREAD";
            # print "%      Result." szDeref szAccessor "_val\t= NULL;";
	    # print "%#endif";
          } else if( szDeref ) {
            print "%      *" szArgument "\t= pResult->" szArgument ";";
          } else {
            print "%      *" szArgument "\t= *pResult;";
	  }
          print "%    }";
        }
      }
      print "%";
    }
    # Check if an error message was returned:
    print "%    if ( pResult && pResult->nErrorLvl ) {";
    print "%      /* Server returned a user error message: */";
    print "%      strncpy ( szErrorMsg,";
    print "%                ( pResult->pszErrorMsg && " \
      "pResult->pszErrorMsg [ 0 ] ) ?";
    print "%                (LPCSTR) pResult->pszErrorMsg : (LPCSTR) szEmpty,";
    print "%                sizeof ( szErrorMsg ) );";
    print "%      szErrorMsg [ sizeof ( szErrorMsg ) - 1 ]\t= (char) 0;";
    print "%      xdr_free ( (xdrproc_t) xdr_" szRpcType ", (char *) pResult );";
    print "%      if ( pResult->nErrorLvl == errLvlSuspended ) { ";
    print "%        if ( nTries == 0 ) {";
    print "%          RPC_CLIENT_SERROR ( errLvlInfo, __procedure__,";
    print "%                              szErrorMsg );";
    print "%        }";
    print "%        nTries++;";
    print "%        sleep ( nSuspendPollingInterval );";
    print "%      } else {";
    print "%        RPC_CLIENT_SERROR ( pResult->nErrorLvl, __procedure__,";
    print "%                            szErrorMsg );";
    print "%      }";
    print "%    } else {";
    print "%      xdr_free ( (xdrproc_t) xdr_" szRpcType ", (char *) pResult );";
    print "%    }";
    print "%  }";
    if( nArgumentsOut ) {
      for( i = 1; i <= nArgumentsOut; i++ ) {
        szArgument	= szArgumentsOut[ i ];
        if( fnIn( szArgument, szArguments, nArguments ) ) {
          szAccessor	= szAccessorsOut[ i ];
          szSize	= szSizesOut[ i ];
          szType	= szTypesOut[ i ];
          if( szDeref ) {
	    szDeref	= szArgument ".";
          }
          # Generate code to check if the output buffer pointer
          # is != NULL:
          print "%  if ( ";
	  if( szType ~ /^string_t/ ) {
	    print "%       Result" ( ( szDeref ) ? "." szArgument : "" );
	    print "%       && *(Result" \
	      ( ( szDeref ) ? "." szArgument : "" ) ") &&";
	  } else if ( szSize ) {
	    # Generate code to check if the output buffer size is
	    # big enough:
            print "%       pResult->" szDeref szAccessor "_len" \
	      " != 0 &&";
	  }
          print "%       " szArgument " == NULL ) {";
          print "%    /* Error handling for passing a NULL pointer on */";
          print "%    /* out argument: */";
          print "%    sprintf ( szErrorMsg, szFormatNullPassed,";
          print "%              __procedure__, \"" szArgument "\" );";
          print "%    RPC_CLIENT_CERROR ( szContNullOutPassed, szErrorMsg );";
	  if( szSize && szType !~ /^string_t/ ) {
	    # Generate code to check if the output buffer size is
	    # big enough:
            print "%  } else if ( pResult->" szDeref szAccessor "_len > " \
	      szSize " ) {";
	    print "%      /* Error handling for an output buffer overflow, */";
	    print "%      /* i.e. the buffer size passed is too small: */";
	    print "%      /* For example, signal a CERROR here ... */";
	    print "%      sprintf ( szErrorMsg, szFormatBufferOverflow,";
            print "%                __procedure__, \"" szArgument "\", ";
            print "%                " szSize ", "
	    print "%                pResult->" szDeref szAccessor "_len );";
            print "%      RPC_CLIENT_CERROR ( szContBufferOverflow, szErrorMsg );";
          }
          print "%  }";
        }
      }
    }
    print "%";
    # Generate code for returning the result:
    if( szResultType !~ "void" ) {
      print "%  " szReturn ";";
      print "%";
    }
  }
  print "%} /* " szFunctionName " */";
  if( szFunctionBatch ) {
    # Generate the batch function stub:
    print "%";
    print "%#ifdef MULTITHREAD";
    print "%enum clnt_stat";
    print "%#else";
    print "%void *";
    print "%#endif";
    print "%fnrpc_" tolower( szFunctionName ) \
      szFunctionBatch "_" nVersion;
    print "%\t( " szPackedArgs " * pArguments,";
    print "%#ifdef MULTITHREAD";
    print "%\t  void * clnt_res,";
    print "%#endif";
    print "%\t  PCLIENT pClient )";
    print "%{";
    print "%#ifdef MULTITHREAD";
    print "%  return clnt_call ( pClient, fnRpc_" \
      szFunctionName szFunctionBatch ",";
    print "%                     (xdrproc_t) xdr_"  szClientType ",";
    print "%                     (caddr_t) pArguments,";
    print "%                     (xdrproc_t) NULL, (caddr_t) NULL,";
    print "%                     NullTimeout );";
    print "%#else";
    print "%  if ( clnt_call ( pClient, fnRpc_" \
      szFunctionName szFunctionBatch ",";
    print "%                   (xdrproc_t) xdr_"  szClientType ",";
    print "%                   (caddr_t) pArguments,";
    print "%                   (xdrproc_t) NULL, (caddr_t) NULL,";
    print "%                   NullTimeout ) != RPC_SUCCESS ) {";
    print "%    return NULL;";
    print "%  }";
    print "%  return (void *) TRUE;";
    print "%#endif";
    print "%} /* fnrpc_" tolower( szFunctionName ) \
      szFunctionBatch "_" nVersion " */";
    print "%#define\t" \
      "fnrpc_" tolower ( szFunctionName ) szFunctionBatch "_" nVersion "\t" \
      "fnrpc_" tolower ( szFunctionName ) "_" nVersion 
  }
  print "#endif\t/* RPC_CLNT */";
  print "%";
} # fnPrintFunction

/^%/ {
  GlobalLine[ nGlobalLineCounter++ ]	= $0;
  next;
}

/@const@/ {
  GlobalConst[ nGlobalConstCounter ]			= \
    fnScanSimpleBlock( "@const@" );
  nGlobalConstCounter++;
}
/@typedef@/ {
  szTypedefBlock					= \
    fnScanSimpleBlock( "@typedef@" );
  if( szTypedefBlock ~ /</ ) {
    GlobalTypedefArgs[ nGlobalTypedefArgsCounter ]	= szTypedefBlock;
    nGlobalTypedefArgsCounter++;
  } else {
    GlobalTypedef[ nGlobalTypedefCounter ]		= szTypedefBlock;
    nGlobalTypedefCounter++;
  }
}
/@enum@/ {
  GlobalEnum[ nGlobalEnumCounter ]			= \
    fnScanSimpleBlock( "@enum@" );
  nGlobalEnumCounter++;
}
/@args@/ {
  szArgsBlock						= \
    fnScanArgsBlock( "@args@" );
  if( szArgsBlock ) {
    GlobalTypedefArgs[ nGlobalTypedefArgsCounter ]	= szArgsBlock;
    nGlobalTypedefArgsCounter++;
  }
}
/@function@/ {
  GlobalFunction[ nGlobalFunctionCounter ]		= \
    fnScanFunctionBlock( "@function@" );
  nGlobalFunctionCounter++;
}
END {
  print "%";
  print "%/* ------------------------------------------------------------------------";
  print "%| " szOutputFile " generated by " szScript " on " szDate;
  print "%| " szScript " (C) 1996/09/19 Heiko Kirschke kirschke@informatik.uni-hamburg.de";
  print "% ----------------------------------------------------------------------- */"
  print "%";
  for( i = 0; i < nGlobalLineCounter; i++ ) {
    print GlobalLine[ i ];
  }
  print "#if RPC_HDR || RPC_XDR";
  print "%";
  print "%#ifndef RPCNOTYPES";
  print "%";
  for( i = 0; i < nGlobalConstCounter; i++ ) {
    print GlobalConst[ i ];
  }
  for( i = 0; i < nGlobalEnumCounter; i++ ) {
    gsub( /[	 ]*,[	 ]*/, ",\n\t", GlobalEnum[ i ] );
    print GlobalEnum[ i ];
  }
  for( i = 0; i < nGlobalTypedefCounter; i++ ) {
    print GlobalTypedef[ i ];
  }
  print "%";
  print "%#endif\t/* ! RPCNOTYPES */";
  print "#endif\t/* RPC_HDR || RPC_XDR */";
  print "%";
  for( i = 0; i < nGlobalTypedefArgsCounter; i++ ) {
    print GlobalTypedefArgs[ i ];
  }
  print "#if RPC_HDR";
  print "%";
  print "%typedef long\tRPCPORT;";
  print "%extern RPCPORT DLLEXPORT\tfn" szCapitalized \
	"GetPortOffset\t( void );";
  print "%extern RPCPORT DLLEXPORT\tfn" szCapitalized \
	"GetPort\t( void );";
  print "%extern RPCPORT DLLEXPORT\tfn" szCapitalized \
	"SetPort\t( RPCPORT\tlPortNew );";
  print "%";
  print "%typedef u_long\tRPCVERSION;";
  print "%extern RPCVERSION DLLEXPORT\tfn" szCapitalized \
	"GetVersion\t( void );";
  print "%extern RPCVERSION DLLEXPORT\tfn" szCapitalized \
	"SetVersion\t( RPCVERSION\tlVersionNew );";
  print "%";
  print "%extern struct svc_req *\tfnServer" szCapitalized \
    "Request\t( void );";
  print "%extern struct svc_req *\tfnServer" szCapitalized "Reply";
  print "%\t( void (*pfnCalled)(), void * pReturnValue );";
  print "%";
  print "%extern PCLIENT DLLEXPORT fnClient" szCapitalized "Create";
  print "%\t( LPCSTR pszHost, LPCSTR pszTransport );";
  print "%extern PCLIENT DLLEXPORT fnClient" szCapitalized "\t( void );";
  print "%extern LPCSTR DLLEXPORT fnClient" szCapitalized "Host\t( void );";
  print "%extern LPCSTR DLLEXPORT fnClient" szCapitalized "Transport\t( void );";
  print "%extern bool_t DLLEXPORT fnClient" szCapitalized "Flush\t( PCLIENT pClient );";
  print "%extern PCLIENT DLLEXPORT fnClient" szCapitalized "Destroy\t( PCLIENT pClient );";
  print "%";
  print "#elif ! RPC_XDR";
  print "%";
  print "%#define\tSIZEOF(type)((sizeof(type)<1)?1:sizeof(type))";
  print "%";
  print "%static RPCVERSION __Version__ = " nVersion ";";
  print "%";
  print "%RPCVERSION DLLEXPORT\tfn" szCapitalized \
	"GetVersion\t( void )";
  print "%{";
  print "%  return __Version__;";
  print "%} /* fn" szCapitalized "GetVersion */";
  print "%";
  print "%RPCVERSION DLLEXPORT\tfn" szCapitalized \
	"SetVersion\t( RPCVERSION\tlVersionNew )";
  print "%{";
  print "%  RPCVERSION\tlVersionOld\t= __Version__;";
  print "%  __Version__\t= lVersionNew;";
  print "%  return lVersionOld;";
  print "%} /* fn" szCapitalized "SetVersion */";
  print "%";
  print "%static const RPCPORT __PortOffset__ = " nProgram ";";
  print "%static RPCPORT __Port__ = 0;";
  print "%";
  print "%RPCPORT DLLEXPORT\tfn" szCapitalized \
	"GetPortOffset\t( void )";
  print "%{";
  print "%  return __PortOffset__;";
  print "%} /* fn" szCapitalized "GetPortOffset */";
  print "%";
  print "%RPCPORT DLLEXPORT\tfn" szCapitalized \
	"GetPort\t( void )";
  print "%{";
  print "%  return __Port__;";
  print "%} /* fn" szCapitalized "GetPort */";
  print "%";
  print "%RPCPORT DLLEXPORT\tfn" szCapitalized \
	"SetPort\t( RPCPORT\tlPortNew )";
  print "%{";
  print "%  RPCPORT\tlPortOld\t= __Port__;";
  print "%  __Port__\t= lPortNew;";
  print "%  return lPortOld;";
  print "%} /* fn" szCapitalized "SetPort */";
  print "%";
  print "#endif\t/* RPC_XDR */";
  print "#if RPC_CLNT || RPC_SVC";
  print "%#ifdef " szProgram;
  print "%#undef " szProgram;
  print "%#endif /* " szProgram " */";
  print "%#define " szProgram "\t((u_long)fn" szCapitalized \
	"GetPortOffset()+(u_long)fn" szCapitalized "GetPort())";
  print "%";
  print "%#ifdef " szProgram "VERS";
  print "%#undef " szProgram "VERS";
  print "%#endif /* " szProgram "VERS */";
  print "%#define " szProgram "VERS\t((u_long)fn" szCapitalized \
	"GetVersion())";
  print "%";
  print "% /* static char\t\tszEmpty []\t= \"\"; */";
  print "#endif /* RPC_CLNT || RPC_SVC */";
  print "%";
  print "#if RPC_SVC";
  print "%";
  print "%#include\t<stdlib.h>";
  print "%#include\t<rpc/svc.h>";
  print "%#if WIN32";
  print "%#include\t<rpc/pmap_cln.h>";
  print "%#else";
  print "%#include\t<rpc/pmap_clnt.h>";
  print "%#endif";
  print "%";
  print "%static struct svc_req\t* __pRequest__\t= NULL;";
  print "%";
  print "%static struct {";
  print "%  bool_t\tbAnswered;";
  print "%  void\t\t(*pfnCalled)();";
  print "%  int\t\t*pnErrorLvl;";
  print "%  LPCSTR\t*ppszErrorMsg;";
  print "%  void\t\t*pResult;";
  print "%  void\t\t*pReturnValue;";
  print "%  size_t\tnReturnValueSize;";
  print "%  bool_t\t(*pfnXdrResult)();";
  print "%}\tReply;";
  print "%";
  print "%static const char\tszFormatMallocFailed []\t=";
  print "%\"%s: Allocating a result buffer of size %d for argument %s failed.\";";
  print "%";
  print "%#ifndef\tRPC_SERVER_INITIALIZE";
  print "%#define\tRPC_SERVER_INITIALIZE(argc,argv)";
  print "%#endif\t/* ! RPC_SERVER_INITIALIZE */";
  print "%";
  print "%#ifndef\tRPC_SERVER_ERROR";
  print "%#define\tRPC_SERVER_ERROR(msg)" \
    "(fprintf(stderr,\"Error: %s\\n\",msg),exit(1),0)";
  print "%#endif\t/* ! RPC_SERVER_ERROR */";
  print "%";
  print "%#ifndef\tRPC_SERVER_CERROR";
  print "%#define\tRPC_SERVER_CERROR(cont,msg)" \
   "(fprintf(stderr,\"Continuable error: %s\\n%s\\n\",msg,cont),fflush(stdin),fgetc(stdin),0)";
  print "%#endif\t/* ! RPC_SERVER_CERROR */";
  print "%";
  print "%#ifndef\tRPC_SERVER_ERRLVL";
  print "%#define\tRPC_SERVER_ERRLVL()\t0";
  print "%#endif\t/* ! RPC_SERVER_ERRLVL */";
  print "%";
  print "%#ifndef\tRPC_SERVER_ERRMSG";
  print "%#define\tRPC_SERVER_ERRMSG()\tNULL";
  print "%#endif\t/* ! RPC_SERVER_ERRMSG */";
  print "%";
  print "%#ifndef\tRPC_SERVER_ENTRY";
  print "%#define\tRPC_SERVER_ENTRY()";
  print "%#endif\t/* ! RPC_SERVER_ENTRY */";
  print "%";
  print "#endif\t/* RPC_SVC */";
  print "#if RPC_CLNT";
  print "%";
  print "%static bool_t\t__bClientInitialize__\t= TRUE;";
  print "%static char\t__szHost__ [ 256 ]\t= \"\";";
  print "%static char\t__szTransport__ [ 256 ]\t= \"\";";
  print "%static PCLIENT\t__pClient__\t= NULL;";
  print "%";
  print "%static const char\tszFormatNullPassed []\t=";
  print "%\"%s: A NULL pointer was passed for the %s argument.\";";
  print "%static const char\tszContNullPassed []\t=";
  print "%\"Set associated buffer size to 0.\";";
  print "%";
  print "%static const char\tszFormatNullString []\t=";
  print "%\"%s: A NULL string was passed for the %s argument.\";";
  print "%static const char\tszContNullString []\t=";
  print "%\"Handle as empty string of length 0.\";";
  print "%";
  print "%static const char\tszContNullOutPassed []\t=";
  print "%\"Dont copy back the values returned from the server.\";";
  print "%";
  print "%static const char\tszFormatNoServer []\t=";
  print "%\"%s: No connection to server established.\";";
  print "%static const char\tszContNoServer []\t=";
  print "%\"(Re)try to connect to a server.\";";
  print "%";
  print "%static const char\tszFormatRpcFailed []\t=";
  print "%\"%s: RPC call failed:\\n       %s\";";
  print "%static const char\tszContRpcFailed []\t=";
  print "%\"Return from client procedure.\";";
  print "%";
  print "%static const char\tszFormatBufferOverflow []\t=";
  print "%\"%s: Output buffer size for argument %s is %d, server returned a buffer size of %d.\";";
  print "%static const char\tszContBufferOverflow []\t=";
  print "%\"Ignore the additional values returned from server.\";";
  print "%";
  print "%static struct timeval\tNullTimeout\t= {  0, 0 };";
  print "%static struct timeval\tDefaultTimeout\t= { 20, 0 };";
  print "%";
  print "%static int nSuspendPollingInterval\t= 10; /* seconds */";
  print "%";
  print "%#ifndef\tRPC_CLIENT_CREATE";
  print "%#define\tRPC_CLIENT_CREATE(host,transport)\tfnClient" szCapitalized "Create ( host, transport )";
  print "%#define\tCLIENT_CREATE_BY_USER\t0";
  print "%#else";
  print "%#define\tCLIENT_CREATE_BY_USER\t1";
  print "%#endif\t/* ! RPC_CLIENT_CREATE */";
  print "%";
  print "%#ifndef\tRPC_CLIENT_DESTROY";
  print "%#define\tRPC_CLIENT_DESTROY(pclient)\tfnClient" szCapitalized "Destroy ( pclient )";
  print "%#endif\t/* ! RPC_CLIENT_DESTROY */";
  print "%";
  print "%#ifndef\tRPC_CLIENT_INITIALIZE";
  print "%#define\tRPC_CLIENT_INITIALIZE()";
  print "%#endif\t/* ! RPC_CLIENT_INITIALIZE */";
  print "%";
  print "%#ifndef\tRPC_CLIENT_ERROR";
  print "%#define\tRPC_CLIENT_ERROR(msg)\\";
  print "%\t{fprintf(stderr,\"Error: %s\\n\",msg);clnt_perror(__pClient__,__szHost__);exit(1);}";
  print "%#endif\t/* ! RPC_CLIENT_ERROR */";
  print "%";
  print "%#ifndef\tRPC_CLIENT_CERROR";
  print "%static const char\tszFormatCError []\t=";
  print "%\"Continuable error: %s\\n%s\\n\";";
  print "%#define\tRPC_CLIENT_CERROR(cont,msg)\tfprintf(stderr,szFormatCError,msg,cont)";
  print "%#endif\t/* ! RPC_CLIENT_CERROR */";
  print "%";
  print "%#ifndef\tRPC_CLIENT_SERROR";
  print "%static const char\tszFormatSError []\t=";
  print "%\"%s: Server error, level %d: %s\\n\";";
  print "%#define\tRPC_CLIENT_SERROR(lvl,proc,msg)\tfprintf(stderr,szFormatSError,proc,lvl,msg)";
  print "%#endif\t/* ! RPC_CLIENT_SERROR */";
  print "%";
  print "#endif\t/* RPC_CLNT */";
  if( nGlobalFunctionCounter > 1 ) {
    print "program " szProgram " {";
    print "  version " szProgram "VERS {";
    # Generate the module body for the client code:
    print "%";
    print "#if RPC_HDR";
    print "%";
    print "%/* 1996/09/19 HK kirschke@informatik.uni-hamburg.de\t\t*/";
    print "%/* Following #define bypasses a declaration error in\t\t*/";
    print "%/* /usr/include/rpc/xdr.h; the declaration found there is:\t*/";
    print "%/*\textern bool_t xdr_bool(XDR *, bool_t *);\t\t*/";
    print "%/* but should be:\t\t\t\t\t\t*/";
    print "%/*\textern bool_t xdr_bool_t(XDR *, bool_t *);\t\t*/";
    print "%";
    print "%#define xdr_bool_t xdr_bool";
    print "%";
    print "#endif\t/* RPC_HDR */";
    print "#if RPC_SVC";
    print "%";
    print "%extern struct svc_req *\tfnServer" szCapitalized \
      "Request\t( void )";
    print "%{";
    print "%  return __pRequest__;";
    print "%} /* fnServer" szCapitalized "Request */";
    print "%";
    print "%extern struct svc_req *\tfnServer" szCapitalized "Reply";
    print "%\t( void (*pfnCalled)(), void * pReturnValue )";
    print "%{";
    print "%  struct svc_req\t*pRequest = (struct svc_req *) NULL;";
    print "%  LPCSTR\t\tpszErrorMsg;";
    print "%";
    print "%  if ( ! Reply.bAnswered && pfnCalled == Reply.pfnCalled ) {";
    print "%    Reply.bAnswered\t= TRUE;";
    print "%    if ( Reply.pReturnValue != NULL ) {";
    print "%      if ( pReturnValue ) {";
    print "%        memcpy ( Reply.pReturnValue, pReturnValue," \
      " Reply.nReturnValueSize );";
    print "%      } else {";
    print "%        memset ( Reply.pReturnValue, 0, Reply.nReturnValueSize );";
    print "%      }";
    print "%    }";
    print "%    if ( Reply.pnErrorLvl ) {";
    print "%      *Reply.pnErrorLvl\t= RPC_SERVER_ERRLVL();";
    print "%      if ( *Reply.pnErrorLvl && Reply.ppszErrorMsg != NULL ) {";
    print "%        pszErrorMsg\t= RPC_SERVER_ERRMSG();";
    print "%        if ( pszErrorMsg && *pszErrorMsg ) {";
    print "%          *Reply.ppszErrorMsg\t= strdup ( pszErrorMsg );";
    print "%        }";
    print "%      }";
    print "%    }";
    print "%    if ( Reply.ppszErrorMsg != NULL &&";
    print "%         *Reply.ppszErrorMsg == NULL ) {";
    print "%      *Reply.ppszErrorMsg\t= szEmpty;";
    print "%    }";
    print "%    pRequest\t= fnServer" szCapitalized "Request ();";
    print "%    if ( ! svc_sendreply ( pRequest->rq_xprt,";
    print "%                           (xdrproc_t) Reply.pfnXdrResult,";
    print "%                           (caddr_t) Reply.pResult ) ) {";
    print "%      svcerr_systemerr ( pRequest->rq_xprt );";
    print "%    }";
    print "%  }";
    print "%  return pRequest;"
    print "%} /* fnServer" szCapitalized "Reply */";
    print "%";
    print "#endif\t/* RPC_SVC */";
    print "#if RPC_CLNT";
    print "%";
    print "%PCLIENT DLLEXPORT fnClient" szCapitalized "Create";
    print "%\t( LPCSTR pszHost, LPCSTR pszTransport )";
    print "%{";
    print "%";
    print "%  fnClient" szCapitalized "Destroy ( __pClient__ );";
    print "%  strncpy ( __szHost__, pszHost, sizeof ( __szHost__ ) );";
    print "%  strncpy ( __szTransport__, pszTransport,";
    print "%            sizeof ( __szTransport__ ) );";
    print "%  __pClient__    = clnt_create ( pszHost, " szProgram ",";
    print "%                                    " \
      szProgram "VERS, pszTransport );";
    print "%  return __pClient__;";
    print "%} /* fnClient" szCapitalized "Create */";
    print "%";
    print "%PCLIENT DLLEXPORT fnClient" szCapitalized "\t( void )";
    print "%{";
    print "%  return __pClient__;";
    print "%} /* fnClient" szCapitalized " */";
    print "%";
    print "%LPCSTR DLLEXPORT fnClient" szCapitalized "Host\t( void )";
    print "%{";
    print "%  return __szHost__;";
    print "%} /* fnClient" szCapitalized "Host */";
    print "%";
    print "%LPCSTR DLLEXPORT fnClient" szCapitalized "Transport\t( void )";
    print "%{";
    print "%  return __szTransport__;";
    print "%} /* fnClient" szCapitalized "Transport */";
    print "%";
    print "%bool_t DLLEXPORT fnClient" szCapitalized "Flush\t( PCLIENT pClient )";
    print "%{";
    print "%  enum clnt_stat\tnState;";
    print "%";
    print "%  if ( pClient == NULL ) {";
    print "%    pClient\t= __pClient__;";
    print "%  }";
    print "%  if ( pClient != NULL ) {";
    print "%    nState = clnt_call ( pClient, NULLPROC,";
    print "%                         (xdrproc_t) xdr_void, (caddr_t) NULL,";
    print "%                         (xdrproc_t) xdr_void, (caddr_t) NULL,";
    print "%                         DefaultTimeout );";
    print "%    return ( nState == RPC_SUCCESS );";
    print "%  }";
    print "%  return FALSE;";
    print "%} /* fnClient" szCapitalized "Flush */";
    print "%";
    print "%PCLIENT DLLEXPORT fnClient" szCapitalized "Destroy\t( PCLIENT pClient )";
    print "%{";
    print "%  if ( pClient == NULL ) {";
    print "%    pClient\t= __pClient__;";
    print "%  }";
    print "%  if ( pClient != NULL ) {";
    print "%    auth_destroy ( pClient->cl_auth );";
    print "%    clnt_destroy ( pClient );";
    print "%  }";
    print "%  if ( __pClient__ == pClient ) {";
    print "%    __pClient__\t= NULL;";
    print "%  }";
    print "%  pClient\t= NULL;";
    print "%  return pClient;";
    print "%} /* fnClient" szCapitalized "Destroy */";
    print "%";
    print "#endif\t/* RPC_CLNT */";
    print "%";
    for( i = 1; i < nGlobalFunctionCounter; i++ ) {
      fnPrintFunction( GlobalFunction[ i ] );
    }
    print "  } = " nVersion ";";
    print "} = " nProgram ";";
  }
  print "#ifdef RPC_SVC";
  print "%int fnMain ();";
  print "%int main (int argc, char * argv [] )";
  print "%{";
  print "%  static const char\t__procedure__[]\t = \"main\";";
  print "%";
  print "%  RPC_SERVER_INITIALIZE ( argc, argv );";
  print "%";
  print "%  fnMain ();";
  print "%";
  print "%  return 0;";
  print "%} /* main */";
  print "%#define main fnMain";
  print "#endif\t/* RPC_SVC */";
  print "%";
  print "%/* ------------------------------------------------------------------------";
  print "%| End of " szOutputFile;
  print "% ----------------------------------------------------------------------- */"
}

# Local variables:
# mode: awk
# buffer-file-coding-system: raw-text-unix
# End:
