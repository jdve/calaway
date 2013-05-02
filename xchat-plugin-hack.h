#ifndef XCHAT_PLUGIN_HACK_H
#define XCHAT_PLUGIN_HACK_H

#undef WIN32
#include "xchat-plugin.h"

/* The xchat_emit_print function is a vararg.
   It was also the case for xchat_printf and xchat_commandf,
   but they were not necessary.
   While browsing the code, it occured that this version
   of XChat doesn't use more than 4 args
   (it is explicitely used in the implementation of xchat_emit_print),
   so to ease my Haskell binding, I provide the 5 versions.
*/

int xchat_emit_print0 (
	xchat_plugin *ph,
	const char *event_name
);
int xchat_emit_print1 (
	xchat_plugin *ph,
	const char *event_name,
	const char *arg0
);
int xchat_emit_print2 (
	xchat_plugin *ph,
	const char *event_name,
	const char *arg0,
	const char *arg1
);
int xchat_emit_print3 (
	xchat_plugin *ph,
	const char *event_name,
	const char *arg0,
	const char *arg1,
	const char *arg2
);
int xchat_emit_print4 (
	xchat_plugin *ph,
	const char *event_name,
	const char *arg0,
	const char *arg1,
	const char *arg2,
	const char *arg3
);

char* strListChan     ;
char* strListDcc      ;
char* strListIgn      ;
char* strListNotify   ;
char* strListUser     ;
char* strAddress      ;
char* strCps          ;
char* strDestfile     ;
char* strFile         ;
char* strNick         ;
char* strPort         ;
char* strPos          ;
char* strResume       ;
char* strSize         ;
char* strSizehigh     ;
char* strStatus       ;
char* strChannel      ;
char* strChantypes    ;
char* strContext      ;
char* strFlags        ;
char* strId           ;
char* strLag          ;
char* strMaxmodes     ;
char* strNetwork      ;
char* strNickprefixes ;
char* strNickmodes    ;
char* strQueue        ;
char* strServer       ;
char* strType         ;
char* strUsers        ;
char* strMask         ;
char* strAway         ;
char* strLasttalk     ;
char* strHost         ;
char* strPrefix       ;
char* strRealname     ;
char* strSelected     ;
char* strNetworks     ;
char* strOn           ;
char* strOff          ;
char* strSeen         ;
#endif
