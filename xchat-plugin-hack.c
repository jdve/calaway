#include "xchat-plugin-hack.h"

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
) { return ph->xchat_emit_print(ph,event_name,NULL); }
int xchat_emit_print1 (
	xchat_plugin *ph,
	const char *event_name,
    const char *arg0
) { return ph->xchat_emit_print(ph,event_name,arg0,NULL); }
int xchat_emit_print2 (
	xchat_plugin *ph,
	const char *event_name,
    const char *arg0,
    const char *arg1
) { return ph->xchat_emit_print(ph,event_name,arg0,arg1,NULL); }
int xchat_emit_print3 (
	xchat_plugin *ph,
	const char *event_name,
    const char *arg0,
    const char *arg1,
    const char *arg2
) { return ph->xchat_emit_print(ph,event_name,arg0,arg1,arg2,NULL); }
int xchat_emit_print4 (
	xchat_plugin *ph,
	const char *event_name,
    const char *arg0,
    const char *arg1,
    const char *arg2,
    const char *arg3
) { return ph->xchat_emit_print(ph,event_name,arg0,arg1,arg2,arg3,NULL); }

/* There is a cast in the interface to get a context from a pointer.
   To keep well typed, we do this cast explicitely here.
*/
const xchat_context * xchat_list_context (
	xchat_plugin *ph,
	xchat_list *xlist,
	const char *name
) { return (const xchat_context *) xchat_list_str(ph, xlist, name); }

xchat_hook *xchat_hook_command (xchat_plugin *ph,
		    const char *name,
		    int pri,
		    int (*callback) (char *word[], char *word_eol[], void *user_data),
		    const char *help_text,
		    void *userdata)
{
    return ph->xchat_hook_command(ph, name, pri, callback, help_text, userdata);
}

xchat_hook *xchat_hook_server (xchat_plugin *ph,
		   const char *name,
		   int pri,
		   int (*callback) (char *word[], char *word_eol[], void *user_data),
		   void *userdata)
{
    return ph->xchat_hook_server(ph, name, pri, callback, userdata);
}

xchat_hook *xchat_hook_print (xchat_plugin *ph,
		  const char *name,
		  int pri,
		  int (*callback) (char *word[], void *user_data),
		  void *userdata)
{
    return ph->xchat_hook_print(ph, name, pri, callback, userdata);
}

xchat_hook *xchat_hook_timer (xchat_plugin *ph,
		  int timeout,
		  int (*callback) (void *user_data),
		  void *userdata)
{
    return ph->xchat_hook_timer(ph, timeout, callback, userdata);
}

xchat_hook *xchat_hook_fd (xchat_plugin *ph,
		int fd,
		int flags,
		int (*callback) (int fd, int flags, void *user_data),
		void *userdata)
{
    return ph->xchat_hook_fd(ph, fd, flags, callback, userdata);
}

void *xchat_unhook (xchat_plugin *ph,
	      xchat_hook *hook)
{
    ph->xchat_unhook(ph, hook);
}

void xchat_print (xchat_plugin *ph,
	     const char *text)
{
    ph->xchat_print(ph, text);
}

void xchat_command (xchat_plugin *ph,
	       const char *command)
{
    ph->xchat_command(ph, command);
}

int xchat_nickcmp (xchat_plugin *ph,
	       const char *s1,
	       const char *s2)
{
    return ph->xchat_nickcmp(ph, s1, s2);
}

int xchat_set_context (xchat_plugin *ph,
		   xchat_context *ctx)
{
    return ph->xchat_set_context(ph, ctx);
}

xchat_context *xchat_find_context (xchat_plugin *ph,
		    const char *servname,
		    const char *channel)
{
    return ph->xchat_find_context(ph, servname, channel);
}

xchat_context *xchat_get_context (xchat_plugin *ph)
{
    return ph->xchat_get_context(ph);
}

const char *xchat_get_info (xchat_plugin *ph,
		const char *id)
{
    return ph->xchat_get_info(ph, id);
}

int xchat_get_prefs (xchat_plugin *ph,
		 const char *name,
		 const char **string,
		 int *integer)
{
    return ph->xchat_get_prefs(ph, name, string, integer);
}

xchat_list *xchat_list_get (xchat_plugin *ph,
		const char *name)
{
    return ph->xchat_list_get(ph, name);
}

void xchat_list_free (xchat_plugin *ph,
		 xchat_list *xlist)
{
    return ph->xchat_list_free(ph, xlist);
}

const char * const *xchat_list_fields (xchat_plugin *ph,
		   const char *name)
{
    return ph->xchat_list_fields(ph, name);
}

int xchat_list_next (xchat_plugin *ph,
		 xchat_list *xlist)
{
    return ph->xchat_list_next(ph, xlist);
}

const char *xchat_list_str (xchat_plugin *ph,
		xchat_list *xlist,
		const char *name)
{
    return ph->xchat_list_str(ph, xlist, name);
}

int xchat_list_int (xchat_plugin *ph,
		xchat_list *xlist,
		const char *name)
{
    return ph->xchat_list_int(ph, xlist, name);
}

time_t xchat_list_time (xchat_plugin *ph,
		 xchat_list *xlist,
		 const char *name)
{
    return ph->xchat_list_time(ph, xlist, name);
}

void *xchat_plugingui_add (xchat_plugin *ph,
		     const char *filename,
		     const char *name,
		     const char *desc,
		     const char *version,
		     char *reserved)
{
    ph->xchat_plugingui_add(ph, filename, name, desc, version, reserved);
}

void xchat_plugingui_remove (xchat_plugin *ph,
			void *handle)
{
    ph->xchat_plugingui_remove(ph, handle);
}

char *xchat_gettext (xchat_plugin *ph,
	       const char *msgid)
{
    return ph->xchat_gettext(ph, msgid);
}

void xchat_send_modes (xchat_plugin *ph,
		  const char **targets,
		  int ntargets,
		  int modes_per_line,
		  char sign,
		  char mode)
{
    ph->xchat_send_modes(ph, targets, ntargets, modes_per_line, sign, mode);
}

char *xchat_strip (xchat_plugin *ph,
	     const char *str,
	     int len,
	     int flags)
{
    return ph->xchat_strip(ph, str, len, flags);
}

void xchat_free (xchat_plugin *ph,
	    void *ptr)
{
    ph->xchat_free(ph, ptr);
}

char* strListChan     = "channels"     ;
char* strListDcc      = "dcc"          ;
char* strListIgn      = "ignore"       ;
char* strListNotify   = "notify"       ;
char* strListUser     = "users"        ;
char* strAddress      = "address32"    ;
char* strCps          = "cps"          ;
char* strDestfile     = "destfile"     ;
char* strFile         = "file"         ;
char* strNick         = "nick"         ;
char* strPort         = "port"         ;
char* strPos          = "pos"          ;
char* strResume       = "iresume"      ;
char* strSize         = "isize"        ;
char* strSizehigh     = "isizehigh"    ;
char* strStatus       = "istatus"      ;
char* strChannel      = "channel"      ;
char* strChantypes    = "chantypes"    ;
char* strContext      = "context"      ;
char* strFlags        = "flags"        ;
char* strId           = "id"           ;
char* strLag          = "lag"          ;
char* strMaxmodes     = "maxmodes"     ;
char* strNetwork      = "network"      ;
char* strNickprefixes = "nickprefixes" ;
char* strNickmodes    = "nickmodes"    ;
char* strQueue        = "queue"        ;
char* strServer       = "server"       ;
char* strType         = "type"         ;
char* strUsers        = "users"        ;
char* strMask         = "mask"         ;
char* strAway         = "away"         ;
char* strLasttalk     = "lasttalk"     ;
char* strHost         = "host"         ;
char* strPrefix       = "prefix"       ;
char* strRealname     = "realname"     ;
char* strSelected     = "seleceted"    ;
char* strNetworks     = "networks"     ;
char* strOn           = "on"           ;
char* strOff          = "off"          ;
char* strSeen         = "seen"         ;
