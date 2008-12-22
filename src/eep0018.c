/* eep0018.c */

#include <stdio.h>
#include <erl_driver.h>

int json_to_term(char* inBuf, char** retBuf, int retBufLen);
int term_to_json(char* inBuf, char** retBuf, int retBufLen);

typedef struct {
    ErlDrvPort port;
} eep0018_data;

static ErlDrvData eep0018_drv_start(ErlDrvPort port, char *buff)
{
    eep0018_data* d = (eep0018_data*)driver_alloc(sizeof(eep0018_data));
    d->port = port;
    return (ErlDrvData)d;
}

static void eep0018_drv_stop(ErlDrvData handle)
{
    driver_free((char*)handle);
}

static int eep0018_drv_control(ErlDrvData handle, unsigned int command, char* inBuf,
        int inBufLen, char **retBuf, int retBufLen)
{
    //eep0018_data* pData = (eep0018_data*) handle;
    switch(command) {
    case 0: //json_to_term
        return json_to_term(inBuf, retBuf, retBufLen);
    case 1: //term_to_json
        return term_to_json(inBuf, retBuf, retBufLen);
    default:
        return -1;
    }
}

ErlDrvEntry eep0018_driver_entry = {
    NULL,                   /* F_PTR init, N/A */
    eep0018_drv_start,      /* L_PTR start, called when port is opened */
    eep0018_drv_stop,       /* F_PTR stop, called when port is closed */
    NULL,                   /* F_PTR output, called when erlang has sent */
    NULL,                   /* F_PTR ready_input, called when input descriptor ready */
    NULL,                   /* F_PTR ready_output, called when output descriptor ready */
    "eep0018_drv",          /* char *driver_name, the argument to open_port */
    NULL,                   /* F_PTR finish, called when unloaded */
    NULL,                   /* Not used */
    eep0018_drv_control,    /* F_PTR control, port_command callback */
    NULL,                   /* F_PTR timeout, reserved */
    NULL,                   /* F_PTR outputv, reserved */
    NULL,                   /* F_PTR ready_async */
    NULL,                   /* F_PTR flush */
    NULL,                   /* F_PTR call */
    NULL,                   /* F_PTR event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL,                   /* Reserved -- Used by emulator internally */
    NULL,                   /* F_PTR process_exit */
};

DRIVER_INIT(eep0018_drv)    /* must match name in driver_entry */
{
    return &eep0018_driver_entry;
}
