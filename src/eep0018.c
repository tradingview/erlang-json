/* eep0018.c */

#include "eep0018.h"

static void
eep0018_error(ErlDrvPort port, const char* err)
{
    ei_x_buff b;
    ei_x_new_with_version(&b);
    ei_x_encode_tuple_header(&b, 2);
    ei_x_encode_atom(&b, "error");
    ei_x_encode_atom(&b, (const char*) err);
    driver_output(port, b.buff, b.index);
    ei_x_free(&b);
}

static ErlDrvData
eep0018_start(ErlDrvPort port, char *buff)
{
    if(port == NULL) return ERL_DRV_ERROR_GENERAL;
    return (ErlDrvData) port;
}

static void
eep0018_free(void* ctx)
{
    eep0018_data* state = (eep0018_data*) ctx;

    if(state->direction == 1)
    {
        ei_x_free(&state->buf);
        yajl_free(state->phandle);
    }
    else if(state->direction == 2)
    {
        yajl_gen_free(state->ghandle);
    }

    if(state->bin != NULL) driver_free_binary(state->bin);
    driver_free(state);
}

static int
eep0018_control(ErlDrvData drv_data, unsigned int command, char* buf, int len, char **rbuf, int rlen)
{
    const unsigned char* json;
    unsigned int jlen;

    if(command < 1 || command > 2)
    {
        //eep0018_error(port, "invalid_command");
        return -1;
    }

    eep0018_data* ctx = (eep0018_data*) driver_alloc(sizeof(eep0018_data));
    if(ctx == NULL)
    {
        //eep0018_error(port, "no_memmory");
        return -1;
    }

    ctx->direction = command;
    ctx->result = -1;
   
    ctx->bin = driver_alloc_binary(len * sizeof(char));
    if(ctx->bin == NULL)
    {
        driver_free(ctx);
        //eep0018_error(port, "no_memmory");
        return -1;
    }
    memcpy(ctx->bin->orig_bytes, buf, len);
    
    if(ctx->direction == 1)
    {
        json_to_term(ctx);
    }
    else
    {
        term_to_json(ctx);
    }
    
    if(ctx->result != OK)
    {
        //eep0018_error(port, "json_error");
        eep0018_free(ctx);
        return -1;
    }

    if(ctx->direction == 1)
    {
        *rbuf = driver_alloc(ctx->buf.index);
        memcpy(*rbuf, ctx->buf.buff, ctx->buf.index);
        eep0018_free(ctx);
        return ctx->buf.index;
    }
    else
    {
        if(yajl_gen_get_buf(ctx->ghandle, &json, &jlen))
        {
            //eep0018_error(port, "gen_buf_error");
            eep0018_free(ctx);
            return -1;
        }
        *rbuf = driver_alloc(jlen);
        memcpy(*rbuf, json, jlen);
        eep0018_free(ctx);
        return jlen;
    }
}

static ErlDrvEntry
eep0018_driver_entry =
{
    NULL,               /* Init */
    eep0018_start,
    NULL,               /* Stop */
    NULL,               /* Output */
    NULL,               /* Input Ready */
    NULL,               /* Output Ready */
    "eep0018_drv",      /* Driver Name */
    NULL,               /* Finish */
    NULL,               /* Handle */
    eep0018_control,
    NULL,               /* Timeout */
    NULL,               /* Outputv */
    NULL,               /* Ready Async */
    NULL,               /* Flush */
    NULL,               /* Call */
    NULL,               /* Event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL,               /* Reserved */
    NULL,               /* Process Exit */
};

DRIVER_INIT(eep0018_drv)    /* must match name in driver_entry */
{
    return &eep0018_driver_entry;
}
