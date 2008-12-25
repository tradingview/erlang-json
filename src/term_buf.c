
#include "eep0018.h"
#include "term_buf.h"

#define INIT_DBL_STORE_SIZE 16
#define INIT_TERM_BUF_SIZE 2048

#define CHECK_REQUIRE(BUF, N) if(term_buf_require(BUF, N)) return ERROR;

dbl_store*
dbl_store_init()
{
    dbl_store* ret = (dbl_store*) malloc(sizeof(dbl_store));
    if(ret == NULL)
    {
        return NULL;
    }
    
    ret->data = (double*) malloc(INIT_DBL_STORE_SIZE * sizeof(double));
    if(ret->data == NULL)
    {
        free(ret);
        return NULL;
    }
    
    ret->length = INIT_DBL_STORE_SIZE;
    ret->used = 0;
    return ret;
}

void
dbl_store_destroy(dbl_store* store)
{
    free(store->data);
    free(store);
}

double*
dbl_store_add(dbl_store* store, double val)
{
    double* next;
    if(store->used >= store->length)
    {
        store->length *= 2;
        next = (double*) realloc(store->data, store->length * sizeof(double));
        if(next == NULL)
        {
            return NULL;
        }
    }
    
    store->data[store->used++] = val;
    return store->data + (store->used-1);
}

term_buf*
term_buf_init(void)
{
    term_buf* ret = (term_buf*) malloc(sizeof(term_buf));
    if(ret == NULL)
    {
        //fprintf(stderr, "STRUCT FAIL\r\n");
        return NULL;
    }
    
    ret->terms = (ErlDrvTermData*) malloc(INIT_TERM_BUF_SIZE * sizeof(ErlDrvTermData));
    if(ret->terms == NULL)
    {
        //fprintf(stderr, "TERMS FAIL\r\n");
        free(ret);
        return NULL;
    }
    
    ret->store = dbl_store_init();
    if(ret->store == NULL)
    {
        //fprintf(stderr, "STORE FAIL\r\n");
        free(ret->terms);
        free(ret);
        return NULL;
    }

    ret->length = INIT_TERM_BUF_SIZE;
    ret->used = 0;
    return ret;
}

void
term_buf_destroy(term_buf* buf)
{
    dbl_store_destroy(buf->store);
    free(buf->terms);
    free(buf);
}

int
term_buf_require(term_buf* buf, int want)
{
    //fprintf(stderr, "REQ: %p\r\n", buf);
    ErlDrvTermData* next;

    if(buf->length - buf->used > want)
    {
        //fprintf(stderr, "ROOM IS GOOD\r\n");
        return OK;
    }

    //fprintf(stderr, "NEED ROOM\r\n");
    
    buf->length *= 2;
    next = (ErlDrvTermData*) realloc(buf->terms, buf->length * sizeof(ErlDrvTermData));

    if(next == NULL)
    {
        //fprintf(stderr, "TB NEXT IS NULL");
        return ERROR;
    }
    else
    {
        //fprintf(stderr, "TB REQ OK\r\n");
        buf->terms = next;
        return OK;
    }
}

int
term_buf_tuple(term_buf* buf, unsigned int elements)
{
    //fprintf(stderr, "TB TUPLE: %u\r\n", elements);
    CHECK_REQUIRE(buf, 2);
    buf->terms[buf->used++] = ERL_DRV_TUPLE;
    buf->terms[buf->used++] = elements;
    return OK;
}

int
term_buf_list(term_buf* buf, unsigned int elements)
{
    //fprintf(stderr, "TB LIST: %u\r\n", elements);
    CHECK_REQUIRE(buf, 3);
    buf->terms[buf->used++] = ERL_DRV_NIL;
    buf->terms[buf->used++] = ERL_DRV_LIST;
    buf->terms[buf->used++] = elements+1;
    return OK;
}

int
term_buf_binary(term_buf* buf, const void* data, unsigned int length)
{
    //fprintf(stderr, "TB BINARY: %u\r\n", length);
    CHECK_REQUIRE(buf, 3);
    buf->terms[buf->used++] = ERL_DRV_BUF2BINARY;
    buf->terms[buf->used++] = (ErlDrvTermData) data;
    buf->terms[buf->used++] = length;
    return OK;
}

int
term_buf_true(term_buf* buf)
{
    //fprintf(stderr, "TB TRUE\r\n");
    CHECK_REQUIRE(buf, 2);
    buf->terms[buf->used++] = ERL_DRV_ATOM;
    buf->terms[buf->used++] = driver_mk_atom("true");
    return OK;
}

int
term_buf_false(term_buf* buf)
{
    //fprintf(stderr, "TB FALSE\r\n");
    CHECK_REQUIRE(buf, 2);
    buf->terms[buf->used++] = ERL_DRV_ATOM;
    buf->terms[buf->used++] = driver_mk_atom("false");
    return OK;
}

int
term_buf_null(term_buf* buf)
{
    //fprintf(stderr, "TB NULL\r\n");
    CHECK_REQUIRE(buf, 2);
    buf->terms[buf->used++] = ERL_DRV_ATOM;
    buf->terms[buf->used++] = driver_mk_atom("null");
    return OK;
}

int
term_buf_int(term_buf* buf, int value)
{
    //fprintf(stderr, "TB INT\r\n");
    CHECK_REQUIRE(buf, 2);
    buf->terms[buf->used++] = ERL_DRV_INT;
    buf->terms[buf->used++] = (ErlDrvSInt) value;
    return OK;
}

int
term_buf_double(term_buf* buf, double value)
{
    //fprintf(stderr, "TB DOUBLE\r\n");
    CHECK_REQUIRE(buf, 2);

    double* pos = dbl_store_add(buf->store, value);
    if(pos == NULL)
    {
        return ERROR;
    }
    
    buf->terms[buf->used++] = ERL_DRV_FLOAT;
    buf->terms[buf->used++] = (ErlDrvTermData) pos;

    return OK;
}
