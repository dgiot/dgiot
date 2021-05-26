/*
 * Copyright (c) 2010-2013 Aleksey Yeschenko <aleksey@yeschenko.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include "erl_nif.h"

#include <iconv.h>
#include <errno.h>
#include <assert.h>

static ErlNifResourceType *iconv_cd_type = NULL;

typedef struct { iconv_t cd; } iconv_cd;

static struct {
    ERL_NIF_TERM ok;
    ERL_NIF_TERM error;
    ERL_NIF_TERM enomem;
    ERL_NIF_TERM eilseq;
    ERL_NIF_TERM einval;
    ERL_NIF_TERM eunknown;
} iconverl_atoms;

static void
gc_iconv_cd(ErlNifEnv *env, void *cd)
{
    iconv_t icd = ((iconv_cd *) cd)->cd;
    if (icd != (iconv_t) -1)
        iconv_close(icd);
}

static int
load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    ErlNifResourceType *rt = enif_open_resource_type(env, "iconverl",
        "iconv_cd_type", gc_iconv_cd, ERL_NIF_RT_CREATE, NULL);

    if (rt == NULL)
        return -1;

    assert(iconv_cd_type == NULL);
    iconv_cd_type = rt;

    iconverl_atoms.ok       = enif_make_atom(env, "ok");
    iconverl_atoms.error    = enif_make_atom(env, "error");
    iconverl_atoms.enomem   = enif_make_atom(env, "enomem");
    iconverl_atoms.eilseq   = enif_make_atom(env, "eilseq");
    iconverl_atoms.einval   = enif_make_atom(env, "einval");
    iconverl_atoms.eunknown = enif_make_atom(env, "eunknown");

    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static ERL_NIF_TERM
erl_iconv_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char to[32], from[32];
    iconv_cd *cd;
    ERL_NIF_TERM result;

    if (!enif_get_string(env, argv[0], to, 32, ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    if (!enif_get_string(env, argv[1], from, 32, ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    cd = enif_alloc_resource(iconv_cd_type, sizeof(iconv_cd));

    cd->cd = iconv_open(to, from);

    if (cd->cd == (iconv_t) -1) {
        enif_release_resource(cd);
        return enif_make_badarg(env);
    }

    result = enif_make_resource(env, cd);
    enif_release_resource(cd);

    return result;
}

static ERL_NIF_TERM
erl_iconv(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary orig_bin, conv_bin;
    size_t inleft, outleft, outsize;
    unsigned char *in, *out;
    size_t rc;
    iconv_cd *cd;
    ERL_NIF_TERM error, result;

    if (!enif_get_resource(env, argv[0], iconv_cd_type, (void **) &cd))
        return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[1], &orig_bin))
        return enif_make_badarg(env);

    in = orig_bin.data;
    inleft = orig_bin.size;

    outsize = inleft;
    outleft = outsize;

    if (!enif_alloc_binary(outsize, &conv_bin))
        return enif_make_tuple2(env, iconverl_atoms.error, iconverl_atoms.enomem);

    out = conv_bin.data;

    iconv(cd->cd, NULL, NULL, NULL, NULL);

    do {
        rc = iconv(cd->cd, (char **) &in, &inleft, (char **) &out, &outleft);

        if (rc == 0)
            break;

        if (errno == E2BIG) { /* double the binary */
            outleft += outsize;
            outsize *= 2;

            if (!enif_realloc_binary(&conv_bin, outsize)) {
                enif_release_binary(&conv_bin);
                return enif_make_tuple2(env, iconverl_atoms.error, iconverl_atoms.enomem);
            }

            out = conv_bin.data + (outsize - outleft);
        } else {
            enif_release_binary(&conv_bin);

            if      (errno == EILSEQ) { error = iconverl_atoms.eilseq;   }
            else if (errno == EINVAL) { error = iconverl_atoms.einval;   }
            else                      { error = iconverl_atoms.eunknown; }

            return enif_make_tuple2(env, iconverl_atoms.error, error);
        }
    } while (rc != 0);

    if (outleft > 0)
        enif_realloc_binary(&conv_bin, outsize - outleft);

    result = enif_make_binary(env, &conv_bin);

    return enif_make_tuple2(env, iconverl_atoms.ok, result);
}

static ErlNifFunc nif_funcs[] = {
    {"open", 2, erl_iconv_open},
    {"conv", 2, erl_iconv}
};

ERL_NIF_INIT(iconverl, nif_funcs, load, NULL, upgrade, NULL)
