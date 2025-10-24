#pragma once
#include "sc_gluon_v1_types.h"

#ifdef __cplusplus
#    define SC_GLUON_EXTERN extern "C"
#endif

#define SC_GLUON_EXPORT SC_GLUON_EXTERN __attribute__((__visibility__("hidden")))

// Required
SC_GLUON_EXPORT uint32_t sc_gluon_version();

// Required
SC_GLUON_EXPORT sc_gluon_library_data_v1_t
sc_gluon_load_library(sc_gluon_do_callback_v1_f, sc_gluon_release_callback_object_v1_f,
                      struct sc_gluon_function_declarations_v1_t** const out_decls, uint32_t* out_size);


// Optional
SC_GLUON_EXPORT void sc_gluon_post_load_library(sc_gluon_library_data_v1_t,
                                                struct sc_gluon_function_declarations_v1_t* decls_to_be_freed,
                                                uint32_t decls_size);


// Optional
SC_GLUON_EXPORT void sc_gluon_unload_library(sc_gluon_library_data_v1_t);
