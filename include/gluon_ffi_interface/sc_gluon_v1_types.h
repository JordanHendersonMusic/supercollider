
#pragma once
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <cstring>
/*

SuperCollider foreign function interface.
Author Jordan Henderson - JordanHendersonMusic 2025

There are three parts to this header:
    1. Entry points, these are functions that will be called from the sc host. They must be named accordingly.
    2. The definition of the function signature. When implementing a function to be called from sc, it must have this
        signature.
    3. The parameter definition. To avoid exposing sc internals (which may change), an intermediate format is given for
        arguments, this means not all supercollider types can be passed to foreign functions.


The three types of entry point are 'version', 'load', 'post_load', and 'unload'.

    Version declared which api to use, it is the only entry point shared across all versions.

    In 'load', a pointer to an array of structures describing what functions the library offers must be set.
    A name, an implementation pointer, the number of params expected (-1 if accepts any number), and a
    bool indicating whether the function accepts a callback must all be provided. A pointer to some data
    for the library may be returned, this will be passed to each function allowing for library state without using
    globals. Additionally, the function provides a callback function, which should be stored in the case a function
    wishes to return execution back to sc from another thread.

    In 'post_load' you are provided the opportunity to deallocate the array of function descriptions.

    In 'unload' you can deallocate the library state.


The functions accepts the library state, a nullable pointer to some callback object (this is a type erased
PyrObject* of which `value' will be called upon), the input parameter array, and an output parameter or potential
diagnostic. Additionally, it must return a tag indicating whether an output parameter or diagnostic has been produced.

Parameters are essentially tagged unions that when containing allocated resources indicate their size and whether they
own their memory.

All input parameters that have allocated resources (arrays) will be freed after the function has finished if they 'own'
their data. To prevent this, the 'owns_data' member may be set to false, thereby taking ownership of the memory.
This memory can either be freed manually at a later date, or returned as a part of an output parameter, ensuring the
output parameter 'owns' the data.

Input parameters who do not own their data have unknown lifetimes, the data will persist for the duration of the
function call, but may be freed immediately after, or by sc's garbage collector at an unknown point in time.

When passing any 'owned' memory back to the sc host, it must be allocate with malloc as free will be called on it.
Alternatively, you can pass unowned memory back to the host and no attempt to deallocate will be made.

*/

typedef uint32_t (*sc_gluon_version_f)();

typedef void* sc_gluon_library_data_v1_t;
typedef void* sc_gluon_callable_object_v1_t;
typedef void (*sc_gluon_do_callback_v1_f)(sc_gluon_callable_object_v1_t, struct sc_gluon_param_v1_t* params,
                                          uint32_t num_params);
typedef void (*sc_gluon_release_callback_object_v1_f)(sc_gluon_callable_object_v1_t);

//////////// ENTRY POINTS FOR LIBRARY.

// Required. Must be called: sc_gluon_load_library
typedef sc_gluon_library_data_v1_t (*sc_gluon_load_library_v1_f)(
    sc_gluon_do_callback_v1_f, sc_gluon_release_callback_object_v1_f,
    struct sc_gluon_function_declarations_v1_t** const out_decls, uint32_t* out_size);

// Optional. Must be called: sc_gluon_post_load_library.
typedef void (*sc_gluon_post_load_library_v1_f)(sc_gluon_library_data_v1_t,
                                                struct sc_gluon_function_declarations_v1_t* decls_to_be_freed,
                                                uint32_t decls_size);

// Optional. Must be called: sc_gluon_unload_library.
typedef void (*sc_gluon_unload_library_v1_f)(sc_gluon_library_data_v1_t);

////////////  FUNCTION PTRS

enum sc_gluon_out_param_tag_v1 {
    sc_gluon_produced_param,
    sc_gluon_error_with_owned_diagnostic,
    sc_gluon_error_with_non_owned_diagnostic,
    sc_gluon_error_without_diagnostic,
};

typedef enum sc_gluon_out_param_tag_v1 (*sc_gluon_function_v1_f)(
    sc_gluon_library_data_v1_t library_data, sc_gluon_callable_object_v1_t maybe_callback_data,
    struct sc_gluon_param_v1_t* in_params, uint32_t num_in_params,
    union sc_gluon_out_param_or_maybe_diagnostic_v1* out_param);


//////////// PARAMS

struct sc_gluon_function_declarations_v1_t {
    const char* name;
    sc_gluon_function_v1_f ptr;
    int32_t num_parms; // negative means accepts any number of arguments.
    bool accepts_callback;
};

enum sc_gluon_param_tag_v1 {
    sc_gluon_nil,
    sc_gluon_i32,
    sc_gluon_f64,
    sc_gluon_char,
    sc_gluon_bool,
    sc_gluon_raw_pointer,
    sc_gluon_symbol_value,
    sc_gluon_u8_array,
    sc_gluon_f64_array,
    sc_gluon_f32_array,
    sc_gluon_char_array, // Note, this is not a null terminated string, but an array with a size.
    //
    sc_gluon_param_array,
};

union sc_gluon_data_v1 {
    int32_t nil_;
    int32_t i32;
    double f64;
    char character;
    bool boolean;
    void* raw_pointer;
    uint64_t symbol_value;
    uint8_t* u8_array;
    double* f64_array;
    float* f32_array;
    char* character_array;
    struct sc_gluon_param_v1_t* param_array;
};

struct sc_gluon_param_v1_t {
    union sc_gluon_data_v1 data;
    uint32_t size; // For non-array parameters, this is ignored, but set to '1' as a good practice.
    enum sc_gluon_param_tag_v1 tag;
    bool owns_data; // Indicates that the param owns some heap allocated data, and will be deallocated when the lifetime
                    // of the parameter ends.
};

union sc_gluon_out_param_or_maybe_diagnostic_v1 {
    struct sc_gluon_param_v1_t out_param;
    const char* maybe_diagnostic;
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// HELPERS

inline void sc_gluon_free_param_v1(struct sc_gluon_param_v1_t param) {
    if (param.tag == sc_gluon_param_array) {
        for (uint32_t i = 0; i < param.size; ++i)
            sc_gluon_free_param_v1(param.data.param_array[i]);

        if (param.owns_data)
            free(param.data.param_array);
    } else {
        if (param.owns_data) {
            switch (param.tag) {
            case sc_gluon_u8_array: {
                free(param.data.u8_array);
                break;
            }
            case sc_gluon_f64_array: {
                free(param.data.f64_array);
                break;
            }
            case sc_gluon_f32_array: {
                free(param.data.f32_array);
                break;
            }
            case sc_gluon_char_array: {
                free(param.data.character_array);
                break;
            }
            default:
                break;
            }
        }
    }
}

inline struct sc_gluon_param_v1_t sc_gluon_copy_param_data_v1(struct sc_gluon_param_v1_t param) {
    switch (param.tag) {
    case sc_gluon_param_array: {
        // Recursive case.
        sc_gluon_data_v1 d {};
        d.param_array = (sc_gluon_param_v1_t*)malloc(param.size * sizeof(sc_gluon_param_v1_t));
        for (uint32_t i = 0; i < param.size; ++i)
            d.param_array[i] = sc_gluon_copy_param_data_v1(param.data.param_array[i]);

        return { d, param.size, sc_gluon_param_array, true };
    }
    case sc_gluon_u8_array: {
        sc_gluon_data_v1 d {};
        d.u8_array = (uint8_t*)malloc(param.size * sizeof(uint8_t));
        memcpy(d.u8_array, param.data.u8_array, param.size * sizeof(uint8_t));
        return { d, param.size, sc_gluon_u8_array, true };
    }
    case sc_gluon_f64_array: {
        sc_gluon_data_v1 d {};
        d.f64_array = (double*)malloc(param.size * sizeof(double));
        memcpy(d.f64_array, param.data.f64_array, param.size * sizeof(double));
        return { d, param.size, sc_gluon_f64_array, true };
    }
    case sc_gluon_f32_array: {
        sc_gluon_data_v1 d {};
        d.f32_array = (float*)malloc(param.size * sizeof(float));
        memcpy(d.f32_array, param.data.f64_array, param.size * sizeof(float));
        return { d, param.size, sc_gluon_f32_array, true };
    }
    case sc_gluon_char_array: {
        sc_gluon_data_v1 d {};
        d.character_array = (char*)malloc(param.size * sizeof(char));
        memcpy(d.character_array, param.data.character_array, param.size * sizeof(char));
        return { d, param.size, sc_gluon_char_array, true };
    }
    default: {
        return param;
    }
    }
}
