/*
 * sexp_helpers.c - SEXP pointer helpers with GC protection
 *
 * These functions allow extracting pointers from R objects while ensuring
 * the objects remain protected from garbage collection.
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* Tag symbols for identifying our protected pointers */
static SEXP protected_sexp_tag = NULL;
static SEXP protected_data_tag = NULL;

/* Initialize tag symbols (called once) */
static void init_tags(void) {
    if (protected_sexp_tag == NULL) {
        protected_sexp_tag = Rf_install("protected_sexp");
        R_PreserveObject(protected_sexp_tag);
    }
    if (protected_data_tag == NULL) {
        protected_data_tag = Rf_install("protected_data");
        R_PreserveObject(protected_data_tag);
    }
}

/* Finalizer for external pointer - releases protection */
static void protected_ptr_finalizer(SEXP ptr) {
    SEXP protected_obj = R_ExternalPtrProtected(ptr);
    if (protected_obj != R_NilValue) {
        R_ReleaseObject(protected_obj);
        R_SetExternalPtrProtected(ptr, R_NilValue);
    }
}

/*
 * R_sexp_ptr - Get SEXP pointer with GC protection
 *
 * Returns an external pointer to the SEXP itself.
 * The SEXP is protected via R_PreserveObject until the external pointer
 * is garbage collected.
 */
SEXP R_sexp_ptr(SEXP x) {
    init_tags();
    
    /* Protect the object from GC */
    R_PreserveObject(x);
    
    /* Create external pointer with:
     * - ptr: the SEXP address (as void*)
     * - tag: our marker symbol
     * - prot: the protected SEXP (so finalizer knows what to release)
     */
    SEXP extptr = PROTECT(R_MakeExternalPtr((void*)x, protected_sexp_tag, x));
    
    /* Register finalizer to release protection */
    R_RegisterCFinalizerEx(extptr, protected_ptr_finalizer, TRUE);
    
    UNPROTECT(1);
    return extptr;
}

/*
 * R_data_ptr - Get data pointer with GC protection
 *
 * Returns an external pointer to the underlying data array.
 * Works for INTSXP, REALSXP, CPLXSXP, RAWSXP, VECSXP, STRSXP.
 */
SEXP R_data_ptr(SEXP x) {
    init_tags();
    
    void *data = NULL;
    
    switch (TYPEOF(x)) {
        case INTSXP:
        case LGLSXP:
            data = INTEGER(x);
            break;
        case REALSXP:
            data = REAL(x);
            break;
        case CPLXSXP:
            data = COMPLEX(x);
            break;
        case RAWSXP:
            data = RAW(x);
            break;
        case VECSXP:  /* List */
            data = (void*)VECTOR_PTR_RO(x);
            break;
        case STRSXP:  /* Character vector - pointer to SEXP array */
            data = (void*)STRING_PTR_RO(x);
            break;
        default:
            Rf_error("Cannot get data pointer for type %s", Rf_type2char(TYPEOF(x)));
    }
    
    /* Protect the object from GC */
    R_PreserveObject(x);
    
    /* Create external pointer */
    SEXP extptr = PROTECT(R_MakeExternalPtr(data, protected_data_tag, x));
    
    /* Register finalizer */
    R_RegisterCFinalizerEx(extptr, protected_ptr_finalizer, TRUE);
    
    UNPROTECT(1);
    return extptr;
}

/*
 * R_data_ptr_ro - Get read-only data pointer (ALTREP-safe)
 */
SEXP R_data_ptr_ro(SEXP x) {
    init_tags();
    
    const void *data = NULL;
    
    switch (TYPEOF(x)) {
        case INTSXP:
        case LGLSXP:
            data = INTEGER_RO(x);
            break;
        case REALSXP:
            data = REAL_RO(x);
            break;
        case CPLXSXP:
            data = COMPLEX_RO(x);
            break;
        case RAWSXP:
            data = RAW_RO(x);
            break;
        case VECSXP:
            data = VECTOR_PTR_RO(x);
            break;
        case STRSXP:
            data = STRING_PTR_RO(x);
            break;
        default:
            Rf_error("Cannot get data pointer for type %s", Rf_type2char(TYPEOF(x)));
    }
    
    /* Protect the object from GC */
    R_PreserveObject(x);
    
    /* Create external pointer (cast away const - user must respect read-only) */
    SEXP extptr = PROTECT(R_MakeExternalPtr((void*)data, protected_data_tag, x));
    
    /* Register finalizer */
    R_RegisterCFinalizerEx(extptr, protected_ptr_finalizer, TRUE);
    
    UNPROTECT(1);
    return extptr;
}

/*
 * R_is_protected_ptr - Check if pointer was created by our helpers
 */
SEXP R_is_protected_ptr(SEXP ptr) {
    init_tags();
    
    if (TYPEOF(ptr) != EXTPTRSXP) {
        return Rf_ScalarLogical(FALSE);
    }
    
    SEXP tag = R_ExternalPtrTag(ptr);
    return Rf_ScalarLogical(tag == protected_sexp_tag || tag == protected_data_tag);
}

/*
 * R_release_protected_ptr - Manually release protection early
 */
SEXP R_release_protected_ptr(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Not an external pointer");
    }
    
    SEXP protected_obj = R_ExternalPtrProtected(ptr);
    if (protected_obj != R_NilValue) {
        R_ReleaseObject(protected_obj);
        R_SetExternalPtrProtected(ptr, R_NilValue);
        R_ClearExternalPtr(ptr);  /* Invalidate the pointer */
    }
    
    return R_NilValue;
}

/*
 * R_ptr_to_sexp - Get the original SEXP from a protected pointer
 */
SEXP R_ptr_to_sexp(SEXP ptr) {
    init_tags();
    
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("Not an external pointer");
    }
    
    SEXP tag = R_ExternalPtrTag(ptr);
    
    if (tag == protected_sexp_tag) {
        /* For sexp_ptr, the pointer IS the SEXP */
        void *p = R_ExternalPtrAddr(ptr);
        if (p == NULL) {
            Rf_error("Pointer has been released");
        }
        return (SEXP)p;
    } else if (tag == protected_data_tag) {
        /* For data_ptr, the SEXP is in the protected slot */
        SEXP obj = R_ExternalPtrProtected(ptr);
        if (obj == R_NilValue) {
            Rf_error("Pointer has been released");
        }
        return obj;
    } else {
        Rf_error("Not a protected SEXP pointer");
    }
}
