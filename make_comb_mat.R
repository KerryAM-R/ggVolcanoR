make_comb_mat <- function (..., mode = c("distinct", "intersect", "union"), top_n_sets = Inf, 
                           min_set_size = -Inf, universal_set = NULL, complement_size = NULL, 
                           value_fun = NULL, set_on_rows = TRUE) 
{
  lt = list(...)
  if ("remove_complement_set" %in% names(lt)) {
    stop_wrap("Argument `remove_complement_set` has been removed.")
  }
  mode = match.arg(mode)[1]
  if (length(lt) == 1) {
    lt = lt[[1]]
    if (length(dim(lt)) == 2) {
      if (ncol(lt) > 31) {
        stop_wrap("Only support number of sets <= 31.")
      }
      m = make_comb_mat_from_matrix(lt, mode = mode, top_n_sets = top_n_sets, 
                                    min_set_size = min_set_size, universal_set = universal_set, 
                                    complement_size = complement_size, set_on_rows = set_on_rows)
      return(m)
    }
  }
  if (is.null(value_fun)) {
    if (inherits(lt[[1]], "GRanges")) {
      value_fun = function(x) sum(as.numeric(getFromNamespace("width", 
                                                              ns = "BiocGenerics")(x)))
    }
    else if (inherits(lt[[1]], "IRanges")) {
      value_fun = function(x) sum(as.numeric(getFromNamespace("width", 
                                                              ns = "BiocGenerics")(x)))
    }
    else {
      value_fun = length
    }
  }
  if (length(lt) > 31) {
    stop_wrap("Only support number of sets <= 31.")
  }
  if (is.atomic(lt[[1]])) {
    m = make_comb_mat_from_matrix(list_to_matrix(lt), mode = mode, 
                                  top_n_sets = top_n_sets, min_set_size = min_set_size, 
                                  universal_set = universal_set, complement_size = complement_size, 
                                  set_on_rows = set_on_rows)
    return(m)
  }
  m = make_comb_mat_from_list(lt, value_fun, mode = mode, 
                              top_n_sets = top_n_sets, min_set_size = min_set_size, 
                              universal_set = universal_set, complement_size = complement_size, 
                              set_on_rows = set_on_rows)
  return(m)
}
