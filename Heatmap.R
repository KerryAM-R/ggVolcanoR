Heatmap <- function (matrix, col, name, na_col = "grey", color_space = "LAB", 
          rect_gp = gpar(col = NA), border = NA, border_gp = gpar(col = "black"), 
          cell_fun = NULL, layer_fun = NULL, jitter = FALSE, row_title = character(0), 
          row_title_side = c("left", "right"), row_title_gp = gpar(fontsize = 13.2), 
          row_title_rot = switch(row_title_side[1], left = 90, right = 270), 
          column_title = character(0), column_title_side = c("top", 
                                                             "bottom"), column_title_gp = gpar(fontsize = 13.2), 
          column_title_rot = 0, cluster_rows = TRUE, cluster_row_slices = TRUE, 
          clustering_distance_rows = "euclidean", clustering_method_rows = "complete", 
          row_dend_side = c("left", "right"), row_dend_width = unit(10, 
                                                                    "mm"), show_row_dend = TRUE, row_dend_reorder = is.logical(cluster_rows) || 
            is.function(cluster_rows), row_dend_gp = gpar(), cluster_columns = TRUE, 
          cluster_column_slices = TRUE, clustering_distance_columns = "euclidean", 
          clustering_method_columns = "complete", column_dend_side = c("top", 
                                                                       "bottom"), column_dend_height = unit(10, "mm"), show_column_dend = TRUE, 
          column_dend_gp = gpar(), column_dend_reorder = is.logical(cluster_columns) || 
            is.function(cluster_columns), row_order = NULL, column_order = NULL, 
          row_labels = rownames(matrix), row_names_side = c("right", 
                                                            "left"), show_row_names = TRUE, row_names_max_width = unit(6, 
                                                                                                                       "cm"), row_names_gp = gpar(fontsize = 12), row_names_rot = 0, 
          row_names_centered = FALSE, column_labels = colnames(matrix), 
          column_names_side = c("bottom", "top"), show_column_names = TRUE, 
          column_names_max_height = unit(6, "cm"), column_names_gp = gpar(fontsize = 12), 
          column_names_rot = 90, column_names_centered = FALSE, top_annotation = NULL, 
          bottom_annotation = NULL, left_annotation = NULL, right_annotation = NULL, 
          km = 1, split = NULL, row_km = km, row_km_repeats = 1, row_split = split, 
          column_km = 1, column_km_repeats = 1, column_split = NULL, 
          gap = unit(1, "mm"), row_gap = unit(1, "mm"), column_gap = unit(1, 
                                                                          "mm"), show_parent_dend_line = ht_opt$show_parent_dend_line, 
          heatmap_width = unit(1, "npc"), width = NULL, heatmap_height = unit(1, 
                                                                              "npc"), height = NULL, show_heatmap_legend = TRUE, heatmap_legend_param = list(title = name), 
          use_raster = NULL, raster_device = c("CairoPNG", "CairoJPEG", 
                                               "CairoTIFF", "png", "jpeg", "tiff", "agg_png"), raster_quality = 1, 
          raster_device_param = list(), raster_resize_mat = FALSE, 
          raster_by_magick = requireNamespace("magick", quietly = TRUE), 
          raster_magick_filter = NULL, post_fun = NULL) 
{
  dev.null()
  on.exit(dev.off2())
  verbose = ht_opt("verbose")
  .Object = new("Heatmap")
  if (missing(name)) {
    name = paste0("matrix_", get_heatmap_index() + 1)
    increase_heatmap_index()
  }
  else if (is.null(name)) {
    name = paste0("matrix_", get_heatmap_index() + 1)
    increase_heatmap_index()
  }
  if (name == "") {
    stop_wrap("Heatmap name cannot be empty string.")
  }
  .Object@name = name
  called_args = names(as.list(match.call())[-1])
  for (opt_name in c("row_names_gp", "column_names_gp", "row_title_gp", 
                     "column_title_gp")) {
    opt_name2 = paste0("heatmap_", opt_name)
    if (!opt_name %in% called_args) {
      if (!is.null(ht_opt(opt_name2))) {
        if (verbose) 
          qqcat("re-assign @{opt_name} with `ht_opt('@{opt_name2}'')`\n")
        assign(opt_name, ht_opt(opt_name2))
      }
    }
  }
  if ("top_annotation_height" %in% called_args) {
    stop_wrap("`top_annotation_height` is removed. Set the height directly in `HeatmapAnnotation()`.")
  }
  if ("bottom_annotation_height" %in% called_args) {
    stop_wrap("`bottom_annotation_height` is removed. Set the height directly in `HeatmapAnnotation()`.")
  }
  if ("combined_name_fun" %in% called_args) {
    stop_wrap("`combined_name_fun` is removed. Please directly set `row_names_title`. See https://jokergoo.github.io/ComplexHeatmap-reference/book/a-single-heatmap.html#titles-for-splitting")
  }
  if ("heatmap_legend_param" %in% called_args) {
    for (opt_name in setdiff(c("title_gp", "title_position", 
                               "labels_gp", "grid_width", "grid_height", "border"), 
                             names(heatmap_legend_param))) {
      opt_name2 = paste0("legend_", opt_name)
      if (!is.null(ht_opt(opt_name2))) 
        if (verbose) 
          qqcat("re-assign heatmap_legend_param$@{opt_name} with `ht_opt('@{opt_name2}'')`\n")
      heatmap_legend_param[[opt_name]] = ht_opt(opt_name2)
    }
  }
  else {
    for (opt_name in c("title_gp", "title_position", "labels_gp", 
                       "grid_width", "grid_height", "border")) {
      opt_name2 = paste0("legend_", opt_name)
      if (!is.null(ht_opt(opt_name2))) 
        if (verbose) 
          qqcat("re-assign heatmap_legend_param$@{opt_name} with `ht_opt('@{opt_name2}'')`\n")
      heatmap_legend_param[[opt_name]] = ht_opt(opt_name2)
    }
  }
  if (is.data.frame(matrix)) {
    if (verbose) 
      qqcat("convert data frame to matrix\n")
    warning_wrap("The input is a data frame, convert it to the matrix.")
    matrix = as.matrix(matrix)
  }
  fa_level = NULL
  if (!is.matrix(matrix)) {
    if (is.atomic(matrix)) {
      if (is.factor(matrix)) {
        fa_level = levels(matrix)
      }
      rn = names(matrix)
      matrix = matrix(matrix, ncol = 1)
      if (!is.null(rn)) 
        rownames(matrix) = rn
      if (!missing(name)) 
        colnames(matrix) = name
      if (verbose) 
        qqcat("convert simple vector to one-column matrix\n")
    }
    else {
      stop_wrap("If input is not a matrix, it should be a simple vector.")
    }
  }
  if (missing(row_km)) 
    row_km = km
  if (is.null(row_km)) 
    row_km = 1
  if (missing(row_split)) 
    row_split = split
  if (missing(row_gap)) 
    row_gap = gap
  if (is.null(column_km)) 
    column_km = 1
  if (ncol(matrix) == 0 || nrow(matrix) == 0) {
    if (!inherits(cluster_columns, c("dendrogram", "hclust"))) {
      cluster_columns = FALSE
      show_column_dend = FALSE
    }
    if (!inherits(cluster_rows, c("dendrogram", "hclust"))) {
      cluster_rows = FALSE
      show_row_dend = FALSE
    }
    row_km = 1
    column_km = 1
    if (verbose) 
      qqcat("zero row/column matrix, set cluster_columns/rows to FALSE\n")
  }
  if (ncol(matrix) == 1) {
    if (!inherits(cluster_columns, c("dendrogram", "hclust"))) {
      cluster_columns = FALSE
      show_column_dend = FALSE
    }
    column_km = 1
    if (verbose) 
      qqcat("one-column matrix, set cluster_columns to FALSE\n")
  }
  if (nrow(matrix) == 1) {
    if (!inherits(cluster_rows, c("dendrogram", "hclust"))) {
      cluster_rows = FALSE
      show_row_dend = FALSE
    }
    row_km = 1
    if (verbose) 
      qqcat("one-row matrix, set cluster_rows to FALSE\n")
  }
  if (is.character(matrix)) {
    called_args = names(match.call()[-1])
    if ("clustering_distance_rows" %in% called_args) {
    }
    else if (inherits(cluster_rows, c("dendrogram", "hclust"))) {
    }
    else {
      cluster_rows = FALSE
      show_row_dend = FALSE
    }
    row_dend_reorder = FALSE
    cluster_row_slices = FALSE
    if (inherits(cluster_rows, c("dendrogram", "hclust")) && 
        length(row_split) == 1) {
      if (!"cluster_row_slices" %in% called_args) {
        cluster_row_slices = TRUE
      }
    }
    if ("clustering_distance_columns" %in% called_args) {
    }
    else if (inherits(cluster_columns, c("dendrogram", "hclust"))) {
    }
    else {
      cluster_columns = FALSE
      show_column_dend = FALSE
    }
    column_dend_reorder = FALSE
    cluster_column_slices = FALSE
    if (inherits(cluster_columns, c("dendrogram", "hclust")) && 
        length(column_split) == 1) {
      if (!"cluster_column_slices" %in% called_args) {
        cluster_column_slices = TRUE
      }
    }
    row_km = 1
    column_km = 1
    if (verbose) 
      qqcat("matrix is character. Do not cluster unless distance method is provided.\n")
  }
  class(matrix) = "matrix"
  .Object@matrix = matrix
  .Object@matrix_param$row_km = row_km
  .Object@matrix_param$row_km_repeats = row_km_repeats
  .Object@matrix_param$row_gap = row_gap
  .Object@matrix_param$column_km = column_km
  .Object@matrix_param$column_km_repeats = column_km_repeats
  .Object@matrix_param$column_gap = column_gap
  .Object@matrix_param$jitter = jitter
  if (!is.null(row_split)) {
    if (inherits(cluster_rows, c("dendrogram", "hclust"))) {
      if (is.numeric(row_split) && length(row_split) == 
          1) {
        .Object@matrix_param$row_split = row_split
      }
      else {
        stop_wrap("When `cluster_rows` is a dendrogram, `row_split` can only be a single number.")
      }
    }
    else {
      if (identical(cluster_rows, TRUE) && is.numeric(row_split) && 
          length(row_split) == 1) {
      }
      else {
        if (!is.data.frame(row_split)) 
          row_split = data.frame(row_split)
        if (nrow(row_split) != nrow(matrix)) {
          stop_wrap("Length or nrow of `row_split` should be same as nrow of `matrix`.")
        }
      }
    }
  }
  .Object@matrix_param$row_split = row_split
  if (!is.null(column_split)) {
    if (inherits(cluster_columns, c("dendrogram", "hclust"))) {
      if (is.numeric(column_split) && length(column_split) == 
          1) {
        .Object@matrix_param$column_split = column_split
      }
      else {
        stop_wrap("When `cluster_columns` is a dendrogram, `column_split` can only be a single number.")
      }
    }
    else {
      if (identical(cluster_columns, TRUE) && is.numeric(column_split) && 
          length(column_split) == 1) {
      }
      else {
        if (!is.data.frame(column_split)) 
          column_split = data.frame(column_split)
        if (nrow(column_split) != ncol(matrix)) {
          stop_wrap("Length or ncol of `column_split` should be same as ncol of `matrix`.")
        }
      }
    }
  }
  .Object@matrix_param$column_split = column_split
  .Object@matrix_param$gp = check_gp(rect_gp)
  if (missing(border)) {
    if (!is.null(ht_opt$heatmap_border)) 
      border = ht_opt$heatmap_border
  }
  if (!missing(border_gp) && missing(border)) 
    border = TRUE
  .Object@matrix_param$border = border
  .Object@matrix_param$border_gp = border_gp
  .Object@matrix_param$cell_fun = cell_fun
  .Object@matrix_param$layer_fun = layer_fun
  if (nrow(matrix) > 100 || ncol(matrix) > 100) {
    if (!is.null(cell_fun)) {
      warning_wrap("You defined `cell_fun` for a heatmap with more than 100 rows or columns, which might be very slow to draw. Consider to use the vectorized version `layer_fun`.")
    }
  }
  if (ncol(matrix) > 0 && nrow(matrix) > 0) {
    if (missing(col)) {
      col = default_col(matrix, main_matrix = TRUE)
      if (!is.null(fa_level)) {
        col = col[fa_level]
      }
      if (verbose) 
        qqcat("color is not specified, use randomly generated colors\n")
    }
    if (is.null(col)) {
      col = default_col(matrix, main_matrix = TRUE)
      if (!is.null(fa_level)) {
        col = col[fa_level]
      }
      if (verbose) 
        qqcat("color is not specified, use randomly generated colors\n")
    }
    if (is.function(col)) {
      .Object@matrix_color_mapping = ColorMapping(col_fun = col, 
                                                  name = name, na_col = na_col)
      if (verbose) 
        qqcat("input color is a color mapping function\n")
    }
    else if (inherits(col, "ColorMapping")) {
      .Object@matrix_color_mapping = col
      if (verbose) 
        qqcat("input color is a ColorMapping object\n")
    }
    else {
      if (is.null(names(col))) {
        if (length(col) == length(unique(as.vector(matrix)))) {
          if (is.null(fa_level)) {
            if (is.numeric(matrix)) {
              names(col) = sort(unique(as.vector(matrix)))
              col = rev(col)
            }
            else {
              names(col) = sort(unique(as.vector(matrix)))
            }
          }
          else {
            names(col) = fa_level
          }
          .Object@matrix_color_mapping = ColorMapping(colors = col, 
                                                      name = name, na_col = na_col)
          if (verbose) 
            qqcat("input color is a vector with no names, treat it as discrete color mapping\n")
        }
        else if (is.numeric(matrix)) {
          col = colorRamp2(seq(min(matrix, na.rm = TRUE), 
                               max(matrix, na.rm = TRUE), length = length(col)), 
                           col, space = color_space)
          .Object@matrix_color_mapping = ColorMapping(col_fun = col, 
                                                      name = name, na_col = na_col)
          if (verbose) 
            qqcat("input color is a vector with no names, treat it as continuous color mapping\n")
        }
        else {
          stop_wrap("`col` should have names to map to values in `mat`.")
        }
      }
      else {
        full_col = col
        if (is.null(fa_level)) {
          col = col[intersect(c(names(col), "_NA_"), 
                              as.character(matrix))]
        }
        else {
          col = col[intersect(c(fa_level, "_NA_"), names(col))]
        }
        .Object@matrix_color_mapping = ColorMapping(colors = col, 
                                                    name = name, na_col = na_col, full_col = full_col)
        if (verbose) 
          qqcat("input color is a named vector\n")
      }
    }
    .Object@matrix_legend_param = heatmap_legend_param
  }
  if (identical(row_title, NA) || identical(row_title, "")) {
    row_title = character(0)
  }
  .Object@row_title = row_title
  .Object@row_title_param$rot = row_title_rot%%360
  .Object@row_title_param$side = match.arg(row_title_side)[1]
  .Object@row_title_param$gp = check_gp(row_title_gp)
  .Object@row_title_param$just = get_text_just(rot = row_title_rot, 
                                               side = .Object@row_title_param$side)
  if (identical(column_title, NA) || identical(column_title, 
                                               "")) {
    column_title = character(0)
  }
  .Object@column_title = column_title
  .Object@column_title_param$rot = column_title_rot%%360
  .Object@column_title_param$side = match.arg(column_title_side)[1]
  .Object@column_title_param$gp = check_gp(column_title_gp)
  .Object@column_title_param$just = get_text_just(rot = column_title_rot, 
                                                  side = .Object@column_title_param$side)
  if (is.null(rownames(matrix))) {
    if (is.null(row_labels)) {
      show_row_names = FALSE
    }
  }
  .Object@row_names_param$labels = row_labels
  .Object@row_names_param$side = match.arg(row_names_side)[1]
  .Object@row_names_param$show = show_row_names
  .Object@row_names_param$gp = check_gp(row_names_gp)
  .Object@row_names_param$rot = row_names_rot
  .Object@row_names_param$centered = row_names_centered
  .Object@row_names_param$max_width = row_names_max_width + 
    unit(2, "mm")
  if (show_row_names) {
    if (length(row_labels) != nrow(matrix)) {
      stop_wrap("Length of `row_labels` should be the same as the nrow of matrix.")
    }
    if (row_names_centered) {
      row_names_anno = anno_text(row_labels, which = "row", 
                                 gp = row_names_gp, rot = row_names_rot, location = 0.5, 
                                 just = "center")
    }
    else {
      row_names_anno = anno_text(row_labels, which = "row", 
                                 gp = row_names_gp, rot = row_names_rot, location = ifelse(.Object@row_names_param$side == 
                                                                                             "left", 1, 0), just = ifelse(.Object@row_names_param$side == 
                                                                                                                            "left", "right", "left"))
    }
    .Object@row_names_param$anno = row_names_anno
  }
  if (is.null(colnames(matrix))) {
    if (is.null(column_labels)) {
      show_column_names = FALSE
    }
  }
  .Object@column_names_param$labels = column_labels
  .Object@column_names_param$side = match.arg(column_names_side)[1]
  .Object@column_names_param$show = show_column_names
  .Object@column_names_param$gp = check_gp(column_names_gp)
  .Object@column_names_param$rot = column_names_rot
  .Object@column_names_param$centered = column_names_centered
  .Object@column_names_param$max_height = column_names_max_height + 
    unit(2, "mm")
  if (show_column_names) {
    if (length(column_labels) != ncol(matrix)) {
      stop_wrap("Length of `column_labels` should be the same as the ncol of matrix.")
    }
    if (column_names_centered) {
      column_names_anno = anno_text(column_labels, which = "column", 
                                    gp = column_names_gp, rot = column_names_rot, 
                                    location = 0.5, just = "center")
    }
    else {
      column_names_anno = anno_text(column_labels, which = "column", 
                                    gp = column_names_gp, rot = column_names_rot, 
                                    location = ifelse(.Object@column_names_param$side == 
                                                        "top", 0, 1), just = ifelse(.Object@column_names_param$side == 
                                                                                      "top", ifelse(.Object@column_names_param$rot >= 
                                                                                                      0, "left", "right"), ifelse(.Object@column_names_param$rot >= 
                                                                                                                                    0, "right", "left")))
    }
    .Object@column_names_param$anno = column_names_anno
  }
  if (missing(cluster_rows) && !missing(row_order)) {
    cluster_rows = FALSE
  }
  if (is.logical(cluster_rows)) {
    if (!cluster_rows) {
      row_dend_width = unit(0, "mm")
      show_row_dend = FALSE
    }
    .Object@row_dend_param$cluster = cluster_rows
  }
  else if (inherits(cluster_rows, "dendrogram") || inherits(cluster_rows, 
                                                            "hclust")) {
    .Object@row_dend_param$obj = cluster_rows
    .Object@row_dend_param$cluster = TRUE
  }
  else if (inherits(cluster_rows, "function")) {
    .Object@row_dend_param$fun = cluster_rows
    .Object@row_dend_param$cluster = TRUE
  }
  else {
    oe = try(cluster_rows <- as.dendrogram(cluster_rows), 
             silent = TRUE)
    if (!inherits(oe, "try-error")) {
      .Object@row_dend_param$obj = cluster_rows
      .Object@row_dend_param$cluster = TRUE
    }
    else {
      stop_wrap("`cluster_rows` should be a logical value, a clustering function or a clustering object.")
    }
  }
  if (!show_row_dend) {
    row_dend_width = unit(0, "mm")
  }
  .Object@row_dend_list = list()
  .Object@row_dend_param$distance = clustering_distance_rows
  .Object@row_dend_param$method = clustering_method_rows
  .Object@row_dend_param$side = match.arg(row_dend_side)[1]
  .Object@row_dend_param$width = row_dend_width + ht_opt$DENDROGRAM_PADDING
  .Object@row_dend_param$show = show_row_dend
  .Object@row_dend_param$gp = check_gp(row_dend_gp)
  .Object@row_dend_param$reorder = row_dend_reorder
  .Object@row_order_list = list()
  if (is.null(row_order)) {
    .Object@row_order = seq_len(nrow(matrix))
  }
  else {
    if (is.character(row_order)) {
      row_order = structure(seq_len(nrow(matrix)), names = rownames(matrix))[row_order]
    }
    if (any(is.na(row_order))) {
      stop_wrap("`row_order` should not contain NA values.")
    }
    if (length(row_order) != nrow(matrix)) {
      stop_wrap("length of `row_order` should be same as the number of marix rows.")
    }
    .Object@row_order = row_order
  }
  .Object@row_dend_param$cluster_slices = cluster_row_slices
  if (missing(cluster_columns) && !missing(column_order)) {
    cluster_columns = FALSE
  }
  if (is.logical(cluster_columns)) {
    if (!cluster_columns) {
      column_dend_height = unit(0, "mm")
      show_column_dend = FALSE
    }
    .Object@column_dend_param$cluster = cluster_columns
  }
  else if (inherits(cluster_columns, "dendrogram") || inherits(cluster_columns, 
                                                               "hclust")) {
    .Object@column_dend_param$obj = cluster_columns
    .Object@column_dend_param$cluster = TRUE
  }
  else if (inherits(cluster_columns, "function")) {
    .Object@column_dend_param$fun = cluster_columns
    .Object@column_dend_param$cluster = TRUE
  }
  else {
    oe = try(cluster_columns <- as.dendrogram(cluster_columns), 
             silent = TRUE)
    if (!inherits(oe, "try-error")) {
      .Object@column_dend_param$obj = cluster_columns
      .Object@column_dend_param$cluster = TRUE
    }
    else {
      stop_wrap("`cluster_columns` should be a logical value, a clustering function or a clustering object.")
    }
  }
  if (!show_column_dend) {
    column_dend_height = unit(0, "mm")
  }
  .Object@column_dend_list = list()
  .Object@column_dend_param$distance = clustering_distance_columns
  .Object@column_dend_param$method = clustering_method_columns
  .Object@column_dend_param$side = match.arg(column_dend_side)[1]
  .Object@column_dend_param$height = column_dend_height + 
    ht_opt$DENDROGRAM_PADDING
  .Object@column_dend_param$show = show_column_dend
  .Object@column_dend_param$gp = check_gp(column_dend_gp)
  .Object@column_dend_param$reorder = column_dend_reorder
  if (is.null(column_order)) {
    .Object@column_order = seq_len(ncol(matrix))
  }
  else {
    if (is.character(column_order)) {
      column_order = structure(seq_len(ncol(matrix)), 
                               names = colnames(matrix))[column_order]
    }
    if (any(is.na(column_order))) {
      stop_wrap("`column_order` should not contain NA values.")
    }
    if (length(column_order) != ncol(matrix)) {
      stop_wrap("length of `column_order` should be same as the number of marix columns")
    }
    .Object@column_order = column_order
  }
  .Object@column_dend_param$cluster_slices = cluster_column_slices
  .Object@top_annotation = top_annotation
  if (is.null(top_annotation)) {
    .Object@top_annotation_param$height = unit(0, "mm")
  }
  else {
    if (inherits(top_annotation, "AnnotationFunction")) {
      stop_wrap("The annotation function `anno_*()` should be put inside `HeatmapAnnotation()`.")
    }
    .Object@top_annotation_param$height = height(top_annotation) + 
      ht_opt$COLUMN_ANNO_PADDING
  }
  if (!is.null(top_annotation)) {
    if (length(top_annotation) > 0) {
      if (!.Object@top_annotation@which == "column") {
        stop_wrap("`which` in `top_annotation` should only be `column`.")
      }
    }
    nb = nobs(top_annotation)
    if (!is.na(nb)) {
      if (nb != ncol(.Object@matrix)) {
        stop_wrap("number of observations in top annotation should be as same as ncol of the matrix.")
      }
    }
  }
  if (!is.null(top_annotation)) {
    validate_anno_names_with_matrix(matrix, top_annotation, 
                                    "column")
  }
  .Object@bottom_annotation = bottom_annotation
  if (is.null(bottom_annotation)) {
    .Object@bottom_annotation_param$height = unit(0, "mm")
  }
  else {
    if (inherits(bottom_annotation, "AnnotationFunction")) {
      stop_wrap("The annotation function `anno_*()` should be put inside `HeatmapAnnotation()`.")
    }
    .Object@bottom_annotation_param$height = height(bottom_annotation) + 
      ht_opt$COLUMN_ANNO_PADDING
  }
  if (!is.null(bottom_annotation)) {
    if (length(bottom_annotation) > 0) {
      if (!.Object@bottom_annotation@which == "column") {
        stop_wrap("`which` in `bottom_annotation` should only be `column`.")
      }
    }
    nb = nobs(bottom_annotation)
    if (!is.na(nb)) {
      if (nb != ncol(.Object@matrix)) {
        stop_wrap("number of observations in bottom annotation should be as same as ncol of the matrix.")
      }
    }
  }
  if (!is.null(bottom_annotation)) {
    validate_anno_names_with_matrix(matrix, bottom_annotation, 
                                    "column")
  }
  .Object@left_annotation = left_annotation
  if (is.null(left_annotation)) {
    .Object@left_annotation_param$width = unit(0, "mm")
  }
  else {
    if (inherits(left_annotation, "AnnotationFunction")) {
      stop_wrap("The annotation function `anno_*()` should be put inside `rowAnnotation()`.")
    }
    .Object@left_annotation_param$width = width(left_annotation) + 
      ht_opt$ROW_ANNO_PADDING
  }
  if (!is.null(left_annotation)) {
    if (length(left_annotation) > 0) {
      if (!.Object@left_annotation@which == "row") {
        stop_wrap("`which` in `left_annotation` should only be `row`, or consider using `rowAnnotation()`.")
      }
    }
    nb = nobs(left_annotation)
    if (!is.na(nb)) {
      if (nb != nrow(.Object@matrix)) {
        stop_wrap("number of observations in left annotation should be same as nrow of the matrix.")
      }
    }
  }
  if (!is.null(left_annotation)) {
    validate_anno_names_with_matrix(matrix, left_annotation, 
                                    "row")
  }
  .Object@right_annotation = right_annotation
  if (is.null(right_annotation)) {
    .Object@right_annotation_param$width = unit(0, "mm")
  }
  else {
    if (inherits(right_annotation, "AnnotationFunction")) {
      stop_wrap("The annotation function `anno_*()` should be put inside `rowAnnotation()`.")
    }
    .Object@right_annotation_param$width = width(right_annotation) + 
      ht_opt$ROW_ANNO_PADDING
  }
  if (!is.null(right_annotation)) {
    if (length(right_annotation) > 0) {
      if (!.Object@right_annotation@which == "row") {
        stop_wrap("`which` in `right_annotation` should only be `row`, or consider using `rowAnnotation()`.")
      }
    }
    nb = nobs(right_annotation)
    if (!is.na(nb)) {
      if (nb != nrow(.Object@matrix)) {
        stop_wrap("number of observations in right annotation should be same as nrow of the matrix.")
      }
    }
  }
  if (!is.null(right_annotation)) {
    validate_anno_names_with_matrix(matrix, right_annotation, 
                                    "row")
  }
  .Object@layout = list(layout_size = list(column_title_top_height = unit(0, 
                                                                          "mm"), column_dend_top_height = unit(0, "mm"), column_anno_top_height = unit(0, 
                                                                                                                                                       "mm"), column_names_top_height = unit(0, "mm"), column_title_bottom_height = unit(0, 
                                                                                                                                                                                                                                         "mm"), column_dend_bottom_height = unit(0, "mm"), column_anno_bottom_height = unit(0, 
                                                                                                                                                                                                                                                                                                                            "mm"), column_names_bottom_height = unit(0, "mm"), row_title_left_width = unit(0, 
                                                                                                                                                                                                                                                                                                                                                                                                           "mm"), row_dend_left_width = unit(0, "mm"), row_names_left_width = unit(0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "mm"), row_dend_right_width = unit(0, "mm"), row_names_right_width = unit(0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "mm"), row_title_right_width = unit(0, "mm"), row_anno_left_width = unit(0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "mm"), row_anno_right_width = unit(0, "mm")), layout_index = matrix(nrow = 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ncol = 2), graphic_fun_list = list(), initialized = FALSE)
  if (is.null(width)) {
    width = unit(ncol(matrix), "null")
  }
  else if (is.numeric(width) && !inherits(width, "unit")) {
    width = unit(width, "null")
  }
  else if (!inherits(width, "unit")) {
    stop_wrap("`width` should be a `unit` object or a single number.")
  }
  if (is.null(height)) {
    height = unit(nrow(matrix), "null")
  }
  else if (is.numeric(height) && !inherits(height, "unit")) {
    height = unit(height, "null")
  }
  else if (!inherits(height, "unit")) {
    stop_wrap("`height` should be a `unit` object or a single number.")
  }
  if (!is.null(width) && !is.null(heatmap_width)) {
    if (is_abs_unit(width) && is_abs_unit(heatmap_width)) {
      stop_wrap("`heatmap_width` and `width` should not all be the absolute units.")
    }
  }
  if (!is.null(height) && !is.null(heatmap_height)) {
    if (is_abs_unit(height) && is_abs_unit(heatmap_height)) {
      stop_wrap("`heatmap_height` and `height` should not all be the absolute units.")
    }
  }
  if (is.null(use_raster)) {
    if (nrow(matrix) > 2000 && ncol(matrix) > 10) {
      use_raster = TRUE
      if (ht_opt$message) {
        message_wrap("`use_raster` is automatically set to TRUE for a matrix with more than 2000 rows. You can control `use_raster` argument by explicitly setting TRUE/FALSE to it.\n\nSet `ht_opt$message = FALSE` to turn off this message.")
      }
    }
    else if (ncol(matrix) > 2000 && nrow(matrix) > 10) {
      use_raster = TRUE
      if (ht_opt$message) {
        message_wrap("`use_raster` is automatically set to TRUE for a matrix with more than 2000 columns You can control `use_raster` argument by explicitly setting TRUE/FALSE to it.\n\nSet `ht_opt$message = FALSE` to turn off this message.")
      }
    }
    else {
      use_raster = FALSE
    }
  }
  if (use_raster) {
    if (missing(raster_by_magick)) {
      if (!raster_by_magick) {
        if (ht_opt$message) {
          message_wrap("'magick' package is suggested to install to give better rasterization.\n\nSet `ht_opt$message = FALSE` to turn off this message.")
        }
      }
    }
  }
  .Object@matrix_param$width = width
  .Object@matrix_param$height = height
  .Object@heatmap_param$width = heatmap_width
  .Object@heatmap_param$height = heatmap_height
  .Object@heatmap_param$show_heatmap_legend = show_heatmap_legend
  .Object@heatmap_param$use_raster = use_raster
  .Object@heatmap_param$raster_device = match.arg(raster_device)[1]
  .Object@heatmap_param$raster_quality = raster_quality
  .Object@heatmap_param$raster_device_param = raster_device_param
  .Object@heatmap_param$raster_resize_mat = raster_resize_mat
  .Object@heatmap_param$raster_by_magick = raster_by_magick
  .Object@heatmap_param$raster_magick_filter = raster_magick_filter
  .Object@heatmap_param$verbose = verbose
  .Object@heatmap_param$post_fun = post_fun
  .Object@heatmap_param$calling_env = parent.frame()
  .Object@heatmap_param$show_parent_dend_line = show_parent_dend_line
  if (nrow(matrix) == 0) {
    .Object@matrix_param$height = unit(0, "mm")
  }
  if (ncol(matrix) == 0) {
    .Object@matrix_param$width = unit(0, "mm")
  }
  return(.Object)
}
