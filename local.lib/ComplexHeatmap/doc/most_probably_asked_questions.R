## ---- echo = FALSE, message = FALSE-------------------------------------------
library(markdown)
options(markdown.HTML.options = c(options('markdown.HTML.options')[[1]], "toc"))

## ---- message = FALSE, echo = FALSE-------------------------------------------
library(ComplexHeatmap)

## ---- eval = FALSE------------------------------------------------------------
#  default_axis_param("column")
#  default_axis_param("row")

## ---- eval = FALSE------------------------------------------------------------
#  pushViewport(viewport(layout = grid.layout(...)))
#  pushViewport(viewport(layout.pos.row = ..., layout.pos.col = ...))
#  draw(ht, newpage = FALSE) # or draw(ht_list, newpage = FALSE)
#  popViewport()
#  ...

## ---- eval = FALSE------------------------------------------------------------
#  ht_grob = grid.grabExpr(draw(ht, ...))
#  
#  pushViewport(viewport(layout = grid.layout(...)))
#  pushViewport(viewport(layout.pos.row = ..., layout.pos.col = ...))
#  grid.draw(ht_grob)
#  popViewport()
#  ...

## -----------------------------------------------------------------------------
m = matrix(rnorm(1000*10), nr = 1000)
hc = hclust(dist(m))
group = cutree(hc, k = 6)
Heatmap(m, cluster_rows = cluster_within_group(t(m), group), 
	row_split = 6, border = TRUE) # it would be better if also set row_split

## -----------------------------------------------------------------------------
m = matrix(rnorm(100), 10)

ht = Heatmap(m, name = "foo", 
	row_dend_width = unit(4, "cm"),
	column_dend_height = unit(4, "cm")
)
draw(ht, padding = unit(c(15, 2, 2, 2), "mm"))
decorate_column_dend("foo", {
	grid.yaxis()
})
decorate_row_dend("foo", {
	vp = current.viewport()
	xscale = vp$xscale
	grid.xaxis(at = xscale[2] - 0:5, label = 0:5)
})

## -----------------------------------------------------------------------------
hc = hclust(dist(matrix(rnorm(100), 10)))
Heatmap(matrix(nc = 0, nr = 10), cluster_rows = hc, 
	right_annotation = rowAnnotation(
		foo = anno_points(1:10),
		sth = 1:10,
		bar = anno_barplot(1:10)),
	row_split = 2)

