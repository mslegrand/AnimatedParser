library(grid)
library(scales)

resizingTextGrob <- function(...)
{
  grob(tg=textGrob(...), cl="resizingTextGrob")
}

drawDetails.resizingTextGrob <- function(x, recording=TRUE)
{
  grid.draw(x$tg)
}
#http://ryouready.wordpress.com/2012/08/01/creating-a-text-grob-that-automatically-adjusts-to-viewport-size/#more-854

# g <- resizingTextGrob("test 1")
# grid.draw(g)
# grid.text("test 2", y=.4)

preDrawDetails.resizingTextGrob <- function(x)
{
  h <- convertHeight(unit(10, "snpc"), "mm", valueOnly=TRUE)
  fs <- rescale(h, to=c(18, 7), from=c(120, 20))
  pushViewport(viewport(gp = gpar(fontsize = fs)))
}

postDrawDetails.resizingTextGrob <- function(x){
  popViewport()  
}

# g <- resizingTextGrob(label="test 1")
# grid.draw(g)
# grid.text("test 2", y=.4)
