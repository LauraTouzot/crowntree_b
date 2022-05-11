# utils

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


fun.boxplot.breaks <-  function(x,y,Nclass=25,add=TRUE,...){
  vec_remove<- !is.na(x) & !is.na(y)
  x <- x[vec_remove]
  y <- y[vec_remove]
  breakss <- seq(min(x),max(x),length=Nclass+1)
  x.cut <- cut(x,breakss)
  mid.points <- breakss[-(Nclass+1)]+abs(breakss[2]-breakss[1])/2
  if(add){boxplot(y~x.cut,at=mid.points,outline=FALSE,add=add,names=NULL,
                  col=scales::alpha(grey(1-table(x.cut)/max(table(x.cut))), 0.3),
                  boxwex=(breakss[2]-breakss[1])*0.9,
                  pars=list(xaxt='n',axes=FALSE),...)
  }else{
    boxplot(y~x.cut,at=mid.points,outline=FALSE,names=round(mid.points,0),
            boxwex=(breakss[2]-breakss[1])*0.9,
            col=scales::alpha(grey(1-table(x.cut)/max(table(x.cut))), 0.3),...)
  }
  
}

densCols2 <- function(x, y) {
  as.vector(
    suppressWarnings(
      densCols(x, y,
               colramp = colorRampPalette(c("yellow", "red")))))
}

