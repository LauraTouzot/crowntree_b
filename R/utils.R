densCols2 <- function(x, y) {
as.vector(
    suppressWarnings(
               densCols(x, y,
                        colramp = colorRampPalette(c("yellow", "red")))))
}

category_to_logical <- function(x, trueval) {
  x[x == ""] <- NA
  x == trueval
}

change_column_names <- function(dat, table) {
  i <- match(names(dat), table$var_in)
  j <- !is.na(i) & !is.na(table$var_out[i])
  names(dat)[j] <- table$var_out[i[j]]
  dat[,j, drop=FALSE]
}

change_column_names_file <- function(dat, table_file) {
  change_column_names(dat, read.csv(table_file, stringsAsFactors=FALSE))
}

render_md_as_html <- function(filename) {
  rmarkdown::render(filename, "html_document", quiet=TRUE)
}


# Returns up to 80 unique, nice colors, generated using
# http://tools.medialab.sciences-po.fr/iwanthue/
# Starts repeating after 80
nice_colors<-function(n=80){
  cols<-rep(c("#75954F","#D455E9","#E34423","#4CAAE1","#5DE737","#DC9B94",
    "#DC3788","#E0A732","#67D4C1","#5F75E2","#1A3125","#65E689","#A8313C",
    "#8D6F96","#5F3819","#D8CFE4","#BDE640","#DAD799","#D981DD","#61AD34",
    "#B8784B","#892870","#445662","#493670","#3CA374","#E56C7F","#5F978F",
    "#BAE684","#DB732A","#7148A8","#867927","#918C68","#98A730","#DDA5D2",
    "#456C9C","#2B5024","#E4D742","#D3CAB6","#946661","#9B66E3","#AA3BA2",
    "#A98FE1","#9AD3E8","#5F8FE0","#DF3565","#D5AC81","#6AE4AE","#652326",
    "#575640","#2D6659","#26294A","#DA66AB","#E24849","#4A58A3","#9F3A59",
    "#71E764","#CF7A99","#3B7A24","#AA9FA9","#DD39C0","#604458","#C7C568",
    "#98A6DA","#DDAB5F","#96341B","#AED9A8","#55DBE7","#57B15C","#B9E0D5",
    "#638294","#D16F5E","#504E1A","#342724","#64916A","#975EA8","#9D641E",
    "#59A2BB","#7A3660","#64C32A", "#451431"),
            ceiling(n/80))
  cols[seq_len(n)]
}

make.transparent <- function(col, opacity=0.5) {
  tmp <- col2rgb(col)/255
  rgb(tmp[1,], tmp[2,], tmp[3,], alpha=opacity)
}

## Position label at a fractional x/y position on a plot
label <- function(px, py, lab, ..., adj=c(0, 1), log.y=FALSE, log.x=FALSE) {
  u <- par("usr")
  x <- u[1] + px*(u[2]-u[1])
  y <- u[3] + py*(u[4]-u[3])

  if(log.x)
    x <- 10^x

  if(log.y)
    y <- 10^y

  text(x,y,lab, adj=adj,...)
}

plot.bin2d <- function(x, y, xlab, ylab){
require(ggplot2)
data <- data.frame(x = x, y = y)
c <- ggplot(data, aes(x, y))+
     stat_bin2d( na.rm=TRUE)+
     scale_fill_gradientn(colours=topo.colors(10),
                          name = "Count log",na.value=NA,trans = 'log') +
     scale_x_continuous(limits = diff(range(data$x)) * c(-0.2, 0.2) +
                                     range(data$x)) +
     coord_cartesian(xlim = range(pretty(data$x))) +
     theme_bw() +
     theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())+
      theme(legend.position="none")+
      xlab(xlab) + ylab(ylab)
return(c)
}


fun.plot.colors.density <- function(x,y,...){
mat   <- cbind(x,y)
data <- as.data.frame(mat)
colors.dens  <- densCols(mat)
plot(x,y, col=colors.dens, pch=20,...)
}

fun.points.colors.density <- function(x,y,...){
mat   <- cbind(x,y)
data <- as.data.frame(mat)
colors.dens  <- densCols(mat)
points(x,y, col=colors.dens, pch=20,...)
}

fun.boxplot.breaks <-  function(x,y,Nclass=25,add=TRUE,...){
vec_remove<- !is.na(x) & !is.na(y)
x <- x[vec_remove]
y <- y[vec_remove]
breakss <- seq(min(x),max(x),length=Nclass+1)
x.cut <- cut(x,breakss)
mid.points <- breakss[-(Nclass+1)]+abs(breakss[2]-breakss[1])/2
if(add){boxplot(y~x.cut,at=mid.points,outline=TRUE,add=add,names=NULL,
                col=grey(1-table(x.cut)/max(table(x.cut))),
                boxwex=(breakss[2]-breakss[1])*0.9,
                pars=list(xaxt='n',axes=FALSE),...)
    }else{
boxplot(y~x.cut,at=mid.points,outline=TRUE,names=round(mid.points,0),
                boxwex=(breakss[2]-breakss[1])*0.9,
                col=grey(1-table(x.cut)/max(table(x.cut))),...)
    }

}

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

ggQQ <- function(resid) # argument: a linear model
{
    y <- quantile(resid, c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope * x[1L]
    p <- ggplot(data.frame(resid = resid), aes(sample=resid)) +
        stat_qq(alpha = 0.5) +
        geom_abline(slope = slope, intercept = int, color="blue")

    return(p)
}
