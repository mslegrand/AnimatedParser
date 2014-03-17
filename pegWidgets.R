#source("gDraw.R")
#source("drawTree.R")
#source("parseExp.R")
source("treeBuilder.R")

library(grid)
plot.new()
grid.newpage()
source("resizeTxt.R")

#print("*********************hello*************************************")

#begin drawing
colorChoices<-c("lightblue","lightgreen", "green, yellow", "red", "orange", "pink", "grey", "burlywood", "gold", "lightgoldenrod",
                "lightsteelblue", "greenyellow" )

nRows<-18
nCols<-10
bWidth=1/nCols
sbWidth=bWidth/4
bHeight=1/nRows
#interSpX<-.1*bWidth #since box width is reduced by .9

bOffSetx<-0 #*bWidth 
bOffSety<-0 #6*bHeight

#bColOffset<-0
#bRowOffset<-0

offSetStack<-matrix(c(2,2,0,0),2,2)

col2x<- function(col){ bOffSetx +bWidth*(col + offSetStack[1,2] )}
row2y<-function(row) { bOffSety + bHeight*(row + offSetStack[1,1])}

push.offSet<-function(rows, cols){
  v<-c(rows,cols)+offSetStack[1,]
  offSetStack<<-rbind(v,offSetStack)
  offSetStack[1,]
}

pop.offSet<-function(){
  if(nrow(offSetStack)>2){
    offSetStack<<-offSetStack[-1,]
  }
  offSetStack[1,]
}


vp0At<-function(row, col, w=1, h=1){
  x<-col2x(col)
  y<-row2y(row)
  viewport(x,y,width=bWidth*w, height=bHeight*h)  
}

g.box0At<-function(row, col, id, w=1, h=1, fill="lightgray"){ #used by OR
  vp0<-vp0At(row, col, w, h)
 # tg<-resizingTextGrob(txt,name="tx")
  rg<-rectGrob(width=1,height=1, name="bg", hjust=0, vjust=0)  
  gTree(children=gList(rg), name=id, gp=gpar(fill=fill), vp=vp0) 
}

vp2At<-function(row, col, w=1, h=1){
  x<-col2x(col) + .5*bWidth
  y<-row2y(row) + (.5)*bHeight
  viewport(x,y,width=bWidth*(w-1), height=bHeight*.2)
}

g.box2At<-function(row, col, id, w=1, h=1, txt="txt", fill="lightblue"){ #used by ! & * + ?
  vp2<-vp2At(row, col, w, h)
  rg<-rectGrob(width=1,height=1, name="bg", hjust=0) #, vjust=0)
  gTree(children=gList(rg), name=id, gp=gpar(fill=fill), vp=vp2) 
}

vp1At<-function(row, col){
  x<-col2x(col) + (.5)*bWidth
  y<-row2y(row) + (.5)*bHeight
  viewport(x,y,width=bWidth, height=bHeight)
}

g.box1At<-function(row, col, id, txt="b1", fill="steelblue", color="black"){ #used by atom, ident
  vp1<-vp1At(row, col)
  rg<-rectGrob(width=.8, height=.6, name="rg")
  tg<-resizingTextGrob(txt)
  gTree(children=gList(rg,tg), name=id, gp=gpar(fill=fill, col=color), vp=vp1)  
}

g.boxR1At<-function(row, col, id, txt="b1", fill="steelblue", color="black"){ #used by call
  vp1<-vp1At(row, col)
  rg<-roundrectGrob(width=.8, height=.6, name="rg")
  tg<-resizingTextGrob(txt)
  gTree(children=gList(rg,tg), name=id, gp=gpar(fill=fill, col=color), vp=vp1)  
}

g.circe1At<-function(row, col, id, txt="t1", fill="greenyellow"){
#   x<-col2x(col) + (.5)*bWidth
#   y<-row2y(row) + (.5)*bHeight
#   vp1<-viewport(x,y,width=bWidth, height=bHeight)
  vp1<-vp1At(row, col)
  id<-paste0(id,"-c1")  
  rg<-circleGrob(x=.5,y=.5, r=.4, name="rg")
  tg<-resizingTextGrob(txt)
  gTree(children=gList(rg,tg),name=id, gp=gpar(fill=fill), vp=vp1)  
}

g.arrow<-function(row, col, id,  len, txt="a<-'b'", fill="orange"){
  id<-paste0(id,"-a")
  x1<- col2x(col) + (.5)*bWidth
  y1<- row2y(row) + (.5)*bHeight  #from
  y2<- y1 -bHeight*len #row2y(row+len) #to
  vp1<-viewport(x1,y2,width=.8*bWidth, height=bHeight)
  #pushViewport(vp1)
  #rg<-rectGrob(width=.9,height=.7, name="rg")
  ba<-.8
  er<-1-ba
  wu<-.6
  yt<-len+ba + .05
  yb<-len+er-.05 #-.05
  # xu<-c(.1, .1, wu, wu )
  # yu<-c(ba, yt, yt, ba )  
  
  xu<-c(.0, .0, 1. ,  1. , wu, wu )
  yu<-c(ba, yt, yt,   yb,  yb, ba )
  #   x<-c( .1, .8,  .8, 1.0, .8, .8,  .1 )
  #   y<-c( .8, .8, 1.0, 0.5,  0, .2, .2 )
  x<-c( ba,  ba, 1.0, ba, ba,  .4 ) #starts at arrow, bending up, end at lower left
  y<-c( ba,  1,  wu,  0,  er, er )
  x<-c(xu,x)
  y<-c(yu,y)
  tg<-resizingTextGrob(txt, x=.5, y=len+.5)
  
  pg<-polygonGrob(x,y)
  gTree(children=gList(pg,tg),   gp=gpar(fill=fill), vp=vp1)  
}

g.drawGridCoord<-function(){
  #grid.rect(0,0,1,1, hjust=0, vjust=0, gp=gpar(fill="wheat"))
  grid.rect(0,0,1,1, hjust=0, vjust=0, gp=gpar(fill="cornsilk"))
  # draw grid for reference
  maxRow<-1/bHeight
  nmaxRow<- -maxRow
  maxCol<-1/bWidth
  nmaxCol<- - maxCol
  #color<-"wheat4"
  color<-"cornsilk3"
  for(i in nmaxRow:maxRow ){
    y<-row2y(i) #+.2*bHeight
    ln<-linesGrob(c(0,1), c(y,y), gp=gpar(col=color))
    grid.draw(ln)  
  }
  for(j in nmaxCol:maxCol ){
    x<-col2x(j) #+.5*bWidth
    ln<-linesGrob(c(x,x), c(0,1) , gp=gpar(col=color) )
    grid.draw(ln)
  }  
}

g.ATOM<-function(node.id, tree){
    node<-tree[[node.id]]
    row<-node$coord[["row"]]
    col<-node$coord[["col"]]
    id<-node$id
    fill<-switch(node$status,
                 U="steelblue",
                 C="yellow",
                 OK="greenyellow",
                 BAD="red"
    )
    g.box1At(row,col, id, txt=node$val,  fill=fill)
}


g.IDENT<-function(node.id, tree){
    node<-tree[[node.id]]
    row<-node$coord[["row"]]
    col<-node$coord[["col"]]
    id<- node$id
    g.box1At(row,col,id, txt=node$val,  color="orange")
}

g.xxxfix<-function(node.id, txt, f1, f2, pre, tree){
  node<-tree[[node.id]]
  row<-node$coord[["row"]]
  col<-node$coord[["col"]]
  w<-node$coord[["w"]]
  h<-node$coord[["h"]]
  id<-node.id
  bid<-paste0(node.id,"-b1")
  b1<-g.box2At(row, col, bid, w=w, h=1, , fill=f1)
  if(pre==TRUE){
    c1<-g.circe1At(row, col, id, txt=txt,  fill=f2)
  } else {
    c1<-g.circe1At(row, col+w-1, id, txt=txt, fill=f2)
  } 
  gTree(children=gList(b1,c1), id=id)   
}

g.prefix<-function(node.id, txt, f1, f2, tree){
  g.xxxfix(node.id, txt, f1, f2, TRUE, tree)
#   node<-tree[[node.id]]
#   row<-node$coord[["row"]]
#   col<-node$coord[["col"]]
#   w<-node$coord[["w"]]
#   h<-node$coord[["h"]]
#   id<-node.id
#   bid<-paste0(node.id,"-b1")
#   b1<-g.box2At(row, col, bid, w=w, h=1, , fill=f1)
#   c1<-g.circe1At(row, col, id, txt=txt,  fill=f2)
#   gTree(children=gList(b1,c1), id=id)   
}

g.AND<-function(node.id, tree){
  g.prefix(node.id, "&", "green", "cyan", tree)
}

g.NOT<-function(node.id, tree){
  g.prefix(node.id, "!", "red", "hotpink", tree)
}

g.suffix<-function(node.id, txt, f1, f2, tree){
  g.xxxfix(node.id, txt, f1, f2, FALSE, tree)
#   node<-tree[[node.id]]
#   row<-node$coord[["row"]]
#   col<-node$coord[["col"]]
#   w<-node$coord[["w"]]
#   h<-node$coord[["h"]]
#   id<-node.id
#   bid<-paste0(node.id,"-b1")  
#   b1<-g.box2At(row, col, bid, w=w, h=1, , fill="green")
#   c1<-g.circe1At(row, col+w-1, id, txt=txt, fill="cyan")
#   gTree(children=gList(b1,c1), id=id) 
}

g.QUES<-function(node.id,tree){
  g.suffix(node.id, "?", "green", "cyan", tree)
}

g.STAR<-function(node.id,tree){
  g.suffix(node.id, "*", "green", "cyan", tree)
}

g.PLUS<-function(node.id,tree){
  g.suffix(node.id, "?", "green", "cyan", tree)
}

#---------------------------stopped coding here  

g.SEQ<-function(node.id, tree){
    #  
    #   box<-gTree(id=id)     
    #   #loop thru kids and add their grobs
    #   children<-node$children
    TRUE
}

g.OR<-function(node.id, tree){
  #browser()
  node<-tree[[node.id]]
  col<-node$coord[["col"]]
  w<-node$coord[["w"]]
  id<-node$id
  kids<-node$children
  gt<-gTree(name=node.id)    
  for(kid.id in kids){
    kid<-tree[[kid.id]]
    row<-kid$coord[["row"]]
    #kid.id<-paste0("G",kid.id)
    #cat("kid.id=",kid.id,"\n")
    kid.grob<-g.box0At(row,col, kid.id, w=w, h=1, fill="pink")
    gt<-addGrob(gt, kid.grob)
  }
  gt
}
#---------------DRAWS----------------------------------------

drawAtoms<-function(tree){
  tmp<-ls(tree, pattern=glob2rx("ATM-*"))
  for(id in tmp){
    grob<-g.ATOM(id, tree)
    grid.draw(grob)
  }
}



drawIdent<-function(tree){
  tmp<-ls(tree, pattern=glob2rx("IDENT-*"))
  for(id in tmp){
    grob<-g.IDENT(id,tree)
    grid.draw(grob)
  }
}


drawNOT<-function(tree){
  tmp<-ls(tree, pattern=glob2rx("NOT-*"))
  for(id in tmp){
    grob<-g.NOT(id,tree)
    grid.draw(grob)
  }
}


drawAND<-function(tree){
  tmp<-ls(tree, pattern=glob2rx("AND-*"))
  for(id in tmp){
    grob<-g.AND(id,tree)
    grid.draw(grob)
  }
}

drawQUES<-function(tree){
  tmp<-ls(tree, pattern=glob2rx("QUES-*"))
  for(id in tmp){
    grob<-g.QUES(id,tree)
    grid.draw(grob)
  }
}

drawSTAR<-function(tree){
  tmp<-ls(tree, pattern=glob2rx("STAR-*"))
  for(id in tmp){
    grob<-g.STAR(id,tree)
    grid.draw(grob)
  }
}

drawPLUS<-function(tree){
  tmp<-ls(tree, pattern=glob2rx("PLUS-*"))
  for(id in tmp){
    grob<-g.PLUS(id,tree)
    grid.draw(grob)
  }
}

drawOR<-function(tree){
  tmp<-ls(tree, pattern=glob2rx("GOR-*"))
  #browser()
  for(id in tmp){
    grob<-g.OR(id, tree)
    grid.draw(grob)
  } 
}


#---MOVES---------------------

ATOM.move<-function(node){
  row<-node$coord[["row"]]
  col<-node$coord[["col"]]
  id<-node$id
  grid.edit( id, vp=vp1At(row,col))
}


IDENT.move<-function(node){
  row<-node$coord[["row"]]
  col<-node$coord[["col"]]
  id<-node$id
  grid.edit( id, vp=vp1At(row,col))
}

# OR.move<-function(node, tree){
#   # we move the children!
#   #node<-tree[[node.id]]
#   node.id<-node$id
#   w<-node$coord[["w"]]
#   kids<-node$children
#   for(kid.id in kids){
#     kid<-tree[[kid.id]]
#     row<-node$coord[["row"]]
#     col<-node$coord[["col"]]
#     path<-gPath(node.id,kid.id)
#     grid.edit(path, vp=vp0At(row,col,w,1)) #may want gpath here gPath("xa", "ticks")
#   }
# }

OR.move<-function(node, tree){
  # we move the children!
  #node<-tree[[node.id]]
  node.id<-node$id
  w<-node$coord[["w"]]
  col<-node$coord[["col"]]
  kids<-node$children
  for(kid.id in kids){
    kid<-tree[[kid.id]]
    row<-kid$coord[["row"]]
    path<-gPath(node.id,kid.id)
    #cat("path=",path," row=",row,"\n")
    grid.edit(path, vp=vp0At(row,col,w,1)) #may want gpath here gPath("xa", "ticks")
  }
}

suffix.move<-function(node){
  row<-node$coord[["row"]]
  col<-node$coord[["col"]]
  w<-node$coord[["w"]]
  h<-node$coord[["h"]]
  id<-node$id
  grid.edit( id, vp=vp2At(row,col,w,1))
}

prefix.move<-function(node){
  row<-node$coord[["row"]]
  col<-node$coord[["col"]]
  w<-node$coord[["w"]]
  id<-node$id
  grid.edit( id, vp=vp2At(row,col,w,1))
}




#--Code to star drawing-----------------------

drawPegTree<-function(tree, row=2, col=3){
#   plot.new()
#   grid.newpage()
  seekViewport("upper")
  g.drawGridCoord()
  drawOR(tree)
  drawNOT(tree)
  drawAND(tree)
  drawQUES(tree)
  drawSTAR(tree)
  drawPLUS(tree)
  drawIdent(tree)
  drawAtoms(tree) 
  upViewport(0)
}

drawLower<-function(){
  seekViewport("lower")
  #grid.rect(x=.5, y=.5, width=1, height=1, gp=gpar(fill="khaki3") ) 
  #grid.rect(x=.5, y=.5, width=1, height=1, gp=gpar(fill="cornflowerblue") ) 
  grid.rect(x=.5, y=.5, width=1, height=1, gp=gpar(fill="lightblue3") ) 
  #grid.rect(x=.5, y=.8, width=1, height=.3,  gp=gpar(fill="white") )
  seekViewport("text")
  #grid.rect(x=.5,y=.5, width=1, height=1, gp=gpar(fill="pink"))
  upViewport(0)
}

# text<-"hello there way, I hope it is all good!"
# txt<-strsplit(text,"")[[1]]
# txt<-paste(txt, collapse="       ")
# # 
# for(p in 1:nchar(txt)){
#   drawTextIO(txt,p)->move.text
#   Sys.sleep(.1)
# }

