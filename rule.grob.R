#---------------add.S----------------------------------------

add.Atoms<-function(rule.id, geom.tree, rule.grob){
  tmp<-ls(geom.tree, pattern=paste0(rule.id,"-ATM-*"))
  for(id in tmp){
    grob<-g.ATOM(id, geom.tree)
    rule.grob<-addGrob(rule.grob, grob)
  }
  rule.grob
}



add.Ident<-function(rule.id, geom.tree, rule.grob){
  tmp<-ls(geom.tree, pattern=paste0(rule.id,"-IDENT-*"))
  for(id in tmp){
    grob<-g.IDENT(id,geom.tree)
    rule.grob<-addGrob(rule.grob, grob)
  }
  rule.grob
}


add.NOT<-function(rule.id, geom.tree, rule.grob){
  tmp<-ls(geom.tree, pattern=paste0(rule.id,"-NOT-*"))
  for(id in tmp){
    grob<-g.NOT(id,geom.tree)
    rule.grob<-addGrob(rule.grob, grob)
  }
  rule.grob
}


add.AND<-function(rule.id, geom.tree, rule.grob){
  tmp<-ls(geom.tree, pattern=paste0(rule.id,"-AND-*"))
  for(id in tmp){
    grob<-g.AND(id,geom.tree)
    rule.grob<-addGrob(rule.grob, grob)
  }
  rule.grob
}

add.QUES<-function(rule.id, geom.tree, rule.grob){
  tmp<-ls(geom.tree, pattern=paste0(rule.id,"-QUES-*"))
  for(id in tmp){
    grob<-g.QUES(id,geom.tree)
    rule.grob<-addGrob(rule.grob, grob)
  }
  rule.grob
}

add.STAR<-function(rule.id, geom.tree, rule.grob){
  tmp<-ls(geom.tree, pattern=paste0(rule.id,"-STAR-*"))
  for(id in tmp){
    grob<-g.STAR(id,geom.tree)
    rule.grob<-addGrob(rule.grob, grob)
  }
  rule.grob
}

add.PLUS<-function(rule.id, geom.tree, rule.grob){
  tmp<-ls(geom.tree, pattern=paste0(rule.id,"-PLUS-*"))
  for(id in tmp){
    grob<-g.PLUS(id,geom.tree)
    rule.grob<-addGrob(rule.grob, grob)
  }
  rule.grob
}

add.OR<-function(rule.id, geom.tree, rule.grob){
  tmp<-ls(geom.tree, pattern=paste0(rule.id,"-GOR-*"))
  #browser()
  for(id in tmp){
    grob<-g.OR(id, geom.tree)
    rule.grob<-addGrob(rule.grob, grob)
  } 
  rule.grob
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

# OR.move<-function(node, geom.tree){
#   # we move the children!
#   #node<-geom.tree[[node.id]]
#   node.id<-node$id
#   w<-node$coord[["w"]]
#   kids<-node$children
#   for(kid.id in kids){
#     kid<-geom.tree[[kid.id]]
#     row<-node$coord[["row"]]
#     col<-node$coord[["col"]]
#     path<-gPath(node.id,kid.id)
#     grid.edit(path, vp=vp0At(row,col,w,1)) #may want gpath here gPath("xa", "ticks")
#   }
# }

OR.move<-function(node, geom.tree){
  # we move the children!
  #node<-geom.tree[[node.id]]
  node.id<-node$id
  w<-node$coord[["w"]]
  col<-node$coord[["col"]]
  kids<-node$children
  for(kid.id in kids){
    kid<-geom.tree[[kid.id]]
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




#--Code to star add.ing-----------------------

create.rule.grob<-function(rule.id, geom.tree, row=0, col=0){
  #   plot.new()
  #   grid.newpage()
  seekViewport("upper")
 #g.add.GridCoord()
 x<-.5+col2x(col)
 y<-.5+row2y(row)
 vp<-viewport(x=x,y=y)
 
  rule.grob<-gTree(name=rule.id, vp=vp)
  rule.grob<-add.OR(rule.id, geom.tree, rule.grob)
  rule.grob<-add.NOT(rule.id, geom.tree, rule.grob)
  rule.grob<-add.AND(rule.id, geom.tree, rule.grob)
  rule.grob<-add.QUES(rule.id, geom.tree, rule.grob)
  rule.grob<-add.STAR(rule.id, geom.tree, rule.grob)
  rule.grob<-add.PLUS(rule.id, geom.tree, rule.grob)
  rule.grob<-add.Ident(rule.id, geom.tree, rule.grob)
  rule.grob<-add.Atoms(rule.id, geom.tree, rule.grob) 
  #upViewport(0)
  rule.grob
}


draw.rule.grob<-function(rule.grob, row=0, col=0){
  seekViewport("upper")
  #g.drawGridCoord()
  grid.draw(rule.grob)
}

move.rule.to<-function(rule.id, row=0, col=0){
  seekViewport("upper")
  x<-.5+col2x(col)
  y<-.5+row2y(row)
  vp<-viewport(x=x,y=y)
  grid.edit(rule.id, vp=vp)
}

test.rule.grob<-function(){
 # source("parseExp.R")
  source("textDisplayNoPak.R")
  source("geomtree.R")
  draw.new()
  #tree<-build.tree(res, "A")
  #geom.tree<-build.geomtree(res, rule.id="A")
  
  forest<-new.geom.forest()
  #rule.id<-paste0("A",id.new.rule.no())
  #given rule id
  rule.id<-"A"
  #create parse expression
  value(pegR[["GSEQ"]](" 'a' / 'b' / 'c' / 'd' 'd' "))->res
  #create rule instance inside forest
  rule.instance.id<-add.geom.tree(forest, res, rule.id)
  #create grob for this rule instance
  rule.grob<-create.rule.grob(rule.instance.id, forest)
  #draw the grob for this rule instance
  draw.rule.grob(rule.grob)
  #drawLower()
  
}

#test.rule.grob()

