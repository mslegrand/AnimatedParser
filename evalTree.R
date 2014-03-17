source("pegWidgets.R")

step<-function(){
  cat ("Press [enter] to continue")
  line <- readline() 
}

#used only by OR.node
delete.first.child<-function(node, tree){
  if(length(node$children>1)){
    kids<-node$children
    #get first kid
    first.id<-kids[1]
    f.kid<-tree[[first.id]]
    fh<-f.kid$coord['h'] #how much the stack will decrease
    h<-node$coord['h']
    node$coord["h"]<-h-fh #height of the new stack
    #delete the first row
    rec.delete.nodes(first.id, tree)  #remove the first child    
    node$children<-kids[-1] #take it off the childrens list
    grid.remove(gPath(node$id, first.id))
    for(kid.id in node$children){
      vertical.move(kid.id, tree, -fh) 
    }
    tree[[node$id]]<-node #restore the tree
    if(length(node$children)>1){
      OR.move(node, tree)
    } else {
      grid.remove(node$id, redraw=TRUE)
    }    
    #be happy!!      
  }
  node
}


vertical.move<-function(node.id, tree, d.rows){
  node<-tree[[node.id]]
  #do children
  children<-node$children
  if(!is.null(children)){
    for(k.id in children){
      vertical.move(k.id, tree, d.rows)
    }
  }    
  #do self
  #1. decrement row coord
  node$coord[["row"]]<-node$coord[["row"]] + d.rows
  tree[[node.id]]<-node
  #2. move viewport 
  type<-class(node)
  switch(type,
         ATOM.node=ATOM.move(node),
         IDENT.node=INDENT.move(node),
         PLUS.node=suffix.move(node),
         STAR.node=suffix.move(node),
         QUES.node=suffix.move(node),
         AND.node=prefix.move(node),
         NOT.node=prefix.move(node),
         OR.node=OR.move(node),
         SEQ.node=TRUE      
  )
}

#removes this node and all children from both tree and display
rec.delete.nodes<-function(node.id, tree){ 
  node<-tree[[node.id]]
  children<-node$children
  for( kid.id in children){
    rec.delete.nodes(kid.id, tree)
  }
  if(class(node) %in% c("ATOM.node", "IDENT.node","NOT.node","AND.node", "QUES.node", "STAR.node", "PLUS.node", "OR.node")
  ){
    grid.remove(node.id, redraw=TRUE)
  }
  rm(list=node.id, envir=tree)
}




eval.tree<-function(text, tree, id=tree$root.id, pos=1){ #, text, pos){
  
  update.status.children<-function( tree,id,status=status){
    update.status(tree,id,status)
    for(kid.id in tree[[id]]$children){
      update.status.children(tree, kid.id, status)
    }
  }
    
  update.status<-function( tree,id,status=status){
    node<-tree[[id]]
    node$status<-status
    tree[[id]]<-node
    if(class(node)=="ATOM.node"){
      fill<-switch(node$status,
                            U="steelblue",
                            C="yellow",
                            OK="greenyellow",
                            BAD="red"
      )
      grid.edit(id, gp=gpar(fill=fill), redraw = TRUE)
    }
    if(class(node)=="IDENT.node"){
      fill<-switch(node$status,
                   U="steelblue",
                   C="yellow",
                   OK="greenyellow",
                   BAD="red"
      )
      grid.edit(id, gp=gpar(fill=fill), redraw = TRUE)
    }
    if(class(node) %in% c("NOT.node", "AND.node", "STAR.node", "QUES.node", "PLUS.node")){
      fill<-switch(node$status,
                   U="steelblue",
                   C="yellow",
                   OK="greenyellow",
                   BAD="red"
      )
      idc<-paste0(id,"-c1")
      idb<-paste0(id,"-b1")
      grid.edit(idc, gp=gpar(fill=fill), redraw = TRUE)
      grid.edit(idb, gp=gpar(fill=fill), redraw = TRUE)
    }
    
  }
  
  
  eval.seq<-function(text, tree, id, pos){
    #do each child
     node<-tree[[id]]
    ok<-T
    consumed<-0
    for(kid.id in node$children){
      res.kid<-eval.tree(text, tree, kid.id, pos+consumed )
      ok<-res.kid$ok
      if(is.null(ok)){
        cat("id=",id,"kid.id=",kid.id,"pos+consumed=",pos+consumed,"\n")
        cat("id=",id,"kid.id=",kid.id,"consumed=",consumed,"\n")
        cat("id=",id,"length(node$children)=",length(node$children),"pos=",pos,"\n")
      }
      if(ok==T){
        consumed<-consumed + res.kid$consumed
      } else {
        consumed<-0
        text.pos.back(pos)
        break
      }
    }
    status<-ifelse(ok, "OK","BAD")
    if(status=="BAD"){    
      for(kid.id in node$children){
        update.status.children(tree, kid.id, "BAD" )
        #update.status(tree, kid.id, "BAD" )
      }
      step()
    }
    res<-list(ok=ok,  consumed=consumed)
  }
 
  eval.atom<-function(text, tree, id, pos){
    #consider : set color to yellow and redraw
    update.status(tree,id, status="C")
    step()
    #answer phase
    node<-tree[[id]]
    ok<-ifelse(node$val==substr(text,pos,pos-1+nchar(node$val)), T, F)
    status<-ifelse(ok, "OK", "BAD")  
    consumed<-ifelse(ok, nchar(node$val), 0)
    #success :set color to green and redraw
    update.status(tree, id, status=status)
    if(ok){
      text.pos.right(consumed)
    }
    res<-list(ok=ok,  consumed=consumed)
  }
#---------------------


  #used only by OR.node!!!!
  delete.remaining.chidren<-function(node, tree){
    kids<-node$children
    indx<-length(kids)
    for(i in 2:length(kids)){
      kid.id<-kids[i]
      rec.delete.nodes(kid.id, tree)
    }
    node$children<-node$children[1]
    if(!is.null(grid.get(node$id))){
      grid.remove(node$id, redraw=TRUE)
    }    
    #change the class of the node?
    tree[[node$id]]<-node
    node
  }


#--------------------------- 
  eval.or<-function(text, tree, id, pos){
    update.status(tree, id, status="C")
    step()
    node<-tree[[id]]
    consumed<-0
    while(TRUE){
      kid.id<-node$children[1]
      res.kid<-eval.tree(text, tree, kid.id, pos)
      ok<-res.kid$ok
      if(ok==T){ #ok==T
        consumed<- res.kid$consumed
        delete.remaining.chidren(node, tree) #remove all later nodes
        break
      } else { #ok==F
        if(length(node$children)==1){
          text.pos.back(pos)
          break
        }
        node<-delete.first.child( node, tree )
      }      
    }
    update.status(tree, id, status=status)
    res<-list(ok=ok,  consumed=consumed)    
  }

#    eval.not<-function(text, tree, id, pos){
#      node<-tree[[id]]
#      update.status( tree, id, 'C')
#      ok<-T
#      consumed<-0
#      for(kid.id in node$children){
#        res.kid<-eval.tree(text, tree, kid.id, pos+consumed )
#        ok<-res.kid$ok
#        if(ok==T){
#          consumed<-consumed + res.kid$consumed
#        } else { #(ok=F)
#          #consumed<-0
#          break
#        }
#      }
#      ok<-!ok
#      status<-ifelse(ok, "OK","BAD")
#      update.status.children(tree, id, status)
#      res<-list(ok=ok,  consumed=0)     
#    }

  eval.ahead<-function(text, tree, id, pos){
    node<-tree[[id]]
    update.status( tree, id, 'C')
    consumed<-0
    for(kid.id in node$children){
      res.kid<-eval.tree(text, tree, kid.id, pos+consumed )
      ok<-res.kid$ok
      if(ok==T){
        consumed<-consumed + res.kid$consumed
      } else { #(ok=F)
        return(FALSE)
      }
    }
    TRUE
  }

  eval.and<-function(text, tree, id, pos){
    ok<-eval.ahead(text, tree, id, pos)
    status<-ifelse(ok, "OK","BAD")
    update.status.children(tree, id, status)
    text.pos.back(pos)
    res<-list(ok=ok,  consumed=0)         
  }

  eval.not<-function(text, tree, id, pos){
    ok<-eval.ahead(text, tree, id, pos)
    ok<-!ok
    status<-ifelse(ok, "OK","BAD")
    update.status.children(tree, id, status)
    text.pos.back(pos)
    res<-list(ok=ok,  consumed=0)         
  }

  eval.star<-function(text, tree, id, pos){
    node<-tree[[id]]
    update.status( tree, id, 'C')
    consumed<-0
    kid.id=node$children[1]
    more<-TRUE
    while(more){
      res.kid<-eval.tree(text, tree, kid.id, pos+consumed )
      more<-res.kid$ok
      consumed<-consumed+res.kid$consumed
    }
    ok<-TRUE
    status<- "OK"
    update.status.children(tree, id, status)
    res<-list(ok=ok,  consumed=consumed)         
  }
 
  eval.ques<-function(text, tree, id, pos){
    node<-tree[[id]]
    update.status( tree, id, 'C')
    kid.id=node$children[1]
    res.kid<-eval.tree(text, tree, kid.id, pos)
    consumed<-res.kid$consumed
    ok<-TRUE
    status<- "OK"
    update.status.children(tree, id, status)
    res<-list(ok=ok,  consumed=consumed)         
  }

  eval.plus<-function(text, tree, id, pos){
    node<-tree[[id]]
    update.status( tree, id, 'C')
    kid.id=node$children[1]
    res.kid<-eval.tree(text, tree, kid.id, pos)
    ok<-res.kid$ok
    consumed<-res.kid$consumed
    more<-ok
    while(more){
      res.kid<-eval.tree(text, tree, kid.id, pos+consumed )
      more<-res.kid$ok
      consumed<-consumed+res.kid$consumed
    }
    status<-ifelse(ok, "OK","BAD")
    update.status.children(tree, id, status)
    res<-list(ok=ok,  consumed=consumed)         
  }

  node<-tree[[id]]
  type<-class(node)
  switch(type,
    ATOM.node=eval.atom(text, tree, id, pos),    
    SEQ.node =eval.seq(text, tree, id, pos),
    NOT.node=eval.not(text, tree, id,pos),
    AND.node=eval.and(text, tree, id,pos),
    STAR.node=eval.star(text, tree, id,pos),
    PLUS.node=eval.plus(text, tree, id,pos),
    QUES.node=eval.ques(text, tree, id,pos),
    OR.node  =eval.or(text, tree, id, pos)
    )
}



# update.consumed<-function(oldePos, newPos){
#   
# }

test.eval.tree<-function(){
  source("textDisplayNoPak.R")
  
  
  
  value(pegR[["GSEQ"]](" 'a' / 'b' / 'c' / 'd' 'd' "))->res
  #value(pegR[["GSEQ"]]("'c'  ! 'a'  'b' "))->res
  #value(pegR[["GSEQ"]]("'c'? 'e'"))->res

  draw.new()
  tree<-build.tree(res)
  drawPegTree(tree)
  drawLower()
  
  #text.input<-"ceddd"
  #text.input<-"cbddd"
  text.input<-"cddd"
  draw.TextPanel(text.input)
  
  eval.tree(text.input, tree)
  
}

test.eval.tree()
