source("pegWidgets.R")

source("textDisplayNoPak.R")
source("geomtree.R")
source("rule.grob.R")

step<-function(){
  cat ("Press [enter] to continue")
  line <- readline() 
}

apply_rule<-function(ruleSet, root.rule, input.txt){
  
parsedRuleDefs<-list()

  
pardef<-function(parsedRuleDefs, rule.id, rule.def){
  value(pegR[["GSEQ"]](rule.def))->parsed.rule.def
  parsedRuleDefs[[rule.id]]<-parsed.rule.def
  parsedRuleDefs
}

get.new.instance<-function(forest, rule.id){
  res<-parsedRuleDefs[[rule.id]]  
  instance.id<-add.geom.tree(forest , res, rule.id)
  instance.id
}
#____________________________________________

#used only by OR.node
delete.first.child<-function(node, forest){
  if(length(node$children>1)){
    kids<-node$children
    #get first kid
    first.id<-kids[1]
    f.kid<-forest[[first.id]]
    fh<-f.kid$coord['h'] #how much the stack will decrease
    h<-node$coord['h']
    node$coord["h"]<-h-fh #height of the new stack
    #delete the first row
    rec.delete.nodes(first.id, forest)  #remove the first child    
    node$children<-kids[-1] #take it off the childrens list
    grid.remove(gPath(node$id, first.id))
    for(kid.id in node$children){
      vertical.move(kid.id, forest, -fh) 
    }
    forest[[node$id]]<-node #restore the forest
    if(length(node$children)>1){
      OR.move(node, forest)
    } else {
      grid.remove(node$id, redraw=TRUE)
    }    
    #be happy!!      
  }
  node
}


vertical.move<-function(node.id, forest, d.rows){
  node<-forest[[node.id]]
  #do children
  children<-node$children
  if(!is.null(children)){
    for(k.id in children){
      vertical.move(k.id, forest, d.rows)
    }
  }    
  #do self
  #1. decrement row coord
  node$coord[["row"]]<-node$coord[["row"]] + d.rows
  forest[[node.id]]<-node
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

#removes this node and all children from both forest and display
rec.delete.nodes<-function(node.id, forest){ 
  node<-forest[[node.id]]
  children<-node$children
  for( kid.id in children){
    rec.delete.nodes(kid.id, forest)
  }
  if(class(node) %in% c("ATOM.node", "IDENT.node","NOT.node","AND.node", "QUES.node", "STAR.node", "PLUS.node", "OR.node")
  ){
    grid.remove(node.id, redraw=TRUE)
  }
  rm(list=node.id, envir=forest)
}



#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

eval.forest<-function(text, forest, id=forest$root.id,  pos=1){       #, text, pos){
  
  update.status.children<-function( forest,id,status=status){
    update.status( forest,id,status)
    for(kid.id in forest[[id]]$children){
      update.status.children( forest, kid.id, status)
    }
  }
    
  update.status<-function( forest,id,status=status){
    node<-forest[[id]]
    node$status<-status
    forest[[id]]<-node
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
  
  
  eval.seq<-function(text, forest, id, pos){
    #do each child
     node<-forest[[id]]
    ok<-T
    consumed<-0
    for(kid.id in node$children){
      res.kid<-eval.forest(text, forest, kid.id, pos+consumed )
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
        update.status.children( forest, kid.id, "BAD" )
        #update.status( forest, kid.id, "BAD" )
      }
      step()
    }
    res<-list(ok=ok,  consumed=consumed)
  }
 
  eval.atom<-function(text, forest, id, pos){
    #consider : set color to yellow and redraw
    update.status( forest,id, status="C")
    step()
    #answer phase
    node<-forest[[id]]
    ok<-ifelse(node$val==substr(text,pos,pos-1+nchar(node$val)), T, F)
    status<-ifelse(ok, "OK", "BAD")  
    consumed<-ifelse(ok, nchar(node$val), 0)
    #success :set color to green and redraw
    update.status( forest, id, status=status)
    if(ok){
      text.pos.right(consumed)
    }
    res<-list(ok=ok,  consumed=consumed)
  }
#---------------------


  #used only by OR.node!!!!
  delete.remaining.chidren<-function(node, forest){
    kids<-node$children
    indx<-length(kids)
    for(i in 2:length(kids)){
      kid.id<-kids[i]
      rec.delete.nodes(kid.id, forest)
    }
    node$children<-node$children[1]
    if(!is.null(grid.get(node$id))){
      grid.remove(node$id, redraw=TRUE)
    }    
    #change the class of the node?
    forest[[node$id]]<-node
    node
  }


#--------------------------- 
  eval.or<-function(text, forest, id, pos){
    update.status( forest, id, status="C")
    step()
    node<-forest[[id]]
    consumed<-0
    while(TRUE){
      kid.id<-node$children[1]
      res.kid<-eval.forest(text, forest, kid.id, pos)
      ok<-res.kid$ok
      if(ok==T){ #ok==T
        consumed<- res.kid$consumed
        delete.remaining.chidren(node, forest) #remove all later nodes
        break
      } else { #ok==F
        if(length(node$children)==1){
          text.pos.back(pos)
          break
        }
        node<-delete.first.child( node, forest )
      }      
    }
    update.status( forest, id, status=status)
    res<-list(ok=ok,  consumed=consumed)    
  }

#    eval.not<-function(text, forest, id, pos){
#      node<-forest[[id]]
#      update.status( forest, id, 'C')
#      ok<-T
#      consumed<-0
#      for(kid.id in node$children){
#        res.kid<-eval.forest(text, forest, kid.id, pos+consumed )
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
#      update.status.children( forest, id, status)
#      res<-list(ok=ok,  consumed=0)     
#    }

  eval.ahead<-function(text, forest, id, pos){
    node<-forest[[id]]
    update.status( forest, id, 'C')
    consumed<-0
    for(kid.id in node$children){
      res.kid<-eval.forest(text, forest, kid.id, pos+consumed )
      ok<-res.kid$ok
      if(ok==T){
        consumed<-consumed + res.kid$consumed
      } else { #(ok=F)
        return(FALSE)
      }
    }
    TRUE
  }

  eval.and<-function(text, forest, id, pos){
    ok<-eval.ahead(text, forest, id, pos)
    status<-ifelse(ok, "OK","BAD")
    update.status.children( forest, id, status)
    text.pos.back(pos)
    res<-list(ok=ok,  consumed=0)         
  }

  eval.not<-function(text, forest, id, pos){
    ok<-eval.ahead(text, forest, id, pos)
    ok<-!ok
    status<-ifelse(ok, "OK","BAD")
    update.status.children( forest, id, status)
    text.pos.back(pos)
    res<-list(ok=ok,  consumed=0)         
  }

  eval.star<-function(text, forest, id, pos){
    node<-forest[[id]]
    update.status( forest, id, 'C')
    consumed<-0
    kid.id=node$children[1]
    more<-TRUE
    while(more){
      res.kid<-eval.forest(text, forest, kid.id, pos+consumed )
      more<-res.kid$ok
      consumed<-consumed+res.kid$consumed
    }
    ok<-TRUE
    status<- "OK"
    update.status.children( forest, id, status)
    res<-list(ok=ok,  consumed=consumed)         
  }
 
  eval.ques<-function(text, forest, id, pos){
    node<-forest[[id]]
    update.status( forest, id, 'C')
    kid.id=node$children[1]
    res.kid<-eval.forest(text, forest, kid.id, pos)
    consumed<-res.kid$consumed
    ok<-TRUE
    status<- "OK"
    update.status.children( forest, id, status)
    res<-list(ok=ok,  consumed=consumed)         
  }

  eval.plus<-function(text, forest, id, pos){
    node<-forest[[id]]
    update.status( forest, id, 'C')
    kid.id=node$children[1]
    res.kid<-eval.forest(text, forest, kid.id, pos)
    ok<-res.kid$ok
    consumed<-res.kid$consumed
    more<-ok
    while(more){
      res.kid<-eval.forest(text, forest, kid.id, pos+consumed )
      more<-res.kid$ok
      consumed<-consumed+res.kid$consumed
    }
    status<-ifelse(ok, "OK","BAD")
    update.status.children( forest, id, status)
    res<-list(ok=ok,  consumed=consumed)         
  }

  eval.ident<-function(text, forest, id, pos){
    #consider : set color to yellow and redraw
    update.status( forest,id, status="C")
    step()
    #answer phase
    node<-forest[[id]]
    # find current expression at position (this woud be in row and col node$coord)
    current.row<-node$coord['row']
    current.col<-node$coord['col']
    # find current rule start pos (assume 1,1)
    # find call.id height (this woud be in  forest[[call.id]]$coord)
    call.symbol<-node$val
    call.instance.id<-get.new.instance(forest, call.symbol)
    call.instance.root.id<-instanceRoot(forest, call.instance.id)
    dy<-current.row+forest[[call.instance.root.id]]$coord[['h']] 
    dx<-0 #1 -current.col
    # find current rule gTree (this would be the root node's value?)
    # current rule name: split at "-IDENT", take first part, that is the gTree name
    tr.name<-strsplit(id,"-IDENT-")[[1]][1]
    # move that rule
    move.rule.to(tr.name,dy,dx)
    #drawarrow
    arrow.id<-paste0("arrow-",call.instance.id)
    len<-dy
    ga<-g.arrow(dy+1, current.col, arrow.id,  len, txt=call.symbol)
    grid.draw(ga)
    # display call rule
    #res<-eval.forest(text.input,  forest , call.instance.root.id) 
    
    #rule.grob<-create.rule.grob(call.instance.root.id, forest)
    rule.grob<-create.rule.grob(call.instance.id, forest)
    draw.rule.grob(rule.grob)
    #move it over
    move.rule.to(call.instance.id, current.row-1, current.col) #don't know why the -1
    # call the rule( i.e get it's root and eval it)
    # 
    step()
    res.call<-eval.forest(text.input,  forest , call.instance.root.id)
    #update arrow status
    ok<-res.call$ok
    arrow.fill<-ifelse(ok, "greenyellow", "red")
    grid.edit(arrow.id, gp=gpar(fill=arrow.fill), redraw = TRUE)
    step()
    #remove call
    rec.delete.nodes(call.instance.root.id, forest)    
    #move rule back
    step()
    #remove arrow 
    grid.remove(gPath(arrow.id))
    
    move.rule.to(tr.name,0,0) #kludge for now
    # update call box.
    #ok<-res.call$ok
    consumed<-res.call$consumed
    status<-ifelse(ok, "OK", "BAD")  
    consumed<-ifelse(ok, nchar(node$val), 0)
    #success :set color to green and redraw
    update.status( forest, id, status=status)
    if(ok){
      text.pos.right(consumed)
    }
    res<-list(ok=ok,  consumed=consumed)
  }

  node<-forest[[id]]
  type<-class(node)
  switch(type,
    ATOM.node=eval.atom(text, forest, id, pos),    
    SEQ.node =eval.seq(text, forest, id, pos),
    NOT.node=eval.not(text, forest, id,pos),
    AND.node=eval.and(text, forest, id,pos),
    STAR.node=eval.star(text, forest, id,pos),
    PLUS.node=eval.plus(text, forest, id,pos),
    QUES.node=eval.ques(text, forest, id,pos),
    IDENT.node=eval.ident(text, forest, id, pos),
    OR.node  =eval.or(text, forest, id, pos)
    )
}


parsedRuleDefs<-pardef(parsedRuleDefs, 'A', " 'a' B ")
parsedRuleDefs<-pardef(parsedRuleDefs, 'B', " 'b' 'b' 'b' ")

draw.new()
forest<-new.geom.forest()
drawLower()
text.input<-"ab"
draw.TextPanel(text.input) 
seekViewport("upper")
g.drawGridCoord()

rule.id<-"A"
rule.instance.id<-get.new.instance(forest, rule.id)

#create grob for this rule instance
rule.grob<-create.rule.grob(rule.instance.id, forest)
# draw the grob for this rule instance
draw.rule.grob(rule.grob)

id<-instanceRoot(forest, rule.instance.id)
eval.forest(text.input,  forest , id) 

}

#-------------------------------------------------------------------
# update.consumed<-function(oldePos, newPos){
#   
# }

test.eval.forest<-function(){
  source("textDisplayNoPak.R")
  
  
#   
#   value(pegR[["GSEQ"]](" 'a' / 'b' / 'c' / 'd' 'd' "))->res
#   #value(pegR[["GSEQ"]]("'c'  ! 'a'  'b' "))->res
#   #value(pegR[["GSEQ"]]("'c'? 'e'"))->res
# 
#   draw.new()
#   forest<-build.tree(res)
#   drawPegTree( forest)
#   drawLower()
#   
#   #text.input<-"ceddd"
#   #text.input<-"cbddd"
#   text.input<-"cddd"
#   draw.TextPanel(text.input)
#   
#   eval.forest(text.input, forest)
  
  #---------------------------
  source("textDisplayNoPak.R")
  source("geomtree.R")
  source("rule.grob.R")

#   parsedRuleDefs<-list()
# 
#   pardef<-function(parsedRuleDefs, rule.id, rule.def){
#     value(pegR[["GSEQ"]](rule.def))->parsed.rule.def
#     parsedRuleDefs[[rule.id]]<-parsed.rule.def
#     parsedRuleDefs
#   }
# 
#   get.new.instance<-function(forest, rule.id){
#     res<-parsedRuleDefs[[rule.id]]  
#     instance.id<-add.geom.tree(forest , res, rule.id)
#     instance.id
#   }

#   push.call.stack(current.instance, curRow, curCol, current.node,){
#     
#     stack.item<-list(instance=current.instance,
#                      row=current.Row,
#                      col=current.Col
#                      node=current.node)
#   }
#   
#   pop.call.stack(){
#     
#   }

#   rule.def<-" 'a' / 'b' / 'c' / 'd' 'd' "
#   rule.id<-rule.id<-"A"
#   value(pegR[["GSEQ"]](" 'a' / 'b' / 'c' / 'd' 'd' "))->parsed.rule.def
#   parsedRuleDefs[[rule.id]]<-parsed.rule.def

#parsedRuleDefs<-pardef(parsedRuleDefs, 'A', " 'a' / 'b' / 'c' / 'd' 'd' ")
  parsedRuleDefs<-pardef(parsedRuleDefs, 'A', " 'a' B ")
  parsedRuleDefs<-pardef(parsedRuleDefs, 'B', " 'b' 'b' 'b' ")



  draw.new()
  forest<-new.geom.forest()

  drawLower()
   #text.input<-"cddd"
  text.input<-"abb"
  draw.TextPanel(text.input)  

  #given rule id
  #rule.id<-"A"
  #create parse expression
  #value(pegR[["GSEQ"]](" 'a' / 'b' / 'c' / 'd' 'd' "))->res
  #res<-parsedRuleDefs[['A']]
  #create rule instance inside forest
  #rule.instance.id<-add.geom.tree(forest, res, rule.id)
  rule.id<-"A"
  rule.instance.id<-get.new.instance(forest, rule.id)
  
  #create grob for this rule instance
  rule.grob<-create.rule.grob(rule.instance.id, forest)
  # draw the grob for this rule instance
  draw.rule.grob(rule.grob)

  id<-instanceRoot(forest, rule.instance.id)
  eval.forest(text.input,  forest , id) 


}

#test.eval.forest()

apply_rule(1,2,3)