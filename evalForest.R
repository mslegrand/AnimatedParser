source("pegWidgets.R")

source("textDisplayNoPak.R")
source("geomtree.R")
source("rule.grob.R")

step<-function(){
  cat ("Press [enter] to continue")
  line <- readline() 
}


apply_rule<-function(parsedRuleDefs, root.rule, text.input){
 
  #begin helper functions
  get.new.instance<-function(forest, rule.id){
    res<-parsedRuleDefs[[rule.id]]  
    instance.id<-add.geom.tree(forest , res, rule.id)
    #cat("instance.id=", instance.id, "\n")
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
      #cat("dfc> grid.remove(gPath(", node$id, ",", first.id, "))\n")
      grid.remove(gPath(node$id, first.id))
      for(kid.id in node$children){
        vertical.move(kid.id, forest, -fh) 
      }
      forest[[node$id]]<-node #restore the forest
      if(length(node$children)>1){
        OR.move(node, forest)
      } else {
        #grid.remove(node$id, redraw=TRUE)
        #cat("dfc> edit(",node$id,", gp=gpar(alpha=0)))\n")
        grid.edit(node$id, gp=gpar(alpha=0))
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
    #cat("rdn> node.id=",node.id,"\n")
    node<-forest[[node.id]]
    children<-node$children
    #cat("rnd> children=",children,"\n")
    for( kid.id in children){
      #cat("rdn>kid.id=",kid.id,"\n")
      rec.delete.nodes(kid.id, forest)
    }
    if(class(node) %in% c("ATOM.node", "IDENT.node","NOT.node","AND.node", "QUES.node", "STAR.node", "PLUS.node", "OR.node")
    ){
      #cat("rdn>grid.remove(",node.id,")\n")
      if(!is.null(grid.get(node.id))){
        grid.remove(node.id, redraw=TRUE)
      }
    }
    rm(list=node.id, envir=forest)
  }
  
  instance.offset.stack<-data.frame()
  
  push.instance<-function(instance.id, row, col){
    instance.offset.stack<<-rbind(data.frame(iid=instance.id, row=row, col=col), instance.offset.stack)
  }
  pop.instance<-function(){
    item<-instance.offset.stack[1,]
    instance.offset.stack<<-instance.offset.stack[-1,]
    item
  }
  
  update.instance.offset.stack<-function(){
    for(i in 1:nrow(instance.offset.stack)){
      id<-as.character(instance.offset.stack[i,1])
      row<-instance.offset.stack[i,2]
      col<-instance.offset.stack[i,3]
      move.rule.to(id, row, col)
    }
  }
  
  
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
# main helper function

eval.forest<-function(text, forest, id=forest$root.id,  pos=1){      
  
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
      grid.edit(id, gp=gpar(fill=fill, col="black"), redraw = TRUE)
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
    len<-nchar(node$val)
    ok<-ifelse( (len==0) | (node$val==substr(text,pos,pos-1+len) ), T, F)
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
  delete.remaining.children<-function(node, forest){
    kids<-node$children
    #cat("kids:\n")
    #for(kid in kids){ cat("kid=", kid,", ")}
    #cat("\n")
    indx<-length(kids)
    i<-2
    while(i<=length(kids)){
      kid.id<-kids[i]
      rec.delete.nodes(kid.id, forest) 
      i<-i+1
    }
        
    node$children<-node$children[1]
    
    if(!is.null(grid.get(node$id))){
      #browser(expr=(node$id=="ASTAR-2-GOR-1"))
      #cat("drc2 > grid.remove(", node$id,")\n")
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
        delete.remaining.children(node, forest) #remove all later nodes
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
    #cat("id=",id,"\n")
    update.status( forest, id, status="C")
    step()
    #answer phase
    node<-forest[[id]]
    # find current expression at position (this woud be in row and col node$coord)
    current.node.row<-node$coord['row']
    current.node.col<-node$coord['col']
    # find current rule start pos 
    current.instance.id<-as.character(instance.offset.stack[1,1])
    current.instance.row<-instance.offset.stack[1,2]
    current.instance.col<-instance.offset.stack[1,3]
    
    # compute starting pos of new call instance
    call.instance.row<-current.instance.row
    call.instance.col<-current.instance.col + current.node.col
    
    # find call.id height (this woud be in  forest[[call.id]]$coord)
    call.symbol<-node$val
    call.instance.id<-get.new.instance(forest, call.symbol)
    call.instance.root.id<-instanceRoot(forest, call.instance.id)
    delta.row<-1+forest[[call.instance.root.id]]$coord[['h']] 
        
    #move the row of everybody (except new.call instance) up by delta.row
    instance.offset.stack$row<<-instance.offset.stack$row + delta.row  
    update.instance.offset.stack()
      
    #add arrow to current instance arrow
    arrow.id<-paste0("arrow-",call.instance.id)
    len<-delta.row
    ga<-g.arrow(current.node.row, 
                current.node.col, 
                arrow.id,  delta.row, txt=call.symbol)
    grid.add(gPath(current.instance.id), ga )

    # display call rule
    #first create at the correct location
    rule.grob<-create.rule.grob(call.instance.id, forest, call.instance.row, call.instance.col)
    #add location to instance.offset (via push.instance)
    
    push.instance(call.instance.id, call.instance.row,call.instance.col )
    
    #push.instance(call.instance.id, current.row-1, current.col)
    draw.rule.grob(rule.grob)
    #move it over (or push it and update)
    
    #move.rule.to(call.instance.id, current.row-1, current.col) #don't know why the -1
    
    # call the rule( i.e get it's root and eval it)
    
    step()
    res.call<-eval.forest(text.input,  forest , call.instance.root.id, pos)
    
    #update arrow status
    ok<-res.call$ok
    arrow.fill<-ifelse(ok, "greenyellow", "red")
    grid.edit(arrow.id, gp=gpar(fill=arrow.fill), redraw = TRUE)
    step()
    #remove call
    pop.instance() #to get call.instance off of the instance.offset stack
    #cat("rec.delete.nodes(",call.instance.root.id,",forest)\n where forest = ")
    #cat(ls(forest))
    #cat("\n")
    #cat("grid.ls=", grid.ls(),"\n")
    rec.delete.nodes(call.instance.root.id, forest)  
    
    # update call box.
    #ok<-res.call$ok
    consumed<-res.call$consumed
    status<-ifelse(ok, "OK", "BAD")  
    consumed<-ifelse(ok, nchar(node$val), 0)
    #success :set color to green and redraw
    update.status( forest, id, status=status)
       
    step()
    #remove arrow 
    grid.remove(gPath(current.instance.id, arrow.id))
    #grid.remove(gPath(arrow.id))
    step()
    
    #move instance stack back down up by -delta.row
    instance.offset.stack$row<<-instance.offset.stack$row - delta.row 
    update.instance.offset.stack()
    
    #move.rule.to(tr.name,0,0) #kludge for now 
    #item<-pop.rule.stack() 
    #move everybody back
    
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


draw.new()
forest<-new.geom.forest()
drawLower()
draw.TextPanel(text.input) 
seekViewport("upper")
#g.drawGridCoord()

rule.id<-root.rule
rule.instance.id<-get.new.instance(forest, rule.id)
instance.offset.stack<-data.frame(iid=rule.instance.id, row=0, col=0)

#create grob for this rule instance
rule.grob<-create.rule.grob(rule.instance.id, forest)
# draw the grob for this rule instance
draw.rule.grob(rule.grob)

id<-instanceRoot(forest, rule.instance.id)
res<-eval.forest(text.input,  forest , id)
status<-ifelse(res$ok,"rule accepted","rule rejected")
status
}

pardef<-function(parsedRuleDefs, rule.id, rule.def){
  value(pegR[["GSEQ"]](rule.def))->parsed.rule.def
  parsedRuleDefs[[rule.id]]<-parsed.rule.def
  parsedRuleDefs
}

test.apply.rule<-function(){
  parsedRuleDefs<-list()
#   parsedRuleDefs<-pardef(parsedRuleDefs, 'A', " 'a' B ")
#   parsedRuleDefs<-pardef(parsedRuleDefs, 'B', " 'b' 'b' 'b' ")
#   rule.id<-"A"
 # parsedRuleDefs<-pardef(parsedRuleDefs, 'ASTAR', " 'a' (ASTAR / '') ")
  parsedRuleDefs<-pardef(parsedRuleDefs, 'ASTAR', " 'a' ASTAR / '' ")

  text.input<-"aaabbb"
  rule.id<-"ASTAR"

#   parsedRuleDefs<-pardef(parsedRuleDefs, 'A', "'c' ('a' / 'b') ")
#   rule.id<-"A"
#   text.input<-"cbbb"
  
  apply_rule(parsedRuleDefs, rule.id, text.input)  
}

test.apply.rule()
