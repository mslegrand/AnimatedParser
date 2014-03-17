

counter.new<-function(txt){
  count<-0
  inc<-function(){
    count<<-count+1
    paste0(txt,count)
  }
  inc
}

build.tree<-function(l, rule.id=""){ # "GOR-"=>paste0(rule.id,"_GOR-")

tree<-new.env()


id.new.GOR<-  counter.new("GOR-")
id.new.ATM<-  counter.new("ATM-")
id.new.R<-    counter.new("R-")
id.new.NOT<-  counter.new("NOT-")
id.new.AND<-  counter.new("AND-")
id.new.QUES<- counter.new("QUES-")
id.new.STAR<- counter.new("STAR-")
id.new.PLUS<- counter.new("PLUS-")
id.new.CALL<- counter.new("CALL-")
id.new.IDENT<-counter.new("IDENT-")

#' tree.builder should look like
#' node consists of val, coord, id (but node is a list)
#' so children are in val
tree.builder<-function(l, row=1, col=1 , type="R"){   
  tree.builderGOR<-function(l, row, col){
    w<-0
    h<-0
    id<-id.new.GOR()
    children<-c()
    for(R in l){
      tree.builder(R, row+h, col, type="R")->kid.id
      kid.coord<-tree[[kid.id]]$coord
      h<-h + kid.coord[['h']]
      w<-max(w, kid.coord[['w']])
      children<-c(children, kid.id)
    }
    coord<-c(row=row, col=col, w=w, h=h)
    lres<-list(children=children, coord=coord, id=id, status="U" )
    #  cat("************************************************GORNODE id=",id,"\n")
    class(lres)<-"OR.node"
    tree[[id]]<-lres
    id
  }
  
  tree.builderForward<-function(l,row,col){
    w<-1
    h<-0
    children<-c()
    for(i in 1:length(l)){
      type<-names(l)[i]
      ll<-l[[i]]
      tree.builder(ll, row, col+w, type=type)->kid.id
      kid.coord<-tree[[kid.id]]$coord
      h<-max(h,kid.coord[['h']])
      w<-w + kid.coord[['w']]
      children<-c(children, kid.id)
    }
    coord<-c(row=row, col=col, w=w, h=h)    
    lres<-list(children=children, coord=coord)
  }
  
  tree.builderNOT<-function(l, row, col){
    id<-id.new.NOT() 
    lres<-tree.builderForward(l,row,col)
    lres$id<-id
    class(lres)<-"NOT.node"
    tree[[id]]<-lres
    id
  }
  
  tree.builderAND<-function(l, row, col){
    id<-id.new.AND() 
    lres<-tree.builderForward(l,row,col)    
    lres$id=id
    class(lres)<-"AND.node"
    tree[[id]]<-lres
    id
  }
  
  tree.builderBackward<-function(l,row,col){
    w<-0
    h<-0
    children<-c()
    for(i in 1:length(l)){
      type<-names(l)[i]
      ll<-l[[i]]
      tree.builder(ll, row, col+w, type=type)->kid.id
      #cat("tree.builderR: lres$coord=", lres$coord, "\n")
      kid.coord<-tree[[kid.id]]$coord
      h<-max(h,kid.coord[['h']])
      w<-w + kid.coord[['w']]
      children<-c(children, kid.id)
    }
    coord<-c(row=row, col=col, w=w+1, h=h)    
    lres<-list(children=children, coord=coord)
  }
  
  
  tree.builderQUES<-function(l,row,col){
    id<-id.new.QUES() 
    lres<-tree.builderBackward(l,row,col)    
    lres$id=id
    class(lres)<-"QUES.node"
    tree[[id]]<-lres
    id
  }
  
  tree.builderSTAR<-function(l,row,col){
    id<-id.new.STAR() 
    lres<-tree.builderBackward(l,row,col)    
    lres$id=id
    class(lres)<-"STAR.node"
    tree[[id]]<-lres
    id
  }
  
  tree.builderPLUS<-function(l,row,col){
    id<-id.new.PLUS() 
    lres<-tree.builderBackward(l,row,col)    
    lres$id=id
    class(lres)<-"PLUS.node"
    tree[[id]]<-lres
    id
  }
    
  tree.builderR<-function(l, row, col){
    w<-0
    h<-0
    id<-id.new.R()
    #browser()
    children<-c()
    for(i in 1:length(l)){
      type<-names(l)[i]
      ll<-l[[i]]
      tree.builder(ll, row, col+w, type=type)->kid.id
      # cat("tree.builderR: lres$coord=", lres$coord, "\n")
      kid.coord<-tree[[kid.id]]$coord
      h<-max(h, kid.coord[['h']])
      w<- w + kid.coord[['w']]
      children<-c(children, kid.id)
    }
    coord<-c(row=row, col=col, w=w, h=h)
    #names(coord)<-c("row", "col", "w", "h")
    #class(coord)="coord"
    lres<-list(children=children, coord=coord, id=id, status="U" )
    class(lres)<-"SEQ.node"
    tree[[id]]<-lres
    id
  } 
  
  
  tree.builderAtom<-function(l, row, col){
    id<-id.new.ATM()
    coord<-c(row=row, col=col, w=1, h=1)
    lres<-list(children=NULL, val=l, coord=coord, id=id, status="U" )
    class(lres)<-"ATOM.node"
    tree[[id]]<-lres
    id
  }

  tree.builderIDENT<-function(l,row, col){
    id<-id.new.IDENT()
    coord<-c(row=row, col=col, w=1, h=1)
    lres<-list(children=NULL, val=l, coord=coord, id=id, status="U" )
    class(lres)<-"IDENT.node"
    tree[[id]]<-lres
    id
  }
  
  default<-function(l, row, col){
    id<-id.new.ATM()
    coord<-c(row=row, col=col, w=1, h=1)
    stop("hit default")
    lres<-list(val=l, coord=coord, id=id, status="U" )
    class(lres)<-"Unkwn"
    tree[[id]]<-lres
    id
  }
  
  switch(type,
         R=tree.builderR(l, row, col),
         GOR=tree.builderGOR(l, row, col),
         atom=tree.builderAtom(l, row, col),
         NOT=tree.builderNOT(l, row, col),
         AND=tree.builderAND(l, row, col),
         QUES=tree.builderQUES(l,row,col),
         STAR=tree.builderSTAR(l,row,col),
         PLUS=tree.builderPLUS(l,row,col),
         LITERAL=tree.builderAtom(l,row,col),
         IDENTIFIER=tree.builderIDENT(l,row,col),
         default(l,row,col)
  )
}

root.id<-tree.builder(l)
#tree<-as.list(tree) #return as list?
tree[["root.id"]]<-root.id
tree
}

# value(pegR[["GSEQ"]](" 'd' 'c'*"))->res
# 
# tree<-build.tree(res)
