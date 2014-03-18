

counter.new<-function(txt){
  count<-0
  inc<-function(){
    count<<-count+1
    paste0(txt,count)
  }
  inc
}

instance.id.name.new<-function(){
  count<-0
  inc<-function(txt){
    count<<-count+1
    paste0(txt,"-",count)
  }
  inc
}

id.new.rule.no<- counter.new("-")

instanceRoot<-function(forest, rule.instance.id){
  id<-paste0(rule.instance.id,"-ROOT.ID") 
  forest[[id]]
}



new.geom.forest<-function(rule.id){
  forest<-new.env()
  forest$.new.instance.id.name<-instance.id.name.new()
  
  
  
  forest
}

add.geom.tree<-function(forest, l, rule.id=""){ # "GOR-"=>paste0(rule.id,"_GOR-") 
  #tree<-new.env() 
  rule.instance.id<- forest$.new.instance.id.name(rule.id) 
  id.new.GOR<-  counter.new(paste0(rule.instance.id,"-GOR-"))
  id.new.ATM<-  counter.new(paste0(rule.instance.id,"-ATM-"))
  id.new.R<-    counter.new(paste0(rule.instance.id,"-SEQ-"))
  id.new.NOT<-  counter.new(paste0(rule.instance.id,"-NOT-"))
  id.new.AND<-  counter.new(paste0(rule.instance.id,"-AND-"))
  id.new.QUES<- counter.new(paste0(rule.instance.id,"-QUES-"))
  id.new.STAR<- counter.new(paste0(rule.instance.id,"-STAR-"))
  id.new.PLUS<- counter.new(paste0(rule.instance.id,"-PLUS-"))
  id.new.CALL<- counter.new(paste0(rule.instance.id,"-CALL-"))
  id.new.IDENT<-counter.new(paste0(rule.instance.id,"-IDENT-"))
  
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
        kid.coord<-forest[[kid.id]]$coord
        h<-h + kid.coord[['h']]
        w<-max(w, kid.coord[['w']])
        children<-c(children, kid.id)
      }
      coord<-c(row=row, col=col, w=w, h=h)
      lres<-list(children=children, coord=coord, id=id, status="U" )
      #  cat("************************************************GORNODE id=",id,"\n")
      class(lres)<-"OR.node"
      forest[[id]]<-lres
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
        kid.coord<-forest[[kid.id]]$coord
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
      forest[[id]]<-lres
      id
    }
    
    tree.builderAND<-function(l, row, col){
      id<-id.new.AND() 
      lres<-tree.builderForward(l,row,col)    
      lres$id=id
      class(lres)<-"AND.node"
      forest[[id]]<-lres
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
        kid.coord<-forest[[kid.id]]$coord
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
      forest[[id]]<-lres
      id
    }
    
    tree.builderSTAR<-function(l,row,col){
      id<-id.new.STAR() 
      lres<-tree.builderBackward(l,row,col)    
      lres$id=id
      class(lres)<-"STAR.node"
      forest[[id]]<-lres
      id
    }
    
    tree.builderPLUS<-function(l,row,col){
      id<-id.new.PLUS() 
      lres<-tree.builderBackward(l,row,col)    
      lres$id=id
      class(lres)<-"PLUS.node"
      forest[[id]]<-lres
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
        kid.coord<-forest[[kid.id]]$coord
        h<-max(h, kid.coord[['h']])
        w<- w + kid.coord[['w']]
        children<-c(children, kid.id)
      }
      coord<-c(row=row, col=col, w=w, h=h)
      #names(coord)<-c("row", "col", "w", "h")
      #class(coord)="coord"
      lres<-list(children=children, coord=coord, id=id, status="U" )
      class(lres)<-"SEQ.node"
      forest[[id]]<-lres
      id
    } 
    
    
    tree.builderAtom<-function(l, row, col){
      id<-id.new.ATM()
      coord<-c(row=row, col=col, w=1, h=1)
      lres<-list(children=NULL, val=l, coord=coord, id=id, status="U" )
      class(lres)<-"ATOM.node"
      forest[[id]]<-lres
      id
    }
    
    tree.builderIDENT<-function(l,row, col){
      id<-id.new.IDENT()
      coord<-c(row=row, col=col, w=1, h=1)
      lres<-list(children=NULL, val=l, coord=coord, id=id, status="U" )
      class(lres)<-"IDENT.node"
      forest[[id]]<-lres
      id
    }
    
    default<-function(l, row, col){
      id<-id.new.ATM()
      coord<-c(row=row, col=col, w=1, h=1)
      stop("hit default")
      lres<-list(val=l, coord=coord, id=id, status="U" )
      class(lres)<-"Unkwn"
      forest[[id]]<-lres
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
  forest[[paste0(rule.instance.id,"-ROOT.ID")]]<-root.id
  #forest[["root.id"]]<-root.id
  #tree
  rule.instance.id #rule instance id
}

# value(pegR[["GSEQ"]](" 'd' 'c'*"))->res
# 
# tree<-build.tree(res)
