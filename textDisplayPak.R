source("pegWidgets.R")


draw.new<-function(){
  plot.new()
  grid.newpage()
  vpTop<-viewport(.5, 1, width=1, height=5/6, just="top", clip="on", name="upper")
  pushViewport(vpTop)
  upViewport(0)
  vpBot<-viewport(.5, 0, width=1, height=1/6, just="bottom", clip="on", name="lower")
  pushViewport(vpBot)
  vpTxt<-viewport(x=.5, y=.8, width=1, height=.2, name="text")
  pushViewport(vpTxt)
  #grid.rect(gp=gpar(fill="red"),vp=vpBot)  
  upViewport(0)
}


g.pak<-function( bk.grnd="lightblue3"){ #lightblue3
  seekViewport("text")
  #convert height into some kind of fixed units
  #grid.rect(.5,.5,.5,.5, gp=gpar(fill="black"))
  h<-1.5
  hin<-convertHeight(unit(h,"npc"), "in")
  wh<-convertWidth(hin, "npc")
  vpc<-viewport(wh, .5, wh,  h,  clip =TRUE)
  #pushViewport(vpc)
  #grid.rect(.5,.5,1,1, gp=gpar(col="red"))
  vppac<-viewport(.5,.5, 1, 1, name="pac", angle=45)
  #pushViewport(vppac)
  #grid.rect(.5, .5, 1.5,  1.5, gp=gpar(col="red"))
  #   head<-grid.circle(x=.5,y=.5, r=.5, gp=gpar(fill="yellow"), name="pac-head")
  #   mouth<-grid.rect(x=1, y=0, width=1, height=1, ,gp=gpar(fill=bk.grnd, col=bk.grnd), name="pac-mouth")  
  head<-circleGrob(x=.5,y=.5, r=.5, gp=gpar(fill="yellow"), name="pac-head")
  mouth<-rectGrob(x=1, y=0, width=1, height=1, ,gp=gpar(fill=bk.grnd, col=bk.grnd), name="pac-mouth",vp=vppac)  
  #mouth<-grid.rect(x=.5, y=.5, width=.5, height=.5, ,gp=gpar(fill=bk.grnd, col=bk.grnd), name="pac-mouth") 
  #vp<-vpStack()
  pt<-gTree(children=gList(head, mouth), name="pac-m", vp=vpc) 
  #grid.draw(pt)
  upViewport(0)
  #seekViewport("text")
  #grid.draw(pt)
  #seekViewport("pac")
  #grid.draw(pt)
  pt
}


text.pos.right<-function(count=1){
  seekViewport("text")
  next.pos<-text.pos+count
  length(childNames(grid.get(gPath("node.TEXT"))))->text.len
  
  if( 0 < next.pos & next.pos <=text.len +1){ #the next pos is x+pos 
    while(text.pos<next.pos){
      grid.edit(gPath("node.TEXT" ,paste0("txt-",text.pos)),  gp=gpar(alpha=.4))
      text.pos<-text.pos+1
    }
    
    #slide pac    
#     grid.edit(gPath("node.TEXT",paste0("lg-",text.pos) ,paste0("txt-",text.pos)),  gp=gpar(alpha=.4))
#     grid.edit(gPath("node.TEXT",paste0("lg-",text.pos) ,paste0("bck-",text.pos)),  gp=gpar(alpha=0))
    #seekViewport("text")
    x<-  1.0*(1-next.pos)*wh 
    vpc<-viewport(wh*next.pos, .5, wh,  1.5,  clip =TRUE)
    grid.edit(gPath("pac-m"), vp=vpc)
  text.pos<<-next.pos
  } else {    
  }
}

text.pos.left<-function(count=1){
  seekViewport("text")
  next.pos<-text.pos-count
  length(childNames(grid.get(gPath("node.TEXT"))))->text.len
  if( 0 < next.pos & next.pos <=text.len +1){ #the next pos is x+pos 
    #grid.edit(gPath("node.TEXT" ,paste0("txt-",text.pos)),  gp=gpar(alpha=.4))
    #grid.edit(gPath("node.TEXT",paste0("lg-",text.pos) ,paste0("txt-",text.pos)),  gp=gpar(alpha=.4))
    #grid.edit(gPath("node.TEXT",paste0("lg-",text.pos) ,paste0("bck-",text.pos)),  gp=gpar(alpha=0))
    #slide pac
    #seekViewport("text")
    
    x<-  1.0*(1-next.pos)*wh 
    vpc<-viewport(wh*next.pos, .5, wh,  1.5,  clip =TRUE)
    grid.edit(gPath("pac-m"), vp=vpc)
    
    while(text.pos>next.pos){
      text.pos<-text.pos-1
      grid.edit(gPath("node.TEXT" ,paste0("txt-",text.pos)),  gp=gpar(alpha=1))      
#       grid.edit(gPath("node.TEXT" ,paste0("txt-",text.pos)),  gp=gpar(alpha=1))      
    }
    #grid.edit(gPath("node.TEXT",paste0("lg-",text.pos) ,paste0("txt-",text.pos)),  gp=gpar(alpha=1))
    text.pos<<-next.pos
  } else {    
  }
}

#------------------------------------------

# id.new.TEXT<-counter.new("TXT-")
text.pos<-1
wh<-0

draw.TextPanel<-function( text.input, pos){
  seekViewport("text")
  xa<-strsplit(text.input,"")[[1]]
  
  h<-1.5
  hin<-convertHeight(unit(h,"npc"), "in")
  wh<<-convertWidth(hin, "npc")  
  w<-grobWidth(textGrob('MM'))
  
  gtr<-gTree(name="node.TEXT", vp=viewport( x=wh, just="left"))  
  for(i in 1:length(xa)){
    #x<-unit(.5,"npc") + wh*.25 -.5*w + 1.5*(i)*w # convertWidth(unit(.5, "snpc")) + (i+1)*w
    #x<-unit(.5,"npc") + wh*.5 -.75*w + 1.5*(i)*w # convertWidth(unit(.5, "snpc")) + (i+1)*w
    x<-  1.0*(i)*wh   # convertWidth(unit(.5, "snpc")) + (i+1)*w
    alpha=1 #ifelse(i<9, 1, .5)
    tg<-textGrob(xa[i] , x=x, name=paste0("txt-",i),  gp=gpar(col="black", alpha=alpha ))
    rg<-rectGrob(x=x, y=.5, width=.5*wh, height=1, name=paste0("bck-",i), gp=gpar(fill="white", alpha=alpha, col="white") )
    #bg<-gTree(name=paste0("lg-",i), children=gList(rg,tg))
    #gtr<-addGrob(gtr, bg )
    gtr<-addGrob(gtr, tg )
  }
  #gtr<-grid.edit("node.TEXT", vp=viewport(x=.5, just="left"))
  #gPath("node.TEXT",names[i]), vp=vpt)
  grid.draw(gtr)
  
  pak<-g.pak()
  vp<-seekViewport("text")
  grid.draw(pak)
  text.pos<<-1  
}


test.Pak<-function(){
  #print("hi")
  #----test code---------
  #value(pegR[["GSEQ"]]("'x'/ 'y'"))->res
  #value(pegR[["GSEQ"]]('! x '))->res
  value(pegR[["GSEQ"]]("'a' / ( 'b' ('e'/ 'c') ) 'd' 'd' (x/y) !('a' b / & 'c')'"))->res
  
  
  
  #value(pegR[["GSEQ"]]('!(a /! b)'))->res
  # value(pegR[["GSEQ"]]("  'k' / 'b' " ))->res
  # value(pegR[["GSEQ"]]("  A / B " ))->res
  # 
  # value(pegR[["GSEQ"]]("  K / 'b' " ))->res
  # value(pegR[["GSEQ"]]("  'k' / B " ))->res
  #value(pegR[["GSEQ"]]("  'k' / 'b' " ))->res
  
  # value(pegR[["GSEQ"]]("  'k'* " ))->res
  #m
  
  draw.new()
  tree<-build.tree(res)
  drawPegTree(tree)
  drawLower()
  
  text.input<-"hello there way, I hope it is all good!"
  draw.TextPanel(text.input)
  
  
  for(i in 1:10){
    text.pos.right()
    Sys.sleep(1)
  }
  
  tmp<-readline("press to continue
                ")
  for(i in 1:10){
    text.pos.left()
    Sys.sleep(1)
  }
}