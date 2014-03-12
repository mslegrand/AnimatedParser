library(pegr)

# pegR<-new.parser()
# pegR<-pegR + "EXPRESSION <- SEQUENCE  ('/'  SEQUENCE)* "
# pegR<-pegR + "SEQUENCE <- PREFIX*" 
# pegR<-pegR + "PREFIX <- ('&' / '!')?  SUFFIX" 
# pegR<-pegR + "SUFFIX <- PRIMARY  ( '?' / '*' / '+' )?"
# pegR<-pegR + "PRIMARY <-  P1 / P2/ P3"
# pegR<-pegR + "P3 <-  LITERAL / CLASS / '.'"
# pegR<-pegR + "P2 <- '(' EXPRESSION ')'"
# pegR<-pegR + "P1 <- IDENTIFIER +  !'<-' "
# pegR<-pegR + "IDENTIFIER<-IDENTSTART  IDENTCONT*  SPACING"
# pegR<-pegR + "IDENTCONT<-IDENTSTART / [0-9]"
# pegR<-pegR + "IDENTSTART<-[a-z] / [A-Z]"
# pegR<-pegR + "LITERAL <- [a-z]"
# 
# pegR<-pegR + "SPACING<- ' '*"

#( QUOTE1 (!(QUOTE1) + CHAR)*  QUOTE1  SPACING )  / (QUOTE2 !(QUOTE2) + CHAR)*  QUOTE2  SPACING )"

#goal to isolate the or blocks

#input start pos, expressiontxt
#to return: width, height of expression, drawingfunction
#just to get dimensions
# pegR<-new.parser()
# pegR<-pegR + "EXPRESSION <- SEQUENCE  ('/'  SEQUENCE)* "
# pegR<-pegR + "SEQUENCE <- PREFIX*" 
# pegR<-pegR + "PREFIX <- ('&' / '!')?  SUFFIX" 
# pegR<-pegR + "SUFFIX <- PRIMARY  ( '?' / '*' / '+' )?"
#pegR<-pegR + "PRIMARY <-  P1 / P2/ P3"
#pegR<-pegR + "P3 <-  LITERAL /  '.'"
#pegR<-pegR + "P2 <- '(' EXPRESSION ')'"
#pegR<-pegR + "P1 <- IDENTIFIER  !'<-' "
#pegR<-pegR + "IDENTIFIER<-IDENTSTART  IDENTCONT*  SPACING"
#pegR<-pegR + "IDENTCONT<-IDENTSTART / [0-9]"
#pegR<-pegR + "IDENTSTART<-[a-z] / [A-Z]"
#pegR[["LITERAL"]] <- "{list(c(w=1,h=1))}" 

# to get dimension
# pegR[["P3"]] <- "{list(dim=c(w=1,h=1))}" 
# pegR[["P1"]] <- "{list(dim=c(w=1,h=1))}" 
# pegR[["P2"]] <- "{ v[2] }" #get the middle part only
# pegR[["SPACING"]]<- "{list(dim=c(w=0,h=0))}" 
# pegR[["SEQUENCE"]]<-"{}" #max over the h each prefix, sum the widths of all prefixes
# pegR[["EXPRESSION"]] <-"{}" # max over each width of each sequence, sum over all heights
# pegR[["PREFIX"]] <- "{}"   # take the last 
# pegR[["SUFFIX"]] <- "{}"   # take the first 


# 
# given intial row, col
# function to:
#     print at row, col (or save row,col, txt)
#     return width, height
# 
# just group layer (layer0)
#   note expressions stacks, (so will create tall blocks)
# grouping:
# 
# a b / ( c d ) /( e f )   
pegR<-new.parser(action.exe=T)
pegR<-pegR + c("OPEN<-'(' SPACING", "{}")
pegR<-pegR + c("CLOSE<-')' SPACING", "{}")
pegR<-pegR + c("OR<-'/' SPACING", "{}")
pegR<-pegR + c("LEFTARROW<- '<-'", "{}")

#pegR<-pegR + "EXPRESSION <- SEQUENCE  ('/'  SEQUENCE)* "
pegR<-pegR + "SEQUENCE <- (PREFIX SPACING)*" 


##################-begin----------------------------------------------------------
# pegR<-pegR + c("PREFIX <- ('&' / '!')?  SUFFIX") #THIS IS REPLACE BY THE FOLLOWING
pegR<-pegR + c("NOT<-'!' SPACING", "{}")
pegR<-pegR + c("AND<-'&' SPACING", "{}")
pegR<-pegR + c("GNOTSUFFIX <-  NOT  SUFFIX", "{list(NOT=v)}") 
pegR<-pegR + c("GANDSUFFIX <-  AND  SUFFIX", "{list(AND=v)}") 
pegR<-pegR + c("PREFIX <- GNOTSUFFIX  /  GANDSUFFIX  / SUFFIX") 
###########################--end---------------------------------------------------

##################-begin----------------------------------------------------------
# pegR<-pegR + "SUFFIX <- PRIMARY  ( '?' / '*' / '+' )?"
pegR<-pegR + c("QUEST <- '?'","{}")
pegR<-pegR + c("STAR  <- '*'", "{}")
pegR<-pegR + c("PLUS  <- '+'", "{}")
pegR<-pegR + c("PRIMQ <- PRIMARY SPACING QUEST", "{list(QUES=v)}")
pegR<-pegR + c("PRIMS <- PRIMARY SPACING STAR", "{list(STAR=v)}")
pegR<-pegR + c("PRIMP <- PRIMARY SPACING PLUS", "{list(PLUS=v)}")
pegR<-pegR + "SUFFIX <- PRIMQ / PRIMS /PRIMP / PRIMARY"
###########################--end---------------------------------------------------



pegR<-pegR + "PRIMARY <-  P1 / GP2 / P3"
pegR<-pegR + "P3 <-  LITERAL /  '.'"
#pegR<-pegR + "P2 <- OPEN EXPRESSION CLOSE"
pegR<-pegR + "GP2 <- OPEN GSEQ CLOSE"
pegR<-pegR + "P1 <- IDENTIFIER !'<-' "

##################-begin----------------------------------------------------------

pegR<-pegR + c("IDENTIFIER<-IDENTSTART  IDENTCONT*  SPACING", "{-}")
pegR<-pegR + "IDENTCONT<- IDENTSTART / [0-9]"
pegR<-pegR + "IDENTSTART<-[a-z] / [A-Z]"
###########################--end---------------------------------------------------

##################-begin----------------------------------------------------------
pegR<-pegR+c("CHAR <-  .")  #any single character?
pegR<-pegR+c("QUOTE1<- \"\'\"", "{}") 
pegR<-pegR+c("QUOTE2<- '\"\'", "{}")
pegR<- pegR+c("LITERAL <-  (QUOTE1 (!(QUOTE1) + CHAR)*  QUOTE1 SPACING) / (QUOTE2 (!(QUOTE2) + CHAR)*  QUOTE2  SPACING) ", "{-}")
#pegR<- pegR+c("LITERAL <-  (QUOTE1 (!(QUOTE1) + CHAR)*  QUOTE1)   SPACING ", "{-}")
###########################--end---------------------------------------------------
pegR<-pegR + c("SPACING<- ' '*",  "{}")

#-----------------------
#pegR<-pegR + "GOR   <- PREFIX+   ('/'  (GP2 / PREFIX) )+ " # Only one naked gor per line (other gors are inside GP2)
pegR<-pegR + c("GP2P<-OR  (GP2 / PREFIX)", "{list(R=v)}")
pegR<-pegR + c("GPREPLUS<-PREFIX+", "{list(R=v)}")
pegR<-pegR + c("GOR   <- GPREPLUS  (GP2P)+" , "{list(GOR=v)}")          
#-------------------------

pegR<-pegR + "GSEQ  <- SPACING & PREFIX GOR? (PREFIX SPACING)*" #The whole line


#TODO dot and range

# action for gor:
# take current row,col and start drawing
# each leaf should have length 1
# GP2, GOR an GSEQ should return their dims (or maybe contents?)

# value(pegR[["GSEQ"]]('a? b'))->res
# res
# value(pegR[["GSEQ"]]('a b/c'))
# value(pegR[["GSEQ"]]('a ( b / c )'))
# value(pegR[["GSEQ"]]('a ( b / c ) d'))
# value(pegR[["GSEQ"]]('a / ( b (e/ c) ) d'))->res
# res




# width<-function(l, type="R"){
#   w<-switch(type,
#          atom=1,
#          R   = sum( sapply(1:length(l),  function(i) width(l[[i]], names(l)[i])) ),
#          GOR = max( sapply(1:length(l),  function(i) width(l[[i]], names(l)[i])) ),
#          1
#          )
#   w
# }
# 
# height<-function(l, type="R"){
#   h<-switch(type,
#          atom=1,
#          R   = max( sapply(1:length(l),  function(i) height(l[[i]], names(l)[i])) ),
#          GOR = sum( sapply(1:length(l),  function(i) height(l[[i]], names(l)[i])) ),
#          1
#   )
#   h
# }
# 

# 
# library(grid)
# plot.new()
# grid.newpage()
# source("resizeTxt.R")
# 
# # value(pegR[["GSEQ"]]('a / ( b (e/ c) ) d'))->res
# # res
# # resl<-layout(res,0,0)
# 
# # value(pegR[["GSEQ"]]('a  d'))->res
# # res
# # resl<-layout(res,1,1)
# # resl
# 
# #value(pegR[["GSEQ"]]('! x '))->res
# value(pegR[["GSEQ"]]('a / ( b (e/ c) ) d d (x/y) !(a b / & c)'))->res
# #value(pegR[["GSEQ"]]('!(a /! b)'))->res
# res
# resl<-layout(res)
# 
# drawGridCoord()
# 
# drawPeg(resl)
# 
# # box0At(1,1,2,1,fill="yellow")
# # box0At(2,1,2,1)
# # box1At(1,1)
# # box1At(1,2)
