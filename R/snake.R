
#' importFrom keypress keypress
snake<-setRefClass("snake",
   fields=list(
     # System variables
     body="matrix",
     debug='logical',
     head='matrix',
     direction='character',
     length="numeric",
     dead="logical",

     score="numeric",
     height="numeric",
     width="numeric",
     board="matrix",

     food = "numeric"
     ),

   methods=list(

     init = function(height=20,width=20){
       body<<-matrix(c(floor(width/2),floor(height/2),floor(width/2),floor(height/2)-1),byrow = FALSE,ncol=2)
       direction<<-"up"
       score<<-0
       length<<-2
       height<<-height
       width<<-width
       dead<<-FALSE
       updatefood()
       updateboard()

     },

     stepforward = function(){
       nextloc<-nextstep()
       #check if nextLoc is body or wall, if so, fail
       isbody<-any(do.call('c',lapply(1:nrow(body),function(x){all(body[x,]==nextloc)})))
       iswall<-any(nextloc%in%c(-1,height+1,width+1))
       if(isbody || iswall){die()}

       #check if nextloc is food
       isfood=all(food==nextloc)
       if(isfood){
         length<<-length+1
         score <<- score + ((floor(log(length))+1) * 5)
         updatefood()
       }

       updatebody(nextloc)
       updateboard()
     },
     nextstep = function(){
       switch(direction,
              "up"=c(0,1),
              "down"=c(0,-1),
              "left"=c(-1,0),
              "right"=c(1,0))+body[1,]

     },
     updatebody = function(nextloc){
       bod<-as.numeric(body)
       if(length>nrow(body)){
         body<<-matrix(c(nextloc[1],bod[1:nrow(body)],
                         nextloc[2],bod[(nrow(body)+1):(2*nrow(body))]),byrow=FALSE,ncol=2)
       }else{
         body<<-matrix(c(nextloc[1],bod[1:(nrow(body)-1)],
                         nextloc[2],bod[(nrow(body)+1):((2*nrow(body))-1)]),byrow=FALSE,ncol=2)
       }
     },
     updatefood = function(){
       pos<-sample(1:(width*height),1)
       col<-ceiling(pos/height)
       row<-pos%%height

       isbody<-any(do.call('c',lapply(1:nrow(body),function(x){all(body[x,]==c(col,row))})))
       if(isbody){
         updatefood()
       }else{
         food<<-c(col,row)
       }
     },

     updateboard = function(){
       boardtemp<-matrix(rep(0,width*height),nrow=width)
       boardtemp[body[1,2],body[1,1]]<-2
       for(snake_segment in seq(2,length)){
         boardtemp[body[snake_segment,2],body[snake_segment,1]]<-1
       }
       boardtemp[food[2],food[1]]<-3
       board<<-boardtemp
     },

     plotboard = function(){
       plot(0,0,xlim=c(0,width),ylim=c(0,height),type='n',axes=FALSE, frame.plot=TRUE)
       Axis(side=1, labels=FALSE)
       Axis(side=2, labels=FALSE)
       points(body[,1],body[,2],col=c("blue",rep('black',length-1)),pch=15,cex=2)
       points(food[1],food[2],col="red",pch=16,cex=2)
       title(main = paste("SNAKE! score -",score))
     },

     returnstatus = function(){
       list(score=score,
            board=board,
            direction=direction)
     },

     die = function(){
       dead<<-TRUE
       text(floor(width/2),floor(height/2),labels = "GAME OVER",col="red",cex=3)
       return(score)
     },

     updatedirection = function(){
       line<-readline("snakedir: a,w,s,d")
       print(line)
       dir<-switch(tolower(line),"a"="left","w"="up","s"="down","d"="right",direction)
       if(dir!=direction & okayDir(dir)){
         direction<<-dir
       }
     },

     okayDir=function(dir){
       dir!=switch(direction,"right"="left","down"="up","up"="down","left"="right")
     },

     run = function(return_info=TRUE,plot_board=FALSE,delay=.3){
       init()
       if(plot_board){plotboard()}
       if(return_info){returnstatus()}
       while(!dead){
         stepforward()
         plotboard()
         if(plot_board){plotboard()}
         if(return_info){returnstatus()}
         updatedirection()

       }
       text(floor(width/2),floor(height/2),labels = "GAME OVER",col="red",cex=3)
     }
   )
)

game<-new("snake")

game$run(return_info = TRUE,plot_board = TRUE)

