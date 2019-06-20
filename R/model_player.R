library(abind)
library(keras)


modelGame<-setRefClass("model_player",
    fields=list(
      # System variables
      model="list",
      game="snake",
      datasets="array",
      distance="array",
      scores = "vector",
      direction = "array",
      logs = "list",
      tmplog = "character",
      fitfunc = "function",
      gamma = "numeric",
      tuningvar = "numeric"
    ),
    methods=list(
      addModel = function(newmodel,fitfun=keras::fit,gamma=.9,tuning_var=80){
        model<<-list(newmodel)
        setfitfunction(fitfun)
        gamma<<-gamma
        tuningvar<<-tuning_var
      },
      train = function(ngames){
        init()
        for(game_num in seq(1:ngames)){
          cat(paste("training on game:",game_num))
          game_status<-game$returnstatus()
          final_score<-0

          while(any(game_status!="DEAD")){
           nextDir<-pred_next(game_status,game_num)
           game_status<-game$run_iter(nextDir,returnStatus = TRUE)
           if(any(game_status!="DEAD")){
             append_data(nextDir,game_status)
             if(game_status$score!=-30){
              final_score<-game_status$total_score
             }
           }
           fitModel(latest=TRUE,game_number = game_num)
           # fitModel(latest=FALSE,game_number = game_num)

           tmplog<<-game$log
          }

          fitModel(game_number = game_num)
          cat(paste0("\t",final_score,"\n"))
          log<-logs
          log[[game_num]]<-list(
            gamelog=game$log,
            score=final_score,
            game_num=game_num)
          logs<<-log
          newgame()
        }

      },
      init = function(){
        game<<-new("snake")
        game$init()
        status_old<-game$returnstatus()
        status_new <-game$run_iter(status_old$direction,returnStatus = TRUE)
        nextstep<-as.numeric(c("up","down","left","right")%in%status_old$direction)

        dist_fruit_orig<-sqrt(((status_old$fruit[1]-status_old$body[1,1])^2) + (status_old$fruit[2]-status_old$body[1,2])^2)
        dist_fruit_next<-sqrt(((status_new$fruit[1]-status_new$body[1,1])^2) + (status_new$fruit[2]-status_new$body[1,2])^2)

        nextstep[nextstep==1] <- (dist_fruit_orig-dist_fruit_next)*10

        headLoc<-status_old$body[1,]
        fruitLoc<-status_old$fruit

        dirFruit<-c(headLoc[1]<fruitLoc[1],headLoc[1]>fruitLoc[1],headLoc[2]<fruitLoc[2],headLoc[2]>fruitLoc[2])


        danger_body<-vector("numeric",4)
        danger_wall<-vector("numeric",4)
        i<-1
        for(nextdir in c("up","down","left","right")){
            nextpos<-switch(nextdir,"up"=c(0,1),"down"=c(0,-1),"left"=c(-1,0),"right"=c(1,0))+status_old$body[1,]
            danger_body[i]<-as.numeric(any(do.call('c',lapply(1:nrow(status_old$body),function(x){all(status_old$body[x,]==nextpos)}))))
            danger_wall[i]<-as.numeric(any(nextpos%in%c(-1,nrow(status_old$board)+1,ncol(status_old$board)+1)))
            i<-i+1
        }


        datasets <<- array(c(headLoc,fruitLoc,dirFruit,danger_body,danger_wall),dim = c(1,16))

        headLoc<-status_new$body[1,]
        fruitLoc<-status_new$fruit


        dirFruit<-c(headLoc[1]<fruitLoc[1],headLoc[1]>fruitLoc[1],headLoc[2]<fruitLoc[2],headLoc[2]>fruitLoc[2])


        danger_body<-vector("numeric",4)
        danger_wall<-vector("numeric",4)
        i<-1
        for(nextdir in c("up","down","left","right")){
          nextpos<-switch(nextdir,"up"=c(0,1),"down"=c(0,-1),"left"=c(-1,0),"right"=c(1,0))+status_new$body[1,]
          danger_body[i]<-as.numeric(any(do.call('c',lapply(1:nrow(status_new$body),function(x){all(status_new$body[x,]==nextpos)}))))
          danger_wall[i]<-as.numeric(any(nextpos%in%c(-1,nrow(status_new$board)+1,ncol(status_new$board)+1)))
          i<-i+1
        }

        datasets  <<- abind(datasets,array(c(headLoc,fruitLoc,dirFruit,danger_body,danger_wall),dim = c(1,16)),along=1)
        direction <<- array(nextstep,dim=c(1,4))
        scores    <<-c(status_new$score)
        fitModel(game_number=1)

        logs<<-list()
      },
      newgame = function(){
        game<<-new("snake")
        game$init()
      },
      pred_next = function(nextds,game_number,prob=FALSE,is.ds=FALSE){

        if(sample(1:200,1) < tuningvar || (length(game$log)<5 & game_number < 3)){
          rawpreds<-runif(4)
          nextmove<-rawpreds
        }else{
          if(is.ds){
            nextds_rs<-nextds
          }else{
            headLoc<-nextds$body[1,]
            fruitLoc<-nextds$fruit
            dirFruit<-c(headLoc[1]<fruitLoc[1],headLoc[1]>fruitLoc[1],headLoc[2]<fruitLoc[2],headLoc[2]>fruitLoc[2])

            danger_body<-vector("numeric",4)
            danger_wall<-vector("numeric",4)
            i<-1
            for(nextdir in c("up","down","left","right")){
              nextpos<-switch(nextdir,"up"=c(0,1),"down"=c(0,-1),"left"=c(-1,0),"right"=c(1,0))+nextds$body[1,]
              danger_body[i]<-as.numeric(any(do.call('c',lapply(1:nrow(nextds$body),function(x){all(nextds$body[x,]==nextpos)}))))
              danger_wall[i]<-as.numeric(any(nextpos%in%c(-1,nrow(nextds$board)+1,ncol(nextds$board)+1)))
              i<-i+1
            }
            nextds_rs <- array(c(headLoc,fruitLoc,dirFruit,danger_body,danger_wall),dim = c(1,16))
          }
          nextmove<-predict(model[[1]],nextds_rs)
        }

        if(prob){
          nextmove
        }else{
          c("up","down","left","right")[which.max(nextmove)]
        }
      },

      fitModel = function(latest=FALSE,game_number){
        if(latest==TRUE){
          minibatch<-max(dim(scores[1]),1)
        }else if(dim(direction)[1]>1000){
          minibatch = sample(seq(1,dim(direction)[1]-1), 1000,replace = FALSE)
        }else{
          minibatch = seq(1,max(dim(direction)[1]-1,1))
        }

        minibatch<-unique(c(max(dim(direction)[1],1),minibatch))

        mini_dataset      <- array_reshape(datasets[minibatch,,drop=FALSE],c(length(minibatch),16))
        mini_dataset_next <- array_reshape(datasets[minibatch+1,,drop=FALSE],c(length(minibatch),16))
        mini_dir          <- direction[minibatch,,drop=FALSE]
        mini_score        <- scores[minibatch]


        trainmodel<-model[[1]]

        for(j in seq(1,length(minibatch))){

            predmove<-max(pred_next(mini_dataset_next[j,,drop=FALSE],game_number,prob = TRUE,is.ds=TRUE))
            target <- mini_score[j] + gamma * predmove
            target_f <- pred_next(mini_dataset[j,,drop=FALSE],game_number,prob = TRUE,is.ds=TRUE)

            dist_fruit_orig<-sqrt(((mini_dataset[j,3]-mini_dataset[j,1])^2) + (mini_dataset[j,4]-mini_dataset[j,2])^2)
            dist_fruit_next<-sqrt(((mini_dataset_next[j,3]-mini_dataset_next[j,1])^2) + (mini_dataset_next[j,4]-mini_dataset_next[j,2])^2)

            if(mini_score[j]==0)
              target <- target + (-5+((dist_fruit_orig-dist_fruit_next)*10))

            target_f[which.max(mini_dir[j,])] <- target

            target_f<-array_reshape(target_f,c(1,4))

            fitfunc( trainmodel,
             mini_dataset[j,,drop=FALSE],
             target_f,
             epochs = 1,
             verbose = 0)
        }

        model<<-list(trainmodel)


      },

      append_data = function(dir,game_stat){

        nextstep<-as.numeric(c("up","down","left","right")%in%dir)

        headLoc<-game_stat$body[1,]
        fruitLoc<-game_stat$fruit
        dirFruit<-c(headLoc[1]<fruitLoc[1],headLoc[1]>fruitLoc[1],headLoc[2]<fruitLoc[2],headLoc[2]>fruitLoc[2])

        danger_body<-vector("numeric",4)
        danger_wall<-vector("numeric",4)
        i<-1
        for(nextdir in c("up","down","left","right")){
          nextpos<-switch(nextdir,"up"=c(0,1),"down"=c(0,-1),"left"=c(-1,0),"right"=c(1,0))+game_stat$body[1,]
          danger_body[i]<-as.numeric(any(do.call('c',lapply(1:nrow(game_stat$body),function(x){all(game_stat$body[x,]==nextpos)}))))
          danger_wall[i]<-as.numeric(any(nextpos%in%c(-1,nrow(game_stat$board)+1,ncol(game_stat$board)+1)))
          i<-i+1
        }

        datasets  <<- abind(datasets,array(c(headLoc,fruitLoc,dirFruit,danger_body,danger_wall),dim = c(1,16)),along=1)
        direction <<- abind(direction,nextstep,along=1)
        scores    <<- c(scores,game_stat$score)
      },

      setfitfunction = function(func){
        fitfunc<<-func
      }


    )
)
