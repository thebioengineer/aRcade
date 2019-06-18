library(keras)
library(abind)


modelGame<-setRefClass("model_player",
    fields=list(
      # System variables
      model="list",
      game="snake",
      datasets="array",
      distance="array",
      scores = "array",
      direction = "array",
      logs = "list",
      tmplog = "character"
    ),
    methods=list(
      addModel = function(newmodel){
        model<<-list(newmodel)
      },
      train = function(ngames){
        init()
        for(game_num in seq(1:ngames)){
          cat(paste("training on game:",game_num))
          game_status<-game$returnstatus()
          final_score<-0

          while(any(game_status!="DEAD")){
           nextDir<-pred_next(game_status$board,game_status$dist_to_fruit)
           game_status<-game$run_iter(nextDir,returnStatus = TRUE)
           if(any(game_status!="DEAD")){
             append_data(nextDir,game_status$board,game_status$score,game_status$dist_to_fruit)
             if(game_status$score!=-10){
              final_score<-game_status$score
             }
           }
           fitModel()
           tmplog<<-game$log
          }

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
        nextstep[nextstep==1]<-status_new$score

        datasets <<- array(as.matrix(data.frame(status_old$board)),dim = c(nrow(status_old$board),ncol(status_old$board),1))
        distance <<- array(as.matrix(data.frame(status_old$dist_to_fruit)),dim = c(1,2))

        datasets <<- abind(datasets,
                           array(as.matrix(data.frame(status_new$board)),dim = c(nrow(status_old$board),ncol(status_old$board),1)),
                           along=3)
        distance <<- abind(distance,
                           array(as.matrix(data.frame(status_new$dist_to_fruit)),dim = c(1,2)),
                           along=1)


        direction <<- array(nextstep,dim=c(1,4))
        fitModel()

        logs<<-list()
      },
      newgame = function(){
        game<<-new("snake")
        game$init()
      },
      pred_next = function(nextds,dist_to_fruit){

        nextds_rs<-array_reshape(array(nextds,dim=c(nrow(nextds),ncol(nextds),1)),c(1,ncol(datasets)*nrow(datasets)))

        nextds_rs<-abind(nextds_rs,array(as.matrix(data.frame(dist_to_fruit)),dim = c(1,2)),along=2)

        c("up","down","left","right")[which.max(predict(model[[1]],nextds_rs))]
      },

      fitModel = function(){

        if(dim(datasets)[3]>1000){
          minibatch = sample(seq(1,dim(datasets)[3]-1), 1000,replace = FALSE)
        }else{
          minibatch = seq(1,dim(datasets)[3]-1)
        }

        mini_dataset <- array_reshape(datasets[,,minibatch,drop=FALSE],c(length(minibatch),ncol(datasets)*nrow(datasets)))
        mini_dataset <- abind(mini_dataset,
                              array(as.matrix(data.frame(distance[minibatch,,drop=FALSE])),dim = c(length(minibatch),2)),along=2)
        mini_dir     <- direction[minibatch,,drop=FALSE]

        model[[1]] %>%
          fit(mini_dataset,
              mini_dir,
              epochs = 1,
              verbose = 0)

        model<<-list(model[[1]])


      },

      append_data = function(dir,new_board,newscore,dist_to_fruit){

        nextstep<-as.numeric(c("up","down","left","right")%in%dir)
        nextstep[nextstep==1]<-newscore

        datasets <<- abind(datasets,
                           array(as.matrix(data.frame(new_board)),dim = c(nrow(new_board),ncol(new_board),1)),
                           along=3)

        distance <<- abind(distance,
                           array(as.matrix(data.frame(dist_to_fruit)),dim = c(1,2)),
                           along=1)

        direction <<- abind(direction,nextstep,along=1)


      }


    )
)
