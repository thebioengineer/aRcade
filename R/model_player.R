library(keras)


modelGame<-setRefClass("model_player",
    fields=list(
      # System variables
      model="keras",#TBD
      optimizer="keras", #TBD
      compiledModel="keras" #TBD
    ),
    methods=list(

      defineModel = function(){
        model<<-keras_model_sequential()
      },

      addModelLayer = function(x){
        model<<-model %>% x
      },

      addOptimizer=function(x){
        x
      },

      compileModel = function(){

        compiledModel<<-compile

      }

    )
)
