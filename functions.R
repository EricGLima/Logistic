# May 04, 2023
# CEFET - RJ
# Logistic
# Eric G. Lima

## All functions of each archive

#####
# Cross_validation.R
ada = function(x,y) {
  
  set.seed(seed)
  adaboost = boosting(
    quality ~ .,
    data      = data[y[["idx"]],],
    mfinal    = x,
    coeflearn = 'Breiman',
    control   = rpart.control(maxdepth=20)
  )
  
  barra$tick()
  
  return(adaboost)
}

get_pred = function(model, test_index){
  
  set.seed(seed)
  p = predict.boosting(
    model,
    newdata = data[test_index[["idx"]], ]
  )
  
  return(p$error)
}
