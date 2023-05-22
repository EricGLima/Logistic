# May 04, 2023
# CEFET - RJ
# Logistic
# Eric G. Lima

## All functions of each archive

#####
# cross_validation.R
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

# error_by_tree_and_percent_of_data.R
get_pred_by_data_and_tree = function(x,y) {
  
  sub = sample(1:l,x*l)
  
  adaboost = boosting(
    quality ~ .,
    data      = data[sub, ],
    mfinal    = y,
    coeflearn = 'Breiman',
    control   = rpart.control(maxdepth=20)
  )
  
  p = predict.boosting(
    adaboost,
    newdata = data[-sub, ]
  )
  
  message(paste(x, y, p$error, sep="-"))
  return(p$error)
}

