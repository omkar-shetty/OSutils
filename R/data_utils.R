#' Title
#'
#' @param train_dat Dataframe to be split
#' @param partitions Number of partitions that the data is to be split into (defaults to 3)
#' @param prop Set of proportions to split the dataset (should sum to 1)
#' @param seed
#'
#' @return list of split datasets
#' @export
#'
#' @examples
split_train_dt <- function(train_dat, partitions = 3, prop = c(0.6,0.2,0.2) , seed = 123){

  if(length(prop) != partitions) stop('Mismatch between the number of partitions and proportions provided')
  if(sum(prop) != 1) stop('Incorrect proportions provided')
  if(partitions > 3) stop('Partitions cannot exceed 3')
  if(!is.data.frame(train_dat)) stop('Dataset should be in the form of a data frame')

  train_dat = data.table::setDT(train_dat)

  train_dat[,id := .I]
  set.seed(seed)

  train_samples <- sample(round(prop[1]*nrow(train_dat)),x = 1:nrow(train_dat), replace = F)
  test_samples <- sample(round(prop[2]*nrow(train_dat)),x = setdiff(train_dat$id, train_samples), replace = F)
  train_part <- train_dat[id %in% train_samples,][,id := NULL]
  test_part <- train_dat[id %in% test_samples,][,id := NULL]

  if(partitions > 2) {
    valid_samples <- setdiff(train_dat$id, c(train_samples,test_samples))
    valid_part <- train_dat[id %in% valid_samples,][,id := NULL]
  }else{
    valid_part <- NULL
  }

  return(list(train_dt = train_part,
              test_dt = test_part,
              valid_dt = valid_part))
}


implement_OHE <- function(dt, ...){
  #FIXME
}
