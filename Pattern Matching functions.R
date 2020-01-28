RowVar <- function(x, ...) {
  rowSums((x - rowMeans(x, ...))^2, ...)/(dim(x)[2] - 1)
}

Var.dist = function(sample, global, n.matches = 3, f.length = NULL){
  
  #move sample above global
  if(is.null(f.length)){
    f.length = round(0.33*length(sample))
  }
  
  k = length(sample)
  
  scale.f =  abs(max(global, na.rm = T) - min(sample, na.rm = T))
  sample.scaled = sample + scale.f
  
  sample.scaled = rev(sample.scaled)
  
  
  ts.lagged = quantmod::Lag(global, 0:(k-1))
  
  ts.lagged = ts.lagged[complete.cases(ts.lagged), ]
  
  b2 <- sweep(ts.lagged, 2, sample.scaled)
  distances.vec = RowVar(b2)
  
  matches.matrix = matrix(ncol = n.matches, nrow = k)
  match.index = c(length = n.matches)
  forecast.matrix = matrix(ncol = n.matches, nrow = f.length)
  match.score = c()
  
  for(i in 1:n.matches){
    match = which.min(distances.vec)
    match.score = c(match.score, distances.vec[match])
    trans.match = match + k -1
    match.index[i] = trans.match
    match.window = (trans.match-k+1):trans.match
    matches.matrix[,i] = global[match.window]
    forecast.matrix[,i] = global[(match.window + k)[1:f.length]]
    drop.window = (match-k+1):(match+k-1)
    drop.window = drop.window[which(drop.window > 0)]
    
    distances.vec[drop.window] = NA
    
  }
  
  forecast.df = as.data.frame(forecast.matrix)
  matches.df = as.data.frame(matches.matrix)
  
  matches.df$original = sample
  matches.df$time = seq(1, k)
  #matches.df$score = match.score
  
  return(list("matches.df" = matches.df, "match.index" = match.index, "distances.vec" = distances.vec,
              "forecast" = forecast.df, "match.score" = match.score))
}

GetNextPattern = function(matches.df, distances.vec, match.index, match.score, data, forecast, f.length = NULL){
  k = nrow(matches.df)
  
  match = which.min(distances.vec)
  match.score = c(match.score, distances.vec[match])
  trans.match = match + k -1
  match.index = c(match.index, trans.match)
  match.window = match:trans.match
  
  
  
  drop.window = (match-k+1):(match+k-1)
  drop.window = drop.window[which(drop.window > 0)]
  
  distances.vec[drop.window] = NA
  
  if(is.null(f.length)){
    f.length = round(0.33*length(match.window))
  }
  
  column.name = paste0("V", ncol(matches.df) +1)
  forecast[[column.name]] = data[(match.window + k)[1:f.length]]
  
  
  column.name2 = paste0("V", ncol(matches.df) - 1)
  matches.df[[column.name2]] = data[match.window]
  
  return(list("matches.df" = matches.df, "match.index" = match.index, "distances.vec" = distances.vec, "match.score" = match.score, "forecast" = forecast))
}