# ogi_5lines.R: objective general index

ogi = function(X){
  f = function(w, S) sum(-log(w)) + sum(w * (S%*%w)) / 2
  gr = function(w, S) -1/w + S%*%w
  S = cov(X) * (nrow(X)-1) / nrow(X)
  w = optim(rep(1,ncol(X)), f, gr, S, method="L-BFGS-B", lower=0, upper=Inf)$par
  list(g = X %*% w, X = X, w = w)
}
