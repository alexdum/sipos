krige1<-function (formula, data, newdata, model, beta, y, ..., computeVar = FALSE, 
    fullCovariance = FALSE) 
{
    stopifnot(identical(proj4string(data), proj4string(newdata)))
    lst = extractFormula(formula, data, newdata)
    X = lst$X
    x0 = lst$x0
    if (missing(y)) 
        y = lst$y
    ll = (!is.na(is.projected(data)) && !is.projected(data))
    s = coordinates(data)
    s0 = coordinates(newdata)
    if (is(model, "variogramModel")) {
        require(gstat)
        V = variogramLine(model, dist_vector = spDists(s, s, 
            ll), covariance = TRUE)
        v0 = variogramLine(model, dist_vector = spDists(s, s0, 
            ll), covariance = TRUE)
        c0 = variogramLine(model, dist_vector = c(0), covariance = TRUE)$gamma
    }
    else {
        V = model(data, data)
        v0 = model(data, newdata)
        d0 = data[1, drop = FALSE]
        c0 = as.numeric(model(d0, d0))
    }
    if (!missing(beta)) {
        skwts = solve(V, v0)
        if (computeVar) 
            var = c0 - t(v0) %*% skwts
    }
    else {
        skwts = solve(V, cbind(v0, X))
        ViX = skwts[, -(1:nrow(s0))]
        skwts = skwts[, 1:nrow(s0)]
        beta = solve(t(X) %*% ViX, t(ViX) %*% y)
        if (computeVar) {
            Q = t(x0) - t(ViX) %*% v0
            var = c0 - t(v0) %*% skwts + t(Q) %*% CHsolve(t(X) %*% 
                ViX, Q)
        }
    }
    pred = x0 %*% beta + t(skwts) %*% (y - X %*% beta)
    if (computeVar) {
        if (!fullCovariance) 
            var = diag(var)
        list(pred = pred, var = var)
    }
    else pred
}


 extractFormula = function(formula, data, newdata) {
# extract y and X from data:
  m = model.frame(terms(formula), as(data, "data.frame"))
   y = model.extract(m, "response")
   if (length(y) == 0)
       stop("no response variable present in formula")
    Terms = attr(m, "terms")
    X = model.matrix(Terms, m)

  # extract x0 from newdata:
    terms.f = delete.response(terms(formula))
   mf.f = model.frame(terms.f, newdata) #, na.action = na.action)
    x0 = model.matrix(terms.f, mf.f)
 	list(y = y, X = X, x0 = x0)
 }
v = function(x,y) { spDists(coordinates(x),coordinates(y)) }