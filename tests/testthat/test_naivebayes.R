library(naivebayes)
n<-50; k<-1000
X<-matrix(rnorm(n*k),nrow=n)
b<-rnorm(k)
eta<-drop(X%*%b)
eta<-eta-mean(eta)
y<-rbinom(n,1,plogis(eta))
tr_idx<-1:floor(.8*n)
Xtrn<-X[tr_idx,]
ytrn<-y[tr_idx]
Xtst<-X[-tr_idx,]
ytst<-y[-tr_idx]
fit<-naive_bayes(Xtrn,ytrn,usekernel=TRUE)
preds<-as.character(predict(fit,Xtst,type="class"))
#n_obs==1 case
probs1<-predict(fit,t(as.matrix(Xtst[1,])),type="prob")
#n_obs>1 case
probs2<-predict(fit,Xtst,type="prob")
preds2<-apply(probs2,1,which.max)
# maxprobs<-apply(probs2,1,max)
preds2<-colnames(probs2)[preds2]

test_that("posterior probabilities not NaN", {
    expect_false(any(is.nan(probs1)),info="n_obs=1")
    expect_false(any(is.nan(probs2)),info="n_obs>1")
})

test_that("posterior probabilities sum to 1", {
    expect_equal(rowSums(probs2),rep(1,nrow(probs2)))
})

test_that("probability predictions consistent with class predictions", {
    expect_equal(preds2,preds)
})
