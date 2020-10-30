
# POPULATION DATA
n = 1000
age_p_cdf <- spline(age_p[,1], age_p[,3], n=n)
inc_p_cdf <- spline(inc_p[,1], inc_p[,3], n=n)

for (i in 2:nrow(age_p)) {
	for (j in 1:length(age_p_cdf$x)) {
		if (age_p_cdf$x[j] < age_p[(i),1] & age_p_cdf$x[j] >= age_p[i-1,1]) {
			age_p_cdf$y[j] = age_p[i-1,3]
		}
	}
}
for (i in 2:nrow(inc_p)) {
	for (j in 1:length(inc_p_cdf$x)) {
		if (inc_p_cdf$x[j] < inc_p[(i),1] & inc_p_cdf$x[j] >= inc_p[i-1,1]) {
			inc_p_cdf$y[j] = inc_p[i-1,3]
		}
	}
}

age_p_x <- age_p_cdf$x
age_p_y <- age_p_cdf$y
inc_p_x <- inc_p_cdf$x
inc_p_y <- inc_p_cdf$y

# SAMPLE DATA
age_s_x <- data$age
inc_s_x <- data$income
N = length(age_s_x)

# create cdf matrix for age and income
age_X <- matrix(data=0,nrow=length(age_p_x),ncol=N)
inc_X <- matrix(data=0,nrow=length(inc_p_x),ncol=N)
for (i in 1:length(age_p_cdf$x)) {
	age_X[i,which(age_s_x <= age_p_x[i])] <- 1
	inc_X[i,which(inc_s_x <= inc_p_x[i])] <- 1
}

# Initialize weights vector
W <- matrix(rep(1, N,ncol=1))

# RUN OPTIMIZATION WITH CONSTRAINTS
lb <- matrix(rep(l,N),ncol=1) # lower constraints
ub <- matrix(rep(u,N),ncol=1) # upper constraints

out <- optim(W, fn=ob_func, method="L-BFGS-B", control=list(trace=TRUE, REPORT=1, maxit=300), lower=lb, upper=ub, age_X=age_X, inc_X=inc_X, y1_p=age_p_y, y2_p=inc_p_y)

W_hat <- out$par
err_hat <- out$value

# Weighted CDF is simply X*W/sum(W):
age_s_y <- age_X%*%W/sum(W)
inc_s_y <- inc_X%*%W/sum(W)
age_s_y_W <- age_X%*%W_hat/sum(W_hat)
inc_s_y_W <- inc_X%*%W_hat/sum(W_hat)
