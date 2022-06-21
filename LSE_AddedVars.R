load("/Users/jakewren/Downloads/CPS1991_female.RData")
data <- data[data$paidhr == 1, ]
data$wagesq <- data$wage^2

summary(data$wage)
summary(data$hours)

labsup = lm(hours~wage+wagesq+educ+age+married,data=data)

phrsfun <- function(w,e,a,m) {
  nvals = length(w)
  # newd = list(wage = w, wagesq = w^2, educ = rep(e, nvals))
  newd = list(wage = w, wagesq = w^2, educ = e, age = a, married = m)
  h = predict(labsup, newdata = newd)
  return(h)
  # return(newd)
}

labsupderiv <- function(w,e,a,m){
  dw = .0001
  h1 = phrsfun(w+dw,e,a,m)
  h0 = phrsfun(w,e,a,m)
  deriv = (h1-h0)/dw
  return(deriv)
}

# predict(labsup, newdata = list(wage=8, wagesq=64, educ=12))
lscoef = labsup$coefficients



# elas <- function(w) {
#  dhdw <- lscoef[2] + 2*lscoef[3]*w
#  h <- lscoef[1] + lscoef[2]*w + lscoef[3]*w^2 + lscoef[4]*w
#  e <- dhdw*(w/h)
#  return(e)
# }


elas <- function(w,e,a,m) {
  dhdw <- labsupderiv(w,e,a,m)
  h <- phrsfun(w,e,a,m)
  ey <- dhdw*(w/h)
  return(ey)
}

mean_educ <- mean(data$educ)
mean_age <- mean(data$age)
mean_married <- mean(data$married)

elas(5,mean_educ,mean_age,mean_married)
elas(15,mean_educ,mean_age,mean_married)
elas(20,mean_educ,mean_age,mean_married)

wagevals <- 1:30
hourspred2 <- lscoef[1] + lscoef[2]*wagevals + lscoef[3]*wagevals^2 + lscoef[4]*mean_educ + lscoef[5]*mean_age + lscoef[6]*mean_married
plot(hourspred2,wagevals,'l')

