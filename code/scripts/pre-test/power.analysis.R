library(DeclareDesign)

simple_design_diagnosands <- 
  declare_diagnosands(select = c(bias, rmse, power))


simple_design_diagnosis <- 
  diagnose_design(simple_design, diagnosands = simple_design_diagnosands, sims = 500)


population <- declare_population(N = 2331, u = rnorm(N)) 
potential_outcomes <- declare_potential_outcomes(
  Y_Z_0 = u, 
  Y_Z_1 = Y_Z_0 + 0.25,
  Y_Z_2 = Y_Z_0 + 0.25,
  Y_Z_3 = Y_Z_0 + 0.25,
  Y_Z_4 = Y_Z_0 + 0.25)

estimand <- declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0)) 




mydat <- data.frame( v1 = rep( c(3,6,9), each=2 ),
    v2 = rep( 0:1, 3 ), 
    resp=c(0.0025, 0.00395, 0.003, 0.0042, 0.0035, 0.002) )

mydat


install.packages("WebPower")
library(WebPower)

wp.logistic(n = NULL, p0 = NULL, p1 = NULL, alpha = 0.05,
  power = NULL, alternative = c("two.sided", "less", "greater"),
  family = c("Bernoulli", "exponential", "lognormal", "normal", "Poisson",
  "uniform"), parameter = NULL)

wp.logistic(n = 100, 



œvoter_file <- voter_file %>% 
  
  mutate(Z = sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.5, 0.5)))

declare_potential_outcomes(
  Y_Z_0 = u, 
  Y_Z_1 = Y_Z_0 + 0.25,
  Y_Z_2 = Y_Z_0 + 0.25,
  Y_Z_3 = Y_Z_0 + 0.25,
  Y_Z_4 = Y_Z_0 + 0.25)
declare_potential_outcomes(Y ~ u + 0.25 * Z, assignment_variables = 1:5)


 rbinom(n = 2331, size = 1, prob = 0.5 + 0.05 * X)
 rbinom(n = 100, size = 5, prob = 0.5 + 0.05 * Z)
 sample(c(1:5), 100, replace = TRUE)

design <-
  declare_population(N = 100) +
  declare_potential_outcomes(Y ~ rbinom(n = N, size = 5, prob = 0.2 + 0.05 * Z)) +
  declare_estimand(ATE = 0.05) +
  declare_assignment(m = 50) +
  declare_estimator(Y ~ Z)

diagnosands <-
  declare_diagnosands(bias = mean(estimate - estimand),
                      power = mean(p.value <= 0.05))

diagnosis <- diagnose_design(design, diagnosands = diagnosands)
diagnosis

my_estimand_ATE <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
my_estimand_ATT <- 

  model <- 
  declare_population(N = 100, U = rnorm(N)) +
  declare_potential_outcomes(Y ~ 0.2*Z + U)
  
survey_data <- fabricate(
  N = 100,
  Q1 = draw_likert(x = rnorm(N)),
  Q2 = draw_likert(x = rnorm(N)),
  Q3 = draw_likert(x = rnorm(N))
)  
  
  
library(rms)

tmpfun <- function(n, beta0, beta1, beta2) {
    x <- runif(n, 0, 10)
    eta1 <- beta0 + beta1*x
    eta2 <- eta1 + beta2
    p1 <- exp(eta1)/(1+exp(eta1))
    p2 <- exp(eta2)/(1+exp(eta2))
    tmp <- runif(n)
    y <- (tmp < p1) + (tmp < p2)
    fit <- lrm(y~x)
    fit$stats[5]
}  
out <- replicate(1000, tmpfun(100, -1/2, 1/4, 1/4))
mean( out < 0.05 )  



design <-
  declare_population(N = 2331) +
  declare_potential_outcomes(Y ~ rbinom(n = N, size = 1, prob = 0.5 + 0.05 * Z)) +
  declare_estimand(ATE = 0.05) +
  declare_assignment(m = 50) +
  declare_estimator(Y ~ Z)

diagnosands <-
  declare_diagnosands(bias = mean(estimate - estimand),
                      power = mean(p.value <= 0.05))

diagnosis <- diagnose_design(design, diagnosands = diagnosands)
diagnosis





model <- 
  declare_population(N = 2000, U = rnorm(N)) +
  declare_potential_outcomes(Y ~ 0.2*Z + U)

# I -- Inquiry: A query defined in terms of potential outcomes
inquiry <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))

# D -- Data Strategy: Researcher interventions on the world
data_strategy <- 
  declare_assignment(m = 20) +
  reveal_outcomes(Y, Z)

# A -- Answer Strategy: Conclusions to be drawn from data
answer_strategy <- 
  declare_estimator(Y ~ Z, estimand = "ATE")

estimator_probit <- declare_estimator(
  Y ~ Z,
  model = glm,
  family = binomial(link = "probit"),
  term = "Z"
)

# Design: Putting it all together
design <- model + inquiry + data_strategy + answer_strategy

diagnosis <- diagnose_design(design, sims = 1000, bootstrap_sims = 500)
diagnosis

