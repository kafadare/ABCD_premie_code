## This is a script to save gamlss models to loop through, each saved as an .rds file.

#We want to do family selection. We will choose between three 3-parameter families.
#Box-Cox cole and Green, Generalized Gamma, Generalized Inverse Gaussian, Exponential Gaussian, Power Exponential

family_set <- c("BCGG()", "GG()", "GIG()", "exGAUS()", "PE()")

age_term <- c("ns(age, 3)", "age")

n_cyc = 20