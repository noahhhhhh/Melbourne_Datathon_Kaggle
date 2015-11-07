###########################################################################################
## Calculate AUC manually #################################################################
###########################################################################################
# Below is the diagnostic stats. (1), (2), etc. are threshold
# (1) Definitely normal: 33/3
# (2) Probably normal: 6/2
# (3) Questionable: 6/2
# (4) Probably abnormal: 11/11
# (5) Definitely abnormal: 2/33
# you can think of this as, the predicted profit loss of 5 ppl are -100, 100, 200, 300, 400
# , they are threshold too.

#######################################
## 1.0 data ###########################
#######################################
norm = rep(1:5, times = c(33,6,6,11,2))
abnorm = rep(1:5, times = c(3,2,2,11,33))
testres = c(abnorm,norm)
truestat = c(rep(1,length(abnorm)), rep(0,length(norm)))
tab = as.matrix(table(truestat, testres))

#######################################
## 2.0 stats ##########################
#######################################
tot = colSums(tab)                            # Number of patients with each type of test result
truepos = unname(rev(cumsum(rev(tab[2,]))))   # Number of true positives
falsepos = unname(rev(cumsum(rev(tab[1,]))))  # Number of false positives
totpos = sum(tab[2,])                         # The total number of positives (one number)
totneg = sum(tab[1,])                         # The total number of negatives (one number)
sens = truepos / totpos                       # Sensitivity (fraction true positives)
omspec = falsepos / totneg                    # 1 − specificity (false positives)
sens = c(sens,0); omspec = c(omspec,0)        # Numbers when we classify all patients as normal

#######################################
## 3.0 plot ###########################
#######################################
plot(
    omspec, sens, type = "b", xlim = c(0,1), ylim = c(0,1), lwd = 2,
    xlab = "1 − specificity", ylab = "Sensitivity"
) # perhaps with xaxs="i"
grid()
abline(0,1, col = "red", lty = 2)

#######################################
## 4.0 AUC ############################
#######################################
height = (sens[-1] + sens[-length(sens)]) / 2
width = -diff(omspec) # = diff(rev(omspec))
sum(height * width)
