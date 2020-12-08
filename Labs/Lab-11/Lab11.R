##############################
# Problem #1 Car Safety Data #
##############################

#Read in the File
safetyData = read.csv(file="rstudio/lab/Lab11Prob1.csv", header=TRUE, sep=",")

#Separate the Vector of Outcomes
X = safetyData$x;

#Calculate T = \sum_i X_i
T = sum(X);

#Calculate the Number of Observations
N = length(X);

#95% Confidence Test
print(binom.test(T, N, 0.30, alternative=c('greater'), conf.level=0.95)) # (0.196659, 1.000000)

# 1.b
# Yes we may conclude that the bumper design was a success, because of how large the gap between the lower and upper bound of the confidence interval are. We should not reject the null hypothesis at alpha = 0.05

###################################
# Problem #2 Sprinker System Data #
###################################

#Read in the File
sprinklerData = read.csv(file="rstudio/lab/Lab11Prob2.csv", header=TRUE, sep=",")

#Separate the Vector of Outcomes
activationTemps = sprinklerData$x;

#Calculate the Number of Observations
N = length(activationTemps);

# Calculate the Z-Score
# Sample Average -> sum(activationTemps) / N = 131.188407694883
z = ((sum(activationTemps) / N) - 130) / (1.5 / sqrt(N)) # 7.92271796588921

upperBound = qnorm(0.01/2, mean = 0, sd = 1, lower.tail = FALSE)
print(upperBound)
lowerBound = qnorm(0.01/2, mean = 0, sd = 1, lower.tail = TRUE);
print(lowerBound)

# cross check the computed Z-Score with the
if(z < lowerBound){
  print('We reject the null hypothesis because z < z_{0.01/2}')
}
if(z > upperBound){
  print('We reject the null hypothesis because z > z_{0.01/2}')
}

# 2.c
# No we should not reject the null hypothesis

#################################################
# Problem #3 Independent Short Film Length Data #
#################################################

filmData = read.csv(file="rstudio/lab/Lab11Prob3.csv", header=TRUE, sep=",")

#Separate the Run Times (in Minutes)
runTimes = filmData$x;

#Calculate the Number of Observations
N = length(runTimes);

# Generate Box plot diagram
boxplot(runTimes, main="Run Times of Independent Films", names=c("Running Times"), col=c("red","blue"), ylab="Minutes")

# Run a hypothesis test on the sample data
t.test(runTimes, mu=15, alternative="less")

# 2.b
# There are 49 degrees of freedom aka 49 independent values in the One Sample t-test
# 2.c
# We reject the null hypothesis

###########################
# Problem #4 Medical Data #
###########################

dataFrame = read.csv(file="rstudio/lab/Lab11Prob4.csv",header=TRUE, sep=",")

medicalDF   = dataFrame[which(dataFrame$Type=="Medical"),]$Duration;
emergencyDF = dataFrame[which(dataFrame$Type=="Emergency"),]$Duration;
extendedDF  = dataFrame[which(dataFrame$Type=="Extended"),]$Duration;

boxplot(medicalDF,emergencyDF,extendedDF, main="Pregnancy Duration", names=c("Standard","Emergency","Extended"), col=c("red","blue","green"), ylab="Weeks")

# t-Test between the different data frames
t.test(medicalDF, emergencyDF)
t.test(emergencyDF, extendedDF)
t.test(medicalDF, extendedDF)

# 4.b
# reject the null hypothesis
# 4.c
# The hypothesis seems pretty accurate but feels like it does not meet the world wide average distribution

