# coin-flipping demo

# NOTE: this is a beginner's code, definitely not optimal...
# by the end of Math 32, you will know a MUCH better way to write this code

#Goal: Run an experiment where we "flip a coin" 5 times and count the number of heads.
#      We want to see if it agrees with what we calculated the probability to be by hand!

########################
# Set up the Variables #
########################

#How many times are we going to "flip 5 coins"?
numtrials     = 1000

#We need a variable to "count" the number of times our event (3 heads) comes up.
oureventcount = 0

######################
# Run the Experiment #
######################

for (j in seq(from=1,to=numtrials,by=1))
{
    # flip a coin 5 times (i.e., generates 5 numbers uniformly distributed between [0,1])
    x = runif(5)
    
    #Let's call the outcome heads if it is larger than 0.5
    #counts the number of heads
    numheads = sum(x>0.5)

    #if we have exactly 3 heads add 1 to oureventcount
    if (numheads == 3)
        oureventcount = oureventcount + 1
}

#output the probability we computed: number of times 3 heads/(total Trials)
empiricalProbability = oureventcount/numtrials;
cat(sprintf("Empirical Probability:= %f\n", empiricalProbability))

#output the true probability we determined with our brains!
trueProbability = 5/16;
cat(sprintf("True Probability:=\t%f\n", trueProbability))

#compare the values
error = (empiricalProbability - trueProbability)^2/trueProbability^2
cat(sprintf("Relative Error^2:=\t%f\n", error))

