# Dice roller v1.0 by Mike Frish
#Enter complex dice-roll specifications to simulate, such as: 2d20 + d7 - 1d6 + 22d3 - 14

#set this to true/false to show some additional details in outputs
debugmode <- FALSE

dorolls <- function() {
    rollcount <- 1
    while (TRUE) {
        x <- getspecs(rollcount)
        y <- getshowoption()
        diceroll(x, y)
        rollcount <- rollcount + 1
    }
}

#sanitizes specs into the format 2d5-d20+7  etc., removes blank spaces, adds 1 to unspecified roll quantities, checks for invalid inputs
getspecs <- function(count) {

    #use a different prompt for the 1st time vs all subsequent roll requests
    if (count == 1) {
        rollspecs <- readline(prompt = "\nDice roller v1.0 by Mike Frish\nEnter complex dice-roll specifications to simulate, such as: 2d20 + d7 - 1d6 + 22d3 - 14
    \nRoll specifications: ")
    }
    else if (count > 1) {
        rollspecs <- readline(prompt = "")
    }


    rollspecslower <- tolower(rollspecs) #get it in lowercase

    #in the special case where there are spaces but not +/-, replace spaces with +'s, allowing entry like: 3d5 d20 15 to be seen as 3d5+d20+15
    if (gsub('[0-9d ]', '', rollspecslower) == "") { rollspecslower <- gsub(' ','+',rollspecslower) }

    rollspecslower <- gsub(' ','',rollspecslower) #remove all white space

    #validity check, remove all of: [1-9, d, +, -], if anything remains then there are invalid characters
    validitycheck <- rollspecslower
    validitycheck <- gsub('[0-9d+-]', '', validitycheck) #remove all numbers, d's, and +/-
    if (nchar(validitycheck) > 0) { return(paste("Invalid characters in roll specs:", validitycheck)) }

    rollspecslower <- gsub('[d]+','d',rollspecslower) #reduce multiple d's in a row with a single d

    return(rollspecslower)



    # add a special case where something like 'd20 5d3 2d4' interprets the spaces as +'s if there are no +'s/-'s


}

#allow y,Y,yes,Yes,true,TRUE,True or n,N,No,no,false,FALSE,False as responses to the show-specs prompt
getshowoption <- function() {
    showdetail <- readline(prompt = "Show roll details: ") 
    if (showdetail %in% c('y', 'Y', 'yes', 'Yes', 'YES', 'true', 'TRUE', 'True')) { return(TRUE) }
    if (showdetail %in% c('n', 'N', 'no', 'No', 'NO', 'false', 'FALSE', 'False')) { return(FALSE) }
    else { return(paste("Invalid response:", showdetail)) }
}

#do a dice-roll of a dicesize-sided dice (integers only!)
roll <- function(dicesize) { return(sample(1:dicesize,1)) } #make sure to specify a sample of 1, otherwise it multiplies the results

diceroll <- function(dr.rollspecs, dr.showdetail) {
    if (substr(dr.rollspecs, 1, 1) == "I") #if the roll is invalid...
        { return(print(dr.rollspecs)) }
    if (substr(dr.showdetail, 1, 1) == "I") #if the detail option is invalid...
        { dr.showdetail <- FALSE
        print("Invalid show-detail response, defaulting to False") }

    #dice roll shenanigans begin here
    #split the dice roll instructions into terms
    rollterms <- strsplit(dr.rollspecs, split = '[+-]') #separate the actual terms
    rollmultipliers <- gsub('[0-9d]', '', dr.rollspecs) #reduce to just + & -'s for multipliers
    rolltermcount <- length(rollterms[[1]]) #get count of separate roll terms

    #create an array for our roll specifications:
    #   Each row represents a separate roll term like: +2d7
    #   column 1 = +/- multiplier, column 2 = # of dice to roll, column 3 = size of dice to roll
    rollarray <- array(0, dim = c(rolltermcount,3))
    colnames(rollarray) = c("+/-","Count","Size")

    #load column 1, the +/- multipliers
    rollarray[1,1] <- 1 #set the first term multiplier to 1, if the roll specs begin with - then the 1st term will be ""
    if (nchar(rollmultipliers) != 0) { #only do this if we actually have any terms, otherwise a single roll like "d20" gives an error
        for (i in 1:nchar(rollmultipliers)) {
            #only need to fill in terms 2-to-termcount
            if (substr(rollmultipliers, i, i) == "+") { rollarray[i + 1, 1] <- 1 } else { rollarray[i + 1, 1] <- -1 }
            #if (substr(rollmultipliers, i, i) == "-") { rollarray[i + 1, 1] <- -1 }
        }
    }

    #parse through the terms and enter them into the array
    rollterms <- unlist(rollterms) #break the rollterms from a list into an array to allow access to individual entries
    for (i in 1:rolltermcount) {
        currterm <- unlist(strsplit(rollterms[[i]], split = 'd')) #split term into parts
        #check for special cases, these are mutually exclusive
        if (length(currterm) == 1) #we have a flat addition/subtraction value, jam this in the count (column 2), leave dice size as 0
            { rollarray[i, 2] <- strtoi(currterm[[1]]) }
        else if (length(currterm) > 1) #we have an actual dice roll ?d? (we ignore terms after the 2nd, i.e. 2d6d7 -> 2d6
            { 
            if (currterm[[1]] == "" && currterm[[2]] == "") # _d_ = no term, do nothing (value remains 0d0) (not needed)
                { }
            if (currterm[[1]] != "" && currterm[[2]] == "") # ?d_ = an undefined dice size, treat it as a flat addition
                { rollarray[i, 2] <- strtoi(currterm[[1]]) }
            if (currterm[[1]] == "" && currterm[[2]] != "") # _d? = a 'd20' situation, implicitly 1 d?
                {
                if (strtoi(currterm[[2]]) == 0) #special case of _d?, if ? is 0 we have _d0, do nothing
                    { }
                if (strtoi(currterm[[2]]) == 1) #special case of _d?, if ? is 1 we have _d1, treat this as adding a flat 1, i.e. 1d0
                    { rollarray[i, 2] <- 1 }
                if (strtoi(currterm[[2]]) > 1) #the default case, we have _d? where ? > 1 (i.e. d2, d5, etc), treat as 1d?
                    { rollarray[i, 2] <- 1
                    rollarray[i, 3] <- strtoi(currterm[[2]]) }
                }
            if (currterm[[1]] != "" && currterm[[2]] != "") # ?d? = default dice roll
                {
                if (strtoi(currterm[[1]]) == 0) #special case of 0d?, do nothing, no value
                    { }
                else
                    {
                    if (strtoi(currterm[[2]]) == 0) #special case of ?d0 where the 0 actually exists, treat as 0, no change
                        { }
                    if (strtoi(currterm[[2]]) == 1) #special case of ?d1, treat as adding a flat value ?
                        { rollarray[i, 2] <- strtoi(currterm[[1]]) }
                    if (strtoi(currterm[[2]]) > 1) #default case where we have ?d? where the 2nd ? is > 1, ex. 2d6
                        { rollarray[i, 2] <- strtoi(currterm[[1]])
                        rollarray[i, 3] <- strtoi(currterm[[2]]) }
                    }
                }
            }
        }

    #initialize stats & variables for output
    cleanedrollspecs <- ""
    individualrollresulttext <- ""
    totalrollresult <- 0
    minresult <- 0
    maxresult <- 0
    avgresult <- 0

    temprollresult <- 0

    #Do all of the rolls and construct the new strings and statistics
    for (i in 1:rolltermcount) #for each row in the array...
        {
        if (rollarray[i, 2] != 0) #if the 'count' column is non-zero (i.e. it's a valid term, including flat additions)
            {

            if (cleanedrollspecs != "") #check to see if we are continuing the string, if so, add a +/-, otherwise nothing
                {
                if (rollarray[i, 1] == 1) { cleanedrollspecs <- paste(sep = "", cleanedrollspecs, " + ") }
                if (rollarray[i, 1] == -1) { cleanedrollspecs <- paste(sep = "", cleanedrollspecs, " - ") }
                }
            if (individualrollresulttext != "" && rollarray[i, 3] != 0) #likewise add a , to denote the next batch of rolls if valid
            { individualrollresulttext <- paste(sep = "", individualrollresulttext, ", ") }

            if (rollarray[i, 3] > 1) #if this is a valid dice-roll, then do...
                {
                cleanedrollspecs <- paste(sep = "", cleanedrollspecs, as.character(rollarray[i, 2]), "d", as.character(rollarray[i, 3])) #formatting for the cleaned specs
                individualrollresulttext <- paste(sep = "", individualrollresulttext, "d", as.character(rollarray[i, 3]), "(") #formatting for the individual rolls

                for (j in 1:rollarray[i, 2]) #for each dice roll in each roll-term...
                    {
                    temprollresult <- roll(rollarray[i,3]) #do the dice roll, store the result for temporary use here

                    if (j > 1) { individualrollresulttext <- paste(sep = "", individualrollresulttext, ",") }
                    #add ',' if subsequent value
                    totalrollresult <- (totalrollresult + (rollarray[i,1] * temprollresult)) #add the new roll to the running total

                    individualrollresulttext <- paste(sep = "", individualrollresulttext, as.character(temprollresult)) #build up the individual text

                    if (rollarray[i, 1] == 1) #if this term was positive
                        {
                        minresult <- (minresult + 1)
                        avgresult <- (avgresult + ((rollarray[i,3] + 1) / 2))
                        maxresult <- (maxresult + rollarray[i,3])
                        }
                    else if (rollarray[i, 1] == -1) #if this term was negative
                        {
                        minresult <- (minresult - rollarray[i,3])
                        avgresult <- (avgresult - ((rollarray[i,3] + 1) / 2))
                        maxresult <- (maxresult - 1)
                        }
                    }
                individualrollresulttext <- paste(sep = "", individualrollresulttext, ")") #cap off the term's individual results text
                }

            if (rollarray[i, 2] > 0 && rollarray[i, 3] == 0) #if this is a flat addition/subtraction, then do...
                {
                cleanedrollspecs <- paste(sep = "", cleanedrollspecs, as.character(rollarray[i, 2]))

                totalrollresult <- (totalrollresult + (rollarray[i, 2] * rollarray[i, 1]))
                minresult <- (minresult + (rollarray[i, 2] * rollarray[i, 1]))
                avgresult <- (avgresult + (rollarray[i, 2] * rollarray[i, 1]))
                maxresult <- (maxresult + (rollarray[i, 2] * rollarray[i, 1]))
                }
            }
        }

    if (debugmode) {
        print(rollarray)
        cat("\n")} #show the array if debug set to true

    cat(paste(sep = "","Result:   ", totalrollresult, "   Roll Specs:   ", cleanedrollspecs))

    if (dr.showdetail)
    { cat(paste(sep = "", "\nIndividual rolls:   ", individualrollresulttext, "\nMin:  ", minresult, "    Avg:  ", avgresult, "    Max:  ", maxresult)) }

    cat("\n\nYou may continue entering new rolls below:")
}

dorolls()