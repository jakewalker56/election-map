
#https://www.census.gov/support/USACdataDownloads.html - not used
#https://www.census.gov/mycd/
#https://docs.google.com/spreadsheets/d/1VfkHtzBTP5gf4jAu8tcVQgsBJ1IDvXEHjuMqYlOgYbA/edit#gid=1474862967
#http://www.cnn.com/election/results/exit-polls

library(XLConnect)
library(maps)
library(jsonlite)
setwd("~/github/election-map/data")
election_file_name <- "2016 pres-by-cd"
demographics_file_name <- "_All_Districts"

electoral.college <- read.csv("electoral-college-votes.csv", header=FALSE)
names(electoral.college) <- c("State", "electoral_votes")

states = c(
  "Alabama",
  "Alaska",
  "Arizona",
  "Arkansas",
  "California",
  "Colorado",
  "Connecticut",
  "Delaware",
  "District of Columbia",
  "Florida",
  "Georgia",
  "Hawaii",
  "Idaho",
  "Illinois",
  "Indiana",
  "Iowa",
  "Kansas",
  "Kentucky",
  "Louisiana",
  "Maine",
  "Maryland",
  "Massachusetts",
  "Michigan",
  "Minnesota",
  "Mississippi",
  "Missouri",
  "Montana",
  "Nebraska",
  "Nevada",
  "New Hampshire",
  "New Jersey",
  "New Mexico",
  "New York",
  "North Carolina",
  "North Dakota",
  "Ohio",
  "Oklahoma",
  "Oregon",
  "Pennsylvania",
  "Rhode Island",
  "South Carolina",
  "South Dakota",
  "Tennessee",
  "Texas",
  "Utah",
  "Vermont",
  "Virginia",
  "Washington",
  "West Virginia",
  "Wisconsin",
  "Wyoming"
)
national_election <- data.frame(Stat=c(), County=c(), CD=c(), Clinton=c(), Trump=c())
for(state in states){
  #files are organized into sections depending on whether counties are split between
  #districts and potentially absentee/provisional votes
  #we want to store one row for each state/county/district tuple
  #If there is no numeric district row, it means it's a total, and we should discard it
  print(state)
  state_election = read.csv(paste("election/", state, " ", election_file_name, ".csv", sep=""))
  state_election <- state_election[,grep("County|CD|Clinton|Trump", colnames(state_election))]
  state_election <- state_election[state_election$County != "",]
  state_election <- state_election[state_election$CD != "",]
  state_election <- state_election[!is.na(as.numeric(as.character(state_election$CD))),]
  state_election$County <- as.character(state_election$County)
  state_election$CD <- factor(state_election$CD)
  
  #remove commas
  state_election$Clinton <- as.numeric(unlist(lapply(strsplit(as.character(state_election$Clinton), ","), function(x) {paste(x, sep="", collapse="")})))
  state_election$Trump <- as.numeric(unlist(lapply(strsplit(as.character(state_election$Trump), ","), function(x) {paste(x, sep="", collapse="")})))
  
  state_election[grep("(pt.)", state_election$County),]$County <-
    substr(state_election[grep("(pt.)", state_election$County),]$County, 0,
           nchar(as.character(state_election[grep("(pt.)", state_election$County),]$County))-6)
  state_election$State = state
  national_election = rbind(national_election, state_election, make.row.names=FALSE) 
}

age_labels <- c("Under 5 years",                                                             
  "5 to 9 years",
  "10 to 14 years",
  "15 to 19 years",
  "20 to 24 years",
  "25 to 34 years",
  "35 to 44 years",
  "45 to 54 years",
  "55 to 59 years",
  "60 to 64 years",
  "65 to 74 years",
  "75 to 84 years",
  "85 years and over")

gender_labels <- c(
  "Male", 
  "Female")

race_labels <- c(
  "White",
  "Hispanic or Latino (of any race)", 
  "Black or African American",
  "American Indian and Alaska Native",
  "Asian",
  "Native Hawaiian and Other Pacific Islander",
  "Some other race",
  "Two or more races")

labels <- list()
labels[["race"]] <- race_labels
labels[["gender"]] <- gender_labels
labels[["age"]] <- age_labels

national_population <- data.frame(CD=c(), Label=c(), Count=c(), State=c())
national_ages <- data.frame(CD=c(), Label=c(), Count=c(), State=c())
national_genders <- data.frame(CD=c(), Label=c(), Count=c(), State=c())
national_races <- data.frame(CD=c(), Label=c(), Count=c(), State=c())


for(state in states){
  #deal with underscores
  state_file = paste(unlist(strsplit(state, " ")), collapse="_")

  state_demographics = read.csv(paste("demographics/", state_file, demographics_file_name, ".csv", sep=""))
  state_demographics <- state_demographics[,grep("Value|Topic|Title|Subject", colnames(state_demographics))]
  state_demographics <- state_demographics[state_demographics$Topic %in% c("People", "Education"),]
  state_demographics <- state_demographics[state_demographics$Title != "Total population" | state_demographics$Subject == "Sex and Age",]
  
  state_demographics[state_demographics$Title == "Total population",]
  
  state_population <- data.frame(CD=c(), Label=c(), Count=c())
  state_ages <- data.frame(CD=c(), Label=c(), Count=c())
  state_genders <- data.frame(CD=c(), Label=c(), Count=c())
  state_races <- data.frame(CD=c(), Label=c(), Count=c())
  colnames(state_demographics)
  for(district in colnames(state_demographics)[grep("Value", colnames(state_demographics))])
  {
    d <- strsplit(district, "\\.")[[1]][2]
    for(label in c(
      "Total population")) {
      state_population <- rbind(state_population,
                                data.frame(CD=c(d), 
                                           Label = c(label), 
                                           Count = c(
                                             as.numeric(as.character(state_demographics[state_demographics$Title == label,district]))
                                           )))
      }
    for(label in race_labels) {
      count <- as.numeric(as.character(state_demographics[state_demographics$Title == label,district]))
      #note Hispanic people sometimes self-identify as "white", sometimes as "other", sometimes as "two or more races" 
      #It is non-trivial to adjust the numbers to make sure all three categories are >0
      #however, we can do it realtively easily for the two major categories.
      #There is one specific district (NY - 15) for which this strategy fails, and we special case that at the end
      if(label == "White"){
        latino = as.numeric(as.character(state_demographics[state_demographics$Title == "Hispanic or Latino (of any race)",district]))
        other = as.numeric(as.character(state_demographics[state_demographics$Title == "Some other race", district]))
        #assume latinos report white or other at the same ratio white/other shows up in their district
        white_percent = count / (other + count)
        #we treat white and latino as separate buckets
        white_total <- count - white_percent * latino
        other_total <- other - (1 - white_percent) * latino
        if(white_total < 0){
          #assume the other bucket will take any of our negative values.
          #This breaks if some people report differently than
          #we expect (e.g. two or more races)
          white_total <- 0
        }
        if(other_total < 0) {
          #if our allocation drops the other bucket under 0, take those 
          #people from the white bucket instead
          white_total <- white_total + other_total 
        }
        count <- white_total
      }
      if(label == "Some other race"){
        latino = as.numeric(as.character(state_demographics[state_demographics$Title == "Hispanic or Latino (of any race)",district]))
        white = as.numeric(as.character(state_demographics[state_demographics$Title == "White", district]))
        #assume latinos report white or other at the same ratio white/otehr shows up in their district
        other_percent = count / (white + count)
        other_total <- count - other_percent * latino
        white_total <- white - (1 - other_percent) * latino
        if(other_total < 0){
          #assume the white bucket will take any of our negative values.
          #This breaks if some people report differently than
          #we expect (e.g. two or more races)
          other_total <- 0
        }
        if(white_total < 0) {
          #if this allocation drops the other bucket under 0, take those 
          #people from the white bucket instead
          other_total <- other_total + white_total 
        }
        count <- other_total
      }
      state_races <- rbind(state_races,
                           data.frame(CD=c(d), 
                                      Label = c(label), 
                                      Count = c(count)))
      }
    for(label in age_labels) {
      state_ages <- rbind(state_ages,
                          data.frame(CD=c(d), 
                                     Label = c(label), 
                                     Count = c(
                                       as.numeric(as.character(state_demographics[state_demographics$Title == label,district]))
                                     )))
      }
    for(label in gender_labels) {
      state_genders <- rbind(state_genders,
                             data.frame(CD=c(d), 
                                        Label = c(label), 
                                        Count = c(
                                          as.numeric(as.character(state_demographics[state_demographics$Title == label,district]))
                                        )))
    }
    # state_races <- rbind(state_races,
    #                        data.frame(CD=c(d), 
    #                                   Label = "Other", 
    #                                   Count = c(
    #                                     state_population[state_population$CD == d, "Count"] - sum(state_races[state_races$CD == d, "Count"])
    #                                   )))
    
  }
  if(state == "New York"){
    #special case the time we know our estimation strategy doesn't work
    state_races[state_races$CD == "15" & state_races$Label == "Two or more races",]$Count <-
      state_races[state_races$CD == "15" & state_races$Label =="Two or more races",]$Count + 
      #this is a negative number
      state_races[state_races$CD == "15" & state_races$Label == "White",]$Count +
      #this is a negative number
      state_races[state_races$CD == "15" & state_races$Label == "Some other race",]$Count
    
    state_races[state_races$CD == "15" & state_races$Label == "Some other race",]$Count <- 0
    state_races[state_races$CD == "15" & state_races$Label == "White",]$Count <- 0
  }
  state_ages$State = state
  state_genders$State = state
  state_races$State = state
  
  state_population$State = state
  #aggregate voter behavior to the district level
  national_ages <- rbind(national_ages, state_ages)
  national_genders <- rbind(national_genders, state_genders)
  national_races <- rbind(national_races, state_races)
  national_population <- rbind(national_population, state_population)
}

#merge election rows with the same state/district values 
temp_election <- data.frame(State=c(), CD=c(), Clinton=c(), Trump=c())
for(state in unique(national_election$State)){
  for(district in unique(national_election[national_election$State == state,"CD"])){
    trump = sum(national_election$Trump[national_election$State == state & national_election$CD == district])
    clinton = sum(national_election$Clinton[national_election$State == state & national_election$CD == district])
    temp_election <- rbind(temp_election, data.frame(State = state, CD = district, Trump = trump, Clinton = clinton))
  }
}
national_election <- temp_election

#remove leading 0 from district values by convert to strings, casting to 
#numbers, then releveling.  There is probably a better way to do this, but
#whatever, it works
national_ages$CD <- as.factor(as.double(as.character(national_ages$CD)))
national_population$CD <- as.factor(as.double(as.character(national_population$CD)))
national_races$CD <- as.factor(as.double(as.character(national_races$CD)))
national_genders$CD <- as.factor(as.double(as.character(national_genders$CD)))
national_election$CD <- as.factor(as.double(as.character(national_election$CD)))

demos <- list(
  "gender" = cbind(national_genders, InterpolatedTrump=0, InterpolatedClinton=0),
  "race" = cbind(national_races, InterpolatedTrump=0, InterpolatedClinton=0),
  "age" = cbind(national_ages, InterpolatedTrump=0, InterpolatedClinton=0)
)

vote_share <- list(
  "gender" = read.csv("turnout/gender_vote_share.csv"),
  "age" = read.csv("turnout/age_vote_share.csv"),
  "race" = read.csv("turnout/race_vote_share.csv")
)

turnout <- list ()
for(demo in c("gender", "age", "race")){
  #to estimate the voting behavior of people in this demographic grouping,
  #we use known voting behavior of demographics combined with
  #known population data in this area
  
  #specifically, we first figure out the implied voting turnout from each demo
  for(x in unique(demos[[demo]]$Label)){
    #how many people are in this demo nationally
    demographic_population <- sum(demos[[demo]][demos[[demo]]$Label == x,"Count"])
    
    #total votes nationally across all demos
    total_votes <- sum(national_election$Trump) + sum(national_election$Clinton)
    
    #total votes in this demo based on exit polls
    total_demographic_voters <- vote_share[[demo]][vote_share[[demo]]$Label == x,]$voteshare * total_votes
    
    #what percent of this demo turns out to vote
    turnout[[demo]][x] <- total_demographic_voters / demographic_population
  }
  
  #Next we come up with an "expected number of votes" in each State/District combo 
  #based on demographic behavior and actual observed turnout for each demographic
  #group of this category.  Then we find the multiplier required
  #to get us from expected to actual observed turnout.
  demographic_projection <<- data.frame(CD=c(), State=c(), 
                                        ProjectedTrump=c(), ProjectedClinton=c(),
                                        ActualTrump=c(), ActualClinton=c())
  sapply(1:nrow(national_election), function(x){
      expected_trump_votes <- 0
      expected_clinton_votes <- 0
      for(i in unique(demos[[demo]]$Label)){
        #total people in this state/district in this demo * voter turnout rate * proportion voting for Trump/Clinton
        expected_trump_votes <- expected_trump_votes +
          demos[[demo]][demos[[demo]]$CD == national_election[x,]$CD & demos[[demo]]$State == national_election[x,]$State & demos[[demo]]$Label == i,]$Count *
          turnout[[demo]][i] *
          vote_share[[demo]][vote_share[[demo]]$Label == i,]$Trump
        expected_clinton_votes <- expected_clinton_votes +
          demos[[demo]][demos[[demo]]$CD == national_election[x,]$CD & demos[[demo]]$State == national_election[x,]$State & demos[[demo]]$Label == i,]$Count *
          turnout[[demo]][i] *
          vote_share[[demo]][vote_share[[demo]]$Label == i,]$Clinton
      }
      multiplier <- (national_election[x,"Trump"] + national_election[x,"Clinton"])/(expected_trump_votes + expected_clinton_votes)
      margin_shift <- 2 * (multiplier * expected_trump_votes - national_election[x,"Trump"])/((national_election[x,"Trump"] + national_election[x,"Clinton"]))
      
      demographic_projection <<- rbind(demographic_projection, data.frame(
        CD = c(national_election[x,"CD"]),
        State = c(as.character(national_election[x,"State"])),
        ProjectedTrump = c(expected_trump_votes),
        ProjectedClinton = c(expected_clinton_votes),
        ActualTrump = c(national_election[x,"Trump"]),
        ActualClinton = c(national_election[x,"Clinton"]),
        VoteMultiplier = multiplier,
        MarginShift = margin_shift
        ), make.row.names=FALSE)
    }
  ) 
  sapply(1:nrow(demos[[demo]]), function(x){
    vote_multiplier <- demographic_projection[demographic_projection$CD == demos[[demo]][x,]$CD & demographic_projection$State == demos[[demo]][x,]$State,]$VoteMultiplier
    margin_shift <- demographic_projection[demographic_projection$CD == demos[[demo]][x,]$CD & demographic_projection$State == demos[[demo]][x,]$State,]$MarginShift
    people_in_demo <- demos[[demo]][x,]$Count
    turnout_rate <- turnout[[demo]][demos[[demo]][x,"Label"]]
    expected_clinton_percentage <- vote_share[[demo]][vote_share[[demo]]$Label == demos[[demo]][x,]$Label,]$Clinton
    expected_trump_percentage <- vote_share[[demo]][vote_share[[demo]]$Label ==  demos[[demo]][x,]$Label,]$Trump
    
    #Note interopolated votes can be negative
    #because we are applyinig a uniform margin shift for an entire district
    #across all demographics, if one demographic voted very strongly in one
    #direction, the overall shift could make them negative
    #this mostly happens with Black and African Americans, 92% of whom voted
    #for democrats.  If a distric voted more heavily than their demographics
    #indicate for democrats, the Black vote for Trump will appear negative
    #To solve this, we'd have to find a non-linear way of updating demographic
    #margins. 
    
    #Specifically we need a transform that preserves total number of votes
    #across all demos but doesn't push any of them negative. You could do something
    #like find the percentage shift of the available vote share, rather than
    #a flat shift in margin.  For example:
    #if African Amaericans vote 92% democrat, then any shfit in African American
    #voters would be "X% of the remaining 8%", and any shift in whites would be 
    #"X% of the remaining 40%". Then we would solve for the X that creates the
    #corrct total number of votes for each group.  Solving for X would be 
    #easy, since the total vote shift goes linearly with X
    
    #find the total % change we need
    #demos[[demo]][x,]$InterpolatedTrump <<- people_in_demo * turnout_rate * vote_multiplier * (expected_trump_percentage + (1 - expected_trump_percentage) * x_shift)
    #demos[[demo]][x,]$InterpolatedClinton <<- people_in_demo * turnout_rate * vote_multiplier * (expected_clinton_percentage + (1 - expected_clinton_percentage) * x_shift)
    
    #change each demo interpolated value from expected value by taking the 
    #proportion of available slack and multiplying it by the desired margin shift
    rep_vote_slack = 0;
    dem_vote_slack = 0;
    for(label in labels[[demo]])
    {
      inner_people_in_demo <- demos[[demo]][demos[[demo]]$CD == demos[[demo]][x,]$CD & demos[[demo]]$State == demos[[demo]][x,]$State & demos[[demo]]$Label == label,]$Count
      inner_turnout_rate <- turnout[[demo]][label]
      inner_expected_clinton_percentage <- vote_share[[demo]][vote_share[[demo]]$Label == label,]$Clinton
      inner_expected_trump_percentage <- vote_share[[demo]][vote_share[[demo]]$Label == label,]$Trump
      dem_vote_slack = dem_vote_slack + inner_people_in_demo * inner_turnout_rate * vote_multiplier * inner_expected_clinton_percentage
      rep_vote_slack = rep_vote_slack + inner_people_in_demo * inner_turnout_rate * vote_multiplier * inner_expected_trump_percentage
    }
    voted_dem <- people_in_demo * turnout_rate * vote_multiplier * expected_clinton_percentage
    voted_rep <- people_in_demo * turnout_rate * vote_multiplier * expected_trump_percentage
    
    if(margin_shift > 0) {
      #shift democrat
      shift <- (voted_rep / rep_vote_slack) * margin_shift * (dem_vote_slack + rep_vote_slack)/2
    } else {
      #shift republican
      shift <- (voted_dem / dem_vote_slack) * margin_shift * (dem_vote_slack + rep_vote_slack)/2
    }
    demos[[demo]][x,]$InterpolatedTrump <<- voted_rep - shift
    demos[[demo]][x,]$InterpolatedClinton <<- voted_dem + shift
    if(demos[[demo]][x,]$InterpolatedClinton < 0 | demos[[demo]][x,]$InterpolatedTrump < 0){
       print(paste("shifted", demos[[demo]][x,]$State, "-", demos[[demo]][x,]$CD, 
                "(", demos[[demo]][x,"Label"],") vote by", shift, 
                ": dem-", demos[[demo]][x,]$InterpolatedClinton, "(", voted_dem, ")",
                ": rep-", demos[[demo]][x,]$InterpolatedTrump, "(", voted_rep, ")"))
      print(voted_dem)
      print(voted_rep)
      print(dem_vote_slack)
      print(rep_vote_slack)
      print(margin_shift)
    }
    #40%, 60%, 80%
    #60%->63%
    #.03/3 + .03/3 + .03/3
    #(.03*.6/3 + .03*.4/3 + .03*.2/3) /(.6/3 + .4/3 + .2/3)
    #(.03)*(.6/3)/(.6/3 + .4/3 + .2/3)
    #demos[[demo]][x,]$InterpolatedTrump <<- people_in_demo * turnout_rate * vote_multiplier * (expected_trump_percentage - margin_shift/2)
    #demos[[demo]][x,]$InterpolatedClinton <<- people_in_demo * turnout_rate * vote_multiplier * (expected_clinton_percentage + margin_shift/2)
  })
}

#verify these should all equal zero...
sum(demos[["race"]]$InterpolatedTrump) - sum(national_election$Trump)
sum(demos[["race"]]$InterpolatedClinton) - sum(national_election$Clinton)

sum(demos[["race"]]$InterpolatedTrump) - sum(national_election$Trump)
sum(demos[["gender"]]$InterpolatedTrump) - sum(national_election$Trump)
sum(demos[["gender"]]$InterpolatedClinton) - sum(national_election$Clinton)

sum(demos[["age"]]$InterpolatedTrump) - sum(national_election$Trump)
sum(demos[["age"]]$InterpolatedClinton) - sum(national_election$Clinton)


evaluate <- function(aggregation=c("electoral", "population"), 
                      election_data, 
                      demographic_data, 
                      population_data,
                      modifiers,
                      normalization=c("model", "uniform")){
  #score election data
  for(modifier in modifiers){
    #for each row in election data
    helped_rep <- sapply(1:nrow(election_data), function(x, modifier, normalization){
      if(x %% 1000 == 0) {
        print(paste("processing row", x))
      }
      rep_demo_votes = demographic_data[[modifier[[1]]]][
        demographic_data[[modifier[[1]]]]$CD == election_data[x,]$CD &
        demographic_data[[modifier[[1]]]]$State == election_data[x,]$State &
        demographic_data[[modifier[[1]]]]$Label == modifier[[2]]
          ,]$InterpolatedTrump
      dem_demo_votes = demographic_data[[modifier[[1]]]][
        demographic_data[[modifier[[1]]]]$CD == election_data[x,]$CD &
        demographic_data[[modifier[[1]]]]$State == election_data[x,]$State &
        demographic_data[[modifier[[1]]]]$Label == modifier[[2]]
        ,]$InterpolatedClinton
      if(modifier[3] == "margin"){
        net_change = (rep_demo_votes + dem_demo_votes) * as.double(modifier[4])/2
        election_data[x,"Clinton"] <<- election_data[x,"Clinton"] + net_change
        election_data[x,"Trump"] <<- election_data[x,"Trump"] - net_change
      } else if (modifier[3] == "turnout") {
        #this doesn't really do anything, because we assume demo share 
        vote_multiple = as.double(modifier[4])
        election_data[x,"Clinton"] <<- election_data[x,"Clinton"] + vote_multiple * dem_demo_votes
        election_data[x,"Trump"] <<- election_data[x,"Trump"] + vote_multiple * rep_demo_votes
        return(rep_demo_votes > dem_demo_votes)
      }
    }, modifier, normalization)
    #print(paste("this modifier helped", helped_rep, "repulican districts (", nrow(election_data) - helped_rep,"democrat districts)"))
    print(summary(helped_rep))
  }
  if(aggregation == "electoral")
    {
    rep.total = 0
    dem.total = 0
    for(state in unique(election_data$State)){
      rep.votes = sum(election_data[election_data$State == state,"Trump"])
      dem.votes = sum(election_data[election_data$State == state,"Clinton"])
      if(state == "Maine" || state == "Nebraska"){
        #"Maine and Nebraska ... allocate two electoral votes to the state 
        # popular vote winner, and then one electoral vote to the popular 
        #vote winner in each Congressional district"
        #http://www.270towin.com/content/split-electoral-votes-maine-and-nebraska/
        
        if(rep.votes > dem.votes){
          rep.total <- rep.total + 2
        } else {
          dem.total <- dem.total + 2
        }
        for(district in unique(election_data[election_data$State == state,]$CD)){
          rep.votes = sum(election_data[election_data$State == state & election_data$CD == district,"Trump"])
          dem.votes = sum(election_data[election_data$State == state & election_data$CD == district,"Clinton"])
          
          if(rep.votes > dem.votes){
            rep.total <- rep.total + 1
          } else {
            dem.total <- dem.total + 1
          }
        }
      }
      else if(rep.votes > dem.votes){
        rep.total <- rep.total + electoral.college[electoral.college$State == state, "electoral_votes"]
      } else {
        dem.total <- dem.total + electoral.college[electoral.college$State == state, "electoral_votes"]
      }
    }
    print(paste("Total votes cast:", sum(election_data$Trump + election_data$Clinton)))
    return(data.frame(rep.votes = rep.total, dem.votes = dem.total))
  } else
  {
    return("ERROR")
  }
}

millenial_turnout_increase = 0.17
modifiers <- list(
  c("age", "25 to 34 years", "turnout", millenial_turnout_increase),
  c("age", "20 to 24 years", "turnout", millenial_turnout_increase),
  c("age", "15 to 19 years", "turnout", millenial_turnout_increase)
  #c("race", "White", "margin", 0.7)
  #c("race", "Hispanic or Latino (of any race)", "turnout", 0.0)
)
evaluate(aggregation="electoral", election_data = national_election, demographic_data = demos, population_data = national_population, modifiers = modifiers, normalization="uniform")

write(toJSON(national_election, digits=8), "election_data.json")
write(toJSON(demos, digits=8), "demographic_data.json")
write(toJSON(national_population, digits=8), "population_data.json")
write(toJSON(electoral.college, digits=8), "electoral_college_data.json")

election_tree = list()
for(state in unique(national_election$State)){
  election_tree[[state]] = list()
  for(cd in unique(national_election[national_election$State == state,]$CD)){
    election_tree[[state]][[cd]] = list()
    election_tree[[state]][[cd]][["Trump"]] = national_election[national_election$State == state & national_election$CD == cd,]$Trump
    election_tree[[state]][[cd]][["Clinton"]] = national_election[national_election$State == state & national_election$CD == cd,]$Clinton
  }
}
population_tree = list()
for(state in unique(national_population$State)){
  population_tree[[state]] = list()
  for(cd in unique(national_population[national_population$State == state,]$CD)){
    population_tree[[state]][[cd]] = list()
    population_tree[[state]][[cd]][["Total"]] = national_population[national_population$State == state & national_population$CD == cd,]$Count
  }
}

demographic_tree = list()
#first create the empty lists that can't be overwritten
for(demo in c("gender")){
  for(state in unique(demos[[demo]]$State)){
    demographic_tree[[state]] = list()
    for(cd in unique(demos[[demo]][demos[[demo]]$State == state,]$CD)){
      demographic_tree[[state]][[cd]] = list()
    }
  }
}
for(demo in c("gender", "age", "race")){
  for(state in unique(demos[[demo]]$State)){
    for(cd in unique(demos[[demo]][demos[[demo]]$State == state,]$CD)){
      demographic_tree[[state]][[cd]][[demo]] = list()
      for(label in unique(demos[[demo]][demos[[demo]]$State == state & demos[[demo]]$CD == cd,]$Label)){
        demographic_tree[[state]][[cd]][[demo]][[label]] = list();
        demographic_tree[[state]][[cd]][[demo]][[label]][["InterpolatedTrump"]] = demos[[demo]][demos[[demo]]$State == state & demos[[demo]]$CD == cd & demos[[demo]]$Label == label,]$InterpolatedTrump  
        demographic_tree[[state]][[cd]][[demo]][[label]][["InterpolatedClinton"]] = demos[[demo]][demos[[demo]]$State == state & demos[[demo]]$CD == cd & demos[[demo]]$Label == label,]$InterpolatedClinton  
      }
    }
  }
}

electoral_college_tree = list()
for(state in unique(electoral.college$State)){
  electoral_college_tree[[state]] = electoral.college[electoral.college$State == state,]$electoral_votes
}

write(toJSON(election_tree, digits=10), "election_tree.json")
write(toJSON(population_tree, digits=10), "population_tree.json")
write(toJSON(demographic_tree, digits=10), "demographic_tree.json")
write(toJSON(electoral_college_tree, digits=10), "electoral_college_tree.json")

#https://gist.github.com/glamp/223ee1b5665b8ab8d90fc2786f4b6b44
#http://bl.ocks.org/michellechandra/0b2ce4923dc9b5809922
