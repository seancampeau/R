#2 Servers in Parallel

#Replace With Desired Mean Checkout Times (Default = 10 Minutes)
#Default is Poisson-Distributed Checkout Times
CheckoutTime <- 10
MeanWaitingTime <- numeric()

#Generate Poisson Process for Arrivals
for (n in 1:7) {
  TotalArrivals8to9 <- rpois(1, 4)
  TotalArrivals9to10 <- rpois(1, 4)
  TotalArrivals10to11 <- rpois(1, 4)
  TotalArrivals11to12 <- rpois(1, 4)
  TotalArrivalsLunch <- rpois(1, 6)
  TotalArrivals13to14 <- rpois(1, 4)
  TotalArrivals14to15 <- rpois(1, 4)
  TotalArrivals15to16 <- rpois(1, 4)
  TotalArrivals16to17 <- rpois(1, 4)

#Generate Uniform Distribution for Poisson Counts
  ArrivalTimes <- c(runif(TotalArrivals8to9, 8, 9), 
    runif(TotalArrivals9to10, 9, 10), runif(TotalArrivals10to11, 10, 11), 
    runif(TotalArrivals11to12, 11, 12), runif(TotalArrivalsLunch, 12, 13), 
    runif(TotalArrivals13to14, 13, 14), runif(TotalArrivals14to15, 14, 15),
    runif(TotalArrivals15to16, 15, 16), runif(TotalArrivals16to17, 16, 17))

#Generate People Who Wait
  ArrivalTimesSorted <- numeric()
  ArrivalTimesSorted <- sort(ArrivalTimes, decreasing = FALSE)
  ArrivalTimesSortedNew <- ArrivalTimesSorted

#Generate Mean Waiting Time 
#To Change Checkout Time Distribution,
#replace the "rpois" with desired Distribution and Parameters
  pois <- numeric()
  pois1 <- numeric()
  for (i in 1:(length(ArrivalTimesSorted) - 2)) {
    pois[i] <- (rpois(1, CheckoutTime)) / 60
    pois1[i] <- (rpois(1, CheckoutTime)) / 60
    if (ArrivalTimesSorted[i + 2] <= (min(ArrivalTimesSorted[i] +
      pois[i], ArrivalTimesSorted[i + 1]) + pois[i])) {
        ArrivalTimesSorted[i + 2] <- (min(ArrivalTimesSorted[i] +
          pois[i], ArrivalTimesSorted[i + 1]) + pois1[i])
        }
    WaitingTime <- ArrivalTimesSorted - ArrivalTimesSortedNew
    MeanWaitingTime[n] <- mean(WaitingTime) * 60
    }
  }

#7-Day Average Waiting Time
mean(MeanWaitingTime)

#2 Servers in Tandem

#Replace With Desired Checkout Time (Default = 10 Minutes)
CheckoutTime <- 10
MeanWaitingTime <- numeric()

#Generate Poisson Process
for (n in 1:7) {
  TotalArrivals8to9 <- rpois(1, 4)
  TotalArrivals9to10 <- rpois(1, 4)
  TotalArrivals10to11 <- rpois(1, 4)
  TotalArrivals11to12 <- rpois(1, 4)
  TotalArrivalsLunch <- rpois(1, 6)
  TotalArrivals13to14 <- rpois(1, 4)
  TotalArrivals14to15 <- rpois(1, 4)
  TotalArrivals15to16 <- rpois(1, 4)
  TotalArrivals16to17 <- rpois(1, 4)

#Generate Uniform Distribution
  ArrivalTimes <- c(runif(TotalArrivals8to9, 8, 9), 
    runif(TotalArrivals9to10, 9, 10), runif(TotalArrivals10to11, 10, 11), 
    runif(TotalArrivals11to12, 11, 12), runif(TotalArrivalsLunch, 12, 13), 
    runif(TotalArrivals13to14, 13, 14), runif(TotalArrivals14to15, 14, 15), 
    runif(TotalArrivals15to16, 15, 16), runif(TotalArrivals16to17, 16, 17))

#Generate People Who Wait
  ArrivalTimesSorted <- numeric()
  ArrivalTimesSorted <- sort(ArrivalTimes, decreasing = FALSE)
  ArrivalTimesSortedNew <- ArrivalTimesSorted

#Generate Mean Waiting Time 
#To Change Checkout Time Distribution,
#replace "rpois" with desired Distribution and Parameters
  pois <- numeric()
  pois1 <- numeric()
  for (i in 1) {
    pois[i] <- ((rpois(1, CheckoutTime)) / 60)
    if (ArrivalTimesSorted[i + 1] <= ArrivalTimesSorted[i] + pois[i]) {
      ArrivalTimesSorted[i + 1] <- ArrivalTimesSorted[i] + pois[i]
      }
    }
  for (i in 1:(length(ArrivalTimesSorted) - 2)) {
    pois[i] <- ((rpois(1, CheckoutTime)) / 60)
    pois1[i] <- ((rpois(1, CheckoutTime)) / 60)
    if (ArrivalTimesSorted[i + 1] <= ArrivalTimesSorted[i] + pois[i]) {
      ArrivalTimesSorted[i + 1] <- ArrivalTimesSorted[i] + pois[i]
      }
    if (ArrivalTimesSorted[i + 2] <= ArrivalTimesSorted[i + 1] + pois1[i]) {
      ArrivalTimesSorted[i + 2] <- ArrivalTimesSorted[i + 1] + pois1[i]
      }
    WaitingTime <- ArrivalTimesSorted - ArrivalTimesSortedNew
    MeanWaitingTime[n] <- (mean(WaitingTime) * 60)
    }
  }
mean(MeanWaitingTime)

#Generating Beta Random Quantities

#Use Accept/Reject to generate Beta(a, b) random variates.
#We consider only the case a, b > 1 so that the density is bounded.
#Implement this in R.

#Give Desired Parameter Values (defaults = 2)
a <- 2
b <- 2
#Rescaling Coefficient (default = 2)
c <- 2

#Give Desired Number of Repetitions (default = 1000000)
x <- 1000000
vector <- numeric()

#Program
for (i in 1:x) {
  unif1 <- runif(1, 0, 1)
  unif2 <- runif(1, 0, 1)
  if (unif2 <= (dbeta(unif1, a, b) / ( c * dunif(unif1)))) {
    vector[i] <- unif1
    }
  }
vector <- vector[!is.na(vector)]
plot(density(vector))