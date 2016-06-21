library(foreach)
library(doMC)

registerDoMC(cores=8)

############# INITIAL VALUES #############
nofYears <- 10
driveSize <- 3820 # In Gigabytes
nofDrives <- 2*100 #
dwpd <- 3
lifeTime <- dwpd*365 # Expected lifetime of drive in days
iops <-20000# # Average IOPS
writesPerDay <- iops*24*60*60
maxWritesPerDay <- 2*writesPerDay
writesPerDaySD <- 0.2*writesPerDay # standard deviation of the number of writes per day. Assumed to be normally distributed
writeSizez <- c(1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192) # In KiloBytes.
writeWeights <- c(1,2,4,8,16,32,64,32,16,8,4,2,1,1) # Weights for different write sizes
writeSize <- rep(writeSizez,writeWeights)
driveWriteCapacitySD <- driveSize*dwpd*1*365*1000 # This essentially is standard deviation of 1 year
rebuildSize <- 8192
#### Growth parameters
driveSizeYearlyGrowth <- c(3.93,2.50,1.50,1.00,0.50,0.25,0.20,0.15,0.13,0.10) #
yearlyCapacityGrowth <- 0.0 #50% increase in capacity per year
yearlyIOPSGrowth <- 0.00 #30
#### Random failure parameters, failure rate is relative (percentage) to number of drives
averageYearlyFailures <- rep(0,10) #c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05)
dailyFailures <- averageYearlyFailures[1]*nofDrives/365

simID <- sample(1:999999,1)
description <- "noRandomFail_noCapGrowth_noIOPSGrowth"

##Initialize first values

drives <- rep(driveSize,nofDrives)

#Assign total number of MegaBytes that can be written to each drive
driveWriteCapacity <- rnorm(nofDrives, mean = driveSize*dwpd*lifeTime*1000, sd = driveWriteCapacitySD) # The standard deviation here is equivalent to one year
driveWriteCapacity[driveWriteCapacity<0] <- 0
#MegaBytes writteen to disk
cumulativeWrites <- rep(0,nofDrives)

#Number of drive replacements for a given drive slot
replace <- rep(0,nofDrives)

#Days
failures <- rep(0,nofYears*365)
randomFailures <- rep(0,nofYears*365)
dailyWrites <- rep(0,nofYears*365)
calcTimes <- rep(0,nofYears*365)

#Drives add
drivesAdded <- c(nofDrives)

divs <- floor((maxWritesPerDay*4)/(0.5*1024^3)) #Trying to maintain the size of randomDrive vector undex 512MB
divs <- 8
if(divs < 1){ divs <- 1}

#Yearly for loop
for (year in 1:nofYears){

	#Random list of drives
	#randomDrives <- sample(1:nofDrives, ceiling(maxWritesPerDay/nofDrives)+101, replace=T)
	
	#Random write sizes
	ws <- sample(writeSize, ceiling((1+yearlyIOPSGrowth)*maxWritesPerDay/nofDrives)+1001, replace=T)

	#Daily for loop
	for (dayOfYear in 1:365) {
		start.time <- as.numeric(Sys.time())
		day <- (year-1)*365 + dayOfYear
		print(paste0("Y",year,":","D",dayOfYear,"d",day))
		
		nofWrites <- min(round(rnorm(1,mean=writesPerDay,sd=writesPerDaySD)),maxWritesPerDay)
		nofWrites <- nofWrites - (nofWrites %% 4)
		print(paste0("Writes: ", nofWrites))
		dailyWrites[day] <- nofWrites
		
			

			q<-rep(0,nofDrives)
			setSize <- nofDrives/divs
			dayWrites <- 0
			dayWrites = foreach (j = 1:divs, .combine="+") %dopar%{
				for (i in ((j-1)*setSize+1):(j*setSize)){
			

				
					rndIndxB <- sample(1:1000,1)
					s <- sum(ws[rndIndxB:(rndIndxB+floor(nofWrites/(nofDrives)))])
					q[i] <- q[i] + s
					if(i <= (nofDrives/2)){
						q[(nofDrives/2)+i] <- q[(nofDrives/2)+i] + s
					}else{
						q[i - (nofDrives/2)] <- q[i - (nofDrives/2)] + s
					}
				}
			cat(paste0(round(100*j/divs),"%-"))
			q <- q
			}
			
			#cumulativeWrites <- cumulativeWrites + (q/1000)
			cumulativeWrites <- cumulativeWrites + (dayWrites/1000)
			
		#Simulate random failures, here we assume that only one drive can fail per day, which isn't entirely true
		ru <- runif(1)
		if( ru < dailyFailures){
			diskToFail <- sample(1:nofDrives,1)
			cat(paste0("Disk ",diskToFail," failed\n"))
			cumulativeWrites[diskToFail] <- driveWriteCapacity[diskToFail] + 1000
			randomFailures[day] <- randomFailures[day]+1
		}

		
		for (i in 1:nofDrives){
			if( cumulativeWrites[i] > driveWriteCapacity[i] ){
				rebuildIOPerDrive <- ceiling(drives[i]*1000/rebuildSize) # Rebuild is done in 8192 KB blocks
				replace[i] <- replace[i] + 1
				cumulativeWrites[i] <- 0
				driveWriteCapacity[i] <- rnorm(1, mean = driveSize*dwpd*lifeTime*1000, sd = driveWriteCapacitySD)
				failures[day] <- failures[day] + 1
				drives[i] <- driveSize
			
				#Add impact of rebuilding data
				cat(paste0("Rebuild drive: ", i, "\n"))
				q<-rep(0,nofDrives)
				for (i in 1:nofDrives){
			

				
					rndIndxB <- sample(1:1000,1)
					s <- sum(ws[rndIndxB:(rndIndxB+floor(nofWrites/(nofDrives)))])
					q[i] <- q[i] + s
					if(i <= (nofDrives/2)){
						q[(nofDrives/2)+i] <- q[(nofDrives/2)+i] + s
					}else{
						q[i - (nofDrives/2)] <- q[i - (nofDrives/2)] + s
					}
				}
				
				cumulativeWrites <- cumulativeWrites + q/1000
			}
		}
		
		end.time <- as.numeric(Sys.time())
		elapsed.time <- end.time - start.time
		cat(paste0("Time: ",elapsed.time,"\n"))
		calcTimes[day] <- elapsed.time
		
		#Calculate new IOPS
		iops <- iops * ( (1 + yearlyIOPSGrowth)^(1/365) )
		writesPerDay <- iops*24*60*60
		maxWritesPerDay <- 2*writesPerDay
		writesPerDaySD <- 0.2*writesPerDay
	}
	
	#Yearly plots, save to file
	#Fill ratio histogram
	png(filename=paste0("fill_ratio_year",year,"_",description,"_id",simID,".png"), width=1000, height=1000, units="px", pointsize=25)
	hist(cumulativeWrites/driveWriteCapacity, breaks=seq(0,1,0.05), main="", xlab="",ylab="");title(main=paste0("Distribution of fill ratios, end of year ",year),xlab="Fill Ratio", ylab="Frequency");
	dev.off()
	

	#Cumulative writes
	png(filename=paste0("cumulativeWrites_year",year,"_",description,"_id",simID,".png"), width=1000, height=1000, units="px", pointsize=25)
	hist(cumulativeWrites, breaks=20, main="", xlab="",ylab="");title(main=paste0("Distribution of cumulative writes (MB), end of year ",year),xlab="Cumulative MB written", ylab="Frequency");
	dev.off()

	
	#Take yearly snapshot of data
	#yearlyData <- data.frame(cumulativeWrites, driveWriteCapacity, replace, drivesAdded, failures,dailyWrites, calcTimes, drives)
	#dump("yearlyData", paste0("year",year,"_",description,".Rdmpd"))
	save.image(paste0("year",year,"_",description,"_id",simID,".Rdmpd"))
	
	#Add drives
	totalCapacity <- sum(drives)
	capToAdd <- yearlyCapacityGrowth*totalCapacity
	driveSize <- driveSize*(1+driveSizeYearlyGrowth[year])
	nofDrivesToAdd <- 8*ceiling((capToAdd/driveSize)/8)
	dailyFailures <- averageYearlyFailures[year]*nofDrives/365
	driveWriteCapacitySD <- driveSize*dwpd*1*365*1000 
	q <- addDrives(nofDrivesToAdd, driveSize)
	drivesAdded <- c(drivesAdded,nofDrivesToAdd)
	nofDrives <- q[[1]]
	drives <- q[[2]]
	driveWriteCapacity <- q[[3]]
	cumulativeWrites <- q[[4]]
	replace <- q[[5]]

	currentState <- list(nofDrives,drives,driveWriteCapacity, cumulativeWrites, replace, year, day, driveSize, failures, dailyWrites, calcTimes)
	dump("currentState", paste0("currentState",year,"_",description,".Rdmpd"))
}