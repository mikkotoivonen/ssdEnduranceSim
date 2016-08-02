library(colorRamps)


writeSizez <- c(1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192) # In KiloBytes.
writeWeights <- c(1,2,4,8,16,32,64,32,16,8,4,2,1,1) # Weights for different write sizes
writeSize <- rep(writeSizez,writeWeights)
meanWriteSize <- mean(writeSize)

driveSize <- seq(500,30000,250)

nofDrives <- 100000 # number of drives to sample

dwpd <- seq(1,20,0.25)
lifeTime <- 5*365 # Expected lifetime of drive in days


yg <- 1+0.4 # yearly IOPS growth
day <- 365

iopd <- 1000

years <- seq(0,10,1/12)

for(k in 1:length(driveSize)){

start.time <- as.numeric(Sys.time())

ii <- iopd*nofDrives # initial iops is based on number of drives

results <- matrix(nrow=length(years),ncol=length(dwpd))

for(j in 1:length(dwpd)){

driveWriteCapacitySD <- driveSize[k]*dwpd[j]*1*365*1000 # This essentially is standard deviation of 1 year
driveWriteCapacity <- rnorm(nofDrives, mean = driveSize[k]*dwpd[j]*lifeTime*1000, sd = driveWriteCapacitySD) # The standard deviation here is equivalent to one year
driveWriteCapacity[driveWriteCapacity<0] <- 1



for(i in 1:length(years)){

	day <- round(365*years[i])

	#Assuming writes are normally distributed, from the central limit theorem we get the following
	mtw <- 24*60*60*ii*((yg^((day+1)/365) - yg^(1/365))/(yg^(1/365)-1)) #mean total writes
	tws <- 0.2*mtw #total writes standard deviation

	#Assuming writes are uniformly distributed between zero and two times the mean, only the standard deviation is affected
	#tws <- ((day/12)^(1/2))*mtw
	

	total_writes <- (1/1000)*(rnorm(nofDrives,mean=mtw, sd=tws)*meanWriteSize)/nofDrives
	total_writes[total_writes<0] <- 0


	fillRatio <- total_writes/driveWriteCapacity
	failedN <- 1.1
	fillRatio[fillRatio>1] <- failedN

	failedDrivesP <- (sum(fillRatio[fillRatio==failedN])/failedN)/nofDrives
	

	
	results[i,j] <- failedDrivesP

}

}

end.time <- as.numeric(Sys.time())
elapsed.time <- end.time - start.time
eta.time <- (length(driveSize)-k)*elapsed.time
dev.new()
png(filename=paste0("failure_driveSize_",sprintf("%05d",driveSize[k]),".png"), width=1000, height=1000, units="px", pointsize=25)
image(y=dwpd,x=years,results,col=green2red(255),ylab="DWPD",xlab="Year",main=paste0("Failure %, IOPS per drive: ", iopd, ", Drive size: ", driveSize[k]))
contour(y=dwpd,x=years,results, levels = c(0.05,0.25,0.5,0.75,0.8), add = TRUE, col = "black")
grid()
dev.off()
print(paste0("File: ", paste0("failure_driveSize_",sprintf("%05d",driveSize[k]),".png")))
cat(paste0("Time: ",elapsed.time,"\n"))
cat(paste0("Estimate time to complete: ", eta.time))
}