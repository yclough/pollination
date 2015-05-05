###########################################
# code to read in parameter distributions,
# sample from them and generate input for the
# pollinator function runpoll()
###########################################

# Read in data
###############
install.packages('XLConnect')
require(XLConnect)
wb = loadWorkbook("pollparam_rastercode_20150416_YC.xlsx")
lucat<-read.table("lu_categories.txt", h=T)
# note that XLConnect requires Java runtime environment to be installed on your machine.
# if you are running 64-bit R then it is the 64-bit Java that is needed 

# Notes on the data: b, l, u: best, lower, upper

# define  helper functions
###########################
# given a dataset, the following function converts columns (col.id) to numeric

as.numeric_mult<-function(data,col.id){
	out<-data
	for(i in 1:length(col.id)){
	 out[,col.id[i]]<-as.numeric(data[,col.id[i]])
	}
return(out)
}

# given a dataset, the following function uses a column (samma.id) containing references to other lines to fill 
# missing data in one or more other columns (col.id)

repl_flornest<-function(data,col.id,samma.id){
	out<-data
	for(i in 1:length(col.id)){
	 out[,col.id[i]][which(is.na(out[,col.id[i]]))]<-
	data[,col.id[i]] [ data[,samma.id][which(is.na(out[,col.id[i]]))] ]
	}
return(list(out=out,nr = sum(!is.na(out[,col.id[i]]))*length(col.id)/3))
}


# Floral cover
df = readWorksheet(wb, sheet = "Floral cover", header = TRUE)
df$samma.som.attrBt<-as.numeric(df$samma.som.attrBt)
df$samma.som<-as.numeric(df$samma.som)
df<-as.numeric_mult(df,c(4:11))
df2<-merge(lucat,df,by="code",all=TRUE)
df2$lu[is.na(df2$Flor_Cov_P1_b)&is.na(df2$samma.som)]
df2[is.na(df2$Flor_Cov_P1_b)&is.na(df2$samma.som),'samma.som'][1:16]=c(13,52,53,20,NA,32,NA,50,50,50,1,52,NA,52,NA,53)
	# 3 lines have a samma.som values which refer to lines that do not have parameter values,but rather samma.crop entries themselves
	#df2$samma.som[df2$samma.som] # Hampa, Rörflen (övrigt), Permanent crop
	#df2$samma.som[df2$samma.som][is.na(df2$samma.som[df2$samma.som])==FALSE]<-
	# OUTDATED: this is now changed in the original file: pollparam_rastercode_20150416_YC.xlsx


# B.terr attract #
db = readWorksheet(wb, sheet = "B.terr attract", header = TRUE)
lucat<-read.table("lu_categories.txt", h=T)
db$samma.som<-as.numeric(db$samma.som)
db<-as.numeric_mult(db,c(4:13))
db2<-merge(lucat,db,by="code",all=TRUE)
db2$lu[is.na(db2$Flor_P1_B.ter_b)&is.na(db2$samma.som)][1:16]
db2[is.na(db2$Flor_P1_B.ter_b)&is.na(db2$samma.som),'samma.som'][1:16]=c(13,52,52,20,NA,32,NA,50,50,50,1,52,NA,52,NA,52)

# Sol.bee attract 
ds = readWorksheet(wb, sheet = "Solit bee attract", header = TRUE)
lucat<-read.table("lu_categories.txt", h=T)
ds$samma.som<-as.numeric(ds$samma.som)
ds<-as.numeric_mult(ds,c(4:13))
ds2<-merge(lucat,ds,by="code",all=TRUE)
ds2$lu[is.na(ds2$Flor_P1_Sol.bee_b)&is.na(ds2$samma.som)][1:16]
ds2[is.na(ds2$Flor_P1_Sol.bee_b)&is.na(ds2$samma.som),'samma.som'][1:16]=c(13,52,52,20,NA,32,NA,50,50,50,1,52,NA,52,NA,52)

# A.mel  attract
da = readWorksheet(wb, sheet = "Honey bee attract", header = TRUE)
da$samma.som<-as.numeric(da$samma.som)
da<-as.numeric_mult(da,c(4:10))
da2<-merge(lucat,da,by="code",all=TRUE)
da2$lu[is.na(da2$Flor_P1_A.mel_b)&is.na(da2$samma.som)][1:16]
da2[is.na(da2$Flor_P1_A.mel_b)&is.na(da2$samma.som),'samma.som'][1:16]=c(13,52,52,20,NA,32,NA,50,50,50,1,52,NA,52,NA,52)

# replacing the missing FlorCov, Flor & Nest values for all lines referring to (if NA in samma.som,  FlorCov is set to NA)

df3<-repl_flornest(df2,col.id=c(8:13),samma.id=7)$out
db3<-repl_flornest(db2,col.id=c(7:15),samma.id=6)$out
ds3<-repl_flornest(ds2,col.id=c(7:15),samma.id=6)$out
da3<-repl_flornest(da2,col.id=c(7:12),samma.id=6)$out

totnr <- c(repl_flornest(df2,col.id=c(8:13),samma.id=7)$nr,
repl_flornest(db2,col.id=c(7:15),samma.id=6)$nr,
repl_flornest(ds2,col.id=c(7:15),samma.id=6)$nr,
repl_flornest(da2,col.id=c(7:12),samma.id=6)$nr)

sample_repl_flornest<-function(data,col.id,samma.id,
q=runif(sum(!is.na(data[,col.id[1]]))*length(col.id)/3)){
# sample from the triangular distribution and
# replace the missing FlorCov, Flor & Nest values for all lines referring to (if NA in samma.som,  FlorCov is set to NA)
require(mc2d)
	out <- data
	for(i in 1:2){                           #YC! change 2 to "number of columns that are 3"
	# generate sample 
	b <- data[,col.id[3*(i-1)+1]] [ data[,samma.id][ which(is.na(out[,col.id[3*(i-1)+1]])) ] ]
	l <- data[,col.id[3*(i-1)+2]] [ data[,samma.id][ which(is.na(out[,col.id[3*(i-1)+1]])) ] ]
	u <- data[,col.id[3*(i-1)+3]] [ data[,samma.id][ which(is.na(out[,col.id[3*(i-1)+1]])) ] ]
	input <- cbind(l,b,u)
	output <- rep(NaN,dim(input)[1])
	ind <- which(!is.na(l))
	red.input <- input[ind,]
	for(j in 1:length(ind)){
		#output[ind[j]] <- rtriang(1,min=red.input[j,1], mode=red.input[j,2], max=red.input[j,3])
		output[ind[j]] <- qtriang(q[j],min=red.input[j,1], mode=red.input[j,2], max=red.input[j,3])
	}
	out[,col.id[3*(i-1)+1]][which(is.na( out[,col.id[3*(i-1)+1]] ))] <- output # assign new values to b
	}
return(out)
}


#########################
# Transfer to input file #
#########################

# paramz.1 is the number lu lu2 species period 
# paramz.2 are samples of floral and nesting
paramz.1 <- data.frame(
number=rep(df3$code,6),
lu=rep(df3$lu,6),
species=rep(c(1,2,3),each=199*2),
period=rep(c(1,2,1,2,1,2),each=199))


require(lhs)
sample.size <- 100
paramz.2 <- array(0,c(2,dim(paramz.1)[1],sample.size))
q.sample <- randomLHS(sample.size, sum(totnr))                        # tot nr 800 - can that be right?
for(i in 1:sample.size){
df3<-sample_repl_flornest(df2,col.id=c(8:13),samma.id=7,q=q.sample[i,1:totnr[1]])
db3<-sample_repl_flornest(db2,col.id=c(7:15),samma.id=6,q=q.sample[i,(1+totnr[1]):totnr[2]])
ds3<-sample_repl_flornest(ds2,col.id=c(7:15),samma.id=6,q=q.sample[i,(1+totnr[2]):totnr[3]])
da3<-sample_repl_flornest(da2,col.id=c(7:12),samma.id=6,q=q.sample[i,(1+totnr[3]):totnr[4]])

paramz.2[,,i] = cbind(c(df3$Flor_Cov_P1_b*db3$Flor_P1_B.ter_b,df3$Flor_Cov_P2_b*db3$Flor_P2_B.ter_b,
	df3$Flor_Cov_P1_b*ds3$Flor_P1_Sol.bee_b,df3$Flor_Cov_P2_b*ds3$Flor_P2_Sol.bee_b,
	df3$Flor_Cov_P1_b*da3$Flor_P1_A.mel_b,df3$Flor_Cov_P2_b*da3$Flor_P2_A.mel_b),
	c(db3$Nest_P1_B.ter_b,db3$Nest_P1_B.ter_b,ds3$Nest_P1_Sol.bee_b,
	ds3$Nest_P1_Sol.bee_b,rep(1,199*2)))
}


## the random numbers for one of the parameters
hist(paramz.2[1,2,])
