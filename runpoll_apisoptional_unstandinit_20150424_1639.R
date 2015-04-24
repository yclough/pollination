#runpoll
runpoll_apisoptional_unstandinit_20150424_1639<-function(M_poll0,firstyear,honeybees=TRUE,
	L.gismat.curr=map_2009[[2]][[1]],cell.size=25,map_path=map_path,
	poll.param_path=poll.param_path,rastercodefile="rastercode_20150416.txt",key_out="number",key_in="number"){

t.elapsed<-data.frame(code=c("prep","pollres.calc","init.pop","for.calc","over.disp"),t.start=rep(0,5),t.elapsed=rep(0,5))
t.elapsed[1,2]=Sys.time()

	setwd(map_path)
	rastercode<-read.table(rastercodefile,h=T)
	require(plyr)
	L.gismat.curr<-mapvalues(L.gismat.curr,rastercode[,key_in],rastercode[,key_out], warn_missing = FALSE)
	L.gis.curr<-data.frame(row=as.vector(row(L.gismat.curr)),col=as.vector(col(L.gismat.curr)),v=as.vector(L.gismat.curr))
	nr	 <- dim(L.gismat.curr)[1]
	nc	 <- dim(L.gismat.curr)[2]
	# codes are: other=0, grassland: 1, (decid.)forest=2, (W)OSR: 3, arable other:5 (4 is left over for previosu year OSR)
	# not that the alphas work differently, because others is a 6 (relevant to understand the mapvalues() arguments in the latfordisp

	ncells <- nr*nc
	
	setwd(poll.param_path)
	lfn<-read.table("all_codes_allflornestvalues_bumblebees.txt",h=T)
		
	hab_names<-as.character(unique(lfn$Landuse))
	poll_names<-as.character(read.table("poll_species_info.txt",h=T)[,2])

	#num_hab<-length(unique(lfn$Landuse_code))
	num_hab<-length(unique(lfn$number))
	num_bees<-length(unique(lfn$species))
	num_floral<-length(unique(lfn$period))

	#hab_floral<-array(lfn$floral,c(num_hab,num_bees,num_floral))
	#hab_nest<-array(lfn$nesting,c(num_hab,num_bees,num_floral))

	#imp<-read.table("crop_pollspec_importance.txt",h=T)
	#importance<-array(imp$importance,c(num_crop,num_bees))
	#importance<-importance/ifelse(rowSums(importance)>0,rowSums(importance),1)

	weig<-read.table("pollspec_period_weight.txt",h=T)
	weight<-array(weig$weight,c(num_bees,num_floral))

	sdist<-read.table("disp_dist.txt",h=T)
	sp_disp<-array(sdist$distance,c(num_bees,length(levels(sdist$activity))))

	avdens<-read.table("init_av_dens.txt",h=T)
	av<-array(avdens$av_density,c(num_bees))

	rm(weig,avdens,sdist) # remove the original objects
	                      
	TIME=1 # the pollination model will be run for a single year.
	t.disp = 1 # the time counter is set at 1
	
	snm<-which(poll_names!="Apis mellifera")
	sm <- which(poll_names=="Apis mellifera")

if(honeybees==FALSE & "Apis mellifera"%in%poll_names==TRUE){
if(poll_names[num_bees]!="Apis mellifera") stop("honey_bees not in last species slot")
num_bees<-num_bees-1
}

# Define pollinator population size, flower visitation and pollinator resource storage arrays #
	M_poll <- array(0,c(ncells,num_bees,TIME+1))
	N_poll <- array(0,c(ncells,num_bees,TIME))
	flowvis<-array(0,c(ncells,num_bees,num_floral)) # flower visitation assuming pollinator population growth has happened
	pollres<-array(0,c(ncells,num_bees,num_floral)) # resource availability in the surrounding landscape for pollinators nesting in the focal cell

# Define arrays for storing visitation rates over time in agricultural fields #
	#crop_vis_rate_year <- array(0,c(nr*nc,num_bees,TIME))  #YC: MIGHT not NEED THIS

  	nest <- array(0,c(nr, nc,num_bees))
	floral <- array(0,c(nr, nc,num_bees,num_floral))
	F_index <- array(0,c(nr, nc,num_bees))


# Derive nesting values for the different species #

	for(s in 1:num_bees){
	  		nest[,,s]<-mapvalues(L.gismat.curr,
				lfn[lfn$species==s&lfn$period==1,'number'],
				lfn[lfn$species==s&lfn$period==1,'nesting'], warn_missing = FALSE)
	}

# Derive floral values for the different species #

  	for(s in 1:num_bees){
	  for(v in 1:num_floral){
	  		floral[,,s,v]<-mapvalues(L.gismat.curr,
				lfn[lfn$species==s&lfn$period==v,'number'],
				lfn[lfn$species==s&lfn$period==v,'floral'], warn_missing = FALSE)

	  	  }
	  ## calculate floral score
	  F_index[,,s] <- floral[,,s,1]*weight[s,1]+floral[,,s,2]*weight[s,2] # F (floral) index is a weighted sum of the floral resources in the two season, with weights being bee species-specific
	}
# check that nesting and floral values are different from NA
if(sum(is.na(nest)>0)) warning("NA(s) detected in nesting values")
if(sum(is.na(floral)>0)) warning("NA(s) detected in floral values")

# Define pollinator population growth function (preliminary) #

	# this is a preliminary implementation of pollinator population growth, dependent of spatial flor, 
	# which will be defined as the aggregate value of a cellfor nesting bees based on nesting qualities 
	# in that particular cell as well as the flower resources available to pollinators that would nest there.

	growth.func <- function(Nindiv=0,S=0){
	  return(Nindiv)
	}

		t.elapsed[2,2]<-Sys.time()
		t.elapsed[1,3]<-t.elapsed[2,2]-t.elapsed[1,2]

# Calculate pollres, the value of resources available to pollinators in each potential nest cell #


	for(s in 1:num_bees){
		for(v in 1:num_floral){
			foraging_growth<-lat_fordisp(N=as.vector(nest[,,s]),alpha = floral[,,s,v],window=qexp(0.95,1/sp_disp[s,1]*cell.size),
                      beta=(1/sp_disp[s,1]*cell.size),L.gis=L.gis.curr,surv.per.m=1,cell.size=cell.size)
			pollres[,s,v][as.vector(nest[,,s])!=0]<-foraging_growth$sumrawalpha
		}
	}

		t.elapsed[3,2]<-Sys.time()
		t.elapsed[2,3]<-t.elapsed[3,2]-t.elapsed[2,2]

# Initialize population size (1st year) #
if(firstyear==TRUE){
	## Distribute individuals for all species except honeybees
	
	for(i in 1:length(snm)){
		M_poll[,snm[i],t.disp] <- array(
			nest[,,snm[i]]*(pollres[,snm[i],1]*weight[snm[i],1]+pollres[,snm[i],2]*weight[snm[i],2]) *av[snm[i]] *cell.size^2/100^2*ncells)
	}

if(honeybees==TRUE & "Apis mellifera"%in%poll_names==TRUE){
	## Distribute individuals for honey bees
	sm <- which(poll_names=="Apis mellifera") ## for honey bees we place a certain number of apiearies in the landscape
	hive_factor <- 1000 ## this is a number that we assign to
	tot_apiaries <- round(av[sm] *cell.size^2/100^2*ncells / hive_factor)
	M_per_apiary <- av[sm] *cell.size^2/100^2*ncells / tot_apiaries

	## select places in the landscape, not too close to each other and where the bees should be
                           ind <- 1:tot_apiaries
                           pos <- 1:ncells # possible positions
                           loc <- cbind(L.gis.curr$row, L.gis.curr$col)
                           w <- 1000 # smallest distance between bee hives
                           r <- 50 # half size of a bee apiary , this is used to make the apiary stretch over more than one cell, when cells are a bit small
                           count <- 1
                           #prob=L.gis_poll0$crop.1
                           prob=array(nest[,,sm]*(pollres[,sm,1]*weight[sm,1]+pollres[,sm,2]*weight[sm,2])) # the probaility of being selected depend on quality
                           ind[count] <- sample(pos,1,replace=FALSE,prob=prob)
                           rem <- abs(loc[,1] - L.gis.curr$row[ind[count]]) < (w/cell.size) & abs(loc[,2] - L.gis.curr$col[ind[count]]) < (w/cell.size)
			         #following lines are new (Johann, Ullrika march 2015)
				   dist <- ((loc[,1] - L.gis.curr$row[ind[count]])^2 + (loc[,2] - L.gis.curr$col[ind[count]])^2)^0.5 
                           reduction <- dist/max(dist[rem]) 
                           prob[rem] <- prob[rem]*reduction[rem] # these cells have a very small probability of being selected in the future 
                           #prob[rem] <- 0 # these cells cannot be selected in the future 
                           while(count<tot_apiaries){
                                                       count = count +1
                                                       ind[count] <- sample(pos,1,replace=FALSE,prob=prob)
                                                       rem <- abs(loc[,1] - L.gis.curr$row[ind[count]]) < (w/cell.size) & abs(loc[,2] - L.gis.curr$col[ind[count]]) < (w/cell.size)
                                                       prob[rem] <- 0
                           }
                 
           


	M_poll[,sm,t.disp] <- M_poll[,sm,t.disp]*0
	loc <- cbind(L.gis.curr$row, L.gis.curr$col)
	for(i in 1:tot_apiaries){
  		place <- abs(loc[,1] - L.gis.curr$row[ind[i]]) <= (r/cell.size) & abs(loc[,2] - L.gis.curr$col[ind[i]]) <= (r/cell.size)
  		M_poll[place,sm,t.disp] <- M_per_apiary/sum(place)
	}
}}
if(firstyear==FALSE){M_poll[,,1]<-M_poll0[,,2]}

		t.elapsed[4,2]<-Sys.time()
		t.elapsed[3,3]<-t.elapsed[3,2]-t.elapsed[4,2]

# Calculate number of foraging pollinators per cell and species,and the value of resources available to each nest cell #

	for(i in 1:length(snm)){
	N_poll[,snm[i],t.disp]<-growth.func(Nindiv=M_poll[,snm[i],t.disp],S=(pollres[,snm[i],1]*weight[snm[i],1]+pollres[,snm[i],2]*weight[snm[i],2]))
	}

	if(honeybees==TRUE & "Apis mellifera"%in%poll_names==TRUE){
		N_poll[,sm,t.disp]<-M_poll[,sm,t.disp]
	}

	for(s in 1:num_bees){
		for(v in 1:num_floral){
			foraging<-lat_fordisp(N=N_poll[,s,t.disp],alpha = floral[,,s,v],window=qexp(0.95,1/sp_disp[s,1]*
			cell.size),beta=(1/sp_disp[s,1]*cell.size),surv.per.m=1,L.gis=L.gis.curr,cell.size=cell.size)
			flowvis[,s,v]<-foraging$receiving				 # flower visitation in the cells surrounding the focal cell
			pollres[,s,v][N_poll[,s,1]!=0]<-foraging$sumrawalpha	 # aggregated resources experienced by the pollinator nesting in the focal cell
			
		}
	}

		t.elapsed[5,2]<-Sys.time()
		t.elapsed[4,3]<-t.elapsed[5,2]-t.elapsed[4,2]

# (non-honey) bees disperse to their new nesting sites #

	for(i in 1:length(snm)){
		poll.out.start<-lat_fordisp(N=N_poll[,snm[i],t.disp],alpha=nest[,,snm[i]],surv.per.m=1,window=qexp(0.95,1/sp_disp[snm[i],2]*cell.size),beta=(1/sp_disp[snm[i],2]*cell.size),
						L.gis=L.gis.curr,cell.size=cell.size)
		M_poll[,snm[i],t.disp+1]<-poll.out.start$receiving
	}
# honey-bees stay put #
if(honeybees==TRUE & "Apis mellifera"%in%poll_names==TRUE){
	M_poll[,sm,t.disp+1]<-N_poll[,sm,t.disp]
}
	t.elapsed[5,3]<-Sys.time()-t.elapsed[5,2]
return(list(t.elapsed=t.elapsed,M_poll=M_poll,N_poll=N_poll,
flowvis=flowvis, floral=floral,nest=nest,pollres=pollres))
}
