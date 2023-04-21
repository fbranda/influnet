library(dplyr)
library(reshape)
library(stringr)
library("ggplot2")
library(forcats)
library("data.table")
library(cowplot)

pal1<-c("#D48E88","#66B8BC", "black", "gray")
#pal2<-c("#D48E88","red", "#66B8BC", "green", "yellow", "black", "gray")
pal2<-c("red", "#D48E88", "#66B8BC", "green", "yellow", "black", "gray")

getPartDataFromJH <- function(link, us=FALSE) {

	
	data_raw<-read.csv(link)
	# aggregate data and remove not useful ones
	if (us) {
		data_raw_sub<-subset(data_raw, select = -c(Country_Region,UID,iso2,iso3,code3,FIPS,Admin2,Lat,Long_,Combined_Key))
		if("Population" %in% colnames(data_raw_sub)) {
			data_raw_sub$Population<-NULL
		}
		colnames(data_raw_sub)[1] <- "Country.Region"
		data_raw<-data_raw_sub
	} else {
		data_raw$Province.State<-NULL
		data_raw$Lat<-NULL
		data_raw$Long<-NULL
	}
	data_raw.agg<-data_raw %>%
		group_by(Country.Region) %>% 
		summarize_all(sum, na.rm = TRUE)
	data_raw.agg.df<-as.data.frame(data_raw.agg)
	data_raw.agg.df$country<-data_raw.agg.df$Country.Region
	data_raw.agg.df$Country.Region<-NULL	
	mdata<-reshape::melt(data_raw.agg.df, id=c("country"))
	mdata$variable<-as.Date(str_replace_all(str_replace(mdata$variable, "X", ""), '\\.', "-"), format="%m-%d-%y")
	return(mdata)
}

getSingleCountryData <- function(data_all=NULL, country=NULL, source=NULL ) {
	ord_data<-NULL
	if (!is.null(data_all)) {
		if (country %in% data_all$country) {
			single_data<-data_all[grep(country, data_all$country), ]
			merge_data<-single_data
			death<-as.data.frame(diff(single_data$deaths))
			row.names(death)<-tail(row.names(single_data), -1)
			merge_data<-merge(tail(single_data, -1), death, by="row.names")
			merge_data$Row.names<-NULL
			merge_data$deaths<-NULL
			names(merge_data)<-c("country", "date","cases", "deaths")
			merge_data <- merge_data[order(merge_data$date),]
			rownames(merge_data) <- 1:nrow(merge_data)

			if (!grepl("PC", source, fixed = TRUE)) {
				pos<-as.data.frame(diff(merge_data$cases))
				row.names(pos)<-tail(row.names(merge_data), -1)
				merge_data2 <- merge(tail(merge_data, -1), pos, by="row.names")
				merge_data2$Row.names<-NULL
				merge_data2$cases<-NULL
				names(merge_data2)<-c("country", "date","deaths", "cases")
				merge_data <- merge_data2[, c(1, 2, 4, 3)]
				merge_data<-merge_data2	
			}
			ord_data <- merge_data[order(merge_data$date),]
			rownames(ord_data) <- 1:nrow(ord_data)
		}
	}
	return (ord_data)
}

getDataFromJH<-function(death_web, cases_web, single=FALSE){
	deat_jh<-getPartDataFromJH(death_web, single)
	pos_jh<-getPartDataFromJH(cases_web, single)
	colnames(deat_jh)<-c("country", "date", "deaths")
	colnames(pos_jh)<-c("country", "date", "cases")
	jh_data<-pos_jh
	jh_data$deaths<-deat_jh$deaths
	jh_data$country<-gsub("[()]","",as.character(jh_data$country))
	return(jh_data)

}


makePlotCovid <- function(death_web, cases_web, ita_web, title, mypalette) {

	virol.agg.df<-reshapeData(ita_web)
	
	data_web<-getDataFromJH(death_web, cases_web)
	cases_ita<-getSingleCountryData(data_web, "Italy", "JH")
    cases_ita$year_week<-strftime(cases_ita$date, format = "%Y-%V")

	cases_ita.agg<-cases_ita %>%
		  group_by(year_week) %>%
		  summarise(cases = round(mean(cases)), 
			   deaths = round(mean(deaths)))

	merge.df<-merge(virol.agg.df, cases_ita.agg, by.y = "year_week", by.x = "year_week")

	myweek<-c("46","47","48","49","50","51","52","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17")
	pl1 <- ggplot(merge.df, aes(x=fct_inorder(factor(week)), y=Total_Samples*100)) +
		  geom_bar(stat = "unique", width=1, fill = "#f6e8e8", alpha=0.7, colour = "gray") + 
		  geom_line(aes(y = Positives*100, group = group_vir, color=group_vir)) +
		  scale_color_manual(values=mypalette) + 
		  geom_line(aes(y=cases), color="blue", group=1) +
		  scale_y_continuous(
				"Incidence Covid", 
				sec.axis = sec_axis(~ . * 0.01, name = "INFN-ISS cases")
			) +
		  theme_classic() +  
		  ggtitle(title) +   theme(plot.title = element_text(hjust = 0.5)) +  
		  theme(axis.title.y.left =element_text(colour="blue")) +
 		  xlab("week of the year") +
 		  scale_x_discrete(limits=factor(myweek))

	return(pl1)
}

makePlotFlu2 <- function(ita_web, title, mypalette) {

	merge.df<-reshapeData(ita_web)
	
	myweek<-c("46","47","48","49","50","51","52","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17")
	pl1 <- ggplot(merge.df, aes(x=fct_inorder(factor(week)), y=Total_Samples)) +
		  geom_bar(stat = "unique", width=1, fill = "#f6e8e8", alpha=0.7, colour = "gray") + 
		  geom_line(aes(y = Positives, group = group_vir, color=group_vir)) +
		  scale_color_manual(values=mypalette) + 
		  ggtitle(title) +   theme(plot.title = element_text(hjust = 0.5)) +  
 		  xlab("week of the year") +
 		  scale_x_discrete(limits=factor(myweek)) +
 		  ylab("INFN-ISS cases")

	return(pl1)
}



reshapeData<-function(ita_web) {

	virol<-read.csv(ita_web, sep=",")
	virol[is.na(virol)] <- 0
	virol$group_vir <- ifelse(virol$"influenza_viruses" == "SARS-CoV-2", 'SARS CoV 2', 
		ifelse(virol$"influenza_viruses" == "FLU A", 'Flu A',
		ifelse(virol$"influenza_viruses" == "FLU B", 'Flu B',
		ifelse(virol$"influenza_viruses" == "Rhinovirus", 'Rhinovirus',
		ifelse(virol$"influenza_viruses" == "Adenovirus", 'Adenovirus',
		ifelse(virol$"influenza_viruses" == "RSV", 'RSV',
		ifelse(virol$"influenza_viruses" == "Generic coronavirus", 'other',
		ifelse(virol$"influenza_viruses" == "Bocavirus", 'other',
		ifelse(virol$"influenza_viruses" == "Metapneumovirus", 'other',
		ifelse(virol$"influenza_viruses" == "Parainfluenzali", 'other',	
	"remove"))))))))))

	virol.2<-virol[-grep("remove", virol$group_vir), ]
    
	if (dim(virol.2)[2] == 5) {
		virol.agg<-virol.2 %>%
		  group_by(year_week, group_vir) %>%
		  summarise(Positives = sum(number_detections_influenza_viruses), 
			   Total_Samples = mean(number_samples))
	} else {
		virol.agg<-virol.2 %>%
		  group_by(year_week, group_vir) %>%
		  summarise(Positives = sum(number_detections_influenza_viruses), 	
		  Total_Samples = mean(number_sequenced))	  	
	}
	
	virol.agg.df<-as.data.frame(virol.agg)

	raw_date<-as.data.frame(str_split(virol.agg.df$year_week, "-", simplify = TRUE))

	virol.agg.df$week<-as.character(raw_date$V2)
	
	return(virol.agg.df)
}


makePlotFlu<-function(ita_web, title, yaxis) {
	virol.agg.df<-reshapeData(ita_web)

	pl1 <- ggplot(virol.agg.df, aes(x=fct_inorder(week), y=Total_Samples)) + 
	      ylab(yaxis) +
		  geom_bar(stat = "unique", width=1, fill = "#f6e8e8", alpha=0.7, colour = "gray") + 
		  geom_bar(stat = "unique", width=1, alpha=0.7, aes(y = Positives, fill = group_vir, group = group_vir, colour=group_vir)) +
 		  xlab("week of the year") +
		  theme_classic() +  
		  ggtitle(title) +   theme(plot.title = element_text(hjust = 0.5)) 
	return(pl1)

}


getFluHistoricalData<-function(path) {
	ff <- list.files( path = path, recursive=T, pattern = "*national_typing_subtyping_influenza_viruses.csv$", full.names = TRUE )
	counts.files <- lapply( ff, read.csv)

	counts.files2 = data.frame()
	i=0
	for (counts.file in counts.files) {
		i = i+1
		df = counts.file[,!names(counts.file) %in% c("number_sequenced")]
		df$season = i
		counts.files2 = rbind(counts.files2, df)
	}

	raw_date<-as.data.frame(str_split(counts.files2$year_week, "-", simplify = TRUE))
	counts.files2$week<-raw_date$V2
	counts.files2$year<-raw_date$V1
	counts.files2$season_year<-paste(counts.files2$year, as.numeric(counts.files2$year)+1, sep="-")
	return(counts.files2)
}

makePlotFluHistory<-function(counts.files2, flutype, colorPast, colorNow) {

	last_season = max(unique(counts.files2$season))

	flu<-counts.files2[grep(flutype, counts.files2$influenza_viruses), ]
	flu$year_week<-NULL

    level_order=c("41","42","43","44","45","46","47","48","49","50","51","52","53","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17")
	pl1<-ggplot(data=flu, aes(x=factor(week, level = level_order), y=number_detections_influenza_viruses)) +
 		  geom_line(data = flu, aes(group=season), color=colorPast) +
 		  geom_line(data = subset(flu, season==last_season), color = colorNow, aes(group=season)) +
 		  xlab("Week of the year") +
 		  ylab("Positive cases") +
		  theme_classic() +
		  ggtitle(flutype) +   theme(plot.title = element_text(hjust = 0.5)) 
	return(pl1)
}

getIliHistoricalData<-function(path) {
	ff <- list.files( path = path, recursive=T, pattern = "*national_cases.csv$", full.names = TRUE )
	counts.files <- lapply( ff, read.csv)

	counts.files2 = data.frame()
	i=0
	for (counts.file in counts.files) {
		i = i+1
		df = counts.file
		df$season = i		
		counts.files2 = rbind(counts.files2, df)
	}

	raw_date<-as.data.frame(str_split(counts.files2$year_week, "-", simplify = TRUE))
	counts.files2$week<-raw_date$V2
	counts.files2$year<-raw_date$V1
	counts.files2$season_year<-paste(counts.files2$year, as.numeric(counts.files2$year)+1, sep="-")
	return(counts.files2)
}



makePlotIliHistory<-function(counts.files2) {

	last_season = max(unique(counts.files2$season))

	flu<-counts.files2
	flu$year_week<-NULL

    level_order=c("41","42","43","44","45","46","47","48","49","50","51","52","53","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17")
	pl1<-ggplot(data=flu, aes(x=factor(week, level = level_order), y=incidence)) +
 		  geom_line(data = flu, aes(group=season), color="gray") +
 		  geom_line(data = subset(flu, season==last_season), aes(group=season)) +
 		  xlab("Week of the year") +
 		  ylab("Incidence") +
		  theme_classic() +
		  ggtitle("Influenza like illness") +   theme(plot.title = element_text(hjust = 0.5))
	return(pl1)
}

makePlotIliCumHistory<-function(counts.files2) {

	last_season = max(unique(counts.files2$season))

	flu<-counts.files2
	#flu$year_week<-NULL
    flu_slim<-data.frame(season=flu$season,week=flu$week,incidence=flu$incidence)
	flu_slim2<-data.frame(season=flu$season,week=flu$week,incidence=flu$incidence, year=flu$year)
	flu_slim2$season_year <- ifelse(as.numeric(flu_slim2$week)>30, paste(flu_slim2$year, as.numeric(flu_slim2$year)+1, sep="-"), 
		paste(as.numeric(flu_slim2$year)-1, as.numeric(flu_slim2$year), sep="-")
	)

	flu_slim2$year<-NULL
	flu_slim2$week<-NULL
	
	DT <- data.table(flu_slim)
	
	DT2<-aggregate(x = flu_slim2$incidence,             # Sum by group
          by = list(flu_slim2$season_year),
          FUN = sum)
          
 	names(DT2)<-c("season", "incidence")
           	
	DT[, Cum.Sum := cumsum(incidence), by=list(season)]
	DT$week<-as.factor(DT$week)
    level_order=c("41","42","43","44","45","46","47","48","49","50","51","52","53","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17")

	pl1<-ggplot(data=DT, aes(x=factor(week, level = level_order), y=Cum.Sum)) +
 		  geom_line(data = DT, aes(group=season), color="gray", na.rm = TRUE) +
 		  geom_line(data = subset(DT, season==last_season), aes(group=season), na.rm = TRUE) +
 		  xlab("Week of the year") +
 		  ylab("Cumulative incidence") +
		  theme_classic() 
	#	  ggtitle("Influenza like illness") +   theme(plot.title = element_text(hjust = 0.5))

	pl2<-ggplot(data=DT2, aes(x=season, y=incidence)) +
  		geom_bar(stat="identity") + coord_flip() +
		  xlab("Season") +
 		  ylab("Cumulative incidence") +
		  theme_classic() 
	#+ ggtitle("Influenza like illness") +   theme(plot.title = element_text(hjust = 0.5))

	pl3<-plot_grid(pl1, pl2, labels = "AUTO", ncol = 1, align = 'v')


	return(pl3)
}
