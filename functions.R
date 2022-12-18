library(dplyr)
library(reshape)
library(stringr)
library("ggplot2")
library(forcats)

pal1<-c("#D48E88","#66B8BC", "black", "gray")
pal2<-c("#D48E88","#66B8BC", "red", "green", "yellow", "black", "gray")

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
	mdata<-melt(data_raw.agg.df, id=c("country"))
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
