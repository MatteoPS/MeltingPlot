#!/usr/bin/env Rscript

#Rscript MeltingPlot.R -in [inputfile] -meta ['y' or 'n'] -ref ['y' or 'n']

# Developed by Matteo Perini 2020
# SkyNet UNIMI
# Pediatric Clinical Research Center
# Romeo ed Enrica Invernizzi
# Universita degli Studi di Milano 
# matteo.perini@unimi.it
# Please cite our work:
# Perini, M. et al. MeltingPlot: a user-friendly online tool for epidemiological investigation using High Resolution Melting data, 2020

args = commandArgs(trailingOnly=TRUE)

	
##############################################################
########################   HELP SCREEN   #####################
##############################################################

helps= c("-h", "--help", "--h", "-help")

if (args[1] %in% helps | is.na(args[1]))
{
print("                                                                                                ")
print("     WELCOME TO MELTINGPLOT!                                                                    ")
print("                                                                                                ")
print("  Rscript MeltingPlot.R -in [inputfile] -meta ['y' or 'n'] -ref ['y' or 'n']                    ")
print("                                                                                                ")
print("MANDATORY ARGUMENTS:                                                                            ")
print("  -in              (required) input file name                                                   ")
print("  -meta            (required) ['y' or 'n'] presence of strain metadata in the .xls file         ")
print("  -ref             (required) ['y' or 'n'] presence of reference temperatures in the .xls file  ")
print("                                                                                                ")
print("Optional arguments:                                                                             ")
print("  -rep             repilicates of HRM expriment, default = 3                                    ")
print("  -png_res         PPI value resolution for the .png images, 0 = no png, default = 0            ")
print("  -day_thr         minimum value to have bolder links in patients graph, default = 7            ")
print("  -btw_thr         Betweenness value treshold for undetermined isolates, default = 0.5          ")
print("  -h,--h,--help    show help page                                                               ")
print("                                                                                                ")
print("If you are using MeltingPlot for a scientific publication please cite:                          ")
print("Perini, M. et al. MeltingPlot: a user-friendly online tool for epidemiological  investigation   ")
print(" 		  using High Resolution Melting data, 2020                                     ")
print("  * THANK YOU *                                                                                 ")
print("                                                                                                ")
quit()
}




##############################################################
##################   LIBRARY LOADER  #########################
##############################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(igraph,gplots,xlsx,ggplot2,scales)



##############################################################
##################   GET INPUTS  #############################
##############################################################

#available arguments
arguments = c("-in","-rep","-meta","-ref", "-png_res", "-day_thr", "-btw_thr")

mandatory = c("-in", "-ref", "-meta")

#if not in mandatory, it MUST have a default.
defaults = c("none",3,"none","none", 0, 7, 0.5)


args_df <- data.frame(matrix(ncol = length(arguments), nrow = 0))
default_args_df <- data.frame(matrix(ncol = length(arguments), nrow = 0))
colnames(args_df) <- arguments
colnames(default_args_df) <- arguments
default_args_df[1,] <- defaults

for (arg_name in arguments)
{
	if (arg_name %in% args)
	{
		for (word in args)
		{
		if (word == arg_name)
			{
			position= match(word,args)
			args_df[1,arg_name] = args[position+1]
			}
		}

	}else
	{
		args_df[1,arg_name] = default_args_df[1,arg_name]
	}
	if (arg_name %in% mandatory & args_df[1,arg_name] == default_args_df[1,arg_name])
	{
print("                                                                                                ")
print("            ERROR                                                                               ")
print("                                                                                                ")
print(paste(arg_name, "is a mandatory argument and it's missing")) 
print("                                                                                                ")
print("The mandatory arguments are:")
print(mandatory)
print("                                                                                                ")
quit()

	}


}


###assign the argument values to the variables:
input_file <- args_df[1,"-in"]
replicates <- as.numeric(args_df[1,"-rep"])
with_metadata <- args_df[1,"-meta"]
with_background <- args_df[1,"-ref"]
png_resolution = as.numeric(args_df[1,"-png_res"])
day_thr = as.numeric(args_df[1,"-day_thr"])
btw_thr = as.numeric(args_df[1,"-btw_thr"])



##############################################################
##################   FUNCTIONS  ##############################
##############################################################

#to add the blank space in a plot for the legend
add_legend <- function(...)
{
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
    mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

##############################################################
##################   DEFAULTS  ###############################
##############################################################

### Color palettes

# palette dendrogram temperatures
values_palette<-colorRampPalette(c("royalblue3","gray92","firebrick1"))(100)

# palette clusters
clusters_palette = c("#e6c800","#3cb44b","#e6194B","#4363d8","#f58231","#911eb4","#42d4f4","#f032e6","#bfef45","#fabebe","#469990","#e6beff","#9A6324","#fffac8","#800000","#aaffc3","#808000","#ffd8b1")

col_nohrm = "gray10"
col_und = "gray50"

# palette locations
loc_palette = adjustcolor(c("#A6CEE3","#FDBF6F","#B2DF8A","#FB9A99","#CAB2D6","#FFFF99","#1F78B4","#33A02C","#E31A1C","#FF7F00","#6A3D9A","#B15928"),alpha.f = .7)

### Strings

und_string <- "Undetermined"
nohrm_string <- "NoHRM"

### Thresholds

#edge betwenness threshold
eb_thr <- 1

# Clustering CELSISUS threshold, max temp difference to cluster 2 isolate toghether (as established in https://doi.org/10.1038/s41598-020-57742-z)
clus_thr <- 0.5


# minimum of days span to plot the month plots
plot_month_thr = 45

### Plot parameters

# Isolates graph
clus_graph_label_color = "black"

# trasmission graph
tra_graph_label_color = "black"

# Timeline symbols
positivized_size = 5
not_positivized_size = 3
positivized_alpha = 1
not_positivized_alpha = 0.5


### Output files

input_file_name <- sub('\\..*$', '', basename(input_file))

output_folder <- paste("MeltingPlot_",input_file_name, sep="")

output_folder_png <- paste(output_folder,"/","images_png", sep="")
output_folder_pdf <- paste(output_folder,"/","images_pdf", sep="")
output_folder_tab <- paste(output_folder,"/","tables_xls", sep="")

dir.create(output_folder, showWarnings = FALSE)
dir.create(output_folder_png, showWarnings = FALSE)
dir.create(output_folder_pdf, showWarnings = FALSE)
dir.create(output_folder_tab, showWarnings = FALSE)

out_table_xls <- paste(output_folder_tab,"/",input_file_name,"_isolates.xls", sep="")
out_table_clus_xls <- paste(output_folder_tab,"/",input_file_name,"_clusters.xls", sep="")



#1.1. Isolates clustering: isolates graph
out_isolate_graph_pdf <- paste(output_folder_pdf,"/1a_",input_file_name,"_MeltingPlot_isolates_graph.pdf", sep="")
out_isolate_graph_png <- paste(output_folder_png,"/1a_",input_file_name,"_MeltingPlot_isolates_graph.png", sep="")

#1.2. Isolates clustering: isolates heatmap
out_isolate_heatmap_pdf <- paste(output_folder_pdf,"/1b_",input_file_name,"_MeltingPlot_isolates_heatmap.pdf", sep="")
out_isolate_heatmap_png <- paste(output_folder_png,"/1b_",input_file_name,"_MeltingPlot_isolates_heatmap.png", sep="")

#2.1. Prevalence analysis: all isolates per location (daily)
out_isolate_location_day_pdf <- paste(output_folder_pdf,"/2a_",input_file_name,"_MeltingPlot_isolates_location_daily.pdf", sep="")
out_isolate_location_day_png <- paste(output_folder_png,"/2a_",input_file_name,"_MeltingPlot_isolates_location_daily.png", sep="")

#2.2. Prevalence analysis: all isolates per location (monthly)
out_isolate_location_month_pdf <- paste(output_folder_pdf,"/2b_",input_file_name,"_MeltingPlot_isolates_location_monthly.pdf", sep="")
out_isolate_location_month_png <- paste(output_folder_png,"/2b_",input_file_name,"_MeltingPlot_isolates_location_monthly.png", sep="")

#2.3. Prevalence analysis: positivizations per location (daily)
out_positivization_location_day_pdf <- paste(output_folder_pdf,"/2c_",input_file_name,"_MeltingPlot_positivization_location_daily.pdf", sep="")
out_positivization_location_day_png <- paste(output_folder_png,"/2c_",input_file_name,"_MeltingPlot_positivization_location_daily.png", sep="")

#2.4. Prevalence analysis: positivizations per location (monthly)
out_positivization_location_month_pdf <- paste(output_folder_pdf,"/2d_",input_file_name,"_MeltingPlot_positivization_location_monthly.pdf", sep="")
out_positivization_location_month_png <- paste(output_folder_png,"/2d_",input_file_name,"_MeltingPlot_positivization_location_monthly.png", sep="")

#2.5. Prevalence analysis: global positivizations (daily)
out_positivization_global_day_pdf <- paste(output_folder_pdf,"/2e_",input_file_name,"_MeltingPlot_positivization_global_daily.pdf", sep="")
out_positivization_global_day_png <- paste(output_folder_png,"/2e_",input_file_name,"_MeltingPlot_positivization_global_daily.png", sep="")

#2.6. Prevalence analysis: global positivizations (monthly)
out_positivization_global_month_pdf <- paste(output_folder_pdf,"/2f_",input_file_name,"_MeltingPlot_positivization_global_monthly.pdf", sep="")
out_positivization_global_month_png <- paste(output_folder_png,"/2f_",input_file_name,"_MeltingPlot_positivization_global_monthly.png", sep="")

#3.1. Transmission analysis: patients' timeline
out_patients_timeline_pdf <- paste(output_folder_pdf,"/3a_",input_file_name,"_MeltingPlot_patients_timeline.pdf", sep="")
out_patients_timeline_png <- paste(output_folder_png,"/3a_",input_file_name,"_MeltingPlot_patients_timeline.png", sep="")

#3.2. Transmission analysis: patient−to−patient graph
out_patients_graph_pdf <- paste(output_folder_pdf,"/3b_",input_file_name,"_MeltingPlot_patients_graph.pdf", sep="")
out_patients_graph_png <- paste(output_folder_png,"/3b_",input_file_name,"_MeltingPlot_patients_graph.png", sep="")




##############################################################
##################   LOGFILE  ################################
##############################################################


log_file <- paste(paste(output_folder,"/MeltingPlot_",input_file_name, ".log", sep=""))
current_time <- Sys.time()
txt <- paste("##### MeltingPlot analysis START ( ", current_time," ) #####\n\n", sep="")
write(txt,file = log_file)

####################################
### Open files and check

# HRM teperature sheet
temp_in <- read.xlsx(input_file, sheetName="HRM_temperatures")

if (nrow(temp_in) == 0)
{
	txt = "\r\nThe HRM temperature sheet is empty, the run has been interrupted\r\n"
	write(txt, file=log_file, append=T)

	write(txt,file = "Analysis_INTERRUPTED.log")

	current_time <- Sys.time()
	txt <- paste("\r\n##### MeltingPlot analysis INTERRUPTED ( ", current_time," ) #####\n\n", sep="")
	write(txt,file = log_file, append=T)
	
	quit()
}


temp_ncol <- replicates+2
temp_in <- temp_in[,1:temp_ncol]

# Isolate metadata sheet
if (with_metadata == "y")
{

	metadata_in <- read.xlsx(input_file,sheetName="Isolates_metadata")

	#remove rows of all missing values

	metadata_in=metadata_in[complete.cases(metadata_in$ID_isolate),]

	metadata_in <- metadata_in[,c("ID_isolate","ID_patient","Date","Location"),]

}

if (with_background == "y")
{

	background_in <- read.xlsx(input_file, sheetName="Reference_isolates")
	
	if (nrow(background_in) == 0)
		{
		txt = "\r\nThe background sheet is empty, the run has been interrupted\r\n"
		write(txt, file=log_file, append=T)

		write(txt,file = "Analysis_INTERRUPTED.log")

		current_time <- Sys.time()
		txt <- paste("\r\n##### MeltingPlot analysis INTERRUPTED ( ", current_time," ) #####\n\n", sep="")
		write(txt,file = log_file, append=T)
		
		quit()
		}
	bkg_ncol <- replicates+3
	bkg = background_in[,1:bkg_ncol]

}


##############################################################
###############   INPUT CHECKS  ##############################
##############################################################


#####################
### HRM TEMPERATURES

txt <- paste("1. Check HRM temperature data ...\r\n", sep="")
write(txt,file = log_file, append=T)

### Check the presence of isolate with:

# a) Lack of HRM temp values OR ANY EMPTY VALUE

#removal of rows without ID_isolate
temp_in=temp_in[!is.na(temp_in[,"ID_isolate"]),]

#check NA in the rest of the input
temp_in_col_val <- temp_in[,2:temp_ncol]
temp_excl_lack_hrm_val <- temp_in[rowSums(is.na(temp_in_col_val)) > 0, "ID_isolate"]

# Write name in the log file

if (length(temp_excl_lack_hrm_val) > 0)
{
        txt <- paste("\r\nThe following isolates will be exluded because some empty vlaues were found in:\r\n" , sep="")
        write(txt, file=log_file, append=T)
        write(as.matrix(temp_excl_lack_hrm_val), file=log_file, ncol=1, append=T)
}

temp_in2 <- temp_in[!temp_in$ID_isolate %in% temp_excl_lack_hrm_val,]

# b) All HRM teperatures range within the clus_thr threshold

#converts temperature in numeric (if they were text)

temp_in2check <- temp_in2[,3:temp_ncol]
temp_in2check[,1:ncol(temp_in2check)]=data.frame(sapply(temp_in2check[,1:ncol(temp_in2check)], function(x) as.numeric(as.character(x))))
temp_in2check_range <- apply(temp_in2check, 1, function(x) abs(as.numeric(max(x))-as.numeric(min(x))))

temp_excl_range_hrm_val <- temp_in[temp_in2check_range > clus_thr, "ID_isolate"]

if (length(temp_excl_range_hrm_val) > 0)
{
        txt <- paste("\r\nThe following isolates will be exluded because HRM replicates range above ", clus_thr, ":\r\n" , sep="")
        write(txt, file=log_file, append=T)
        write(as.matrix(temp_excl_range_hrm_val), file=log_file, ncol=1, append=T)
}

temp_in <- temp_in[!temp_in$ID_isolate %in% temp_excl_range_hrm_val,]



# c) no duplicates or absent set

temp_in_org_set <- table(temp_in2$ID_isolate, temp_in2$Primers_set)

temp_in_org_set_MIN <- apply(temp_in_org_set, 1, min)
temp_in_org_set_MAX <- apply(temp_in_org_set, 1, max)

temp_excl_wrong_set <- row.names(temp_in_org_set[temp_in_org_set_MAX != 1 | temp_in_org_set_MIN != 1,])

# Write name in the log file

if (length(temp_excl_wrong_set) > 0)
{
	txt <- paste("\r\nThe following isolates will be exluded because of wrong number of temperature relative to the primer sets (check if some temperatures are absent or duplicated):\r\n" , sep="")
	write(txt, file=log_file, append=T)
	write(as.matrix(temp_excl_wrong_set), file=log_file, ncol=1, append=T)
}

temp_in3 <- temp_in2[!temp_in2$ID_isolate %in% temp_excl_wrong_set,]




# d) all the isolates must have a row for each primer set


	
	isolates_names = unique(temp_in$ID_isolate)
	primers_names = unique(temp_in$Primers_set)
	
	primer_miss_ids = c()
	for (iso_name in isolates_names)
	{
		present_primers = temp_in3[temp_in3$ID_isolate==iso_name,"Primers_set"]
		if (length(present_primers) != length(primers_names))
		{
			primer_miss_ids = append(primer_miss_ids,iso_name)
		}
		
	}
	
	
	# Write name in the log file

if (length(primer_miss_ids) > 0)
{
	txt <- paste("\r\nThe following isolates will be exluded because temperature relative to some primer sets are missing:\r\n" , sep="")
	write(txt, file=log_file, append=T)
	write(as.matrix(primer_miss_ids), file=log_file, ncol=1, append=T)
}

temp_in3 <- temp_in3[!temp_in3$ID_isolate %in% primer_miss_ids,]



txt <- paste("\r\nDONE!\r\n" , sep="")
write(txt, file=log_file, append=T)

#############
### METADATA
if (with_metadata == "y")
{
	txt <- paste("\r\n\r\n2. Check metadata input ...\r\n", sep="")
	write(txt,file = log_file, append=T)

	# Check duplicated isolates
	
	#removal of rows without ID_isolate or ID_patient
	metadata_in=metadata_in[!is.na(metadata_in[,"ID_isolate"]),]
	metadata_in=metadata_in[!is.na(metadata_in[,"ID_patient"]),]
	


	metadata_excluded_duplicated <- metadata_in[as.matrix(table(metadata_in$ID_isolate))[,1]>1,"ID_isolate"]

	if (length(metadata_excluded_duplicated) > 0)
	{
		txt <- paste("\r\nThe following isolates will be not included in the barplot and timeline plots because duplicated isolates in the input table:\r\n", sep="")
		write(txt, file=log_file, append=T)
		write(as.matrix(metadata_excluded_duplicated), file=log_file, ncol=1, append=T)
	}

	metadata_in2 <- metadata_in[! metadata_in$ID_isolate %in% metadata_excluded_duplicated, ]

	# Check empty cells

	metadata_NA_count <- rowSums(is.na(metadata_in2))
	metadata_empty_count <- rowSums(metadata_in2 == "")

	metadata_excluded_isolates <- metadata_in2[metadata_NA_count + metadata_empty_count > 0, "ID_isolate"]

	if (length(metadata_excluded_isolates) > 0)
	{
		txt <- paste("\r\nThe following isolates will be not included in the barplot and timeline plots because of empty cells in the input table:\r\n", sep="")
		write(txt, file=log_file, append=T)
		write(as.matrix(metadata_excluded_isolates), file=log_file, ncol=1, append=T)
	}

	metadata_in3 <- metadata_in2[! metadata_in$ID_isolate %in% metadata_excluded_isolates, ]

	# Check dates

	metadata_in3$Date <- as.Date(metadata_in3$Date, format="%Y-%m-%d")

	metadata_excluded_dates <- metadata_in3[as.numeric(metadata_in3$Date) < 0 | is.na(metadata_in3$Date), "ID_isolate"]

	if (length(metadata_excluded_dates) > 0)
	{
		txt <- paste("\r\nThe following isolates will be not included in the barplot and timeline plots because of errors in the date repoted in the input table, try to check the date format in the input sheet (yyyy-mm-dd):\r\n", sep="")
		write(txt, file=log_file, append=T)
		write(as.matrix(metadata_excluded_dates), file=log_file, ncol=1, append=T)
	}


	metadata_in4 <- metadata_in3[! metadata_in3$ID_isolate %in%  metadata_excluded_dates,]

	txt <- paste("\r\nDONE!\r\n" , sep="")
	write(txt, file=log_file, append=T)


}



#############
### Reference

if (with_background == "y")
{
	txt <- paste("\r\n\r\n3. Check Reference isolates ...\r\n", sep="")
	write(txt,file = log_file, append=T)
	
	#remove the rows where id_isolate is missing
	bkg=bkg[!is.na(bkg[,"ID_isolate"]),]
	
	background_NA_count <- rowSums(is.na(bkg))
	background_empty_count <- rowSums(bkg == "")
	background_empty_count [is.na(background_empty_count )] <- 1

	background_excluded_isolates <- bkg[background_NA_count + background_empty_count > 0, "ID_isolate"]

	if (length(background_excluded_isolates) > 0)
	{
		txt <- paste("\r\nThe following reference isolates will be not included in analysis, some data is missing:\r\n", sep="")
		write(txt, file=log_file, append=T)
		write(as.matrix(background_excluded_isolates), file=log_file, ncol=1, append=T)
	}

	bkg2 <- bkg[! bkg$ID_isolate %in% background_excluded_isolates, ]

	# All HRM teperatures range within the clus_thr threshold

	#converts temperature in numeric (if they were text)
	bkg2check <- bkg2[,4:bkg_ncol]
	bkg2check[,1:ncol(bkg2check)]=data.frame(sapply(bkg2check[,1:ncol(bkg2check)], function(x) as.numeric(as.character(x))))
	bkg2check_range <- apply(bkg2check, 1, function(x) abs(as.numeric(max(x))-as.numeric(min(x))))

	bkg_excl_range_hrm_val <- bkg[bkg2check_range > clus_thr, "ID_isolate"]

	if (length(bkg_excl_range_hrm_val) > 0)
	{
	        txt <- paste("\r\nThe following isolates will be exluded because HRM replicates range above ", clus_thr, ":\r\n" , sep="")
	        write(txt, file=log_file, append=T)
	        write(as.matrix(bkg_excl_range_hrm_val), file=log_file, ncol=1, append=T)
	}

	bkg3 <- bkg2[!bkg2$ID_isolate %in% bkg_excl_range_hrm_val,]



	# c) no duplicates or absent set

	bkg_org_set <- table(bkg3$ID_isolate, bkg3$Primers_set)

	bkg_org_set_MIN <- apply(bkg_org_set, 1, min)
	bkg_org_set_MAX <- apply(bkg_org_set, 1, max)

	bkg_excl_wrong_set <- row.names(bkg_org_set[bkg_org_set_MAX != 1 | bkg_org_set_MIN != 1,])

	# Write name in the log file

	if (length(bkg_excl_wrong_set) > 0)
	{
	txt <- paste("\r\nThe following reference isolates will be exluded because of wrong number of temperature relative to the primer sets (check if some temperatures are absent or duplicated):\r\n" , sep="")
	write(txt, file=log_file, append=T)
	write(as.matrix(bkg_excl_wrong_set), file=log_file, ncol=1, append=T)
	}

	bkg4 <- bkg3[!bkg3$ID_isolate %in% bkg_excl_wrong_set,]

	##check annotation field < 15 characters	
	
	bkg_long_annotation=bkg[nchar(as.matrix(bkg$Annotation))>15,"ID_isolate"]
	
	if (length(bkg_long_annotation) > 0)
	{
	txt <- paste("\r\nThe following reference isolates will be exluded because their annotation is longher than 15 characters:\r\n" , sep="")
	write(txt, file=log_file, append=T)
	write(as.matrix(bkg_long_annotation), file=log_file, ncol=1, append=T)
	}
		
	bkg <- bkg4[!bkg3$ID_isolate %in% bkg_long_annotation,]


	#### all the isolates must have a temps for each primer set

	
	isolates_ref_names = unique(bkg$ID_isolate)
	primers_ref_names = unique(bkg$Primers_set)
	
	primer_miss_ref_ids = c()
	for (iso_name in isolates_ref_names)
	{
		present_primers = bkg[bkg$ID_isolate==iso_name,"Primers_set"]
		if (length(present_primers) != length(primers_ref_names))
		{
			primer_miss_ref_ids = append(primer_miss_ref_ids,iso_name)
		}
		
	}
	
	
	# Write name in the log file

	if (length(primer_miss_ref_ids) > 0)
	{
		txt <- paste("\r\nThe following isolates will be exluded because temperature relative to some primer sets are missing:\r\n" , sep="")
		write(txt, file=log_file, append=T)
		write(as.matrix(primer_miss_ref_ids), file=log_file, ncol=1, append=T)
	}

	bkg <- bkg[!bkg$ID_isolate %in% primer_miss_ref_ids,]

	txt <- paste("\r\nDONE!\r\n" , sep="")
	write(txt, file=log_file, append=T)
}


txt <- paste("\r\n***HRM-based clustering/typing starts***\r\n" , sep="")
write(txt, file=log_file, append=T)

##############################################################
##############################################################
###########  1) CLUSTERING ANALYSIS  #########################
##############################################################
##############################################################


####################
### FIXED TABLES

hrm_temp_table <- temp_in3
hrm_temp_table <- droplevels(hrm_temp_table)

if (with_metadata == "y")
{
	metadata_table <- metadata_in4
	metadata_table <- droplevels(metadata_table)
}

if (with_background == "y")
{
	bkg_temp_table = bkg
	bkg_temp_table = droplevels(bkg_temp_table)

	bkg_table_unique <- unique(bkg_temp_table[,1:2])
	row.names(bkg_table_unique) <- bkg_table_unique$ID_isolate
}

####################
### DISTANCE MATRIX

# Compute the average temperature matrix

temp2mean <- hrm_temp_table[,3:temp_ncol]

#converts temperature in numeric (if they were text)
temp2mean=data.frame(sapply(temp2mean, function(x) as.numeric(as.character(x))))

Mean_T <- rowMeans(temp2mean)

hrm_temp_table_with_mean <- cbind(hrm_temp_table, Mean_T)

if (with_background == "y")
{
	bkg_temp2mean <- bkg_temp_table[,4:bkg_ncol]
	bkg_temp2mean=data.frame(sapply(bkg_temp2mean, function(x) as.numeric(as.character(x))))
	bkg_Mean_T <- rowMeans(bkg_temp2mean)
	bkg_temp_table_with_mean <- cbind(bkg_temp_table, bkg_Mean_T)
	bkg_temp_table_with_mean$Annotation <- NULL

	colnames(bkg_temp_table_with_mean)[colnames(bkg_temp_table_with_mean) == "bkg_Mean_T"] <- "Mean_T"

	hrm_temp_table_with_mean <- rbind(hrm_temp_table_with_mean, bkg_temp_table_with_mean)

}


# Create empty matrix

temp_mat_col <- unique(hrm_temp_table_with_mean$Primers_set)
temp_mat_row <- unique(hrm_temp_table_with_mean$ID_isolate)

temp_mat <- matrix(nrow = length(temp_mat_row), ncol = length(temp_mat_col))
row.names(temp_mat) <- temp_mat_row
colnames(temp_mat) <- temp_mat_col

# Fill in the matrix

for (i in 1:nrow(hrm_temp_table_with_mean))
{
        org <- hrm_temp_table_with_mean[i,"ID_isolate"]
        set <- hrm_temp_table_with_mean[i,"Primers_set"]
        mean <- hrm_temp_table_with_mean[i,"Mean_T"]

        temp_mat[as.character(as.matrix(org)), as.character(as.matrix(set))] <- mean
}

# Make distance matrix

m_dist <- matrix(nrow = length(temp_mat_row), ncol= length(temp_mat_row))
row.names(m_dist) <- temp_mat_row
colnames(m_dist) <- temp_mat_row

for (i in 1:(length(temp_mat_row)-1))
{
	for (u in (i+1):length(temp_mat_row))
	{

		org1 <- as.matrix(row.names(m_dist)[i])
		org2 <- as.matrix(row.names(m_dist)[u])

		org_tab <- temp_mat[c(org1,org2),]

		diff_tab <- apply(org_tab, 2, function(x) abs(diff(x)))
		diff <- sum(diff_tab > clus_thr)

		m_dist[as.matrix(org1),as.matrix(org2)] <- diff
		m_dist[as.matrix(org2),as.matrix(org1)] <- diff

	}

}


################################################
### GRAPH-BASED CLUSTERING (GRAPH + DENDROGRAM)

# Convert the matrix to binary

m_dist_zero <- ifelse(m_dist == 0, 1, 0)

# create the graph

hrm_graph_initial <- simplify(graph_from_adjacency_matrix(m_dist_zero, mode="undirected"))

# Remove links with betweeness over a threshold

eb <- edge_betweenness(hrm_graph_initial)



#edge betweennes normalization:
if (min(eb) != max(eb))
{
	eb_norm= (eb-min(eb))/(max(eb)-min(eb))
}else
{
	eb_norm=eb
}




edges_rm <- E(hrm_graph_initial)[eb_norm > eb_thr]

if (length(edges_rm) > 0)
{
hrm_graph <- delete_edges(hrm_graph_initial, edges_rm)
}else{
hrm_graph <- hrm_graph_initial
}


####################################
# BETWEEENESS PER CLUSTER

hrm_graph_dec <- decompose(hrm_graph)

# finding Betweenness clusters for each graph
btw_tab<- matrix(ncol=3, nrow=0)

colnames(btw_tab) <- c("node","Betweenness","Betweenness_clusters")

max_clus = 0


for (i in 1:length(hrm_graph_dec))
{
	if (length(V(hrm_graph_dec[[i]])$name) > 1)
	{
		cls <- cluster_edge_betweenness(hrm_graph_dec[[i]])
		btw <- betweenness(hrm_graph_dec[[i]], normalized=T)

	}else

	{
		cls <- list()
		cls$membership = 1
		btw <- c(0)
	}
	tmp <- cbind.data.frame(V(hrm_graph_dec[[i]])$name, btw, cls$membership+max_clus)
	colnames(tmp) <- colnames(btw_tab)

	btw_tab <- rbind(btw_tab, tmp)

	max_clus <- max_clus + max(cls$membership)
}

row.names(btw_tab) <- btw_tab[,1]
btw_tab[,1] <- NULL
#sometimes the normalization of betweenness gives NaN insted of 0, so it substitute them
btw_tab$Betweenness[is.na(btw_tab$Betweenness)] <- 0



# giving names to clusters
merge_clus_tab<-btw_tab

for (strain in rownames(btw_tab))
{
	btw_clus_num = as.numeric(btw_tab[strain, "Betweenness_clusters"])
	btw_clus_name = ifelse(btw_clus_num < 10, paste("Cluster_0", btw_clus_num, sep=""), paste("Cluster_", btw_clus_num, sep=""))
	merge_clus_tab[strain, "Betweenness_clusters"] = btw_clus_name

}



# undetermined table
und_tab <- merge_clus_tab[merge_clus_tab[,"Betweenness"] > btw_thr, ]

if (nrow(und_tab) > 0)
{
	und_clusters=und_tab$Betweenness_clusters
	merge_clus_tab[merge_clus_tab[,"Betweenness"] > btw_thr, "Betweenness_clusters"] <- und_string
}

#### Change names to clusters on the basis of background

if (with_background == "y")
{
	bkg_table_unique_clus <- merge(bkg_table_unique, merge_clus_tab, by.x="ID_isolate", by.y="row.names", all.x=T)
	bkg_clus_uniq <- unique(bkg_table_unique_clus[,c("Annotation","Betweenness_clusters")])
	bkg_clus_uniq_aggr <- aggregate(Annotation ~ Betweenness_clusters, bkg_clus_uniq, paste, collapse = "#-#")
	bkg_clus_uniq_aggr <- bkg_clus_uniq_aggr[bkg_clus_uniq_aggr$Betweenness_clusters != und_string,]
	row.names(bkg_clus_uniq_aggr) <- bkg_clus_uniq_aggr$Betweenness_clusters
	bkg_clus_uniq_aggr$Annotation <- paste("#C#", bkg_clus_uniq_aggr$Annotation,sep="")


	merge_clus_tab$Betweenness_clusters <- ifelse(merge_clus_tab$Betweenness_clusters %in% bkg_clus_uniq_aggr$Betweenness_clusters, bkg_clus_uniq_aggr[as.matrix(merge_clus_tab$Betweenness_clusters), "Annotation"], merge_clus_tab$Betweenness_clusters)

}


# getting cluster list

cluster_list=sort(unique(merge_clus_tab$Betweenness_clusters))
tot_clus = length(cluster_list)


# Colors attribution (Undetermined MUST be alphabetically after the clusters)

if (nrow(und_tab) > 0)
{
	color_hexa <- c(clusters_palette[1:tot_clus-1], col_und)
} else {

	color_hexa <- c(clusters_palette[1:tot_clus])
}

cluster_color<- cbind.data.frame(cluster_list, color_hexa)
rownames(cluster_color) = cluster_list
cluster_color$cluster_list=NULL
cluster_color$cluster_text = gsub("#-#", "\n", row.names(cluster_color))
cluster_color$cluster_text  = gsub("#C#", "", cluster_color$cluster_text)

txt <- paste("\r\n*plotting 1a) Isolates graph*\r\n" , sep="")
write(txt, file=log_file, append=T)


##############################################################
###########  PLOT 1a) ISOLATE GRAPH  #########################
##############################################################

merge_clus_tab$color_hexa = cluster_color[merge_clus_tab$Betweenness_clusters,"color_hexa"]

clus_tab_ord_graph <- merge_clus_tab[as.matrix(V(hrm_graph)$name),]
color_tab_ord_graph <-as.character(clus_tab_ord_graph$color_hexa)

#betweenness to vertex size
btw_ord=clus_tab_ord_graph$Betweenness
btw_score = (1-btw_ord)^1.8
vertex_max_size=10
vertex_size=btw_score * vertex_max_size


out_isolate_graph_width = length(rownames(temp_mat))/2
out_isolate_graph_height = length(rownames(temp_mat))/2


clus_graph_hrm_legend_size = out_isolate_graph_width / 10
clus_graph_title_size = clus_graph_hrm_legend_size * 1.3

if (with_background == "y")
{
	V(hrm_graph)$label.cex = ifelse(V(hrm_graph)$name %in% bkg_table_unique$ID_isolate, 0.0001, clus_graph_title_size*0.75)
}else
{
	V(hrm_graph)$label.cex = clus_graph_title_size*0.75

}
V(hrm_graph)$color <- adjustcolor(color_tab_ord_graph, alpha.f = .4)
V(hrm_graph)$label.color = clus_graph_label_color



clus_graph_weight = (347/14100)-(1/85500)*length(E(hrm_graph))
E(hrm_graph)$weight <- rep(clus_graph_weight, length(E(hrm_graph)))

edge_width=(1-eb_norm)*(max(vertex_size)/1.5)

lay <- layout.auto(hrm_graph)
subtitle = paste("smaller nodes = uncertain clustering, thinner link = uncertain connection")


#the size of the pdf and of the png depends on the number of strains


#min PDF sizes
if (out_isolate_graph_width < 8)
{
out_isolate_graph_width = 8
out_isolate_graph_height = 8

}



pdf(out_isolate_graph_pdf, out_isolate_graph_width, out_isolate_graph_height)
par(mar = c(0.5, 0.5, clus_graph_title_size*2, clus_graph_hrm_legend_size*10))
plot(hrm_graph, layout = lay, vertex.frame.color =  color_tab_ord_graph,vertex.size=vertex_size, edge.width = edge_width)
mtext(subtitle,adj=0, cex=clus_graph_title_size*0.75)
title(main = list("1a) HRM-based clustering/typing: isolates graph", cex = clus_graph_title_size),adj=0)
add_legend("right", legend=cluster_color$cluster_text, pch=19, col=as.character(cluster_color[,1]),pt.cex=(1+clus_graph_hrm_legend_size), cex=clus_graph_hrm_legend_size, ncol = 1, bty="n", title = "HRM_Cluster")
dev.off()
if (png_resolution>0)
{
png(out_isolate_graph_png, width= out_isolate_graph_width , height = out_isolate_graph_height, units = 'in', res = png_resolution)
par(mar = c(0.5, 0.5, clus_graph_title_size*2, clus_graph_hrm_legend_size*10))
plot(hrm_graph, layout = lay, vertex.frame.color =  color_tab_ord_graph,vertex.size=vertex_size, edge.width = edge_width)
mtext(subtitle,adj=0, cex=clus_graph_title_size*0.75)
title(main = list("1a) HRM-based clustering/typing: isolates graph", cex = clus_graph_title_size),adj=0)
add_legend("right", legend=cluster_color$cluster_text, pch=19, col=as.character(cluster_color[,1]),pt.cex=(1+clus_graph_hrm_legend_size), cex=clus_graph_hrm_legend_size, ncol = 1, bty="n", title = "HRM_Cluster")
dev.off()
}

txt <- paste("\r\nDONE\r\n" , sep="")
write(txt, file=log_file, append=T)


##############################################################
##########  PLOT 1b) ISOLATE HEATMAP  ########################
##############################################################
txt <- paste("\r\n*plotting 1b) Isolates heatmap*\r\n" , sep="")
write(txt, file=log_file, append=T)



# plot preparation
clus_tab_ord <- merge_clus_tab[order(merge_clus_tab$Betweenness_clusters),"Betweenness_clusters",drop=F]

clus_tab_ord_fact <- factor(clus_tab_ord[,"Betweenness_clusters"])

# format values for heatmap
temp_mat_ord <- temp_mat[as.matrix(row.names(clus_tab_ord)),]
temp_mat_ord_format <- format(round(temp_mat_ord, 2), nsmall = 2)

# set plot parameters

n_primers=length(colnames(temp_mat_ord))
lower_margin=max(nchar(colnames(temp_mat_ord)))

labels_max_length=max(nchar(rownames(temp_mat_ord)))
if (labels_max_length > 30)
{
	labels_max_length = 30
	right_margin=(labels_max_length*1.3)+(n_primers*2)

}else if (labels_max_length > 20)
{
	right_margin=(labels_max_length)+(n_primers*2)
}else
{
	right_margin= 20 +(n_primers*2)
}



dendro_height=(nrow(temp_mat_ord_format)/1.8)
dendro_label_height=nrow(temp_mat_ord_format)/3
dendro_text_factor = 1.2
dendro_width =right_margin/3.1

#min PDF sizes
if (dendro_height < 10)
{
	dendro_height=5
	dendro_label_height=2
	dendro_text_factor = 0.9
}





if (with_background == "y")
{
	col_Row = ifelse(rownames(temp_mat_ord) %in% bkg_temp_table$ID_isolate, "grey40", "black")
	subtitle_dendro = "       Reference strains' names are gray"
}else
{
	col_Row = "black"
	subtitle_dendro =""
}


dendro_cex_row <-  dendro_text_factor
dendro_cex_col <- dendro_text_factor
dendro_cex_note <- dendro_text_factor
dendro_hrm_legend_size = dendro_text_factor
dendro_title_size = dendro_text_factor*1.5

pdf(out_isolate_heatmap_pdf, width= dendro_width , height = dendro_height)
#external upper margin to fit title and subtitle
par( oma=c(0,0,dendro_title_size*2,0))

heatmap.2(as.matrix(temp_mat_ord), Rowv=FALSE, dendrogram = "none", trace = "none", srtCol=45, cexRow=dendro_cex_row, cexCol=dendro_cex_col, col=values_palette, RowSideColors=as.character(cluster_color[clus_tab_ord_fact,1]),key.xlab="°C",key.ylab="", denscol="black",density.info='density',key.title="",cellnote = temp_mat_ord_format, notecol="black", colRow = col_Row, notecex=dendro_cex_note, lhei=c(1,dendro_label_height),lwid=c(1,4), margins=c(lower_margin,right_margin))

add_legend("right", legend=cluster_color$cluster_text, pch=15, col=as.character(cluster_color[,1]),pt.cex=(1+dendro_hrm_legend_size), cex=dendro_hrm_legend_size, ncol = 1, bty="n", title = "HRM_Cluster")

#new blank plot to add title and subtitle (new=T to overwite, mar=... to fit the texts, oma=.. avoid further blank space)
par(new=T,mar = c(0.5, 0.5, dendro_title_size*2, dendro_hrm_legend_size*10),oma=c(0,0,0,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
title(main = list("1b) HRM-based clustering/typing: isolates heatmap", cex = dendro_title_size),adj = 0)
mtext(subtitle_dendro,adj=0,padj=0.1, cex=dendro_title_size*0.75)


dev.off()


if (png_resolution>0)
{
png(out_isolate_heatmap_png, width= dendro_width , height = dendro_height, units = 'in', res = png_resolution)
#external upper margin to fit title and subtitle
par( oma=c(0,0,dendro_title_size*2,0))

heatmap.2(as.matrix(temp_mat_ord), Rowv=FALSE, dendrogram = "none", trace = "none", srtCol=45, cexRow=dendro_cex_row, cexCol=dendro_cex_col, col=values_palette, RowSideColors=as.character(cluster_color[clus_tab_ord_fact,1]),key.xlab="°C",key.ylab="", denscol="black",density.info='density',key.title="",cellnote = temp_mat_ord_format, notecol="black", colRow = col_Row, notecex=dendro_cex_note, lhei=c(1,dendro_label_height),lwid=c(1,4), margins=c(lower_margin,right_margin))

add_legend("right", legend=cluster_color$cluster_text, pch=15, col=as.character(cluster_color[,1]),pt.cex=(1+dendro_hrm_legend_size), cex=dendro_hrm_legend_size, ncol = 1, bty="n", title = "HRM_Cluster")

#new blank plot to add title and subtitle (new=T to overwite, mar=... to fit the texts, oma=.. avoid further blank space)
par(new=T,mar = c(0.5, 0.5, dendro_title_size*2, dendro_hrm_legend_size*10),oma=c(0,0,0,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
title(main = list("1b) HRM-based clustering/typing: isolates heatmap", cex = dendro_title_size),adj = 0)
mtext(subtitle_dendro,adj=0,padj=0.1, cex=dendro_title_size*0.75)
}


#####################  cluster table  #################
tab2file=data.frame("ID_isolate"=rownames(merge_clus_tab),"HRM_Cluster"=merge_clus_tab$Betweenness_clusters,"Betweenness"=merge_clus_tab$Betweenness, "color_hexa"=merge_clus_tab$color_hexa)
tab2file$HRM_Cluster = gsub("#-#", " & ", tab2file$HRM_Cluster)
tab2file$HRM_Cluster = gsub("#C#", "", tab2file$HRM_Cluster)

write.xlsx(tab2file, file = out_table_clus_xls,row.names=F)

txt <- paste("\r\nDONE\r\n" , sep="")
write(txt, file=log_file, append=T)


##############################################################
###################  when -meta n   ##########################
###################  IT STOPS HERE  ##########################
##############################################################


if (with_metadata == "y")
{
	# merge clustering and metadata info

	metadata_table_clus <- merge(metadata_table, merge_clus_tab, by.x="ID_isolate", by.y="row.names", all.x=T)
	HRM_Cluster=metadata_table_clus$Betweenness_clusters
	metadata_table_clus=cbind.data.frame(metadata_table_clus,HRM_Cluster)

	#check for no hrm temp
	metadata_table_clus$HRM_Cluster <- ifelse(is.na(metadata_table_clus$HRM_Cluster), nohrm_string, as.matrix(metadata_table_clus$HRM_Cluster))

	metadata_table_clus$color_hexa=cluster_color[metadata_table_clus$HRM_Cluster,"color_hexa"]

	columns2write=c("ID_isolate" ,"ID_patient","Date","Location","HRM_Cluster","Betweenness", "color_hexa")
	metadata_table_clus_2write = metadata_table_clus[,columns2write]
	metadata_table_clus_2write$HRM_Cluster = gsub("#-#", " & ", metadata_table_clus_2write$HRM_Cluster)	
	metadata_table_clus_2write$HRM_Cluster = gsub("#C#", "", metadata_table_clus_2write$HRM_Cluster)

	write.xlsx(metadata_table_clus_2write, file = out_table_xls, row.names=F)

	# HRM Cluster colors

	
	txt <- paste("\r\n***Prevalence analysis starts***\r\n" , sep="")
	write(txt, file=log_file, append=T)
	##############################################################
	##############################################################
	###########  2)  PREVALENCE ANALYSIS  ########################
	##############################################################
	##############################################################


	###
	# ALL isolates
	###

	metadata_table_clus_2barplot <- cbind.data.frame(metadata_table_clus, rep(1, nrow(metadata_table_clus)))
	colnames(metadata_table_clus_2barplot)[ncol(metadata_table_clus_2barplot)] <- "count"

	month<-as.Date(paste(strftime(metadata_table_clus_2barplot$Date, format="%Y-%m"), "-1",sep=""), format="%Y-%m-%d")

	metadata_table_clus_2barplot <- cbind(metadata_table_clus_2barplot, month)

	num_location <- length(unique(metadata_table_clus_2barplot$Location))
	max_num_date <- max(table(metadata_table_clus_2barplot$Date))
	max_num_month <- max(table(metadata_table_clus_2barplot$month))
	day_plot_width = as.integer(max(as.Date(as.character(metadata_table_clus$Date)))-min(as.Date(as.character(metadata_table_clus$Date))))
	date_barplot_size_day = day_plot_width/15
	count_barplot_size = num_location

	#set palette and nemes for legends
	cluster_color$color_hexa<-as.character(cluster_color$color_hexa)
	cluster_color$cluster_text<-as.character(cluster_color$cluster_text)
	pal=setNames(cluster_color[,"color_hexa"],rownames(cluster_color))
	lab=setNames(cluster_color[,"cluster_text"],rownames(cluster_color))


	##############################################################
	##########  PLOT 2a) isolate location daily  #################
	##############################################################
	txt <- paste("\r\n*plotting 2a) Isolates location (daily)*\r\n" , sep="")
	write(txt, file=log_file, append=T)
	
	barplot_day_width = day_plot_width *0.2
	barplot_day_height = num_location
	#min PDF sizes
	if(barplot_day_width < 10)
	{
		barplot_day_width = 10
		date_barplot_size_day = 8
	}
	if(barplot_day_height < 10)
	{
		barplot_day_height  = 10
		count_barplot_size = 8
	}


	val <- vector()
	for (clus in rownames(cluster_color))
	{

	}





	barplot_day <- ggplot(metadata_table_clus_2barplot, aes(x=Date, y=count, fill=HRM_Cluster)) +
	geom_bar(stat="identity", width = 0.5) +
	facet_wrap(~Location, nrow=num_location) +
	scale_x_date(labels = date_format("%d-%m-%Y"), breaks = date_breaks("2 day"), minor_breaks= "day") +
	scale_fill_manual(values=pal, labels=lab) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1, size = date_barplot_size_day), axis.text.y = element_text(size = count_barplot_size) , strip.background =element_rect(fill="gray90") , strip.text = element_text(colour = 'black')) +
	ggtitle("2a) Prevalence analysis: all isolates per location (daily)") +
	scale_y_continuous(breaks=seq(0,max_num_date,by=1), minor_breaks= NULL)


	g <- ggplot_gtable(ggplot_build(barplot_day))
	stript <- which(grepl('strip-t', g$layout$name))
	fills <- loc_palette
	k <- 1
	for (i in rev(stript))
	{
		j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
 		g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
		k <- k+1
	}



	pdf(out_isolate_location_day_pdf, barplot_day_width, barplot_day_height)
	print (grid::grid.draw(g), height = num_location)
	dev.off()

	if (png_resolution>0)
	{
	png(out_isolate_location_day_png, barplot_day_width, barplot_day_height, units = 'in', res = png_resolution)
	print (grid::grid.draw(g), height = num_location)
	dev.off()
	}
	txt <- paste("\r\nDONE\r\n" , sep="")
	write(txt, file=log_file, append=T)
	##############################################################
	##########  PLOT 2b) isolate location monthly  ###############
	##############################################################
	months_amount= ceiling(day_plot_width/30)
	
	
	#minimum width for monthly
	if (months_amount< 7.5) {monthly_width = 7.5} else {monthly_width = months_amount}
	
	#depends on the height of the pdf
	date_barplot_size_month = barplot_day_height


	#if the lenght of the period analised in days is below plot_month_thr days, no month plot will be plotted
	if (day_plot_width < plot_month_thr)
	{
	plot_month_tag=FALSE
	}else
	{
	plot_month_tag=TRUE
	}

	if (plot_month_tag)
	{
	txt <- paste("\r\n*plotting 2b) Isolates location (monthly)*\r\n" , sep="")
	write(txt, file=log_file, append=T)
	months_amount= ceiling(day_plot_width/30)
	
	
	barplot_month <- ggplot(metadata_table_clus_2barplot, aes(x=month, y=count, fill=HRM_Cluster)) + geom_bar(stat="identity", width = 3) + facet_wrap(~Location, nrow=num_location) + scale_x_date(labels = date_format("%b-%Y"), breaks = date_breaks("month"), minor_breaks= NULL) + scale_fill_manual(values=pal, labels=lab) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = date_barplot_size_month), axis.text.y = element_text(size = count_barplot_size), strip.background =element_rect(fill="gray90"), strip.text = element_text(colour = 'black')) + ggtitle("2b) Prevalence analysis: all isolates per location (monthly)") + scale_y_continuous(breaks=seq(0,max_num_month,by=1), minor_breaks= NULL)

	g <- ggplot_gtable(ggplot_build(barplot_month))
	stript <- which(grepl('strip-t', g$layout$name))
	fills <- loc_palette
	k <- 1
	for (i in rev(stript))
	{
		j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
 		g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
		k <- k+1
	}



	pdf(out_isolate_location_month_pdf,monthly_width, barplot_day_height)
	print (grid::grid.draw(g), height = num_location)
	dev.off()
	if (png_resolution>0)
	{
	png(out_isolate_location_month_png, monthly_width, barplot_day_height, units = 'in', res = png_resolution)
	print (grid::grid.draw(g), height = num_location)
	dev.off()
	}
	txt <- paste("\r\nDONE\r\n" , sep="")
	write(txt, file=log_file, append=T)
	months_amount= ceiling(day_plot_width/30)
	}


	##############################################################
	#####  PLOT 2c) positivization location daily  ###############
	##############################################################
	txt <- paste("\r\n*plotting 2c) Positivizations location (daily)*\r\n" , sep="")
	write(txt, file=log_file, append=T)
	
	months_amount= ceiling(day_plot_width/30)

	# sort by date
	metadata_table_clus_2barplot_ord <- metadata_table_clus_2barplot[order(metadata_table_clus_2barplot$Date),]

	# get the first patient-cluster case (positivized moment)
	metadata_table_clus_2barplot_ord_nodup <- metadata_table_clus_2barplot_ord[!duplicated(paste(metadata_table_clus_2barplot_ord$ID_patient, metadata_table_clus_2barplot_ord$HRM_Cluster)),]

	metadata_table_clus_2barplot_ord_nodup <- droplevels(metadata_table_clus_2barplot_ord_nodup)

	max_num_day <- max(table(metadata_table_clus_2barplot_ord_nodup$Date))
	max_num_month <- max(table(metadata_table_clus_2barplot_ord_nodup$month))



	loc_barplot_day <- ggplot(metadata_table_clus_2barplot_ord_nodup, aes(x=Date, y=count, fill=HRM_Cluster)) + geom_bar(stat="identity", width = 0.5) + scale_x_date(labels = date_format("%d-%m-%Y"), breaks = date_breaks("2 day"), minor_breaks= "day") + scale_fill_manual(values=pal, labels=lab) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = date_barplot_size_day), axis.text.y = element_text(size = count_barplot_size), strip.background =element_rect(fill="gray90"), strip.text = element_text(colour = 'black')) + facet_wrap(~Location, nrow=num_location) + ggtitle("2c) Prevalence analysis: positivizations per location (daily)") + scale_y_continuous(breaks=seq(0,max_num_day,by=1), minor_breaks= NULL)


	g <- ggplot_gtable(ggplot_build(loc_barplot_day))
	stript <- which(grepl('strip-t', g$layout$name))
	fills <- loc_palette
	k <- 1
	for (i in rev(stript))
	{
		j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
 		g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
		k <- k+1
	}

	pdf(out_positivization_location_day_pdf,barplot_day_width, barplot_day_height)
	print (grid::grid.draw(g))
	dev.off()
	if (png_resolution>0)
	{
	png(out_positivization_location_day_png,barplot_day_width, barplot_day_height, units = 'in', res = png_resolution)
	print (grid::grid.draw(g))
	dev.off()
	}
	
	txt <- paste("\r\nDONE\r\n" , sep="")
	write(txt, file=log_file, append=T)
	
	##############################################################
	#####  PLOT 2d) positivization location monthly  #############
	##############################################################

	if (plot_month_tag)
	{
	txt <- paste("\r\n*plotting 2d) Positivizations location (monthly)*\r\n" , sep="")
	write(txt, file=log_file, append=T)
	
	loc_barplot_month <- ggplot(metadata_table_clus_2barplot_ord_nodup, aes(x=month, y=count, fill=HRM_Cluster)) + geom_bar(stat="identity", width = 3) + scale_x_date(labels = date_format("%b-%Y"), breaks = date_breaks("month"), minor_breaks= NULL) + scale_fill_manual(values=pal, labels=lab) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = date_barplot_size_month), axis.text.y = element_text(size = count_barplot_size), strip.background =element_rect(fill="gray90"), strip.text = element_text(colour = 'black')) + facet_wrap(~Location, nrow=num_location) + ggtitle("2d) Prevalence analysis: positivizations per location (monthly)") + scale_y_continuous(breaks=seq(0,max_num_month,by=1), minor_breaks= NULL)

	g <- ggplot_gtable(ggplot_build(loc_barplot_month ))
	stript <- which(grepl('strip-t', g$layout$name))
	fills <- loc_palette
	k <- 1
	for (i in rev(stript))
	{
		j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
 		g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
		k <- k+1
	}



	pdf(out_positivization_location_month_pdf,monthly_width, barplot_day_height)
	print (grid::grid.draw(g))
	dev.off()
	if (png_resolution>0)
	{
	png(out_positivization_location_month_png,monthly_width, barplot_day_height, units = 'in', res = png_resolution)
	print (grid::grid.draw(g))
	dev.off()
	}
	txt <- paste("\r\nDONE\r\n" , sep="")
	write(txt, file=log_file, append=T)
	
	}


	##############################################################
	########  PLOT 2e) global positivization daily  ##############
	##############################################################
	txt <- paste("\r\n*plotting 2e) Positivizations global (daily)*\r\n" , sep="")
	write(txt, file=log_file, append=T)
	
	
	global_barplot_day <- ggplot(metadata_table_clus_2barplot_ord_nodup, aes(x=Date, y=count, fill=HRM_Cluster)) + geom_bar(stat="identity", width = 0.5) + scale_x_date(labels = date_format("%d-%m-%Y"), breaks = date_breaks("2 day"), minor_breaks= "day") + scale_fill_manual(values=pal, labels=lab) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = date_barplot_size_day), axis.text.y = element_text(size = count_barplot_size), strip.background =element_rect(fill="gray90"), strip.text = element_text(colour = 'black')) +
	 ggtitle("2e) Prevalence analysis: global positivizations (daily)") + scale_y_continuous(breaks=seq(0,max_num_day,by=1), minor_breaks= NULL)

	pdf(out_positivization_global_day_pdf,barplot_day_width, barplot_day_height)
	print (global_barplot_day)
	dev.off()

	if (png_resolution>0)
	{
	png(out_positivization_global_day_png ,barplot_day_width, barplot_day_height, units = 'in', res = png_resolution)
	print (global_barplot_day)
	dev.off()
	}

	txt <- paste("\r\nDONE\r\n" , sep="")
	write(txt, file=log_file, append=T)
		
	
	##############################################################
	#######  PLOT 2f) global positivization monthly  #############
	##############################################################
	if (plot_month_tag)
	{
	txt <- paste("\r\n*plotting 2f) Positivizations global (monthly)*\r\n" , sep="")
	write(txt, file=log_file, append=T)
	
	global_barplot_month <- ggplot(metadata_table_clus_2barplot_ord_nodup, aes(x=month, y=count, fill=HRM_Cluster)) + geom_bar(stat="identity", width = 3) + scale_x_date(labels = date_format("%b-%Y"), breaks = date_breaks("month"), minor_breaks= NULL) + scale_fill_manual(values=pal, labels=lab) + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = date_barplot_size_month), axis.text.y = element_text(size = count_barplot_size), strip.background =element_rect(fill="gray90"), strip.text = element_text(colour = 'black')) + ggtitle("2f) Prevalence analysis: global positivizations (monthly)") + scale_y_continuous(breaks=seq(0,max_num_month,by=1), minor_breaks= NULL)

	pdf(out_positivization_global_month_pdf,monthly_width,monthly_width)
	print (global_barplot_month)
	dev.off()

	if (png_resolution>0)
	{
	png(out_positivization_global_month_png,monthly_width, monthly_width, units = 'in', res = png_resolution)
	print (global_barplot_month)
	dev.off()
	}
	txt <- paste("\r\nDONE\r\n" , sep="")
	write(txt, file=log_file, append=T)
	
	}



	txt <- paste("\r\n***Transmission analysis starts***\r\n" , sep="")
	write(txt, file=log_file, append=T)

	##############################################################
	##############################################################
	############# 3)  TRANSMISION ANALYSIS  ######################
	##############################################################
	##############################################################

	##############################################################
	##############  PLOT 3a) patients timeline  ##################
	##############################################################
	txt <- paste("\r\n*plotting 3a) Patients timeline*\r\n" , sep="")
	write(txt, file=log_file, append=T)
	
	# set point size. bigger for positivized
	point_size <- ifelse(metadata_table_clus_2barplot$ID_isolate %in% metadata_table_clus_2barplot_ord_nodup$ID_isolate & metadata_table_clus_2barplot$HRM_Cluster != nohrm_string, positivized_size, not_positivized_size)
	point_alpha <- ifelse(metadata_table_clus_2barplot$ID_isolate %in% metadata_table_clus_2barplot_ord_nodup$ID_isolate & metadata_table_clus_2barplot$HRM_Cluster != nohrm_string, positivized_alpha, not_positivized_alpha)

	metadata_table_clus_2barplot_with_point_size <- cbind.data.frame(metadata_table_clus_2barplot, point_size, point_alpha)

	# format date
	metadata_table_clus_2barplot_with_point_size$Date <- as.POSIXct(as.character(metadata_table_clus_2barplot_with_point_size$Date))

	metadata_table_clus_2barplot_with_point_size$ID_patient = factor(metadata_table_clus_2barplot_with_point_size$ID_patient, levels = unique(metadata_table_clus_2barplot_with_point_size$ID_patient[order(metadata_table_clus_2barplot_with_point_size$Date)]))
		
	timeline <- ggplot(metadata_table_clus_2barplot_with_point_size, aes(Date, ID_patient))+
  scale_x_datetime(date_breaks='day',date_labels = "%d-%m-%Y", minor_breaks= NULL)+
  geom_point(data=metadata_table_clus_2barplot_with_point_size, aes(Date,ID_patient,color=HRM_Cluster,shape=Location),
             alpha=point_alpha, position=position_jitter(w=0.03,h=0.04),
             size=point_size,stroke = 1)+labs(color="HRM_Cluster", shape="Note: For each patient\nthe first isolate of each\nHRM cluster is bolder\r\n\r\n\r\n\r\nLocation")+
  theme(axis.text.x=element_text(angle=45,hjust = 1, size = 7), legend.key = element_rect(fill = "white"))+ guides(color= guide_legend(override.aes = list(shape=15, size = 5)),shape = guide_legend(override.aes = list(size = 4)))+
  scale_color_manual(values = pal,labels=lab)+
 scale_shape_manual(values=seq(1,25)) + ggtitle("3a) Transmission analysis: patients' timeline")

#  scale_shape_discrete(solid=T) +
	pdf(out_patients_timeline_pdf,barplot_day_width, barplot_day_height)
	print (timeline, height = num_location)
	dev.off()

	if (png_resolution>0)
	{
	png(out_patients_timeline_png,barplot_day_width, barplot_day_height, units = 'in', res = png_resolution)
	print (timeline, height = num_location)
	dev.off()
	}

	txt <- paste("\r\nDONE\r\n" , sep="")
	write(txt, file=log_file, append=T)
	
	##############################################################
	##############  PLOT 3a) patients graph  #####################
	##############################################################
	txt <- paste("\r\n*plotting 3b) Patient-to-patient graph*\r\n" , sep="")
	write(txt, file=log_file, append=T)

	# connection on the basis of HRM

# 	# get identified cluster only
	metadata_table_clus_2barplot_nounk <- metadata_table_clus_2barplot[metadata_table_clus_2barplot$HRM_Cluster != nohrm_string,]

	tograph <- merge(metadata_table_clus_2barplot_nounk, metadata_table_clus_2barplot_nounk, by="HRM_Cluster", all=T)
	tograph$Date.x <- as.Date(tograph$Date.x)
	tograph$Date.y <- as.Date(tograph$Date.y)
	day_dist <- abs(as.numeric(tograph$Date.x)-as.numeric(tograph$Date.y))
	tograph <- cbind(tograph,day_dist)
	tograph_ord <- tograph[order(tograph$day_dist),]
	tograph_nodup <- tograph_ord[tograph_ord$ID_patient.x != tograph_ord$ID_patient.y, ]

	e_color <- cluster_color[as.matrix(tograph_nodup$HRM_Cluster),1]
	e_day_dist <- as.numeric(tograph_nodup$day_dist)
	e_loc_x <- as.matrix(tograph_nodup$Location.x)
	e_loc_y <- as.matrix(tograph_nodup$Location.y)

	gtab_tmp <- as.matrix(cbind.data.frame(t(apply(tograph_nodup[,c("ID_patient.x","ID_patient.y")],1,sort)), e_color, e_day_dist, e_loc_x, e_loc_y))

	# graph for HRM
	gtab_hrm_tmp2 <- gtab_tmp[gtab_tmp[,1] != gtab_tmp[,2],]
	gtab_hrm <- as.data.frame(gtab_hrm_tmp2[!duplicated(gtab_hrm_tmp2[,c(1,2,3)]),])

	#check if is possible to plot the patient graph:
	#at least two edges are required!
	if (length(colnames(gtab_hrm)) < 2)
	{
	
	txt = "\r\nThere aren't enough possible transmissions to build the patient-to-patient graph\r\n"
	write(txt, file=log_file, append=T)

	write(txt,file = "Analysis_INTERRUPTED.log")

	
	current_time <- Sys.time()
	txt <- paste("\r\n##### MeltingPlot analysis INTERRUPTED ( ", current_time," ) #####\n\n", sep="")
	write(txt,file = log_file, append=T)
	
	quit()
	}
	
	graph_hrm <- graph_from_data_frame(gtab_hrm, directed = F)
	
	graph_hrm_elist_tmp <- as_edgelist(graph_hrm, names = TRUE)
	graph_hrm_elist <- paste(graph_hrm_elist_tmp[,1], graph_hrm_elist_tmp[,2], sep="_")

	# graph for location and day
	gtab_loc_tmp0 <- gtab_tmp[gtab_tmp[,1] != gtab_tmp[,2],]
	gtab_loc_tmp1 <- gtab_loc_tmp0[gtab_loc_tmp0[,"e_loc_x"] == gtab_loc_tmp0[,"e_loc_y"],]
	gtab_loc_tmp2 <- gtab_loc_tmp1[as.numeric(gtab_loc_tmp1[,"e_day_dist"]) <= day_thr,]
	gtab_loc <- as.data.frame(gtab_loc_tmp2[!duplicated(gtab_loc_tmp2[,c(1,2,3)]),])

	graph_loc <- graph_from_data_frame(gtab_loc, directed = F)

	graph_loc_elist_tmp <- as_edgelist(graph_loc, names = TRUE)
	graph_loc_elist <- paste(graph_loc_elist_tmp[,1], graph_loc_elist_tmp[,2], sep="_")

	
	tra_graph_bolder_edges_size = length(V(graph_hrm))/5.75
	tra_graph_not_bolder_edges_size = tra_graph_bolder_edges_size/4
	
	
	E(graph_hrm)$width <- ifelse(graph_hrm_elist %in% graph_loc_elist, tra_graph_bolder_edges_size, tra_graph_not_bolder_edges_size)

	# patients table vs location, then ordering nodes as the graph

	pat_loc <- as.matrix(table(metadata_table_clus_2barplot_nounk$ID_patient, metadata_table_clus_2barplot_nounk$Location))[as.matrix(V(graph_hrm)$name),]

	# converts in list
	pat_loc_list <- unname(split(pat_loc, 1:nrow(pat_loc)))

	V(graph_hrm)$pie.color <- list(loc_palette[1:ncol(pat_loc)])
	
	V(graph_hrm)$label.color = tra_graph_label_color
	
	tra_graph_weight = (347/14100)-(1/85500)*length(E(graph_hrm))
	E(graph_hrm)$weight <- rep(tra_graph_weight, length(E(graph_hrm)))
	

	lay <- layout.auto(graph_hrm)

	subtitle = paste("Note: links of connections occurrend within ", day_thr, " days are bolder", sep="")

	out_patient_graph_width = length(V(graph_hrm))*0.8
	out_patient_graph_height = length(V(graph_hrm))*0.8




	tra_graph_location_legend_size = out_patient_graph_width /9

	tra_graph_hrm_legend_size = tra_graph_location_legend_size
	
	tra_graph_title_size=tra_graph_location_legend_size*1.5
	
	#pie sizes
	tra_graph_vertex_cex = 15
	
	#pie labels sizes
	V(graph_hrm)$label.cex = tra_graph_title_size*0.55
	

	
	margin_loc_legend = length(colnames(pat_loc))*2

	metadata_table_clus_paz =  metadata_table_clus[metadata_table_clus$ID_patient %in% V(graph_hrm)$name,]

	unique_clusters = unique(metadata_table_clus_paz$HRM_Cluster)


	legend_text = cluster_color[unique_clusters,"cluster_text"]

	legend_col = as.character(cluster_color[unique_clusters,"color_hexa"])

	pdf(out_patients_graph_pdf, out_patient_graph_width, out_patient_graph_height)
	par(mar = c(margin_loc_legend, 0,tra_graph_title_size*2, tra_graph_location_legend_size*10))
	plot(graph_hrm, edge.color = E(graph_hrm)$e_color, vertex.shape="pie", vertex.pie=pat_loc_list, layout = lay, vertex.frame.color = NA, vertex.size = tra_graph_vertex_cex)
	mtext(subtitle,adj=1, cex=tra_graph_title_size*0.75)
	title(main = list("3b) Transmission analysis: patient-to-patient graph", cex = tra_graph_title_size), adj=1)

	add_legend("bottomleft", legend = colnames(pat_loc), pch=19 ,col = loc_palette[1:ncol(pat_loc)], ncol = 1, pt.cex=(1+tra_graph_location_legend_size), cex = tra_graph_location_legend_size, bty="n", title = "Location (nodes)")
	add_legend("right", legend=legend_text, lty=1, lwd=5, ,col=legend_col ,pt.cex=(1+tra_graph_hrm_legend_size), cex=tra_graph_hrm_legend_size, ncol = 1, bty="n", title = "HRM_Cluster (links)")
	dev.off()

	if (png_resolution>0)
	{
	png(out_patients_graph_png, out_patient_graph_width, out_patient_graph_height, units = 'in', res = png_resolution)
		par(mar = c(margin_loc_legend, 0,tra_graph_title_size*2, tra_graph_location_legend_size*10))
	plot(graph_hrm, edge.color = E(graph_hrm)$e_color, vertex.shape="pie", vertex.pie=pat_loc_list, layout = lay, vertex.frame.color = NA, vertex.size = tra_graph_vertex_cex)
	mtext(subtitle,adj=1, cex=tra_graph_title_size*0.75)
	title(main = list("3b) Transmission analysis: patient-to-patient graph", cex = tra_graph_title_size), adj=1)

	add_legend("bottomleft", legend = colnames(pat_loc), pch=19 ,col = loc_palette[1:ncol(pat_loc)], ncol = 1, pt.cex=(1+tra_graph_location_legend_size), cex = tra_graph_location_legend_size, bty="n", title = "Location (nodes)")
	add_legend("right", legend=legend_text, lty=1, lwd=5, ,col=legend_col ,pt.cex=(1+tra_graph_hrm_legend_size), cex=tra_graph_hrm_legend_size, ncol = 1, bty="n", title = "HRM_Cluster (links)")
	dev.off()


	}
	txt <- paste("\r\nDONE\r\n" , sep="")
	write(txt, file=log_file, append=T)
}

current_time <- Sys.time()
txt <- paste("\r\n##### MeltingPlot analysis END ( ", current_time," ) #####\n\n", sep="")
write(txt,file = log_file, append=T)


