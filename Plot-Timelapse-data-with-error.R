# Script that:
# Reads csv file with header, first column is time. Assumes that each of the other columns
# represents a single time-dependent measurement (e.g. the reponse of a single cell)
# Caluclates summary statistics per time-point (mean, sd, 95%CI)
# Plots time dependent data: single time-dependent measurements, average and error (95%CI)


# Created by: 
# Joachim Goedhart, @joachimgoedhart, 2017

#Requires the packages tidyr, ggplot2, magrittr, dplyr

require(tidyr)
require(ggplot2)


######################## Set some parameters #########################
#Confidence level, typically 95%
Conf_level <-  0.95

#Start and end point of stimulation
stim_start <- 44
stim_end <- 146

######################### import the data ##############################

#Read a text file (comma separated values)
df_wide <- read.csv("FRET-ratio-wide.csv", na.strings = "")


#Tidy the data, i.e. long format with each row is variable
df_tidy <- gather(df_wide, Cell, Ratio, -Time)

#Clear the df.summary dataframe if it exist
if (exists("df_summary")) rm(df_summary)


######### Calulcate summary statistics to fill dataframe 'df_summary' ########
# This is tidyverse approach

# require(magrittr)
# require(dplyr)
# df_summary <- df_tidy %>% group_by(Time) %>% summarise(mean = mean(Ratio, na.rm = TRUE),
            											# sd = sd(Ratio, na.rm = TRUE),
            											# n = n()) %>%
  		# mutate(sem = sd / sqrt(n - 1),
        # CI_lower = mean + qt((1-Conf_level)/2, n - 1) * sem,
        # CI_upper = mean - qt((1-Conf_level)/2, n - 1) * sem)


######### Calulcate summary statistics to fill dataframe 'df_summary' ########
# This is base R approach

df_summary <- data.frame(Time=df_wide$Time, n=tapply(df_tidy$Ratio, df_tidy$Time, length), mean=tapply(df_tidy$Ratio, df_tidy$Time, mean))

#Add SD and standard error of the mean to the dataframe
df_summary$sd <- tapply(df_tidy$Ratio, df_tidy$Time, sd)
df_summary$sem <- df_summary$sd/sqrt(df_summary$n-1)

#Add 95% CI of the mean to the dataframe
df_summary$CI_lower <- df_summary$mean + qt((1-Conf_level)/2, df=df_summary$n-1)*df_summary$sem
df_summary$CI_upper <- df_summary$mean - qt((1-Conf_level)/2, df=df_summary$n-1)*df_summary$sem

####################################################

#### Command to prepare the plot ####
ggplot(df_summary, aes(x=Time, y=mean)) +

#### plot individual measurements ####
	geom_line(data=df_tidy, aes(x=Time, y=Ratio, group=Cell), color="grey") +

#### plot average response over time ####
	geom_line(data=df_summary, aes(x=Time, y=mean), size=1, alpha=0.8)+


#### plot error (95%CI) of the response over time ####
	geom_ribbon(data=df_summary, aes(ymin=CI_lower, ymax=CI_upper) ,fill="blue", alpha=0.2)+

#### plot s.e.m. of the average response over time ####
	# geom_ribbon(data=df_summary, aes(ymin=mean-sem, ymax=mean+sem) ,fill="green", alpha=0.5)+


#### Draw a filled, transparant rectangle to indicte when stimulation takes place
	annotate("rect", xmin=stim_start, xmax=stim_end, ymin=-Inf, ymax=Inf, alpha=0.1, fill="black")+

## Set the Y-axis scale, remove for autoscale

	coord_cartesian(ylim = c(0.5, 1.05)) +
  
## Set theme&basic font
	theme_light(base_size = 16) +


  
### set the title
	ggtitle("Timeseries; average & 95%CI") +
    
### Style the axis (font size)
	# theme(axis.text.x = element_text(size=16, angle=0, vjust = 0), axis.text.y = element_text(size=16)) +

### Set layout of the graph 
	# theme(panel.border = element_rect(size = 1, linetype = "solid", colour = "black", fill=NA)) +

### Set label of x- and y-axis
	ylab("Ratio YFP/CFP [-]") + xlab("Time [s]") +
  
### Set aspect ratio of the graph n/n = square
	theme(aspect.ratio=4/4)
  


