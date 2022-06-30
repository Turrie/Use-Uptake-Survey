# Use-Uptake-Survey

The Use & Uptake dashboard serves to give a summarized, interactive presentation of the output from a survey developed and piloted as part of ACAI’s monitoring, evaluation and learning (MEL) system. The use and uptake survey is used to monitor the extent to which targeted end-users adopt and apply the AKILIMO tools as well as identify the initial factors influencing the adoption.  
This interactive dashboard provide access to timely feedback to allow adjustment of the dissemination and scaling strategy in order to reach maximum impact. 
 The steps to follow:
1.	Run or source the ‘global.R’ script
In this script you need the following input scripts found on this link:         /home/akilimo/projects/AKILIMO_Dashboards/Use&Uptake/Input/scripts
i.	‘AKILIMO_use_uptake.R’
This will require the ‘AKILIMO_use_uptake-perceptions_repeat.csv’ found here:            /home/akilimo/projects/AKILIMO_Dashboards/Use&Uptake/Input/data/form_data
                    And the following xlsx files 
          ‘AKILIMO use and uptake survey (without farmer details).xlsx
           ‘statements.xlsx’
      found here:
     /home/akilimo/projects/AKILIMO_Dashboards/Use&Uptake/Input/data/supplementary

ii.	PieDonutFunction.R
Here you need the uptake_use_cases.xlsx. Found on this link:   /home/akilimo/projects/AKILIMO_Dashboards/Use&Uptake/Input/scripts
You also need the following csv found here: /home/akilimo/projects/AKILIMO_Dashboards/Use&Uptake/Input/data/form_data
i.	dissemination_events_2021_06_16_05_52_07_849567.csv

Here is how to get the most out of the dashboard:
There are 4 panels:
1.	Overall
The landing page gives a brief summary of the use by uptake as well as uptake by use for each country.
2.	Use & Uptake
This panel provides 4 tabs: 
‘Uptake of 6 steps’ which gives charts on uptake of the six steps per usecase and by country.
‘By covariates’ which gives charts of AKILIMO tools disaggregated by gender and country.
‘By perceptions’ which gives charts of use by perception on attitude towards innovation by country.
‘Drivers of use and uptake’ that provides a rank of perceptions in order of impact on use by country.
There is a column on the left of each tab that provides filtering capabilities. This allows for each chart to be viewed by:
i.	Country/comparison of both countries
ii.	The four use cases FR, IC, SPHS & BPP
iii.	Grouping variables
iv.	Type of plot

3.	Map of events organized
This panel offers an interactive map providing an overview of the events organized as well as total participation. There is a floating panel that offers filtering capability and a legend to explain the colours. Click the “click here to view map” in order to see the output.
4.	AKILIMO reach
This panel provides an overview of how many farmers are reached through partners’ dissemination events, including training and sensitization events, field days and demonstrations, agric fairs and video shows. 
The ‘Overview of events organized’ tab provides bar plots that allow disaggregation by event type, partner, or use case, and stacked fills by event type, partner and use case. 
The ‘Total number of attendees’ tab provides details on AKILIMO reach, permitting disaggregation by partner, use case, event type, as well as stacked fills by gender, and faceting by country (default).

