# CSC481-SeniorProject

<p>Saint Martin’s University staff will be using test plugins on their Learning Management System, Moodle. Plugins for Moodle are add-ons to the base version of Moodle that add additional features that can be useful for both administrative and instructional purposes. The objective of this project is to analyze the effectiveness of the test plugins to improve the instructional quality of Saint Martin’s University using Moodle.</p>
<p><strong>Specifications</strong></p>
<p>This project will use logs generated by Moodle to analyze the effectiveness of plugins that will be tested during the Spring semester. To do this, I will be working with Saint Martin’s Information Technology Services to gather the logs that are stored from the Moodle vendor, eThink. The analysis of the plugins should clearly show the answer to the following:
<ul>
  <li>How often students are using the plugin?</li>
<li>Are the students using the plugin more than once?</li>
<li>Are the students using the plugin over the entire course of the semester?</li>
<li>What time of day are the students using the plugin?</li>
<li>Are students using the plugin before a specific activity like quizzes or discussions?</li>
<li>Do the new plugins change how students interact with other Moodle features?</li>
</ul>
<p>The answers to these questions will be useful for instructors that are trying to plan effective Moodle courses using these new plugins in the future. To provide a clearer answer, the logs of the test Moodle courses in the Spring will be compared to the logs from Moodle courses in the Fall. The logs from the Fall will be useful in showing if the use of the new plugins affects the usage of other base Moodle features like the viewership of notes and the interactivity of Moodle discussions.
</p>

<p><strong>Design</strong></p>
<p>The analysis of this project will be done entirely with R. The output of Moodle logs comes in the form of a CSV file which makes it easy to be imported into RStudio. Once the logs have been imported, a significant amount of data wrangling is needed to use the data. The Tidyverse package will provide most of the tools needed for the data cleaning. The data contains a few string multi-valued attributes and redundant log files. For example, the process of turning in an assignment generates four log files for the same action. While useful for error handling, the redundant logs could create misleading data if left alone. All types of redundant files will need to be resolved before analysis is complete to ensure that all data used to examine the usage of a Moodle feature is recorded accurately. To extract useful data from the string multi-valued attributes, regular expressions using the Rebus R package will be used. Finally, the date and time attribute of the log files will be expanded using functions from the Lubridate package. This will make it easy to track the usage of Moodle throughout the duration of the course and during each day. </p>

<p>Once the data is cleaned it will be prepared for visualizations using the Ggplot2 package. These visualizations with commentary will be the main method of presenting the findings of the analysis. They will use the cleaned Moodle logs to answer the questions detailed in the specification section. While it is undetermined at the moment if Saint Martin’s ITS will be able to get access to the logs regularly, if they are able to, the R scripts used for the analysis can be expanded for regular Moodle courses. Should this happen, a separate version of the R script used to create the report for the plugins tested in the Spring will be created to give a more general analysis of a Moodle course and provide visualizations. This result would likely be in the form of an RShiny Dashboard that would allow professors to view how students are interacting with their Moodle course at any time during the semester.</p>

<p><strong> Presentation </strong></p>

<p>The Powerpoint presentation of this project is located in the files of this repository.</p>

<p>This is the RPubs link for this project which contains the knitted version of the main R Markdown file of the project code, moodleSpringFinalWCode.Rmd.<br>
https://rpubs.com/braden1/751094</p>

<p>The HTML knitted version of the main R Markdown file is also hosted on the Github Pages of this repository.<br>
https://braden126.github.io/CSC481-SeniorProject/</p>

<p>This link is for the video presentation of the project and a demonstration of the code used for analysis and visualization.<br>
https://www.youtube.com/watch?v=YKMLm05_LEk</p>




