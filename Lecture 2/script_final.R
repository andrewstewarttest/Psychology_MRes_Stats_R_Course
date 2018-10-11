# First we need to load the packages we need - the following loads all packages within the
# tidyverse universe of packages (plus associated datasets).
# If you haven't yet installed them on your computers, you'll first need to type:
# install.packages("tidyverse") and install.packages("viridis")
library(tidyverse)
library(viridis)

# One dataset that is included in the tidverse packages contains information about the characters 
# in the Star Wars movies.  The dataset is called 'starwars'.  Note this is all lower case.
# R is sensitive to case - so 'starwars' is not the same as 'StarWars'
# You can just type the name of the dataset to see the first 10 lines
starwars

# You can open the full dataset in a Viewing window
View(starwars)

# The following displays just the column called "name" in the "starwars" dataset.
# The syntax dataframe$columnname is used to refer to a column called "columnname" in
# the dataframe called "dataframe".
starwars$name

# The length function tells us how many elements are in the vector of names
length(starwars$name)

# This displays just the column called "homeworld" in the dataset - you'll see lots of missing
# data that is indicated by "NA"
starwars$homeworld

# The unique functions gives us just the unique homeworlds
unique(starwars$homeworld)

# The following combines the length and unique functions to return the number of unique homeworlds
length(unique(starwars$homeworld))

# The following displays just the column called "height" in the dataset
# Notice again that missing values are labelled as "NA"
starwars$height

# We can use the filter and is.na functions to filter out "NA" values in the height column
# if we wanted to we could map the output of this onto a new variable (we'll see how to do that in a bit)
filter(starwars, !is.na(height))$height

# Now let's plot a basic histogram using the ggplot function.  ggplot is based on the grammar of graphics
# and involves building up a plot layer by layer. Below we tell ggplot we want to use the starwars
# dataset and set up our aestheitcs such that the x-axis corresponds to the height column. We then
# add the geom_histogram() layer which uses the information from the ggplot() layer to build a histogram.
ggplot(starwars, aes(x=height)) + 
  geom_histogram()

# We can now add another layer involving labels and a title
ggplot(starwars, aes(x=height)) + 
  geom_histogram() +
  labs(x="Height (cm)", y="Count", title="Histogram of Characters' Heights in Star Wars")

# Use the facet_wrap function to plot separately by species - this will take a little time
# You can click on the 'Zoom' button to make the plot bigger. You can resize the zoomed window and 
# also copy or save it (by clickig on Export).
ggplot(starwars, aes(x=height)) + 
  geom_histogram() +
  labs(x="Height (cm)", y="Count", title="Histogram of Characters' Heights in Star Wars") +
  facet_wrap(~species)

# Let's create a new dataframe called starwars_filtered that includes only Humans, Droids, and cases
# where we have both their height and mass recorded
# the assignment operator "<-" maps what's on the right hand side to a new variable we have created
# and named on the left.
starwars_filtered <- filter(starwars, (species=="Human" | species=="Droid") & (!is.na(height) & !is.na(mass)))

# Now if we type the name of our new variable, we can see the data it contains.
starwars_filtered

# Let's plot our filtered data separately for Droids and Humans
ggplot(starwars_filtered, aes(x=height)) + 
  geom_histogram() +
  labs(x="Height (cm)", y="Count", title="Histogram of Characters' Heights in Star Wars\nOnly Droids and Humans") +
  facet_wrap(~species)

# Let's aggregate our filtered dataset working out the means and standard deviations
# for each of our two groups (Humans vs. Droids)
# Here we are using the pipe operator %>% which pipes the result of what is on the left hand side
# to the right. We are piping the starwars_filtered data through to the group_by function
# which is then grouping the data by species.  We are then piping this grouped data through to the
# summarise() function which is calculating the mean and sd deviation for our height and mass variables.
# Finally, we are mapping the output of this onto a new variable we are calling starwars_summary
starwars_summary <- starwars_filtered %>% group_by(species) %>% 
  summarise(mean_height=mean(height), sd_height=sd(height), mean_mass=mean(mass), sd_mass=sd(mass))

starwars_summary

# We can do a basic plot of our means by species based on this new variable starwars_summary - 
# Notice we use the geom_col() function to add a layer to our plot involve plotting a bar graph.
ggplot(starwars_summary, aes(x=species, y=mean_height)) + geom_col()

# Now we can add some error bars using the geom_errorbar function. This tells us about variability
# around our means.
ggplot(starwars_summary, aes(x=species, y=mean_height)) + geom_col() +
  geom_errorbar(aes(ymin=mean_height-sd_height, ymax=mean_height+sd_height, width=.25))

# Now let's add some colour and nicer labels!
ggplot(starwars_summary, aes(x=species, y=mean_height, fill=species)) + geom_col() +
  geom_errorbar(aes(ymin=mean_height-sd_height, ymax=mean_height+sd_height, width=.25)) +
  labs(x="Species", y="Mean Height (cm)", title="Mean Height of Droids vs. Humans in Star Wars \nwith SD bars") +
  guides(fill=FALSE) 

# Is there a difference in Heights between Droids and Humans?
# Let's do a t-test to see...
t.test(starwars_filtered[starwars_filtered$species=="Human",]$height, 
       starwars_filtered[starwars_filtered$species=="Droid",]$height)

# Now can you redo the code from line 73 but this time do everything for the
# mass variable instead of height?

# Next you need to import the dataset called "intro_data"
# Do that by clcking on the Import dataset button in the window on the top right
# These data correspond to the amount of money each team spent on each driver in F1
# And the number of points the driver scored

# We're going to plot three graphs next - the first is a simple scatteplot of investment in the driver
# by the team again the points scored by the driver in a particular F1 season.

ggplot(intro_data, aes(points, investment)) + geom_point() + 
  xlab("Points scored") + ylab("Investment (in £100,000s)") +
  ggtitle ("Plot of Team Investment in an F1 Driver \nvs. Points Scored")

# This next plot adds a regression line - the shaded area is the 95% CI interval - note
# that the interval increases slighly as we move away from the mean - this is because precision drops
# when we move away from the centre of the distribution
ggplot(intro_data, aes(points, investment)) + geom_point() + geom_smooth(method='lm') + 
  xlab("Points scored") + ylab("Investment (in £100,000s)") +
  ggtitle ("Plot of Team Investment in an F1 Driver \nvs. Points Scored with Regression Line")

# Next we're going to plot a heat map - heat maps can be quite a useful way to visualise the 
# relationship between two variables
ggplot(intro_data, aes(points, investment)) +
  stat_bin2d(bins = 8, colour = "black") + scale_fill_viridis() + 
  xlab("Points scored") + ylab("Investment (in £100,000s)") +
  ggtitle ("Density Heat Map of Team Investment in an \nF1 Driver vs. Points Scored")

