# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(viridis)

# Load the dataset
mutation_data <- read.delim("MutationRate.txt", header = TRUE)
str(mutation_data)

#Generating first plot using facets. Given two genomes (mito & plastid) it will be useful to visualize mutations rates separately for each. 
#We used the facets grid using facets_grid function to distribute Genotype and Genome in and rows/columns orientation. 
#We separated Mutation Type for each genome, and put mutation rate on the y-axis.

mutation_plot <- ggplot(data = mutation_data, aes(x = MutationType, y = MutationRate)) +
  geom_point(size = 3) + labs(x = "Mutation Type", y = "Mutation Rate") +  
  facet_grid(Genotype ~ Genome) + 
  theme_bw()  

# Print the plot
print(mutation_plot)



#The facet plotting looks good but its hard to tell which genotype has a higher mutation rate. 
#We tried with just the mito genome to start and filtered for substitutions indel type. 

submito = mutation_data %>% filter(MutationType == "Substitutions") %>% filter(Genome == "Mito")
ggplot (data=submito, aes(x=Genotype, y=MutationRate)) + geom_point()+ scale_color_viridis() + scale_y_log10() + 
  xlab("Genotype") + ylab("Mutation Rate")


#From the above plot, it is going to be better to visualize genotype on the x-axis.
#Now, lets get rid of the mito filtering so we can see both genomes, add the facets back in, and color based on genome.  

subs = mutation_data %>% filter(MutationType == "Substitutions")
ggplot (data=subs, aes(x=Genotype, y=MutationRate, color = Genome)) + geom_point() + 
  scale_y_log10() + xlab("Genotype") + ylab("Mutation Rate") + facet_grid (Genotype ~ Genome)

#That looks pretty good, but we somehow have the genotype on the x-axis and in the rows. 
#Lets go back to our original mutation_data and use the aesthetics function to incorporate both indels and substitutions. 

ggplot (data=mutation_data, aes(x=Genotype, y=MutationRate, color = Genome)) + geom_point() + 
  scale_y_log10() + xlab("Genotype") + ylab("Mutation Rate") + facet_grid (Genotype ~ Genome)

#Now lets see if we can add shape to the mutation type and use the facet function to distribute the indel type into rows and columns. 

ggplot (data=mutation_data, aes(x=Genotype, y=MutationRate, color = Genome, shape = MutationType)) + 
  geom_jitter() + scale_y_log10() + xlab("Genotype") + ylab("Mutation Rate") 

#Uh-oh! We have the faceted graph with indels and substitions and genotype on bottom but we lost our points! Lets add geom_jitter. 

ggplot (data=mutation_data, aes(x=Genotype, y=MutationRate, color = Genome, shape = MutationType)) + 
  geom_jitter() + scale_y_log10() + xlab("Genotype") + ylab("Mutation Rate") + 
  facet_grid (rows = vars(mutation_data$MutationType), cols = vars(mutation_data$Genome))

#That is better!




