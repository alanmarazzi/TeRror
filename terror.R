setwd("~/R")
gtd <- read.csv2("~/R/gtd.csv")
str(gtd)

# Subset unsuccesful ones to not count terrorists as victims
gtd <- gtd[gtd$success != 0, ]

# Let's draw a world map of the attacks by number of victims, note that is not complete because of changing names of countries
library(dplyr)
library(rgdal)
library(ggplot2)
library(maptools)
library(ggthemes)
library(viridis)
library(Cairo)
library(reshape2)
library(cowplot)
library(yarrr)

# Rename countries as in map data
gtd$country_txt <- as.character(gtd$country_txt)
gtd$country_txt[gtd$country_txt == "United States"] <- "USA"
gtd$country_txt[gtd$country_txt == "People's Republic of the Congo"] <- "Republic of Congo"
gtd$country_txt[gtd$country_txt == "Republic of the Congo"] <- "Republic of Congo"
gtd$country_txt[gtd$country_txt == "Zaire"] <- "Democratic Republic of the Congo"
gtd$country_txt[gtd$country_txt == "United Kingdom"] <- "UK"
gtd$country_txt[gtd$country_txt == "East Germany (GDR)"] <- "Germany"
gtd$country_txt[gtd$country_txt == "West Germany (FRG)"] <- "Germany"
gtd$country_txt[gtd$country_txt == "Slovak Republic"] <- "Slovakia"
gtd$country_txt[gtd$country_txt == "Czechoslovakia"] <- "Czech Republic" # Most of the kills in only one attack in Czech Republic
gtd$country_txt[gtd$country_txt == "Soviet Union"] <- "Russia" # For simplicity, just 96 victims

world <- map_data("world")

worldKill <- gtd %>% group_by(country_txt) %>% summarize(kill = sum(nkill, na.rm = TRUE))
mongolia <- c("Mongolia", 0)
worldKill <- rbind(worldKill, mongolia)
worldKill$kill <- as.numeric(worldKill$kill)
worldKill$kill[worldKill$kill > 20000] <- 20000

CairoPNG("World Terror Killed.png", w = 10, h = 7, u = "in", res = 900)
ggplot(worldKill, aes(map_id = country_txt))+
    geom_map(aes(fill = kill), map = world, color = "white", size = .2)+
    expand_limits(x = world$long, y = world$lat)+
    scale_fill_viridis(limits = c(0,20000), "Number of people killed by terrorist attacks", end = .8, begin = .15)+
    theme_map()+
    theme(
        axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.ticks = element_blank()
    )+
    guides(fill = guide_colorbar(barwidth = 20, barheight = .5, title.position = "top"))
dev.off()

# Subset only European countries to make single plots
europeKill <- subset(gtd, region %in% c(8,9))
europeKill <- europeKill %>% group_by(country_txt) %>% summarize(kill = sum(nkill, na.rm = TRUE))


CairoPNG("Europe Killed.png", w = 9, h = 7, u = "in", res = 900)
ggplot(europeKill, aes(map_id = country_txt))+
    geom_map(aes(fill = kill), map = world, color = "white", size = .2)+
    expand_limits(x = c(-20, 40), y = c(37,68))+
    scale_fill_viridis("Number of people killed by terrorist attacks", begin = .15, end = .8)+
    theme_map()+
    theme(
        axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.ticks = element_blank()
    )+
    guides(fill = guide_colorbar(barwidth = 20, barheight = .5, title.position = "top"))
dev.off()

europeSet <- subset(gtd, region %in% c(8,9))
europeTile <- europeSet %>% group_by(iyear, country_txt) %>% summarize(kill = sum(nkill, na.rm = T))
europe <- c(unique(europeTile$country_txt))
years <- paste0("y", seq(1970,2014, by = 1))
europeDummy <- data.frame(matrix(NA, nrow = 44, ncol = 45))
names(europeDummy) <- years
europeDummy[,] <- europe
europeDummy$kills <- rep(0,44)
europeDummy <- melt(europeDummy, id = "kills")
europeDummy$variable <- as.integer(gsub("y", "", europeDummy$variable))
europeDummy <- merge(europeDummy, europeTile, by.x = c("variable", "value"), by.y = c("iyear", "country_txt"), all.x = TRUE)
europeDummy$kill[is.na(europeDummy$kill)] <- 0

# Subset data for a better looking chart
europeGrouped <- europeTile %>% group_by(country_txt) %>% summarize(kill = sum(kill, na.rm = T)) %>% arrange(desc(kill))

# Build index for chart subsetting
europeGrouped <- europeGrouped[1:13,]
index <- europeDummy$value %in% europeGrouped$country_txt 
europeDummy <- europeDummy[index, ]

# Check which is the max event and other deadly ones
europeSet <- arrange(europeSet, desc(nkill))

# Other transformations to make the chart look better
europeDummy$kill[europeDummy$kill > 50] <- 50
europeDummy$value <- factor(europeDummy$value)
europeDummy$value <- factor(europeDummy$value, rev(levels(europeDummy$value)))

theme_tile <- theme_tufte(base_size = 9, base_family = "sans")+
    theme(
        axis.title = element_text(color = NA),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0, vjust = 0),
        axis.ticks = element_line(color = NA)
    )

euTile <- ggplot(europeDummy, aes(variable, value, fill = kill))+
    geom_tile(color = "white", size = .1)+
    scale_fill_viridis(begin = .15, end = .8, "Number of people killed")+
    coord_cartesian(expand = c(0,0))+
    coord_equal()+
    guides(fill = guide_colorbar(barwidth = 20, barheight = .5, title.position = "top"))+
    ggtitle("Top European countries for terrorism deaths")+
    scale_x_continuous(expand = c(0,0), breaks = c(seq(1970, 2010, by = 5), 2014))+
    scale_y_discrete(expand = c(0,0))

#CairoPNG("EuropeTile.png", u = "in", w = 7, h = 4, res = 800)
euTileMult <- ggdraw(switch_axis_position(euTile + theme_tile + theme(legend.position = "none"), axis = "x"))
#dev.off()

# Check single events
gtd[gtd$country_txt == "Croatia" & gtd$iyear == 1991, c("imonth", "city", "crit1", "crit2", "crit3", "attacktype1_txt", "targtype1_txt", "gname", "weaptype1_txt", "nkill", "nwound")]

# Italy
gtd[gtd$country_txt == "Italy" & gtd$iyear == 1974, c("imonth", "city", "crit1", "crit2", "crit3", "attacktype1_txt", "targtype1_txt", "gname", "weaptype1_txt", "nkill", "nwound")]
gtd[gtd$country_txt == "Italy" & gtd$iyear == 1980,c("imonth", "city", "crit1", "crit2", "crit3", "attacktype1_txt", "targtype1_txt", "gname", "weaptype1_txt", "nkill", "nwound")]


# Tile with just suicide attacks
europeSuic <- europeSet %>% filter(suicide == 1)
europeSuic <- europeSuic %>% group_by(iyear, country_txt) %>% summarize(kill = sum(nkill, na.rm = T))
europeDS <- data.frame(matrix(NA, nrow = 44, ncol = 45))
names(europeDS) <- years
europeDS[,] <- europe
europeDS$kills <- rep(0,44)
europeDS <- melt(europeDS, id = "kills")
europeDS$variable <- as.integer(gsub("y", "", europeDS$variable))
europeDS <- merge(europeDS, europeSuic, by.x = c("variable", "value"), by.y = c("iyear", "country_txt"), all.x = TRUE)
europeDS$kill[is.na(europeDS$kill)] <- 0
europeGS <- europeSuic %>% group_by(country_txt) %>% summarize(kill = sum(kill, na.rm = T)) %>% arrange(desc(kill))
europeGS <- europeGS[1:5, ]
index <- europeDS$value %in% europeGS$country_txt
europeDS <- europeDS[index, ]
europeDS$kill[europeDS$kill > 50] <- 50
europeDS$value <- factor(europeDS$value)
europeDS$value <- factor(europeDS$value, rev(levels(europeDS$value)))

euTileSuicide <- ggplot(europeDS, aes(variable, value, fill = kill))+
    geom_tile(color = "white", size = .1)+
    scale_fill_viridis(begin = .15, end = .8, "Number of people killed")+
    coord_cartesian(expand = c(0,0))+
    coord_equal()+
    guides(fill = guide_colorbar(barwidth = 20, barheight = .5, title.position = "top"))+
    ggtitle("Only European countries with deaths caused by suicide attacks")+
    scale_x_continuous(expand = c(0,0), breaks = c(seq(1970, 2010, by = 5), 2014))+
    scale_y_discrete(expand = c(0,0))

#CairoPNG("EuropeTileSuicide.png", u = "in", w = 7, h = 4, res = 800)
euTileSuicMult <- ggdraw(switch_axis_position(euTileSuicide + theme_tile, axis = "x"))
#dev.off()


# Tile with explosive attacks
europeExpl <- europeSet %>% filter(attacktype1 == 3)
europeExpl <- europeExpl %>% group_by(iyear, country_txt) %>% summarize(kill = sum(nkill, na.rm = T))
europeDS <- data.frame(matrix(NA, nrow = 44, ncol = 45))
names(europeDS) <- years
europeDS[,] <- europe
europeDS$kills <- rep(0,44)
europeDS <- melt(europeDS, id = "kills")
europeDS$variable <- as.integer(gsub("y", "", europeDS$variable))
europeDS <- merge(europeDS, europeExpl, by.x = c("variable", "value"), by.y = c("iyear", "country_txt"), all.x = TRUE)
europeDS$kill[is.na(europeDS$kill)] <- 0
europeGS <- europeExpl %>% group_by(country_txt) %>% summarize(kill = sum(kill, na.rm = T)) %>% arrange(desc(kill))
europeGS <- europeGS[1:13, ]
index <- europeDS$value %in% europeGS$country_txt
europeDS <- europeDS[index, ]
europeDS$kill[europeDS$kill > 50] <- 50
europeDS$value <- factor(europeDS$value)
europeDS$value <- factor(europeDS$value, rev(levels(europeDS$value)))

euTileExpl <- ggplot(europeDS, aes(variable, value, fill = kill))+
    geom_tile(color = "white", size = .1)+
    scale_fill_viridis(begin = .15, end = .8, "Number of people killed")+
    coord_cartesian(expand = c(0,0))+
    coord_equal()+
    guides(fill = guide_colorbar(barwidth = 20, barheight = .5, title.position = "top"))+
    ggtitle("Top European countries with deaths caused by explosive attacks")+
    scale_x_continuous(expand = c(0,0), breaks = c(seq(1970, 2010, by = 5), 2014))+
    scale_y_discrete(expand = c(0,0))

#CairoPNG("EuropeTileExplosion.png", u = "in", w = 7, h = 4, res = 800)
euTileExplMult <- ggdraw(switch_axis_position(euTileExpl + theme_tile+ theme(legend.position = "none"), axis = "x"))
# dev.off()


# Tile with international attacks
europeInt <- europeSet %>% filter(INT_ANY == 1)
europeInt <- europeInt %>% group_by(iyear, country_txt) %>% summarize(kill = sum(nkill, na.rm = T))
europeDS <- data.frame(matrix(NA, nrow = 44, ncol = 45))
names(europeDS) <- years
europeDS[,] <- europe
europeDS$kills <- rep(0,44)
europeDS <- melt(europeDS, id = "kills")
europeDS$variable <- as.integer(gsub("y", "", europeDS$variable))
europeDS <- merge(europeDS, europeInt, by.x = c("variable", "value"), by.y = c("iyear", "country_txt"), all.x = TRUE)
europeDS$kill[is.na(europeDS$kill)] <- 0
europeGS <- europeInt %>% group_by(country_txt) %>% summarize(kill = sum(kill, na.rm = T)) %>% arrange(desc(kill))
europeGS <- europeGS[1:13, ]
index <- europeDS$value %in% europeGS$country_txt
europeDS <- europeDS[index, ]
europeDS$kill[europeDS$kill > 50] <- 50
europeDS$value <- factor(europeDS$value)
europeDS$value <- factor(europeDS$value, rev(levels(europeDS$value)))

euTileInt <- ggplot(europeDS, aes(variable, value, fill = kill))+
    geom_tile(color = "white", size = .1)+
    scale_fill_viridis(begin = .15, end = .8, "Number of people killed")+
    coord_cartesian(expand = c(0,0))+
    coord_equal()+
    guides(fill = guide_colorbar(barwidth = 20, barheight = .5, title.position = "top"))+
    ggtitle("Top European countries with deaths caused by international attacks")+
    scale_x_continuous(expand = c(0,0), breaks = c(seq(1970, 2010, by = 5), 2014))+
    scale_y_discrete(expand = c(0,0))

#CairoPNG("EuropeTileInternational.png", u = "in", w = 7, h = 4, res = 800)
euTileIntMult <- ggdraw(switch_axis_position(euTileInt + theme_tile + theme(legend.position = "none"), axis = "x"))
#dev.off()

# Multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}


###### Not allineated
CairoPNG("MultiTileEurope.png", u = "in", w = 6, h = 8, res = 800)
multiplot(euTileMult, euTileExplMult, euTileIntMult, cols = 1)
dev.off()

# RDI plot of kills by suicide variable
palD <- viridis_pal(begin = .15, end = .8)(2)
pirateSuicide <- gtd[gtd$nkill != 0 & gtd$region %in% c(8, 9), ]
pirateSuicide$suicide <- factor(pirateSuicide$suicide, labels = c("Regular attack", "Suicide attack"))

CairoPNG("pirateSuicide.png", u = "in", w = 7, h = 6, res = 800)
pirateplot(nkill ~ suicide, pirateSuicide, line.fun = median, ylim = c(0,100), pal = palD, bar.o = .8, point.o = .7, xlab = NA, ylab = "Number of deaths", jitter.val = .08)
dev.off()

ggplot(pirateSuicide, aes(suicide, nkill, fill = suicide))+
    geom_bar(stat = "identity", alpha = .6)+
    scale_fill_manual(values = palD)+
    ggtitle("Total number of deaths by type of attack")+
    theme_tufte(base_size = 9, base_family = "sans")+
    theme(
        axis.title.x = element_text(color = NA),
        legend.position = "none",
        axis.ticks = element_line(color = NA)
    )
