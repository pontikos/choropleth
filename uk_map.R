# Load necessary libraries
library(sf)
library(ggplot2)

# Read the shapefile
#uk <- st_read('~/Downloads/codepo_gpkg_gb/Data/codepo_gb.gpkg')

PATHS2 <- read.csv('paths.txt')
PATHS <- list(PATHS2[,2])
names(PATHS) <- PATHS2[,1]

uk <- st_read(PATHS[['postal_sectors_shp']])
uk$postcode <- uk$RefPC

uk_simple <- st_simplify(uk, dTolerance = 30)
uk_simple$postcode <- substr(uk_simple$RefPC,1,4)


df <- read(PATHS[['patients_csv']])
#df <- df[which(df$gene_id!='NULL'),]
#df <- as.data.frame(t(t(table(df$PostCode,df$gene_id!='NULL'))))
X <- table(df$PostCode,df$gene_id!='NULL')
X <- (t(apply(X,1,function(x) { x/sum(x) })))
df <- data.frame(postcode=rownames(X),solve_rate=round(100*X[,2],2))
#df <- df[,c(1,3)]
#colnames(df) <- c('postcode','count')
df$postcode <- gsub(' ', '', df$postcode)
df$postcode <- substr(df$postcode,1,4)
#df <- do.call('rbind', by(df,df$postcode,function(x){ return(list(postcode=unique(x$postcode),count=sum(x$count)))}))
#df <- data.frame(postcode=as.character(df[,1]),count=as.numeric(df[,2]))
#df <- df[order(df$count,decreasing=T),]
df <- do.call('rbind', by(df,df$postcode,function(x){ return(list(postcode=unique(x$postcode),avg_solve_rate=mean(x$solve_rate)))}))
df <- data.frame(postcode=as.character(df[,1]),avg_solve_rate=as.numeric(df[,2]))
df <- df[order(df$avg_solve_rate,decreasing=T),]

length(intersect(uk_simple$postcode,df$postcode))

# Assuming you have a data frame (df) with postcodes and a variable (var) to map
# Merge data with shapefile
uk_data <- merge(uk_simple, df, by = "postcode", all.x=TRUE)

# Plot
ggplot() +
  geom_sf(data = uk_data, aes(fill = avg_solve_rate),lwd=0) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "solve_rate",
       title = "Diagnostic yield of IRDs per postcode",
       caption = "")

solve_rate <- na.omit(unique(as.data.frame(uk_data)[,c('postcode','Sprawl','avg_solve_rate')]))
write.csv( head(solve_rate[ order(solve_rate$avg_solve_rate),]), file='', row.names=F)
write.csv( tail(solve_rate[ order(solve_rate$avg_solve_rate),]), file='', row.names=F)
head(solve_rate)


# Plot
ggplot() +
  geom_sf(data = uk_data[which(uk_data$Sprawl=='London'),], aes(fill = avg_solve_rate),lwd=0) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "solve_rate",
       title = "Diagnostic yield of IRDs per postcode (London)",
       caption = "")
solve_rate <- na.omit(unique(as.data.frame(uk_data[which(uk_data$Sprawl=='London'),])[,c('postcode','Locale','avg_solve_rate')]))
write.csv( head(solve_rate[ order(solve_rate$avg_solve_rate),]), file='', row.names=F)
write.csv( tail(solve_rate[ order(solve_rate$avg_solve_rate),]), file='', row.names=F)


# Plot
ggplot() +
  geom_sf(data = uk_data[which(uk_data$Sprawl=='Birmingham'),], aes(fill = avg_solve_rate),lwd=0) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "solve_rate",
       title = "Diagnostic yield of IRDs per postcode (Birmingham)",
       caption = "")
solve_rate <- na.omit(unique(as.data.frame(uk_data[which(uk_data$Sprawl=='Birmingham'),])[,c('postcode','Locale','avg_solve_rate')]))
write.csv( head(solve_rate[ order(solve_rate$avg_solve_rate),]), file='', row.names=F)
write.csv( tail(solve_rate[ order(solve_rate$avg_solve_rate),]), file='', row.names=F)




xxx <- unique(as.data.frame(uk_data[which(uk_data$Sprawl=='London'),])[,c('postcode','Locale','avg_solve_rate')])

xxx[order(xxx$avg_solve_rate),]




ggplot() +
  geom_sf(data = uk_data[which(uk_data$Sprawl=='London'),], aes(fill=count),lwd=0) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "counts",
       title = "Choropleth map of MEH IRD postcodes",
       caption = "")



ggplot() +
  geom_sf(data = uk, aes(fill = PCCnt),lwd=0) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "counts",
       title = "Choropleth map of PCCnt",
       caption = "")



library(tmap)

tmap_mode("plot")

# Create the map
tm_shape(uk_data) +
  tm_polygons("var", palette = "-viridis", title = "Variable") +
  tm_layout(main.title = "Choropleth map of UK postcodes",
            legend.title = "Variable",
            legend.position = c("right", "bottom"))

