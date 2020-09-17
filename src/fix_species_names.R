# function to fidata species names from Gurupi dataset
# Elildo Carvalho Jr @ ICMBio/CENAP, 2020-05-19

f.fix.species.names <- function(data)  {
  data<-data[!(data$bin==" "),]
  levels(data$bin)[levels(data$bin)=="Aramides unknown"] <- "Aramides cajaneus" # rename by name
  data<-data[!(data$bin=="Aramides cajaneus"),]
  data<-data[!(data$bin=="Arremon flavirostris"),]
  data<-data[!(data$bin=="Arremon taciturnus"),]
  data<-data[!(data$bin=="Bos unknown"),]
  data<-data[!(data$bin=="Botaurus pinnatus"),]
  levels(data$bin)[levels(data$bin)=="Cabassous unknown"] <- "Cabassous unicinctus"
  data<-data[!(data$bin=="Caluromys philander"),]
  data<-data[!(data$bin=="Cebus kaapori"),]
  data<-data[!(data$bin=="Cebus unknown"),]
  data<-data[!(data$bin=="Cebus apella"),]
  data<-data[!(data$bin=="Chondrohierax uncinatus"),]
  data<-data[!(data$bin=="Conopophaga roberti"),]
  data<-data[!(data$bin=="Cochlearius cochlearius"),]
  levels(data$bin)[levels(data$bin)=="Crypturellus soui"] <- "Crypturellus spp"
  levels(data$bin)[levels(data$bin)=="Crypturellus sp"] <- "Crypturellus spp"
  levels(data$bin)[levels(data$bin)=="Crypturellus undulatus"] <- "Crypturellus spp"
  levels(data$bin)[levels(data$bin)=="Crypturellus unknown"] <- "Crypturellus spp"
  levels(data$bin)[levels(data$bin)=="Crypturellus variegatus"] <- "Crypturellus spp"
  levels(data$bin)[levels(data$bin)=="Dasyprocta punctata"] <- "Dasyprocta prymnolopha"
  levels(data$bin)[levels(data$bin)=="Dasyprocta unknown"] <- "Dasyprocta prymnolopha"
  levels(data$bin)[levels(data$bin)=="Dasypus kappleri"] <- "Dasypus spp"
  levels(data$bin)[levels(data$bin)=="Dasypus sp"] <- "Dasypus spp"
  levels(data$bin)[levels(data$bin)=="Dasypus unknown"] <- "Dasypus spp"
  data<-data[!(data$bin=="Dendrocincla fuliginosa"),]
  data<-data[!(data$bin=="Dendrocolaptes unknown"),]
  data<-data[!(data$bin=="Dromococcyx pavoninus"),]
  data<-data[!(data$bin=="Eurypyga helias"),]
  data<-data[!(data$bin=="Formicarius analis"),]
  data<-data[!(data$bin=="Formicarius colma"),]
  data<-data[!(data$bin=="Formicarius unknown"),]
  data<-data[!(data$bin=="Galea spixii"),]
  data<-data[!(data$bin=="Geotrygon montana"),]
  data<-data[!(data$bin=="Homo sapiens"),]
  data<-data[!(data$bin=="Hydropsalis unknown"),]
  data<-data[!(data$bin=="Hylopezus unknown"),]
  data<-data[!(data$bin=="Hylopezus unknown"),]
  levels(data$bin)[levels(data$bin)=="Leopardus pardalis"] <- "Leopardus spp"
  levels(data$bin)[levels(data$bin)=="Leopardus tigrinus"] <- "Leopardus spp"
  levels(data$bin)[levels(data$bin)=="Leopardus unknown"] <- "Leopardus spp"
  levels(data$bin)[levels(data$bin)=="Leopardus wiedii"] <- "Leopardus spp"
  data<-data[!(data$bin=="Leptotila rufaxilla"),]
  data<-data[!(data$bin=="Leptotila unknown"),]
  data<-data[!(data$bin=="Leptotila verreauxi"),]
  data<-data[!(data$bin=="Lontra longicaudis"),]
  levels(data$bin)[levels(data$bin)=="Mazama americana"] <- "Mazama spp"
  levels(data$bin)[levels(data$bin)=="Mazama nemorivaga"] <- "Mazama spp"
  levels(data$bin)[levels(data$bin)=="Mazama unknown"] <- "Mazama spp"
  data<-data[!(data$bin=="Metachirus nudicaudatus"),]
  data<-data[!(data$bin=="Micrastur mintoni"),]
  data<-data[!(data$bin=="Momotus momota"),]
  data<-data[!(data$bin=="Monasa morphoeus"),]
  data<-data[!(data$bin=="Monodelphis unknown"),]
  data<-data[!(data$bin=="NA NA"),]
  data<-data[!(data$bin=="Neoctantes niger"),]
  data<-data[!(data$bin=="Neomorphus geoffroyi"),]
  data<-data[!(data$bin=="Neomorphus unknown"),]
  levels(data$bin)[levels(data$bin)=="Odontophorus unknown"] <- "Odontophorus gujanensis"
  levels(data$bin)[levels(data$bin)=="Pauxi unknown"] <- "Mitu tuberosum" 
  levels(data$bin)[levels(data$bin)=="Penelope superciliaris"] <- "Penelope spp"
  levels(data$bin)[levels(data$bin)=="Penelope unknown"] <- "Penelope spp"
  data<-data[!(data$bin=="Philander opossum"),]
  data<-data[!(data$bin=="Philander unknown"),]
  data<-data[!(data$bin=="Phimosus infuscatus"),]
  data<-data[!(data$bin=="Phlegopsis nigromaculata"),]
  levels(data$bin)[levels(data$bin)=="Psophia unknown"] <- "Psophia obscura"
  levels(data$bin)[levels(data$bin)=="Psophia viridis"] <- "Psophia obscura"
  data<-data[!(data$bin=="Pyriglena leuconota"),]
  data<-data[!(data$bin=="Ramphastos vitellinus"),]
  data<-data[!(data$bin=="Ramphastos unknown"),]
  data<-data[!(data$bin=="Saguinus niger"),]
  data<-data[!(data$bin=="Sciurillus unknown"),]
  data<-data[!(data$bin=="Sciurus aestuans"),]
  data<-data[!(data$bin=="Sciurus unknown"),]
  data<-data[!(data$bin=="Tachyphonus luctuosus"),]
  data<-data[!(data$bin=="Tachyphonus unknown"),]
  data<-data[!(data$bin=="Thamnophilus unknown"),]
  levels(data$bin)[levels(data$bin)=="Tinamus guttatus"] <- "Tinamus spp"
  levels(data$bin)[levels(data$bin)=="Tinamus tao"] <- "Tinamus spp" 
  levels(data$bin)[levels(data$bin)=="Tinamus unknown"] <- "Tinamus spp" 
  data<-data[!(data$bin=="Turdus albicollis"),]
  data<-data[!(data$bin==" "),] # remove empty " " rows
  data$bin <- factor(data$bin)
  assign("dataTemp", data, envir=.GlobalEnv)
  }
  
