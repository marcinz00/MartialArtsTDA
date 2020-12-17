#library("ggplot2", lib.loc="~/R/win-library/3.2")
library("ggplot2")
library(reshape2)
#library(dtw)
library("stringr")

if (!require("TDA"))
{
  install.packages("TDA") 
}
library("TDA")

if (!require("RMoCap"))
{
  if (!require("devtools"))
  {
    install.packages("devtools") # if you have not installed "devtools" package
  }
  devtools::install_github("browarsoftware/RMoCap")
}
library("RMoCap")

##########

KATALOG_ROBOCZY <- "C:/MARTIAL_ARTS/Data/1/_REC"
KATALOG_TRENINGOWY <- "C:/MARTIAL_ARTS/Data/1/_TRAIN"
#KATALOG_ROBOCZY <- "C:/MARTIAL_ARTS/Data/2/_REC"
#KATALOG_TRENINGOWY <- "C:/MARTIAL_ARTS/Data/2/_TRAIN"
#KATALOG_ROBOCZY <- "C:/MARTIAL_ARTS/Data/3/_REC"
#KATALOG_TRENINGOWY <- "C:/MARTIAL_ARTS/Data/3/_TRAIN"


setwd(KATALOG_ROBOCZY)


TIME <- 1
TIME_ZOOM <- 0.25

TDA_ON <- 1 #1 # 0
TDA_VERIF_SIZE <- 5 # @#@ #5
TDA_VERIF_MARGIN <- 30 # @#@ #10
TDA_ERR_MOV_SET <- c("empi_right_")

TDA_ALL_DIMENSIONS <- 1
SAMPLING <-1 #20 #5 #10 # 20

TDA_MAXSCALE <- 50 #20 #50 # 30
TDA_MAXDIM <- 0 #0 #1
TDA_DIFF_DIM <- 0
TDA_DIFF_STEP <- 5

TDA_DIST_HIPS0_HANDS <- 1 #0



#TDA_library <- "PHAT"
#TDA_library <- "GUDHI"
TDA_library <- "Dionysus"


EMPI_LEFT_IDX <- -1



DEBUG <- TRUE         # if TRUE, write additional info
N_OF_STDDEV <- 3

# setting up main objects

# =========== FUNCTIONS =============


readFingerprint <- function(file) {
  
  #d <- read.table(file, sep=",", header = TRUE)
  d <- read.mocap(file)
  d <- d$data.frame
  d[["Time"]] <- 1:length(d[["Time"]])
  file_name <- paste(file, ".csv", sep="")
  write.table(d, file = file_name,
              col.names=TRUE, row.names=FALSE, sep=";", quote=FALSE, na="", dec=".")
  
  #d <- data.table(d)
  return(d)
}

readSignal <- function(file, columnName) {
  d <- read.table(file, sep=",", header = TRUE)
  return(d[,as.character(columnName)])
}


createPointCloud <- function(fileData, TIME, SAMPLING = 1) 
{
  si <- seq(1, length(fileData[,"Time"]), SAMPLING)

  time0 <- fileData[,"Time"][1]
  time <- fileData[,"Time"][si]
  hips0_Dx <- fileData[,"Hips.Dx"][1]
  hips0_Dy <- fileData[,"Hips.Dy"][1]
  hips0_Dz <- fileData[,"Hips.Dz"][1]
  #hips0_Dx <- fileData[,"SpineLow.Dx"][1]
  #hips0_Dy <- fileData[,"SpineLow.Dy"][1]
  #hips0_Dz <- fileData[,"SpineLow.Dz"][1]
  
  #nvect = c(hips0_Dx, hips0_Dy, hips0_Dz)
  nvect = c(hips0_Dx, hips0_Dy, hips0_Dz)
  #cat("Nvect: ")
  #print(nvect)
  
  head0_Dx <- fileData[,"Head.Dx"][1]
  head0_Dy <- fileData[,"Head.Dy"][1]
  head0_Dz <- fileData[,"Head.Dz"][1]
  
    
  Hips <- cbind(fileData[,"Hips.Dx"][si], fileData[,"Hips.Dy"][si], fileData[,"Hips.Dz"][si])
  
  LeftThigh <- cbind(fileData[,"LeftThigh.Dx"][si], fileData[,"LeftThigh.Dy"][si], fileData[,"LeftThigh.Dz"][si])
  LeftLeg <- cbind(fileData[,"LeftLeg.Dx"][si], fileData[,"LeftLeg.Dy"][si], fileData[,"LeftLeg.Dz"][si])
  LeftFoot <- cbind(fileData[,"LeftFoot.Dx"][si], fileData[,"LeftFoot.Dy"][si], fileData[,"LeftFoot.Dz"][si])
  #EndSite4 <- cbind(fileData[,"EndSite4.Dx"][si], fileData[,"EndSite4.Dy"][si], fileData[,"EndSite4.Dz"][si])
  
  RightThigh <- cbind(fileData[,"RightThigh.Dx"][si], fileData[,"RightThigh.Dy"][si], fileData[,"RightThigh.Dz"][si])
  RightLeg <- cbind(fileData[,"RightLeg.Dx"][si], fileData[,"RightLeg.Dy"][si], fileData[,"RightLeg.Dz"][si])
  RightFoot <- cbind(fileData[,"RightFoot.Dx"][si], fileData[,"RightFoot.Dy"][si], fileData[,"RightFoot.Dz"][si])
  #EndSite8 <- cbind(fileData[,"EndSite8.Dx"][si], fileData[,"EndSite8.Dy"][si], fileData[,"EndSite8.Dz"][si])
  
  SpineLow <- cbind(fileData[,"SpineLow.Dx"][si], fileData[,"SpineLow.Dy"][si], fileData[,"SpineLow.Dz"][si])
  SpineMid <- cbind(fileData[,"SpineMid.Dx"][si], fileData[,"SpineMid.Dy"][si], fileData[,"SpineMid.Dz"][si])
  
  Chest <- cbind(fileData[,"Chest.Dx"][si], fileData[,"Chest.Dy"][si], fileData[,"Chest.Dz"][si])
  
  LeftShoulder <- cbind(fileData[,"LeftShoulder.Dx"][si], fileData[,"LeftShoulder.Dy"][si], fileData[,"LeftShoulder.Dz"][si])
  LeftArm <- cbind(fileData[,"LeftArm.Dx"][si], fileData[,"LeftArm.Dy"][si], fileData[,"LeftArm.Dz"][si])
  LeftForearm <- cbind(fileData[,"LeftForearm.Dx"][si], fileData[,"LeftForearm.Dy"][si], fileData[,"LeftForearm.Dz"][si])
  LeftHand <- cbind(fileData[,"LeftHand.Dx"][si], fileData[,"LeftHand.Dy"][si], fileData[,"LeftHand.Dz"][si])
  #EndSite16 <- cbind(fileData[,"EndSite16.Dx"][si], fileData[,"EndSite16.Dy"][si], fileData[,"EndSite16.Dz"][si])
  
  RightShoulder <- cbind(fileData[,"RightShoulder.Dx"][si], fileData[,"RightShoulder.Dy"][si], fileData[,"RightShoulder.Dz"][si])
  RightArm <- cbind(fileData[,"RightArm.Dx"][si], fileData[,"RightArm.Dy"][si], fileData[,"RightArm.Dz"][si])
  RightForearm <- cbind(fileData[,"RightForearm.Dx"][si], fileData[,"RightForearm.Dy"][si], fileData[,"RightForearm.Dz"][si])
  RightHand <- cbind(fileData[,"RightHand.Dx"][si], fileData[,"RightHand.Dy"][si], fileData[,"RightHand.Dz"][si])
  EndSite21 <- cbind(fileData[,"EndSite21.Dx"][si], fileData[,"EndSite21.Dy"][si], fileData[,"EndSite21.Dz"][si])
  
  Neck <- cbind(fileData[,"Neck.Dx"][si], fileData[,"Neck.Dy"][si], fileData[,"Neck.Dz"][si])
  
  Head <- cbind(fileData[,"Head.Dx"][si], fileData[,"Head.Dy"][si], fileData[,"Head.Dz"][si])
  
  #EndSite24 <- cbind(fileData[,"EndSite24.Dx"][si], fileData[,"EndSite24.Dy"][si], fileData[,"EndSite24.Dz"][si])

  
  tmp <- c()
  

  if( TDA_ALL_DIMENSIONS == 0 )
  {
    if( TDA_DIFF_DIM > 0 ) {
      
      sb <- seq(from = 0, by = TDA_DIFF_STEP, length.out = length(fileData[,"Time"]))
      
      Hips <- cbind(sb, Hips)
      LeftThigh <- cbind(sb, LeftThigh)
      LeftLeg <- cbind(sb, LeftLeg)
      LeftFoot <- cbind(sb, LeftFoot)
      #EndSite4 <- cbind(sb, EndSite4)
      RightThigh <- cbind(sb, RightThigh)
      RightLeg <- cbind(sb, RightLeg)
      RightFoot <- cbind(sb, RightFoot)
      #EndSite8 <- cbind(sb, EndSite8)
      SpineLow <- cbind(sb, SpineLow)
      SpineMid <- cbind(sb, SpineMid)
      Chest <- cbind(sb, Chest)
      LeftShoulder <- cbind(sb, LeftShoulder)
      LeftArm <- cbind(sb, LeftArm)
      LeftForearm <- cbind(sb, LeftForearm)
      LeftHand <- cbind(sb, LeftHand)
      #EndSite16 <- cbind(sb, EndSite16)
      RightShoulder <- cbind(sb, RightShoulder)
      RightArm <- cbind(sb, RightArm)
      RightForearm <- cbind(sb, RightForearm)
      RightHand <- cbind(sb, RightHand)
      EndSite21 <- cbind(sb, EndSite21)
      Neck <- cbind(sb, Neck)
      Head <- cbind(sb, Head)
      #EndSite24 <- cbind(sb, EndSite24)
    }
    
    
      
    if( TIME > 0 ) {
      #nvect = c(time0, nvect[1], nvect[2], nvect[3])
                    
      Hips <- cbind(time, Hips)
      LeftThigh <- cbind(time, LeftThigh)
      LeftLeg <- cbind(time, LeftLeg)
      LeftFoot <- cbind(time, LeftFoot)
      #EndSite4 <- cbind(time, EndSite4)
      RightThigh <- cbind(time, RightThigh)
      RightLeg <- cbind(time, RightLeg)
      RightFoot <- cbind(time, RightFoot)
      #EndSite8 <- cbind(time, EndSite8)
      SpineLow <- cbind(time, SpineLow)
      SpineMid <- cbind(time, SpineMid)
      Chest <- cbind(time, Chest)
      LeftShoulder <- cbind(time, LeftShoulder)
      LeftArm <- cbind(time, LeftArm)
      LeftForearm <- cbind(time, LeftForearm)
      LeftHand <- cbind(time, LeftHand)
      #EndSite16 <- cbind(time, EndSite16)
      RightShoulder <- cbind(time, RightShoulder)
      RightArm <- cbind(time, RightArm)
      RightForearm <- cbind(time, RightForearm)
      RightHand <- cbind(time, RightHand)
      EndSite21 <- cbind(time, EndSite21)
      Neck <- cbind(time, Neck)
      Head <- cbind(time, Head)
      #EndSite24 <- cbind(time, EndSite24)
    }
  

    tmp <- rbind(tmp, Hips)
    tmp <- rbind(tmp, LeftThigh)
    tmp <- rbind(tmp, LeftLeg)
    tmp <- rbind(tmp, LeftFoot)
    #tmp <- rbind(tmp, EndSite4)
    tmp <- rbind(tmp, RightThigh)
    tmp <- rbind(tmp, RightLeg)
    tmp <- rbind(tmp, RightFoot)
    #tmp <- rbind(tmp, EndSite8)
    tmp <- rbind(tmp, SpineLow)
    tmp <- rbind(tmp, SpineMid)
    tmp <- rbind(tmp, Chest)
    tmp <- rbind(tmp, LeftShoulder)
    tmp <- rbind(tmp, LeftArm)
    tmp <- rbind(tmp, LeftForearm)
    tmp <- rbind(tmp, LeftHand)
    #tmp <- rbind(tmp, EndSite16)
    tmp <- rbind(tmp, RightShoulder)
    tmp <- rbind(tmp, RightArm)
    tmp <- rbind(tmp, RightForearm)
    tmp <- rbind(tmp, RightHand)
    tmp <- rbind(tmp, EndSite21)
    tmp <- rbind(tmp, Neck)
    tmp <- rbind(tmp, Head)
    #tmp <- rbind(tmp, EndSite24)
    
      
    #print(tmp)
    #print(nvect)
    
    
    minVal <- c(min(tmp[,1]), min(tmp[,2]), min(tmp[,3]))
    maxVal <- c(max(tmp[,1]), max(tmp[,2]), max(tmp[,3]))
    #cat("Min: ")
    #print(minVal)
    #cat("Max: ")
    #print(maxVal)
    
  }
  else 
  {
    tmp <- cbind(Hips, 
                  LeftThigh, 
                  LeftLeg, 
                  LeftFoot, 
                  #EndSite4, 
                  RightThigh, 
                  RightLeg,
                  RightFoot, 
                  #EndSite8, 
                  SpineLow, 
                  SpineMid, 
                  Chest, 
                  LeftShoulder, 
                  LeftArm, 
                  LeftForearm,
                  LeftHand, 
                  #EndSite16, 
                  RightShoulder, 
                  RightArm, 
                  RightForearm, 
                  RightHand, 
                  EndSite21,
                  Neck, 
                  Head 
                  #EndSite24
                 )
    
    if( TIME > 0 )
      tmp <- cbind(time, tmp)
    
    if( TDA_DIST_HIPS0_HANDS > 0 )
    {
      x1_x <- seq(from=hips0_Dx,to=hips0_Dx,length.out=length(tmp[,1]))
      x1_y <- seq(from=hips0_Dy,to=hips0_Dy,length.out=length(tmp[,1]))
      x1_z <- seq(from=hips0_Dz,to=hips0_Dz,length.out=length(tmp[,1]))
      x1 <- cbind(x1_x, x1_y, x1_z)
      #print(x1)
      
      x2 <- LeftHand
      dist <- euc_dist_vect(x1, x2)
      tmp <- cbind(tmp, dist)
      
      x2 <- RightHand
      dist <- euc_dist_vect(x1, x2)
      tmp <- cbind(tmp, dist)
      
      x2 <- LeftHand
      dist <- x2 - x1
      colnames(dist) <- c("lhand_x", "lhand_y", "lhand_z")
      tmp <- cbind(tmp, dist)

      x2 <- RightHand
      dist <- x2 - x1
      colnames(dist) <- c("rhand_x", "rhand_y", "rhand_z")
      tmp <- cbind(tmp, dist)
      
  
      x2 <- LeftArm
      dist <- euc_dist_vect(x1, x2)
      tmp <- cbind(tmp, dist)
      
      x2 <- RightArm
      dist <- euc_dist_vect(x1, x2)
      tmp <- cbind(tmp, dist)
      
      x2 <- LeftForearm
      dist <- euc_dist_vect(x1, x2)
      tmp <- cbind(tmp, dist)
      
      x2 <- RightForearm
      dist <- euc_dist_vect(x1, x2)
      tmp <- cbind(tmp, dist)
    

      x2 <- LeftArm
      dist <- x2 - x1
      colnames(dist) <- c("larm_x", "larm_y", "larm_z")
      tmp <- cbind(tmp, dist)
      
      x2 <- RightArm
      dist <- x2 - x1
      colnames(dist) <- c("rarm_x", "rarm_y", "rarm_z")
      tmp <- cbind(tmp, dist)
      
      x2 <- LeftForearm
      dist <- x2 - x1
      colnames(dist) <- c("lforearm_x", "lforearm_y", "lforearm_z")
      tmp <- cbind(tmp, dist)
      
      x2 <- RightForearm
      dist <- x2 - x1
      colnames(dist) <- c("rforearm_x", "rforearm_y", "rforearm_z")
      tmp <- cbind(tmp, dist)

      
      
      x2 <- LeftFoot
      dist <- euc_dist_vect(x1, x2)
      tmp <- cbind(tmp, dist)
      
      x2 <- RightFoot
      dist <- euc_dist_vect(x1, x2)
      tmp <- cbind(tmp, dist)
      
      x2 <- LeftFoot
      dist <- x2 - x1
      colnames(dist) <- c("lfoot_x", "lfoot_y", "lfoot_z")
      tmp <- cbind(tmp, dist)
      
      x2 <- RightFoot
      dist <- x2 - x1
      colnames(dist) <- c("rfoot_x", "rfoot_y", "rfoot_z")
      tmp <- cbind(tmp, dist)
    }
    
  }
  
  tmp[,1] <- tmp[,1] * TIME_ZOOM   
  
  return(tmp)
  
}

euc_dist_vect <- function(x1, x2)
{
  t1 <- (x1 - x2) ^ 2
  t2 <- sqrt( t1[,1] + t1[,2] + t1[,3] )
  return( t2 )
}

euc_dist <- function(x1, x2)
{
  sqrt(sum((x1 - x2) ^ 2))
}

box_distance <- function(bl_1, bl_2)
{
  box_dist <- 0
  
  l1 <- length(bl_1)
  l2 <- length(bl_2)
  
  if( l1 == l2 & l1 > 0)
  {
    for(i in 1:l1) 
    {
      n1 <- length(bl_1[[i]][,1])
      n2 <- length(bl_2[[i]][,1])

      if( n1 == n2 & n1 > 0)
      {
        for(j in 1:n1) 
        {
          box_dist <- box_dist + euc_dist(bl_1[[i]][j,], bl_2[[i]][j,])
        }
      }
    }
  }
    
  
  return( box_dist )
}


# =========== ALGORITHM =============

MovementListPat <- list()

setwd(KATALOG_TRENINGOWY)
FileListPat <- list.files(path = ".", pattern = "sample[0-9].bvh", ignore.case = TRUE, 
                       recursive = TRUE, full.names = TRUE)

FileListPatLength <- length(FileListPat)

cat("PLIKI:/n")

fcnt <- 0
if ( FileListPatLength >= 1 )
  for (i in 1:FileListPatLength)
  {
    FileName <- FileListPat[i]
    
    fend <- substr(FileName, nchar(FileName)-3, nchar(FileName))
    if ( fend == ".bvh" )
    {
      
      fcnt <- fcnt + 1
      cat("   ", fcnt, ": ", FileName, "\n")

      File <- readFingerprint(FileName)
      
      MovName <- unlist(strsplit(FileName, "/", fixed = TRUE))
      MovName <- MovName[2]
      
      MovName_1 <- MovName
      MovName_1 <- str_replace_all(MovName_1, "[0-9]", "")

      MovName_2 <- MovName
      MovName_2 <- str_replace_all(MovName_2, "[0-9]", "")
      MovName_2 <- str_replace_all(MovName_2, "left", "")
      MovName_2 <- str_replace_all(MovName_2, "right", "")
      
      
      
      PointCloud <- createPointCloud(File, TIME, SAMPLING)
      
      file_name <- paste(FileName, "_pc.csv", sep="")
      write.table(PointCloud, file = file_name,
                  col.names=TRUE, row.names=FALSE, sep=";", quote=FALSE, na="", dec=",")
      
  
  #@TDA@  
      Diag.info <- c()
      if( TDA_ON > 0 )
      {
        Diag.info <- ripsDiag(X=PointCloud, maxdimension=TDA_MAXDIM, maxscale=TDA_MAXSCALE, 
                              dist="euclidean", TDA_library, printProgress=FALSE)
        
      }
      pc <- PointCloud
      
      BoxList <- list()
      # LeftHand
      min <- c(min(pc[,"lhand_x"]), min(pc[,"lhand_y"]), min(pc[,"lhand_z"]))
      max <- c(max(pc[,"lhand_x"]), max(pc[,"lhand_y"]), max(pc[,"lhand_z"]))
      #mean <- c(mean(pc[,"lhand_x"]), mean(pc[,"lhand_y"]), mean(pc[,"lhand_z"]))
      Box <- rbind(min, max) #, mean)
      BoxList <- append(BoxList, list(Box))
      # RightHand
      min <- c(min(pc[,"rhand_x"]), min(pc[,"rhand_y"]), min(pc[,"rhand_z"]))
      max <- c(max(pc[,"rhand_x"]), max(pc[,"rhand_y"]), max(pc[,"rhand_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      # LeftFoot
      min <- c(min(pc[,"lfoot_x"]), min(pc[,"lfoot_y"]), min(pc[,"lfoot_z"]))
      max <- c(max(pc[,"lfoot_x"]), max(pc[,"lfoot_y"]), max(pc[,"lfoot_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      # RightFoot
      min <- c(min(pc[,"rfoot_x"]), min(pc[,"rfoot_y"]), min(pc[,"rfoot_z"]))
      max <- c(max(pc[,"rfoot_x"]), max(pc[,"rfoot_y"]), max(pc[,"rfoot_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))

      # LeftArm
      min <- c(min(pc[,"larm_x"]), min(pc[,"larm_y"]), min(pc[,"larm_z"]))
      max <- c(max(pc[,"larm_x"]), max(pc[,"larm_y"]), max(pc[,"larm_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      # RightArm
      min <- c(min(pc[,"rarm_x"]), min(pc[,"rarm_y"]), min(pc[,"rarm_z"]))
      max <- c(max(pc[,"rarm_x"]), max(pc[,"rarm_y"]), max(pc[,"rarm_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      # LeftForeArm
      min <- c(min(pc[,"lforearm_x"]), min(pc[,"lforearm_y"]), min(pc[,"lforearm_z"]))
      max <- c(max(pc[,"lforearm_x"]), max(pc[,"lforearm_y"]), max(pc[,"lforearm_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      # RightForeArm
      min <- c(min(pc[,"rforearm_x"]), min(pc[,"rforearm_y"]), min(pc[,"rforearm_z"]))
      max <- c(max(pc[,"rforearm_x"]), max(pc[,"rforearm_y"]), max(pc[,"rforearm_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      
      
      
      Movement <- list(FileName = FileName, MovName = MovName, File = File,
                       MovName_1 = MovName_1, MovName_2 = MovName_2,
                       PointCloud = PointCloud, Diag.info = Diag.info, BoxList = BoxList)
      
      
       MovementListPat <- append( MovementListPat, list(Movement) )
    }
    
  }  

MAX_SAMPLE_PAT = fcnt


setwd(KATALOG_ROBOCZY)


#################################

setwd(KATALOG_ROBOCZY)

MovementList <- list()

FileList <- list.files(path = ".", pattern = "sample[0-9].bvh", ignore.case = TRUE, 
                       recursive = TRUE, full.names = TRUE)


FileListLength <- length(FileList)

cat("PLIKI:/n")

fcnt <- 0
if ( FileListLength >= 1 )
  for (i in 1:FileListLength)
  {
    FileName <- FileList[i]
    
    fend <- substr(FileName, nchar(FileName)-3, nchar(FileName))
    if ( fend == ".bvh" )
    {
  
      fcnt <- fcnt + 1
      cat("   ", fcnt, ": ", FileName, "\n")

      File <- readFingerprint(FileName)
      
      MovName <- unlist(strsplit(FileName, "/", fixed = TRUE))
      MovName <- MovName[2]
  
      MovName_1 <- MovName
      MovName_1 <- str_replace_all(MovName_1, "[0-9]", "")
      
      MovName_2 <- MovName
      MovName_2 <- str_replace_all(MovName_2, "[0-9]", "")
      MovName_2 <- str_replace_all(MovName_2, "left", "")
      MovName_2 <- str_replace_all(MovName_2, "right", "")
      
      
      
      PointCloud <- createPointCloud(File, TIME, SAMPLING)

      file_name <- paste(FileName, "_pc.csv", sep="")
      write.table(PointCloud, file = file_name,
                  col.names=TRUE, row.names=FALSE, sep=";", quote=FALSE, na="", dec=",")
      
            
  #@TDA@   
      Diag.info <- c()
      
      pc <- PointCloud
      
      BoxList <- list()
      # LeftHand
      min <- c(min(pc[,"lhand_x"]), min(pc[,"lhand_y"]), min(pc[,"lhand_z"]))
      max <- c(max(pc[,"lhand_x"]), max(pc[,"lhand_y"]), max(pc[,"lhand_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      # RightHand
      min <- c(min(pc[,"rhand_x"]), min(pc[,"rhand_y"]), min(pc[,"rhand_z"]))
      max <- c(max(pc[,"rhand_x"]), max(pc[,"rhand_y"]), max(pc[,"rhand_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      # LeftFoot
      min <- c(min(pc[,"lfoot_x"]), min(pc[,"lfoot_y"]), min(pc[,"lfoot_z"]))
      max <- c(max(pc[,"lfoot_x"]), max(pc[,"lfoot_y"]), max(pc[,"lfoot_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      # RightFoot
      min <- c(min(pc[,"rfoot_x"]), min(pc[,"rfoot_y"]), min(pc[,"rfoot_z"]))
      max <- c(max(pc[,"rfoot_x"]), max(pc[,"rfoot_y"]), max(pc[,"rfoot_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      
      # LeftArm
      min <- c(min(pc[,"larm_x"]), min(pc[,"larm_y"]), min(pc[,"larm_z"]))
      max <- c(max(pc[,"larm_x"]), max(pc[,"larm_y"]), max(pc[,"larm_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      # RightArm
      min <- c(min(pc[,"rarm_x"]), min(pc[,"rarm_y"]), min(pc[,"rarm_z"]))
      max <- c(max(pc[,"rarm_x"]), max(pc[,"rarm_y"]), max(pc[,"rarm_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      # LeftForeArm
      min <- c(min(pc[,"lforearm_x"]), min(pc[,"lforearm_y"]), min(pc[,"lforearm_z"]))
      max <- c(max(pc[,"lforearm_x"]), max(pc[,"lforearm_y"]), max(pc[,"lforearm_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      # RightForeArm
      min <- c(min(pc[,"rforearm_x"]), min(pc[,"rforearm_y"]), min(pc[,"rforearm_z"]))
      max <- c(max(pc[,"rforearm_x"]), max(pc[,"rforearm_y"]), max(pc[,"rforearm_z"]))
      Box <- rbind(min, max)
      BoxList <- append(BoxList, list(Box))
      

            
      Movement <- list(FileName = FileName, MovName = MovName, File = File, 
                       MovName_1 = MovName_1, MovName_2 = MovName_2,
                       PointCloud = PointCloud, Diag.info = Diag.info, BoxList = BoxList)
        
      
      MovementList <- append( MovementList, list(Movement) )
    }
    
  }


MAX_SAMPLE = fcnt


for (m2 in 1:MAX_SAMPLE_PAT)
{
  MovName_1 <- MovementListPat[[m2]]$MovName_1
  if( MovName_1 == "empi_left_" )
    EMPI_LEFT_IDX <- m2
}

  
################################## 

MAX_DIM = TDA_MAXDIM

dim_names <- c()
for (d in 0:MAX_DIM)
  dim_names <- c(dim_names, d)
row_names <- c()
for (m1 in 1:MAX_SAMPLE)
  row_names <- c(row_names, MovementList[[m1]]$MovName)
col_names <- c()
for (m2 in 1:MAX_SAMPLE_PAT)
  col_names <- c(col_names, MovementListPat[[m2]]$MovName)

col_names <- c(col_names, "REC_OK", "REC_NAME")
dist <- array(Inf, dim=c(MAX_DIM+1,MAX_SAMPLE,MAX_SAMPLE_PAT + 2), 
              dimnames = list(dim_names,row_names,col_names) )

dist_xyz <- array(Inf, dim=c(MAX_DIM+1,MAX_SAMPLE,MAX_SAMPLE_PAT + 2), 
              dimnames = list(dim_names,row_names,col_names) )

col_names <- c("REC_OK", "REC_NAME")
dist_final <- array(Inf, dim=c(MAX_DIM+1,MAX_SAMPLE,2), 
              dimnames = list(dim_names,row_names,col_names) )

qual <- array(0, dim=MAX_DIM+1)
qual_sym <- array(0, dim=MAX_DIM+1)

qual_xyz <- array(0, dim=MAX_DIM+1)
qual_sym_xyz <- array(0, dim=MAX_DIM+1)

qual_final <- array(0, dim=MAX_DIM+1)
qual_sym_final <- array(0, dim=MAX_DIM+1)




for (d in 0:MAX_DIM) 
{
  good_rec <- 0
  good_rec_sym <- 0
  
  good_rec_xyz <- 0
  good_rec_sym_xyz <- 0

  good_rec_final <- 0
  good_rec_sym_final <- 0
  

  for (m1 in 1:MAX_SAMPLE)
  {
    min_dist <- Inf
    min_idx <- -1

    min_dist_xyz <- Inf
    min_idx_xyz <- -1
    
    min_dist_final <- Inf
    min_idx_final <- -1
    
    
    
    Diag1 <- MovementList[[m1]]$Diag.info$diagram
    
    
    rec_dist <- list()

    for (m2 in 1:MAX_SAMPLE_PAT)
    {
      cat("Obliczam dystans: ", m1, "-", m2, "\n")
      
      Diag2 <- MovementListPat[[m2]]$Diag.info$diagram
      
      distance <- Inf 

      
      dist[d+1, m1, m2] <- distance
      
      if( distance < min_dist )
      {
        min_dist <- distance
        min_idx <- m2
      }
      
      
      distance_xyz <- box_distance(MovementList[[m1]]$BoxList, MovementListPat[[m2]]$BoxList)

      dist_xyz[d+1, m1, m2] <- distance_xyz
      
      if( distance_xyz < min_dist_xyz )
      {
        min_dist_xyz <- distance_xyz
        min_idx_xyz <- m2
      }
      
      res <- list(Indx = m2, Dist = distance_xyz)
      rec_dist <- append( rec_dist, list(res) )
    }
    
    if( min_idx > -1 )
    {
      dist[d+1, m1, MAX_SAMPLE_PAT + 2] <- MovementListPat[[min_idx]]$MovName
      
      mov_name_1 <- MovementList[[m1]]$MovName_1
      mov_name_2 <- MovementListPat[[min_idx]]$MovName_1
      
      if( mov_name_1 == mov_name_2 )
      {
        good_rec <- good_rec + 1
        
        dist[d+1, m1, MAX_SAMPLE_PAT + 1] <- "OK"
      }
      else
      {
        dist[d+1, m1, MAX_SAMPLE_PAT + 1] <- "ERR"
      }

      
      mov_name_1 <- MovementList[[m1]]$MovName_2
      mov_name_2 <- MovementListPat[[min_idx]]$MovName_2
      
      if( mov_name_1 == mov_name_2 )
      {
        good_rec_sym <- good_rec_sym + 1
      }
    }
     
    if( min_idx_xyz > -1 & EMPI_LEFT_IDX > -1 )
    {
      MovName_1 <- MovementListPat[[min_idx_xyz]][["MovName_1"]]
      if( MovName_1 == "empi_right_" )
      {
        l <- length(MovementList[[m1]]$PointCloud[,1])
        ih <- floor(l/2)
        laz <- MovementList[[m1]]$PointCloud[ih, "larm_z"]
        raz <- MovementList[[m1]]$PointCloud[ih, "rarm_z"]
        if( laz > raz )
        {
          min_idx_xyz <- EMPI_LEFT_IDX
        }
      }
      
      
      dist_xyz[d+1, m1, MAX_SAMPLE_PAT + 2] <- MovementListPat[[min_idx_xyz]]$MovName
      
      mov_name_1 <- MovementList[[m1]]$MovName_1
      mov_name_2 <- MovementListPat[[min_idx_xyz]]$MovName_1
      
      if( mov_name_1 == mov_name_2 )
      {
        good_rec_xyz <- good_rec_xyz + 1
        
        dist_xyz[d+1, m1, MAX_SAMPLE_PAT + 1] <- "OK"
      }
      else
      {
        dist_xyz[d+1, m1, MAX_SAMPLE_PAT + 1] <- "ERR"
      }
      
      
      mov_name_1 <- MovementList[[m1]]$MovName_2
      mov_name_2 <- MovementListPat[[min_idx_xyz]]$MovName_2
      
      if( mov_name_1 == mov_name_2 )
      {
        good_rec_sym_xyz <- good_rec_sym_xyz + 1
      }

      
      min_idx_final <- min_idx_xyz
      

      ResMov <- MovementListPat[[min_idx_xyz]]
      if( TDA_ON > 0 ) # @#@ & is.element(ResMov$MovName_1, TDA_ERR_MOV_SET) )
      #if(FALSE)
      {
        rec_dist_sort <- rec_dist[order(sapply(rec_dist, '[[', "Dist"))]
        
        if( rec_dist_sort[[2]]$Dist - rec_dist_sort[[1]]$Dist <= TDA_VERIF_MARGIN ) 
        {
          min_dist_final <- Inf
          min_idx_final <- -1
          
          for (i in 1:min(TDA_VERIF_SIZE, length(rec_dist_sort)))
          {
            PatIndx <- rec_dist_sort[[i]]$Indx
            PatMov <- MovementListPat[[ PatIndx ]]
            Diag2 <- PatMov$Diag.info$diagram
            Mov <- MovementList[[m1]]
            Diag.info <- ripsDiag(X=Mov$PointCloud, maxdimension=TDA_MAXDIM, maxscale=TDA_MAXSCALE, 
                                dist="euclidean", TDA_library, printProgress=FALSE)
            Diag1 <- Diag.info$diagram
            
            distance_TDA <- wasserstein(Diag1 = Diag1, Diag2 = Diag2, dimension = d)
            
            if( distance_TDA < min_dist_final )
            {
              min_dist_final <- distance_TDA
              min_idx_final <- PatIndx
            }
            
            dist[d+1, m1, PatIndx] <- distance_TDA
  
          }
          
          if( min_idx_final > -1 & min_idx_xyz > -1 )
          {
            name_xyz <- MovementListPat[[min_idx_xyz]]$MovName_1
            name_xyz_s <- MovementListPat[[min_idx_xyz]]$MovName_2
            name_TDA <- MovementListPat[[min_idx_final]]$MovName_1
            name_TDA_s <- MovementListPat[[min_idx_final]]$MovName_2
            if( name_xyz != name_TDA & name_xyz_s == name_TDA_s )
            {
              min_dist_final <- distance_xyz
              min_idx_final <- min_idx_xyz
            }
          }
          
          
        }
      }

      if( min_idx_final > -1 )
      {
        dist_final[d+1, m1, 2] <- MovementListPat[[min_idx_final]]$MovName
        
        mov_name_1 <- MovementList[[m1]]$MovName_1
        mov_name_2 <- MovementListPat[[min_idx_final]]$MovName_1
        
        if( mov_name_1 == mov_name_2 )
          good_rec_final <- good_rec_final + 1
        
        
        mov_name_1 <- MovementList[[m1]]$MovName_2
        mov_name_2 <- MovementListPat[[min_idx_final]]$MovName_2
        
        if( mov_name_1 == mov_name_2 )
        {
          good_rec_sym_final <- good_rec_sym_final + 1
          
          dist_final[d+1, m1, 1] <- "OK"
        }
        else
        {
          dist_final[d+1, m1, 1] <- "ERR"
        }
      } 

    }  
    
  }
 
  if( MAX_SAMPLE > 0 )
  {
    qual[d+1] <- good_rec / MAX_SAMPLE
    qual_sym[d+1] <- good_rec_sym / MAX_SAMPLE
  
    qual_xyz[d+1] <- good_rec_xyz / MAX_SAMPLE
    qual_sym_xyz[d+1] <- good_rec_sym_xyz / MAX_SAMPLE

    qual_final[d+1] <- good_rec_final / MAX_SAMPLE
    qual_sym_final[d+1] <- good_rec_sym_final / MAX_SAMPLE
    
  }
}

##########
cat("\n##########################")
cat("RESULTS:\n")
print(qual)
write.csv2(qual, "qual.csv")

cat("RESULTS without symmetry:\n")
print(qual_sym)
write.csv2(qual_sym, "qual_sym.csv")

for (d in 0:MAX_DIM) 
{
  fname <- paste("dist_", toString(d), ".csv")
  write.csv2(dist[d+1,,], fname)
}

cat("\n##########################")
cat("RESULTS (xyz):\n")
print(qual_xyz)
write.csv2(qual_xyz, "qual_xyz.csv")

cat("RESULTS (xyz) without symmetry:\n")
print(qual_sym_xyz)
write.csv2(qual_sym_xyz, "qual_sym_xyz.csv")

for (d in 0:MAX_DIM) 
{
  fname <- paste("dist_xyz_", toString(d), ".csv")
  write.csv2(dist_xyz[d+1,,], fname)
}

cat("\n##########################")
cat("RESULTS (FINAL):\n")
print(qual_final)
write.csv2(qual_final, "qual_final.csv")

cat("RESULTS (FINAL) without symmetry:\n")
print(qual_sym_final)
write.csv2(qual_sym_final, "qual_sym_final.csv")

for (d in 0:MAX_DIM) 
{
  fname <- paste("dist_final_", toString(d), ".csv")
  write.csv2(dist_final[d+1,,], fname)
}
