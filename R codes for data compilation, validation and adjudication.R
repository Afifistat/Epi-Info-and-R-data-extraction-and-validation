
library(tidyverse) #data handling
library(compareDF) #for matching

#working directory------------------------
#rviewer_1
setwd ("D:/Data Extraction_manuscript/data/reviewer_1/data")

#rviewer_2
setwd ("D:/Data Extraction_manuscript/data/reviewer_2/data") 
################################################################

#3.5.1.Exporting and reading data into R---------

################################################################

#s--------------------------------------
s <- read.csv(file="s.csv", 
                  head=TRUE, 
                  sep=",", 
                  stringsAsFactors=FALSE) %>% 
  
    rename (su = GlobalRecordId) %>%
    
    select(-UniqueKey, 
           -RECSTATUS, 
           -(FirstSaveLogonName:FKEY))

#t--------------------------------------
t <- read.csv(file="t.csv", 
              head=TRUE,
              sep=",",
              stringsAsFactors=FALSE) %>% 
  
  rename (tu = GlobalRecordId,
          su = FKEY) %>%
  
  select(-UniqueKey,
         -RECSTATUS,
         -(FirstSaveLogonName:LastSaveTime),
         -study_id_expo)

#g--------------------------------------
g <- read.csv(file="g.csv",
                  head=TRUE,
                  sep=",",
                  stringsAsFactors=FALSE)%>% 
  
        rename (gu = GlobalRecordId,
                tu = FKEY) %>%
  
        select(-UniqueKey,
               -RECSTATUS,
               -(FirstSaveLogonName:LastSaveTime),
               -group_order,
               -study_id_expo,
               -trial_expo)

#i------------------------------------------
i <- read.csv(file="i.csv",
              head=TRUE,
              sep=",",
              stringsAsFactors=FALSE) %>%
  
     rename (iu = GlobalRecordId,
             gu = FKEY) %>%
      
     mutate (outcome = "incidence") %>% 
      
      
     select(-UniqueKey,
            -RECSTATUS,
            -(FirstSaveLogonName:LastSaveTime),
             -study_id_expo,
            -trial_expo,
            -Group_expo,
            -study_id_expo1,
            -trial_expo1,
            -Group_expo1,
            -T,
            -U) 
      
#c--------------------------------------------
c <- read.csv(file="c.csv",
              head=TRUE,
              sep=",",
              stringsAsFactors=FALSE) %>%
  
      rename (cu = GlobalRecordId,
              gu = FKEY) %>%
      
      mutate (outcome = "cure")%>%
      
      select(-UniqueKey,
             -RECSTATUS, 
             -(FirstSaveLogonName:LastSaveTime),
             -study_id_expo,
             -trial_expo,
             -Group_expo, 
             -study_id_expo1,
             -trial_expo1,
             -Group_expo1,
             -T,
             -U)

#p----------------------------------------------
p <- read.csv(file="p.csv",
              head=TRUE,
              sep=",",
              stringsAsFactors=FALSE) %>% 
  
      rename (pu = GlobalRecordId,
              gu = FKEY) %>% 
    
      mutate (outcome = "prevalence")%>% 
    
      select(-UniqueKey,
             -RECSTATUS, 
             -(FirstSaveLogonName:LastSaveTime),
             -study_id_expo,
             -trial_expo,
             -Group_expo, 
             -study_id_expo1,
             -trial_expo1,
             -Group_expo1,
             -T,
             -U)


#i_arm------------------------------
i_arm <- read.csv(file="i_arm.csv",
                      head=TRUE,
                      sep=",",
                      stringsAsFactors=FALSE) %>% 
  
          rename (iu = FKEY) %>%
  
          mutate (d_format = "arm") %>%
  
          select(-UniqueKey,
                 -UniqueRowId,
                 -GlobalRecordId,
                 -RECSTATUS)

#p_arm------------------------------

p_arm <- read.csv(file="p_arm.csv",
                  head=TRUE,
                  sep=",",
                  stringsAsFactors=FALSE)%>%
  
       rename (pu = FKEY,
                 Others_Def = Other_def) %>% # error in var_nmae in epi_info
  
       mutate (d_format = "arm") %>%
  
       select(-UniqueKey,
              -UniqueRowId,
              -GlobalRecordId,
              -RECSTATUS)

#c_arm------------------------------
c_arm <- read.csv(file="c_arm.csv",
                      head=TRUE,
                      sep=",",
                      stringsAsFactors=FALSE)%>%
  
          rename (cu = FKEY) %>%
  
          mutate (d_format = "arm") %>%
  
          select(-UniqueKey,
                  -UniqueRowId,
                  -GlobalRecordId,
                  -RECSTATUS)

#i_cont------------------------------
i_cont <- read.csv(file="i_cont.csv",
                   head=TRUE,
                   sep=",",
                   stringsAsFactors=FALSE)%>%
  
         rename (iu = FKEY) %>%
  
         mutate (d_format = "cont") %>%
  
        select(-UniqueKey,
               -UniqueRowId,
               -GlobalRecordId,
               -RECSTATUS)

#p_cont------------------------------
p_cont <- read.csv(file="p_cont.csv",
                   head=TRUE,
                   sep=",",
                   stringsAsFactors=FALSE)%>%
  
           rename (pu = FKEY) %>%
      
           mutate (d_format = "cont") %>%
      
           select(-UniqueKey,
                  -UniqueRowId,
                  -GlobalRecordId,
                  -RECSTATUS)

#c_cont------------------------------
c_cont <- read.csv(file="c_cont.csv",
                       head=TRUE,
                       sep=",",
                       stringsAsFactors=FALSE)%>%
  
          rename (cu = FKEY) %>%
  
          mutate (d_format = "cont") %>%
  
          select(-UniqueKey,
                 -UniqueRowId,
                 -GlobalRecordId,
                 -RECSTATUS)



################################################################

#3.5.2.Appending and merging multiple data files-------------
################################################################

#appened ipc---------------------------------
ipc <- bind_rows(i,
                 p,
                 c) %>%
  
  unite(ipc, iu, pu, cu, sep = "")

#append (all outcomes,contrast and arm)-----------
# Note that variables captured by the arm and contrast sheets are different 
ipc_arm_cont <- bind_rows(i_arm,
                          p_arm,
                          c_arm,
                          i_cont,
                          p_cont,
                          c_cont) %>%
  
              unite(ipc, iu, pu,cu, sep = "")

#merge_cascade-------------------------------------
full <- inner_join(s, t, by = "su") %>%

        inner_join(g, by = "tu")  %>% 
    
        inner_join(ipc, by = "gu") %>%
    
        inner_join(ipc_arm_cont, by = "ipc") %>%
  
        select(-su,
               -tu,
               -gu,
               -ipc)%>% 
     
      rename_all(tolower)%>% 
        
      rename (no_f = tot_no_of_farm_sselectedforinclusion,
              no_c = totnoofcowsselectedforinclusion,
              no_q = totnoofquartersselectedforinclusion,
              incl_criteria = cowsinclusionexclusioncriteria,
              i_all_unit = interventionallocationunit,
              sample_size = wasanapriorisamplesizecalculationreported,
              hier_bias = ifthedatahavesomekindofhierarchicalstructurehavetheauthorsaccoun,
              itt = wastheanalysisdoneasintentiontotreat,
              out_bias = inappropriate,
              sel_bias = cmcaseswereexcluded,
              teat_prep = teatpreparationbeforedctinfusion1,
              depth_inser = depthofinsertionofdctsyringecannula,
              teat_spray = teatsprayordippingafterdctinfusion,
              path_def = others_def,
              out_def = def)


# remove
rm (s, 
    t,
    g,
    i_arm,
    i_cont,
    p_arm,
    p_cont,
    c_arm,
    c_cont,
    i,c,p,
    ipc,
    ipc_arm_cont)


full_def <- full %>%

            group_by(studyid, outcome, time1) %>%

            mutate(out_def = out_def[1L],
                   from1 = from1[1L],
                   to1 = to1[1L],
                   dayspostcalving = dayspostcalving[1L])   %>% 
  
         select(-incl_criteria,
                -elaborate,
                -tstradename,
                -abtradename,
                -more,
                -pico34,
                -path_def,
                -out_def,
                -stratificationlevels,
                -order_active_ingredient,
                -author, 
                -typeofpublication,
                -challengedose,
                -selectioncriteria,
                -intervaldays,
                -dayspostcalving) %>% 
  
        rename (study_id = studyid ,
                year = year_rw,
                no_farms = no_f,
                no_cows = no_c,
                type_exposure = typeofexposure,
                trial_design = trialdesign,
                studys_objective = studysobjective,
                avrg_parity = parity) %>% 
  
      ungroup() %>% 

     filter(study_id == 118)
  
#assign and save----------------------------------------------

reviewer_1 <- full_def 

reviewer_2 <- full_def 

save(reviewer_1, file = "reviewer_1.RData")
save(reviewer_2, file = "reviewer_2.RData")

################################################################

#3.6.Comparing datasets extracted by reviewers------------

################################################################

#Study/trial level variables-----------
#reviewer_1
rev1_sl <- reviewer_1 %>% 
  
           select(study_id:country,
                  no_farms, no_cows,
                  type_exposure:trial_design,
                  avrg_parity, breed) %>% 
            
          distinct() 
#reviewer_2
rev2_sl <- reviewer_2 %>% 
  
          select(study_id:country,
                 no_farms, no_cows,
                 type_exposure:trial_design,
                 avrg_parity, breed) %>% 
  
         distinct() 

# agreement
sim_sl <- intersect(rev1_sl,
                    rev1_sl) #0

#determine sources of disagreements
comp_sl <- compare_df(rev1_sl, 
                      rev2_sl, c("study_id"))

sl <- create_output_table(comp_sl)

#Group-level variables------------------------
#reviewer_1
rev1_gl <- reviewer_1 %>% 
  
          select(study_id,
                 pico12,
                 pico5, 
                 active_igredient,
                 ts,
                 concentration,
                 route,
                 administrationregime,
                 abpreparation,
                 teat_prep,
                 depth_inser,
                 teat_spray)%>% 
  
        distinct()

#reviewer_2
rev2_gl <- reviewer_2 %>% 
  
          select(study_id,
                 pico12,
                 pico5, 
                 active_igredient,
                 ts,
                 concentration,
                 route,
                 administrationregime,
                 abpreparation,
                 teat_prep,
                 depth_inser,
                 teat_spray)%>% 
  
         distinct() 

#Checking agreement
sim_gl <- intersect(rev1_gl,
                    
                    rev2_gl) #0

#determine sources of disagreements
comp_gl <- compare_df(rev1_gl, 
                      
                      rev2_gl, c("study_id", "pico5"))

gl <- create_output_table(comp_gl)

#arm-level-------------------------------------
#reviewer_1
rev1_arm <- reviewer_1 %>% 
  
            filter(d_format=="arm") %>% 
  
            select(study_id,
                   outcome,
                   pico5,
                   pico12,
                   time1:total, 
                   - ts,
                   - estimatetype, 
                   - estimatescale, 
                   -cilevel)
#reviewer_2
rev2_arm <- reviewer_2 %>% 
  
            filter(d_format=="arm") %>% 
            
            select(study_id,
                   outcome,
                   pico5,
                   pico12,
                   time1:total, 
                   - ts,
                   - estimatetype, 
                   - estimatescale, 
                   -cilevel)

#Checking agreement
sim_arm <- intersect(rev1_arm,rev2_arm) #0

#determine sources of disagreements
comp_arm <-  compare_df (rev1_arm, 
                         rev2_arm, c("outcome",
                                     "time1", 
                                     "ounit1", 
                                     "stratifyingvariable", 
                                     "pathogenspp", 
                                     "pico5"), 
                             keep_unchanged_rows = TRUE)

arm <- create_output_table(comp_arm, 
                           
                           limit = 500)

#contrast-level--------------------------------
#reviewer_1
rev1_cont <- reviewer_1 %>%
  
            filter(d_format=="cont") %>% 
            
            select(study_id, 
                   pico5, 
                   outcome,
                   pathogenspp, 
                   estimatetype, 
                   estimatescale, 
                   comparisongroup:pvalue1) 
#reviewer_2
rev2_cont <- reviewer_2 %>%
  
            filter(d_format=="cont") %>% 
            
            select(study_id, 
                   pico5, 
                   outcome,
                   pathogenspp, 
                   estimatetype, 
                   estimatescale, 
                   comparisongroup:pvalue1) 

#Checking agreement
sim_cont <- intersect(rev1_cont,
                      
                      rev2_cont) #0

#determine sources of disagreements
comp_cont <- compare_df(rev1_cont,
                        rev2_cont, c("study_id", 
                                     "outcome", 
                                     "pathogenspp"), 
                        keep_unchanged_rows = TRUE)

cont <- create_output_table(comp_cont, 
                            
                            limit = 500)

################################################################

#3.7.Data adjudication------------------------------------------

################################################################
#arm and contrast----------------------------------------------
final_v1 <-reviewer_2 %>% 
  
          filter((outcome=="prevalence" &
                    
                    pathogenspp=="Others")|
                   
                   (d_format =="cont")) %>% 
          
          mutate (detectionmethod =  "Culture") %>%
          
          mutate (from1 =  0) %>%
          
          mutate (to1 =  10) %>%  
          
          bind_rows(reviewer_2)

#gl--------------------------------------
final_v1$teat_prep <- "Not-reported"

#sl----------------------------------------
final_v1$parity <- 2.52

