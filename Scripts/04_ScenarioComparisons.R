library(RSVP)
library(viridis)
library(RMODFLOW)
library(ggplot2)
library(colorspace)
library(magrittr)

#-------------------------------------------------------------------------------------------------#
# Settings ----------------------------------------------------------------

origin_date <- as.Date('1990-09-30')

create_sp_charts = FALSE  # Many SPs, very slow

# Lists of scenarios
rel_dir <- '../..'
# scen_names <- c('Basecase', 'Natveg', 'PRMS')
# scen_dirs <- file.path(rel_dir, c('Run_baseupdate', 'Run_natveg', 'Run_prms'))

update_dir <- latest_dir(data_dir['update_dir','loc'])
plot_data_dir <- file.path('../../SVIHM_Input_Files/reference_data_for_plots/')

out_dir = data_dir["scenario_dir","loc"] #file.path("../../")

# Alternate scenario directories list if Run folders have already been moved to Scenarios folder
scen_folders = list.files(path = out_dir)[grepl(pattern = "Run", x = list.files(path=out_dir))]
scen_dirs = file.path(out_dir, scen_folders)
scen_names = gsub(x = scen_folders, pattern = "Run_", replacement="")
scen_names = gsub(x=scen_names, pattern = " 2025-07-31", replacement="")

if (!dir.exists(out_dir)) {dir.create(out_dir, recursive = T)}

# info from svihm.swbm
rep_swbm_file <- file.path(scen_dirs[1],'SWBM',"svihm.swbm")

discretization_settings <- read_swbm_block(rep_swbm_file, block_name = "DISCRETIZATION")
wy_start = discretization_settings[['WYSTART']]
n_stress = discretization_settings[['NMONTHS']]

m3day_to_cfs = 1 * 35.3147 * 1/(60*60*24)

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Read in Data ------------------------------------------------------------

#-- Observed FJ
fj_obs <- read.csv(file.path(update_dir, list.files(update_dir, pattern = 'FJ (USGS 11519500)*')),
                   stringsAsFactors = F)
fj_obs$Date <- as.Date(fj_obs$Date)

#-- Modeled FJ
sfr_locs <- read.csv(file.path(data_dir['ref_data_dir','loc'], 'sfr_gages.csv'),
                     row.names=1, stringsAsFactors = F)
fj_sim <- list()
for (i in 1:length(scen_names)) {
  fj_sim[[i]] <- import_sfr_gauge(file.path(scen_dirs[i], 'MODFLOW/Streamflow_FJ_SVIHM.dat'), origin_date = origin_date)
}
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#

# FJ Flow Comparison Dates
plot_start_date <- as.Date('2020-10-01')
plot_end_date <-  as.Date('2021-12-31')

# Subset dates...
fj_obs_sub <- subset.DateTwoSided(fj_obs, start = plot_start_date, end = plot_end_date, date_col = 'Date')
fj_sim_sub <- lapply(fj_sim, subset.DateTwoSided, start = plot_start_date, end = plot_end_date, date_col = 'Date')

# Simulation Plot colors
simcolors <- c('black','green3', 'orangered3')

#-- Timeseries Plot
#par(mar=c(4,4,2,1))  #bottom, left, top, right
plot.ts_setup(fj_obs_sub$Date, fj_obs_sub$Flow,
              interval = 'month',
              xlabel='Calendar Year Date',
              ylabel='Flow (cfs)',
              log='y')
lines(fj_obs$Date, fj_obs$Flow, col='dodgerblue2', lwd = 2)
for (i in 1:length(scen_names)) {
  lines(fj_sim_sub[[i]]$Date, fj_sim_sub[[i]]$Flow_cfs, col=simcolors[i], lwd = 2)
}

# Add a base R legend
legend("topleft", legend = c("FJ Observed", scen_names),
       col = c("dodgerblue2", simcolors), lwd = 2, bty = "n", y.intersp = 1.2)

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Export Flows to view in SVIHM_Analysis.exe
shortest <- min(unlist(lapply(fj_sim, nrow)))
fj_comb <- do.call(cbind, lapply(fj_sim, function(df) {df[1:shortest,c('Date','Flow_cfs')]}))
names(fj_comb) <- paste0(c('Date_',''), rep(scen_names, rep(2,length(scen_names))))
names(fj_comb)[1] <- 'Date'
write.csv(fj_comb[,c('Date',scen_names)], file.path(out_dir,'scen_combined_fj.csv'), row.names = F)

# Output scenario flows so they can be analysed for functional flows
write_each_scen_as_own_file = F
if(write_each_scen_as_own_file == T){
  func_flow_dir = "C:/Users/ck798/Documents/GitHub/Func_Flows_2025/python-flow-calculator/user_input_files"
  for(i in 1:length(scen_names)){
    scen_name = scen_names[i]
    fj_scen = data.frame(
      date = format(fj_comb$Date, format = "%m/%d/%Y"),
      flow = fj_comb[,scen_name])
    write.csv(fj_scen,
              file.path(func_flow_dir, paste0('fj_',scen_name,'.csv')), row.names = F, quote = F)
  }

  # write summary table, for batch func flow processing
  paths = file.path(func_flow_dir, paste0('fj_',scen_names,".csv"))
  summary_df = data.frame(usgs = "", cdec = "",
                          path = paths,
                          comid="",
                          class = "",
                          lat = 41.64065586831092,
                          lng = -123.01500919353471,
                          calculator = "Flashy")
  summary_df_obs = data.frame(usgs = "11519500", cdec="", path = "", comid="",
                              class = "LSR", lat = "", lng = "",
                              calculator = "Flashy")
  summary_df = rbind(summary_df, summary_df_obs)
  write.csv(summary_df,
            file = file.path(func_flow_dir, "fj_scen_func_flows_batch_2025.10.13.csv"),
            row.names = F, quote = F)



}

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#


# Scenario parameter summary table ----------------------------------------



# Summarize parameters in svihm_parameter_table
scen_update_dirs = list.dirs(path = out_dir,
                             recursive = F)[!grepl(pattern = "Run",
                                                   x = list.dirs(path=out_dir,
                                                                 recursive = F))]
# for each scenario, read in scen_param file, and combine

for(i in 1:length(scen_update_dirs)){
  scen_files = list.files(scen_update_dirs[i])
  has_param_csv = grepl(x = scen_files, pattern="parameter_summary")
  if(sum(has_param_csv)>0){
    param_csv = read.csv(file.path(scen_update_dirs[i],scen_files[has_param_csv]))
    if(exists("param_csv_out")){
      # check for new columns in next scenario
      new_cols = setdiff(colnames(param_csv), colnames(param_csv_out))
      # add new columns (will have no effect if no new cols)
      param_csv_out[,new_cols] = NA
      # what columns exist in the running table that don't exist in the current scenario param table
      existing_extra_cols = setdiff(colnames(param_csv_out), colnames(param_csv))
      param_csv[,existing_extra_cols] = NA
      param_csv_out = rbind(param_csv_out, param_csv)
    }
    if(!exists("param_csv_out")){param_csv_out = param_csv}
  }
}

write.csv(param_csv_out, file.path(out_dir,'scenarios_param_summary.csv'), row.names = F)

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Budget comparison (only can do two models at a time)
models_to_compare <- c(1,2)
mf_budgets <- lapply(models_to_compare, function(x) {
  mfnam <- rmf_read_nam(file.path(scen_dirs[x],'MODFLOW/SVIHM.nam'))
  mfdis <- rmf_read_dis(file.path(scen_dirs[x],'MODFLOW/SVIHM.dis'), nam=mfnam)
  bud <- rmf_read_budget(file.path(scen_dirs[x],'MODFLOW/SVIHM.lst'))
  return(bud)
})


# Export SWBM Budgets -----------------------------------------------------------------


postprocess_SWBM_budgets =  F

if(postprocess_SWBM_budgets == T){
  scen_results_dirs = list.dirs(path = out_dir,
                                recursive = F)[grepl(pattern = "Run",
                                                     x = list.dirs(path=out_dir,
                                                                   recursive = F))]

  scen_param_summary_tab = read.csv(file.path(out_dir,"scenarios_param_summary.csv"))
  budget_results_dir = file.path(out_dir, "Scenario Results_Budgets")

  if(!dir.exists(budget_results_dir)){dir.create(budget_results_dir)}

  for(i in 1:nrow(scen_param_summary_tab)){
    scen_swbm_dir = file.path(out_dir,
                              paste0("Run_",scen_param_summary_tab$full_name[i]),
                              "SWBM")
    scen_id = scen_param_summary_tab$name[i]

    plot_swbm_volumetric_budget(swbm_dir=scen_swbm_dir,
                                out_dir=budget_results_dir,
                                plot_type = "monthly",
                                write_csv = TRUE,
                                scenario_id = scen_id)
  }

  # copy landuse-based SWBM budget to new folder for manuscript

  budgets_dir = "C:/Users/ck798/Documents/GitHub/Fish-and-Ag-Benefits-MS/Data/SVIHM Model Results_2025.10.13/Budgets"
  aet_by_lu_file = "monthly_vol_aET_by_landcover.dat"
  for(i in 1:nrow(scen_param_summary_tab)){
    scen_swbm_dir = file.path(out_dir,
                              paste0("Run_",scen_param_summary_tab$full_name[i]),
                              "SWBM")
    scen_id = scen_param_summary_tab$name[i]
    file.copy(from = file.path(scen_swbm_dir, aet_by_lu_file),
              to = file.path(budgets_dir,
                             paste0(scen_id, "_", aet_by_lu_file)))
  }
}



# Export MODFLOW Budgets -----------------------------------------------------------------

postprocess_MF_budgets =  F
if(postprocess_MF_budgets == T){
  for(i in 1:nrow(scen_param_summary_tab)){
    scen_mf_dir = file.path(out_dir,
                            paste0("Run_",scen_param_summary_tab$full_name[i]),
                            "MODFLOW")
    scen_id = scen_param_summary_tab$name[i]

    mfnam <- rmf_read_nam(file.path(scen_mf_dir,'SVIHM.nam'))
    mfdis <- rmf_read_dis(file.path(scen_mf_dir,'SVIHM.dis'), nam=mfnam)
    bud <- rmf_read_budget(file.path(scen_mf_dir,'SVIHM.lst'))

    bc = bud$cumulative

    # generate table to store monthly cumulative results
    bud_monthly_init = bc[1,]
    bud_monthly_init[1,] = NA
    budget_cols = colnames(bc)[!(colnames(bc) %in% c("kstp","kper"))]

    for(sp in 1:max(bc$kper)){
      bud_monthly_sp = bud_monthly_init
      max_kstp_for_month = max(bc$kstp[bc$kper==sp])
      max_row_i = which(bud$cumulative$kper == sp &
                          bud$cumulative$kstp==max_kstp_for_month)
      bud_monthly_sp = bc[max_row_i, ]

      if(sp==1){
        bud_monthly_cum = bud_monthly_sp
        bud_monthly_rates = bud_monthly_sp
      } else {
        bud_monthly_cum = rbind(bud_monthly_cum, bud_monthly_sp)
        bud_monthly_rates = rbind(bud_monthly_rates, bud_monthly_sp)
        diffs = bud_monthly_sp[,budget_cols] - bud_monthly_rates[sp-1,budget_cols]
        bud_monthly_rates[sp, budget_cols] = diffs
      }
    }

    write.csv(bud_monthly_rates, row.names = F,
              file = file.path(budget_results_dir, paste0(scen_id, "_MODFLOW Budget m3 per month.csv")))
  }

}



