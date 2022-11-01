This repository include the raw data from "Pollination systems and nectar rewards in four Andean species of Salvia (Lamiaceae)" by A. Saravia-Nava, S. Benitez-Vieyra, O. N. Urquizo, H.M Niemeyer & C. F. Pinto, alongwith an R script to replicate all the analyses.

File and variable names. 

visitors.csv. This file contain information about flower visitors
   sp. Plant species
   hour. Observation time
   flowers. Number of flowers observed.
   Amazilia chionogaster; Neocorynura_sp; Chlorostilbon_lucidus; Bombus_pauloensis; Apis_mellifera; Toxomerus_sp; Anthophora_sp1_grey; Anthophora_sp2_yellow; Anthophora_sp3_black; Centris_sp; Eucerini; Bombilidae; Vespidae. Numbers of individuals observed.
   
effectiveness.csv. This file contains information about the number of pollen grains deposited on stigma after a single visit (a measure of pollinator effectivenss). 
  sp. Plant species
  pollinator. Pollinator species.
  pollen.tot. Number of pollen grains observed on a stigma after a single pollinator visit.
  pollen.germ. Number of germinated (empty) pollen grains observed on a stigma after a single pollinator visit.
  
nectar_vol_con.csv. This file contains information about nectar volume, concentration and sugar content.
  sp. Plat species
  type. Samplig strategy. It distinguish between accumulated samples (acumulada) and repeated samples on the same flower (repetida).
  replicate. ID (meaningless in repeated samples).
  hour. Time of nectar harvest.
  vol.ul. Volume in microlitres.
  brix. Concentration in Brix scale
  conc_wv. Concentration in micrograms per microliter.
  sugar. Sugar content in micrograms.
  
nectar_comp.csv. This file contain information about nectar sugar composition.
  sp. Plant species
  fructose. Fructose present in a sample (g).
  glucose. Glucose present in a sample (g).
  sucrose. Sucrose present in a sample (g).
  
general_script.R. This file contains all the details needed to replicate statistical analyses and graphs.
