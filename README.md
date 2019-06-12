# afib
Miscellaneous Code For Analysis and Data Management of MIT-BIH Atrial Fibrillation Database


database can be found at: https://physionet.org/physiobank/database/afdb/


wfdb package used for code can be found at: https://archive.physionet.org/physiotools/wfdb-linux-quick-start.shtml

The extraction of afdb was done with Linux Bash Script and all files found in database link


File paths need to be changed in bash, python, and R script to match your directories

runme.sh needs all files found at database link

makesamples.R needs merge\*.cxv files found in /oldmerge

rmsamptrans.R needs files found in /mergecurrent and /sampscurrent


merge.py needs \*.txt found in /extracted

     atr*.txt is extracted annotations of heartbeat type 
     
     qrs*.txt is extracted RR-Intervals


merge*.csv === new merged files with additional data
               
               found in /mergecurrent


trans*.csv ==== transition matrices for each full data file

               found in /transcurrent


samps*.csv ==== csv of 45 second samples taken for each individual

                found in /sampscurrent
                    includes samps with mix afib/normal readings
                found in /sampsnotrans
                    includes only samples with all afib or all non-afib readings

merge: 
    
     RRint, endsample#, annotation, running mean, running seconds, transition(1 = S, 2 = R, 3 = L)


trans: 
     
     S = short (x<.85\*runningmean), R = regular (.85\*runningmean<x<1.15\*runningmean),      
     L = long (x>1.15\*runningmean) where x is rr interval in seconds
        
     Columns = From, Rows = To
        
        
samps: 

     ID, start sample#, end sample#, length(beats), length(seconds), afib (t/f), proportions of    
     trans matrix(ss, sr, sl, rs, rr, rl ls lr, ll)




Python 2.7.15 was used for compiling


R 3.6.0 was used for R compiling
