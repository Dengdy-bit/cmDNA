#for i in `cat xunhuan.txt`
i="Group"
#do 
mkdir Alpha_$i

#####alpha_diversity_index.xls is the result of alpha diversity for all samples

#alpha_diversity.py -i filter_S_v2.biom -m shannon,simpson,chao1 -o Alpha_$i/$i_alpha_diversity_index.xls -t Alpha_$i/$i_rep_set.tre
head -n 1 alpha_diversity_index.xls > Alpha_$i/${i}_alpha_diversity_index.xls
for m in `awk '{print $1}' group_${i}.txt`;do grep $m  alpha_diversity_index.xls ;done >> Alpha_$i/${i}_alpha_diversity_index.xls

perl alpha_all_index_with_ggsignif.pl -index Alpha_${i}/${i}_alpha_diversity_index.xls -outdir Alpha_${i}/Alpha_index -group group_${i}.txt -diff "Benign&Malignant"

perl  alpha_wilcox_spcial.pl Alpha_${i}/Alpha_index

Rscript Alpha_${i}/Alpha_index/chao1/chao1.Tukey.r
Rscript  Alpha_${i}/Alpha_index/chao1/chao1.boxplot.r
convert  -density 100 Alpha_${i}/Alpha_index/chao1/chao1.box.pdf Alpha_${i}/Alpha_index/chao1/chao1.box.png
Rscript Alpha_${i}/Alpha_index/shannon/shannon.Tukey.r
Rscript  Alpha_${i}/Alpha_index/shannon/shannon.boxplot.r
convert  -density 100 Alpha_${i}/Alpha_index/shannon/shannon.box.pdf Alpha_${i}/Alpha_index/shannon/shannon.box.png
Rscript Alpha_${i}/Alpha_index/simpson/simpson.Tukey.r
Rscript  Alpha_${i}/Alpha_index/simpson/simpson.boxplot.r
convert  -density 100 Alpha_${i}/Alpha_index/simpson/simpson.box.pdf Alpha_${i}/Alpha_index/simpson/simpson.box.png

mkdir NMDS_${i}
Rscript filter_data.R -in otu_table_even.txt -group group_${i}.txt -out ${i}_out_otu.txt   ####Absolute abundance values for all samples
perl NMDS_group.pl -even_otu_table ${i}_out_otu.txt -group group_${i}.txt -outdir NMDS_${i}
Rscript NMDS_${i}/NMDS.r
convert  -density 100 NMDS_${i}/NMDS.pdf NMDS_${i}/NMDS.png

####LEFSE####
mkdir Lefse_${i}
Rscript filter_data.R -in relative_otu_table_even.txt -group group_${i}.txt -out Lefse_${i}/${i}_out_otu.txt

cut -f 2- Lefse_${i}/${i}_out_otu.txt |sed 's/$/;/g'|grep -v 'Unassigned|No blast hit|Unclassified'|sed -e 's/k__;//g;s/ p__;//g;s/ c__;//g;s/ o__;//g;s/ f__;//g;s/ g__;//g;s/ s__;//g;s/.$//g'|awk -F "\t" '{printf $NF"\t";for(o=1;o<NF;o++){printf $o"\t"};printf "\n"}' |sed 's/;/|/g'|perl -ne 'chomp;my @cut=split /\t/,$_;$cut[0]=~s/\s+//g;$cut[0]=~s/\|{1,7}$//g;my $line=join "\t",@cut;print "$line\n";'>Lefse_${i}/${i}_lefse_input_form.xls

python3 /public3/Pipeline/Amplicon/version6.0/lefse_run.py Lefse_${i}/${i}_lefse_input_form.xls Lefse_${i} group_${i}.txt "Male,Female"

source /public/Biosoft/linuxbrew_SG/bin/activate.rc

o="Male-Female" 

/public3/Pipeline/Amplicon/version6.0/lefse/format_input.py /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/lefse_input_form_group.xls /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/hmp_aerobiosis_small.in -c 1 -o 1000000 -u 2
/public3/Pipeline/Amplicon/version6.0/lefse/run_lefse.py /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/hmp_aerobiosis_small.in /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/hmp_aerobiosis_small.res
/public2/Users/hongwj/software/anaconda/bin/python3.7 /public3/Pipeline/Amplicon/version6.0/lefse.top.py /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/hmp_aerobiosis_small.res /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/hmp_aerobiosis_small.res.top 10
/public3/Pipeline/Amplicon/version6.0/lefse/plot_cladogram.py /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/hmp_aerobiosis_small.res.top /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/Top.LEfSe.cladogram.png --format png --dpi 300 --labeled_start_lev 0 --labeled_stop_lev 7 --abrv_start_lev 3 --abrv_stop_lev 7
/public3/Pipeline/Amplicon/version6.0/lefse/plot_cladogram.py /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/hmp_aerobiosis_small.res.top /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/Top.LEfSe.cladogram.svg --format svg --labeled_start_lev 0 --labeled_stop_lev 7 --abrv_start_lev 3 --abrv_stop_lev 7
/public3/Pipeline/Amplicon/version6.0/lefse/plot_res.py /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/hmp_aerobiosis_small.res.top /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/Top.LEfSe.bar.png --dpi 300 
/public3/Pipeline/Amplicon/version6.0/lefse/plot_res.py  /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/hmp_aerobiosis_small.res.top /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/Top.LEfSe.bar.svg --format svg 
/public3/Pipeline/Amplicon/version6.0/lefse/plot_cladogram.py /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/hmp_aerobiosis_small.res /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/All.LEfSe.cladogram.png --format png --dpi 300 --labeled_start_lev 0 --labeled_stop_lev 7 --abrv_start_lev 3 --abrv_stop_lev 7
/public3/Pipeline/Amplicon/version6.0/lefse/plot_cladogram.py /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/hmp_aerobiosis_small.res /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/All.LEfSe.cladogram.svg --format svg --labeled_start_lev 0 --labeled_stop_lev 7 --abrv_start_lev 3 --abrv_stop_lev 7
/public3/Pipeline/Amplicon/version6.0/lefse/plot_res.py /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/hmp_aerobiosis_small.res /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/All.LEfSe.bar.png --dpi 300 
/public3/Pipeline/Amplicon/version6.0/lefse/plot_res.py  /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/hmp_aerobiosis_small.res /public2/Users/dengdy/cmDNA/new_group_file/Lefse_${i}/${o}/All.LEfSe.bar.svg --format svg

source /public/Biosoft/linuxbrew_SG/bin/deactivate.rc
#done



