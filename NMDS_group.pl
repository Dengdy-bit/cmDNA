#!usr/bin/perl
use strict ;
use Getopt::Long ;
my ($even_otu_table,$group,$outdir) ;
GetOptions(
	"even_otu_table:s"=>\$even_otu_table,
	"group:s" =>\$group,
	"outdir:s"=>\$outdir
);
usageall()if(!defined $even_otu_table||!defined $group||!defined $outdir);
sub usageall{
	die qq/
	perl NMDS.pl -even_otu_table <even_otu_table file> -group <group file> -outdir < outdir path>
\n/;
}
open R,">$outdir/NMDS.r" or die ;
`less $even_otu_table |sed  's/#//' |grep -v "Constructed">$outdir/otu_table_even.txt` ;
print R "library(vegan)\nlibrary(MASS)\nlibrary(ggplot2)\n";
print R "data = read.table(\"$outdir/otu_table_even.txt\", head=T, row.names=1,sep=\"\\t\",check.names=F)\n";
print R "groups = read.table(\"$group\",head=F,colClasses=c(\"character\",\"character\"))\n";
print R "len = length(groups[,1])\n" ;
print R "lenA = length(unique(groups[,2]))\n";
print R "data_new = data[,1:len]\n" ;
print R "data <- t(data_new)\n" ;
#print R "vare.dis <- vegdist(data, method=\"bray\");range(vare.dis)\n";
print R "set.seed(1234)\n" ;
print R "vare.dis <- metaMDS(data, trace=T);\n";
#print R "vare.mds0 <- cmdscale(vare.dis, eig=TRUE)\n";
print R "write.table(vare.dis\$stress, file=\"$outdir/NMDS_stress.txt\")\n";
print R "write.table(vare.dis\$points, file=\"$outdir/NMDS_scores.txt\")\n";
print R "NMDS1 = vare.dis\$points[,1]\n";
print R "NMDS2 = vare.dis\$points[,2]\n";
print R "colnames(groups)=c(\"sample\",\"group\")\n" ;
print R "plotdata1 = data.frame(names(NMDS1),NMDS1,NMDS2)\n";
print R "colnames(plotdata1) =c(\"sample\",\"NMDS1\",\"NMDS2\")\n" ;
print R "plotdata = merge(x=plotdata1,y=groups,by=\"sample\")\n" ;
#print R "colnames(plotdata)=c(\"sample\",\"NMDS1\",\"NMDS2\",\"group\")\n";
print R "plotdata\$sample = factor(plotdata\$sample)\n";
print R "plotdata\$NMDS1=as.numeric(as.vector(plotdata\$NMDS1))\n";
print R "plotdata\$NMDS2=as.numeric(as.vector(plotdata\$NMDS2))\n";

print R "data_colname = rownames(data)\n";
print R "mygroup=c()\n";
print R "for(name in data_colname){\n";
print R "	pos = which(groups\$sample==name)\n";
print R "	mygroup = c(mygroup,groups\$group[pos])\n";
print R "}\n";
print R "mygroup = factor(mygroup,levels=unique(mygroup))\n";
print R "mydata = data.frame(group=mygroup)\n";
print R "adonis2_result <- adonis2(data ~ group, data = mydata, permutations = 999, method=\"bray\")\n";

print R "myr=adonis2_result\$R2[1]\n";
print R "myp=adonis2_result\$\"Pr(>F)\"[1]\n";
print R "mytitle=paste(\"NMDS Plot pvalue:\",round(myp,4),sep=\" \")\n";
print R "mytitle1=paste(\"NMDS pvalue:\",round(myp,4),sep=\" \")\n";
print R "print(mytitle1)\n";
#print R "pdf(\"$outdir/NMDS.pdf\",10,9.6)\n";
print R "pdf(\"$outdir/NMDS.pdf\",13,9.6)\n";
print R "shape_type=c(15:25,14:0,33:137)[1:lenA]\n";
#print R "pc=100*vare.mds0\$eig/sum(vare.mds0\$eig)\n";
#print R "pc=sprintf(\"%.2f\",pc)\n";
#print R "pc1=pc[1]\n"; 
#print R "pc2=pc[2]\n";
#print R "cmp1 = paste(\"NMDS1\",pc1,sep=\"(\")\n";
#print R "cmp1 = paste(cmp1,\"%)\",sep=\"\")\n";
#print R "cmp2 = paste(\"NMDS2\",pc2,sep=\"(\")\n";
#print R "cmp2 = paste(cmp2,\"%)\",sep=\"\")\n";

#根据最左侧和最右侧的样本名称设定x轴的范围
my $rcode1.=<<"RCODE1";
min_x=min(plotdata[,2])
max_x=max(plotdata[,2])
offset=(max_x-min_x)*0.05
min_x_name=as.character(plotdata[,1][which(plotdata[,2]==min_x)])
max_x_name=as.character(plotdata[,1][which(plotdata[,2]==max_x)])
len1=length(unlist(strsplit(min_x_name,"")))
len2=length(unlist(strsplit(max_x_name,"")))
if(len1>8){
	len1=len1-8
}
if(len2>8){
	len2=len2-8
}
print(c(min_x_name,max_x_name))
print(c(len1,len2))
min_x = min_x-len1*offset
max_x=max_x+len2*offset
RCODE1
print R "$rcode1\n";

print R "P<-ggplot(plotdata, aes(NMDS1, NMDS2))\n";
print R "P<-P+geom_point(aes(colour=group,shape=group),size=7)+scale_shape_manual(values=shape_type)+scale_colour_manual(values=rainbow(lenA))\n";
print R "P<-P+geom_text(aes(label=sample),size=5,family=\"serif\",hjust=0.5,vjust=-1)\n";
print R "P<-P+labs(title=mytitle)+theme(title=element_text(size=15,color=\"black\"),axis.title=element_text(size=13,color=\"black\"),axis.text=element_text(size=10), panel.grid.major =element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = \"black\"),panel.background = element_rect(fill = \"transparent\",color = \"gray\"))\n" ;##added  by liangqian
print R "P2<-theme(plot.title = element_text(size=20,colour = \"black\",face = \"bold\"),axis.title.x = element_text(size=16,colour = \"black\",face = \"bold\"),axis.title.y =element_text(size=16,colour = \"black\",face = \"bold\") , axis.text.x= element_text(colour = \"black\",size=12),axis.text.y= element_text(colour = \"black\",size=12) , legend.text=element_text(size=12) , legend.title=element_text(size=12),axis.line = element_line(colour = \"black\",size =0.5 ))\n";
print R "P=P + P2+geom_vline(xintercept = 0,linetype='dotted')+geom_hline(yintercept = 0,linetype='dotted')+xlim(c(min_x,max_x))\n";
my $rcode2.=<<"RCODE2";
library(cowplot)
mytheme=theme_bw()+theme(legend.position="none",axis.text = element_blank(),
                         axis.ticks = element_blank(),axis.title = element_blank(),
                         panel.grid.major =element_blank(), panel.grid.minor = element_blank())
p2=ggplot(plotdata,aes(x=group,y=NMDS2,fill=group))+geom_boxplot(varwidth = TRUE)+scale_fill_manual(values=rainbow(lenA))+mytheme
p3=ggplot(plotdata,aes(x=group,y=NMDS1,fill=group))+geom_boxplot(varwidth = TRUE)+coord_flip()+scale_fill_manual(values=rainbow(lenA))+mytheme+ylim(c(min_x,max_x))
plot_grid(P,p2,p3,align = "vh",axis="tblr",rel_widths =c(2,1),rel_heights=c(2.5,1))
RCODE2
print R "$rcode2\n";
print R "dev.off()\n";
close R ;
