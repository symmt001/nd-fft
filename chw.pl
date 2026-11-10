#!/usr/bin/perl
# -*- mode: Perl -*-
opendir(DIR,'.');

$OCACHE=128;
$NCACHE=64;

$word="(.*WSIZE=)$OCACHE(\\*1024/16.*)";

foreach(readdir(DIR))
{
	$src=$_;
	next unless $src =~ /^nd.*\.f$/;

# Read source
	open(IN,"$src")  or die "Can't open $src\n";

	while(<IN>){
		if(/$word/){
			print "$src:$1$NCACHE$2\n";
			push(@flist,$src);
		}
	};
	close(IN,"$src");
}
foreach(@flist){
	$fbak="${_}.bak";
	#print "$fbak\n";
	unlink $fbak;
	rename $_ , $fbak;
	open(IN,"$fbak");
	open(OUT,">$_");
	while(<IN>){
		s/$word/$1$NCACHE$2/;
		print OUT;
	};
	
}
