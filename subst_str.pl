#!/opt/perl/bin/perl
# -*- mode: Perl -*-
opendir(DIR,'.');
$search_str ="iunroll";
$replace_str="nunroll";

foreach(readdir(DIR))
{
	$src=$_;
	$dst=$_;
	next unless $src =~ /\.f$/;

	$dst =~ s/\.f$/.new.f/;
	open(IN,"$src");
	$f="0";
	while(<IN>){
		next unless /$search_str/;
		$f="1";
		last;
	}
	close(IN,"$src");
	next if $f == "0";
	open(IN,"$src");
	open(OUT,">$dst");
	subst_str();
	close(OUT);
	close(IN);
	print "$src\n";
	unlink("$src~");
	rename("$src","$src~");
	rename("$dst","$src");
}

sub subst_str
{
	while(<IN>)
	{
		s/$search_str/$replace_str/;
		print OUT;
	}
}
