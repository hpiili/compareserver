#/perl
use DBI;
use DBD::Proxy;
use Data::Dumper;
use Time::HiRes qw ( gettimeofday );
use CGI::PSGI;
use Plack::Request;
use Test::More;
use Test::Differences;

my $debug=0; 
my $dbh;
my $tc_dbh;

my $app = sub {
	my $env = shift;
	my $q = CGI::PSGI->new($env);
	$q->autoEscape(undef);
	$q->charset('UTF-8');
	my $r = Plack::Request->new($env);
	my $p = $r->parameters;
	if ($env->{REQUEST_METHOD} eq "GET") {
		my $path = $env->{PATH_INFO};
		print "path $path\n";
		# We need to always return 
		if ($path eq "/compare") {
			my %FORM = $r->uri->query_form();
			my $tcengine = $FORM{'tcengine'};
			my $wdmsengine = $FORM{'wdmsengine'};
			if (length($tcengine)<1) {
				$res = &send_form();
				return $res;
			} else {
				if (length($wdmsengine)<1) {
					$res = &send_form();
					return $res;
				} else {
					# there is currently no way of telling user that the oompare is in progress
					# javascript or http://www.stonehenge.com/merlyn/WebTechniques/col20.html
					&compare_ebom($tcengine,$wdmsengine,$c);
				}
			}
		} else {
			$res = &send_form($q);
			return $res;
		}
   } else {
		$res = &send_form();
		return $res;
	}
};

	

sub send_form {
	my $cgi = shift;
	#return [ 
	#	$q->psgi_header('text/plain'),
	#	[ "Hello ", $q->param('tcengine') ],
	#				];

	my $res = 
	[ 200, [ 'Content-Type' => 'text/html' ], 
			[ 
                $cgi->start_html(
                    -title => HOSTNAME,
                    -encoding => 'utf-8',
                ),
                $cgi->p('Input two engine numbers type to compare'),
		$cgi->start_form("GET","compare","multipart/form-data"),
		$cgi->p('Teamcenter EBOM Number:'),
		$cgi->textfield('tcengine','',50,128),
		$cgi->p('WDMS EBOM Number:'),
		$cgi->textfield('wdmsengine','',50,15),
		$cgi->submit(-name=>'Submit'),
		$cgi->end_form(),
                $cgi->end_html()
            ] ];
	return $res;
}


sub compare_ebom {
	my $tcengine = shift;
	my $wdmsengine = shift;
	
	# wdengmod contains only the structure, wdengmoddata contains also attributes
	my %wdeng; my %wdengmod; my $wdengmod; my %wdengmoddata; my $issue;
	
	# if these below options are 0, take the hash contents from previously stored hash
	my $runwdms=1; # if 0, read from wdconsys.hash
	my $runtc=1; # if 0, read from tcbom.hash




	if ($runtc) { &connectDBTC(); }
	if ($runwdms) { &connectDBWDMS(); }

	# wdconsys contains only the structure, wdconsysdata contains also attributes
	my %wdconsys; my $wdconsys; my %wdconsysdata;
	# for debug purposes, we can skip wdms queries
	if ($runwdms) {
		
		&defineWDMSQueries();
		
		# Find first the latest issue of the Engine number
		# select max(ISSUE) from WDENG where ENGNR = 'PAAE267940TC'
		$sth = $dbh->prepare($wdengmaxissuequery);
		$sth->bind_param(1,$wdmsengine);
		$sth->execute;
		while ( my @row = $sth->fetchrow_array() ) {
			chomp(@row);
			$issue = $row[0];
		}
		
		# query everything from WDENGMOD where ENGNR=$wdmsengine and ISSUE=max()
		$sth = $dbh->prepare($wdengmodquery);
		$sth->bind_param(1,$wdmsengine);
		$sth->bind_param(2,$issue);
		$sth->execute;
		my @fields = @{ $sth->{NAME_uc} };
		while ( my @row = $sth->fetchrow_array() ) {
			chomp(@row);
			my %h; # temporary hash
			for $i (0..$#fields) {
				my $value=$row[$i]; chomp($value); $value = &trim($value);
				$h{$fields[$i]}=$value;
			}
			# fix the assembly group value logic
			$syscode=$h{SYSTEMCODE}; $subcode=$h{SUBCODE}; 
			$syscodeshort=substr($syscode,0,3);
			
			$itemid=$h{MODITEMID}; $modissue = $h{MODISSUE};
			$itemkey=$subcode."_".$itemid;
			
			
			# if there is already existing record with same itemid but with different spec1, spec2,spec3, spec4 and 5
			# create a distinct record in hash
			if (defined $wdengmoddata{$syscodeshort}{$itemkey}) {
				$subcode_o=$wdengmoddata{$syscodeshort}{$itemkey}{SUBCODE};
				$systemcode_o=$wdengmoddata{$syscodeshort}{$itemkey}{SYSTEMCODE};
				$modissue_o=$wdengmoddata{$syscodeshort}{$itemkey}{MODISSUE};
				if (($subcode_o ne $subcode) || ($systemcode_o ne $systemcode) || ($modissue_o ne $modissue)) {
					$itemidnew=$itemidnew.";1";
					$itemkey=$itemidnew."_".$subcode;
				}
			}
			
			if ((defined $itemidnew) && ($itemid ne $itemidnew)) { $itemid=$itemidnew; }
			#if (($h4{SYSCODE} ne $syscodeshort) && (length($h4{SYSCODE})>1)) {
			#					$itemid=$itemid.";".$h4{SYSCODE};
			#				}
			$wdengmoddata{$syscodeshort}{$itemkey}=\%h;
			$wdengmod{$syscodeshort}{$itemkey}=1;
		}

		$wdengmod{'00_ENGNR'}=$wdmsengine;
		#store \%wdengmod, ".\\temp\\wdeng_$wdmsengine.hash";
		#$wdengmod=\%wdengmod;
		
		
	} else {
		#$wdconsys = retrieve(".\\temp\\wdconsys_$enginetype.hash");
	}

	# Dump the contents
	if ($debug) {
		  local $Data::Dumper::Terse = 1;
		  local $Data::Dumper::Indent = 1;
		  local $Data::Dumper::Useqq = 1;
		  local $Data::Dumper::Deparse = 1;
		  local $Data::Dumper::Quotekeys = 0;
		  local $Data::Dumper::Sortkeys = 1;
		  warn Dumper($wdconsys);
	}

	&defineTCQueries();

	# tcbom only contains the structure, tcbomdata also contains the attributes
	my %tcbom; my $tcbom; my %tcbomdata;

	if ($runtc) {
		# find the top item for maxibom
		my $topitem;
		$sth = $tc_dbh->prepare($find_latest_itemrev_by_itemid);
		$sth->bind_param(1,$tcengine);
		$sth->execute;
		my @fields = @{ $sth->{NAME_uc} };
		while ( my @row = $sth->fetchrow_array() ) {
			my %h; #temporary hash
			for $i (0..$#fields) {
				my $value=$row[$i]; chomp($value); $value = &trim($value);
				$h{$fields[$i]}=$value;
			}
			$topitem=$h{PITEM_ID};
		}
		die "The top level item $tcengine was not found in Teamcenter $tcenv\n" if not defined($topitem);
		
		$start = Time::HiRes::time();
		# find first level bom - systemcodes
		$sth = $tc_dbh->prepare($find_item_level1_bom);
		$sth->bind_param(1,$tcengine);
		$sth->execute;
		my @fields = @{ $sth->{NAME_uc} };
		while ( my @row = $sth->fetchrow_array() ) {
			my %h; # temporary hash
			for $i (0..$#fields) {
				my $value=$row[$i]; chomp($value); $value = &trim($value);
				$h{$fields[$i]}=$value;
			}
			$syscodeshort=substr($h{NAME},0,3);
			print "$syscodeshort\n" if ($debug);
			#next if ($syscodeshort ne "1.0");
			# then find the modules below systemcode
			$sth2 = $tc_dbh->prepare($find_item_level1_bom);
			$sth2->bind_param(1,$h{ITEMID});
			$sth2->execute;
			my @fields2 = @{ $sth2->{NAME_uc} }; my $variant;
			while ( my @row2 = $sth2->fetchrow_array() ) {
				my %h2; # temporary hash
				for $j (0..$#fields2) {
					my $value2=$row2[$j]; chomp($value2); $value2 = &trim($value2);
					$h2{$fields2[$j]}=$value2;
				}
				$itemid=$h2{ITEMID};
				$variant=$h2{VARIANT};
				($variant,$rest) = split("/",$variant);
				$itemkey=$variant."_".$itemid;
				
				print "\t$itemkey\n" if ($debug);
					
				if (($h2{SYSCODE} ne $syscodeshort) && (length($h2{SYSCODE})>1)) {
					print "BOMLine Note :$h2{SYSCODE} vs short :$syscodeshort --> use special codes\n";
					my ($syscode_t,$counter) = split(";", $h2{SYSCODE});
					if ($syscode_t eq $syscodeshort) {
						$itemid=$itemid.";".$counter;
					} else {
						$itemid=$itemid.";".$h2{SYSCODE};
					}
				}
				#}
				print "\t\t\t$itemid\n" if ($debug);
						# ask wdengmod attributes for $itemid
				$tcbomdata{$syscodeshort}{$itemkey}{ITEMID}=$h2{ITEMID};
				$tcbomdata{$syscodeshort}{$itemkey}{PUID}=$h2{PUID};
				$tcbomdata{$syscodeshort}{$itemkey}{NAME}=$h2{NAME};
				$tcbomdata{$syscodeshort}{$itemkey}{VARIANTPUID}=$h2{PUID};
				$tcbom{$syscodeshort}{$itemkey}=1;
					
			}
		}
		print "(" . (Time::HiRes::time() - $start) . " seconds)\n";

		$tcbom{'00_ENGNR'}=$tcengine;
		#store \%tcbom, ".\\temp\\tcbom_$tcengine.hash";
		#$tcbom = \%tcbom;
	} else {
		#$tcbom = retrieve(".\\temp\\tcbom_$enginetype.hash");
	}

	# Dump the contents
	if ($debug) {
	  local $Data::Dumper::Terse = 1;
	  local $Data::Dumper::Indent = 1;
	  local $Data::Dumper::Useqq = 1;
	  local $Data::Dumper::Deparse = 1;
	  local $Data::Dumper::Quotekeys = 0;
	  local $Data::Dumper::Sortkeys = 1;
	  warn Dumper($tcbom);
	}

	
	# compare hashes
	#use Test::Deep tests=>1;

	use Test::More qw{ no_plan };
	#is_deeply($tcbom, $wdengmod, 'data structures should be the same');
	use Test::Differences;	
	eq_or_diff_data($tcbom, $wdengmod, "Comparing $tcengine (GOT) vs $wdmsengine (EXPECTED)");
	print "Then output the same to File\n";

	#my $tfile=".\\temp\\ebom_compare_output_".int(rand(1000));
	my $testoutput = "";
	my $htmloutput = "";

	Test::More->builder->output (\$testoutput);
	Test::More->builder->failure_output (\$testoutput);
	eq_or_diff($tcbom, $wdengmod, "Comparing $tcengine (GOT) vs $wdmsengine (EXPECTED)");

	#open(I,"<$tfile");
	#open(O,">.\\temp\\ebom_diff_TC_".$tcengine."_WDMS_".$wdmsengine.".htm");

	$htmloutput.= "<html>";
	$htmloutput.= q{
	<style>
	table#t01 tr:nth-child(even) \{
		background-color: #eee;
	\}
	table#t01 th \{
		color: white;
		background-color: black;
	\}
	</style>
	};
	$htmloutput.= "<table id=\"t01\">\n";
	while (<I>) {
		$_=~s/# \+/<td><td>/g;
		$_=~s/-\+-/-<\/td><td>-/g;
		$_=~s/-\+/<\/td><\/tr>/g;
		$_=~s/# \|/<tr><td>/g;
		$_=~s/# \*/<tr bgcolor="red"><td>/g;
		$_=~s/\|\r\n/<\/td><\/tr>\r\n/g;
		$_=~s/\*\r\n/<\/td><\/tr>\r\n/g;
		$_=~s/\|/<\/td><td>/g;
		$_=~s/\*/<\/td><td>/g;
		$_=~s/     //g;
		$_=~s/    /&nbsp;&nbsp;&nbsp;&nbsp;/g;
		$_=~s/   /&nbsp;&nbsp;&nbsp;/g;
		$_=~s/  /&nbsp;&nbsp;/g;
		$_=~s/----//g;
		$_=~s/---//g;
		$_=~s/'//g;
		$_=~s/ => 1,//g;
		$_=~s/ => 1//g;
		$_=~s/Failed test//g;
		$_=~s/at compare_ebom.pl line (\d*)//g;
		$htmloutput.= $_;
	}
	$htmloutput.= "</table></html>\n";
	#close(O);
	#close(I);

	
	_http_response($c, { content_type => 'text/html' },
                start_html(
                    -title => HOSTNAME,
                    -encoding => 'utf-8',
                ),
                p($htmloutput),
                end_html(),
            );

	
	END {
		$dbh->disconnect if defined($dbh);
		$tc_dbh->disconnect if defined($tc_dbh);
	}

}



sub defineWDMSQueries {
	# ##################### Define WDMS queries #####################
	$wdengmaxissuequery = q{select max(ISSUE) from WDENG where ENGNR = ?};
	$wdengmodquery = q{select * from WDENGMOD where ENGNR=? and ISSUE=?};
}
	

sub defineTCQueries {
	# ##################### List Items created after Date #####################
	$find_latest_itemrev_by_creationdate = q{
		select I.pitem_id,IR.pitem_revision_id, P.pcreation_date, I.puid, IR.puid from 
		PITEM I
		INNER JOIN PITEMREVISION IR on IR.ritems_tagu=I.puid
		INNER JOIN PWORKSPACEOBJECT W on IR.puid=W.puid
		INNER JOIN PPOM_APPLICATION_OBJECT P on P.puid=W.puid
		where 
			upper(W.pobject_name)=upper(?)
			AND P.pcreation_date IN
			(
				select max(pcreation_date) from PPOM_APPLICATION_OBJECT xa 
				where xa.puid IN(select xb.puid from PITEMREVISION xb where xb.ritems_tagu=I.puid)
			)
			AND P.pcreation_date > ? AND P.pcreation_date < dateadd(day, 2, ?)
		};
	
	$find_latest_itemrev_by_objectname = q{
		select I.pitem_id,IR.pitem_revision_id, P.pcreation_date, I.puid, IR.puid from 
		PITEM I
		INNER JOIN PITEMREVISION IR on IR.ritems_tagu=I.puid
		INNER JOIN PWORKSPACEOBJECT W on IR.puid=W.puid
		INNER JOIN PPOM_APPLICATION_OBJECT P on P.puid=W.puid
		where 
			upper(W.pobject_name)=upper(?)
			AND P.pcreation_date IN
			(
				select max(pcreation_date) from PPOM_APPLICATION_OBJECT xa 
				where xa.puid IN(select xb.puid from PITEMREVISION xb where xb.ritems_tagu=I.puid)
			)
		};

	$find_latest_itemrev_by_creationdate_and_desc = q{
		select I.pitem_id,IR.pitem_revision_id, P.pcreation_date, I.puid, IR.puid from 
		PITEM I
		INNER JOIN PITEMREVISION IR on IR.ritems_tagu=I.puid
		INNER JOIN PWORKSPACEOBJECT W on IR.puid=W.puid
		INNER JOIN PPOM_APPLICATION_OBJECT P on P.puid=W.puid
		where 
			upper(W.pobject_name)=upper(?)
			AND P.pcreation_date IN
			(
				select max(pcreation_date) from PPOM_APPLICATION_OBJECT xa 
				where xa.puid IN(select xb.puid from PITEMREVISION xb where xb.ritems_tagu=I.puid)
			)
			AND P.pcreation_date > ? AND P.pcreation_date < dateadd(day, 2, ?)
			AND upper(W.pobject_desc)=upper(?)
		};
	
	$find_latest_itemrev_by_itemid = q{
		select I.pitem_id,IR.pitem_revision_id,P.pcreation_date,I.puid,IR.puid,W.pobject_name,W.pobject_type from 
		PITEM I
		INNER JOIN PITEMREVISION IR on IR.ritems_tagu=I.puid
		INNER JOIN PWORKSPACEOBJECT W on IR.puid=W.puid
		INNER JOIN PPOM_APPLICATION_OBJECT P on P.puid=W.puid
		where 
			I.pitem_id=?
		};

	$find_itemrev_by_name_and_desc = q{
		select I.pitem_id,IR.pitem_revision_id, P.pcreation_date,I.puid, IR.puid from 
		PITEM I
		INNER JOIN PITEMREVISION IR on IR.ritems_tagu=I.puid
		INNER JOIN PWORKSPACEOBJECT W on IR.puid=W.puid
		INNER JOIN PPOM_APPLICATION_OBJECT P on P.puid=W.puid
		where 
			upper(W.pobject_name)=upper(?)
			AND upper(W.pobject_desc)=upper(?)
		};
	
	# note that EBOM structure is precise structure (the sql is not the same as in wdconsys migration
	$find_item_level1_bom = q{
		select 
			CI.pitem_id as ITEMID,CI.puid as PUID,WSO_CI.pobject_name as NAME,t1.pval_0 as SYSCODE,t2.pval_0 as VARIANT
		from 
		-- Top Material info
		PITEM PI
		JOIN PITEMREVISION PIR WITH (NOLOCK) ON PIR.ritems_tagu=PI.puid
		JOIN PWORKSPACEOBJECT WSO_PI WITH (NOLOCK) ON PI.puid=WSO_PI.puid
		-- BOM info
		JOIN PSTRUCTURE_REVISIONS WITH (NOLOCK) ON PIR.puid = PSTRUCTURE_REVISIONS.puid
		JOIN PPSBOMVIEWREVISION WITH (NOLOCK) ON PPSBOMVIEWREVISION.puid = PSTRUCTURE_REVISIONS.pvalu_0 AND PPSBOMVIEWREVISION.pis_precise = '1'
		JOIN PPSOCCURRENCE y WITH (NOLOCK) ON PPSBOMVIEWREVISION.puid = y.rparent_bvru
		JOIN PPSBOMVIEW WITH (NOLOCK) ON PPSBOMVIEW.puid=PPSBOMVIEWREVISION.rbom_viewu
		JOIN PPSVIEWTYPE WITH (NOLOCK) ON PPSBOMVIEW.rview_typeu=PPSVIEWTYPE.puid AND PPSVIEWTYPE.pname='view'
		LEFT OUTER JOIN PNOTE_TYPES nt1 WITH (NOLOCK) ON nt1.puid = y.rnotes_refu AND nt1.pvalu_0=(
		     select x.puid from PNOTETYPE x WITH (NOLOCK) where x.pname = 'Wb8_EBOMSystemCode'
		  )
		LEFT OUTER JOIN PNOTE_TEXTS t1 WITH (NOLOCK) ON t1.puid = y.rnotes_refu AND nt1.pseq = t1.pseq
		LEFT OUTER JOIN PNOTE_TYPES nt2 WITH (NOLOCK) ON nt2.puid = y.rnotes_refu AND nt2.pvalu_0=(
		     select x2.puid from PNOTETYPE x2 WITH (NOLOCK) where x2.pname = 'Configuration Remark'
		  )
		LEFT OUTER JOIN PNOTE_TEXTS t2 WITH (NOLOCK) ON t2.puid = y.rnotes_refu AND nt2.pseq = t2.pseq

		-- Child info
		JOIN PITEMREVISION CIR_PRECISE ON CIR_PRECISE.puid = y.rchild_itemu
  		JOIN PITEM CI WITH (NOLOCK) ON CI.puid = CIR_PRECISE.ritems_tagu
		JOIN PITEMREVISION CIR WITH (NOLOCK) ON CIR.ritems_tagu = CI.puid
		JOIN PWORKSPACEOBJECT WSO_CI WITH (NOLOCK) ON CI.puid=WSO_CI.puid


		WHERE
		  PI.pitem_id=?
		ORDER BY NAME
	};
};


sub ltrim { my $s = shift; $s =~ s/^\s+|^\///;       return $s };
sub rtrim { my $s = shift; $s =~ s/\s+$//;       return $s };
sub  trim { my $s = shift; $s =~ s/^\s+|\s+$//g; return $s };





sub connectDBTC {
	# ##################### CONNECTIONS #####################
	my $tc_dsn;
	$tc_dsn = "DBI:Proxy:hostname=tcdbproxy:9002;url=jdbc:sqlserver://$ENV{'tcdbhost'}:$ENV{'tcdbport'};databaseName=$ENV{'tcdatabase'}";

	$tc_dbh = DBI->connect($tc_dsn, $ENV{'tcdbuser'}, $ENV{'tcdbpassword'}, 
			  { PrintError => 1, 
				RaiseError => 1,
				AutoCommit => 0 })
			  or die "Failed to connect: ($DBI::err) $DBI::errstr\n";
}

sub connectDBWDMS {
	# ##################### CONNECTIONS #####################
	my $dsn = "DBI:Proxy:hostname=wdmsdbproxy:9001;url=jdbc:oracle:thin:\@$ENV{'wdmsdbhost'}:$ENV{'wdmsdbport'}:$ENV{'wdmsdatabase'}";
	$dbh = DBI->connect($dsn, $ENV{'wdmsdbuser'}, $ENV{'wdmsdbpassword'}, 
			  { PrintError => 0, 
				RaiseError => 1, 
				AutoCommit => 0 })
			  or die "Failed to connect: ($DBI::err) $DBI::errstr\n";

}



# this sub will return sorted standard Data::Dumper output. 
# an alphabetic sort is the default setting, example:
# sort_dumper( Dumper(\%hash) );
# numeric sort if called with a true second argument, example: 
# sort_dumper( Dumper(\%hash), 1 );
# an automatic sort choice based on first char in first element
# is done if the second param is specified as 'auto', example: 
# sort_dumper( Dumper(\%hash), 'auto' );
# arrays will be sorted if you pass a true third param
# example (alpha sort, including sorting array elements):
# sort_dumper( Dumper(\%hash), 0, 1 );
# to sort everything automatically you call it:
# sort_dumper( Dumper(\%hash), 'auto', 1 );

sub sort_dumper {
    my ($dump, $numeric, $array) = @_;
    my %pairs = $array ? ('['=>']', '{'=>'}') : ('{'=>'}');
    # get opening details
    my ($start, $open, $indent) = $dump =~ m/^(.+(.)\n)(\s*)\S/;
  return $dump unless $open and exists $pairs{$open};
    # get closing details
    my ($close) = $dump =~ m/^(\s*$pairs{$open}.?\n)\z/m;
    # get the key value assignments into an array
    $dump =~ s/^\Q$start\E|\Q$close\E\z//g;
    my @array;
    push @array, $1 while $dump =~ m/^($indent(\S).*?)(?=^$indent(?:\2
+|\d)|\z)/msg;
    $_ = sort_dumper($_,$numeric,$array) for @array;
    # add trailing comma to last element
    $array[-1] =~ s/\n$/,\n/;
    # sort our key value chunks, numeric or alpha depending
    # choose automatically if specified and use the Schzartzian 
    # transform for efficiency on numeric sort to get digits
    $numeric = $array[0] =~ m/^\s*'?\d/ if $numeric eq 'auto';
    @array = $numeric ? map{$_->[0]}sort{$a->[1]<=>$b->[1]}map{[$_,num
+($_)]}@array :
                        sort @array;
    sub num { $_[0] =~ m/(-?\d+)/; $1 || 0 }
    # remove trailing comma from (new) last element
    $array[-1] =~ s/,\n$/\n/;
  return join'', $start, @array, $close;
}
