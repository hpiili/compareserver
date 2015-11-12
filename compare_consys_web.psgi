#/perl
use DBI;
use DBD::JDBC;
use Data::Dumper;
use Time::HiRes qw ( gettimeofday );
use CGI::PSGI;
use Plack::Request;
use Test::More;
use Test::Differences;


my $debug=0; 
my $dbh;
my $tc_dbh;

%enginetypes = (
'32F'=>'32F', 
'46F'=>'46F',
'20'=>'20',
'20L'=>'20L',
'22'=>'22',
'22/26'=>'22/26',
'26'=>'26',
'28'=>'28',
'31'=>'31',
'32'=>'32',
'32F'=>'32F',
'34'=>'34',
'38'=>'38',
'38A'=>'38A',
'46'=>'46',
'46F'=>'46F',
'50'=>'50',
'64'=>'64',
'98'=>'98',
'WA16'=>'WA16',
'WA20'=>'WA20',
'WA32'=>'WA32'
);

@engines=['20','20L','22','22/26','26','28','31','32','32F','34','38','38A','46','46F','50','64','98','WA16','WA20','WA32'];


my $app = sub {
	my $env = shift;
	my $q = CGI::PSGI->new($env);
	$q->autoEscape(undef);
	$q->charset('UTF-8');
	my $r = Plack::Request->new($env);
	my $p = $r->parameters;
	if ($env->{REQUEST_METHOD} eq "GET") {
		my $path = $env->{PATH_INFO};
		#print "path $path\n";
		# We need to always return 
		if ($path eq "/compare") {
			my %FORM = $r->uri->query_form();
			my $tcengine = $FORM{'tcengine'};
			my $wdmsengine = $FORM{'wdmsengine'};
			if (length($tcengine)<1) {
				$res = &send_form($q);
				return $res;
			} else {
				if (length($wdmsengine)<1) {
					$res = &send_form($q);
					return $res;
				} else {
					# there is currently no way of telling user that the oompare is in progress
					# javascript or http://www.stonehenge.com/merlyn/WebTechniques/col20.html
					$res = &compare_wdconsys($enginetype,$c,$q);
					return $res;
				}
			}
		} else {
			$res = &send_form($q);
			return $res;
		}
   } else {
		$res = &send_form($q);
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
                    -title => "Teamcenter-WDMS Engine EBOM Compare",
                    -encoding => 'utf-8',
                ),
                $cgi->p('Input two engine numbers type to compare'),
		$cgi->start_form("GET","compare","multipart/form-data"),
		$cgi->p('Engine type:'),
		$cgi->popup_menu(-name=>'enginetype',
					-values=>@engines),
		$cgi->submit(-name=>'Submit'),
		$cgi->end_form(),
                $cgi->end_html()
            ] ];
	return $res;
}


sub compare_wdconsys {
	my $enginetype = shift @_;
	my $c = shift;
	my $cgi = shift;
	
	print "Extracting data for enginetype $enginetype\n";
	
	# if these below options are 0, take the hash contents from previously stored hash
	my $runwdms=1; # if 0, read from wdconsys.hash
	my $runtc=1; # if 0, read from tcbom.hash

	if (length($enginetype)<1) {
		$c->send_status_line( "401", "Enginetype parameter is mandatory (32F, 46F, 34 ...)" );
	}

	# few engines are split by fuel type, others are all in one maxibom
	my @fueltypelist=("ALL");
	if ($enginetype eq "20") {
		@fueltypelist=("DE","DF");
	}
	if ($enginetype eq "34") {
		@fueltypelist=("SG","DF","LPG");
	}


	if ($runtc) { &connectDBTC(); }
	if ($runwdms) { &connectDBWDMS(); }

	# wdconsys contains only the structure, wdconsysdata contains also attributes
	my %wdconsys; my $wdconsys; my %wdconsysdata;
	# for debug purposes, we can skip wdms queries
	if ($runwdms) {
		
		my $query = &defineWDMSQueries();
		print "Query now WDMS\n" if $debug;
		
		# query everything from WDCONSYS where TYPECH=$enginetype
		$sth = $dbh->prepare($query);
		$sth->bind_param(1,$enginetype);
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
			$syscode=$h{SYSTEMCODE1}; $assgroup=$h{ASSGROUP}; $variant=$h{SUBCODE1};
			$desstage=$h{DESSTAGE}; $spec1=$h{SPEC1}; $spec2=$h{SPEC2}; $spec3=$h{SPEC3}; $spec4=$h{SPEC4}; $spec5=$h{SPEC5};
			$activity=$h{ACTIVITY};
			if (length($assgroup)<1) { $assgroup="NA"; }
			$syscodeshort=substr($syscode,0,3);
			$itemid=$h{ITEMID}; 
			
			# HarriP 18.8.2015 - earlier, we decided with Juhani Sahlberg that items OPEN where activity is not P or N are not added to MaxiBOM compare
			next if (($itemid eq "OPEN") && (($activity ne "P") && ($activity ne "N") && ($activity ne "T")));
			
			
			# Change this to same as TC logic
			#if (defined($wdconsys{$syscodeshort}{$assgroup}{$variant}{$itemid})) {
			#	$itemid=$itemid.";".int(rand(100));
			#}
			$itemidnew=$itemid;
			if (($syscodeshort ne $syscode) && (length($syscode)>1)) {
				$itemidnew=$itemid.";".$syscode;
			}
			
			# if there is already existing record with same itemid but with different spec1, spec2,spec3, spec4 and 5
			# create a distinct record in hash
			if (defined $wdconsysdata{$syscodeshort}{$assgroup}{$variant}{$itemid}) {
				$spec1_o=$wdconsysdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{SPEC1};
				$spec2_o=$wdconsysdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{SPEC2};
				$spec3_o=$wdconsysdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{SPEC3};
				$spec4_o=$wdconsysdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{SPEC4};
				$spec5_o=$wdconsysdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{SPEC5};
				$desstage_o=$wdconsysdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{DESSTAGE};
				$activity_o=$wdconsysdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{ACTIVITY};
				if (($spec1 ne $spec1_o) || ($spec2 ne $spec2_o) || ($spec3 ne $spec3_o) || 
					($spec4 ne $spec4_o) || ($spec5 ne $spec5_o) || ($desstage ne $desstage_o) || 
					($activity ne $activity_o)) {
					$itemidnew=$itemidnew.";1";
				}
			}
			
			if ($itemid ne $itemidnew) { $itemid=$itemidnew; }
			#if (($h4{SYSCODE} ne $syscodeshort) && (length($h4{SYSCODE})>1)) {
			#					$itemid=$itemid.";".$h4{SYSCODE};
			#				}
			$wdconsysdata{$syscodeshort}{$assgroup}{$variant}{$itemid}=\%h;
			print "$syscodeshort\n" if not defined($wdconsys{$syscodeshort});
			$wdconsys{$syscodeshort}{$assgroup}{$variant}{$itemid}=$activity;
			
		}

		store \%wdconsys, ".\\temp\\wdconsys_$enginetype.hash";
		$wdconsys=\%wdconsys;
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
		my $tctoplevelname="";  my %toplevel; my $spectype;

		foreach $fueltype (@fueltypelist) {
			my $spectype="%";
			if ($fueltype eq "DE") { $spectype="SPEC1"; }
			if ($fueltype eq "GD") { $spectype="SPEC2"; }
			if ($fueltype eq "SG") { $spectype="SPEC3"; }
			if ($fueltype eq "DF") { $spectype="SPEC4"; }
			if ($fueltype eq "LPG") { $spectype="SPEC5"; }

			# Check, if we have multiple maxiboms for this enginetype
			if ($enginetype eq "22/26") { $enginetype="2226"; }

			if (($enginetype ne "20") && ($enginetype ne "34")) {
				$tctoplevelname="MaxiBOM-$enginetype";
			} else {
				$tctoplevelname="MaxiBOM-$enginetype-$fueltype";
			}
			print "Query TC TOP assembly\n" if $debug;
			# find the top item for maxibom
			my ($topid, $toprev, $topitempuid, $toprevpuid) = &topLevel($tctoplevelname,$topcreate_date);
			$toplevel{item_id}=$topid; $toplevel{item_revision_id}=$toprev; $toplevel{itempuid}=$topitempuid; $toplevel{revpuid}=$toprevpuid;
			print "Query TC TOP assembly structure\n" if $debug;
			$start = Time::HiRes::time();
			# find first level bom - systemcodes
			$sth = $tc_dbh->prepare($find_item_level1_bom);
			$sth->bind_param(1,$topid);
			$sth->execute;
			my @fields = @{ $sth->{NAME_uc} };
			while ( my @row = $sth->fetchrow_array() ) {
				my %h; # temporary hash
				for $i (0..$#fields) {
					my $value=$row[$i]; chomp($value); $value = &trim($value);
					$h{$fields[$i]}=$value;
				}
				$syscodeshort=substr($h{NAME},0,3);
				print "$syscodeshort\n";
				#next if ($syscodeshort ne "1.0");
				# then find the assembly groups below systemcode
				$sth2 = $tc_dbh->prepare($find_item_level1_bom);
				$sth2->bind_param(1,$h{ITEMID});
				$sth2->execute;
				my @fields2 = @{ $sth2->{NAME_uc} };
				while ( my @row2 = $sth2->fetchrow_array() ) {
					my %h2; # temporary hash
					for $j (0..$#fields2) {
						my $value2=$row2[$j]; chomp($value2); $value2 = &trim($value2);
						$h2{$fields2[$j]}=$value2;
					}
					$assgroup=$h2{NAME};
					print "\t$assgroup\n";
					
					# then find the variants below assembly group
					$sth3 = $tc_dbh->prepare($find_item_level1_bom);
					$sth3->bind_param(1,$h2{ITEMID});
					$sth3->execute;
					my @fields3 = @{ $sth3->{NAME_uc} };
					while ( my @row3 = $sth3->fetchrow_array() ) {
						my %h3; # temporary hash
						for $k (0..$#fields3) {
							my $value3=$row3[$k]; chomp($value3); $value3 = &trim($value3);
							$h3{$fields3[$k]}=$value3;
						}
						$variant=$h3{NAME};
						
						print "\t\t$variant - activity $activity\n" if ($debug);
						my $itemactivity=""; my %activitycheck;
						# then find the variants below assembly group
						$sth4 = $tc_dbh->prepare($find_item_level1_bom);
						$sth4->bind_param(1,$h3{ITEMID});
						$sth4->execute;
						my @fields4 = @{ $sth4->{NAME_uc} };
						while ( my @row4 = $sth4->fetchrow_array() ) {
							my %h4; # temporary hash
							for $l (0..$#fields4) {
								my $value4=$row4[$l]; chomp($value4); $value4 = &trim($value4);
								$h4{$fields4[$l]}=$value4;
							}
							$itemid=$h4{ITEMID};
							#if (defined($tcbom{$syscodeshort}{$assgroup}{$variant}{$itemid})) {
							if (($h4{SYSCODE} ne $syscodeshort) && (length($h4{SYSCODE})>1)) {
								print "BOMLine Note :$h4{SYSCODE} vs short :$syscodeshort --> use special codes\n" if $debug;
								my ($syscode_t,$counter) = split(";", $h4{SYSCODE});
								if ($syscode_t eq $syscodeshort) {
									$itemid=$itemid.";".$counter;
								} else {
									$itemid=$itemid.";".$h4{SYSCODE};
								}
							}
							#print "PUID $h3{PUID}\tITEMID $h4{ITEMID}\n";
							# if this article combination already has activity P or N, we do not need to check it again. Reduced duplicate checks
							# unfortunately we can't trust other answers (O,T,R..) because there are lines (OPEN first in R and then OPEN in P)
							if (($activitycheck{$syscodeshort.$assgroup.$variant.$itemid} ne "P") && ($activitycheck{$syscodeshort.$assgroup.$variant.$itemid} ne "N")) {
								if (!defined $tcbomdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{ACTIVITY} 
									&& length($tcbomdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{ACTIVITY})<1 ) {
									$sth5=$tc_dbh->prepare($find_activity);
									$sth5->bind_param(1,$h3{PUID}); # variant item revision puid
									$sth5->bind_param(2,'%'.$h4{ITEMID}.'%'); # part of the wb8_WDCONSYS_ITEMID_1.0 form name
									$sth5->execute;
									
									while ( my @row5 = $sth5->fetchrow_array() ) {
										$itemactivity=$row5[0];
									}
									$activitycheck{$syscodeshort.$assgroup.$variant.$itemid}=$itemactivity;
									next if (($itemactivity ne "P") && ($itemactivity ne "N"));
									$tcbomdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{ACTIVITY}=$itemactivity;
									$tcbom{$syscodeshort}{$assgroup}{$variant}{$itemid}=$itemactivity;
								}
							}
							#}
							print "\t\t\t$itemid\n" if ($debug);
							# ask wdconsys attributes for $itemid
							$tcbomdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{ITEMID}=$h4{ITEMID};
							$tcbomdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{PUID}=$h4{PUID};
							$tcbomdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{NAME}=$h4{NAME};
							$tcbomdata{$syscodeshort}{$assgroup}{$variant}{$itemid}{VARIANTPUID}=$h3{PUID};
							
							
						}
					}
				}		
			}
			print "(" . (Time::HiRes::time() - $start) . " seconds)\n";


		} #foreach fueltypelist

		store \%tcbom, ".\\temp\\tcbom_$enginetype.hash";
		$tcbom = \%tcbom;
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

	use Test::More qw{ no_plan };
	use Test::Differences;	

	my $randnum = int(rand(1000));
	my $tfile=".\\temp\\wdconsys_compare_output_".$randnum;
	Test::More->builder->output ($tfile);
	Test::More->builder->failure_output ($tfile);
	eq_or_diff(\%tcbom, \%wdconsys, "Comparing TC (GOT) vs WDMS (EXPECTED) Enginetype $enginetype");
	 
	open(I,"<$tfile");
	my $ofile="/tmp/wdconsys_diff_TC_WDMS_".$enginetype.".htm";
	open(O,">",$ofile);

	print O q{
	<style>
	table#t01 th \{
	    color: white;
	    background-color: black;
	\}
	</style>
	};
	print O "<table id=\"t01\">\n";
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
		$htmloutput .= $_;
	}
	$htmloutput .= "</table>\n";
	close(O);
	close(I);

	
	open (O,"<",$ofile);
	while (<O>) {
		$htmloutput.=$_;
	}
	close(O);	
        unlink($ofile);
	unlink($tfile);
	
	#print "Original diff file : $tfile\n";

	my $res =
        	[ 200, [ 'Content-Type' => 'text/html' ],
                        [
                $cgi->start_html(
                    -title => "Teamcenter-WDMS Engine EBOM Compare",
                    -encoding => 'utf-8',
                ),
                $cgi->p($htmloutput),
                $cgi->end_html()
            ] ];

	END {
		$dbh->disconnect if defined($dbh);
		$tc_dbh->disconnect if defined($tc_dbh);
	}
	return $res;
}

sub topLevel {
	my ($object_name, $create_date_filter) = @_;

	$toplevel{name}=$object_name;
	my $itemid; my $revisionid; my $itempuid; my $revpuid; my $creation_date;
	print "\tCheck the TC database for object $object_name created after $create_date_filter\n";
		
	# Check TC Database, if we have this systemcode with creation date > X
	$tc_find_itemrev_by_name_sth = $tc_dbh->prepare($find_latest_itemrev_by_objectname);
	
	$tc_find_itemrev_by_name_sth->bind_param(1,$object_name);
	$tc_find_itemrev_by_name_sth->execute();
	
#	$tc_find_itemrev_by_name_sth->execute(($object_name,$create_date_filter));
	$tc_find_itemrev_by_name_sth->execute($object_name);
	while ( my @tc_row = $tc_find_itemrev_by_name_sth->fetchrow_array()) {
		$itemid=$tc_row[0];
		$revisionid=$tc_row[1];
		$creation_date=$tc_row[2];
		$itempuid=$tc_row[3];
		$revpuid=$tc_row[4];
		print "\tFOUND $itemid/$revisionid creation date $tc_row[2]\n";
	}
	return ($itemid, $revisionid, $itempuid, $revpuid);
}

sub defineWDMSQueries {
	# ##################### Define WDMS queries #####################
	$wdconsysquery = q{select * from WDCONSYS where WDCONSYS.TYPECH=? and (WDCONSYS.ACTIVITY in ('P','N'))};
	
	return $wdconsysquery;
	
}

sub defineTCQueries {
	# ##################### List Items created after Date #####################
	$find_latest_itemrev_by_creationdate = "use $ENV{'tcdatabase'}\;".q{
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
	
	$find_latest_itemrev_by_objectname = "use $ENV{'tcdatabase'}\;".q{
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

	$find_latest_itemrev_by_creationdate_and_desc = "use $ENV{'tcdatabase'}\;".q{
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
	
	$find_latest_itemrev_by_itemid = "use $ENV{'tcdatabase'}\;".q{
		select I.pitem_id,IR.pitem_revision_id,P.pcreation_date,I.puid,IR.puid,W.pobject_name,W.pobject_type from 
		PITEM I
		INNER JOIN PITEMREVISION IR on IR.ritems_tagu=I.puid
		INNER JOIN PWORKSPACEOBJECT W on IR.puid=W.puid
		INNER JOIN PPOM_APPLICATION_OBJECT P on P.puid=W.puid
		where 
			I.pitem_id=?
		};

	$find_itemrev_by_name_and_desc = "use $ENV{'tcdatabase'}\;".q{
		select I.pitem_id,IR.pitem_revision_id, P.pcreation_date,I.puid, IR.puid from 
		PITEM I
		INNER JOIN PITEMREVISION IR on IR.ritems_tagu=I.puid
		INNER JOIN PWORKSPACEOBJECT W on IR.puid=W.puid
		INNER JOIN PPOM_APPLICATION_OBJECT P on P.puid=W.puid
		where 
			upper(W.pobject_name)=upper(?)
			AND upper(W.pobject_desc)=upper(?)
		};
		
	$find_activity = "use $ENV{'tcdatabase'}\;".q{
	select pwb8_ACTIVITY from
		PWB8_WDCONSYSSTORAGE S
		JOIN PFORM F on F.rdata_fileu=S.puid
		JOIN PWORKSPACEOBJECT W on W.puid=F.puid
		JOIN PIMANRELATION R on R.rsecondary_objectu=F.puid
		JOIN PITEMREVISION I on I.puid=R.rprimary_objectu
	where I.ritems_tagu=?
		and W.pobject_name like ?
		and pwb8_ACTIVITY is not null
	};
	
	$find_item_level1_bom = "use $ENV{'tcdatabase'}\;".q{
		select 
			CI.pitem_id as ITEMID,CI.puid as PUID,WSO_CI.pobject_name as NAME,PNOTE_TEXTS.pval_0 as SYSCODE
		from 
		-- Top Material info
		PITEM PI
		JOIN PITEMREVISION PIR WITH (NOLOCK) ON PIR.ritems_tagu=PI.puid
		JOIN PWORKSPACEOBJECT WSO_PI WITH (NOLOCK) ON PI.puid=WSO_PI.puid
		-- BOM info
		JOIN PSTRUCTURE_REVISIONS WITH (NOLOCK) ON PIR.puid = PSTRUCTURE_REVISIONS.puid
		JOIN PPSBOMVIEWREVISION WITH (NOLOCK) ON PPSBOMVIEWREVISION.puid = PSTRUCTURE_REVISIONS.pvalu_0 AND PPSBOMVIEWREVISION.pis_precise = '0'
		JOIN PPSOCCURRENCE y WITH (NOLOCK) ON PPSBOMVIEWREVISION.puid = y.rparent_bvru
		JOIN PPSBOMVIEW WITH (NOLOCK) ON PPSBOMVIEW.puid=PPSBOMVIEWREVISION.rbom_viewu
		JOIN PPSVIEWTYPE WITH (NOLOCK) ON PPSBOMVIEW.rview_typeu=PPSVIEWTYPE.puid AND PPSVIEWTYPE.pname='view'
		LEFT OUTER JOIN PNOTE_TYPES WITH (NOLOCK) ON PNOTE_TYPES.puid = y.rnotes_refu AND PNOTE_TYPES.pvalu_0=(
		     select x.puid from PNOTETYPE x WITH (NOLOCK) where x.pname = 'Wb8_EBOMSystemCode'
		  )
		LEFT OUTER JOIN PNOTE_TEXTS WITH (NOLOCK) ON PNOTE_TEXTS.puid = y.rnotes_refu AND PNOTE_TYPES.pseq = PNOTE_TEXTS.pseq
		

		-- Child info
		JOIN PITEM CI WITH (NOLOCK) ON CI.puid = y.rchild_itemu
		JOIN PITEMREVISION CIR WITH (NOLOCK) ON CIR.ritems_tagu = CI.puid
		JOIN PWORKSPACEOBJECT WSO_CI WITH (NOLOCK) ON CI.puid=WSO_CI.puid


		WHERE
		  PI.pitem_id=?
		  --and PIR.pitem_revision_id=UPPER('%REVISION%')
		  --WSO_PI.pobject_name='MaxiBOM-32F'
		ORDER BY WSO_CI.pobject_name
	};
};

sub ltrim { my $s = shift; $s =~ s/^\s+|^\///;       return $s };
sub rtrim { my $s = shift; $s =~ s/\s+$//;       return $s };
sub  trim { my $s = shift; $s =~ s/^\s+|\s+$//g; return $s };





sub connectDBTC {
	# ##################### CONNECTIONS #####################
	my $tc_dsn;

	my %properties = ('user' => $ENV{'tcdbuser'},
			  'password' => $ENV{'tcdbpassword'},
			  'host.name' => $ENV{'tcdbhost'},
			  'host.port' => $ENV{'tcdbport'});

	die "Required parameter missing : tcdbhost" if length($ENV{'tcdbhost'})<1;
	die "Required parameter missing : tcdbport" if length($ENV{'tcdbport'})<1;
	die "Required parameter missing : tcdatabase" if length($ENV{'tcdatabase'})<1;
	die "Required parameter missing : tcdbuser" if length($ENV{'tcdbuser'})<1;
	die "Required parameter missing : tcdbpassword" if length($ENV{'tcdbpassword'})<1;
	print "Connecting to database $ENV{'tcdatabase'}\n";

	$tc_dsn = "DBI:JDBC:hostname=tcdbproxy:9002;url=jdbc:sqlserver://$ENV{'tcdbhost'}:$ENV{'tcdbport'};databaseName=$ENV{'tcdatabase'}";

	$tc_dbh = DBI->connect($tc_dsn, undef, undef, 
			  { PrintError => 0, 
				RaiseError => 1,
				AutoCommit => 0,
				jdbc_properties => \%properties })
			  or die "Failed to connect: ($DBI::err) $DBI::errstr\n";
}

sub connectDBWDMS {
	# ##################### CONNECTIONS #####################
	my %properties = ('user' => $ENV{'wdmsdbuser'},
			  'password' => $ENV{'wdmsdbpassword'},
			  'host.name' => $ENV{'wdmsdbhost'},
			  'host.port' => $ENV{'wdmsdbport'});

	die "Required parameter missing : wdmsdbuser" if length($properties{'user'})<1;
	die "Required parameter missing : wdmsdbpassword" if length($properties{'password'})<1;
	die "Required parameter missing : wdmsdbhost" if length($properties{'host.name'})<1;
	die "Required parameter missing : wdmsdbport" if length($properties{'host.port'})<1;

	my $dsn = "DBI:JDBC:hostname=wdmsdbproxy:9001;url=jdbc:oracle:thin:\@$ENV{'wdmsdbhost'}:$ENV{'wdmsdbport'}:$ENV{'wdmsdatabase'}";
	$dbh = DBI->connect($dsn, undef, undef, 
			  { PrintError => 0, 
			    RaiseError => 1,
				AutoCommit => 0,
				RowCacheSize=>16, 
				ora_envhp=> 0,
			    jdbc_properties => \%properties })
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
    push @array, $1 while $dump =~ m/^($indent(\S).*?)(?=^$indent(?:\2+|\d)|\z)/msg;
    $_ = sort_dumper($_,$numeric,$array) for @array;
    # add trailing comma to last element
    $array[-1] =~ s/\n$/,\n/;
    # sort our key value chunks, numeric or alpha depending
    # choose automatically if specified and use the Schzartzian 
    # transform for efficiency on numeric sort to get digits
    $numeric = $array[0] =~ m/^\s*'?\d/ if $numeric eq 'auto';
    @array = $numeric ? map{$_->[0]}sort{$a->[1]<=>$b->[1]}map{[$_,num+($_)]}@array :
                        sort @array;
    sub num { $_[0] =~ m/(-?\d+)/; $1 || 0 }
    # remove trailing comma from (new) last element
    $array[-1] =~ s/,\n$/\n/;
  return join'', $start, @array, $close;
}