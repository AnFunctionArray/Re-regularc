#!/usr/bin/perl

use re 'eval';

BEGIN{push @INC, "./misc"};
BEGIN{push @INC, "./regexes/supplement"};

require "typename.regex.pl";

#use re qw(Debug ALL);

use List::Util qw(max);

use experimental 'switch';

use File::Basename;

use Data::Dumper;

$oldfh = select(STDERR);

my $inpar = qr{(?<inpar>\((?<inner>(([^][()\\]|\\.)++|(?&inpar)
                        |(?<insquare>\[\^?+(.|\\.)(([^]\\]|\\.)++
                        |(?&insquare))*\]))*)\))}xxs;

$filename = $ARGV[0];
open my $fh, '<', $filename or die "error opening $filename: $!";

my $subject = do { local $/; <$fh> };

close $fh;

$typedef_regex = qr{(*F)}sxxn;

sub inc2 {

    foreach my $name (@_) {
        "setting $name to -> " . ($$name = ++$$name) . "\n";
    }
}

sub dec2 {

    foreach my $name (@_) {

        "setting $name to -> " . ($$name = --$$name) . "\n";
    }
}

sub set2 {
    #print Dumper(@_);
    foreach my $pair (@_) {
        my ($key, $value) = %$pair;
        #print Dumper($pair);
        push @$key, $value;
        #print "pushing $value to $key\n"
    }
}

sub unset2 {

    foreach my $bulk (@_) {
        #print "popping $bulk\n";
        pop @$bulk;
    }
}

=begin
sub common {
    my @currvalsaltarr = ();
    my $origvars = join(',', @{map { $_ }, @_});

    foreach (@_) {
        if(exists $scopes{$_}) #if belong to a scope 
        {
            my @currstate = map { $_ } @{$scopes{$_}};

            push @currvalsaltarr, @currstate;
        } else {
            push @currvalsaltarr, [$_]
        }
    }
    my $currvalsalt = join(',', @{map { join(',', @{map { $$_ + 0 }, @_}) }, @currvalsaltarr});

    return $currvalsalt
}
=cut

sub inc {
    my $vars = join(',', @_);
    return "((?{inc2 $vars})|(?{dec2 $vars}))"
}

sub set {
    $Data::Dumper::Terse = 1;
    my $vars = join(',', map {Dumper($_)} @_);
    my $unset = join(',', map {(keys %$_)[0]} @_);

    #print $vars . "\n";
    $Data::Dumper::Terse = 0;
    return "((?{set2 $vars})|(?{unset2 $unset}))"
}

sub unset {
    $Data::Dumper::Terse = 1;
    my $vars = join(',', map {Dumper($_)} @_);
    my $unset = join(',', map {(keys %$_)[0]} @_);

    #print $vars . "\n";
    $Data::Dumper::Terse = 0;
    return "((?{unset2 $unset})|(?{set2 $vars}))"
}

sub dec {
    my $vars = join(',', @_);
    return "((?{dec2 $vars})|(?{inc2 $vars}))"
}

#$filename = "output.txt";
#open fhoutput, '>', $filename or die "error opening $filename: $!";

#select fhoutput;

#sub loadregex {
my $isnested = $_[0];

$filename = "regexes/primexpr.regex";
open my $fh, '<', $filename or die "error opening $filename: $!";

my $mainregexfilecontent = do { local $/; <$fh> };

close $fh;

$filename = "./utility/regex.regex";
open my $fh, '<', $filename or die "error opening $filename: $!";

my $metaregexfilecontent = do { local $/; <$fh> };

close $fh;

my $mainregexfinal = "";

my $defaultidentifiers = {};

@typedefidentifiersvector = ($defaultidentifiers);

@typedefidentifierschanged = (0);

my $entryregex;

my $matchinperl = 1;

chdir "regexes";

$mainregexfilecontent =~/$metaregexfilecontent/;

chdir "..";

#(?{parseregexfile($+{filename})})
#(?{entryregexmain($+{entrygroup}, $+{prefix})})

entryregexmain($+{entrygroup}, $+{prefix});

parseregexfile((substr $mainregexfilecontent, length $&), 1);

$mainregexdefs = "$mainregexfinal";

$mainregexdefs =~s/\s|\n//g if(not $matchinperl);

#$mainregexdefs = "$mainregexdefs|(?&&$entryregex)";

=begin
my %cached_instances = ();

while($mainregexdefs =~ m{
    (?=[(][?]<(\w+?)>)($inpar)(?=.*?[(][?]&&\1(facet)?+[)])
}sxxg) {
    $cached_instances{$1} = $+{inpar};
    print "\n\n\n========================================================";
                                print "registered $cached_instances{$1} \n";
                                print "========================================================\n\n\n";
}

while($mainregexdefs =~ m{
    [(][?]&&(?<ident>\w+?)(facet)?+[)]
}sxxg) {
    if(not exists $cached_instances{$1}) {
        $cached_instances{$1} = $+{ident};
        print "\n\n\n========================================================";
                                    print "registered $cached_instances{$1} \n";
                                    print "========================================================\n\n\n";
    }
}


while($mainregexdefs =~ m{
    [(][?]&&(\w+?)(facet)?+[)]
}sxxg) {
    $cached_instances{"$1"} = "(?&$1)";
    $cached_instances{"${1}facet"} = dofacetsubreplacements($1)
}


sub instantiate_cached_instance {
    my $arg = $_[0];
    my $isfacet = $arg =~ m{facet$} or exists $+{facet} ? "facet" : "";
    $arg =~ s{facet$}{};
=begin
    print "\n\n\n========================================================";
    print "\n========================================================";
    print "\n========================================================\n";
                                print "executing $cached_instances{$arg . $isfacet} \n";
                                 print "\n========================================================";
                                 print "\n========================================================\n";
                                print "========================================================\n\n\n";
= cut
    return $cached_instances{$arg . $isfacet};
}

$mainregexdefs =~ s{
    [(][?]&&(?<nm>\w+?(facet)?+)[)]
}{
    (??{instantiate_cached_instance("$+{nm}")})
}sxxg;

$mainregexdefs =~ s{
    [(][?](&{1,3}\+?+)(\w+?)[)]
}{
    (?&$2)
}sxxg;



$mainregexdefs =~ s{
    [(][?](&{1,3})(\w+?)(facet|(?<facet>facet)?+<(?<name>\w+)>)[)]
}{
    if(not $+{name}) {
        dofacetsubreplacements($1, $2)
    } else {
        dotemplatesubreplacements($1, $2, $+{name}, $+{facet})
    }
}sxxge;

=cut

=begin

$mainregexdefs =~ s{
    [(][?]{call(.*?)}[)]
}{
    (?(R&facet)(?{callfacet$1})|(?{call$1}))
}sxxg;



while(my($k, $v) = each %cached_instances) { 
    use if $ARGV[1], re => qw(Debug EXECUTE);

    my $body = $mainregexdefs;

    my $prefix = $entryregex ne $k ? "" : "^";

    my $postfix = "";

    $body = $v =~ s{^[(][?]<(\w+?)>}{(?<${k}entry>}r;

    print "$body\n==========\n";

    $postfix = "entry" if(defined $1);

    $body = $mainregexdefs =~ s{\Q$v\E}{$body}rs;

    $cached_instances{$k} = qr{(*F)($body)|$prefix(?&$k$postfix)}sxxn;

    my $facet = dofacetsubreplacements("$k$postfix");

    $cached_instances{$k . "facet"} = qr{(*F)($body)|$prefix$facet}sxxn;
    
    #$cached_instances{$k . "facet"} = qr{(*F)($body)(?<facet>(?&${k}$postfix))|$prefix(?&facet)}sxxn
}

print $cached_instances{$entryregex};

=cut

print "$mainregexdefs\n";

startmatching($subject, $mainregexfinal, basename($ARGV[0], @suffixlist)) if(not $matchinperl);

exit if(not $matchinperl);

startmodule(basename($ARGV[0], @suffixlist)) if(defined &startmodule and not $nested);

if(not $isnested)
{
    require "extractfns.pm";

    use if $ARGV[1], re => qw(Debug EXECUTE); 

    my $entry = qr{(?(DEFINE)$mainregexdefs)(?&$entryregex)}sxx;

    $subject =~ $entry
}

#}

=for comment

sub recovery_mode() {
    my $failedat = pos();
    my $subslice = substr $subject, pos(), 30;

    $subslice =~ s{\R}{ }g;

    print "Failed to match around: $subslice.\n";

    print {$fh} $failedat;

    {local $,='\n';print @typedefidentifiersvector}

    open my $fh, '>', $filename or die "error opening $filename: $!";
    print {$fh} $file;
    close $fh;
}

=cut

#loadregex();

#print $&;

exit;

sub isfacet {
    use Data::Dumper;
    #print "facet \tchecking -> " . $facet . "\n";
    return $facet#ref $-{facet} ne ARRAY
}

sub checkpoint {
    undef %matches;
}

sub call {
    #print Dumper(\%+);
    my $captures = { %+ };
    my $facet = isfacet;
    return if $facet;
    print $_[0] . "\n";

    my $funcnm = $_[0] =~ s{facet$}{}r;

    @$captures{keys %matches} = values %matches;

    #print "facet -> $facet\n";
    
    my $cond = ($entryregex =~ m{facet$} or not $facet);
    #return unless($entryregex =~ m{facet$} or not $facet);
    #require re;
    #re->import('debug') if($_[1] eq "Ptr64ToPtr");
    #select($oldfh) if($_[1] eq "Ptr64ToPtr");
    #my @arr = @_;
    my $subslice = substr $subject, pos(), 10;

    $subslice =~ s{\R}{ }g;

    if ($cond) {
        use Data::Dumper;
        use POSIX;
    
        print strftime ("%F %T", localtime time) . " capture: " . $subslice . "\n";
        print $funcnm . "\n";
        print Dumper(\$captures)
    }
    
    #foreach my $i (@arr) {
    #    print $i . "\n";
    #}
    my $res;
    eval {
        if($cond) {
            $res = $funcnm->($captures) 
        }
    };

    callout($funcnm, $captures) if(defined &callout and $cond);
    return $res;
}

=for comment

sub defaultcallback {
    print "in";
    given ($_[0])
    {
        when (38) {return getypedefidentifiers() if $+{identifierraw}; return "";};

        when (39) {addtypdefidentifier() if $+{identifierraw};};

    	print("comma\n") when 35;
        print("ternary0\n") when 34;
        print("ternary1\n") when 33;
        print("ternary2\n") when 32;

        print("start func decl params") when 47;
        print("end func decl params") when 48;

        printifdefined("$+{assignopraw}\n", $+{assignopraw}) when 30;
        printifdefined("$+{orlogicopraw}\n", $+{orlogicopraw}) when 29;
        printifdefined("$+{andlogicopraw}\n", $+{andlogicopraw}) when 28;
        printifdefined("$+{oropraw}\n", $+{oropraw}) when 27;
        printifdefined("$+{xoropraw}\n", $+{xoropraw}) when 26;
        printifdefined("$+{andopraw}\n", $+{andopraw}) when 25;
        printifdefined("$+{eqopraw}\n", $+{eqopraw}) when 24;
        printifdefined("$+{relopraw}\n", $+{relopraw}) when 23;
        printifdefined("$+{shiftopraw}\n", $+{shiftopraw}) when 22;
        printifdefined("$+{addopraw}\n", $+{addopraw}) when 20;
        printifdefined("$+{mulopraw}\n", $+{mulopraw}) when 21;
        printifdefined("$+{unaryopraw}\n", $+{unaryopraw}) when 19;
        printifdefined("postfix arithmetic:\n$+{postfixarithraw}\n", $+{postfixarithraw}) when 9;
        printifdefined("prefix arithmetic:\n$+{prefixarithraw}\n", $+{prefixarithraw}) when 10;
        printifdefined("$+{escaperaw}\n", $+{escaperaw}) when 1;
        printifdefined("$+{numberliteralraw}\n", $+{numberliteralraw}) when 5;
        printifdefined("$+{textraw}\n", $+{textraw}) when 2;
        printifdefined("$+{identifierraw}\n", $+{identifierraw}) when 6;
        print("start string\n") when 4;
        print("start function call\n") when 7;
        print("end function call\n") when 13;
        printifdefined("member access operator:\n$+{arrowordotraw}\n", $+{arrowordotraw}) when 8;
        print("end sizeof\n") when 14;
        print("begin sizeof\n") when 15;
        printifdefined("$+{qualifiers}\n", $+{qualifiers}) when 11;

        #default {print "bad call $_[0]\n"; exit}
    }
}

sub parsing {
    given ($_[0])
    {
        when (38) { return $typedefidentifiersvector[-1]; }
        when (39) 
        { 
            my $lastelem = pop @typedefidentifiersvector;
            $lastelem = $lastelem . "|" . $_[1] unless (not $_[3]);
            push @typedefidentifiersvector, $lastelem;
        }

        when (44) {
            push @typedefidentifiersvector, "(*F)";
        }

        when (45) {
            pop @typedefidentifiersvector;
        }
    }
}

=cut


sub entryregexmain {
    $entryregex = $_[0];
}

sub getnext {
    $_[0] =~ s {^\s*+(?<arg>\S+?)\s*+($|,)}{};

    return $+{arg};
}


sub substitutetemplateinstances {
    my $backup = $&;
    my $regexcontent = $_[0];

    my $template = $+{template};

    my $templatetoreplace = $template;

    $templatetoreplace =~ s {\#\S+$}{};

    my $args = $+{args};

    my $name = $+{name};

    my $doubleref = $+{doubleref};

    my $isfacet = $+{isfacet};

    #$isfacet = "(*F)" if(not $isfacet);

    return $backup unless($regexcontent =~ m {\(\?<(?<params>[^<>()]*?)>\)\s*+(?=\(\?<$templatetoreplace(?<isfacet>$isfacet)?+\b)$inpar}gxxs);

    #$isfacet = not $+{isfacet};

    #push @arrayoftemplates, $& unless ($& ~~ @arrayoftemplates);

    my $params = $+{params};

    my $body = $+{inpar};

    my $closure = $+{closure};

    #push @arrayoftemplates, $templatetoreplace unless ($templatetoreplace ~~ @arrayoftemplates);

    while(1) {
        #my $arg = getnext($args);

        $args =~ s{^(\s*+$inpar\s*+|\s*+(?<arg>\S*?)(\s*+\bas\b\s*+(?<alias>\w*))?+\s*+($|,))}{}sxx;

        #print $+{inpar} . "\n\n";

        my $inpar = $+{inpar};

        my $arg = $+{arg};

        my $alias = $+{alias};

        #print $arg . "\n";

        my $argident;

        my $argcallout;

        my $argqualifs;

        my $tmplargs;

        if(not $inpar) {

            $arg =~ m{((?<ident>[_a-zA-Z][_a-zA-Z0-9]*+)|(?<callout>\d++))(?<qualifs>\S*+)}x;

            $argident = $+{ident};

            $argcallout = $+{callout};

            $argqualifs = $+{qualifs};

            $tmplargs = $+{inargs};
            
        }

        my $param = getnext($params);

        last if(!$param);

        #print $inpar;

        $arg = $argident = substitutetemplateinstancesdoregex($inpar, $regexcontent) if($inpar);
        $body = $inpar . $body;

        print $body . "\n";

        die "incorrect invocation for $param" if(not $argident);

        die "expr as arguments not supported currently" if($inpar);

        print $param . "->" . $argident . " as $alias\n";

        $body =~ s{(\(\?&{1,2})$param(facet)?+((?<inargs><(?<args>[^<>]++|(?&inargs))*+>)?+)\)}{
            if($arg) {if(not $argcallout) {
                if($+{inargs} and $alias) {
                    $1 . $argident . ($isfacet // $2) . $3 . "=$alias)" . $argqualifs ;
                } else {
                    $1 . ($alias ? $alias : $argident) . ($isfacet // $2) . $3 . ")" . $argqualifs ;
                }
            } else {
                "(" . $argqualifs . "?C" . $argcallout . ")" ;
            } }else {
                "()"
            }}eg;

        #$body =~ s{\\g\{\b$param(facet)?+\b\}}{\\g{$argident}}g;}

        $body =~ s{@\b$param(facet)?+\b}{($alias ? $alias : $argident)}ge;

        print "matched at\n";

        $body =~ s{\b$param(facet)?+\b\s*+\bas\s*+\w*}{"$argident$1$argqualifs as" . ($alias ? " $alias" : "")}ge;

        print $& . "\n";

        #print $str;
    }

    if($name) {
        $body =~ s{(?<=\(\?<)\Q$template\E\b}{$name}e;
    } elsif(not $doubleref) {
        $body =~ s{\(\?<\Q$template\E\b.*?>}{(};
    }
    if($isfacet) {
        $body = dofacetreplacements($body, $template);
    } else {
        #my $bodyoriginal = $body;
        #dofacetreplacements($body);
        #$body = "(?(DEFINE)" . $body . ")" . $bodyoriginal;
    }

    print "done:\n";

    print $body . "\n";

    return $body;

    #replacefacetgroups($1, $_[1]) while($regexcontent =~ m {(\(([^()]++|(?1))*\))}gxx);
}

sub parseregexfile {
    my $filename;
    if(not defined $_[1])
    {
        $filename = $_[0];
        open my $fh, '<', $filename or die "error opening $filename: $!";

        $regexfilecontent = do { local $/; <$fh> };

        close $fh;

    } else {
        $regexfilecontent = $_[0];
    }

    $mainregexfinal = $mainregexfinal . $regexfilecontent;

    return;

    $regexfilecontent =~ s {(?=\(\?\#)$inpar}{}gsxx;

    $regexfilecontent =~s/#restrictperlonly//g if($matchinperl);

    $regexfilecontent =~s/#restrictpcre2only//g if(not $matchinperl);

    $regexfilecontent =~ s {(?=(\(\?<\w+)\#restrictpcre2only>)$inpar}{}gsxx if($matchinperl);

    $regexfilecontent =~ s {(?=(\(\?<\w+)\#restrictperlonly>)$inpar}{}gsxx if(not $matchinperl);

    #@arrayoftemplates = ();

    my $alltemplates = "";

    $alltemplates = $alltemplates . "\n\n" . $& while($regexfilecontent =~s {\(\?<(?<params>[^()<>]*?)>\)\s*+
                    $inpar}{}sxx);

    
    #print $alltemplates if($filename eq "inner.regex");

    #exit if($filename eq "inner.regex");

    sub substitutetemplateinstancesdoregex {
        my $templatename;
        $_[0] =~s {\(\?&(?<doubleref>&)?+(?<template>((?![)])\S)+?)(?<isfacet>facet)?+
                            (?<inargs><(?<args>[^<>]++|(?&inargs))*+>)
                            (=(?<name>((?![)])\S)+))?\)}
                            {
                                $templatename = $+{template};
                                print "\n\n\n========================================================";
                                print "substituting $& \n";
                                print "========================================================\n\n\n";
                                substitutetemplateinstances($_[1])
                            }gxxe;

        return $templatename;
    }

    while(substitutetemplateinstancesdoregex($regexfilecontent, $alltemplates)) {
        print $regexfilecontent;
    }

    #print $regexfilecontent;
    #exit;

    #print @arrayoftemplates;

    foreach my $template (@arrayoftemplates) {
        #print "\n\n$template\n\n";
        $regexfilecontent =~s {\(\?<(?<params>.*?)>\)\s*+(?=\(\?<\Q$template\E\b)$inpar}{}gxx ;
    }

    #print $regexfilecontent;

    #exit;

    my $regexfile = $regexfilecontent; 
=begin
    sub addfacetdefines {
        $_[0] =~s{(?(DEFINE)$inpar)(?=(?<parnnm>[(]\?<\w+?>(?<!facet>)))(?<body>(?&inpar))}
        {
            my $bodyorig = $+{body}, $bodyfacet = $&;
            my $parnnm = $+{parnnm};
            $bodyorig =~ s{^[(]\?<\w+?>}{}sxx;
            $bodyorig =~ s{[)]$}{}sxx;
            print "$parnnm -> " . $bodyorig . "\n";
            "((*F)" . dofacetreplacements($bodyfacet) . "|" . $parnnm . addfacetdefines($bodyorig) . "))"
            }sxxge;

        return $_[0]
    }

    addfacetdefines($regexfile);
=cut   
    #$regexfile =~s/(?<!<)#restrictoutsidefacet\b>/>(?(?{$facet})/g;

    #$regexfile =~s/\(\?<#restrictoutsidefacet>/((*F)/g;

    #$regexfile =~s/[(]\?&(?>\w+?)\#nofacet[)]/(?&$1)/g;

    #$regexfile =~s/[(]\?[(]<(\w+?)#nofacet>[)]/(?(<$1>)/g;

    #$regexfile =~s/\(\?\?C(\d++)\)/(?C$1)/g if(not $matchinperl);

    #$regexfile =~s/\(\?C(\d++)(<(?<args>.*?)>)?+\)/(?{callout($1 . ($+{args} ? "," . $+{args} : ""))})/g if($matchinperl);

    #$regexfile =~s/\(\?<(\w+)#nofacet>/(?<$1>/g;

    my $regexfilecontentcopy = $regexfilecontent;
=begin
    sub replacefacetgroups {
        $regexfilecontent =~s/\Q$_[0]\E(facet)?>/(/g;
    }

    replacefacetgroups($1, $_[1]) while($regexfilecontentcopy =~/(\(\?<\w+)facet>/g);

    sub replacenofacetgroups {
        my $name = $_[0];
        $_[1] =~s/[(]\?&\Q$name\E[)]/(?&$name#nofacet)/g;
        $_[1] =~s/\(\?<\Q$name\E\#nofacet>($inpar|(?&inner))\)/(?&$name#nofacet)/gsx;
    }

    replacenofacetgroups($1, $regexfilecontent) while($regexfilecontentcopy =~/\(\?<(\w+)#nofacet>/g); # remove references to nofacet groups
=cut
    sub dofacetreplacements {
        my $ret = $_[0];

        $ret =~s/\Q(?(<facet>)\E/(?(?{1})/g;

        #$_[0] =~s/(\(\?C&{1,2})(?!\S+?facet)(\S+?)\s*(<(?<args>[^()<>]*?)>)?\)/$1$2facet$3)/gs;

        #$regexfilecontent =~s/\(\?\?C(\d++)\)/(?C$1)/g if(not $matchinperl);

        $ret =~s/([(]\?([(]C)?+&(&?+\w+?))(facet)?+\b/$1facet/g;

        #$regexfilecontent =~s/([(]\?)?+[(]<(?>\w+?)(facet)?+>[)]/$1(<$2facet>)/g;

        #$regexfilecontent =~s/[(]\?&(?>\w+?)\#nofacet[)]/(?&$1)/g;

        #$regexfilecontent =~s/[(]\?[(]<(\w+?)#nofacet>[)]/(?(<$1>)/g;

        #$ret =~s{([(]\?[(]?+)<(\w+?)(facet)?+>}{$1<$2facet>}g;
        
        #$regexfilecontent =~s/(\(\?<\w+)>/$1facet>/g;
        
        return $ret
    }
=begin
    sub dofacetsubreplacements {
        my $identifier = $_[1];
        my $prefix = $_[0];

        #return "(?(DEFINE)(?<facetsub>(?<facet>)$actual))(?&facetsub)";
        return "(((?{++\$facet})|(?{--\$facet}))
                (?$prefix$identifier)
                ((?{--\$facet})|(?{++\$facet})))";
    }
=cut

    sub dotemplatesubreplacements {
        my $identifier = $_[1];
        my $prefix = $_[0];
        my $name = $_[2];
        my $isalsofacet = $_[3];

        #return "(?(DEFINE)(?<facetsub>(?<facet>)$actual))(?&facetsub)";

        return "(((?{++\$$name})|(?{--\$$name}))
                (?$prefix$identifier)
                ((?{--\$$name})|(?{++\$$name})))";
    }

    #dofacetreplacements($regexfilecontent);

    $mainregexfinal = $mainregexfinal . $regexfile;

    $mainregexfinal =~s/\((\?\??+)C&(\S+?)\s*(<(?<args>[^()<>]*?)>)?\)/
            my $prefix = "(" . $1 . "{debugcallout(" . "\"$2\"";
            $prefix . ($+{args} ? "," . ($+{args} =~ s {\b\S+\b}{\$\+\{$&\}}gr) : "") . ")})"
            /ges if($matchinperl);

    #$mainregexfinal =~s/\(\?\?C&(\S++)\)/(?C&$1)/g if(not $matchinperl);
    $mainregexfinal =~s/\(\?C(\d++)\)//g;
}