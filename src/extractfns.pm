use IO::Handle;
use re 'eval';

$filename = "/Users/alexander/out/decls.h";
open $fdecls, '>', $filename or die "error opening $filename: $!";

sub extract_struc {
    print {$fdecls} $_[0]{strucdecl} . "\n";
    $fdecls->flush()
}

sub extract_func {

    my $funcbody = $_[0]{func};
    my $funcname = $_[0]{ident};

    return if not exists $_[0]{func};

    print "$funcname\n";

    my $filename = "/Users/alexander/out/$funcname.c";
    open my $fdefs, '>', $filename or die "error opening $filename: $!";
    print {$fdefs} "\#include \"decls.h\"\n\n" . $funcbody . "\n";
    close $fdefs;
    #$func_n++
}



sub identifier_decl_function {
    $func_n = $_[0]{'ident'};
}

sub extract_decl {
    print {$fdecls} $_[0]{decls} . "\n";
    $fdecls->flush()
}

#select(<>); 

{
    1
}