use Data::Dumper;

my sub print {CORE::print(@_) if( $ENV{'DEBUG'} )}

sub assignment {
    push2 \@matches, {"binoplast" => $+{assignop}};
    call 'binary'; 
    pop2 \@matches
}
=begin
sub begin_binary {
    shift;
    my $flagsarg = shift;
    my $res = shift;
    print "in begin_binary\n";
    print Dumper \@$flagsarg;
    print $$res = not existsflag "ternaryexpr", {"normalexpr"}, $flagsarg;
}

sub end_binary {
    shift;
    my $flagsarg = shift;
    my $res = shift;
    print $$res = not existsflag "ternaryexpr", {"normalexpr"}, $flagsarg;
}
=cut

1