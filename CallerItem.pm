package Devel::CallerItem;

sub from_depth {
    my($class,$depth) = @_;
    $class = ref($class) || $class;
    $depth ||= 0;

    package DB;
    my(@callVars) = caller($depth + 1);
    @callVars || return undef;

    unless ($callVars[4]) {
	my($i,$h);
	for ($i = $depth + 2; defined( $h = (caller($i))[4] ) && !$h; $i++) {}
    }
    package Devel::CallerItem;

    bless [\@DB::args, @callVars], $class;
}

sub argument_list_ref {$_[0]->[0]}
sub pack {$_[0]->[1]}
sub file {$_[0]->[2]}
sub line {$_[0]->[3]}
sub subroutine {$_[0]->[4]}
sub has_args {$_[0]->[5]}
sub wants_array {$_[0]->[6]}

sub as_array {@{$_[0]}}

sub as_string {
    my($self,$print_level) = @_;
    my($str);
    $str = $self->wants_array() ? '@ = ' : '$ = ';
    if ($self->has_args()) {
	$str .= $self->subroutine() . '(' ;
	$str .= $self->arguments_as_string($print_level) . ')';
    } else {
	$str .= '&' . $self->subroutine() ;
    }
    $str .= ' called from ' . $self->file() . ' line ' . $self->line() . "\n";
    $str;
}

sub arguments_as_string {
    my($self,$print_level) = @_;
    my $count = [0];
    join( ', ' , map( $self->printable_arg($_,$print_level,'',$count),
		         @{$self->argument_list_ref()}
		     )
	 );
}

sub printable_arg {
    my($self,$arg,$print_level,$seen,$n) = @_;
    $print_level ||= 0;
    $seen ||= {};
    $n ||= [0];
    my($str);
    if (ref($arg)) {
	if ($print_level == 0) {
	    $str = "$arg";
	} elsif ($print_level == 1) {
	    if ($seen->{"$arg"}) {
		$str = "$arg";
	    } else {
		$seen->{"$arg"} = 1;
		$str = $self->_ref_printable_arg($arg,$print_level,$seen,$n);
	    }
	} else {
	    if ($seen->{"$arg"}) {
		$str = '$v' . $seen->{"$arg"};
	    } else {
		$n->[0]++;
		$seen->{"$arg"} = $n->[0];
		my $bless = (rindex($arg,'=') == -1) ? '' : ', ' . ref($arg);
		$str = '($v' . $n->[0] . ' = ' . ($bless ? 'bless ' : '' );
		$str .= $self->_ref_printable_arg($arg,$print_level,$seen,$n);
		$str .= ( $bless ? $bless : '' ) . ')';
	    }
        }
    } else {
    	$str = $self->_non_ref_printable_arg($arg);
    }
    $str;
}

sub _non_ref_printable_arg {
    my($self,$arg) = @_;
    my $str = "$arg";
    $str =~ s/'/\\'/g;
    $str =~ s/([^\0]*)/'$1'/
	unless $str =~ /^(?: -?[\d.]+ | \*[\w:]* )$/x;
    $str =~ s/([\200-\377])/sprintf("M-%c",ord($1)&0177)/eg;
    $str =~ s/([\0-\37\177])/sprintf("^%c",ord($1)^64)/eg;
    $str;
}

sub _ref_printable_arg {
    my($self,$arg,$print_level,$seen,$count) = @_;
    my($str);
    if ($arg =~ /ARRAY/) {
	$str = $self->_array_printable_arg($arg,$print_level,$seen,$count);
    } elsif ($arg =~ /HASH/) {
	$str = $self->_hash_printable_arg($arg,$print_level,$seen,$count);
    } elsif ($arg =~ /SCALAR/) {
	$str = $self->_scalar_printable_arg($arg,$print_level,$seen,$count);
    } else {
	$str = "$arg";
    }
    $str;
}

sub _array_printable_arg {
    my($self,$arg,$print_level,$seen,$count) = @_;
    my($str,$element);
    if ($#{$arg} == -1) {
	$str = '[]';
    } else {
	$str = '[';
	foreach $element (@{$arg}) {
	    $str .= $self->printable_arg($element,$print_level,$seen,$count);
	    $str .= ',';
	}
	chop($str);
	$str .= ']';
    }
}

sub _hash_printable_arg {
    my($self,$arg,$print_level,$seen,$count) = @_;
    my($str,$key,$value);
    $str .= '{';
    while( ($key,$value) = each %{$arg} ) {
	$str .= $self->_non_ref_printable_arg($key) .' => ';
	$str .= $self->printable_arg($value,$print_level,$seen,$count) . ',';
    }
    chop($str);
    $str .= '}';
}

sub _scalar_printable_arg {
    my($self,$arg,$print_level,$seen,$count) = @_;
    '\\' . $self->printable_arg(${$arg},$print_level,$seen,$count);
}

1;
__END__

=head1 NAME

Devel::CallerItem - An object representing a function call from
the stack of function calls.

=head1 SYNOPSIS

Usage:

    require Devel::CallerItem;
    
    $call = Devel::CallerItem->from_depth($depth) || return;
    $passed_arguments_ref = $call->argument_list_ref();
    $callpack = $call->pack();
    $callfile = $call->file();
    $callline = $call->line();
    $callsub = $call->subroutine();
    $bool = $call->has_args();
    $bool = $call->wants_array();
    ($arg_ref,@caller) = $call->as_array();
    $call_string = $call->as_string($print_level);
    $passed_arguments_string = $call->arguments_as_string();
    
    $printable_arg = Devel::CallerItem->printable_arg($arg,$print_level);

=head1 DESCRIPTION

Devel::CallerItem objects hold all the information about a specific
function call on the stack. The information is basically that obtained
from caller and @DB::args, packaged into an object. This comes with
some useful methods to print the object in a nice way.

=head2 Methods Available:

=over 4

=item ->from_depth(DEPTH)

This method is the constructor for the class. DEPTH is a number,
corresponding to the stack level as used by caller. The following
two calls are equivalent in terms of what gets put into '@caller'

    @caller = caller($DEPTH);
    ($arg_ref,@caller) = Devel::CallerItem->from_depth($DEPTH)->as_array();

=item ->argument_list_ref()

Returns a reference to an array holding the elements actually
passed to the function on the stack that makes up the function call.

If the function was called as '&func;', then this array is not empty,
it holds the array that was passed down to the function 'func'.

=item ->pack()

The package from which the function was called.

=item ->file()

The file from which the function was called.

=item ->line()

The line from which the function was called.

=item ->subroutine()

The fully qualified package name of the function that was called.

=item ->has_args()

Boolean indicating whether the function was called with arguments.

=item ->wants_array()

Boolean indicating the context in which the function was called.

=item ->as_array()

Equivalent to the following:

    ($call->argument_list_ref(),$call->pack(),$call->file(),
        $call->line(),$call->subroutine(),$call->has_args(),
        $call->wants_array());

=item ->as_string(PRINT_LEVEL)

Returns the object in string format suitable for printing
a fully informative message about the function call. Looks
like one of the following:

    $ = func(args) called from FILE line LINE;
    $ = &func called from FILE line LINE;
    @ = func(args) called from FILE line LINE;
    @ = &func called from FILE line LINE;

giving the context (scalar - $, array - @) and whether it was called
with arguments or without (&). PRINT_LEVEL determines the level
of detail printed about the arguments to the function - see 'printable_arg'
below.

=item ->arguments_as_string(PRINT_LEVEL)

Returns a string representing the arguments held in the
argument_list_ref. Equivalent to calling 'printable_arg' for
each argument and joining them with commas.

=item ->printable_arg(ARG,PRINT_LEVEL)

Renders ARG printable. PRINT_LEVEL affects the detail of what
is printed. There are three levels, 0 or 1 or 2. (Currently
anything other than these values is treated as a '2', but
this is an unsupported feature an is likely to change
if any further levels are added - so use 0/1/2 to be safe.)

Level 0 makes strings printable, but scalars which return
refs are just stringified - i.e. an argument which is
like [33,{'g' => 55}] would just appear as something
like 'ARRAY(0x9882c)'. This is the default.

At level 1, an argument which is like [33,{'g' => 55}]
would be fully expanded to '[33,{'g' => 55}]', but
any scalar which is repeated in the arguments is
just stringified to something like 'ARRAY(0x9882c)'.
i.e. if you had '$a = bless [],A;$b =[$a];$a->[0]=$b;',
which is a recursive object, then '$a' would be printed
as '[[A=ARRAY(0x83038)]]'.

Finally, at the highest level, arguments are printed
with an associated variable and bless statement if
needed - so with '$a' above you would get $a printed
as: '($v1 = bless [($v2 = [$v1])], A)'. NOTE that this
does not actually rebuild '$a' in perl code - perl
parses this as having $v1 empty in internal array -
it is only assigned to after the outer anonymous array
is built. This nomenclature is used purely to make explicit
any recursive or multiply passed arguments - this sort
of level of detail is needed on occasion, but there
is a clear cost in clarity.

NOTE that the format of the printed out items that depends
on the PRINT_LEVEL is likely to change in future versions
when a standardized module for printing variables comes
out.

=back

=head1 EXAMPLE

The following is a simple example, illustrating the three levels
of detail available using the print_level settings, and can be
executed using C<perl -x Devel/CallerItem.pm>

#!perl
    

    require Devel::CallerItem;
    
    $a="pp";
    $c = bless [], A;
    $d = [$c];
    $c->[0] = $d;
    sub level0 {
	print Devel::CallerItem->from_depth(0)->as_string(0),"\n"
    }
    sub level1 {
	print Devel::CallerItem->from_depth(0)->as_string(1),"\n"
    }
    sub level2 {
	print Devel::CallerItem->from_depth(0)->as_string(2),"\n"
    }
    
    level0('hi',21,[44,[66,{"q","hi"},\$a],$c]);
    level1('hi',21,[44,[66,{"q","hi"},\$a],$c]);
    level2('hi',21,[44,[66,{"q","hi"},\$a],$c]);

__END__

=head1 AUTHOR

Jack Shirazi (though the difficult bits were taken from sigtrap)

  Copyright (c) 1995 Jack Shirazi. All rights reserved.
  This program is free software; you can redistribute it and/or
  modify it under the same terms as Perl itself.

=head1 MODIFICATION HISTORY

Version 1.0, 31st July - JS

=cut
