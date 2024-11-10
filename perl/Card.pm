package Card;

use strict;
use warnings;

our @VALUE_MAP = ("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K");
our @SUIT_MAP = ("D", "C", "H", "S");

sub new {
    my $class = shift;
    my ($value, $suit) = @_;

    my $self = bless {
        value => $value,
        suit => $suit
    }, $class;

    return $self;
}

sub to_string {
    my $self = shift;
    
    return $VALUE_MAP[$self->{value}] . $SUIT_MAP[$self->{suit}];
}
