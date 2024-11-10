package HandSorter;

use strict;
use warnings;

sub sort_hands {
    my $hand_ref = shift;

    sort_by_type($hand_ref);
}

sub sort_by_type {
    my $hand_ref = shift;

    @$hand_ref = sort { $b->get_type <=> $a->get_type } @$hand_ref;
}

return 1;
