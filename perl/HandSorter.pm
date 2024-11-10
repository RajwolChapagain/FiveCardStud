package HandSorter;

use strict;
use warnings;

my @comparators = (\&compare_royal_flush, \&compare_royal_flush, \&compare_royal_flush, \&compare_royal_flush, \&compare_royal_flush, \&compare_royal_flush, \&compare_royal_flush, \&compare_royal_flush, \&compare_royal_flush, \&compare_royal_flush);


sub sort_hands {
    my $hand_ref = shift;

    sort_by_type($hand_ref);
    sort_ties($hand_ref);
}

sub sort_by_type {
    my $hand_ref = shift;

    @$hand_ref = sort { $b->get_type <=> $a->get_type } @$hand_ref;
}

sub sort_ties {
    my $hand_ref = shift;

    my $start_index = 0;
    my $last_type = @$hand_ref[0]->get_type;

    for my $i (0..$#$hand_ref) {
        if (@$hand_ref[$i]->get_type != $last_type) {
            @$hand_ref[$start_index..$i-1] = sort { $comparators[$last_type]($a, $b) } @$hand_ref[$start_index..$i-1];

            $start_index = $i;
            $last_type = @$hand_ref[$i]->get_type;
        } elsif ($i == $#$hand_ref) {
            @$hand_ref[$start_index..$i] = sort { $comparators[$last_type]($a, $b) } @$hand_ref[$start_index..$i];
        }
    }
}

# =============== Comparators ===============
# All return:
# 1 if h1 is weaker
# 0 if h1 is stronger

sub compare_royal_flush {
    my ($h1, $h2) = @_;

    my @l1 = $h1->get_sorted_cards;
    my @l2 = $h2->get_sorted_cards;

    if ($l1[0]->get_suit < $l2[0]->get_suit) {
        return 1;
    }

    return 0;
}

return 1;
