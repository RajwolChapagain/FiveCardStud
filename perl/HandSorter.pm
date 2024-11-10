package HandSorter;

use strict;
use warnings;
use HandIdentifier;

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

# =============== Helpers ===============

sub compare_highest_card {
    my ($l1_ref, $l2_ref) = @_;

    my @l1 = @$l1_ref;
    my @l2 = @$l2_ref;

    my @value_list1 = ();
    my @value_list2 = ();


    for my $i (0..$#l1) {
        push @value_list1, $l1[$i]->get_value;

        if ($l1[$i]->get_value == 0) {
            @value_list1[$i] = 13;

            if (@l1 == $Hand::HAND_SIZE) {
                if (HandIdentifier::is_straight(@l1) and !HandIdentifier::is_royal_straight(@l1)) {
                    @value_list1[$i] = 0;
                }
            }
        }

        push @value_list2, $l2[$i]->get_value;

        if ($l2[$i]->get_value == 0) {
            @value_list2[$i] = 13;

            if (@l2 == $Hand::HAND_SIZE) {
                if (HandIdentifier::is_straight(@l2) and !HandIdentifier::is_royal_straight(@l2)) {
                    @value_list2[$i] = 0;
                }
            }
        }
    }

    @value_list1 = sort { $b <=> $a } @value_list1;
    @value_list2 = sort { $b <=> $a } @value_list2;

    for my $i (0..$#value_list1) {
        if ($value_list1[$i] < $value_list2[$i]) {
            return 1;
        } elsif ($value_list1[$i] > $value_list2[$i]) {
            return -1;
        }
    }

    return 0;
}

sub get_cards_occuring_n_times {
    my ($card_list_ref, $n) = @_;

    my @card_list = @$card_list_ref;
    my @result = ();

    foreach my $card (@card_list) {
        if (!(grep {$_->get_value == $card->get_value} @result)) {
            if (HandIdentifier::get_card_count($card, @card_list) == $n) {
                push @result, $card;
            }
        }
    }

    return @result;
}

return 1;
