module card_module
    implicit none

    private
    public :: Card
    public :: VALUE_MAP, SUIT_MAP

    character(2) :: VALUE_MAP(0:12)
    character(1) :: SUIT_MAP(0:3)
    data VALUE_MAP / 'A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K' /
    data SUIT_MAP / 'D', 'C', 'H', 'S' /

    type Card
        integer :: val, suit
    contains
        procedure :: init_card, get_value, get_suit, to_string
    end type Card

contains
    subroutine init_card(this, val, suit)
        class(Card) :: this
        integer :: val, suit
        this%val = val
        this%suit = suit
    end subroutine init_card

    function get_value(this) result (val)
        class(Card) :: this
        integer :: val
        val = this%val
    end function get_value

    function get_suit(this) result (suit)
        class(Card) :: this
        integer :: suit
        suit = this%suit
    end function get_suit

    function to_string(this) result (padded_string)
        class(card) :: this
        character(:), allocatable :: string
        character(4) :: padded_string
        string = trim(VALUE_MAP(this%get_value())) // trim(SUIT_MAP(this%get_suit()))
        padded_string = adjustl(string)
        
    end function to_string

end module card_module
