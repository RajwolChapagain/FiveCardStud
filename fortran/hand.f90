module hand_module
    use card_module

    implicit none

    private
    public hand

    type hand
        type(card) :: cards(0:4)
        integer :: last_index = -1
    contains
        procedure :: add_card
    end type hand

contains
    subroutine add_card(this, new_card)
        class(hand) :: this
        type(card), intent(in) :: new_card

        this%cards(this%last_index + 1) = new_card
        this%last_index = this%last_index + 1
    end subroutine add_card

end module hand_module
