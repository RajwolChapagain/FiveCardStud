module hand_module
    use card_module

    implicit none

    private
    public hand

    type hand
        type(card) :: cards(0:4)
        integer :: last_index = -1
        integer :: hand_type = -1
    contains
        procedure :: add_card, to_string
    end type hand

contains
    subroutine add_card(this, new_card)
        class(hand) :: this
        type(card), intent(in) :: new_card

        this%cards(this%last_index + 1) = new_card
        this%last_index = this%last_index + 1
    end subroutine add_card

    function to_string(this) result(string)
        class(hand), intent(in) :: this
        character(:), allocatable :: string
        integer :: i

        string = ''
        do i = 0, size(this%cards) - 1
            string = string // this%cards(i)%to_string()
        end do

    end function to_string
end module hand_module
