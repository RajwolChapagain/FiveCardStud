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
        procedure :: add_card, to_string, get_sorted_cards
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

    function get_sorted_cards(this) result(sorted_cards)
        class(hand), intent(in) :: this      
        type(card) :: sorted_cards(0:4)
        integer :: i, j
        type(card) :: temp


        sorted_cards = this%cards

        do i = 0, 3
            do j = 0, 3 - i
                if (sorted_cards(j)%get_value() > sorted_cards(j + 1)%get_value()) then
                    ! Swap the sorted_cards
                    temp = sorted_cards(j)
                    sorted_cards(j) = sorted_cards(j + 1)
                    sorted_cards(j + 1) = temp
                end if
            end do
        end do

    end function get_sorted_cards
end module hand_module
