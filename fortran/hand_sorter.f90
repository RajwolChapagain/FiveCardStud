module hand_sorter_module
    use card_module
    use hand_module
    use hand_identifier_module

    implicit none

contains
    subroutine sort_hands(hands)
        type(hand) :: hands(0:5)

        call sort_by_type(hands)
        call sort_ties(hands)
    end subroutine sort_hands

    subroutine sort_by_type(hands)
        type(hand) :: hands(0:5)
        
        call sort(hands, compare_hands_by_type)
    end subroutine sort_by_type

    subroutine sort_ties(hands)
        type(hand) :: hands(0:5)
        integer :: start_index, last_type, i

        start_index = 0
        last_type = hands(0)%hand_type

        do i = 0, 5
            if (hands(i)%hand_type /= last_type) then
                call sort_subarray(hands(start_index: i-1))

                start_index = i
                last_type = hands(i)%hand_type
            else if (i == size(hands) - 1) then
                call sort_subarray(hands(start_index:))
            end if
        end do
    end subroutine sort_ties

    subroutine sort_subarray(hands)
        type(hand) :: hands(:)
        integer :: hand_type

        hand_type = hands(1)%hand_type

        if (hand_type == 9) then
            call sort(hands, compare_royal_flush)
        else if (hand_type == 8) then
            call sort(hands, compare_straight_flush)
        else if (hand_type == 7) then
            call sort(hands, compare_four_of_a_kind)
        else if (hand_type == 6) then
            call sort(hands, compare_full_house)
        else if (hand_type == 5) then
            call sort(hands, compare_flush)
        else if (hand_type == 4) then
            call sort(hands, compare_straight)
        else if (hand_type == 3) then
            call sort(hands, compare_three_of_a_kind)
        else if (hand_type == 2) then
            call sort(hands, compare_two_pair)
        else if (hand_type == 1) then
            call sort(hands, compare_pair)
        else
            call sort(hands, compare_high_card)
        end if

    end subroutine sort_subarray
    
    subroutine sort(array, cmp_fn)
        type(hand) :: array(:)
        type(hand) :: temp
        integer :: i, j
        logical :: cmp_fn

        do i = 1, size(array) - 1
            do j = 1, size(array) - i
                if (cmp_fn(array(j), array(j + 1))) then
                    ! Swap the sorted_cards
                    temp = array(j)
                    array(j) = array(j + 1)
                    array(j + 1) = temp
                end if
            end do
        end do
    end subroutine sort

    !=============== logical comparator methods ===============
    ! All return true if hand1 is weaker

    logical function compare_hands_by_type(hand1, hand2) result(b)
        type(hand), intent(in) :: hand1, hand2

        b = .false.

        if (hand1%hand_type < hand2%hand_type) then
            b = .true.
        end if
    end function compare_hands_by_type


    logical function compare_royal_flush(h1, h2) result(b)
        type(hand), intent(in) :: h1, h2
        type(card) :: l1(5), l2(5)

        b = .true.

        l1 = h1%get_sorted_cards()
        l2 = h2%get_sorted_cards()

        if (l1(1)%get_suit() > l2(1)%get_suit()) then
            b = .false.
         end if
    end function compare_royal_flush

    logical function compare_straight_flush(h1, h2) result(b)
        type(hand), intent(in) :: h1, h2
        type(card) :: l1(5), l2(5)
        integer :: highest_card_comparison

        b = .true.

        l1 = h1%get_sorted_cards()
        l2 = h2%get_sorted_cards()

        highest_card_comparison = compare_highest_card(l1, l2)

        if (highest_card_comparison == 1) then
            return
        else if (highest_card_comparison == -1) then
            b = .false.
            return
        end if

        if (l1(1)%get_suit() > l2(1)%get_suit()) then
            b = .false.
            return
        end if

    end function compare_straight_flush

    logical function compare_four_of_a_kind(h1, h2) result(b)
        type(hand), intent(in) :: h1, h2
        type(card) :: l1(5), l2(5)
        integer :: value1, value2

        l1 = get_cards_occuring_n_times(h1%get_sorted_cards(), 4)
        l2 = get_cards_occuring_n_times(h2%get_sorted_cards(), 4)

        value1 = l1(1)%get_value()
        value2 = l2(1)%get_value()

        b = .true.

        if (value1 == 0) then
            value1 = 13
        end if 

        if (value2 == 0) then
            value2 = 13
        end if

        if (value1 > value2) then
            b = .false.
            return
        end if    
    end function compare_four_of_a_kind

    logical function compare_full_house(h1, h2) result(b)
        type(hand), intent(in) :: h1, h2
        type(card) :: l1(5), l2(5)
        integer :: value1, value2

        l1 = get_cards_occuring_n_times(h1%get_sorted_cards(), 3)
        l2 = get_cards_occuring_n_times(h2%get_sorted_cards(), 3)

        value1 = l1(1)%get_value()
        value2 = l2(1)%get_value()

        b = .true.

        if (value1 == 0) then
            value1 = 13
        end if 

        if (value2 == 0) then
            value2 = 13
        end if

        if (value1 > value2) then
            b = .false.
            return
        end if   
    end function compare_full_house

    logical function compare_flush(h1, h2) result(b)
        type(hand), intent(in) :: h1, h2
        type(card) :: l1(5), l2(5)

        b = .true.

    end function compare_flush

    logical function compare_straight(h1, h2) result(b)
        type(hand), intent(in) :: h1, h2
        type(card) :: l1(5), l2(5)

        b = .true.

    end function compare_straight

    logical function compare_three_of_a_kind(h1, h2) result(b)
        type(hand), intent(in) :: h1, h2
        type(card) :: l1(5), l2(5)

        b = .true.

    end function compare_three_of_a_kind

    logical function compare_two_pair(h1, h2) result(b)
        type(hand), intent(in) :: h1, h2
        type(card) :: l1(5), l2(5)

        b = .true.

    end function compare_two_pair

    logical function compare_pair(h1, h2) result(b)
        type(hand), intent(in) :: h1, h2
        type(card) :: l1(5), l2(5)

        b = .true.

    end function compare_pair

    logical function compare_high_card(h1, h2) result(b)
        type(hand), intent(in) :: h1, h2
        type(card) :: l1(5), l2(5)

        b = .true.

    end function compare_high_card

    !=============== Helpers ===============

    !Returns 1 if first list is weaker, -1 if second list is weaker and 0 if they are tied
    integer function compare_highest_card(l1, l2) result (cmp)
        type(card), intent(in) :: l1(:), l2(:)
        integer :: value_list1(size(l1)), value_list2(size(l2)), i, j, temp

        cmp = 0

        do i = 1, size(l1)
            if (l1(i)%get_value() == 0) then
                if (size(l1) == 5) then
                    if (is_straight(l1)) then
                        value_list1(i) = 0
                    else
                        value_list1(i) = 13
                    end if
                else
                    value_list1(i) = 13
                end if
            else
                value_list1(i) = l1(i)%get_value()
            end if

            if (l2(i)%get_value() == 0) then
                if (size(l2) == 5) then
                    if (is_straight(l2)) then
                        value_list2(i) = 0
                    else
                        value_list2(i) = 13
                    end if
                else
                    value_list2(i) = 13
                end if
            else
                value_list2(i) = l2(i)%get_value()
            end if
        end do

        ! Sort the first value list
        do i = 1, size(value_list1) - 1
            do j = 1, size(value_list1) - i
                if (value_list1(j) < value_list1(j + 1)) then
                    ! Swap the sorted_cards
                    temp = value_list1(j)
                    value_list1(j) = value_list1(j + 1)
                    value_list1(j + 1) = temp
                end if
            end do
        end do

        ! Sort the second value list
        do i = 1, size(value_list2) - 1
            do j = 1, size(value_list2) - i
                if (value_list2(j) < value_list2(j + 1)) then
                    ! Swap the sorted_cards
                    temp = value_list2(j)
                    value_list2(j) = value_list2(j + 1)
                    value_list2(j + 1) = temp
                end if
            end do
        end do

        do i = 1, size(value_list1)
            if (value_list1(i) < value_list2(i)) then
                cmp = 1
                return
            else if (value_list1(i) > value_list2(i)) then
                cmp = -1
                return
            end if
        end do
    end function compare_highest_card

    function get_cards_occuring_n_times(card_list, n) result(c)
        type(card), intent(in) :: card_list(:)
        type(card), allocatable :: c(:)
        integer, intent(in) :: n
        type(card), allocatable :: temp(:)
        integer :: i, j, last_val
        integer, allocatable :: value_list(:), temp_int(:)

        last_val = card_list(1)%get_value()

        allocate(c(0))
        allocate(value_list(0))

        do i = 1, size(card_list)
            if (.not. any(value_list == card_list(i)%get_value())) then
                if (get_card_frequency(card_list(i), card_list) == n) then
                    ! Append card to c
                    allocate(temp(size(c)))
                    temp = c

                    deallocate(c)
                    allocate(c(size(temp) + 1))

                    do j = 1, size(temp)
                        c(j) = temp(j)
                    end do

                    c(size(temp) + 1) = card_list(i) 
                    deallocate(temp) 

                    ! Append card value to value_list
                    allocate(temp_int(size(value_list)))
                    temp_int = value_list

                    deallocate(value_list)
                    allocate(value_list(size(temp_int) + 1))

                    do j = 1, size(temp_int)
                        value_list(j) = temp_int(j)
                    end do

                    value_list(size(temp_int) + 1) = card_list(i)%get_value()
                    deallocate(temp_int) 

                end if
            end if
        end do
    end function get_cards_occuring_n_times

end module
