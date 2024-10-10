program main
    use card_module
    use hand_module
    use hand_identifier_module

    implicit none
    type(Card) :: deck(0:51)
    type(card) :: new_card
    type(hand) :: hands(0:5)
    integer :: i,j, argc
    character(50) :: cmdarg
    logical :: is_testing

    print *, '*** P O K E R   H A N D   A N A L Y Z E R ***'
    print *, ''
    print *, ''
    
    argc = command_argument_count()
    
    if (argc == 1) then
        is_testing = .true.
    else
        is_testing = .false.
    end if

    if (is_testing) then
        print *, '*** USING TEST DECK ***'
        print *, ''
        call get_command_argument(1, cmdarg)
        call print_file(cmdarg) 
        call deal_from_file(hands, cmdarg)

        if (has_duplicate(hands)) then
            stop
        end if

        call print_hands(hands)
        call assign_types(hands)
        call print_ranked_hands(hands)
    else
        call init_deck(deck)
        call print_deck(deck)
        call deal_from_deck(hands, deck)
        call print_hands(hands)
        call print_remaining_deck(deck)
        call assign_types(hands)
        call print_ranked_hands(hands)
    endif

contains
    subroutine init_deck(deck)
        type(Card) :: deck(0:51)
        integer :: i
        real :: rand
        integer :: random_int
        type(Card) :: tmp

        do i = 0, size(deck) -1
            call deck(i)%init_card(mod(i, 13), i / 13)
        end do

        do i = 0, size(deck) - 1
            call random_number(rand)
            random_int = int(rand * 52)
            tmp = deck(i)
            deck(i) = deck(random_int)
            deck(random_int) = tmp
        end do
    end subroutine init_deck

    subroutine print_deck(deck)
        type(Card), intent(in) :: deck(0:51)
        integer :: i
        character(:), allocatable :: line

        print *, '*** USING RANDOMIZED DECK OF CARDS ***'
        print *, ''
        print *, '*** Shuffled 52 card deck:'

        line = ''
        do i = 0, size(deck) - 1
            line = line // deck(i)%to_string()
            if (mod(i+1, 13) == 0) then
                write(*, '(A)') trim(line)
                line = ''
            end if 
        end do
        print *, ''
    end subroutine print_deck

    subroutine deal_from_deck(hands, deck)
        type(card), intent(in) :: deck(0:51)
        type(hand) :: hands(0:5)
        integer :: i, j, counter

        counter = 0
        do i = 0, 4
            do j = 0, 5 
                call hands(j)%add_card(deck(counter))
                counter = counter + 1
            end do
        end do

    end subroutine deal_from_deck


    subroutine print_remaining_deck(deck)
        type(card), intent(in) :: deck(0:51)
        character(:), allocatable :: line

        line = ''
        print *, '*** Here is what remains in the deck...'
        do i = 30, size(deck) - 1
            line = line // deck(i)%to_string()
        end do

        print *, line
        print *
    end subroutine print_remaining_deck


    !=============== Testing procedures ===============   
    subroutine print_file(file_path)
        character(50), intent(in) :: file_path
        character(80) :: line

        print *, '*** File: ', trim(file_path)

        open(unit=5, file=file_path, status='old')

        do i = 0, 5
            read (5,"(a80)") line
            print *, line
        end do

        close(5)

        print *
    end subroutine print_file

    subroutine deal_from_file(hands, file_path)
        type(hand) :: hands(0:5)
        type(card) :: new_card
        character(50), intent(in) :: file_path
        character(80) :: line
        character(3) :: card_token
        integer :: i, j

        open(unit=5, file=file_path, status='old')

        do i = 0, 5 !For each hand
            read (5,"(a80)") line
            
            do j = 0, 4 !For each card
                card_token = line(4 * j + 1: 4 * j + 3)
                call new_card%init_card_from_string(card_token)
                call hands(i)%add_card(new_card)
            end do
        end do

        close(5)
    end subroutine deal_from_file

    logical function has_duplicate(hands) result(found_duplicate)
        type(hand), intent(in) :: hands(0:5)
        type(card) :: current_card
        integer :: card_hashes(30)
        integer i, j, k, card_hash

        card_hashes(:) = -1

        found_duplicate = .false.

        outer_loop : do i = 0, 5
            do j = 0, 4
                current_card = hands(i)%cards(j)
                card_hash = current_card%get_value() * 10 +current_card%get_suit()
                inner_loop: do k = 1, 30
                    if (card_hashes(k) == -1) then
                        card_hashes(k) = card_hash
                        exit inner_loop
                    end if

                    if (card_hash == card_hashes(k)) then
                        found_duplicate = .true.
                        print *, '*** ERROR - DUPLICATED CARD FOUND IN DECK ***'
                        print *
                        print *, '*** DUPLICATE - ', trim(hands(i)%cards(j)%to_string()), ' ***'
                        exit outer_loop
                    endif
                end do inner_loop
            end do
        end do outer_loop
    end function has_duplicate
    

    !=============== Shared procedures ===============   
    subroutine print_hands(hands)
        type(hand), intent(in) :: hands(0:5)
        integer :: i

        print *, '*** Here are the six hands...'
        do i = 0, 5
            print *, hands(i)%to_string()
        end do
        print *, ''
    end subroutine print_hands

    subroutine assign_types(hands)
        type(hand) :: hands(0:5)
        integer :: i
        
        do i = 0, 5
            call assign_type(hands(i))
        end do
    end subroutine assign_types

    subroutine print_ranked_hands(hands)
        type(hand), intent(in) :: hands(0:5)
        integer :: i
        
        print *, '--- WINNING HAND ORDER ---'

        do i = 0, 5
            print *, hands(i)%to_string(), ' - ', trim(hand_types(hands(i)%hand_type))
        end do
    end subroutine print_ranked_hands

end program main
