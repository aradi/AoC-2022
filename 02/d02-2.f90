program d02_2
  implicit none

  character, parameter :: first_choices(*) = ['A', 'B', 'C']
  character, parameter :: expected_outcomes(*) = ['X', 'Y', 'Z']
  integer, parameter :: shape_scores(*) = [1, 2, 3]
  integer, parameter :: outcome_scores(*) = [0, 3, 6]

  integer :: fd, iostat
  character(3) :: buffer
  integer :: first_choice_ind, second_choice_ind, outcome_ind, outcome_shift, score

  score = 0
  open(newunit=fd, file="input.dat", form="formatted", action="read")
  do
    read(fd, "(a)", iostat=iostat) buffer
    if (iostat /= 0) exit
    first_choice_ind = findloc(first_choices, buffer(1:1), dim=1)
    outcome_ind = findloc(expected_outcomes, buffer(3:3), dim=1)
    ! Map outcome index onto [-1, 0, 1]
    outcome_shift = outcome_ind - 2
    second_choice_ind = modulo(first_choice_ind + outcome_shift - 1, 3) + 1
    score = score + shape_scores(second_choice_ind) + outcome_scores(outcome_ind)
  end do
  print "(a, i0)", "Score: ", score

end program d02_2
