program d02_1
  implicit none

  character, parameter :: first_choices(*) = ['A', 'B', 'C']
  character, parameter :: second_choices(*) = ['X', 'Y', 'Z']
  integer, parameter :: shape_scores(*) = [1, 2, 3]
  integer, parameter :: outcome_scores(*) = [0, 3, 6]

  integer :: fd, iostat
  character(3) :: buffer
  integer :: first_choice_ind, second_choice_ind, outcome_ind, score

  score = 0
  open(newunit=fd, file="input.dat", form="formatted", action="read")
  do
    read(fd, "(a)", iostat=iostat) buffer
    if (iostat /= 0) exit
    first_choice_ind = findloc(first_choices, buffer(1:1), dim=1)
    second_choice_ind = findloc(second_choices, buffer(3:3), dim=1)
    ! Bring 2nd choice into the middle of the 0-2 interval (to position 1)
    ! Shift 1st choice by the same amount
    ! Fold 1st choice into the 0-2 interval
    ! Calculate difference between shifted 2nd and 1st choices  (-1, 0 or 1)
    ! Shift difference into the interval [1, 3] to match outcome_score entry
    outcome_ind = 1 - modulo(first_choice_ind - (second_choice_ind - 1), 3) + 2
    score = score + shape_scores(second_choice_ind) + outcome_scores(outcome_ind)
  end do
  close(fd)
  print "(a, i0)", "Score: ", score

end program d02_1
