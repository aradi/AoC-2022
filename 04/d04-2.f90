program d04_2
  implicit none

  integer :: fd, iostat
  character(100) :: buffer
  integer :: intervalbounds(2, 2)
  integer :: overlaps
  logical :: partially_contained

  overlaps = 0
  open(newunit=fd, file="input.dat", action="read")
  readlines: do
    read(fd, "(a)", iostat=iostat) buffer
    if (iostat /= 0) exit readlines
    intervalbounds = extract_interval_bounds(trim(buffer))
    ! Whether the lower bound of the first interval is within the bounds of the second interval,
    ! or the lower bound of the second interval is within the bounds of the first interval
    partially_contained = product(intervalbounds(:, 2) - intervalbounds(1, 1)) <= 0&
        & .or. product(intervalbounds(:, 1) - intervalbounds(1, 2)) <= 0
    if (partially_contained) overlaps = overlaps + 1
  end do readlines
  print "(a, i0)", "Number of pairs with at least partial overlap: ", overlaps

contains


  function extract_interval_bounds(line) result(intervalbounds)
    character(*), intent(in) :: line
    integer :: intervalbounds(2, 2)

    integer :: intpos(2, 2)
    integer :: seppos, iint

    seppos = index(line, ",")
    intpos(:, :) = reshape([1, seppos - 1, seppos + 1, len(line)], [2, 2])
    do iint = 1, 2
      seppos = index(line(intpos(1, iint) : intpos(2, iint)), "-") + intpos(1, iint) - 1
      read(line(intpos(1, iint) : seppos - 1), *) intervalbounds(1, iint)
      read(line(seppos + 1 : intpos(2, iint)), *) intervalbounds(2, iint)
    end do

  end function extract_interval_bounds

end program d04_2