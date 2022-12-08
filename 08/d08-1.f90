program d08_1
  implicit none

  integer, parameter :: max_line_len = 1024

  character(max_line_len) :: line
  integer :: fp, nn, ii, jj
  integer, allocatable :: heights(:,:)
  logical, allocatable :: visible(:,:)

  open(newunit=fp, file="input.dat", action="read")
  read(fp, "(a)") line
  nn = len_trim(line)
  rewind(fp)
  allocate(heights(nn, nn))
  do ii = 1, nn
    read(fp, "(*(i1))") heights(:, ii)
  end do
  close(fp)

  allocate(visible(nn, nn), source=.true.)
  do ii = 2, nn - 1
    do jj = 2, nn - 1
      visible(jj, ii) = &
          & all(heights(jj, ii) > heights(jj, : ii - 1))&
          & .or. all(heights(jj, ii) > heights(jj, ii + 1 :))&
          & .or. all(heights(jj, ii) > heights(: jj - 1, ii))&
          & .or. all(heights(jj, ii) > heights(jj + 1 :, ii))
    end do
  end do

  print "(a, i0)", "Nr. of visible trees: ", count(visible)

end program d08_1
