program d08_2
  implicit none

  integer, parameter :: max_line_len = 1024

  character(max_line_len) :: line
  integer :: fp, nn, jj, ii
  integer, allocatable :: heights(:,:), scores(:,:)
  integer :: bup, bdown, bright, bleft

  open(newunit=fp, file="input.dat", action="read")
  read(fp, "(a)") line
  nn = len_trim(line)
  rewind(fp)
  allocate(heights(nn, nn))
  do ii = 1, nn
    read(fp, "(*(i1))") heights(ii, :)
  end do
  close(fp)

  allocate(scores(nn, nn), source=0)
  do jj = 2, nn - 1
    do ii = 2, nn - 1
      ! Find position of trees at the boundaries
      bleft = findloc(heights(ii, jj) > heights(ii, : jj - 1), .false., back=.true., dim=1)
      if (bleft == 0) bleft = 1
      bright = findloc(heights(ii, jj) > heights(ii, jj + 1 : ), .false., dim=1) + jj
      if (bright == jj) bright = nn
      bup = findloc(heights(ii, jj) > heights(: ii - 1, jj), .false., back=.true., dim=1)
      if (bup == 0) bup = 1
      bdown = findloc(heights(ii, jj) > heights(ii + 1 :, jj), .false., dim=1) + ii
      if (bdown == ii) bdown = nn
      scores(ii, jj) = (jj - bleft) * (bright - jj) * (ii - bup) * (bdown - ii)
    end do
  end do

  print "(a, i0)", "Maximal view score: ", maxval(scores)

end program d08_2
