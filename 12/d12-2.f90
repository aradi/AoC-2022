program d12_2
  implicit none

  integer, parameter :: max_line_len = 1024

  integer, parameter :: steps(2, 4) = reshape([-1, 0,   0, 1,   1, 0,   0, -1], [2, 4])

  integer :: fd, iostat, ii
  character(max_line_len) :: buffer
  integer, allocatable :: heightbuff(:), heights(:,:), distances(:,:), pathlens(:)
  integer, allocatable :: startpos(:), endpos(:), frontpos(:,:), newfrontpos(:,:)
  integer :: oldpos(2), newpos(2)
  integer :: nrows, ncols, nfrontpos, nnewfrontpos, ifront, istep, dist, irow, icol
  logical :: found

  open(newunit=fd, file="input.dat", action="read")
  read(fd, "(a)") buffer
  rewind(fd)
  ncols = len_trim(buffer)
  nrows = 0
  heightbuff = [integer ::]
  do
    read(fd, "(a)", iostat=iostat) buffer
    if (iostat /= 0 .or. len_trim(buffer) /= ncols) exit
    nrows = nrows + 1
    heightbuff = [heightbuff, [(iachar(buffer(ii:ii)) - iachar('a'), ii = 1, len_trim(buffer))]]
  end do
  close(fd)

  heights = reshape(heightbuff, [nrows, ncols], order=[2, 1])
  startpos = findloc(heights, iachar('S') - iachar('a'))
  heights(startpos(1), startpos(2)) = 0
  endpos = findloc(heights, iachar('E') - iachar('a'))
  heights(endpos(1), endpos(2)) = maxval(heights) + 1

  allocate(distances(nrows, ncols))
  allocate(frontpos(2, nrows * ncols))
  allocate(newfrontpos(2, nrows * ncols))
  pathlens = [integer ::]
  do irow = 1, nrows
    do icol = 1, ncols
      if (heights(irow, icol) /= 0) cycle
      distances(:,:) = -1
      nfrontpos = 1
      frontpos(:, nfrontpos) = [irow, icol]
      dist = 0
      distances(startpos(1), startpos(2)) = dist
      distloop: do
        dist = dist + 1
        nnewfrontpos = 0
        do ifront = 1, nfrontpos
          oldpos(:) = frontpos(:, ifront)
          do istep = 1, size(steps, dim=2)
            newpos(:) = oldpos + steps(:, istep)
            if (any(newpos < 1) .or. any(newpos > [nrows, ncols])) cycle
            if (distances(newpos(1), newpos(2)) /= -1) cycle
            if (heights(newpos(1), newpos(2)) - heights(oldpos(1), oldpos(2)) > 1) cycle
            distances(newpos(1), newpos(2)) = dist
            found = all(newpos == endpos)
            if (found) exit distloop
            nnewfrontpos = nnewfrontpos + 1
            newfrontpos(:, nnewfrontpos) = newpos
          end do
        end do
        if (nnewfrontpos == 0) exit distloop
        nfrontpos = nnewfrontpos
        frontpos(:, : nfrontpos) = newfrontpos(:, : nfrontpos)
      end do distloop
      if (found) pathlens = [pathlens, dist]
    end do
  end do

  if (size(pathlens) > 0) then
    print "(a, i0)", "Minimal path length: ", minval(pathlens)
  else
    print "(a)", "Could not find any paths to the end positions"
  end if

end program d12_2
