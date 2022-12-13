program d12_1
  implicit none

  integer, parameter :: max_line_len = 1024

  integer, parameter :: steps(2, 4) = reshape([-1, 0,   0, 1,   1, 0,   0, -1], [2, 4])

  integer :: fd, iostat, ii
  character(max_line_len) :: buffer
  integer, allocatable :: heightbuff(:), heights(:,:), distances(:,:)
  integer, allocatable :: startpos(:), endpos(:), frontpos(:,:), newfrontpos(:,:)
  integer :: oldpos(2), newpos(2)
  integer :: nrows, ncols, nfrontpos, nnewfrontpos, ifront, istep, dist
  logical :: found

  open(newunit=fd, file="input.sample.dat", action="read")
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
  endpos = findloc(heights, iachar('E') - iachar('a'))
  heights(startpos(1), startpos(2)) = 0
  heights(endpos(1), endpos(2)) = maxval(heights) + 1

  allocate(distances(nrows, ncols), source=-1)
  allocate(frontpos(2, nrows * ncols))
  allocate(newfrontpos(2, nrows * ncols))

  nfrontpos = 1
  frontpos(:, nfrontpos) = startpos
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

  if (found) then
    print "(a, i0)", "Path distance: ", dist
  else
    print "(a)", "Could not find any path to the end positions"
  end if

end program d12_1