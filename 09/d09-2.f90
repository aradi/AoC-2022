
program d09_2
  implicit none

  integer, parameter :: nknots = 10

  character, parameter :: directions(*) = ["U", "D", "R", "L"]
  integer, parameter :: shifts(2, 4) = reshape([&
      & 1, 0,   -1, 0,   0, 1,   0, -1], [2, 4])

  integer, allocatable :: knotpos(:, :), visitedpos(:, :), buffer(:, :)
  integer :: diff(2)
  integer :: fd, iostat
  character :: dir
  integer :: nsteps, istep, idir, iknot, ii
  logical :: found
  integer :: nvisited

  allocate(knotpos(2, nknots), source=0)
  allocate(visitedpos(2, 100))
  nvisited = 1
  visitedpos(:, nvisited) = knotpos(:, nknots)
  open(newunit=fd, file="input.dat", action="read")
  do
    read(fd, *, iostat=iostat) dir, nsteps
    if (iostat /= 0) exit
    do istep = 1, nsteps
      idir = findloc(directions, dir, dim=1)
      knotpos(:, 1) = knotpos(:, 1) + shifts(:, idir)
      do iknot = 2, nknots
        diff = knotpos(:, iknot - 1) - knotpos(:, iknot)
        if (any(abs(diff) == 2)) then
          where (abs(diff) > 0) diff = diff / abs(diff)
          knotpos(:, iknot) = knotpos(:, iknot) + diff
        end if
      end do
      ! Check whether any of the items in visitedpos matches last knots position in all coordinates
      found = any([(all(visitedpos(:, ii) == knotpos(:, nknots)), ii = 1, nvisited)])
      if (.not. found) then
        ! Note: naive array extension becomes a bottleneck for large nr. of entries
        !visitedpos = reshape(&
        !    & [visitedpos, [knotpos(:, nknots)]], shape=[2, size(visitedpos, dim=2) + 1])
        nvisited = nvisited + 1
        if (nvisited > size(visitedpos, dim=2)) then
          allocate(buffer(2, int(1.5 * nvisited)))
          buffer(:, 1 : nvisited - 1) = visitedpos
          call move_alloc(buffer, visitedpos)
        end if
        visitedpos(:, nvisited) = knotpos(:, nknots)
      end if
    end do
  end do
  close(fd)

  print "(a, i0)", "Nr. of visited tail positions: ", nvisited

end program d09_2
