program d15_2
  use iso_fortran_env, only : int64
  implicit none

  integer, parameter :: max_line_len = 1024

  integer, parameter :: rows_to_check(*) = [0, 4000000]
  integer, parameter :: cols_to_check(*) = [0, 4000000]

  integer :: fd, iostat
  character(max_line_len) :: buffer
  integer :: entries(4)
  integer, allocatable, target :: inpdata(:), excluded(:)
  integer, pointer :: inpdata2(:,:,:), excluded2(:,:), beaconpos(:), sensorpos(:)
  integer :: ii, jj, pos1, pos2, dist, start, end, rowdist, itmp(2), newstart, newend
  integer :: irow, icol
  integer(int64) :: tuningfreq
  logical :: bigger

  ! Read in data
  inpdata = [integer ::]
  open(newunit=fd, file="input.dat")
  do
    read(fd, "(a)", iostat=iostat) buffer
    if (iostat /= 0 .or. len_trim(buffer) == 0) exit
    pos1 = 1
    do ii = 1, 4
      pos1 = index(buffer(pos1 :), "=") + pos1 - 1
      pos2 = scan(buffer(pos1 + 1 :), ",:") + pos1
      if (pos2 == pos1) pos2 = len_trim(buffer) + 1
      read(buffer(pos1 + 1 : pos2 - 1), *) entries(ii)
      pos1 = pos2 + 1
    end do
    inpdata = [inpdata, entries]
  end do
  close(fd)
  inpdata2(1:2, 1:2, 1 : size(inpdata) / 4) => inpdata

  do irow = rows_to_check(1), rows_to_check(2)
    ! Create list of excluded intervals in the row to check
    excluded = [integer ::]
    do ii = 1, size(inpdata2, dim=3)
      sensorpos => inpdata2(:, 1, ii)
      beaconpos => inpdata2(:, 2, ii)
      dist = sum(abs(beaconpos - sensorpos))
      if (abs(sensorpos(2) - irow) > dist) cycle
      rowdist = dist - abs(irow - sensorpos(2))
      start = max(sensorpos(1) - rowdist, cols_to_check(1))
      end = min(sensorpos(1) + rowdist, cols_to_check(2))
      excluded = [excluded, [start, end]]
    end do
    excluded2(1:2, 1 : size(excluded) / 2) => excluded

    ! Sort excluded interval by bubble sort
    do ii = 1, size(excluded2, dim=2) - 1
      do jj = ii + 1, size(excluded2, dim=2)
        if (excluded2(1, ii) == excluded2(1, jj)) then
          bigger = excluded2(2, ii) > excluded2(2, jj)
        else
          bigger = excluded2(1, ii) > excluded2(1, jj)
        end if
        if (bigger) then
          itmp(:) = excluded2(:, ii)
          excluded2(:, ii) = excluded2(:, jj)
          excluded2(:, jj) = itmp
        end if
      end do
    end do

    ! Merge excluded intervals and count the length they cover
    tuningfreq = 0
    end = excluded2(1, 1) - 1
    icol = cols_to_check(1) - 1
    do ii = 1, size(excluded2, dim=2)
      newstart = excluded2(1, ii)
      newend = excluded2(2, ii)
      if (newend > end) then
        if (newstart > end + 1) then
          icol = newstart - 1
          exit
        end if
        start = max(end + 1, newstart)
        end = newend
      end if
    end do

    if (icol >= cols_to_check(1)) then
      tuningfreq = int(icol, int64) * 4000000_int64 + int(irow, int64)
      exit
    end if
  end do

  print "(a, i0)", "Tuning frequency of the distress bacon: ", tuningfreq

end program d15_2
