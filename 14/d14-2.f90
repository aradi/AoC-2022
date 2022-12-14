program d14_2
  use iso_fortran_env, only : stdout => output_unit
  implicit none

  integer, parameter :: max_line_len = 1024

  integer, parameter :: sand_start_pos(2) = [0, 500]

  integer, parameter :: moves(2, 3) = reshape([1, 0,  1, -1,  1, 1], [2, 3])

  type :: path_item
    integer, allocatable :: points(:,:)
    type(path_item), allocatable :: next
  end type path_item


  integer :: fd, iostat
  character(max_line_len) :: buffer
  integer :: ii, startpos, endpos, trimlen
  integer :: xx, yy, ymin, ymax, nrows
  integer, allocatable :: pointbuffer(:)
  type(path_item), pointer :: item
  type(path_item), allocatable, target :: firstpath
  logical, allocatable :: occupied(:,:)
  integer :: startpt(2), endpt(2), curpt(2), newpt(2)
  integer :: nunits, imove

  ! Read in paths
  item => null()
  ymin = sand_start_pos(1)
  ymax = sand_start_pos(1)
  open(newunit=fd, file="input.dat")
  do
    read(fd, "(a)", iostat=iostat) buffer
    if (iostat /= 0 .or. len_trim(buffer) == 0) exit
    startpos = 1
    endpos = 0
    trimlen = len_trim(buffer)
    pointbuffer = [integer ::]
    do while (endpos <= trimlen)
      endpos = index(buffer(startpos : trimlen), "-") + startpos - 1
      if (endpos < startpos) endpos = len_trim(buffer) + 1
      read(buffer(startpos : endpos - 1), *) xx, yy
      pointbuffer = [pointbuffer, yy, xx]
      startpos = endpos + 3
    end do
    if (size(pointbuffer) > 0) then
      if (associated(item)) then
        allocate(item%next)
        item => item%next
      else
        allocate(firstpath)
        item => firstpath
      end if
      item%points = reshape(pointbuffer, [2, size(pointbuffer) / 2])
      ymin = min(ymin, minval(item%points(1, :)))
      ymax = max(ymax, maxval(item%points(1, :)))
    end if
  end do
  close(fd)

  ! Create simulation field
  ymax = ymax + 2
  nrows = ymax - ymin + 1
  allocate(&
      & occupied(ymin:ymax, sand_start_pos(2) - nrows - 1 : sand_start_pos(2) + nrows + 1),&
      & source=.false.)
  occupied(ubound(occupied, dim=1), :) = .true.

  ! Set up initial configuration
  item => firstpath
  do
    startpt = item%points(:, 1)
    do ii = 2, size(item%points, dim=2)
      endpt = item%points(:, ii)
      if (startpt(1) == endpt(1)) then
        startpos = min(startpt(2), endpt(2))
        endpos = max(startpt(2), endpt(2))
        occupied(startpt(1), startpos : endpos) = .true.
      else
        startpos = min(startpt(1), endpt(1))
        endpos = max(startpt(1), endpt(1))
        occupied(startpos : endpos, startpt(2)) = .true.
      end if
      startpt(:) = endpt
    end do
    if (allocated(item%next)) then
      item => item%next
      cycle
    end if
    exit
  end do

  ! Do simulation
  nunits = 0
  do
    nunits = nunits + 1
    curpt(:) = sand_start_pos
    do while (curpt(1) < ubound(occupied, dim=1))
      do imove = 1, size(moves, dim=2)
        newpt(:) = curpt + moves(:, imove)
        if (.not. occupied(newpt(1), newpt(2))) exit
      end do
      if (imove > size(moves, dim=2)) exit
      occupied(curpt(1), curpt(2)) = .false.
      occupied(newpt(1), newpt(2)) = .true.
      curpt(:) = newpt(:)
    end do
    if (curpt(1) == sand_start_pos(1)) exit
  end do

  print "(a, i0)", "Nr. of units before stationary flow: ", nunits

contains


  subroutine print_simulation(occupied)
    logical, intent(in) :: occupied(:,:)

    integer :: ii, jj
    character :: pixel

    do ii = lbound(occupied, dim=1), ubound(occupied, dim=1)
      do jj = lbound(occupied, dim=2), ubound(occupied, dim=2)
        if (occupied(ii, jj)) then
          pixel = "#"
        else
          pixel = "."
        end if
        write(stdout, "(a1)", advance="no") pixel
      end do
      write(stdout, *)
    end do
    write(stdout, *)

  end subroutine print_simulation

  end program d14_2