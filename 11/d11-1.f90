program d11_1
  implicit none

  integer, parameter :: n_rounds = 20

  integer, parameter :: max_line_len = 1024
  integer, parameter :: max_opval_len = 20


  type :: monkey_data
    integer, allocatable :: items(:)
    character :: op = ""
    character(max_opval_len) :: opval = ""
    integer :: divisor = 0
    integer :: truetarget = 0
    integer :: falsetarget = 0
    integer :: ninspections = 0
  end type


  type(monkey_data), allocatable :: monkeydata(:)
  integer :: nmonkeys, nitems, iround, imonkey, iitem, worrylevel, opval, itarget
  integer :: maxinsp, maxinsp2
  integer, allocatable :: inspections(:)
  integer :: fd, iostat
  character(max_line_len) :: dummy, buffer

  monkeydata = [monkey_data ::]
  open(newunit=fd, file="input.dat", action="read")
  linereader: do
    monkeydata = [monkeydata, monkey_data()]
    associate (mdata => monkeydata(size(monkeydata)))
      read(fd, *)
      read(fd, "(a18, a)") dummy, buffer
      nitems = count(transfer(trim(buffer), ['a']) == ",") + 1
      allocate(mdata%items(nitems))
      read(buffer, *) mdata%items
      read(fd, "(a23, a1, 1x, a)") dummy, mdata%op, mdata%opval
      read(fd, *) dummy, dummy, dummy, mdata%divisor
      read(fd, *) dummy, dummy, dummy, dummy, dummy, mdata%truetarget
      read(fd, *) dummy, dummy, dummy, dummy, dummy, mdata%falsetarget
      read(fd, "(a)", iostat=iostat) dummy
      if (iostat /= 0) exit linereader
    end associate
  end do linereader
  close(fd)
  nmonkeys = size(monkeydata)

  do iround = 1, n_rounds
    do imonkey = 1, nmonkeys
      associate(mdata => monkeydata(imonkey))
        do iitem = 1, size(mdata%items)
          mdata%ninspections = mdata%ninspections + 1
          worrylevel = mdata%items(iitem)
          if (mdata%opval == "old") then
            opval = worrylevel
          else
            read(mdata%opval, *) opval
          end if
          if (mdata%op == "*") then
            worrylevel = worrylevel * opval
          else
            worrylevel = worrylevel + opval
          end if
          worrylevel = worrylevel / 3
          if (modulo(worrylevel, mdata%divisor) == 0) then
            itarget = mdata%truetarget
          else
            itarget = mdata%falsetarget
          end if
          monkeydata(itarget + 1)%items = [monkeydata(itarget + 1)%items, worrylevel]
        end do
        mdata%items = [integer ::]
      end associate
    end do
  end do

  inspections = monkeydata(:)%ninspections
  maxinsp = maxval(inspections, dim=1)
  maxinsp2 = maxval(inspections, mask=(inspections /= maxinsp), dim=1)
  print "(a, i0)", "Monkey business level: ", maxinsp * maxinsp2

end program d11_1
