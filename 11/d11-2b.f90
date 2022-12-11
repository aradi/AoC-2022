! Solution with dynamical rank 1 integer array
module d11_2_helper
  use iso_fortran_env, only : int64
  implicit none

  integer(int64), parameter :: init_size_ = 100
  real, parameter :: incr_factor = 1.4

  type :: dyn_array_i1
    private
    integer(int64), pointer :: values_(:) => null()
    integer(int64) :: nvalues_ = 0
  contains
    procedure :: append_i0
    procedure :: append_i1
    generic :: append => append_i0, append_i1
    procedure :: reset
    procedure :: data_ptr
    procedure, private :: realloc_
    final :: finalize
  end type dyn_array_i1

contains


  subroutine append_i0(this, elem)
    class(dyn_array_i1), intent(inout) :: this
    integer(int64), intent(in) :: elem

    call this%realloc_(this%nvalues_ + 1)
    this%values_(this%nvalues_ + 1) = elem
    this%nvalues_ = this%nvalues_ + 1

  end subroutine append_i0


  subroutine append_i1(this, elems)
    class(dyn_array_i1), intent(inout) :: this
    integer(int64), intent(in) :: elems(:)

    call this%realloc_(this%nvalues_ + size(elems))
    this%values_(this%nvalues_ + 1 : this%nvalues_ + size(elems)) = elems
    this%nvalues_ = this%nvalues_ + size(elems)

  end subroutine append_i1


  subroutine reset(this)
    class(dyn_array_i1), intent(inout) :: this

    call this%realloc_(0_int64)
    this%nvalues_ = 0

  end subroutine reset


  function data_ptr(this) result(ptr)
    class(dyn_array_i1), intent(in) :: this
    integer(int64), pointer :: ptr(:)

    ptr => this%values_(1 : this%nvalues_)

  end function data_ptr


  subroutine finalize(this)
    type(dyn_array_i1), intent(inout) :: this

    if (associated(this%values_)) deallocate(this%values_)

  end subroutine finalize


  subroutine realloc_(this, newsize)
    class(dyn_array_i1), intent(inout) :: this
    integer(int64), intent(in) :: newsize

    integer(int64), pointer :: buffer(:)

    if (.not. associated(this%values_)) then
      allocate(this%values_(max(newsize, init_size_)))
    else if (newsize > size(this%values_)) then
      buffer => this%values_
      allocate(this%values_(max(ceiling(incr_factor * real(size(this%values_))), newsize)))
      this%values_(1 : this%nvalues_) = buffer(1 : this%nvalues_)
      deallocate(buffer)
    end if

  end subroutine realloc_

end module d11_2_helper


program d11_2b
  use iso_fortran_env, only : int64
  use d11_2_helper, only : dyn_array_i1
  implicit none

  integer, parameter :: n_rounds = 10000

  integer, parameter :: max_line_len = 1024
  integer, parameter :: max_opval_len = 20
  integer, parameter :: max_monkeys = 10


  type :: monkey_data
    type(dyn_array_i1) :: items = dyn_array_i1()
    character :: op = ""
    character(max_opval_len) :: opval = ""
    integer :: divisor = 0
    integer :: truetarget = 0
    integer :: falsetarget = 0
    integer(int64) :: ninspections = 0
  end type


  type(monkey_data) :: monkeydata(max_monkeys)
  integer(int64) :: nmonkeys, nitems, iround, imonkey, iitem, worrylevel, opval, itarget
  integer(int64) :: lcm, maxinsp, maxinsp2
  integer(int64), allocatable :: intbuffer(:), inspections(:)
  integer(int64), pointer :: dataptr(:)
  integer :: fd, iostat
  character(max_line_len) :: buffer

  nmonkeys = 0
  open(newunit=fd, file="input.dat", action="read")
  linereader: do
    nmonkeys = nmonkeys + 1
    associate (mdata => monkeydata(nmonkeys))
      read(fd, *)
      read(fd, "(t19, a)") buffer
      nitems = count(transfer(trim(buffer), ['a']) == ",") + 1
      allocate(intbuffer(nitems))
      read(buffer, *) intbuffer
      call mdata%items%append(intbuffer)
      deallocate(intbuffer)
      read(fd, "(t24, a1, 1x, a)") mdata%op, mdata%opval
      read(fd, "(t22, i10)") mdata%divisor
      read(fd, "(t30, i1)") mdata%truetarget
      read(fd, "(t31, i1)") mdata%falsetarget
      read(fd, "(a)", iostat=iostat)
      if (iostat /= 0) exit linereader
    end associate
  end do linereader
  close(fd)

  lcm = product(monkeydata(1 :  nmonkeys)%divisor)
  do iround = 1, n_rounds
    do imonkey = 1, nmonkeys
      associate(mdata => monkeydata(imonkey))
        dataptr => mdata%items%data_ptr()
        do iitem = 1, size(dataptr)
          mdata%ninspections = mdata%ninspections + 1
          worrylevel = dataptr(iitem)
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
          if (modulo(worrylevel, mdata%divisor) == 0) then
            itarget = mdata%truetarget
          else
            itarget = mdata%falsetarget
          end if
          call monkeydata(itarget + 1)%items%append(modulo(worrylevel, lcm))
        end do
        call mdata%items%reset()
      end associate
    end do
  end do

  inspections = monkeydata(1 :  nmonkeys)%ninspections
  maxinsp = maxval(inspections, dim=1)
  maxinsp2 = maxval(inspections, mask=(inspections /= maxinsp), dim=1)
  print "(a, i0)", "Monkey business level: ", int(maxinsp, int64) * int(maxinsp2, int64)

end program d11_2b
