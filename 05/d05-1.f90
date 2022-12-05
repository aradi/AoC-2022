program d05_1
  implicit none

  integer, parameter ::maxlinelen = 1024

  type :: string_t
    character(:), allocatable :: content
  end type string_t

  type(string_t), allocatable :: lines(:)
  type(string_t), allocatable :: stacks(:)
  character(:), allocatable :: topcrates
  integer, allocatable :: operations(:,:)

  call read_lines("input.dat", lines)
  call extract_input(lines, stacks, operations)
  call apply_operations(operations, stacks)
  call get_top_crates(stacks, topcrates)
  print *, "Top crates: ", topcrates

contains


  subroutine read_lines(filename, lines)
    character(*), intent(in) :: filename
    type(string_t), allocatable :: lines(:)

    integer :: fp, nlines, iline, iostat
    character(maxlinelen) :: buffer

    open(newunit=fp, file=filename, form="formatted", action="read", access="stream")
    nlines = 0
    do
      read(fp, *, iostat=iostat)
      if (iostat /= 0) exit
      nlines = nlines + 1
    end do

    allocate(lines(nlines))
    rewind(fp)
    do iline = 1, nlines
      read(fp, "(a)") buffer
      lines(iline)%content = trim(buffer)
    end do
    close(fp)

  end subroutine read_lines


  subroutine extract_input(lines, stacks, operations)
    type(string_t), intent(in) :: lines(:)
    type(string_t), allocatable, intent(out) :: stacks(:)
    integer, allocatable, intent(out) :: operations(:,:)

    character(:), allocatable :: buffer
    integer :: nlayers, nstacks, ilayer, istack, iline, pos
    integer :: noperations, ioperation
    character :: poschar
    character(4) :: movestr, fromstr, tostr

    nlayers = 0
    nstacks = 0
    do
      buffer = adjustl(lines(nlayers + 1)%content)
      if(buffer(1:1) /= "[") exit
      nlayers = nlayers + 1
      nstacks = max(nstacks, (len_trim(lines(nlayers)%content) + 1) / 4)
    end do

    allocate(stacks(nstacks))
    do istack = 1, nstacks
      stacks(istack)%content = ""
    end do

    do ilayer = 1, nlayers
      iline = nlayers - ilayer + 1
      do istack = 1, len(lines(iline)%content) / 4 + 1
        pos = (istack - 1) * 4 + 2
        poschar = lines(iline)%content(pos:pos)
        if (poschar /= " ") stacks(istack)%content = stacks(istack)%content // poschar
      end do
    end do

    noperations = size(lines) - nlayers - 2
    allocate(operations(3, noperations))
    do ioperation = 1, noperations
      iline = nlayers + 2 + ioperation
      associate(line => lines(iline)%content, operation => operations(:, ioperation))
        read(line, *) movestr, operation(1), fromstr, operation(2), tostr, operation(3)
      end associate
    end do

  end subroutine extract_input


  subroutine apply_operations(operations, stacks)
    integer, intent(in) :: operations(:,:)
    type(string_t), intent(inout) :: stacks(:)

    integer :: ioperation, fromstacklen, ic
    character(:), allocatable :: moved, reversed

    do ioperation = 1, size(operations, dim=2)
      associate(&
          & amount => operations(1, ioperation),&
          & fromstack => stacks(operations(2, ioperation)),&
          & tostack => stacks(operations(3, ioperation)))
        fromstacklen = len(fromstack%content)
        moved = fromstack%content(fromstacklen - amount + 1 : fromstacklen)
        reversed = moved
        do ic = 1, len(moved)
          reversed(len(reversed) - ic + 1 : len(reversed) - ic + 1) = moved(ic:ic)
        end do
        tostack%content = tostack%content // reversed
        fromstack%content = fromstack%content(1 : fromstacklen - amount)
      end associate
    end do

  end subroutine apply_operations


  subroutine get_top_crates(stacks, topcrates)
    type(string_t), intent(in) :: stacks(:)
    character(:), allocatable, intent(out) :: topcrates

    integer :: istack, stacksize

    allocate(character(size(stacks)) :: topcrates)
    do istack = 1, size(stacks)
      stacksize = len(stacks(istack)%content)
      topcrates(istack:istack) = stacks(istack)%content(stacksize : stacksize)
    end do

  end subroutine get_top_crates

end program d05_1