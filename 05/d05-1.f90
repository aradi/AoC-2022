! Second version of the solution for day 5
! The input was inspired a lot by S. Lionel's solution at
! https://github.com/sblionel/AOC2022-in-Fortran/blob/main/AOC05_1.f90
!
program d05_1
  implicit none

  integer, parameter ::maxlinelen = 1024

  character(maxlinelen) :: buffer
  integer :: fp, iostat
  integer :: nstacks, istack, nlayers, ilayer
  character :: firstchar
  character, allocatable :: stacks(:,:), newstacks(:,:)
  character, allocatable :: dynbuffer(:), stacktops(:)
  integer, allocatable :: stacksizes(:)
  character(4) :: movestr, fromstr, tostr
  integer :: amount, source, dest

  ! Determine nr. of stacks and layers
  nstacks = 0
  nlayers = 0
  open(newunit=fp, file="input.dat", action="read")
  do
    read(fp, "(a)", iostat=iostat) buffer
    if (iostat /= 0) exit
    read(buffer, *) firstchar
    if (firstchar /= "[") exit
    nlayers = nlayers + 1
    nstacks = max(nstacks, (len_trim(buffer) + 1) / 4)
  end do
  rewind(fp)

  ! Read in stacks
  allocate(stacks(nlayers + 1, nstacks), source=" ")
  do ilayer = 1, nlayers
    read(fp, "(a)") buffer
    dynbuffer = transfer(trim(buffer), ["c"])
    stacks(ilayer + 1, 1 : (size(dynbuffer) + 1) / 4) = dynbuffer(2::4)
  end do
  stacks(:,:) = stacks(size(stacks, dim=1) : 1 : -1, :)

  ! Find sizes and make new array which warrantedly can accomodate any potential stack sizes
  stacksizes = findloc(stacks, " ", dim=1) - 1
  allocate(newstacks(sum(stacksizes), nstacks))
  newstacks(1 : nlayers, :) = stacks(1 : nlayers, :)

  ! Read in and carry out move instructions
  read(fp, *)
  do
    read(fp, *, iostat=iostat) movestr, amount, fromstr, source, tostr, dest
    if (iostat /= 0) exit
    dynbuffer = newstacks(stacksizes(source) - amount + 1 : stacksizes(source), source)
    newstacks(stacksizes(dest) + 1 : stacksizes(dest) + amount, dest) =&
        & dynbuffer(size(dynbuffer) : 1 : -1)
    stacksizes(source) = stacksizes(source) - amount
    stacksizes(dest) = stacksizes(dest) + amount
  end do
  close(fp)

  stacktops = [(newstacks(stacksizes(istack), istack), istack = 1, nstacks)]
  print *, "Top elements of the stacks: ", stacktops

end program d05_1